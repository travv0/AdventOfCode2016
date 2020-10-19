const std = @import("std");
const util = @import("util.zig");
const c = @cImport(@cInclude("pcre.h"));
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const input = try util.readFileIntoString(allocator, "input.txt", 1024 * 50);
    const realRooms = try findRealRooms(allocator, input);
    defer freeRooms(allocator, realRooms);

    var sectorIdSum: u32 = 0;
    for (realRooms) |room| {
        sectorIdSum += room.sectorId;
    }
    std.debug.print("Part 1: {}\n", .{sectorIdSum});

    const northPoleRoom = try findNorthPoleRoom(realRooms);
    std.debug.print("Part 2: {}\n", .{northPoleRoom.sectorId});
}

const Room = struct {
    allocator: *Allocator,
    name: []const u8,
    sectorId: u16,
    checksum: []const u8,

    fn init(allocator: *Allocator, name: []const u8, sectorId: u16, checksum: []const u8) !Room {
        var n = try allocator.alloc(u8, name.len);
        errdefer allocator.free(n);
        var cs = try allocator.alloc(u8, checksum.len);
        errdefer allocator.free(cs);
        std.mem.copy(u8, n, name);
        std.mem.copy(u8, cs, checksum);
        return Room{
            .allocator = allocator,
            .name = n,
            .sectorId = sectorId,
            .checksum = cs,
        };
    }

    fn deinit(this: Room) void {
        this.allocator.free(this.name);
        this.allocator.free(this.checksum);
    }

    fn isReal(this: Room) bool {
        var counts = [_]u16{0} ** 26;
        for (this.name) |char| {
            if ('a' <= char and char <= 'z') {
                counts[char - 'a'] += 1;
            }
        }
        var letters: [26]u8 = undefined;
        std.mem.copy(u8, letters[0..], "abcdefghijklmnopqrstuvwxyz");
        std.sort.sort(u8, letters[0..], counts, letterCountGreaterThan);
        return std.mem.eql(u8, letters[0..5], this.checksum);
    }

    test "isReal" {
        const room = try Room.init(std.testing.allocator, "not-a-real-room", 404, "oarel");
        defer room.deinit();
        std.debug.assert(room.isReal());
    }
};

fn letterCountGreaterThan(counts: [26]u16, char1: u8, char2: u8) bool {
    return counts[char1 - 'a'] > counts[char2 - 'a'];
}

pub fn parseLine(allocator: *Allocator, regexCompiled: *c.pcre, regexExtra: ?*c.pcre_extra, line: []const u8) !Room {
    const subStrsLen = 20;
    var subStrs: [subStrsLen]c_int = undefined;
    var name: ?[*:0]const u8 = undefined;
    var sectorId: ?[*:0]const u8 = undefined;
    var checksum: ?[*:0]const u8 = undefined;
    const execRet = c.pcre_exec(regexCompiled, regexExtra, line.ptr, @intCast(c_int, line.len), 0, 0, &subStrs, subStrsLen);
    if (execRet != 4) {
        util.exitWithError(error.WrongNumberOfParsedGroups, "group count: {}, line: {}", .{ execRet, line });
    }
    _ = c.pcre_get_substring(line.ptr, &subStrs, execRet, 1, &name);
    _ = c.pcre_get_substring(line.ptr, &subStrs, execRet, 2, &sectorId);
    _ = c.pcre_get_substring(line.ptr, &subStrs, execRet, 3, &checksum);
    const roomName = std.mem.span(name orelse return error.CouldNotParseName);
    const roomSectorId = try std.fmt.parseUnsigned(u16, std.mem.span(sectorId orelse return error.CouldNotParseSectorId), 10);
    const roomChecksum = std.mem.span(checksum orelse return error.CouldNotParseChecksum);
    return Room.init(allocator, roomName, roomSectorId, roomChecksum);
}

test "parseLine" {
    var pcreError: ?*const u8 = undefined;
    var pcreErrorOffset: c_int = undefined;
    const regexCompiled = c.pcre_compile("(.*)-(.*)\\[(.*)\\]", 0, &pcreError, &pcreErrorOffset, null).?;
    const regexExtra = c.pcre_study(regexCompiled, 0, &pcreError);
    defer c.pcre_free.?(regexCompiled);
    const room = try parseLine(std.testing.allocator, regexCompiled, regexExtra, "not-a-real-room-404[oarel]");
    defer room.deinit();
    std.debug.assert(std.mem.eql(u8, room.name, "not-a-real-room"));
    std.debug.assert(room.sectorId == 404);
    std.debug.assert(std.mem.eql(u8, room.checksum, "oarel"));
}

fn findRealRooms(allocator: *Allocator, input: []const u8) ![]Room {
    var pcreError: ?*const u8 = undefined;
    var pcreErrorOffset: c_int = undefined;
    const pcreFree = c.pcre_free orelse return error.NoPcreFree;
    const regexCompiled = c.pcre_compile("(.*)-(.*)\\[(.*)\\]", 0, &pcreError, &pcreErrorOffset, null) orelse {
        std.log.err("error compiling regex: {}", .{pcreError});
        std.os.exit(1);
    };
    defer pcreFree(regexCompiled);
    const regexExtra = c.pcre_study(regexCompiled, 0, &pcreError);
    if (pcreError) |err| {
        std.log.err("error studying regex: {}", .{err});
        std.os.exit(1);
    }
    defer if (regexExtra) |re| pcreFree(re);

    var rooms = ArrayList(Room).init(allocator);
    errdefer rooms.deinit();
    var lines = std.mem.split(std.fmt.trim(input), "\n");
    while (lines.next()) |line| {
        const room = try parseLine(allocator, regexCompiled, regexExtra, line);
        errdefer room.deinit();
        if (room.isReal()) {
            try rooms.append(room);
        } else {
            room.deinit();
        }
    }
    return rooms.toOwnedSlice();
}

fn freeRooms(allocator: *Allocator, rooms: []Room) void {
    for (rooms) |room| room.deinit();
    allocator.free(rooms);
}

test "findRealRooms" {
    const input =
        \\aaaaa-bbb-z-y-x-123[abxyz]
        \\a-b-c-d-e-f-g-h-987[abcde]
        \\not-a-real-room-404[oarel]
        \\totally-real-room-200[decoy]
        \\
    ;
    const rooms = try findRealRooms(std.testing.allocator, input);
    defer freeRooms(std.testing.allocator, rooms);
    std.debug.assert(3 == rooms.len);
}

fn shiftCipher(str: []const u8, buffer: []u8, shiftAmount: u16) void {
    for (str) |char, i| {
        if (char == '-')
            buffer[i] = ' '
        else
            buffer[i] = @intCast(u8, (@intCast(u16, char) - 'a' + shiftAmount) % 26 + 'a');
    }
}

test "shiftCipher" {
    const name = "qzmt-zixmtkozy-ivhz";
    const sectorId = 343;
    var buffer: [name.len]u8 = undefined;
    shiftCipher(name, buffer[0..], sectorId);
    std.debug.assert(std.mem.eql(u8, "very encrypted name", buffer[0..]));
}

fn findNorthPoleRoom(rooms: []Room) !Room {
    var buffer: [100]u8 = undefined;
    for (rooms) |room| {
        shiftCipher(room.name, buffer[0..], room.sectorId);
        if (std.mem.startsWith(u8, buffer[0..], "north")) {
            std.log.info("{}", .{buffer[0..room.name.len]});
            return room;
        }
    }
    return error.NorthPoleNotFound;
}
