const std = @import("std");
const util = @import("util");
const c = @cImport(@cInclude("pcre.h"));
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub fn main() !void {
    const allocator = std.heap.c_allocator;
    const input = try util.readInput(allocator, 1024 * 50);
    defer allocator.free(input);
    const real_rooms = try findRealRooms(allocator, input);
    defer freeRooms(allocator, real_rooms);

    var sector_id_sum: u32 = 0;
    for (real_rooms) |room| {
        sector_id_sum += room.sector_id;
    }
    std.debug.print("Part 1: {}\n", .{sector_id_sum});

    const north_pole_room = try findNorthPoleRoom(real_rooms);
    std.debug.print("Part 2: {}\n", .{north_pole_room.sector_id});
}

const Room = struct {
    allocator: *Allocator,
    name: []const u8,
    sector_id: u16,
    checksum: []const u8,

    fn init(allocator: *Allocator, name: []const u8, sector_id: u16, checksum: []const u8) !Room {
        var n = try allocator.alloc(u8, name.len);
        errdefer allocator.free(n);
        var cs = try allocator.alloc(u8, checksum.len);
        errdefer allocator.free(cs);
        std.mem.copy(u8, n, name);
        std.mem.copy(u8, cs, checksum);
        return Room{
            .allocator = allocator,
            .name = n,
            .sector_id = sector_id,
            .checksum = cs,
        };
    }

    fn deinit(this: *Room) void {
        this.allocator.free(this.name);
        this.allocator.free(this.checksum);
        this.* = undefined;
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
        var room = try Room.init(std.testing.allocator, "not-a-real-room", 404, "oarel");
        defer room.deinit();
        std.testing.expect(room.isReal());
    }
};

fn letterCountGreaterThan(counts: [26]u16, char1: u8, char2: u8) bool {
    return counts[char1 - 'a'] > counts[char2 - 'a'];
}

pub fn parseLine(allocator: *Allocator, regex_compiled: *c.pcre, regex_extra: ?*c.pcre_extra, line: []const u8) !Room {
    const sub_strs_len = 20;
    var sub_strs: [sub_strs_len]c_int = undefined;
    var name: ?[*:0]const u8 = undefined;
    var sector_id: ?[*:0]const u8 = undefined;
    var checksum: ?[*:0]const u8 = undefined;
    const exec_ret = c.pcre_exec(regex_compiled, regex_extra, line.ptr, @intCast(c_int, line.len), 0, 0, &sub_strs, sub_strs_len);
    if (exec_ret != 4) {
        return error.WrongNumberOfParsedGroups;
    }
    _ = c.pcre_get_substring(line.ptr, &sub_strs, exec_ret, 1, &name);
    _ = c.pcre_get_substring(line.ptr, &sub_strs, exec_ret, 2, &sector_id);
    _ = c.pcre_get_substring(line.ptr, &sub_strs, exec_ret, 3, &checksum);
    const room_name = std.mem.span(name orelse return error.CouldNotParseName);
    const room_sector_id = try std.fmt.parseUnsigned(u16, std.mem.span(sector_id orelse return error.CouldNotParseSectorId), 10);
    const room_checksum = std.mem.span(checksum orelse return error.CouldNotParseChecksum);
    return Room.init(allocator, room_name, room_sector_id, room_checksum);
}

test "parseLine" {
    var pcre_error: ?*const u8 = undefined;
    var pcre_error_offset: c_int = undefined;
    const regex_compiled = c.pcre_compile("(.*)-(.*)\\[(.*)\\]", 0, &pcre_error, &pcre_error_offset, null).?;
    const regex_extra = c.pcre_study(regex_compiled, 0, &pcre_error);
    defer c.pcre_free.?(regex_compiled);
    var room = try parseLine(std.testing.allocator, regex_compiled, regex_extra, "not-a-real-room-404[oarel]");
    defer room.deinit();
    std.testing.expectEqualStrings("not-a-real-room", room.name);
    std.testing.expectEqual(@as(u16, 404), room.sector_id);
    std.testing.expectEqualStrings("oarel", room.checksum);
}

fn findRealRooms(allocator: *Allocator, input: []const u8) ![]Room {
    var pcre_error: ?*const u8 = undefined;
    var pcre_error_offset: c_int = undefined;
    const pcre_free = c.pcre_free orelse return error.NoPcreFree;
    const regex_compiled = c.pcre_compile("(.*)-(.*)\\[(.*)\\]", 0, &pcre_error, &pcre_error_offset, null) orelse {
        std.log.err("error compiling regex: {}", .{pcre_error});
        std.os.exit(1);
    };
    defer pcre_free(regex_compiled);
    const regex_extra = c.pcre_study(regex_compiled, 0, &pcre_error);
    if (pcre_error) |err| {
        std.log.err("error studying regex: {}", .{err});
        std.os.exit(1);
    }
    defer if (regex_extra) |re| pcre_free(re);

    var rooms = ArrayList(Room).init(allocator);
    errdefer freeRooms(allocator, rooms.items);
    var lines = std.mem.split(util.trim(input), "\n");
    while (lines.next()) |line| {
        var room = try parseLine(allocator, regex_compiled, regex_extra, line);
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
    for (rooms) |*room| room.deinit();
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
    std.testing.expectEqual(@as(usize, 3), rooms.len);
    std.testing.expectError(error.WrongNumberOfParsedGroups, findRealRooms(std.testing.allocator, input ++ "asdf"));
}

fn shiftCipher(str: []const u8, buffer: []u8, shiftAmount: u16) void {
    for (str) |char, i| {
        if (char == '-')
            buffer[i] = ' '
        else if ('a' <= char and char <= 'z')
            buffer[i] = @intCast(u8, (@as(u16, char) - 'a' + shiftAmount) % 26 + 'a');
    }
}

test "shiftCipher" {
    const name = "qzmt-zixmtkozy-ivhz";
    const sector_id = 343;
    var buffer: [name.len]u8 = undefined;
    shiftCipher(name, buffer[0..], sector_id);
    std.testing.expectEqualStrings("very encrypted name", buffer[0..]);
}

fn findNorthPoleRoom(rooms: []Room) !Room {
    var buffer: [100]u8 = undefined;
    for (rooms) |room| {
        shiftCipher(room.name, buffer[0..], room.sector_id);
        if (std.mem.startsWith(u8, buffer[0..], "north")) {
            return room;
        }
    }
    return error.NorthPoleNotFound;
}
