const std = @import("std");
const util = @import("util");
const testing = std.testing;
const expect = testing.expect;
const expectEqual = testing.expectEqual;
const expectEqualStrings = testing.expectEqualStrings;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const input = try util.readInput(&arena.allocator, 1000 * 1024);
    var tlsCount: u32 = 0;
    var sslCount: u32 = 0;
    var lines = std.mem.split(std.fmt.trim(input), "\n");
    while (lines.next()) |line| {
        if (supportsTls(line)) {
            tlsCount += 1;
        }
        if (try supportsSsl(&arena.allocator, line)) {
            sslCount += 1;
        }
    }
    std.debug.print("Part 1: {}\n", .{tlsCount});
    std.debug.print("Part 2: {}\n", .{sslCount});
}

fn supportsTls(ip: []const u8) bool {
    var rest = ip;
    while (true) {
        const hypernetStart = std.mem.indexOfScalar(u8, rest, '[') orelse break;
        const hypernetEnd = std.mem.indexOfScalar(u8, rest, ']') orelse break;
        const hypernet = rest[hypernetStart + 1 .. hypernetEnd];
        rest = rest[hypernetEnd + 1 ..];
        if (hasAbba(hypernet)) {
            return false;
        }
    }
    return hasAbba(ip);
}

test "supportsTls" {
    expect(supportsTls("abba[mnop]qrst"));
    expect(!supportsTls("abcd[bddb]xyyx"));
    expect(!supportsTls("aaaa[qwer]tyui"));
    expect(supportsTls("ioxxoj[asdfgh]zxcvbn"));
}

fn hasAbba(section: []const u8) bool {
    var i: u16 = 0;
    while (i < section.len - 3) : (i += 1) {
        if (section[i] == section[i + 3] and
            section[i + 1] == section[i + 2] and
            section[i] != section[i + 1])
        {
            return true;
        }
    }
    return false;
}

test "hasAbba" {
    expect(hasAbba("abba"));
    expect(!hasAbba("abcd"));
    expect(!hasAbba("aaaa"));
    expect(hasAbba("ioxxoj"));
}

fn supportsSsl(allocator: *Allocator, ip: []const u8) !bool {
    const abas = try findAbas(allocator, ip);
    defer allocator.free(abas);
    const babs = try findBabs(allocator, ip);
    defer allocator.free(babs);
    for (abas) |aba| {
        for (babs) |bab| {
            var bab2: [3]u8 = undefined;
            babify(aba, &bab2);
            if (std.mem.eql(u8, bab, &bab2)) {
                return true;
            }
        }
    }
    return false;
}

test "supportsSsl" {
    expect(try supportsSsl(testing.allocator, "aba[bab]xyz"));
    expect(!try supportsSsl(testing.allocator, "xyx[xyx]xyx"));
    expect(try supportsSsl(testing.allocator, "aaa[kek]eke"));
    expect(try supportsSsl(testing.allocator, "zazbz[bzb]cdb"));
}

fn findAbas(allocator: *Allocator, ip: []const u8) ![][]const u8 {
    var abas = ArrayList([]const u8).init(allocator);
    errdefer abas.deinit();
    var rest = ip;
    var supernetStart: isize = -1;
    while (true) {
        const supernetEnd = std.mem.indexOfScalar(u8, rest, '[') orelse rest.len;
        const supernet = rest[@intCast(usize, supernetStart + 1)..supernetEnd];
        rest = if (supernetEnd < rest.len) rest[supernetEnd + 1 ..] else &[_]u8{};
        var i: u16 = 0;
        while (i < supernet.len - 2) : (i += 1) {
            const slice = supernet[i .. i + 3];
            if (isAba(slice)) {
                try abas.append(slice);
            }
        }
        supernetStart = @intCast(isize, std.mem.indexOfScalar(u8, rest, ']') orelse break);
    }
    return abas.toOwnedSlice();
}

test "findAbas" {
    const abas = try findAbas(testing.allocator, "aba[bab]xyz");
    defer testing.allocator.free(abas);
    expectEqualStrings("aba", abas[0]);
    expectEqual(@as(usize, 1), abas.len);
}

fn findBabs(allocator: *Allocator, ip: []const u8) ![][]const u8 {
    var babs = ArrayList([]const u8).init(allocator);
    errdefer babs.deinit();
    var rest = ip;
    while (true) {
        const hypernetStart = std.mem.indexOfScalar(u8, rest, '[') orelse break;
        const hypernetEnd = std.mem.indexOfScalar(u8, rest, ']') orelse break;
        const hypernet = rest[hypernetStart + 1 .. hypernetEnd];
        rest = rest[hypernetEnd + 1 ..];
        var i: u16 = 0;
        while (i < hypernet.len - 2) : (i += 1) {
            const slice = hypernet[i .. i + 3];
            if (isAba(slice)) {
                try babs.append(slice);
            }
        }
    }
    return babs.toOwnedSlice();
}

test "findBabs" {
    const babs = try findBabs(testing.allocator, "aba[bab]xyz");
    defer testing.allocator.free(babs);
    expectEqualStrings("bab", babs[0]);
    expectEqual(@as(usize, 1), babs.len);
}

fn isAba(section: []const u8) bool {
    return section[0] == section[2] and
        section[0] != section[1];
}

test "isAba" {
    expect(isAba("aba"));
    expect(!isAba("abc"));
    expect(!isAba("aaa"));
    expect(isAba("oxo"));
}

fn babify(aba: []const u8, bab: *[3]u8) void {
    bab[0] = aba[1];
    bab[1] = aba[0];
    bab[2] = aba[1];
}

test "babify" {
    var aba = "aba";
    var bab: [3]u8 = undefined;
    babify(aba, &bab);
    expectEqualStrings("bab", &bab);
}
