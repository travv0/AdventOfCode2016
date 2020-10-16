const std = @import("std");
const util = @import("util.zig");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const print = std.debug.print;
const assert = std.debug.assert;
const mem = std.mem;

const Direction = enum { U, R, D, L };

pub fn main() anyerror!void {
    const allocator = std.heap.page_allocator;
    const input = try util.readFileIntoString(allocator, "input.txt", 1024 * 10);
    const code = try findCode(allocator, input);
    defer allocator.free(code);
    print("Part 1: {}\n", .{code});
}

fn findCode(allocator: *Allocator, input: []const u8) ![]u8 {
    const allDirs = try parseInput(allocator, input);
    defer allocator.free(allDirs);
    defer for (allDirs) |dir| allocator.free(dir);

    var result = try allocator.alloc(u8, allDirs.len);
    var num: u4 = 5;
    for (allDirs) |dirs, i| {
        for (dirs) |dir| {
            num = moveDirection(num, dir);
        }
        result[i] = std.fmt.digitToChar(num, false);
    }
    return result;
}

test "findCode" {
    var code = try findCode(std.testing.allocator, "ULL\nRRDDD\nLURDL\nUUUUD\n");
    defer std.testing.allocator.free(code);
    assert(mem.eql(u8, "1985", code));
}

fn parseInput(allocator: *Allocator, input: []const u8) ![][]Direction {
    var result = ArrayList([]Direction).init(allocator);
    errdefer result.deinit();
    var lines = if (mem.indexOf(u8, input, "\r\n") != null)
        mem.split(std.fmt.trim(input), "\r\n")
    else
        mem.split(std.fmt.trim(input), "\n");
    while (lines.next()) |line| {
        var dirs = try allocator.alloc(Direction, line.len);
        errdefer allocator.free(dirs);
        for (line) |c, i| {
            dirs[i] = switch (c) {
                'U' => .U,
                'R' => .R,
                'D' => .D,
                'L' => .L,
                else => util.exitWithError(error.InvalidDirection, "Character '{c}", .{c}),
            };
        }
        try result.append(dirs);
    }
    return result.items;
}

test "parseInput" {
    const dirs = try parseInput(std.testing.allocator, "ULL\nRRDDD\nLURDL\nUUUUD\n");
    defer std.testing.allocator.free(dirs);
    defer for (dirs) |dir| std.testing.allocator.free(dir);
    assert(4 == dirs.len);
    assert(mem.eql(Direction, &[_]Direction{ .U, .L, .L }, dirs[0]));
    assert(mem.eql(Direction, &[_]Direction{ .R, .R, .D, .D, .D }, dirs[1]));
    assert(mem.eql(Direction, &[_]Direction{ .L, .U, .R, .D, .L }, dirs[2]));
    assert(mem.eql(Direction, &[_]Direction{ .U, .U, .U, .U, .D }, dirs[3]));
}

fn moveDirection(num: u4, dir: Direction) u4 {
    return switch (dir) {
        .U => if (num > 3) num - 3 else num,
        .R => if (num % 3 != 0) num + 1 else num,
        .D => if (num < 7) num + 3 else num,
        .L => if ((num - 1) % 3 != 0) num - 1 else num,
    };
}

test "moveDirection" {
    assert(1 == moveDirection(1, .L));
    assert(3 == moveDirection(3, .U));
    assert(3 == moveDirection(6, .U));
    assert(9 == moveDirection(9, .R));
    assert(7 == moveDirection(7, .L));
    assert(5 == moveDirection(4, .R));
    assert(7 == moveDirection(4, .D));
}
