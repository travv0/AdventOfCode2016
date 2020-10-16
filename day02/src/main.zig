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
    const codePart1 = try findCode(allocator, input, moveDirectionPart1);
    defer allocator.free(codePart1);
    print("Part 1: {}\n", .{codePart1});

    const codePart2 = try findCode(allocator, input, moveDirectionPart2);
    defer allocator.free(codePart2);
    print("Part 2: {}\n", .{codePart2});
}

fn findCode(allocator: *Allocator, input: []const u8, moveFn: fn (u8, Direction) u8) ![]u8 {
    const allDirs = try parseInput(allocator, input);
    defer allocator.free(allDirs);
    defer for (allDirs) |dir| allocator.free(dir);

    var result = try allocator.alloc(u8, allDirs.len);
    var num: u8 = '5';
    for (allDirs) |dirs, i| {
        for (dirs) |dir| {
            num = moveFn(num, dir);
        }
        result[i] = num;
    }
    return result;
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

fn moveDirectionPart1(button: u8, dir: Direction) u8 {
    const num = std.fmt.charToDigit(button, 10) catch 5;
    const newNum = switch (dir) {
        .U => if (num > 3) num - 3 else num,
        .R => if (num % 3 != 0) num + 1 else num,
        .D => if (num < 7) num + 3 else num,
        .L => if ((num - 1) % 3 != 0) num - 1 else num,
    };
    return std.fmt.digitToChar(newNum, false);
}

fn moveDirectionPart2(button: u8, dir: Direction) u8 {
    return switch (dir) {
        .U => switch (button) {
            '3' => '1',
            '6' => '2',
            '7' => '3',
            '8' => '4',
            'A' => '6',
            'B' => '7',
            'C' => '8',
            'D' => 'B',
            else => button,
        },
        .R => switch (button) {
            '2' => '3',
            '3' => '4',
            '5' => '6',
            '6' => '7',
            '7' => '8',
            '8' => '9',
            'A' => 'B',
            'B' => 'C',
            else => button,
        },
        .D => switch (button) {
            '1' => '3',
            '2' => '6',
            '3' => '7',
            '4' => '8',
            '6' => 'A',
            '7' => 'B',
            '8' => 'C',
            'B' => 'D',
            else => button,
        },
        .L => switch (button) {
            '3' => '2',
            '4' => '3',
            '6' => '5',
            '7' => '6',
            '8' => '7',
            '9' => '8',
            'B' => 'A',
            'C' => 'B',
            else => button,
        },
    };
}

test "moveDirectionPart1" {
    assert('1' == moveDirectionPart1('1', .L));
    assert('3' == moveDirectionPart1('3', .U));
    assert('3' == moveDirectionPart1('6', .U));
    assert('9' == moveDirectionPart1('9', .R));
    assert('7' == moveDirectionPart1('7', .L));
    assert('5' == moveDirectionPart1('4', .R));
    assert('7' == moveDirectionPart1('4', .D));
}

test "findCode" {
    var codePart1 = try findCode(std.testing.allocator, "ULL\nRRDDD\nLURDL\nUUUUD\n", moveDirectionPart1);
    defer std.testing.allocator.free(codePart1);
    assert(mem.eql(u8, "1985", codePart1));

    var codePart2 = try findCode(std.testing.allocator, "ULL\nRRDDD\nLURDL\nUUUUD\n", moveDirectionPart2);
    defer std.testing.allocator.free(codePart2);
    assert(mem.eql(u8, "5DB3", codePart2));
}
