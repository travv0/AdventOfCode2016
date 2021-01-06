const std = @import("std");
const util = @import("util");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const print = std.debug.print;
const expectEqual = std.testing.expectEqual;
const expectEqualSlices = std.testing.expectEqualSlices;
const expectEqualStrings = std.testing.expectEqualStrings;
const mem = std.mem;
const ascii = std.ascii;

const Direction = enum { U, R, D, L };

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;
    const input = try util.readInput(allocator, 1024 * 10);
    defer allocator.free(input);

    const code_part1 = try findCode(allocator, input, moveDirectionPart1);
    defer allocator.free(code_part1);
    print("Part 1: {s}\n", .{code_part1});

    const code_part2 = try findCode(allocator, input, moveDirectionPart2);
    defer allocator.free(code_part2);
    print("Part 2: {s}\n", .{code_part2});
}

fn findCode(allocator: *Allocator, input: []const u8, moveFn: fn (u8, Direction) u8) ![]u8 {
    const all_dirs = try parseInput(allocator, input);
    defer allocator.free(all_dirs);
    defer for (all_dirs) |dir| allocator.free(dir);

    var result = try allocator.alloc(u8, all_dirs.len);
    var num: u8 = '5';
    for (all_dirs) |dirs, i| {
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
        mem.split(util.trim(input), "\r\n")
    else
        mem.split(util.trim(input), "\n");
    while (lines.next()) |line| {
        var dirs = try allocator.alloc(Direction, line.len);
        errdefer allocator.free(dirs);
        for (line) |c, i| {
            dirs[i] = switch (c) {
                'U' => .U,
                'R' => .R,
                'D' => .D,
                'L' => .L,
                else => util.exitWithError(error.InvalidDirection, "Character '{c}'", .{c}),
            };
        }
        try result.append(dirs);
    }
    return result.toOwnedSlice();
}

test "parseInput" {
    const dirs = try parseInput(std.testing.allocator, "ULL\nRRDDD\nLURDL\nUUUUD\n");
    defer std.testing.allocator.free(dirs);
    defer for (dirs) |dir| std.testing.allocator.free(dir);
    expectEqual(@as(usize, 4), dirs.len);
    expectEqualSlices(Direction, &[_]Direction{ .U, .L, .L }, dirs[0]);
    expectEqualSlices(Direction, &[_]Direction{ .R, .R, .D, .D, .D }, dirs[1]);
    expectEqualSlices(Direction, &[_]Direction{ .L, .U, .R, .D, .L }, dirs[2]);
    expectEqualSlices(Direction, &[_]Direction{ .U, .U, .U, .U, .D }, dirs[3]);
}

fn moveDirectionPart1(button: u8, dir: Direction) u8 {
    const num = std.fmt.charToDigit(button, 10) catch 5;
    const new_num = switch (dir) {
        .U => if (num > 3) num - 3 else num,
        .R => if (num % 3 != 0) num + 1 else num,
        .D => if (num < 7) num + 3 else num,
        .L => if ((num - 1) % 3 != 0) num - 1 else num,
    };
    return std.fmt.digitToChar(new_num, false);
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
    expectEqual(@as(u8, '1'), moveDirectionPart1('1', .L));
    expectEqual(@as(u8, '3'), moveDirectionPart1('3', .U));
    expectEqual(@as(u8, '3'), moveDirectionPart1('6', .U));
    expectEqual(@as(u8, '9'), moveDirectionPart1('9', .R));
    expectEqual(@as(u8, '7'), moveDirectionPart1('7', .L));
    expectEqual(@as(u8, '5'), moveDirectionPart1('4', .R));
    expectEqual(@as(u8, '7'), moveDirectionPart1('4', .D));
}

test "findCode" {
    var code_part1 = try findCode(std.testing.allocator, "ULL\nRRDDD\nLURDL\nUUUUD\n", moveDirectionPart1);
    defer std.testing.allocator.free(code_part1);
    expectEqualStrings("1985", code_part1);

    var code_part2 = try findCode(std.testing.allocator, "ULL\nRRDDD\nLURDL\nUUUUD\n", moveDirectionPart2);
    defer std.testing.allocator.free(code_part2);
    expectEqualStrings("5DB3", code_part2);
}
