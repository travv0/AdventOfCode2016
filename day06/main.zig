const std = @import("std");
const util = @import("util");
const Allocator = std.mem.Allocator;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    var frequencies: [100][26]u8 = [_][26]u8{[_]u8{0} ** 26} ** 100;
    const input = try util.readInput(allocator, 10 * 1024);
    const line_len = try fillFrequencies(input, &frequencies);

    const message1 = try uncorruptMessage(allocator, &frequencies, line_len, highestFrequency);
    std.debug.print("Part 1: {s}\n", .{message1});
    const message2 = try uncorruptMessage(allocator, &frequencies, line_len, lowestFrequency);
    std.debug.print("Part 2: {s}\n", .{message2});
}

fn fillFrequencies(input: []const u8, frequencies: [][26]u8) !u8 {
    var lines = std.mem.split(input, "\n");
    var line_len: u8 = 0;
    while (lines.next()) |line| {
        for (line) |char, i| {
            frequencies[i][char - 'a'] += 1;
            if (i > line_len) line_len = @intCast(u8, i) + 1;
        }
    }
    return line_len;
}

const test_input =
    \\eedadn
    \\drvtee
    \\eandsr
    \\raavrd
    \\atevrs
    \\tsrnev
    \\sdttsa
    \\rasrtv
    \\nssdts
    \\ntnada
    \\svetve
    \\tesnvt
    \\vntsnd
    \\vrdear
    \\dvrsen
    \\enarar
;

test "fillFrequencies" {
    var frequencies: [100][26]u8 = [_][26]u8{[_]u8{0} ** 26} ** 100;
    const line_len = try fillFrequencies(test_input, &frequencies);
    expectEqual(@as(u8, 6), line_len);
    expectEqual(@as(u8, 3), frequencies[0]['e' - 'a']);
    expectEqual(@as(u8, 2), frequencies[0]['d' - 'a']);
    expectEqual(@as(u8, 3), frequencies[2]['s' - 'a']);
    expectEqual(@as(u8, 2), frequencies[4]['v' - 'a']);
}

fn highestFrequency(frequencies: [][26]u8, col: usize) u8 {
    var max: u32 = 0;
    var char: u8 = undefined;

    for (frequencies[col]) |count, i| {
        if (count > max) {
            max = count;
            char = @intCast(u8, i) + 'a';
        }
    }

    return char;
}

test "highestFrequency" {
    var frequencies: [100][26]u8 = [_][26]u8{[_]u8{0} ** 26} ** 100;
    _ = try fillFrequencies(test_input, &frequencies);
    expectEqual(@as(u8, 'e'), highestFrequency(&frequencies, 0));
    expectEqual(@as(u8, 'a'), highestFrequency(&frequencies, 1));
    expectEqual(@as(u8, 's'), highestFrequency(&frequencies, 2));
    expectEqual(@as(u8, 't'), highestFrequency(&frequencies, 3));
    expectEqual(@as(u8, 'e'), highestFrequency(&frequencies, 4));
    expectEqual(@as(u8, 'r'), highestFrequency(&frequencies, 5));
}

fn lowestFrequency(frequencies: [][26]u8, col: usize) u8 {
    var min: u32 = std.math.maxInt(u32);
    var char: u8 = undefined;

    for (frequencies[col]) |count, i| {
        if (count > 0 and count < min) {
            min = count;
            char = @intCast(u8, i) + 'a';
        }
    }

    return char;
}

test "lowestFrequency" {
    var frequencies: [100][26]u8 = [_][26]u8{[_]u8{0} ** 26} ** 100;
    _ = try fillFrequencies(test_input, &frequencies);
    expectEqual(@as(u8, 'a'), lowestFrequency(&frequencies, 0));
    expectEqual(@as(u8, 'd'), lowestFrequency(&frequencies, 1));
    expectEqual(@as(u8, 'v'), lowestFrequency(&frequencies, 2));
    expectEqual(@as(u8, 'e'), lowestFrequency(&frequencies, 3));
    expectEqual(@as(u8, 'n'), lowestFrequency(&frequencies, 4));
    expectEqual(@as(u8, 't'), lowestFrequency(&frequencies, 5));
}

fn uncorruptMessage(
    allocator: *Allocator,
    frequencies: [][26]u8,
    message_len: u8,
    uncorruptFn: fn (frequencies: [][26]u8, col: usize) u8,
) ![]const u8 {
    var word = std.ArrayList(u8).init(allocator);
    var i: u8 = 0;
    while (i < message_len) : (i += 1) {
        try word.append(uncorruptFn(frequencies, i));
    }
    return word.toOwnedSlice();
}

test "uncorruptMessage" {
    const allocator = std.testing.allocator;
    var frequencies: [100][26]u8 = [_][26]u8{[_]u8{0} ** 26} ** 100;
    const line_len = try fillFrequencies(test_input, &frequencies);
    const easterMessage = try uncorruptMessage(allocator, &frequencies, line_len, highestFrequency);
    defer allocator.free(easterMessage);
    const adventMessage = try uncorruptMessage(allocator, &frequencies, line_len, lowestFrequency);
    defer allocator.free(adventMessage);
    expectEqualStrings("easter", easterMessage);
    expectEqualStrings("advent", adventMessage);
}
