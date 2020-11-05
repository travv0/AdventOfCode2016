const std = @import("std");
const builtin = @import("builtin");
const util = @import("util");
const testing = std.testing;
const expectEqual = testing.expectEqual;
const mem = std.mem;
const fmt = std.fmt;

pub fn main() anyerror!void {
    if (builtin.cpu.arch != .x86_64) {
        util.exitWithError(error.Not64BitCpu, "Day 9 requires 64 bit CPU architecture", .{});
    }

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;
    const input = try util.readInput(allocator, 1024 * 1024);
    defer allocator.free(input);

    std.debug.print("Part 1: {}\n", .{try decompressedLength(util.trim(input), false)});
    std.debug.print("Part 2: {}\n", .{try decompressedLength(util.trim(input), true)});
}

const DecompressError = error{ NoMatchingParenthesis, ParsingError } || std.fmt.ParseIntError;

fn decompressedLength(text: []const u8, recursive: bool) DecompressError!usize {
    var length: usize = 0;
    var rest = text;
    while (rest.len > 0) {
        const c = rest[0];
        rest = rest[1..];
        if (c == '(') {
            const end = mem.indexOfScalar(u8, rest, ')') orelse return DecompressError.NoMatchingParenthesis;
            var marker_iter = mem.split(rest[0..end], "x");
            const chars = try fmt.parseUnsigned(
                usize,
                marker_iter.next() orelse return DecompressError.ParsingError,
                10,
            );
            const multiplier = try fmt.parseUnsigned(
                usize,
                marker_iter.next() orelse return DecompressError.ParsingError,
                10,
            );
            rest = rest[end + 1 ..];
            length += multiplier * if (recursive)
                try decompressedLength(rest[0..chars], true)
            else
                chars;
            rest = rest[chars..];
        } else {
            length += 1;
        }
    }
    return length;
}

test "decompressedLength part 1" {
    expectEqual(@as(usize, 6), try decompressedLength("ADVENT", false));
    expectEqual(@as(usize, 7), try decompressedLength("A(1x5)BC", false));
    expectEqual(@as(usize, 9), try decompressedLength("(3x3)XYZ", false));
    expectEqual(@as(usize, 11), try decompressedLength("A(2x2)BCD(2x2)EFG", false));
    expectEqual(@as(usize, 6), try decompressedLength("(6x1)(1x3)A", false));
    expectEqual(@as(usize, 18), try decompressedLength("X(8x2)(3x3)ABCY", false));
}

test "decompressedLength part 2" {
    expectEqual(@as(usize, 9), try decompressedLength("(3x3)XYZ", true));
    expectEqual("XABCABCABCABCABCABCY".len, try decompressedLength("X(8x2)(3x3)ABCY", true));
    expectEqual(@as(usize, 241920), try decompressedLength("(27x12)(20x12)(13x14)(7x10)(1x12)A", true));
    expectEqual(@as(usize, 445), try decompressedLength("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN", true));
}
