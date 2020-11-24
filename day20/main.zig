const std = @import("std");
const util = @import("util");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const fmt = std.fmt;
const mem = std.mem;
const testing = std.testing;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = &gpa.allocator;

    const input = try util.readInput(allocator, 1024 * 1024);
    defer allocator.free(input);

    var ranges = try parseInput(allocator, input);
    defer allocator.free(ranges);

    var count: usize = 0;
    const ip = findUnblockedIp(ranges, std.math.maxInt(u32), &count);

    std.debug.print("Part 1: {}\n", .{ip});
    std.debug.print("Part 2: {}\n", .{count});
}

const Range = struct { min: u32, max: u32 };

fn parseRange(allocator: *Allocator, str: []const u8) !Range {
    const parts = try util.split(allocator, util.trim(str), "-");
    defer allocator.free(parts);
    return Range{
        .min = try fmt.parseUnsigned(u32, parts[0], 10),
        .max = try fmt.parseUnsigned(u32, parts[1], 10),
    };
}

test "parseRange" {
    {
        const range = try parseRange(testing.allocator, "0-2");
        testing.expectEqual(@as(u32, 0), range.min);
        testing.expectEqual(@as(u32, 2), range.max);
    }
    {
        const range = try parseRange(testing.allocator, "123456-4567890");
        testing.expectEqual(@as(u32, 123456), range.min);
        testing.expectEqual(@as(u32, 4567890), range.max);
    }
}

fn parseInput(allocator: *Allocator, input: []const u8) ![]Range {
    var ranges = ArrayList(Range).init(allocator);
    errdefer ranges.deinit();
    var lines = mem.split(util.trim(input), "\n");
    while (lines.next()) |line| {
        try ranges.append(try parseRange(allocator, line));
    }
    return ranges.toOwnedSlice();
}

test "parseInput" {
    const ranges = try parseInput(testing.allocator, "0-2\n123456-4567890\n");
    defer testing.allocator.free(ranges);
    testing.expectEqual(@as(u32, 0), ranges[0].min);
    testing.expectEqual(@as(u32, 2), ranges[0].max);
    testing.expectEqual(@as(u32, 123456), ranges[1].min);
    testing.expectEqual(@as(u32, 4567890), ranges[1].max);
}

fn rangeLessThan(context: void, a: Range, b: Range) bool {
    return if (a.min == b.min)
        a.max < b.max
    else
        a.min < b.min;
}

fn findUnblockedIp(ranges: []Range, max_ip: u32, count: *usize) ?u32 {
    std.sort.sort(Range, ranges, {}, rangeLessThan);
    var result: ?u32 = null;
    var ip: u32 = 0;
    for (ranges) |range| {
        while (ip < range.min) {
            result = result orelse ip;
            count.* += 1;
            ip += 1;
        }
        if (ip < range.max) {
            if (range.max < max_ip)
                ip = range.max + 1
            else {
                ip = max_ip;
                break;
            }
        }
    }
    count.* += max_ip - ip;
    return result;
}

test "findUnblockedIP" {
    var ranges = try parseInput(testing.allocator,
        \\5-8
        \\0-2
        \\4-7
    );
    defer testing.allocator.free(ranges);

    var count: usize = 0;
    const ip = findUnblockedIp(ranges, 9, &count);

    testing.expectEqual(@as(?u32, 3), ip);
    testing.expectEqual(@as(usize, 2), count);
}
