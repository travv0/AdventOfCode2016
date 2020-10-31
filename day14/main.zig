const std = @import("std");
const Allocator = mem.Allocator;
const ArrayList = std.ArrayList;
const AutoHashMap = std.hash_map.AutoHashMap;
const Md5 = std.crypto.hash.Md5;
const fmt = std.fmt;
const mem = std.mem;
const sort = std.sort;
const testing = std.testing;

pub fn main() anyerror!void {
    const input = "ahsbgdzn";
    const allocator = std.heap.page_allocator;

    const result1 = find64thKey(allocator, input, false);
    std.debug.print("Part 1: {}\n", .{result1});

    const result2 = find64thKey(allocator, input, true);
    std.debug.print("Part 2: {}\n", .{result2});
}

fn find64thKey(allocator: *Allocator, input: []const u8, stretch_hash: bool) !usize {
    var output: [Md5.digest_length]u8 = undefined;
    var threes = AutoHashMap(usize, Char).init(allocator);
    defer threes.deinit();
    var keys = ArrayList(Char).init(allocator);
    defer keys.deinit();
    var count: usize = 0;
    var i: usize = 0;
    while (true) : (i += 1) {
        const salt = try fmt.allocPrint(allocator, "{}{}", .{ input, i });
        defer allocator.free(salt);
        Md5.hash(salt, &output, .{});
        if (stretch_hash) {
            var buf: [32]u8 = undefined;
            var j: usize = 0;
            while (j < 2016) : (j += 1) {
                var input2 = try fmt.bufPrint(&buf, "{x}", .{output});
                Md5.hash(input2, &output, .{});
            }
        }
        const hash = try fmt.allocPrint(allocator, "{x}", .{output});
        defer allocator.free(hash);
        try checkThrees(allocator, i, hash, &threes, &keys);
        if (keys.items.len >= 64) {
            sort.sort(Char, keys.items, {}, charLessThan);
            return keys.items[63].i;
        }
        if (nInARow(3, hash)) |c| {
            try threes.put(i, .{ .char = c, .i = i });
        }
    }
}

pub fn charLessThan(context: void, lhs: Char, rhs: Char) bool {
    return lhs.i < rhs.i;
}

fn checkThrees(
    allocator: *Allocator,
    i: usize,
    hash: []const u8,
    threes: *AutoHashMap(usize, Char),
    keys: *ArrayList(Char),
) !void {
    const maybe_c = nInARow(5, hash);
    if (maybe_c) |c| {
        var to_remove = ArrayList(usize).init(allocator);
        defer to_remove.deinit();
        var iter = threes.iterator();
        while (iter.next()) |three| {
            if (i > three.key + 1000) {
                try to_remove.append(three.key);
            } else if (c == three.value.char) {
                try keys.append(three.value);
                try to_remove.append(three.key);
            }
        }
        for (to_remove.items) |key| {
            _ = threes.remove(key);
        }
    }
}

const Char = struct {
    char: u8,
    i: usize,
};

fn nInARow(n: usize, hash: []const u8) ?u8 {
    var count: usize = 0;
    var prev_char: u8 = 0;
    for (hash) |c| {
        if (prev_char == c) {
            count += 1;
        } else {
            prev_char = c;
            count = 1;
        }
        if (count >= n) {
            return prev_char;
        }
    }
    return null;
}

test "nInARow" {
    testing.expectEqual(@as(?u8, '8'), nInARow(3, "cc38887a5"));
    testing.expectEqual(@as(?u8, 'c'), nInARow(2, "cc38887a5"));
    testing.expectEqual(@as(?u8, '7'), nInARow(5, "cc388877777a5"));
    testing.expectEqual(@as(?u8, null), nInARow(5, "cc38887777a5"));
}

test "find64thKey" {
    const input = "abc";
    testing.expectEqual(@as(usize, 22728), try find64thKey(testing.allocator, input, false));
    testing.expectEqual(@as(usize, 22551), try find64thKey(testing.allocator, input, true));
}
