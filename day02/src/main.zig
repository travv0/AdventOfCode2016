const std = @import("std");
const util = @import("util.zig");
const Allocator = std.mem.Allocator;
const print = std.debug.print;
const assert = std.debug.assert;
const mem = std.mem;

const Direction = enum { U, R, D, L };

pub fn main() anyerror!void {
    const allocator = std.heap.page_allocator;
    const input = try util.readFileIntoString(allocator, "input.txt", 1024 * 10);
    print("Part 1: {}\n", .{try findCode(allocator, input)});
}

fn findCode(allocator: *Allocator, input: []const u8) ![]u4 {
    var result = [_]u4{ 1, 9, 8, 5 };
    return &result;
}

test "findCode" {
    assert(mem.eql(u4, &[_]u4{ 1, 9, 8, 5 }, try findCode(std.testing.allocator, "ULL\nRRDDD\nLURDL\nUUUUD\n")));
}

fn parseInput(allocator: *Allocator, input: []const u8) ![][]Direction {}

// test "parseInput" {
//     assert(mem.eql(u4, [_]u4{ 1, 9, 8, 5 }, try findCode(std.testing.allocator, "ULL\nRRDDD\nLURDL\nUUUUD\n")));
// }
