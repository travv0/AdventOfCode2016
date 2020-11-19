const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const math = std.math;
const mem = std.mem;
const testing = std.testing;

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;

    const input = "01000100010010111";
    const bits = try parseBits(allocator, input);
    defer allocator.free(bits);

    {
        const checksum = try calculateAnswer(allocator, bits, 272);
        defer allocator.free(checksum);

        std.debug.print("Part 1: ", .{});
        for (checksum) |bit| {
            std.debug.print("{}", .{bit});
        }
        std.debug.print("\n", .{});
    }
    {
        const checksum = try calculateAnswer(allocator, bits, 35651584);
        defer allocator.free(checksum);

        std.debug.print("Part 2: ", .{});
        for (checksum) |bit| {
            std.debug.print("{}", .{bit});
        }
        std.debug.print("\n", .{});
    }
}

fn parseBits(allocator: *Allocator, input: []const u8) ![]u1 {
    var bits = try allocator.alloc(u1, input.len);
    for (input) |c, i|
        bits[i] = @intCast(u1, c - '0');
    return bits;
}

test "parseBits" {
    const bits = try parseBits(testing.allocator, "11010010101");
    defer testing.allocator.free(bits);
    testing.expectEqualSlices(u1, &[_]u1{ 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1 }, bits);
}

fn calculateAnswer(allocator: *Allocator, input: []const u1, goal_len: usize) ![]u1 {
    const data = try generateData(allocator, input, goal_len);
    defer allocator.free(data);
    return try calculateChecksum(allocator, data, goal_len);
}

fn generateData(allocator: *Allocator, initial_data: []const u1, goal_len: usize) ![]u1 {
    var result = ArrayList(u1).init(allocator);
    errdefer result.deinit();
    try result.appendSlice(initial_data);
    while (result.items.len < goal_len) {
        const data = try allocator.dupe(u1, result.items);
        defer allocator.free(data);
        try result.append(0);
        var i = data.len;
        while (i > 0) : (i -= 1) {
            try result.append(~data[i - 1]);
        }
    }
    return result.toOwnedSlice();
}

test "generateData" {
    {
        const result = try generateData(testing.allocator, &[_]u1{1}, 3);
        defer testing.allocator.free(result);
        testing.expectEqualSlices(u1, &[_]u1{ 1, 0, 0 }, result);
    }
    {
        const result = try generateData(testing.allocator, &[_]u1{0}, 3);
        defer testing.allocator.free(result);
        testing.expectEqualSlices(u1, &[_]u1{ 0, 0, 1 }, result);
    }
    {
        const result = try generateData(testing.allocator, &[_]u1{ 1, 1, 1, 1, 1 }, 11);
        defer testing.allocator.free(result);
        testing.expectEqualSlices(u1, &[_]u1{ 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0 }, result);
    }
    {
        const result = try generateData(testing.allocator, &[_]u1{ 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0 }, 25);
        defer testing.allocator.free(result);
        testing.expectEqualSlices(
            u1,
            &[_]u1{ 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0 },
            result,
        );
    }
}

fn calculateChecksum(allocator: *Allocator, data: []const u1, goal_len: usize) ![]u1 {
    var result = ArrayList(u1).init(allocator);
    errdefer result.deinit();
    try result.appendSlice(data[0..goal_len]);
    while (result.items.len % 2 == 0) {
        const d = try allocator.dupe(u1, result.items);
        defer allocator.free(d);
        result.items.len = 0;
        var i: usize = 0;
        while (i < d.len) : (i += 2) {
            const pair = d[i .. i + 2];
            if (mem.eql(u1, pair, &[_]u1{ 0, 0 }) or mem.eql(u1, pair, &[_]u1{ 1, 1 }))
                try result.append(1)
            else
                try result.append(0);
        }
    }
    return result.toOwnedSlice();
}

test "calculateChecksum" {
    const result = try calculateChecksum(
        testing.allocator,
        &[_]u1{ 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0 },
        12,
    );
    defer testing.allocator.free(result);
    testing.expectEqualSlices(u1, &[_]u1{ 1, 0, 0 }, result);
}

test "calculateAnswer" {
    const checksum = try calculateAnswer(testing.allocator, &[_]u1{ 1, 0, 0, 0, 0 }, 20);
    defer testing.allocator.free(checksum);
    testing.expectEqualSlices(u1, &[_]u1{ 0, 1, 1, 0, 0 }, checksum);
}
