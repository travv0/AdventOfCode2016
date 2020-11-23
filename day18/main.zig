const std = @import("std");
const util = @import("util");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const mem = std.mem;
const testing = std.testing;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;
    const input = try util.readInput(allocator, 1024);
    defer allocator.free(input);

    var row = try parse(allocator, input);
    defer allocator.free(row);

    std.debug.print("Part 1: {}\n", .{try countSafeTiles(allocator, row, 40)});
    std.debug.print("Part 2: {}\n", .{try countSafeTiles(allocator, row, 400000)});
}

const Tile = enum { trap, safe };

fn isTrap(left: Tile, center: Tile, right: Tile) bool {
    return left == .trap and center == .trap and right != .trap or
        left != .trap and center == .trap and right == .trap or
        left == .trap and center != .trap and right != .trap or
        left != .trap and center != .trap and right == .trap;
}

fn generateRow(allocator: *Allocator, prev_row: []const Tile) ![]Tile {
    var row = try allocator.alloc(Tile, prev_row.len);
    errdefer allocator.free(row);
    var i: usize = 0;
    while (i < row.len) : (i += 1) {
        const left = if (i + 1 >= prev_row.len) .safe else prev_row[i + 1];
        const center = prev_row[i];
        const right = if (i < 1) .safe else prev_row[i - 1];
        row[i] = if (isTrap(left, center, right))
            .trap
        else
            .safe;
    }
    return row;
}

test "generateRow" {
    {
        const row = try generateRow(testing.allocator, &[_]Tile{ .safe, .safe, .trap, .trap, .safe });
        defer testing.allocator.free(row);
        testing.expectEqualSlices(Tile, &[_]Tile{ .safe, .trap, .trap, .trap, .trap }, row);
    }
    {
        const row = try generateRow(testing.allocator, &[_]Tile{ .safe, .trap, .trap, .trap, .trap });
        defer testing.allocator.free(row);
        testing.expectEqualSlices(Tile, &[_]Tile{ .trap, .trap, .safe, .safe, .trap }, row);
    }
}

fn parse(allocator: *Allocator, input: []const u8) ![]Tile {
    var row = ArrayList(Tile).init(allocator);
    errdefer row.deinit();
    for (util.trim(input)) |c| {
        switch (c) {
            '^' => try row.append(.trap),
            '.' => try row.append(.safe),
            else => std.debug.panic("Invalid char in row: {}", .{c}),
        }
    }
    return row.toOwnedSlice();
}

test "parse" {
    const row = try parse(testing.allocator, "..^^.");
    defer testing.allocator.free(row);
    testing.expectEqualSlices(Tile, &[_]Tile{ .safe, .safe, .trap, .trap, .safe }, row);
}

fn countSafeTiles(allocator: *Allocator, start_row: []const Tile, row_count: usize) !usize {
    var row = try allocator.dupe(Tile, start_row);
    defer allocator.free(row);
    var safe_tiles: usize = 0;

    var i: usize = 0;
    while (i < row_count) : (i += 1) {
        for (row) |tile| {
            if (tile == .safe)
                safe_tiles += 1;
        }
        const prev_row = row;
        defer allocator.free(prev_row);
        row = try generateRow(allocator, prev_row);
    }

    return safe_tiles;
}

test "countSafeTiles" {
    const row = try parse(testing.allocator, ".^^.^.^^^^");
    defer testing.allocator.free(row);
    testing.expectEqual(@as(usize, 38), try countSafeTiles(testing.allocator, row, 10));
}
