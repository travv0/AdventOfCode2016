const std = @import("std");
const c = @cImport(@cInclude("AStar.h"));
const absInt = std.math.absInt;
const testing = std.testing;
const expect = testing.expect;

pub fn main() anyerror!void {
    var from = Pos{ .x = 1, .y = 1 };
    var to = Pos{ .x = 31, .y = 39 };
    var favorite_number: usize = 1352;
    const path = c.ASPathCreate(&path_node_source, &favorite_number, &from, &to);
    defer c.ASPathDestroy(path);

    std.debug.print("Part 1: {}\n", .{c.ASPathGetCount(path) - 1});

    const count = countLocationsWithinRange(50, &from, &favorite_number);
    std.debug.print("Part 2: {}\n", .{count});
}

const Pos = struct {
    x: usize,
    y: usize,

    fn isWall(self: Pos, favorite_number: usize) bool {
        const x = self.x;
        const y = self.y;
        const num = (x * x) + (3 * x) + (2 * x * y) + y + (y * y) + favorite_number;
        return countSetBits(num) % 2 != 0;
    }
};

test "isWall" {
    const favorite_number = 10;
    expect(!(Pos{ .x = 0, .y = 0 }).isWall(favorite_number));
    expect((Pos{ .x = 1, .y = 0 }).isWall(favorite_number));
    expect((Pos{ .x = 4, .y = 3 }).isWall(favorite_number));
    expect(!(Pos{ .x = 5, .y = 3 }).isWall(favorite_number));
}

fn countSetBits(num: usize) usize {
    var count: usize = 0;
    var n = num;
    while (n > 0) {
        count += n & 1;
        n >>= 1;
    }
    return count;
}

fn pathNodeNeighbors(neighbors: c.ASNeighborList, node: ?*c_void, context: ?*c_void) callconv(.C) void {
    const pos = @ptrCast(?*Pos, @alignCast(8, node)).?;
    const favorite_number = @ptrCast(?*usize, @alignCast(8, context)).?.*;

    var right = Pos{ .x = pos.x + 1, .y = pos.y };
    if (!right.isWall(favorite_number)) {
        c.ASNeighborListAdd(neighbors, &right, 1);
    }

    if (pos.x > 0) {
        var left = Pos{ .x = pos.x - 1, .y = pos.y };
        if (!left.isWall(favorite_number)) {
            c.ASNeighborListAdd(neighbors, &left, 1);
        }
    }

    var down = Pos{ .x = pos.x, .y = pos.y + 1 };
    if (!down.isWall(favorite_number)) {
        c.ASNeighborListAdd(neighbors, &down, 1);
    }

    if (pos.y > 0) {
        var up = Pos{ .x = pos.x, .y = pos.y - 1 };
        if (!up.isWall(favorite_number)) {
            c.ASNeighborListAdd(neighbors, &up, 1);
        }
    }
}

fn pathNodeHeuristic(from_node: ?*c_void, to_node: ?*c_void, context: ?*c_void) callconv(.C) f32 {
    const from = @ptrCast(?*Pos, @alignCast(@alignOf(Pos), from_node)).?;
    const to = @ptrCast(?*Pos, @alignCast(@alignOf(Pos), to_node)).?;
    return @intToFloat(
        f32,
        (absInt(@intCast(isize, from.x) - @intCast(isize, to.x)) catch @panic("error taking absval")) +
            (absInt(@intCast(isize, from.y) - @intCast(isize, to.y)) catch @panic("error taking absval")),
    );
}

const path_node_source = c.ASPathNodeSource{
    .nodeSize = @sizeOf(Pos),
    .nodeNeighbors = pathNodeNeighbors,
    .pathCostHeuristic = pathNodeHeuristic,
    .earlyExit = null,
    .nodeComparator = null,
};

fn countLocationsWithinRange(range: usize, from: *Pos, favorite_number: *usize) usize {
    var count: usize = 0;
    var y: usize = 0;
    while (y < range + from.y) : (y += 1) {
        var x: usize = 0;
        while (x < range + from.x) : (x += 1) {
            var to = Pos{ .x = x, .y = y };
            const path = c.ASPathCreate(&path_node_source, favorite_number, from, &to);
            defer c.ASPathDestroy(path);

            const distance = c.ASPathGetCount(path);
            if (distance > 0 and distance <= range + 1) {
                count += 1;
            }
        }
    }
    return count;
}
