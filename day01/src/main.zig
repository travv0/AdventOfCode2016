const std = @import("std");
const util = @import("util.zig");
const fs = std.fs;
const mem = std.mem;
const File = fs.File;
const ArrayList = std.ArrayList;
const Allocator = mem.Allocator;
const print = std.debug.print;
const fmt = std.fmt;
const assert = std.debug.assert;
const math = std.math;

const Turn = struct {
    direction: TurnDir,
    distance: u8,
};

const TurnDir = enum { R, L };

const Direction = enum {
    North,
    East,
    South,
    West,

    fn turn(self: Direction, dir: TurnDir) Direction {
        const len = @typeInfo(Direction).Enum.fields.len;
        return @intToEnum(Direction, @intCast(u2, switch (dir) {
            .R => (@intCast(u4, @enumToInt(self)) + 1) % len,
            .L => (@intCast(u4, @enumToInt(self)) + len - 1) % len,
        }));
    }

    test "turn" {
        assert(Direction.North == Direction.West.turn(TurnDir.R));
        assert(Direction.West == Direction.North.turn(TurnDir.L));
        assert(Direction.East == Direction.South.turn(TurnDir.L));
    }
};

const Coords = struct {
    x: i16,
    y: i16,
};

pub fn main() anyerror!void {
    const allocator = std.heap.page_allocator;
    const buf = try util.readFileIntoString(allocator, "input.txt", 1024);
    defer allocator.free(buf);
    const path = try makePath(allocator, buf);
    defer allocator.free(path);

    const final_pos = path[path.len - 1];
    print("Part 1: {}\n", .{try manhattanDistance(.{ .x = 0, .y = 0 }, final_pos)});

    const seen_twice_pos = try findFirstPosVisitedTwice(allocator, path);
    print("Part 2: {}\n", .{try manhattanDistance(.{ .x = 0, .y = 0 }, seen_twice_pos)});
}

fn parseInput(allocator: *Allocator, input: []const u8) ![]Turn {
    var parts = mem.split(fmt.trim(input), ", ");
    var turns = ArrayList(Turn).init(allocator);
    errdefer turns.deinit();

    while (parts.next()) |part| {
        const dir: TurnDir = if (part[0] == 'R') .R else .L;
        const dist = std.fmt.parseUnsigned(u8, part[1..], 10) catch |err| {
            util.exitWithError(err, "couldn't parse '{}' as number", .{part[1..]});
        };
        try turns.append(.{ .direction = dir, .distance = dist });
    }

    return turns.items;
}

test "parseInput" {
    const turns = try parseInput(std.testing.allocator, "R1, L2, R123");
    defer std.testing.allocator.free(turns);

    assert(turns[0].direction == .R);
    assert(turns[0].distance == 1);
    assert(turns[1].direction == .L);
    assert(turns[1].distance == 2);
    assert(turns[2].direction == .R);
    assert(turns[2].distance == 123);
}

fn makePath(allocator: *Allocator, input: []const u8) ![]Coords {
    const turns = try parseInput(allocator, input);
    defer allocator.free(turns);
    var current_pos = Coords{ .x = 0, .y = 0 };
    var path = ArrayList(Coords).init(allocator);
    errdefer path.deinit();
    try path.append(current_pos);
    var facing = Direction.North;

    for (turns) |turn| {
        facing = facing.turn(turn.direction);
        var i: u8 = 0;
        while (i < turn.distance) : (i += 1) {
            switch (facing) {
                .North => current_pos.y -= 1,
                .East => current_pos.x += 1,
                .South => current_pos.y += 1,
                .West => current_pos.x -= 1,
            }
            try path.append(current_pos);
        }
    }

    return path.items;
}

test "makePath" {
    const path = try makePath(std.testing.allocator, "R1, L2, R3");
    defer std.testing.allocator.free(path);
    const expected_path = [_]Coords{
        .{ .x = 0, .y = 0 },
        .{ .x = 1, .y = 0 },
        .{ .x = 1, .y = -1 },
        .{ .x = 1, .y = -2 },
        .{ .x = 2, .y = -2 },
        .{ .x = 3, .y = -2 },
        .{ .x = 4, .y = -2 },
    };

    assert(path.len == expected_path.len);
    for (path) |coords, i| {
        assert(coords.x == expected_path[i].x);
        assert(coords.y == expected_path[i].y);
    }
}

fn manhattanDistance(p1: Coords, p2: Coords) !i16 {
    return (try math.absInt(p1.y - p2.y)) + (try math.absInt(p1.x - p2.x));
}

test "manhattanDistance" {
    assert(12 == try manhattanDistance(.{ .x = 8, .y = 7 }, .{ .x = 1, .y = 2 }));
    assert(13 == try manhattanDistance(.{ .x = 9, .y = 9 }, .{ .x = 10, .y = 21 }));
}

fn findFirstPosVisitedTwice(allocator: *Allocator, path: []const Coords) !Coords {
    var positions = ArrayList(Coords).init(allocator);
    defer positions.deinit();

    for (path) |pos| {
        for (positions.items) |seen_pos| {
            if (pos.x == seen_pos.x and pos.y == seen_pos.y) {
                return pos;
            }
        }
        try positions.append(pos);
    }
    return error.NoCoordsSeenTwice;
}

test "findFirstPosVisitedTwice" {
    const path = try makePath(std.testing.allocator, "R8, R4, R4, R8");
    defer std.testing.allocator.free(path);
    const pos = (try findFirstPosVisitedTwice(std.testing.allocator, path)).?;
    assert(pos.x == 4 and pos.y == 0);
}
