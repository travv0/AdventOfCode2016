const std = @import("std");
const util = @import("util");
const it = @import("ziter");
const fs = std.fs;
const mem = std.mem;
const File = fs.File;
const ArrayList = std.ArrayList;
const Allocator = mem.Allocator;
const print = std.debug.print;
const fmt = std.fmt;
const expectEqual = std.testing.expectEqual;
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

    fn turn(self: *Direction, dir: TurnDir) void {
        const len = @typeInfo(Direction).Enum.fields.len;
        const dir_int = @as(u4, @enumToInt(self.*));
        self.* = @intToEnum(Direction, @intCast(u2, switch (dir) {
            .R => (dir_int + 1) % len,
            .L => (dir_int + len - 1) % len,
        }));
    }

    test "turn" {
        var dir = Direction.West;
        dir.turn(TurnDir.R);
        expectEqual(Direction.North, dir);
        dir.turn(TurnDir.L);
        expectEqual(Direction.West, dir);
        dir.turn(TurnDir.L);
        expectEqual(Direction.South, dir);
    }
};

const Coords = struct {
    const Self = @This();

    x: i16,
    y: i16,

    fn eql(self: Self, other: Self) bool {
        return self.x == other.x and self.y == other.y;
    }
};

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;
    const buf = try util.readInput(allocator, 1024);
    defer allocator.free(buf);
    const path = try makePath(allocator, buf);
    defer allocator.free(path);

    const final_pos = path[path.len - 1];
    print("Part 1: {}\n", .{try manhattanDistance(.{ .x = 0, .y = 0 }, final_pos)});

    const seen_twice_pos = try findFirstPosVisitedTwice(allocator, path);
    print("Part 2: {}\n", .{try manhattanDistance(.{ .x = 0, .y = 0 }, seen_twice_pos)});
}

fn parseInput(allocator: *Allocator, input: []const u8) ![]Turn {
    var parts = mem.split(util.trim(input), ", ");
    var turns = ArrayList(Turn).init(allocator);
    errdefer turns.deinit();

    while (parts.next()) |part| {
        const dir: TurnDir = if (part[0] == 'R') .R else .L;
        const dist = fmt.parseUnsigned(u8, part[1..], 10) catch |err| {
            util.exitWithError(err, "couldn't parse '{}' as number", .{part[1..]});
        };
        try turns.append(.{ .direction = dir, .distance = dist });
    }

    return turns.toOwnedSlice();
}

test "parseInput" {
    const turns = try parseInput(std.testing.allocator, "R1, L2, R123");
    defer std.testing.allocator.free(turns);

    expectEqual(TurnDir.R, turns[0].direction);
    expectEqual(@as(u8, 1), turns[0].distance);
    expectEqual(TurnDir.L, turns[1].direction);
    expectEqual(@as(u8, 2), turns[1].distance);
    expectEqual(TurnDir.R, turns[2].direction);
    expectEqual(@as(u8, 123), turns[2].distance);
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
        facing.turn(turn.direction);
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

    return path.toOwnedSlice();
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

    expectEqual(expected_path.len, path.len);
    for (path) |coords, i| {
        expectEqual(expected_path[i].x, coords.x);
        expectEqual(expected_path[i].y, coords.y);
    }
}

fn manhattanDistance(p1: Coords, p2: Coords) !i16 {
    return (try math.absInt(p1.y - p2.y)) + (try math.absInt(p1.x - p2.x));
}

test "manhattanDistance" {
    expectEqual(@as(i16, 12), try manhattanDistance(.{ .x = 8, .y = 7 }, .{ .x = 1, .y = 2 }));
    expectEqual(@as(i16, 13), try manhattanDistance(.{ .x = 9, .y = 9 }, .{ .x = 10, .y = 21 }));
}

fn findFirstPosVisitedTwice(allocator: *Allocator, path: []const Coords) !Coords {
    var seen_positions = ArrayList(Coords).init(allocator);
    defer seen_positions.deinit();

    for (path) |pos| {
        if (it.span(seen_positions.items) //
            .call(it.any_ex, .{ pos, Coords.eql }))
        {
            return pos;
        }
        try seen_positions.append(pos);
    }
    return error.NoCoordsSeenTwice;
}

test "findFirstPosVisitedTwice" {
    const path = try makePath(std.testing.allocator, "R8, R4, R4, R8");
    defer std.testing.allocator.free(path);
    const pos = try findFirstPosVisitedTwice(std.testing.allocator, path);
    expectEqual(@as(i16, 4), pos.x);
    expectEqual(@as(i16, 0), pos.y);
}
