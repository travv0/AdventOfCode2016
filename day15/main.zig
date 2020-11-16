const std = @import("std");
const util = @import("util");
const it = @import("ziter");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const fmt = std.fmt;
const mem = std.mem;
const testing = std.testing;

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;
    const input = try util.readInput(allocator, 1024 * 1024);
    const discs = try parseInput(allocator, input);
    std.debug.print("Part 1: {}\n", .{try getWinningTime(1, allocator, discs)});
    std.debug.print("Part 2: {}\n", .{try getWinningTime(2, allocator, discs)});
}

const Disc = struct {
    position: usize,
    positions: usize,
};

fn parseLine(allocator: *Allocator, line: []const u8) !Disc {
    const parts = try util.split(allocator, line, " ");
    defer allocator.free(parts);
    return Disc{
        .position = try fmt.parseUnsigned(usize, parts[11][0 .. parts[11].len - 1], 10),
        .positions = try fmt.parseUnsigned(usize, parts[3], 10),
    };
}

fn parseInput(allocator: *Allocator, input: []const u8) ![]Disc {
    const lines = try util.split(allocator, util.trim(input), "\n");
    defer allocator.free(lines);
    return try it.span(lines) //
        .call(it.map_ex, .{ allocator, parseLine }) //
        .call(it.unwrap, .{}) //
        .call(it.collect, .{allocator});
}

fn getWinningTime(comptime part: usize, allocator: *Allocator, discs: []Disc) !usize {
    if (part != 1 and part != 2) @compileError("part must be 1 or 2");

    const ds = ds: {
        if (part == 1) {
            break :ds discs;
        } else {
            var ds = try allocator.alloc(Disc, discs.len + 1);
            mem.copy(Disc, ds, discs);
            ds[discs.len] = .{ .position = 0, .positions = 11 };
            break :ds ds;
        }
    };
    defer if (part == 2) allocator.free(ds);

    var start_time: usize = 0;
    outer: while (true) : (start_time += 1) {
        var time = start_time + 1;
        var disc_index: usize = 0;
        while (disc_index < ds.len) : ({
            time += 1;
            disc_index += 1;
        }) {
            const disc = ds[disc_index];
            if ((disc.position + time) % disc.positions != 0)
                continue :outer;
        }
        return start_time;
    }
}

test "getWinningTime" {
    const input =
        \\Disc #1 has 5 positions; at time=0, it is at position 4.
        \\Disc #2 has 2 positions; at time=0, it is at position 1.
    ;
    const discs = try parseInput(testing.allocator, input);
    defer testing.allocator.free(discs);
    testing.expectEqual(@as(usize, 5), try getWinningTime(1, testing.allocator, discs));
}
