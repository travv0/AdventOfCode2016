const std = @import("std");
const util = @import("util");
const mem = std.mem;
const fmt = std.fmt;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;
    var screen = try Screen.init(allocator, 50, 6);
    defer screen.deinit();

    const input = try util.readInput(allocator, 1024 * 1024);
    defer allocator.free(input);
    var lines = mem.split(util.trim(input), "\n");
    while (lines.next()) |line| {
        try screen.runCommand(line);
    }

    std.debug.print("Part 1: {}\n", .{screen.litPixelCount()});
    std.debug.print("Part 2:\n", .{});
    screen.print();
}

const Screen = struct {
    const Self = @This();

    pixels: [][]bool,
    arena: ArenaAllocator,

    fn init(allocator: *Allocator, width: usize, height: usize) !Self {
        var arena = std.heap.ArenaAllocator.init(allocator);
        var pixels = try arena.allocator.alloc([]bool, height);
        for (pixels) |*row| {
            row.* = try arena.allocator.alloc(bool, width);
            for (row.*) |*pixel| {
                pixel.* = false;
            }
        }
        return Screen{
            .pixels = pixels,
            .arena = arena,
        };
    }

    fn deinit(self: *Self) void {
        self.arena.deinit();
        self.* = undefined;
    }

    fn print(self: Self) void {
        for (self.pixels) |row| {
            for (row) |pixel| {
                const c: u8 = if (pixel) '#' else '.';
                std.debug.print("{c}", .{c});
            }
            std.debug.print("\n", .{});
        }
    }

    fn rect(self: *Self, width: usize, height: usize) void {
        var x: usize = 0;
        while (x < width) : (x += 1) {
            var y: usize = 0;
            while (y < height) : (y += 1) {
                self.pixels[y][x] = true;
            }
        }
    }

    fn rotateColumn(self: *Self, x: usize, by: usize) void {
        const height = self.pixels.len;
        var i: usize = 0;
        while (i < by) : (i += 1) {
            var lastVal = self.pixels[height - 1][x];
            var y: usize = 0;
            while (y < height) : (y += 1) {
                const temp = self.pixels[y][x];
                self.pixels[y][x] = lastVal;
                lastVal = temp;
            }
        }
    }

    fn rotateRow(self: *Self, y: usize, by: usize) void {
        const width = self.pixels[0].len;
        var i: usize = 0;
        while (i < by) : (i += 1) {
            var lastVal = self.pixels[y][width - 1];
            var x: usize = 0;
            while (x < width) : (x += 1) {
                const temp = self.pixels[y][x];
                self.pixels[y][x] = lastVal;
                lastVal = temp;
            }
        }
    }

    fn runCommand(self: *Self, line: []const u8) !void {
        var words = mem.split(line, " ");
        const command = words.next() orelse return error.InvalidCommand;
        if (mem.eql(u8, command, "rect")) {
            try self.runRect(words.next() orelse return error.InvalidCommand);
        } else if (mem.eql(u8, command, "rotate")) {
            try self.runRotate(&words);
        } else {
            return error.InvalidCommand;
        }
    }

    fn runRect(self: *Self, size: []const u8) !void {
        var sizeSplit = mem.split(size, "x");
        const width = try fmt.parseUnsigned(usize, sizeSplit.next() orelse return error.InvalidCommand, 10);
        const height = try fmt.parseUnsigned(usize, sizeSplit.next() orelse return error.InvalidCommand, 10);
        self.rect(width, height);
    }

    fn runRotate(self: *Self, iter: *mem.SplitIterator) !void {
        const dir = iter.next() orelse return error.InvalidCommand;
        const coord_eql = iter.next() orelse return error.InvalidCommand;
        var coord_split = mem.split(coord_eql, "=");
        _ = coord_split.next();
        const coord = try fmt.parseUnsigned(usize, coord_split.next() orelse return error.InvalidCommand, 10);
        _ = iter.next();
        const by = try fmt.parseUnsigned(usize, iter.next() orelse return error.InvalidCommand, 10);
        if (mem.eql(u8, dir, "row")) {
            self.rotateRow(coord, by);
        } else if (mem.eql(u8, dir, "column")) {
            self.rotateColumn(coord, by);
        } else {
            return error.InvalidCommand;
        }
    }

    fn litPixelCount(self: Self) usize {
        var count: usize = 0;
        for (self.pixels) |row| {
            for (row) |pixel| {
                if (pixel) count += 1;
            }
        }
        return count;
    }
};

test "exampleTest" {
    const allocator = std.testing.allocator;
    var screen = try Screen.init(allocator, 7, 3);
    defer screen.deinit();

    const input =
        \\rect 3x2
        \\rotate column x=1 by 1
        \\rotate row y=0 by 4
        \\rotate column x=1 by 1
    ;
    var lines = mem.split(util.trim(input), "\n");
    while (lines.next()) |line| {
        try screen.runCommand(line);
    }
    std.testing.expectEqualSlices(bool, &[_]bool{ false, true, false, false, true, false, true }, screen.pixels[0]);
    std.testing.expectEqualSlices(bool, &[_]bool{ true, false, true, false, false, false, false }, screen.pixels[1]);
    std.testing.expectEqualSlices(bool, &[_]bool{ false, true, false, false, false, false, false }, screen.pixels[2]);

    std.testing.expectEqual(@as(usize, 6), screen.litPixelCount());
}
