const std = @import("std");
const util = @import("util");
const SplitIterator = std.mem.SplitIterator;

const Triangle = struct { x: u16, y: u16, z: u16 };

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;
    const input = try util.readInput(allocator, 1024 * 100);
    defer allocator.free(input);

    const part_one_result = partOne(input);
    std.debug.print("Part 1: {}\n", .{part_one_result});

    const part_two_result = try partTwo(input);
    std.debug.print("Part 2: {}\n", .{part_two_result});
}

fn partOne(input: []const u8) u16 {
    var lines = std.mem.split(util.trim(input), "\n");
    var triangles: u16 = 0;

    while (lines.next()) |line| {
        const x = util.trim(line[0..5]);
        const y = util.trim(line[5..10]);
        const z = util.trim(line[10..]);
        const triangle = .{
            .x = parseSide(x),
            .y = parseSide(y),
            .z = parseSide(z),
        };
        if (isTriangle(triangle)) triangles += 1;
    }

    return triangles;
}

fn partTwo(input: []const u8) !u16 {
    var lines = std.mem.split(util.trim(input), "\n");
    var triangles: u16 = 0;
    const sides = 3;

    while (lines.rest().len > 0) {
        var x: [sides][]const u8 = undefined;
        var y: [sides][]const u8 = undefined;
        var z: [sides][]const u8 = undefined;
        try populateSides(&x, &lines);
        try populateSides(&y, &lines);
        try populateSides(&z, &lines);
        var i: u2 = 0;
        while (i < sides) : (i += 1) {
            const triangle = .{
                .x = parseSide(x[i]),
                .y = parseSide(y[i]),
                .z = parseSide(z[i]),
            };
            if (isTriangle(triangle)) triangles += 1;
        }
    }

    return triangles;
}

fn isTriangle(triangle: Triangle) bool {
    if (triangle.x + triangle.y > triangle.z and
        triangle.y + triangle.z > triangle.x and
        triangle.x + triangle.z > triangle.y)
    {
        return true;
    }
    return false;
}

fn populateSides(sides: *[3][]const u8, lines: *SplitIterator) !void {
    const line = lines.next() orelse return error.WrongNumberOfLines;
    sides[0] = util.trim(line[0..5]);
    sides[1] = util.trim(line[5..10]);
    sides[2] = util.trim(line[10..]);
}

fn parseSide(side: []const u8) u16 {
    return std.fmt.parseUnsigned(u16, side, 10) catch |err| {
        util.exitWithError(err, "couldn't parse '{}'", .{side});
    };
}
