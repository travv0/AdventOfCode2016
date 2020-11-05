const std = @import("std");
const fs = std.fs;
const mem = std.mem;
const testing = std.testing;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub fn readFileIntoString(allocator: *Allocator, path: []const u8, max_bytes: usize) ![]const u8 {
    const file = try fs.cwd().openFile(path, .{ .read = true });
    defer file.close();

    return try file.readToEndAlloc(allocator, max_bytes);
}

pub fn exitWithError(err: anyerror, comptime format: []const u8, args: anytype) noreturn {
    if (@typeInfo(@TypeOf(args)) != .Struct) {
        @compileError("Expected tuple or struct argument, found " ++ @typeName(@TypeOf(args)));
    }
    std.log.err("{}: " ++ format, .{err} ++ args);
    std.os.exit(@intCast(u8, @errorToInt(err)));
}

pub fn getInputPath(allocator: *Allocator) ![]const u8 {
    var args = std.process.args();
    _ = args.skip();
    return try args.next(allocator) orelse "input.txt";
}

pub fn readInput(allocator: *Allocator, max_bytes: usize) ![]const u8 {
    var input_path = try getInputPath(allocator);
    defer allocator.free(input_path);
    return try readFileIntoString(allocator, input_path, max_bytes);
}

pub fn split(allocator: *Allocator, buffer: []const u8, delimiter: []const u8) ![][]const u8 {
    var result = ArrayList([]const u8).init(allocator);
    errdefer result.deinit();

    var iter = mem.split(buffer, delimiter);
    while (iter.next()) |text| {
        try result.append(text);
    }
    return result.toOwnedSlice();
}

pub fn trim(str: []const u8) []const u8 {
    return mem.trim(u8, str, &std.ascii.spaces);
}

pub fn dbg(value: anytype) @TypeOf(value) {
    std.log.debug("{}", .{value});
    return value;
}
