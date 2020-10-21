const std = @import("std");
const fs = std.fs;
const Allocator = std.mem.Allocator;

pub fn readFileIntoString(allocator: *Allocator, path: []const u8, max_bytes: usize) ![]u8 {
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
