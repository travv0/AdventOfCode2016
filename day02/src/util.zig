const std = @import("std");
const fs = std.fs;
const Allocator = std.mem.Allocator;

pub fn readFileIntoString(allocator: *Allocator, path: []const u8, max_bytes: usize) ![]u8 {
    const file = try fs.cwd().openFile("input.txt", .{ .read = true });
    defer file.close();

    return try file.readToEndAlloc(allocator, max_bytes);
}