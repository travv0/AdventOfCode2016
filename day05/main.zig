const std = @import("std");
const Md5 = std.crypto.hash.Md5;
const testing = std.testing;
const fmt = std.fmt;
const mem = std.mem;

pub fn main() anyerror!void {
    const input = "ugkcyxxp";
    var output: [Md5.digest_length]u8 = undefined;
    var password: [8]u8 = undefined;
    try crackPasswordPart1(input, &password);
    std.debug.print("Part 1: {}\n", .{password});
    try crackPasswordPart2(input, &password);
    std.debug.print("Part 2: {}\n", .{password});
}

fn crackPasswordPart1(input: []const u8, password: *[8]u8) !void {
    var buf: [100]u8 = undefined;
    var output: [Md5.digest_length]u8 = undefined;
    var password_index: u4 = 0;
    var i: usize = 0;
    while (password_index < 8) : (i += 1) {
        {
            const result = try fmt.bufPrint(&buf, "{}{}", .{ input, i });
            Md5.hash(result, &output, .{});
        }
        const hash = try fmt.bufPrint(&buf, "{x}", .{output});
        if (mem.eql(u8, hash[0..5], "00000")) {
            password[password_index] = hash[5];
            password_index += 1;
        }
    }
}

test "crackPasswordPart1" {
    const input = "abc";
    var password: [8]u8 = undefined;
    try crackPasswordPart1(input, &password);
    testing.expectEqualStrings("18f47a30", &password);
}

fn crackPasswordPart2(input: []const u8, password: *[8]u8) !void {
    for (password) |*c| {
        c.* = 0;
    }
    var buf: [100]u8 = undefined;
    var output: [Md5.digest_length]u8 = undefined;
    var password_fill_count: u4 = 0;
    var i: usize = 0;
    while (password_fill_count < 8) : (i += 1) {
        {
            const result = try fmt.bufPrint(&buf, "{}{}", .{ input, i });
            Md5.hash(result, &output, .{});
        }
        const hash = try fmt.bufPrint(&buf, "{x}", .{output});
        if (mem.eql(u8, hash[0..5], "00000")) {
            const position = fmt.parseUnsigned(u4, hash[5..6], 10) catch null;
            if (position) |pos| {
                if (pos < 8 and password[pos] == 0) {
                    password[pos] = hash[6];
                    password_fill_count += 1;
                }
            }
        }
    }
}

test "crackPasswordPart2" {
    const input = "abc";
    var password: [8]u8 = undefined;
    try crackPasswordPart2(input, &password);
    testing.expectEqualStrings("05ace8e3", &password);
}
