const std = @import("std");
const Md5 = std.crypto.hash.Md5;
const testing = std.testing;
const fmt = std.fmt;
const mem = std.mem;

pub fn main() anyerror!void {
    const input = "ugkcyxxp";
    var password: [8]u8 = undefined;
    try crackPassword(input, &password, buildPasswordPart1);
    std.debug.print("Part 1: {s}\n", .{password});
    try crackPassword(input, &password, buildPasswordPart2);
    std.debug.print("Part 2: {s}\n", .{password});
}

fn buildPasswordPart1(password: *[8]u8, hash: []const u8, index: *u4) void {
    password[index.*] = hash[5];
    index.* += 1;
}

fn buildPasswordPart2(password: *[8]u8, hash: []const u8, index: *u4) void {
    const position = fmt.parseUnsigned(u4, hash[5..6], 10) catch null;
    if (position) |pos| {
        if (pos < 8 and password[pos] == 0) {
            password[pos] = hash[6];
            index.* += 1;
        }
    }
}

fn crackPassword(
    input: []const u8,
    password: *[8]u8,
    buildPasswordFn: fn (password: *[8]u8, hash: []const u8, index: *u4) void,
) !void {
    for (password) |*c| {
        c.* = 0;
    }
    var buf: [100]u8 = undefined;
    var output: [Md5.digest_length]u8 = undefined;
    var password_index: u4 = 0;
    var i: usize = 0;
    while (password_index < 8) : (i += 1) {
        {
            const result = try fmt.bufPrint(&buf, "{s}{}", .{ input, i });
            Md5.hash(result, &output, .{});
        }
        const hash = try fmt.bufPrint(&buf, "{x}", .{output});
        if (mem.eql(u8, hash[0..5], "00000")) {
            buildPasswordFn(password, hash, &password_index);
        }
    }
}

test "crackPasswordPart1" {
    const input = "abc";
    var password: [8]u8 = undefined;
    try crackPassword(input, &password, buildPasswordPart1);
    testing.expectEqualStrings("18f47a30", &password);
}

test "crackPasswordPart2" {
    const input = "abc";
    var password: [8]u8 = undefined;
    try crackPassword(input, &password, buildPasswordPart2);
    testing.expectEqualStrings("05ace8e3", &password);
}
