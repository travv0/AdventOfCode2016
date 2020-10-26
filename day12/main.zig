const std = @import("std");
const util = @import("util");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const AutoHashMap = std.hash_map.AutoHashMap;
const fmt = std.fmt;
const mem = std.mem;
const expectEqual = std.testing.expectEqual;

pub fn main() anyerror!void {}

const Value = union(enum) {
    register: u8,
    integer: isize,
};

const Cpy = struct {
    from: Value,
    to: u8,
};

const Jnz = struct {
    check: Value,
    jump: isize,
};

const Command = union(enum) {
    cpy: Cpy,
    inc: u8,
    dec: u8,
    jnz: Jnz,
};

fn handleCommand(commands: []*Command, i: usize) void {}

fn parseCommands(allocator: *Allocator, input: []const u8) ![]Command {
    var commands = ArrayList(Command).init(allocator);
    errdefer commands.deinit();
    var lines = mem.split(input, "\n");
    while (lines.next()) |line| {
        const words = try util.split(allocator, line, " ");
        defer allocator.free(words);
        var command: Command = undefined;
        if (mem.eql(u8, words[0], "cpy")) {
            command = .{
                .cpy = .{
                    .from = if (fmt.parseUnsigned(isize, words[1], 10)) |int|
                        .{ .integer = int }
                    else |_|
                        .{ .register = words[1][0] },
                    .to = words[2][0],
                },
            };
        } else if (mem.eql(u8, words[0], "inc")) {
            command = .{ .inc = words[1][0] };
        } else if (mem.eql(u8, words[0], "dec")) {
            command = .{ .dec = words[1][0] };
        } else if (mem.eql(u8, words[0], "jnz")) {
            command = .{
                .jnz = .{
                    .check = if (fmt.parseUnsigned(isize, words[1], 10)) |int|
                        .{ .integer = int }
                    else |_|
                        .{ .register = words[1][0] },
                    .jump = try fmt.parseUnsigned(isize, words[2], 10),
                },
            };
        } else {
            return error.InvalidCommand;
        }
        try commands.append(command);
    }
    return commands.toOwnedSlice();
}

test "parseCommands" {
    const allocator = std.testing.allocator;
    const input =
        \\cpy 41 a
        \\inc a
        \\inc a
        \\dec a
        \\jnz a 2
        \\dec a
    ;
    const commands = try parseCommands(allocator, input);
    defer allocator.free(commands);
    expectEqual(@as(isize, 41), commands[0].cpy.from.integer);
    expectEqual(@as(u8, 'a'), commands[0].cpy.to);
    expectEqual(@as(u8, 'a'), commands[1].inc);
    expectEqual(@as(u8, 'a'), commands[2].inc);
    expectEqual(@as(u8, 'a'), commands[3].dec);
    expectEqual(@as(u8, 'a'), commands[4].jnz.check.register);
    expectEqual(@as(isize, 2), commands[4].jnz.jump);
    expectEqual(@as(u8, 'a'), commands[5].dec);
}
