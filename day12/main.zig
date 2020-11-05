const std = @import("std");
const util = @import("util");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const AutoHashMap = std.hash_map.AutoHashMap;
const fmt = std.fmt;
const mem = std.mem;
const expectEqual = std.testing.expectEqual;

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;
    const input = try util.readInput(allocator, 1024 * 1024);
    defer allocator.free(input);
    const part1_result = try runProgram(allocator, input, false);
    std.debug.print("Part 1: {}\n", .{part1_result});
    const part2_result = try runProgram(allocator, input, true);
    std.debug.print("Part 2: {}\n", .{part2_result});
}

const Value = union(enum) {
    register: u8,
    integer: isize,

    fn resolve(self: @This(), registers: AutoHashMap(u8, isize)) !isize {
        return switch (self) {
            .integer => |i| i,
            .register => |reg| registers.get(reg) orelse error.RegisterEmpty,
        };
    }
};

const Command = union(enum) {
    cpy: struct { from: Value, to: u8 },
    inc: u8,
    dec: u8,
    jnz: struct { check: Value, jump: isize },
};

fn handleCommand(commands: []Command, registers: *AutoHashMap(u8, isize), i: usize) !usize {
    const command = commands[i];
    switch (command) {
        .cpy => |cpy| {
            try registers.put(cpy.to, try cpy.from.resolve(registers.*));
        },
        .inc => |reg| {
            const entry = registers.getEntry(reg) orelse return error.RegisterEmpty;
            entry.value += 1;
        },
        .dec => |reg| {
            const entry = registers.getEntry(reg) orelse return error.RegisterEmpty;
            entry.value -= 1;
        },
        .jnz => |jnz| {
            if ((try jnz.check.resolve(registers.*)) != 0) {
                return @intCast(usize, @intCast(isize, i) + jnz.jump);
            }
        },
    }
    return i + 1;
}

test "runProgram" {
    const allocator = std.testing.allocator;
    const input =
        \\cpy 41 a
        \\inc a
        \\inc a
        \\dec a
        \\jnz a 2
        \\dec a
    ;

    const result = try runProgram(allocator, input, false);
    expectEqual(@as(isize, 42), result);
}

fn runProgram(allocator: *Allocator, input: []const u8, ignite: bool) !isize {
    const commands = try parseCommands(allocator, input);
    defer allocator.free(commands);

    var registers = AutoHashMap(u8, isize).init(allocator);
    defer registers.deinit();

    try registers.put('a', 0);
    try registers.put('b', 0);
    try registers.put('c', if (ignite) 1 else 0);
    try registers.put('d', 0);

    var i: usize = 0;
    while (i < commands.len) {
        i = try handleCommand(commands, &registers, i);
    }
    return registers.get('a') orelse error.RegisterEmpty;
}

fn parseCommands(allocator: *Allocator, input: []const u8) ![]Command {
    var commands = ArrayList(Command).init(allocator);
    errdefer commands.deinit();
    var lines = mem.split(util.trim(input), "\n");
    while (lines.next()) |line| {
        const words = try util.split(allocator, line, " ");
        defer allocator.free(words);
        var command: Command = undefined;
        if (mem.eql(u8, words[0], "cpy")) {
            command = .{
                .cpy = .{
                    .from = if (fmt.parseInt(isize, words[1], 10)) |int|
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
                    .check = if (fmt.parseInt(isize, words[1], 10)) |int|
                        .{ .integer = int }
                    else |_|
                        .{ .register = words[1][0] },
                    .jump = try fmt.parseInt(isize, words[2], 10),
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
