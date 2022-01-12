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
    const allocator = gpa.allocator();
    const input = try util.readInput(allocator, 1024 * 1024);
    defer allocator.free(input);
    {
        const result = try runProgram(allocator, input, 7);
        std.debug.print("Part 1: {}\n", .{result});
    }
    {
        const result = try runProgram(allocator, input, 12);
        std.debug.print("Part 2: {}\n", .{result});
    }
}

const Value = union(enum) {
    register: u8,
    integer: isize,

    fn create(str: []const u8) Value {
        return if (fmt.parseInt(isize, str, 10)) |int| .{ .integer = int } else |_| .{ .register = str[0] };
    }

    fn resolve(self: Value, registers: AutoHashMap(u8, isize)) !isize {
        return switch (self) {
            .integer => |i| i,
            .register => |reg| registers.get(reg) orelse error.RegisterEmpty,
        };
    }
};

const TwoArgs = struct { a: Value, b: Value };

const Command = union(enum) {
    cpy: TwoArgs,
    inc: Value,
    dec: Value,
    jnz: TwoArgs,
    tgl: Value,
};

fn handleCommand(commands: []Command, registers: *AutoHashMap(u8, isize), i: usize) !usize {
    const command = commands[i];
    switch (command) {
        .cpy => |cpy| {
            switch (cpy.b) {
                .register => |reg| try registers.put(reg, try cpy.a.resolve(registers.*)),
                else => {},
            }
        },
        .inc => |reg| {
            const entry = registers.getEntry(reg.register) orelse
                return error.RegisterEmpty;
            entry.value += 1;
        },
        .dec => |reg| {
            const entry = registers.getEntry(reg.register) orelse
                return error.RegisterEmpty;
            entry.value -= 1;
        },
        .jnz => |jnz| {
            if ((try jnz.a.resolve(registers.*)) != 0) {
                return @intCast(usize, @intCast(isize, i) + try jnz.b.resolve(registers.*));
            }
        },
        .tgl => |tgl| {
            const offset = try tgl.resolve(registers.*);
            const new_i = @intCast(usize, @intCast(isize, i) + offset);
            if (0 <= new_i and new_i < commands.len) {
                switch (commands[new_i]) {
                    .inc => |args| commands[new_i] = .{ .dec = args },
                    .dec, .tgl => |args| commands[new_i] = .{ .inc = args },
                    .jnz => |args| commands[new_i] = .{ .cpy = args },
                    .cpy => |args| commands[new_i] = .{ .jnz = args },
                }
            }
        },
    }
    return i + 1;
}

test "runProgram" {
    const allocator = std.testing.allocator;
    const input =
        \\cpy 2 a
        \\tgl a
        \\tgl a
        \\tgl a
        \\cpy 1 a
        \\dec a
        \\dec a
    ;

    const result = try runProgram(allocator, input, 7);
    try expectEqual(@as(isize, 3), result);
}

fn runProgram(allocator: Allocator, input: []const u8, initial_val: isize) !isize {
    const commands = try parseCommands(allocator, input);
    defer allocator.free(commands);

    var registers = AutoHashMap(u8, isize).init(allocator);
    defer registers.deinit();

    try registers.put('a', initial_val);
    try registers.put('b', 0);
    try registers.put('c', 0);
    try registers.put('d', 0);

    var i: usize = 0;
    while (i < commands.len) {
        i = try handleCommand(commands, &registers, i);
    }
    return registers.get('a') orelse error.RegisterEmpty;
}

fn parseCommands(allocator: Allocator, input: []const u8) ![]Command {
    var commands = ArrayList(Command).init(allocator);
    errdefer commands.deinit();
    var lines = mem.split(u8, util.trim(input), "\n");
    while (lines.next()) |line| {
        const words = try util.split(allocator, util.trim(line), " ");
        defer allocator.free(words);
        var command: Command = undefined;
        if (mem.eql(u8, words[0], "cpy")) {
            command = .{
                .cpy = .{
                    .a = Value.create(words[1]),
                    .b = Value.create(words[2]),
                },
            };
        } else if (mem.eql(u8, words[0], "inc")) {
            command = .{ .inc = .{ .register = words[1][0] } };
        } else if (mem.eql(u8, words[0], "dec")) {
            command = .{ .dec = .{ .register = words[1][0] } };
        } else if (mem.eql(u8, words[0], "jnz")) {
            command = .{
                .jnz = .{
                    .a = Value.create(words[1]),
                    .b = Value.create(words[2]),
                },
            };
        } else if (mem.eql(u8, words[0], "tgl")) {
            command = .{ .tgl = Value.create(words[1]) };
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
    try expectEqual(@as(isize, 41), commands[0].cpy.a.integer);
    try expectEqual(@as(u8, 'a'), commands[0].cpy.b.register);
    try expectEqual(@as(u8, 'a'), commands[1].inc.register);
    try expectEqual(@as(u8, 'a'), commands[2].inc.register);
    try expectEqual(@as(u8, 'a'), commands[3].dec.register);
    try expectEqual(@as(u8, 'a'), commands[4].jnz.a.register);
    try expectEqual(@as(isize, 2), commands[4].jnz.b.integer);
    try expectEqual(@as(u8, 'a'), commands[5].dec.register);
}
