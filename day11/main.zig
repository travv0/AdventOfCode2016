const std = @import("std");
const util = @import("util");
const AutoHashSet = @import("hashset").AutoHashSet;
const HashSet = @import("hashset").HashSet;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;
const LinearFifo = std.fifo.LinearFifo;
const mem = std.mem;
const testing = std.testing;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;
    const input = try util.readInput(allocator, 1024 * 1024);
    defer allocator.free(input);

    var state = State.init(allocator);
    defer state.deinit();
    try state.floors[0].put(.{ .element = "strontium", .kind = .generator });
    try state.floors[0].put(.{ .element = "strontium", .kind = .microchip });
    try state.floors[0].put(.{ .element = "plutonium", .kind = .generator });
    try state.floors[0].put(.{ .element = "plutonium", .kind = .microchip });
    try state.floors[1].put(.{ .element = "thulium", .kind = .generator });
    try state.floors[1].put(.{ .element = "ruthenium", .kind = .generator });
    try state.floors[1].put(.{ .element = "ruthenium", .kind = .microchip });
    try state.floors[1].put(.{ .element = "curium", .kind = .generator });
    try state.floors[1].put(.{ .element = "curium", .kind = .microchip });
    try state.floors[2].put(.{ .element = "thulium", .kind = .microchip });

    std.debug.print(
        "Part 1: {}\n",
        .{try numOfStepsToComplete(testing.allocator, &state)},
    );

    try state.floors[0].put(.{ .element = "elerium", .kind = .generator });
    try state.floors[0].put(.{ .element = "elerium", .kind = .microchip });
    try state.floors[0].put(.{ .element = "dilithium", .kind = .generator });
    try state.floors[0].put(.{ .element = "dilithium", .kind = .microchip });

    std.debug.print(
        "Part 2: {}\n",
        .{try numOfStepsToComplete(testing.allocator, &state)},
    );
}

const Kind = enum { generator, microchip };

const Object = struct {
    element: []const u8,
    kind: Kind,
};

const State = struct {
    const Self = @This();

    step: usize = 0,
    floors: [4]AutoHashSet(Object),
    elevator_floor: usize = 0,

    fn init(allocator: *Allocator) Self {
        var floors: [4]AutoHashSet(Object) = undefined;
        for (floors) |*floor|
            floor.* = AutoHashSet(Object).init(allocator);
        return Self{ .floors = floors };
    }

    fn deinit(self: *Self) void {
        for (self.floors) |*floor|
            floor.deinit();
    }

    fn isValid(self: Self) bool {
        for (self.floors) |floor| {
            var iter = floor.iterator();
            while (iter.next()) |object| {
                if (object.kind == .microchip) {
                    if (!floor.exists(.{ .kind = .generator, .element = object.element }) and
                        floor.count() > 1)
                    {
                        var inner_iter = floor.iterator();
                        while (inner_iter.next()) |other| {
                            if (other.kind == .generator)
                                return false;
                        }
                    }
                }
            }
        }
        return true;
    }

    fn isFinal(self: Self) bool {
        for (self.floors) |floor, i| {
            if (i != self.floors.len - 1 and floor.count() > 0)
                return false;
        }
        return true;
    }

    fn nextSteps(self: Self, allocator: *Allocator) ![]Self {
        var result = ArrayList(Self).init(allocator);
        var floor = self.floors[self.elevator_floor];
        var objects = ArrayList(Object).init(allocator);
        defer objects.deinit();

        {
            var iter = floor.iterator();
            while (iter.next()) |object| {
                try objects.append(object);
            }
        }

        var combos = ArrayList([]Object).init(allocator);
        defer combos.deinit();
        defer for (combos.items) |combo| allocator.free(combo);

        {
            var i: usize = 1;
            while (i <= 2 and i <= objects.items.len) : (i += 1) {
                try combos.appendSlice(try combinations(Object, allocator, objects.items, i));
            }
        }

        for (combos.items) |combo| {
            if (self.elevator_floor > 0) {
                var new_state = Self.init(allocator);
                new_state.step = self.step + 1;
                new_state.elevator_floor = self.elevator_floor - 1;
                for (self.floors) |f, i| {
                    var iter = f.iterator();
                    while (iter.next()) |obj| {
                        try new_state.floors[i].put(obj);
                    }
                }
                for (combo) |object| {
                    const obj = new_state.floors[self.elevator_floor].delete(object) orelse unreachable;
                    try new_state.floors[new_state.elevator_floor].put(obj);
                }
                if (new_state.isValid()) {
                    try result.append(new_state);
                }
            }
            if (self.elevator_floor < self.floors.len - 1) {
                var new_state = Self.init(allocator);
                new_state.step = self.step + 1;
                new_state.elevator_floor = self.elevator_floor + 1;
                for (self.floors) |f, i| {
                    var iter = f.iterator();
                    while (iter.next()) |obj| {
                        try new_state.floors[i].put(obj);
                    }
                }
                for (combo) |object| {
                    const obj = new_state.floors[self.elevator_floor].delete(object) orelse unreachable;
                    try new_state.floors[new_state.elevator_floor].put(obj);
                }
                if (new_state.isValid()) {
                    try result.append(new_state);
                }
            }
        }

        return result.toOwnedSlice();
    }
};

const CombosError = error{ OutOfMemory, ArrayTooShort };

fn combinations_(
    comptime T: type,
    allocator: *Allocator,
    arr: []const T,
    data: []T,
    result: *ArrayList([]T),
    index: usize,
    len: usize,
) CombosError!void {
    if (len > arr.len) return error.ArrayTooShort;
    if (len == 0) {
        try result.append(try allocator.dupe(T, data));
    } else {
        var i = index;
        while (i <= arr.len - len) : (i += 1) {
            data[data.len - len] = arr[i];
            try combinations_(T, allocator, arr, data, result, i + 1, len - 1);
        }
    }
}

fn combinations(
    comptime T: type,
    allocator: *Allocator,
    arr: []const T,
    len: usize,
) ![][]T {
    var result = ArrayList([]T).init(allocator);
    errdefer result.deinit();
    errdefer for (result.items) |item| allocator.free(item);
    var data = try allocator.alloc(T, len);
    defer allocator.free(data);
    try combinations_(T, allocator, arr, data, &result, 0, len);
    return result.toOwnedSlice();
}

test "combinations" {
    var chars = "abcde";
    {
        var combos = try combinations(u8, testing.allocator, chars, 3);
        defer testing.allocator.free(combos);
        defer for (combos) |combo| testing.allocator.free(combo);

        testing.expectEqual(@as(usize, 10), combos.len);
        testing.expectEqualStrings("abc", combos[0]);
        testing.expectEqualStrings("abd", combos[1]);
        testing.expectEqualStrings("abe", combos[2]);
        testing.expectEqualStrings("acd", combos[3]);
        testing.expectEqualStrings("ace", combos[4]);
        testing.expectEqualStrings("ade", combos[5]);
        testing.expectEqualStrings("bcd", combos[6]);
        testing.expectEqualStrings("bce", combos[7]);
        testing.expectEqualStrings("bde", combos[8]);
        testing.expectEqualStrings("cde", combos[9]);
    }
    {
        var combos = try combinations(u8, testing.allocator, chars, 1);
        defer testing.allocator.free(combos);
        defer for (combos) |combo| testing.allocator.free(combo);

        testing.expectEqual(@as(usize, 5), combos.len);
        testing.expectEqualStrings("a", combos[0]);
        testing.expectEqualStrings("b", combos[1]);
        testing.expectEqualStrings("c", combos[2]);
        testing.expectEqualStrings("d", combos[3]);
        testing.expectEqualStrings("e", combos[4]);
    }
}

fn hashState(state: State) u64 {
    var hasher = std.hash.Wyhash.init(0);
    std.hash.autoHash(&hasher, state.elevator_floor);
    for (state.floors) |floor, i| {
        var iter = floor.iterator();
        while (iter.next()) |obj| {
            std.hash.autoHash(&hasher, obj.kind);
            std.hash.autoHash(&hasher, i);
        }
    }
    return hasher.final();
}

fn stateEql(state1: State, state2: State) bool {
    if (state1.elevator_floor != state2.elevator_floor)
        return false;
    for (state1.floors) |floor, i| {
        if (floor.count() != state2.floors[i].count())
            return false;
        var iter = floor.iterator();
        while (iter.next()) |obj| {
            if (!state2.floors[i].exists(obj))
                return false;
        }
    }
    return true;
}

const StateSet = HashSet(
    State,
    hashState,
    stateEql,
    std.hash_map.DefaultMaxLoadPercentage,
);

fn numOfStepsToComplete(allocator: *Allocator, state: *State) !usize {
    var arena = ArenaAllocator.init(allocator);
    defer arena.deinit();
    var queue = LinearFifo(State, .Dynamic).init(allocator);
    defer queue.deinit();
    var discovered = StateSet.init(allocator);
    defer discovered.deinit();

    try discovered.put(state.*);
    try queue.writeItem(state.*);

    while (queue.readableLength() > 0) {
        var v = queue.readItem() orelse return error.EmptyQueue;
        if (v.isFinal())
            return v.step;

        var adj_edges = try v.nextSteps(&arena.allocator);
        for (adj_edges) |edge| {
            if (!discovered.exists(edge)) {
                try discovered.put(edge);
                try queue.writeItem(edge);
            }
        }
    }
    return error.NoResult;
}

test "find number of steps" {
    var state = State.init(testing.allocator);
    defer state.deinit();
    try state.floors[0].put(.{ .element = "hydrogen", .kind = .microchip });
    try state.floors[0].put(.{ .element = "lithium", .kind = .microchip });
    try state.floors[1].put(.{ .element = "hydrogen", .kind = .generator });
    try state.floors[2].put(.{ .element = "lithium", .kind = .generator });

    testing.expectEqual(@as(usize, 11), try numOfStepsToComplete(testing.allocator, &state));
}
