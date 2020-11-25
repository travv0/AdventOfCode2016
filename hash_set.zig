// SPDX-License-Identifier: MIT
// Copyright (c) 2015-2020 Zig Contributors
// This file is part of [zig](https://ziglang.org/), which is MIT licensed.
// The MIT license requires this copyright notice to be included in all copies
// and substantial portions of the software.
const std = @import("std");
const HashMap = std.HashMap;
const hash_map = std.hash_map;
const mem = std.mem;
const Allocator = mem.Allocator;
const testing = std.testing;

pub fn AutoHashSet(comptime T: type) type {
    return HashSet(
        T,
        hash_map.getAutoHashFn(T),
        hash_map.getAutoEqlFn(T),
        hash_map.DefaultMaxLoadPercentage,
    );
}

pub const StringHashSet = HashSet(
    []const u8,
    hash_map.hashString,
    hash_map.eqlString,
    hash_map.DefaultMaxLoadPercentage,
);

pub fn HashSet(
    comptime T: type,
    comptime hashFn: fn (key: T) u64,
    comptime eqlFn: fn (a: T, b: T) bool,
    comptime MaxLoadPercentage: u64,
) type {
    return struct {
        const Self = @This();

        hash_map: SetHashMap,

        const SetHashMap = HashMap(T, void, hashFn, eqlFn, MaxLoadPercentage);

        pub fn init(allocator: *Allocator) Self {
            return .{ .hash_map = SetHashMap.init(allocator) };
        }

        pub fn deinit(self: *Self) void {
            self.hash_map.deinit();
            self.* = undefined;
        }

        pub fn put(self: *Self, value: T) !void {
            _ = try self.hash_map.put(value, {});
        }

        pub fn exists(self: Self, value: T) bool {
            return self.hash_map.get(value) != null;
        }

        pub fn delete(self: *Self, value: T) ?T {
            if (self.hash_map.remove(value)) |entry| return entry.key;
            return null;
        }

        pub fn count(self: Self) usize {
            return self.hash_map.count();
        }

        pub fn iterator(self: Self) Iterator {
            return .{ .iterator = self.hash_map.iterator() };
        }

        const Iterator = struct {
            iterator: SetHashMap.Iterator,

            pub fn next(it: *Iterator) ?T {
                if (it.iterator.next()) |entry| return entry.key;
                return null;
            }
        };
    };
}

test "HashSet" {
    var str_hash_set = StringHashSet.init(std.testing.allocator);
    defer str_hash_set.deinit();

    try str_hash_set.put("x");
    testing.expectEqual(@as(usize, 1), str_hash_set.count());
    _ = str_hash_set.delete("x");
    testing.expectEqual(@as(usize, 0), str_hash_set.count());

    try str_hash_set.put("x");
    try str_hash_set.put("y");
    try str_hash_set.put("z");
    try str_hash_set.put("x");

    testing.expectEqual(@as(usize, 3), str_hash_set.count());
    testing.expect(str_hash_set.exists("x"));
    testing.expect(str_hash_set.exists("y"));
    testing.expect(str_hash_set.exists("z"));
    testing.expect(!str_hash_set.exists("a"));

    var int_hash_set = AutoHashSet(u8).init(std.testing.allocator);
    defer int_hash_set.deinit();

    try int_hash_set.put(1);
    testing.expectEqual(@as(usize, 1), int_hash_set.count());
    _ = int_hash_set.delete(1);
    testing.expectEqual(@as(usize, 0), int_hash_set.count());

    try int_hash_set.put(1);
    try int_hash_set.put(2);
    try int_hash_set.put(3);
    try int_hash_set.put(1);

    testing.expectEqual(@as(usize, 3), int_hash_set.count());
    testing.expect(int_hash_set.exists(1));
    testing.expect(int_hash_set.exists(2));
    testing.expect(int_hash_set.exists(3));
    testing.expect(!int_hash_set.exists(4));

    var i: usize = 0;
    var iterator = int_hash_set.iterator();
    while (iterator.next()) |v| {
        testing.expect(v == 1 or v == 2 or v == 3);
        i += 1;
    }
    testing.expectEqual(@as(usize, 3), i);
}
