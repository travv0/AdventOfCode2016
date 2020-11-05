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

fn Payload(comptime T: type) type {
    return switch (@typeInfo(T)) {
        .ErrorUnion => |u| u.payload,
        else => T,
    };
}

pub fn map(
    comptime T: type,
    allocator: *Allocator,
    slice: anytype,
    context: anytype,
    func: fn (context: @TypeOf(context), elem: ElemType(@TypeOf(slice))) T,
) ![]Payload(T) {
    const U = Payload(T);
    var result = ArrayList(U).init(allocator);
    errdefer result.deinit();
    for (slice) |elem| {
        const r = func(context, elem);
        std.log.debug("{}", .{@typeInfo(@TypeOf(r))});
        if (@typeInfo(@TypeOf(r)) == .ErrorUnion) {
            try result.append(try r);
        } else {
            try result.append(r);
        }
    }
    return result.toOwnedSlice();
}

const Error = error{BadThingHappened};
fn multiplyBad(by: usize, i: usize) Error!usize {
    if (by == 2) return Error.BadThingHappened;
    return i * by;
}
fn multiply(by: usize, i: usize) usize {
    return i * by;
}
test "map" {
    const items = [_]usize{ 1, 2, 3, 4 };
    const new_items = try map(usize, testing.allocator, items, @as(usize, 2), multiply);
    defer testing.allocator.free(new_items);
    testing.expectEqualSlices(usize, &[_]usize{ 2, 4, 6, 8 }, new_items);
    testing.expectError(
        Error.BadThingHappened,
        map(Error!usize, testing.allocator, items, @as(usize, 2), multiplyBad),
    );
}

pub fn every(
    slice: anytype,
    context: anytype,
    predicateFn: fn (context: @TypeOf(context), elem: ElemType(@TypeOf(slice))) bool,
) bool {
    for (slice) |elem| {
        if (!predicateFn(context, elem)) return false;
    }
    return true;
}

test "every" {
    const items = [_]usize{ 1, 2, 3, 4 };
    testing.expect(every(items, @as(usize, 0), greaterThan));
    testing.expect(!every(&items, @as(usize, 1), greaterThan));
}

pub fn ElemType(comptime T: type) type {
    return switch (@typeInfo(T)) {
        .Pointer => |p| ElemType(p.child),
        .Array => |a| a.child,
        .Struct => |s| T,
        else => @compileError("Type doesn't have elements"),
    };
}

pub fn any(
    slice: anytype,
    context: anytype,
    predicateFn: fn (context: @TypeOf(context), elem: ElemType(@TypeOf(slice))) bool,
) bool {
    for (slice) |elem| {
        if (predicateFn(context, elem)) return true;
    }
    return false;
}

fn greaterThan(numToBeat: usize, i: usize) bool {
    return i > numToBeat;
}
test "any" {
    const items = [_]usize{ 1, 2, 3, 4 };
    testing.expect(any(items, @as(usize, 3), greaterThan));
    testing.expect(!any(&items, @as(usize, 4), greaterThan));
}
