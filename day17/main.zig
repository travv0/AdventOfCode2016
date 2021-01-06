const std = @import("std");
const c = @cImport(@cInclude("AStar.h"));
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const Md5 = std.crypto.hash.Md5;
const absInt = std.math.absInt;
const fmt = std.fmt;
const mem = std.mem;
const testing = std.testing;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = &gpa.allocator;

    const input = "yjjvjgan";
    const path = try findPath(allocator, input);
    defer if (path) |p| allocator.free(p);

    std.debug.print("Part 1: {s}\n", .{path});
    std.debug.print("Part 2: {}\n", .{findLongestPathLen(allocator, input)});
}

const rooms_width = 4;
const rooms_height = 4;

const Node = struct {
    const Self = @This();

    arena: *ArenaAllocator,
    x: usize,
    y: usize,
    path_until: []const u8,

    fn init(arena: *ArenaAllocator, x: usize, y: usize, path: []const u8, new_dir: ?u8) !Self {
        const path_until = try if (new_dir) |nd|
            fmt.allocPrint(&arena.allocator, "{s}{c}", .{ path, nd })
        else
            arena.allocator.dupe(u8, path);
        return Self{ .arena = arena, .x = x, .y = y, .path_until = path_until };
    }

    const ValidDirs = struct {
        up: bool,
        down: bool,
        left: bool,
        right: bool,
    };

    fn validDirs(self: Self, input: []const u8) !ValidDirs {
        var buf: [10000]u8 = undefined;
        var output: [Md5.digest_length]u8 = undefined;
        {
            const result = try fmt.bufPrint(&buf, "{s}{s}", .{ input, self.path_until });
            Md5.hash(result, &output, .{});
        }
        const hash = try fmt.bufPrint(&buf, "{x}", .{output});
        return ValidDirs{
            .up = self.y > 0 and 'b' <= hash[0],
            .down = self.y < rooms_height - 1 and 'b' <= hash[1],
            .left = self.x > 0 and 'b' <= hash[2],
            .right = self.x < rooms_width - 1 and 'b' <= hash[3],
        };
    }
};

const Context = struct { input: []const u8, part: enum { one, two } };

fn pathNodeNeighbors(
    neighbors: c.ASNeighborList,
    as_node: ?*c_void,
    as_context: ?*c_void,
) callconv(.C) void {
    const node = @ptrCast(?*Node, @alignCast(@alignOf(Node), as_node)).?;
    const context = @ptrCast(?*Context, @alignCast(@alignOf(Context), as_context)).?;
    const valid_dirs = node.validDirs(mem.spanZ(context.input)) catch unreachable;

    if (valid_dirs.up) {
        c.ASNeighborListAdd(
            neighbors,
            &Node.init(node.arena, node.x, node.y - 1, node.path_until, 'U'),
            1,
        );
    }
    if (valid_dirs.down) {
        c.ASNeighborListAdd(
            neighbors,
            &Node.init(node.arena, node.x, node.y + 1, node.path_until, 'D'),
            if (context.part == .two and node.x == rooms_width - 1 and node.y == rooms_height - 2)
                @intToFloat(f32, 100000 - node.path_until.len * 2)
            else
                1,
        );
    }
    if (valid_dirs.left) {
        c.ASNeighborListAdd(
            neighbors,
            &Node.init(node.arena, node.x - 1, node.y, node.path_until, 'L'),
            1,
        );
    }
    if (valid_dirs.right) {
        c.ASNeighborListAdd(
            neighbors,
            &Node.init(node.arena, node.x + 1, node.y, node.path_until, 'R'),
            if (context.part == .two and node.y == rooms_height - 1 and node.x == rooms_width - 2)
                @intToFloat(f32, 100000 - node.path_until.len * 2)
            else
                1,
        );
    }
}

fn earlyExit(
    visited_count: usize,
    visiting_node: ?*c_void,
    goal_node: ?*c_void,
    context: ?*c_void,
) callconv(.C) c_int {
    const node1 = @ptrCast(?*Node, @alignCast(@alignOf(Node), visiting_node)).?;
    const node2 = @ptrCast(?*Node, @alignCast(@alignOf(Node), goal_node)).?;
    if (node1.x == node2.x and node1.y == node2.y)
        return 1
    else
        return 0;
}

const path_node_source = c.ASPathNodeSource{
    .nodeSize = @sizeOf(Node),
    .nodeNeighbors = pathNodeNeighbors,
    .pathCostHeuristic = null,
    .earlyExit = earlyExit,
    .nodeComparator = null,
};

fn findPath(allocator: *Allocator, input: []const u8) !?[]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var from = try Node.init(&arena, 0, 0, "", null);
    var to = try Node.init(&arena, rooms_width - 1, rooms_height - 1, "", null);

    var context = Context{
        .input = try std.cstr.addNullByte(&arena.allocator, input),
        .part = .one,
    };

    const path = c.ASPathCreate(&path_node_source, &context, &from, &to);
    defer c.ASPathDestroy(path);

    const node = @ptrCast(
        ?*align(1) Node,
        c.ASPathGetNode(path, c.ASPathGetCount(path) - 1),
    );
    return if (node) |n|
        try allocator.dupe(u8, n.path_until)
    else
        null;
}

test "findPath" {
    {
        const path = (try findPath(testing.allocator, "ihgpwlah")).?;
        defer testing.allocator.free(path);
        testing.expectEqualStrings("DDRRRD", path);
    }
    {
        const path = (try findPath(testing.allocator, "kglvqrro")).?;
        defer testing.allocator.free(path);
        testing.expectEqualStrings("DDUDRLRRUDRD", path);
    }
    {
        const path = (try findPath(testing.allocator, "ulqzkmiv")).?;
        defer testing.allocator.free(path);
        testing.expectEqualStrings("DRURDRUDDLLDLUURRDULRLDUUDDDRR", path);
    }
}

fn findLongestPathLen(allocator: *Allocator, input: []const u8) !usize {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var from = Node.init(&arena, 0, 0, "", null);
    var to = Node.init(&arena, rooms_width - 1, rooms_height - 1, "", null);

    var context = Context{
        .input = try std.cstr.addNullByte(&arena.allocator, input),
        .part = .two,
    };

    const path = c.ASPathCreate(&path_node_source, &context, &from, &to);
    defer c.ASPathDestroy(path);

    return c.ASPathGetCount(path) - 1;
}

test "findLongestPathLength" {
    {
        const path_len = try findLongestPathLen(testing.allocator, "ihgpwlah");
        testing.expectEqual(@as(usize, 370), path_len);
    }
    {
        const path_len = try findLongestPathLen(testing.allocator, "kglvqrro");
        testing.expectEqual(@as(usize, 492), path_len);
    }
    {
        const path_len = try findLongestPathLen(testing.allocator, "ulqzkmiv");
        testing.expectEqual(@as(usize, 830), path_len);
    }
}
