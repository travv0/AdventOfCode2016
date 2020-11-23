const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
const LinkedList = std.SinglyLinkedList(usize);
const Node = LinkedList.Node;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;
    const input = 3018458;
    std.debug.print("Part 1: {}\n", .{playGame(1, allocator, input)});
    std.debug.print("Part 2: {}\n", .{playGame(2, allocator, input)});
}

fn playGame(comptime part: u2, allocator: *Allocator, num_of_elves: usize) !usize {
    if (part != 1 and part != 2) @compileError("part must be 1 or 2");
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    // build circular list of elves
    var elves = LinkedList{};
    {
        var elf = num_of_elves;
        var last_elf: *Node = undefined;
        while (elf > 0) {
            var node = try arena.allocator.create(Node);
            node.* = .{ .data = elf };
            elves.prepend(node);
            if (elf == num_of_elves)
                last_elf = node;
            elf -= 1;
        }
        last_elf.*.next = elves.first;
    }

    // get a pointer to the node before the elf to steal from.
    // for part 1, this is the current elf, for part 2, it's the elf across from the current elf
    var take_from_next = elves.first.?;
    if (part == 2) {
        var take_from_next_index = num_of_elves / 2 - 1;
        var i: usize = 0;
        while (i < take_from_next_index) : ({
            take_from_next = take_from_next.next.?;
            i += 1;
        }) {}
    }

    // keep removing nodes of stolen-from elves until there are only 2 elves left,
    // at which point you know the winning elf. keep the take_from_next pointer on
    // the current elf for part 1, and across from the current elf for part 2
    var elf_count = num_of_elves;
    var node = elves.first.?;
    while (elf_count > 2) {
        _ = take_from_next.removeNext().?;
        if (part == 1 or elf_count % 2 == 1)
            take_from_next = take_from_next.next.?;
        node = node.next.?;
        elf_count -= 1;
    }
    return node.data;
}

test "playGame" {
    testing.expectEqual(@as(usize, 3), try playGame(1, testing.allocator, 5));
    testing.expectEqual(@as(usize, 2), try playGame(2, testing.allocator, 5));
}
