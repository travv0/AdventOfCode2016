const std = @import("std");
const util = @import("util");
const testing = std.testing;
const expect = testing.expect;
const expectEqual = testing.expectEqual;
const expectEqualStrings = testing.expectEqualStrings;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const input = try util.readInput(&arena.allocator, 1000 * 1024);
    var tls_count: u32 = 0;
    var ssl_count: u32 = 0;
    var lines = std.mem.split(util.trim(input), "\n");
    while (lines.next()) |line| {
        if (supportsTls(line)) {
            tls_count += 1;
        }
        if (try supportsSsl(&arena.allocator, line)) {
            ssl_count += 1;
        }
    }
    std.debug.print("Part 1: {}\n", .{tls_count});
    std.debug.print("Part 2: {}\n", .{ssl_count});
}

fn supportsTls(ip: []const u8) bool {
    var iter = IpIter.init(ip);
    while (iter.next()) |section| {
        if (section.type == .Hypernet and hasAbba(section.content)) {
            return false;
        }
    }
    return hasAbba(ip);
}

test "supportsTls" {
    expect(supportsTls("abba[mnop]qrst"));
    expect(!supportsTls("abcd[bddb]xyyx"));
    expect(!supportsTls("aaaa[qwer]tyui"));
    expect(supportsTls("ioxxoj[asdfgh]zxcvbn"));
}

fn hasAbba(section: []const u8) bool {
    var i: u16 = 0;
    while (i < section.len - 3) : (i += 1) {
        if (section[i] == section[i + 3] and
            section[i + 1] == section[i + 2] and
            section[i] != section[i + 1])
        {
            return true;
        }
    }
    return false;
}

test "hasAbba" {
    expect(hasAbba("abba"));
    expect(!hasAbba("abcd"));
    expect(!hasAbba("aaaa"));
    expect(hasAbba("ioxxoj"));
}

fn supportsSsl(allocator: *Allocator, ip: []const u8) !bool {
    const nets = try findAbasAndBabs(allocator, ip);
    const abas = nets.abas;
    defer allocator.free(abas);
    const babs = nets.babs;
    defer allocator.free(babs);
    for (abas) |aba| {
        for (babs) |bab| {
            var bab2: [3]u8 = undefined;
            babify(aba, &bab2);
            if (std.mem.eql(u8, bab, &bab2)) {
                return true;
            }
        }
    }
    return false;
}

test "supportsSsl" {
    expect(try supportsSsl(testing.allocator, "aba[bab]xyz"));
    expect(!try supportsSsl(testing.allocator, "xyx[xyx]xyx"));
    expect(try supportsSsl(testing.allocator, "aaa[kek]eke"));
    expect(try supportsSsl(testing.allocator, "zazbz[bzb]cdb"));
}

const Nets = struct {
    abas: [][]const u8,
    babs: [][]const u8,
};

const SectionType = enum { Supernet, Hypernet };

const Section = struct {
    content: []const u8, type: SectionType
};

const IpIter = struct {
    const Self = @This();

    rest: []const u8,
    section_type: SectionType,

    fn init(ip: []const u8) Self {
        return .{
            .section_type = .Hypernet,
            .rest = ip,
        };
    }

    fn next(self: *Self) ?Section {
        if (self.rest.len == 0) return null;

        self.section_type = switch (self.section_type) {
            .Supernet => .Hypernet,
            .Hypernet => .Supernet,
        };

        const delimiter: u8 = switch (self.section_type) {
            .Supernet => '[',
            .Hypernet => ']',
        };
        const end = std.mem.indexOfScalar(u8, self.rest, delimiter) orelse self.rest.len;
        const section = self.rest[0..end];

        self.rest = if (end < self.rest.len) self.rest[end + 1 ..] else &[_]u8{};

        return Section{ .content = section, .type = self.section_type };
    }
};

fn findAbasAndBabs(allocator: *Allocator, ip: []const u8) !Nets {
    var abas = ArrayList([]const u8).init(allocator);
    errdefer abas.deinit();
    var babs = ArrayList([]const u8).init(allocator);
    errdefer babs.deinit();

    var iter = IpIter.init(ip);

    while (iter.next()) |section| {
        var i: u16 = 0;
        while (i < section.content.len - 2) : (i += 1) {
            const slice = section.content[i .. i + 3];
            if (isAba(slice)) {
                try switch (section.type) {
                    .Supernet => abas.append(slice),
                    .Hypernet => babs.append(slice),
                };
            }
        }
    }

    return Nets{
        .abas = abas.toOwnedSlice(),
        .babs = babs.toOwnedSlice(),
    };
}

test "findAbasAndBabs" {
    const nets = try findAbasAndBabs(testing.allocator, "aba[bab]xyz");
    defer testing.allocator.free(nets.abas);
    defer testing.allocator.free(nets.babs);
    expectEqualStrings("aba", nets.abas[0]);
    expectEqual(@as(usize, 1), nets.abas.len);
    expectEqualStrings("bab", nets.babs[0]);
    expectEqual(@as(usize, 1), nets.babs.len);
}

fn isAba(section: []const u8) bool {
    return section[0] == section[2] and
        section[0] != section[1];
}

test "isAba" {
    expect(isAba("aba"));
    expect(!isAba("abc"));
    expect(!isAba("aaa"));
    expect(isAba("oxo"));
}

fn babify(aba: []const u8, bab: *[3]u8) void {
    bab[0] = aba[1];
    bab[1] = aba[0];
    bab[2] = aba[1];
}

test "babify" {
    var aba = "aba";
    var bab: [3]u8 = undefined;
    babify(aba, &bab);
    expectEqualStrings("bab", &bab);
}
