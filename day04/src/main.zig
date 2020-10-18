const std = @import("std");
const util = @import("util");
const c = @cImport(@cInclude("pcre.h"));

pub fn main() anyerror!void {
    var pcreError: ?*const u8 = undefined;
    var pcreErrorOffset: c_int = undefined;
    const pcreFree = c.pcre_free orelse return error.NoPcreFree;
    const regexCompiled = c.pcre_compile("(.*)-(.*)\\[(.*)\\]", 0, &pcreError, &pcreErrorOffset, null) orelse {
        std.log.err("error compiling regex: {}", .{pcreError});
        std.os.exit(1);
    };
    defer pcreFree(regexCompiled);
    const regexExtra = c.pcre_study(regexCompiled, 0, &pcreError);
    if (pcreError) |err| {
        std.log.err("error studying regex: {}", .{err});
        std.os.exit(1);
    }
    defer if (regexExtra) |re| pcreFree(re);

    // const allocator = std.heap.page_allocator;
    // const input = util.readFileIntoString(allocator, "input.txt", 1024*
}

const Line = struct {
    name: []const u8,
    sectorId: u16,
    checksum: []const u8,
};

pub fn parseLine(regexCompiled: *c.pcre, regexExtra: ?*c.pcre_extra, line: []const u8) !Line {
    const subStrsLen = 20;
    var subStrs: [subStrsLen]c_int = undefined;
    var name: ?[*:0]const u8 = undefined;
    var sectorId: ?[*:0]const u8 = undefined;
    var checksum: ?[*:0]const u8 = undefined;
    const execRet = c.pcre_exec(regexCompiled, regexExtra, line.ptr, @intCast(c_int, line.len), 0, 0, &subStrs, subStrsLen);
    std.debug.assert(4 == execRet);
    _ = c.pcre_get_substring(line.ptr, &subStrs, execRet, 1, &name);
    _ = c.pcre_get_substring(line.ptr, &subStrs, execRet, 2, &sectorId);
    _ = c.pcre_get_substring(line.ptr, &subStrs, execRet, 3, &checksum);
    return Line{
        .name = std.mem.span(name orelse return error.CouldNotParseName),
        .sectorId = try std.fmt.parseUnsigned(u16, std.mem.span(sectorId orelse return error.CouldNotParseSectorId), 10),
        .checksum = std.mem.span(checksum orelse return error.CouldNotParseChecksum),
    };
}

test "parseLine" {
    var pcreError: ?*const u8 = undefined;
    var pcreErrorOffset: c_int = undefined;
    const regexCompiled = c.pcre_compile("(.*)-(.*)\\[(.*)\\]", 0, &pcreError, &pcreErrorOffset, null).?;
    defer c.pcre_free.?(regexCompiled);
    const line = try parseLine(regexCompiled, null, "not-a-real-room-404[oarel]");
    std.debug.assert(std.mem.eql(u8, line.name, "not-a-real-room"));
    std.debug.assert(line.sectorId == 404);
    std.debug.assert(std.mem.eql(u8, line.checksum, "oarel"));
}
