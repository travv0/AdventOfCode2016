const std = @import("std");
const builtin = @import("builtin");
const Builder = std.build.Builder;

pub fn build(b: *Builder) !void {
    const mode = b.standardReleaseOptions();
    const target = b.standardTargetOptions(.{
        .default_target = if (builtin.os.tag == .windows)
            .{
                .cpu_arch = .i386,
                .os_tag = .windows,
                .abi = .gnu,
            }
        else
            .{},
    });

    var daysList = std.ArrayList([]const u8).init(b.allocator);

    const cwd = try std.fs.cwd().openDir(".", .{ .iterate = true });
    var iter = cwd.iterate();
    while (try iter.next()) |entry| {
        const name = try b.allocator.dupe(u8, entry.name);
        if (name.len == 5 and std.mem.startsWith(u8, name, "day")) {
            try daysList.append(name);
        }
    }

    const days = daysList.toOwnedSlice();

    var buildZigPath = [_][]const u8{"build.zig"};
    const fmtPaths = try std.mem.concat(b.allocator, []const u8, &[_][][]const u8{ &buildZigPath, days });
    const fmt = b.addFmt(fmtPaths);

    const build_all_step = b.step("build", "Build executables for all days.");
    const test_all_step = b.step("test", "Run all tests.");
    const run_all_step = b.step("run-all", "Run all days.");
    for (days) |day| {
        const exe = b.addExecutable(day, try std.fs.path.join(b.allocator, &[_][]const u8{ day, "main.zig" }));
        const tests = b.addTest(try std.fs.path.join(b.allocator, &[_][]const u8{ day, "main.zig" }));

        if (std.mem.eql(u8, day, "day04")) {
            exe.addIncludeDir("day04" ++ std.fs.path.sep_str ++ "include");
            exe.addLibPath("day04" ++ std.fs.path.sep_str ++ "lib");
            exe.linkSystemLibrary("c");
            exe.linkSystemLibrary("pcre");
            tests.addIncludeDir("day04" ++ std.fs.path.sep_str ++ "include");
            tests.addLibPath("day04" ++ std.fs.path.sep_str ++ "lib");
            tests.linkSystemLibrary("c");
            tests.linkSystemLibrary("pcre");
        }

        exe.addPackagePath("util", "util.zig");
        exe.setTarget(target);
        exe.setBuildMode(mode);
        exe.install();

        tests.addPackagePath("util", "util.zig");
        tests.setBuildMode(mode);
        tests.setTarget(target);
        tests.setNamePrefix(try std.fmt.allocPrint(b.allocator, "{} ", .{day}));

        const build_step = b.step(
            try std.fmt.allocPrint(b.allocator, "build-{}", .{day}),
            try std.fmt.allocPrint(b.allocator, "Build executable for {}.", .{day}),
        );
        build_step.dependOn(&exe.step);
        build_all_step.dependOn(&exe.step);

        const test_step = b.step(
            try std.fmt.allocPrint(b.allocator, "test-{}", .{day}),
            try std.fmt.allocPrint(b.allocator, "Run all tests for {}.", .{day}),
        );
        test_step.dependOn(&tests.step);
        test_all_step.dependOn(test_step);

        const log_step = b.addLog("\nResults for {}:\n", .{day});

        const run_cmd = exe.run();
        run_cmd.addArg(try std.fs.path.join(b.allocator, &[_][]const u8{ day, "input.txt" }));

        const run_step = b.step(
            try std.fmt.allocPrint(b.allocator, "run-{}", .{day}),
            try std.fmt.allocPrint(b.allocator, "Run the executable for {}.", .{day}),
        );
        run_step.dependOn(&log_step.step);
        run_step.dependOn(&run_cmd.step);
        run_all_step.dependOn(run_step);
    }

    const fmt_step = b.step("fmt", "Format source files.");
    fmt_step.dependOn(&fmt.step);

    const all_step = b.step("all", "Run all test and all days.");
    all_step.dependOn(fmt_step);
    all_step.dependOn(build_all_step);
    all_step.dependOn(test_all_step);
    all_step.dependOn(run_all_step);

    b.default_step.dependOn(fmt_step);
    b.default_step.dependOn(test_all_step);
}
