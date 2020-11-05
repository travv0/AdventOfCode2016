const std = @import("std");
const builtin = @import("builtin");
const Builder = std.build.Builder;

pub fn build(b: *Builder) !void {
    const mode = b.standardReleaseOptions();
    const target = b.standardTargetOptions(.{});

    var days_list = std.ArrayList([]const u8).init(b.allocator);

    const cwd = try std.fs.cwd().openDir(".", .{ .iterate = true });
    var iter = cwd.iterate();
    while (try iter.next()) |entry| {
        const name = try b.allocator.dupe(u8, entry.name);
        if (name.len == 5 and std.mem.startsWith(u8, name, "day")) {
            try days_list.append(name);
        }
    }

    const days = days_list.toOwnedSlice();
    std.sort.sort([]const u8, days, {}, strLessThan);

    var build_zig_path = [_][]const u8{ "build.zig", "util.zig" };
    const fmt_paths = try std.mem.concat(b.allocator, []const u8, &[_][][]const u8{ &build_zig_path, days });
    const fmt = b.addFmt(fmt_paths);

    const build_all_step = b.step("build", "Build executables for all days.");
    const test_all_step = b.step("test", "Run all tests.");
    const run_all_step = b.step("run-all", "Run all days.");
    for (days) |day| {
        const exe = b.addExecutable(day, try std.fs.path.join(b.allocator, &[_][]const u8{ day, "main.zig" }));
        const tests = b.addTest(try std.fs.path.join(b.allocator, &[_][]const u8{ day, "main.zig" }));

        exe.setTarget(target);
        tests.setTarget(target);

        if (std.mem.eql(u8, day, "day04")) {
            exe.addIncludeDir("day04" ++ std.fs.path.sep_str ++ "include");
            exe.addLibPath("day04" ++ std.fs.path.sep_str ++ "lib");
            exe.linkSystemLibrary("c");
            exe.linkSystemLibrary("pcre");
            tests.addIncludeDir("day04" ++ std.fs.path.sep_str ++ "include");
            tests.addLibPath("day04" ++ std.fs.path.sep_str ++ "lib");
            tests.linkSystemLibrary("c");
            tests.linkSystemLibrary("pcre");
            if (builtin.os.tag == .windows) {
                exe.setTarget(.{
                    .cpu_arch = .i386,
                    .os_tag = .windows,
                    .abi = .gnu,
                });
                tests.setTarget(.{
                    .cpu_arch = .i386,
                    .os_tag = .windows,
                    .abi = .gnu,
                });
            }
        }

        if (std.mem.eql(u8, day, "day13")) {
            exe.addCSourceFile("AStar/AStar.c", &[_][]const u8{"-std=c99"});
            exe.addIncludeDir("AStar");
            exe.linkSystemLibrary("c");
        }

        exe.addPackagePath("util", "util.zig");
        exe.setBuildMode(mode);
        exe.install();

        tests.addPackagePath("util", "util.zig");
        tests.setBuildMode(mode);
        tests.setNamePrefix(try std.fmt.allocPrint(b.allocator, "{} ", .{day}));

        const build_step = b.step(
            try std.fmt.allocPrint(b.allocator, "build-{}", .{day}),
            try std.fmt.allocPrint(b.allocator, "Build executable for {}.", .{day}),
        );
        build_step.dependOn(&exe.step);
        build_all_step.dependOn(&exe.step);

        const test_log_step = b.addLog("\nTest results for {}:\n", .{day});

        const test_step = b.step(
            try std.fmt.allocPrint(b.allocator, "test-{}", .{day}),
            try std.fmt.allocPrint(b.allocator, "Run all tests for {}.", .{day}),
        );
        test_step.dependOn(&test_log_step.step);
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
    all_step.dependOn(b.getInstallStep());
    all_step.dependOn(build_all_step);
    all_step.dependOn(test_all_step);
    all_step.dependOn(run_all_step);

    b.default_step.dependOn(fmt_step);
}

fn strLessThan(context: void, a: []const u8, b: []const u8) bool {
    var i: usize = 0;
    while (true) : (i += 1) {
        if (a.len <= i and b.len <= i) {
            return false;
        } else if (a.len <= i) {
            return true;
        } else if (b.len <= i) {
            return false;
        }

        if (a[i] < b[i]) {
            return true;
        } else if (a[i] > b[i]) {
            return false;
        }
    }
}
