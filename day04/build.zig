const Builder = @import("std").build.Builder;
const builtin = @import("builtin");

pub fn build(b: *Builder) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const default_target = if (builtin.os.tag == .windows)
        .{
            .cpu_arch = .i386,
            .os_tag = .windows,
            .abi = .gnu,
        }
    else
        .{};

    const target = b.standardTargetOptions(.{
        .default_target = default_target,
    });

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("day04", "src/main.zig");
    exe.addIncludeDir("include");
    exe.addLibPath("lib");
    exe.linkSystemLibrary("c");
    exe.linkSystemLibrary("pcre");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const test_exe = b.addTest("src/main.zig");
    test_exe.addIncludeDir("include");
    test_exe.addLibPath("lib");
    test_exe.linkSystemLibrary("c");
    test_exe.linkSystemLibrary("pcre");
    test_exe.setTarget(target);
    test_exe.setBuildMode(mode);

    const test_step = b.step("test", "Run all the tests");
    test_step.dependOn(&test_exe.step);
}
