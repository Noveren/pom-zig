
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const pom = b.addModule("pom", .{
        .root_source_file = .{ .path = "src/root.zig" },
    });

    const tests = b.addTest(.{
        .root_source_file = .{ .path = "tests/root.zig"},
        .target = target,
        .optimize = optimize,
    });
    tests.root_module.addImport("pom", pom);
    const run_tests = b.addRunArtifact(tests);
    run_tests.skip_foreign_checks = true;
    const step_tests = b.step("test", "Run tests");
    step_tests.dependOn(&run_tests.step);
}