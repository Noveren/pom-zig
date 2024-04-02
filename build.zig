
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const pom = b.addModule("pom", .{
        .root_source_file = .{ .path = "src/root.zig" },
    });

    const tests = b.addTest(.{
        .root_source_file = .{ .path = "example/root.zig"},
        .target = target,
        .optimize = optimize,
    });
    tests.root_module.addImport("pom", pom);
    const run_tests = b.addRunArtifact(tests);
    run_tests.skip_foreign_checks = true;
    const step_tests = b.step("test", "Run tests");
    step_tests.dependOn(&run_tests.step);

    const json = b.addExecutable(.{
        .name = "json",
        .root_source_file = .{ .path = "example/json.zig" },
        .optimize = optimize,
        .target = target,
    });
    json.root_module.addImport("pom", pom);
    const install_json = b.addInstallArtifact(json, .{});

    const run_json = b.addRunArtifact(json);
    const step_run_json = b.step("run_json", "Run Example Json");
    step_run_json.dependOn(&run_json.step);
    step_run_json.dependOn(&install_json.step);
}