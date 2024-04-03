
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // ============================================================
    const pom = b.addModule("pom", .{
        .root_source_file = .{ .path = "src/root.zig" },
    });

    // ============================================================
    const leptjson = b.addStaticLibrary(.{
        .name = "leptjson",
        .root_source_file = .{ .path = "leptjson/root.zig" },
        .optimize = optimize,
        .target = target,
    });
    leptjson.root_module.addImport("pom", pom);
    const leptjson_install = b.addInstallArtifact(leptjson, .{});
    const leptjson_install_step = b.step("leptjson", "Build and install leptjson");
    leptjson_install_step.dependOn(&leptjson_install.step);

    const leptjson_test = b.addTest(.{
        .root_source_file = .{ .path = "leptjson/root.zig" },
        .target = target,
        .optimize = optimize,
    });
    leptjson_test.root_module.addImport("pom", pom);
    const leptjson_test_run = b.addRunArtifact(leptjson_test);
    const leptjson_test_step = b.step("leptjson_test", "Run leptjson test");
    leptjson_test_step.dependOn(&leptjson_test_run.step);
}