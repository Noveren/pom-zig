
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // ============================================================
    const pom = b.addModule("pom", .{
        .root_source_file = .{ .path = "src/root.zig" },
    });

    // ============================================================
    const mjson = b.addStaticLibrary(.{
        .name = "mjson",
        .root_source_file = .{ .path = "mjson/root.zig" },
        .optimize = optimize,
        .target = target,
    });
    mjson.root_module.addImport("pom", pom);
    const mjson_install = b.addInstallArtifact(mjson, .{});
    const mjson_install_step = b.step("mjson", "Build and install mjson");
    mjson_install_step.dependOn(&mjson_install.step);

    const mjson_test = b.addTest(.{
        .root_source_file = .{ .path = "mjson/root.zig" },
        .target = target,
        .optimize = optimize,
    });
    const mjson_test_step = b.step("mjson_test", "Run mjson test");
    mjson_test_step.dependOn(&mjson_test.step);
}