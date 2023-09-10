const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const lib = b.addStaticLibrary(.{
        .name = "zig-tree-sitter",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    const tree_sitter_dep = b.dependency("tree_sitter", .{
        .target = target,
        .optimize = optimize,
    });

    const tree_sitter_mod = b.addModule("tree-sitter", .{
        .source_file = .{ .path = "src/main.zig" },
    });

    lib.linkLibrary(tree_sitter_dep.artifact("tree-sitter"));
    lib.addCSourceFile(.{ .file = .{ .path = "src/tree-sitter/lib/src/lib.c" }, .flags = &.{} });
    lib.addIncludePath(.{ .path = "src/tree-sitter/lib/include" });

    b.installArtifact(lib);

    const main_tests = b.addTest(.{
        .root_source_file = .{ .path = "tests/test.zig" },
        .target = target,
        .optimize = optimize,
    });
    main_tests.addModule("tree-sitter", tree_sitter_mod);

    _ = b.addRunArtifact(main_tests);

    var main_test_step = b.step("test", "Run all tests");
    const tests = .{ "node", "parser" };

    inline for (tests) |test_name| {
        var ts_test = b.addTest(.{
            .name = test_name,
            .root_source_file = .{ .path = "tests/" ++ test_name ++ ".zig" },
            .target = target,
            .optimize = optimize,
        });
        ts_test.addIncludePath(.{ .path = "src/tree-sitter/lib/include" });
        ts_test.linkLibrary(tree_sitter_dep.artifact("tree-sitter"));
        ts_test.addModule("tree-sitter", tree_sitter_mod);
        const run_test = b.addRunArtifact(ts_test);

        var test_step = b.step("test_" ++ test_name, std.fmt.allocPrint(b.allocator, "Run {s} tests", .{test_name}) catch unreachable);
        test_step.dependOn(&run_test.step);
        main_test_step.dependOn(test_step);
    }
}
