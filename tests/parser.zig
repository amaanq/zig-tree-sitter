const std = @import("std");
const testing = std.testing;
const tree_sitter = @import("tree-sitter");

const RndGen = std.rand.DefaultPrng;
const Node = tree_sitter.Node;
const Tree = tree_sitter.Tree;
const Language = tree_sitter.Language;
const LanguageOpts = tree_sitter.LanguageOpts;
const Point = tree_sitter.Point;
const Parser = tree_sitter.Parser;

var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
const allocator = arena.allocator();

fn load_except_recompile(lang: []const u8, scanner: bool, cpp: bool) !Language {
    const output = try std.fmt.allocPrint(allocator, "./tests/scratch/{s}.so", .{lang});
    defer allocator.free(output);

    return Language.init(allocator, lang, LanguageOpts{ .lib_path = output }) catch
        {
        const parser_c = try std.fmt.allocPrint(allocator, "./tests/fixtures/tree-sitter-{s}/src/parser.c", .{lang});
        defer allocator.free(parser_c);

        const scanner_c = if (cpp)
            try std.fmt.allocPrint(allocator, "./tests/fixtures/tree-sitter-{s}/src/scanner.cc", .{lang})
        else
            try std.fmt.allocPrint(allocator, "./tests/fixtures/tree-sitter-{s}/src/scanner.c", .{lang});
        defer allocator.free(scanner_c);

        const input = if (scanner)
            &[_][]const u8{ parser_c, scanner_c }
        else
            &[_][]const u8{parser_c};

        const opts = LanguageOpts{ .paths = .{ .input = input, .output = output } };

        return try Language.init(allocator, lang, opts);
    };
}

test "parsing simple string" {
    const Rust = try load_except_recompile("rust", true, false);

    var parser = Parser.init();
    defer parser.deinit();
    try testing.expectEqual(parser.set_language(Rust), true);

    const tree = parser.parse_string(
        \\
        \\        struct Stuff {}
        \\        fn main() {}"
    , null);
    const root_node = tree.root_node();
    try testing.expectEqualStrings(root_node.kind(), "source_file");
}
