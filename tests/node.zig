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

const JSON_EXAMPLE =
    \\
    \\
    \\[
    \\  123,
    \\  false,
    \\  {
    \\    "x": null
    \\  }
    \\]
;
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

fn get_all_nodes(tree: Tree) std.mem.Allocator.Error!std.ArrayList(Node) {
    var result = std.ArrayList(Node).init(allocator);
    var visited_children = false;
    var cursor = tree.walk();
    while (true) {
        if (!visited_children) {
            try result.append(cursor.node());
            if (!cursor.goto_first_child()) {
                visited_children = true;
            }
        } else if (cursor.goto_next_sibling()) {
            visited_children = false;
        } else if (!cursor.goto_parent()) {
            break;
        }
    }
    return result;
}

// pub fn get_random_edit(rand: &mut Rand, input: &Vec<u8>) -> Edit {
//     let choice = rand.unsigned(10);
//     if choice < 2 {
//         // Insert text at end
//         let inserted_text = rand.words(3);
//         Edit {
//             position: input.len(),
//             deleted_length: 0,
//             inserted_text,
//         }
//     } else if choice < 5 {
//         // Delete text from the end
//         let deleted_length = rand.unsigned(30).min(input.len());
//         Edit {
//             position: input.len() - deleted_length,
//             deleted_length,
//             inserted_text: vec![],
//         }
//     } else if choice < 8 {
//         // Insert at a random position
//         let position = rand.unsigned(input.len());
//         let word_count = 1 + rand.unsigned(3);
//         let inserted_text = rand.words(word_count);
//         Edit {
//             position,
//             deleted_length: 0,
//             inserted_text,
//         }
//     } else {
//         // Replace at random position
//         let position = rand.unsigned(input.len());
//         let deleted_length = rand.unsigned(input.len() - position);
//         let word_count = 1 + rand.unsigned(3);
//         let inserted_text = rand.words(word_count);
//         Edit {
//             position,
//             deleted_length,
//             inserted_text,
//         }
//     }
// }

// pub fn words(&mut self, max_count: usize) -> Vec<u8> {
//     let mut result = Vec::new();
//     let word_count = self.unsigned(max_count);
//     for i in 0..word_count {
//         if i > 0 {
//             if self.unsigned(5) == 0 {
//                 result.push('\n' as u8);
//             } else {
//                 result.push(' ' as u8);
//             }
//         }
//         if self.unsigned(3) == 0 {
//             let index = self.unsigned(OPERATORS.len() - 1);
//             result.push(OPERATORS[index] as u8);
//         } else {
//             for _ in 0..self.unsigned(8) {
//                 result.push(self.0.sample(Alphanumeric) as u8);
//             }
//         }
//     }
//     result
// }

const OPERATORS = &[_]u8{ '+', '-', '<', '>', '(', ')', '*', '/', '&', '|', '!', ',', '.', '%' };

fn words(self: std.rand.Xoshiro256, max_count: usize) std.mem.Allocator.Error!std.ArrayList(u8) {
    var result = std.ArrayList(u8).init(allocator);
    const word_count = self.next() % max_count;
    for (0..word_count) |i| {
        if (i > 0) {
            if (self.next() % 5 == 0) {
                try result.append('\n');
            } else {
                try result.append(' ');
            }
        }
        if (self.next() % 3 == 0) {
            const index = self.next() % OPERATORS.len;
            try result.append(OPERATORS[index]);
        } else {
            for (0..(self.next() % 8)) |j| {
                _ = j;
                try result.append(self.next() % 256);
            }
        }
    }
    return result;
}

fn get_random_edit(rng: std.rand.Xoshiro256, input: []const u8) .{ usize, usize, []const u8 } {
    const choice = rng.next() % 10;
    if (choice < 2) {
        // Insert text at end
        const inserted_text = try words(rng, 3);
        return .{ input.len, 0, inserted_text };
    } else if (choice < 5) {
        // Delete text from the end
        // let deleted_length = rand.unsigned(30).min(input.len());
        var deleted_length = rng.next() % 30;
        if (deleted_length > input.len) {
            deleted_length = input.len;
        }
        return .{ input.len - deleted_length, deleted_length, &[]const u8{} };
    } else if (choice < 8) {
        // Insert at a random position
        const position = rng.next() % input.len;
        const word_count = 1 + rng.next() % 3;
        const inserted_text = try words(rng, word_count);
        return .{ position, 0, inserted_text };
    } else {
        // Replace at random position
        const position = rng.next() % input.len;
        const deleted_length = rng.next() % (input.len - position);
        const word_count = 1 + rng.next() % 3;
        const inserted_text = try words(rng, word_count);
        return .{ position, deleted_length, inserted_text };
    }
}

test "child()" {
    const JSON = try load_except_recompile("json", false, false);

    var parser = Parser.init();
    defer parser.deinit();
    try testing.expectEqual(parser.set_language(JSON), true);

    const tree = parser.parse_string(JSON_EXAMPLE, null);
    defer tree.deinit();
    const array_node = tree.root_node().child(0).?;

    try testing.expectEqualStrings(array_node.kind(), "array");
    try testing.expectEqual(array_node.named_child_count(), 3);
    try testing.expectEqual(array_node.start_byte(), std.mem.indexOf(u8, JSON_EXAMPLE, "[").?);
    try testing.expectEqual(array_node.end_byte(), std.mem.indexOf(u8, JSON_EXAMPLE, "]").? + 1);
    try testing.expectEqual(array_node.start_position(), Point{ .row = 2, .column = 0 });
    try testing.expectEqual(array_node.end_position(), Point{ .row = 8, .column = 1 });
    try testing.expectEqual(array_node.child_count(), 7);

    const left_bracket_node = array_node.child(0).?;
    const number_node = array_node.child(1).?;
    const comma_node1 = array_node.child(2).?;
    const false_node = array_node.child(3).?;
    const comma_node2 = array_node.child(4).?;
    const object_node = array_node.child(5).?;
    const right_bracket_node = array_node.child(6).?;

    try testing.expectEqualStrings(left_bracket_node.kind(), "[");
    try testing.expectEqualStrings(number_node.kind(), "number");
    try testing.expectEqualStrings(comma_node1.kind(), ",");
    try testing.expectEqualStrings(false_node.kind(), "false");
    try testing.expectEqualStrings(comma_node2.kind(), ",");
    try testing.expectEqualStrings(object_node.kind(), "object");
    try testing.expectEqualStrings(right_bracket_node.kind(), "]");

    try testing.expect(!left_bracket_node.is_named());
    try testing.expect(number_node.is_named());
    try testing.expect(!comma_node1.is_named());
    try testing.expect(false_node.is_named());
    try testing.expect(!comma_node2.is_named());
    try testing.expect(object_node.is_named());
    try testing.expect(!right_bracket_node.is_named());

    try testing.expectEqual(number_node.start_byte(), std.mem.indexOf(u8, JSON_EXAMPLE, "123").?);
    try testing.expectEqual(number_node.end_byte(), std.mem.indexOf(u8, JSON_EXAMPLE, "123").? + 3);
    try testing.expectEqual(number_node.start_position(), Point{ .row = 3, .column = 2 });
    try testing.expectEqual(number_node.end_position(), Point{ .row = 3, .column = 5 });

    try testing.expectEqual(false_node.start_byte(), std.mem.indexOf(u8, JSON_EXAMPLE, "false").?);
    try testing.expectEqual(false_node.end_byte(), std.mem.indexOf(u8, JSON_EXAMPLE, "false").? + 5);
    try testing.expectEqual(false_node.start_position(), Point{ .row = 4, .column = 2 });
    try testing.expectEqual(false_node.end_position(), Point{ .row = 4, .column = 7 });

    try testing.expectEqual(object_node.start_byte(), std.mem.indexOf(u8, JSON_EXAMPLE, "{").?);
    try testing.expectEqual(object_node.start_position(), Point{ .row = 5, .column = 2 });
    try testing.expectEqual(object_node.end_position(), Point{ .row = 7, .column = 3 });
    try testing.expectEqual(object_node.child_count(), 3);

    const left_brace_node = object_node.child(0).?;
    const pair_node = object_node.child(1).?;
    const right_brace_node = object_node.child(2).?;

    try testing.expectEqualStrings(left_brace_node.kind(), "{");
    try testing.expectEqualStrings(pair_node.kind(), "pair");
    try testing.expectEqualStrings(right_brace_node.kind(), "}");

    try testing.expect(!left_brace_node.is_named());
    try testing.expect(pair_node.is_named());
    try testing.expect(!right_brace_node.is_named());

    try testing.expectEqual(pair_node.start_byte(), std.mem.indexOf(u8, JSON_EXAMPLE, "\"x\"").?);
    try testing.expectEqual(pair_node.end_byte(), std.mem.indexOf(u8, JSON_EXAMPLE, "null").? + 4);
    try testing.expectEqual(pair_node.start_position(), Point{ .row = 6, .column = 4 });
    try testing.expectEqual(pair_node.end_position(), Point{ .row = 6, .column = 13 });
    try testing.expectEqual(pair_node.child_count(), 3);

    const string_node = pair_node.child(0).?;
    const colon_node = pair_node.child(1).?;
    const null_node = pair_node.child(2).?;

    try testing.expectEqualStrings(string_node.kind(), "string");
    try testing.expectEqualStrings(colon_node.kind(), ":");
    try testing.expectEqualStrings(null_node.kind(), "null");

    try testing.expect(string_node.is_named());
    try testing.expect(!colon_node.is_named());
    try testing.expect(null_node.is_named());

    try testing.expectEqual(string_node.start_byte(), std.mem.indexOf(u8, JSON_EXAMPLE, "\"x\"").?);
    try testing.expectEqual(string_node.end_byte(), std.mem.indexOf(u8, JSON_EXAMPLE, "\"x\"").? + 3);
    try testing.expectEqual(string_node.start_position(), Point{ .row = 6, .column = 4 });
    try testing.expectEqual(string_node.end_position(), Point{ .row = 6, .column = 7 });

    try testing.expectEqual(null_node.start_byte(), std.mem.indexOf(u8, JSON_EXAMPLE, "null").?);
    try testing.expectEqual(null_node.end_byte(), std.mem.indexOf(u8, JSON_EXAMPLE, "null").? + 4);
    try testing.expectEqual(null_node.start_position(), Point{ .row = 6, .column = 9 });
    try testing.expectEqual(null_node.end_position(), Point{ .row = 6, .column = 13 });

    try testing.expectEqual(string_node.parent().?, pair_node);
    try testing.expectEqual(null_node.parent().?, pair_node);
    try testing.expectEqual(null_node.parent().?, pair_node);
    try testing.expectEqual(pair_node.parent().?, object_node);
    try testing.expectEqual(number_node.parent().?, array_node);
    try testing.expectEqual(false_node.parent().?, array_node);
    try testing.expectEqual(object_node.parent().?, array_node);
    try testing.expectEqual(array_node.parent().?, tree.root_node());
    try testing.expectEqual(tree.root_node().parent(), null);
}

test "children()" {
    const JSON = try load_except_recompile("json", false, false);

    var parser = Parser.init();
    defer parser.deinit();
    try testing.expectEqual(parser.set_language(JSON), true);

    const tree = parser.parse_string(JSON_EXAMPLE, null);
    var cursor = tree.walk();
    const array_node = tree.root_node().child(0).?;

    const array_children = try array_node.children(allocator, &cursor);
    var kinds = std.ArrayList([]const u8).init(allocator);
    for (array_children.items) |child| {
        try kinds.append(child.kind());
    }
    const array_expected = [_][]const u8{ "[", "number", ",", "false", ",", "object", "]" };
    for (0..kinds.items.len) |i| {
        try testing.expectEqualStrings(kinds.items[i], array_expected[i]);
    }

    const named_children = try array_node.named_children(allocator, &cursor);
    const object_node = for (named_children.items) |child| {
        if (std.mem.eql(u8, child.kind(), "object")) {
            break child;
        }
    } else unreachable;
    const object_children = try object_node.children(allocator, &cursor);
    kinds = std.ArrayList([]const u8).init(allocator);
    for (object_children.items) |child| {
        try kinds.append(child.kind());
    }
    const object_expected = [_][]const u8{ "{", "pair", "}" };
    for (0..kinds.items.len) |i| {
        try testing.expectEqualStrings(kinds.items[i], object_expected[i]);
    }
}

test "children_by_field_name()" {
    const PYTHON = try load_except_recompile("python", true, false);

    var parser = Parser.init();
    defer parser.deinit();
    try testing.expectEqual(parser.set_language(PYTHON), true);

    const source =
        \\
        \\    if one:
        \\        a()
        \\    elif two:
        \\        b()
        \\    elif three:
        \\        c()
        \\    elif four:
        \\        d()
    ;

    const tree = parser.parse_string(source, null);
    defer tree.deinit();
    const node = tree.root_node().child(0).?;
    try testing.expectEqualStrings(node.kind(), "if_statement");
    var cursor = tree.walk();
    const alternatives = try node.children_by_field_name(allocator, "alternative", &cursor);
    var alternative_texts = std.ArrayList([]const u8).init(allocator);
    for (alternatives.items) |n| {
        const byte_range = n.child_by_field_name("condition").?.byte_range();
        try alternative_texts.append(source[byte_range[0]..byte_range[1]]);
    }

    const expected = [_][]const u8{ "two", "three", "four" };
    for (0..expected.len) |i| {
        try testing.expectEqualStrings(alternative_texts.items[i], expected[i]);
    }
}

test "parent_of_child_by_field_name" {
    const JavaScript = try load_except_recompile("javascript", true, false);

    var parser = Parser.init();
    defer parser.deinit();
    try testing.expectEqual(parser.set_language(JavaScript), true);

    const tree = parser.parse_string("foo(a().b[0].c.d.e())", null);
    defer tree.deinit();
    const call_node = tree.root_node().named_child(0).?.named_child(0).?;
    try testing.expectEqualStrings(call_node.kind(), "call_expression");

    // Regression test - when a field points to a hidden node (in this case, `_expression`)
    // the hidden node should not be added to the node parent cache.
    try testing.expectEqual(call_node.child_by_field_name("function").?.parent().?, call_node);
}

test "field_name_for_child()" {
    const C = try load_except_recompile("c", false, false);

    var parser = Parser.init();
    defer parser.deinit();
    try testing.expectEqual(parser.set_language(C), true);

    const tree = parser.parse_string("int w = x + y;", null);
    defer tree.deinit();

    const translation_unit_node = tree.root_node();

    const declaration_node = translation_unit_node.named_child(0).?;

    const binary_expression_node = declaration_node.child_by_field_name("declarator").?.child_by_field_name("value").?;

    try testing.expectEqualStrings(binary_expression_node.field_name_for_child(0).?, "left");
    try testing.expectEqualStrings(binary_expression_node.field_name_for_child(1).?, "operator");
    try testing.expectEqualStrings(binary_expression_node.field_name_for_child(2).?, "right");
    try testing.expectEqual(binary_expression_node.field_name_for_child(3), null);
}

test "child_by_field_name() with extra hidden children" {
    const Python = try load_except_recompile("python", true, false);

    var parser = Parser.init();
    defer parser.deinit();
    try testing.expectEqual(parser.set_language(Python), true);

    const tree = parser.parse_string("while a:\n  pass", null);
    defer tree.deinit();
    const while_node = tree.root_node().child(0).?;
    try testing.expectEqualStrings(while_node.kind(), "while_statement");
    try testing.expectEqual(while_node.child_by_field_name("body").?, while_node.child(3).?);
}

test "named_child()" {
    const JSON = try load_except_recompile("json", false, false);

    var parser = Parser.init();
    defer parser.deinit();
    try testing.expectEqual(parser.set_language(JSON), true);

    const tree = parser.parse_string(JSON_EXAMPLE, null);
    defer tree.deinit();
    const array_node = tree.root_node().child(0).?;

    const number_node = array_node.named_child(0).?;
    const false_node = array_node.named_child(1).?;
    const object_node = array_node.named_child(2).?;

    try testing.expectEqualStrings(number_node.kind(), "number");
    try testing.expectEqual(number_node.start_byte(), std.mem.indexOf(u8, JSON_EXAMPLE, "123").?);
    try testing.expectEqual(number_node.end_byte(), std.mem.indexOf(u8, JSON_EXAMPLE, "123").? + 3);
    try testing.expectEqual(number_node.start_position(), Point{ .row = 3, .column = 2 });
    try testing.expectEqual(number_node.end_position(), Point{ .row = 3, .column = 5 });

    try testing.expectEqualStrings(false_node.kind(), "false");
    try testing.expectEqual(false_node.start_byte(), std.mem.indexOf(u8, JSON_EXAMPLE, "false").?);
    try testing.expectEqual(false_node.end_byte(), std.mem.indexOf(u8, JSON_EXAMPLE, "false").? + 5);
    try testing.expectEqual(false_node.start_position(), Point{ .row = 4, .column = 2 });
    try testing.expectEqual(false_node.end_position(), Point{ .row = 4, .column = 7 });

    try testing.expectEqualStrings(object_node.kind(), "object");
    try testing.expectEqual(object_node.start_byte(), std.mem.indexOf(u8, JSON_EXAMPLE, "{").?);
    try testing.expectEqual(object_node.start_position(), Point{ .row = 5, .column = 2 });
    try testing.expectEqual(object_node.end_position(), Point{ .row = 7, .column = 3 });
    try testing.expectEqual(object_node.named_child_count(), 1);

    const pair_node = object_node.named_child(0).?;
    try testing.expectEqualStrings(pair_node.kind(), "pair");
    try testing.expectEqual(pair_node.start_byte(), std.mem.indexOf(u8, JSON_EXAMPLE, "\"x\"").?);
    try testing.expectEqual(pair_node.end_byte(), std.mem.indexOf(u8, JSON_EXAMPLE, "null").? + 4);
    try testing.expectEqual(pair_node.start_position(), Point{ .row = 6, .column = 4 });
    try testing.expectEqual(pair_node.end_position(), Point{ .row = 6, .column = 13 });

    const string_node = pair_node.named_child(0).?;
    const null_node = pair_node.named_child(1).?;

    try testing.expectEqualStrings(string_node.kind(), "string");
    try testing.expectEqualStrings(null_node.kind(), "null");

    try testing.expectEqual(string_node.start_byte(), std.mem.indexOf(u8, JSON_EXAMPLE, "\"x\"").?);
    try testing.expectEqual(string_node.end_byte(), std.mem.indexOf(u8, JSON_EXAMPLE, "\"x\"").? + 3);
    try testing.expectEqual(string_node.start_position(), Point{ .row = 6, .column = 4 });
    try testing.expectEqual(string_node.end_position(), Point{ .row = 6, .column = 7 });

    try testing.expectEqual(null_node.start_byte(), std.mem.indexOf(u8, JSON_EXAMPLE, "null").?);
    try testing.expectEqual(null_node.end_byte(), std.mem.indexOf(u8, JSON_EXAMPLE, "null").? + 4);
    try testing.expectEqual(null_node.start_position(), Point{ .row = 6, .column = 9 });
    try testing.expectEqual(null_node.end_position(), Point{ .row = 6, .column = 13 });

    try testing.expectEqual(string_node.parent().?, pair_node);
    try testing.expectEqual(null_node.parent().?, pair_node);
    try testing.expectEqual(pair_node.parent().?, object_node);
    try testing.expectEqual(number_node.parent().?, array_node);
    try testing.expectEqual(false_node.parent().?, array_node);
    try testing.expectEqual(object_node.parent().?, array_node);
    try testing.expectEqual(array_node.parent().?, tree.root_node());
    try testing.expectEqual(tree.root_node().parent(), null);
}

test "descendant_count()" {
    const JSON = try load_except_recompile("json", false, false);

    var parser = Parser.init();
    defer parser.deinit();
    try testing.expectEqual(parser.set_language(JSON), true);

    const tree = parser.parse_string(JSON_EXAMPLE, null);
    defer tree.deinit();

    const value_node = tree.root_node();
    const all_nodes = try get_all_nodes(tree);
    defer all_nodes.deinit();

    try testing.expectEqual(value_node.descendant_count(), all_nodes.items.len);

    var cursor = value_node.walk();
    for (all_nodes.items, 0..) |node, i| {
        cursor.goto_descendant(i);
        try testing.expectEqual(cursor.node(), node);
    }

    var size = all_nodes.items.len - 1;
    while (size > 0) {
        cursor.goto_descendant(size);
        try testing.expectEqual(cursor.node(), all_nodes.items[size]);
        size -= 1;
    }
}

test "descendant_count() with a single node tree" {
    const EmbeddedTemplate = try load_except_recompile("embedded-template", false, false);

    var parser = Parser.init();
    defer parser.deinit();
    try testing.expectEqual(parser.set_language(EmbeddedTemplate), true);

    const tree = parser.parse_string("hello", null);

    const nodes = try get_all_nodes(tree);
    defer nodes.deinit();
    try testing.expectEqual(nodes.items.len, 2);
    try testing.expectEqual(tree.root_node().descendant_count(), 2);

    var cursor = tree.root_node().walk();

    cursor.goto_descendant(0);
    try testing.expectEqual(cursor.depth(), 0);
    try testing.expectEqual(cursor.node(), nodes.items[0]);
    cursor.goto_descendant(1);
    try testing.expectEqual(cursor.depth(), 1);
    try testing.expectEqual(cursor.node(), nodes.items[1]);
}

test "descendant_for_(byte|point)_range()" {
    const JSON = try load_except_recompile("json", false, false);

    var parser = Parser.init();
    defer parser.deinit();
    try testing.expectEqual(parser.set_language(JSON), true);

    const tree = parser.parse_string(JSON_EXAMPLE, null);
    defer tree.deinit();
    const array_node = tree.root_node();

    // Leaf node exactly matches the given bounds - byte query
    const colon_index = std.mem.indexOf(u8, JSON_EXAMPLE, ":").?;
    var colon_node = array_node.descendant_for_byte_range(colon_index, colon_index + 1).?;
    try testing.expectEqualStrings(colon_node.kind(), ":");
    try testing.expectEqual(colon_node.start_byte(), colon_index);
    try testing.expectEqual(colon_node.end_byte(), colon_index + 1);
    try testing.expectEqual(colon_node.start_position(), Point{ .row = 6, .column = 7 });
    try testing.expectEqual(colon_node.end_position(), Point{ .row = 6, .column = 8 });

    // Leaf node exactly matches the given bounds - point query
    colon_node = array_node.descendant_for_point_range(Point{ .row = 6, .column = 7 }, Point{ .row = 6, .column = 8 }).?;
    try testing.expectEqualStrings(colon_node.kind(), ":");
    try testing.expectEqual(colon_node.start_byte(), colon_index);
    try testing.expectEqual(colon_node.end_byte(), colon_index + 1);
    try testing.expectEqual(colon_node.start_position(), Point{ .row = 6, .column = 7 });
    try testing.expectEqual(colon_node.end_position(), Point{ .row = 6, .column = 8 });

    // The given point is between two adjacent leaf nodes - byte query
    colon_node = array_node.descendant_for_byte_range(colon_index, colon_index).?;
    try testing.expectEqualStrings(colon_node.kind(), ":");
    try testing.expectEqual(colon_node.start_byte(), colon_index);
    try testing.expectEqual(colon_node.end_byte(), colon_index + 1);
    try testing.expectEqual(colon_node.start_position(), Point{ .row = 6, .column = 7 });
    try testing.expectEqual(colon_node.end_position(), Point{ .row = 6, .column = 8 });

    // The given point is between two adjacent leaf nodes - point query
    colon_node = array_node.descendant_for_point_range(Point{ .row = 6, .column = 7 }, Point{ .row = 6, .column = 7 }).?;
    try testing.expectEqualStrings(colon_node.kind(), ":");
    try testing.expectEqual(colon_node.start_byte(), colon_index);
    try testing.expectEqual(colon_node.end_byte(), colon_index + 1);
    try testing.expectEqual(colon_node.start_position(), Point{ .row = 6, .column = 7 });
    try testing.expectEqual(colon_node.end_position(), Point{ .row = 6, .column = 8 });

    // Leaf node starts at the lower bound, ends after the upper bound - byte query
    const string_index = std.mem.indexOf(u8, JSON_EXAMPLE, "\"x\"").?;
    var string_node = array_node.descendant_for_byte_range(string_index, string_index + 2).?;
    try testing.expectEqualStrings(string_node.kind(), "string");
    try testing.expectEqual(string_node.start_byte(), string_index);
    try testing.expectEqual(string_node.end_byte(), string_index + 3);
    try testing.expectEqual(string_node.start_position(), Point{ .row = 6, .column = 4 });
    try testing.expectEqual(string_node.end_position(), Point{ .row = 6, .column = 7 });

    // Leaf node starts at the lower bound, ends after the upper bound - point query
    string_node = array_node.descendant_for_point_range(Point{ .row = 6, .column = 4 }, Point{ .row = 6, .column = 6 }).?;
    try testing.expectEqualStrings(string_node.kind(), "string");
    try testing.expectEqual(string_node.start_byte(), string_index);
    try testing.expectEqual(string_node.end_byte(), string_index + 3);
    try testing.expectEqual(string_node.start_position(), Point{ .row = 6, .column = 4 });
    try testing.expectEqual(string_node.end_position(), Point{ .row = 6, .column = 7 });

    // Leaf node starts before the lower bound, ends at the upper bound - byte query
    const null_index = std.mem.indexOf(u8, JSON_EXAMPLE, "null").?;
    var null_node = array_node.descendant_for_byte_range(null_index + 1, null_index + 4).?;
    try testing.expectEqualStrings(null_node.kind(), "null");
    try testing.expectEqual(null_node.start_byte(), null_index);
    try testing.expectEqual(null_node.end_byte(), null_index + 4);
    try testing.expectEqual(null_node.start_position(), Point{ .row = 6, .column = 9 });
    try testing.expectEqual(null_node.end_position(), Point{ .row = 6, .column = 13 });

    // Leaf node starts before the lower bound, ends at the upper bound - point query
    null_node = array_node.descendant_for_point_range(Point{ .row = 6, .column = 11 }, Point{ .row = 6, .column = 13 }).?;
    try testing.expectEqualStrings(null_node.kind(), "null");
    try testing.expectEqual(null_node.start_byte(), null_index);
    try testing.expectEqual(null_node.end_byte(), null_index + 4);
    try testing.expectEqual(null_node.start_position(), Point{ .row = 6, .column = 9 });
    try testing.expectEqual(null_node.end_position(), Point{ .row = 6, .column = 13 });

    // The bounds span multiple leaf nodes - return the smallest node that does span it.
    var pair_node = array_node.descendant_for_byte_range(string_index + 2, string_index + 4).?;
    try testing.expectEqualStrings(pair_node.kind(), "pair");
    try testing.expectEqual(pair_node.start_byte(), string_index);
    try testing.expectEqual(pair_node.end_byte(), string_index + 9);
    try testing.expectEqual(pair_node.start_position(), Point{ .row = 6, .column = 4 });
    try testing.expectEqual(pair_node.end_position(), Point{ .row = 6, .column = 13 });
    try testing.expectEqual(colon_node.parent(), pair_node);

    // No leaf spans the given range - return the smallest node that does span it.
    pair_node = array_node.descendant_for_point_range(Point{ .row = 6, .column = 6 }, Point{ .row = 6, .column = 8 }).?;
    try testing.expectEqualStrings(pair_node.kind(), "pair");
    try testing.expectEqual(pair_node.start_byte(), string_index);
    try testing.expectEqual(pair_node.end_byte(), string_index + 9);
    try testing.expectEqual(pair_node.start_position(), Point{ .row = 6, .column = 4 });
    try testing.expectEqual(pair_node.end_position(), Point{ .row = 6, .column = 13 });
}

// test "edit()" {
//     const JSON = try load_except_recompile("json", false);
//
//     var parser = Parser.init();
//     defer parser.deinit();
//     try testing.expectEqual(parser.set_language(JSON), true);
//
//     var tree = parser.parse_string(JSON_EXAMPLE, null);
//     defer tree.deinit();
//
//     var rng = RndGen.init(0);
//     _ = rng;
//
//     for (0..10) |_| {
//         var nodes_before = get_all_nodes(tree);
//
//         var edit = get_random_edit(rng, JSON_EXAMPLE);
//         var tree2 = tree;
//     }
// }

test "root_node_with_offset()" {
    const JavaScript = try load_except_recompile("javascript", true, false);

    var parser = Parser.init();
    defer parser.deinit();
    try testing.expectEqual(parser.set_language(JavaScript), true);

    const source = "  if (a) b";
    const tree = parser.parse_string(source, null);
    defer tree.deinit();

    const node = tree.root_node_with_offset(6, Point{ .row = 2, .column = 2 });
    try testing.expectEqualSlices(usize, &node.byte_range(), &[2]usize{ 8, 16 });
    try testing.expectEqual(node.start_position(), Point{ .row = 2, .column = 4 });
    try testing.expectEqual(node.end_position(), Point{ .row = 2, .column = 12 });

    var child = node.child(0).?.child(2).?;
    try testing.expectEqualStrings(child.kind(), "expression_statement");
    try testing.expectEqualSlices(usize, &child.byte_range(), &[2]usize{ 15, 16 });
    try testing.expectEqual(child.start_position(), Point{ .row = 2, .column = 11 });
    try testing.expectEqual(child.end_position(), Point{ .row = 2, .column = 12 });

    var cursor = node.walk();
    _ = cursor.goto_first_child();
    _ = cursor.goto_first_child();
    _ = cursor.goto_next_sibling();
    child = cursor.node();
    try testing.expectEqualStrings(child.kind(), "parenthesized_expression");
    try testing.expectEqualSlices(usize, &child.byte_range(), &[2]usize{ 11, 14 });
    try testing.expectEqual(child.start_position(), Point{ .row = 2, .column = 7 });
    try testing.expectEqual(child.end_position(), Point{ .row = 2, .column = 10 });
}

test "is_extra()" {
    const JavaScript = try load_except_recompile("javascript", true, false);

    var parser = Parser.init();
    defer parser.deinit();
    try testing.expectEqual(parser.set_language(JavaScript), true);

    const source = "foo(/* hi */)";
    const tree = parser.parse_string(source, null);
    defer tree.deinit();

    const root_node = tree.root_node();
    const comment_node = root_node.descendant_for_byte_range(7, 7).?;

    try testing.expectEqualStrings(root_node.kind(), "program");
    try testing.expectEqualStrings(comment_node.kind(), "comment");
    try testing.expectEqual(root_node.is_extra(), false);
    try testing.expectEqual(comment_node.is_extra(), true);
}

test "to_sexp()" {
    const JavaScript = try load_except_recompile("javascript", true, false);

    var parser = Parser.init();
    defer parser.deinit();
    try testing.expectEqual(parser.set_language(JavaScript), true);

    const tree = parser.parse_string("if (a) b", null);
    defer tree.deinit();
    const root_node = tree.root_node();
    const if_node = root_node.descendant_for_byte_range(0, 0).?;
    const paren_node = root_node.descendant_for_byte_range(3, 3).?;
    const identifier_node = root_node.descendant_for_byte_range(4, 4).?;

    try testing.expectEqualStrings(if_node.kind(), "if");
    try testing.expectEqualStrings(if_node.to_sexp(), "(\"if\")");
    try testing.expectEqualStrings(paren_node.kind(), "(");
    try testing.expectEqualStrings(paren_node.to_sexp(), "(\"(\")");
    try testing.expectEqualStrings(identifier_node.kind(), "identifier");
    try testing.expectEqualStrings(identifier_node.to_sexp(), "(identifier)");
}

test "numeric_symbols_respect_simple_aliases" {
    const Python = try load_except_recompile("python", true, false);

    var parser = Parser.init();
    defer parser.deinit();
    try testing.expectEqual(parser.set_language(Python), true);

    // Example 1:
    // Python argument lists can contain "splat" arguments, which are not allowed within
    // other expressions. This includes `parenthesized_list_splat` nodes like `(*b)`. These
    // `parenthesized_list_splat` nodes are aliased as `parenthesized_expression`. Their numeric
    // `symbol`, aka `kind_id` should match that of a normal `parenthesized_expression`.
    var tree = parser.parse_string("(a((*b)))", null);
    var root = tree.root_node();
    try testing.expectEqualStrings(root.to_sexp(), "(module (expression_statement (parenthesized_expression (call function: (identifier) arguments: (argument_list (parenthesized_expression (list_splat (identifier))))))))");

    const outer_expr_node = root.child(0).?.child(0).?;
    try testing.expectEqualStrings(outer_expr_node.kind(), "parenthesized_expression");

    const inner_expr_node = outer_expr_node.named_child(0).?.child_by_field_name("arguments").?.named_child(0).?;
    try testing.expectEqualStrings(inner_expr_node.kind(), "parenthesized_expression");
    try testing.expectEqual(inner_expr_node.kind_id(), outer_expr_node.kind_id());

    // Example 2:
    // Ruby handles the unary (negative) and binary (minus) `-` operators using two different
    // tokens. One or more of these is an external token that's aliased as `-`. Their numeric
    // kind ids should match.
    const Ruby = try load_except_recompile("ruby", true, true);

    try testing.expectEqual(parser.set_language(Ruby), true);
    tree = parser.parse_string("-a - b", null);
    root = tree.root_node();
    try testing.expectEqualStrings(root.to_sexp(), "(program (binary left: (unary operand: (identifier)) right: (identifier)))");

    const binary_node = root.child(0).?;
    try testing.expectEqualStrings(binary_node.kind(), "binary");

    const unary_minus_node = binary_node.child_by_field_name("left").?.child(0).?;
    try testing.expectEqualStrings(unary_minus_node.kind(), "-");

    const binary_minus_node = binary_node.child_by_field_name("operator").?;
    try testing.expectEqualStrings(binary_minus_node.kind(), "-");
    try testing.expectEqual(binary_minus_node.kind_id(), unary_minus_node.kind_id());
}
