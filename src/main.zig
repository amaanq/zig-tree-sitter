const std = @import("std");
const testing = std.testing;

const c = @cImport({
    @cInclude("tree_sitter/api.h");
    @cInclude("dlfcn.h");
});

pub const TSStateId = c.TSStateId;
pub const TSSymbol = c.TSSymbol;
pub const TSFieldId = c.TSFieldId;

pub const TSSymbolType = enum(c.TSSymbolType) {
    Regular,
    Anonymous,
    Auxiliary,
};

pub const LanguageOpts = union(enum) {
    paths: struct {
        input: []const []const u8,
        output: []const u8,
    },
    lib_path: []const u8,
};

pub const Language = struct {
    inner: ?*const c.TSLanguage,
    name: []const u8,

    const LanguageInitError = error{
        NoPaths,
        TooManyPaths,
        CompileFailed,
        InvalidLib,
        InvalidPtr,
    };

    pub fn init(allocator: std.mem.Allocator, name: []const u8, opts: LanguageOpts) LanguageInitError!Language {
        switch (opts) {
            .paths => |paths| {
                if (paths.input.len == 0) {
                    return LanguageInitError.NoPaths;
                } else if (paths.input.len > 2) {
                    return LanguageInitError.TooManyPaths;
                }

                const parser_path = paths.input[0];
                const scanner_path = if (paths.input.len == 2) paths.input[1] else null;

                var args = std.ArrayList([]const u8).init(allocator);
                defer args.deinit();

                var lib_path = paths.output;
                var src_path = std.fs.path.dirname(parser_path) orelse return LanguageInitError.InvalidPtr;

                args.appendSlice(&[_][]const u8{
                    "zig",
                    "c++",
                    "-o",
                    lib_path,
                    parser_path,
                    "-lc",
                    "-I",
                    src_path,
                    "-shared",
                }) catch {
                    return LanguageInitError.CompileFailed;
                };

                if (scanner_path != null) {
                    args.appendSlice(&[_][]const u8{
                        scanner_path.?,
                    }) catch {
                        return LanguageInitError.CompileFailed;
                    };
                }

                var child = std.process.Child.init(args.items, allocator);
                switch (child.spawnAndWait() catch {
                    return LanguageInitError.CompileFailed;
                }) {
                    inline else => |e| if (e != 0) {
                        std.debug.print("child exited with error code {d}\n", .{e});
                        std.debug.print("stdout: {?}\n", .{child.stdout});
                        return LanguageInitError.CompileFailed;
                    },
                }

                const ptr = extract_ptr(allocator, lib_path, name, true);
                if (ptr == null) {
                    return LanguageInitError.InvalidLib;
                }
                return Language{
                    .inner = ptr,
                    .name = name,
                };
            },
            .lib_path => |lib_path| {
                const ptr = extract_ptr(allocator, lib_path, name, false);
                if (ptr == null) {
                    return LanguageInitError.InvalidLib;
                }
                return Language{
                    .inner = ptr,
                    .name = name,
                };
            },
        }
    }

    fn extract_ptr(allocator: std.mem.Allocator, _lib_path: []const u8, language_name: []const u8, print_fail: bool) ?*c.TSLanguage {
        const lib_path = allocator.dupeZ(u8, _lib_path) catch return null;
        defer allocator.free(lib_path);

        const lib = std.c.dlopen(lib_path, 1);
        if (lib == null) {
            const err = @as(?[*c]const u8, c.dlerror());
            if (err != null and print_fail) {
                std.debug.print("dlopen failed: {s}\n", .{err.?});
            }
            return null;
        }

        // replace - with _ in language name
        var name: []u8 = allocator.dupe(u8, language_name) catch return null;
        defer allocator.free(name);
        for (0..name.len) |i| {
            if (name[i] == '-') {
                name[i] = '_';
            }
        }
        const _init_fn_name = std.fmt.allocPrint(allocator, "tree_sitter_{s}", .{name}) catch return null;
        defer allocator.free(_init_fn_name);
        const init_fn_name = allocator.dupeZ(u8, _init_fn_name) catch return null;
        defer allocator.free(init_fn_name);

        const symbol = std.c.dlsym(lib, init_fn_name) orelse return null;
        const init_fn = @as(*const fn () callconv(.C) *c.TSLanguage, @ptrFromInt(@intFromPtr(symbol)));
        const language = init_fn();

        return language;
    }

    pub fn from_language_pointer(name: []const u8, ptr: ?*const c.TSLanguage) Language {
        return Language{
            .inner = ptr,
            .name = name,
        };
    }

    /// Get the ABI version number that indicates which version of the Tree-sitter CLI
    /// that was used to generate this [`Language`].
    pub fn version(self: *Language) u32 {
        return c.ts_language_version(self.inner);
    }

    /// Get the number of distinct node types in this language.
    pub fn node_kind_count(self: *Language) u32 {
        return c.ts_language_symbol_count(self.inner);
    }

    /// Get the number of valid states in this language.
    pub fn parse_state_count(self: *Language) u32 {
        return c.ts_language_state_count(self.inner);
    }

    /// Get the name of the node kind for the given numerical id.
    pub fn node_kind_for_id(self: *Language, id: TSSymbol) ?[]const u8 {
        const c_name = c.ts_language_symbol_name(self.inner, id);
        if (c_name == null) {
            return null;
        }
        return std.mem.span(c_name);
    }

    /// Get the numerical id for the given node kind.
    pub fn id_for_node_kind(self: *Language, kind: []const u8, named: bool) TSSymbol {
        return c.ts_language_symbol_for_name(self.inner, kind.ptr, kind.len, named);
    }

    /// Check if the node type for the given numerical id is named (as opposed
    /// to an anonymous node type).
    pub fn node_kind_is_named(self: *Language, id: TSSymbol) bool {
        return c.ts_language_symbol_type(self.inner, id) == TSSymbolType.Regular;
    }

    pub fn node_kind_is_visible(self: *Language, id: TSSymbol) bool {
        return c.ts_language_symbol_type(self.inner, id) <= TSSymbolType.Anonymous;
    }

    /// Get the number of distinct field names in this language.
    pub fn field_count(self: *Language) u32 {
        return c.ts_language_field_count(self.inner);
    }

    /// Get the field names for the given numerical id.
    pub fn field_name_for_id(self: *Language, field_id: TSFieldId) ?[]const u8 {
        const c_field_name = c.ts_language_field_name_for_id(self.inner, field_id);
        if (c_field_name == null) {
            return null;
        }
        return std.mem.span(c_field_name);
    }

    /// Get the numerical id for the given field name.
    pub fn field_id_for_name(self: Language, name: []const u8) TSFieldId {
        return c.ts_language_field_id_for_name(self.inner, name.ptr, @intCast(name.len));
    }

    /// Get the next parse state. Combine this with
    /// [`lookahead_iterator`](Language::lookahead_iterator) to
    /// generate completion suggestions or valid symbols in error nodes.
    pub fn next_state(self: *Language, state: TSStateId, symbol: TSSymbol) TSStateId {
        return c.ts_language_next_state(self.inner, state, symbol);
    }

    // TODO: lookahead_iterator
};

pub const LogType = enum {
    Parse,
    Lex,
};

/// A callback that receives log messages during parser.
pub const Logger = fn (LogType, []const u8) void;

pub const Parser = struct {
    inner: ?*c.TSParser,
    name: []const u8,

    pub fn init() Parser {
        return Parser{
            .inner = c.ts_parser_new(),
            .name = "",
        };
    }

    pub fn deinit(self: *Parser) void {
        // self.stop_printing_dot_graphs();
        // self.set_logger(NULL);
        c.ts_parser_delete(self.inner);
    }

    pub fn set_language(self: *Parser, lang: Language) bool {
        const result = c.ts_parser_set_language(self.inner, lang.inner);
        self.name = lang.name;
        return result;
    }

    pub fn language(self: Parser) ?Language {
        return Language.from_language_pointer(self.name, c.ts_parser_language(self.inner));
    }

    pub fn logger(self: Parser) ?Logger {
        const _logger = c.ts_parser_logger(self.inner);
        if (_logger == null) {
            return null;
        }
        return Logger.init(logger);
    }

    fn log(payload: ?*anyopaque, c_log_type: c.TSLogType, c_message: [*c]u8) callconv(.C) void {
        const callback = @as(*Logger, @ptrFromInt(payload)) orelse return;
        const message_len = std.mem.len(c_message);
        const message = @as([]const u8, @ptrFromInt(c_message))[0..message_len];
        const log_type = switch (c_log_type) {
            c.TSLogTypeParse => LogType.Parse,
            c.TSLogTypeLex => LogType.Lex,
            else => unreachable,
        };
        callback(log_type, message);
    }

    pub fn set_logger(self: Parser, allocator: std.mem.Allocator, comptime ts_logger: ?Logger) void {
        const prev_logger = c.ts_parser_logger(self.inner);
        if (prev_logger.payload != null) {
            const old_logger = @as(*Logger, @alignCast(@ptrCast(prev_logger.payload)));
            allocator.destroy(old_logger);
            allocator.free(old_logger);
        }

        var c_logger: c.TSLogger = undefined;
        if (ts_logger) |log_callback| {
            const logger_ptr: *const Logger = allocator.create(Logger) catch return;
            logger_ptr.* = log_callback;

            c_logger = c.TSLogger{
                .payload = logger_ptr,
                .log = log,
            };
        } else {
            c_logger = c.TSLogger{
                .payload = null,
                .log = null,
            };
        }

        c.ts_parser_set_logger(self.inner, c_logger);
    }

    pub fn parse_string(self: *const Parser, input: []const u8, old_tree: ?*c.TSTree) Tree {
        const tree = c.ts_parser_parse_string(
            self.inner,
            old_tree,
            input.ptr,
            @intCast(input.len),
        );

        return Tree.init(tree);
    }

    fn callback_fn(payload: ?*anyopaque, byte_index: u32, position: c.TSPoint, bytes_read: [*c]u32) ?*const u8 {
        const c_callback: ?*const fn (?*anyopaque, u32, c.TSPoint, [*c]u32) callconv(.C) [*c]const u8 = @as(?*const fn (?*anyopaque, u32, c.TSPoint, [*c]u32) callconv(.C) [*c]const u8, payload);
        const result = c_callback(payload, byte_index, position, bytes_read);
        return result;
    }

    // TODO: fix
    pub fn parse(self: *Parser, input: []const u8, old_tree: ?*c.TSTree) ?*const Tree {
        _ = input;
        var callback: ?*const fn (?*anyopaque, u32, c.TSPoint, [*c]u32) callconv(.C) [*c]const u8 = callback_fn;

        var ts_input = c.TSInput{
            .payload = null,
            .read = callback,
        };

        return c.ts_parser_parse(
            self.inner,
            old_tree,
            ts_input,
        );
    }

    pub fn reset(self: *Parser) void {
        c.ts_parser_reset(self.inner);
    }

    pub fn timeout_micros(self: *Parser) u64 {
        return c.ts_parser_timeout_micros(self.inner);
    }

    pub fn set_timeout_micros(self: *Parser, timeout: u64) void {
        c.ts_parser_set_timeout_micros(self.inner, timeout);
    }

    pub fn set_included_ranges(self: *Parser, allocator: std.mem.Allocator, ranges: []Range) bool {
        var c_ranges: []c.TSRange = allocator.alloc(c.TSRange, ranges.len) catch return false;
        defer allocator.free(c_ranges);
        for (0..ranges.len) |i| {
            c_ranges[i] = c.TSRange{
                .start_byte = ranges[i].start_byte,
                .end_byte = ranges[i].end_byte,
                .start_point = ranges[i].start_point.into(),
                .end_point = ranges[i].end_point.into(),
            };
        }
        return c.ts_parser_set_included_ranges(self.inner, &c_ranges[0], @intCast(ranges.len));
    }

    pub fn cancellation_flag(self: *Parser) [*c]const usize {
        return c.ts_parser_cancellation_flag(self.inner);
    }

    pub fn set_cancellation_flag(self: *Parser, flag: [*c]const usize) void {
        c.ts_parser_set_cancellation_flag(self.inner, flag);
    }
};

pub const Tree = struct {
    inner: ?*c.TSTree,

    fn init(tree: ?*c.TSTree) Tree {
        return Tree{
            .inner = tree,
        };
    }

    pub fn deinit(self: Tree) void {
        c.ts_tree_delete(self.inner);
    }

    pub fn is_none(self: Tree) bool {
        return self.inner == null;
    }

    pub fn root_node(self: Tree) Node {
        return Node.init(c.ts_tree_root_node(self.inner)).?;
    }

    pub fn root_node_with_offset(self: Tree, offset_bytes: usize, offset_extent: Point) Node {
        return Node.init(c.ts_tree_root_node_with_offset(self.inner, @intCast(offset_bytes), offset_extent.into())).?;
    }

    pub fn language(self: Tree) Language {
        return Language.from_language_pointer("", c.ts_tree_language(self.inner));
    }

    pub fn edit(self: *Tree, input_edit: InputEdit) void {
        c.ts_tree_edit(self.inner, input_edit.inner);
    }

    pub fn walk(self: Tree) TreeCursor {
        return self.root_node().walk();
    }

    pub fn changed_ranges(self: Tree, other: Tree) []Range {
        var result = []Range{0};
        const c_ranges = c.ts_tree_get_changed_ranges(self.inner, other.inner);
        for (0..c_ranges.size) |i| {
            result.append(Range{
                .start_byte = c_ranges.contents[i].start_byte,
                .end_byte = c_ranges.contents[i].end_byte,
                .start_point = Point{
                    .row = c_ranges.contents[i].start_point.row,
                    .column = c_ranges.contents[i].start_point.column,
                },
                .end_point = Point{
                    .row = c_ranges.contents[i].end_point.row,
                    .column = c_ranges.contents[i].end_point.column,
                },
            });
        }
        return result;
    }

    pub fn included_ranges(self: Tree, allocator: std.mem.Allocator) std.mem.Allocator.Error!std.ArrayList(Range) {
        var result = std.ArrayList(Range).init(allocator);
        var count: u32 = 0;
        const c_ranges = c.ts_tree_included_ranges(self.inner, &count);
        for (0..count) |i| {
            try result.append(Range{
                .start_byte = c_ranges[i].start_byte,
                .end_byte = c_ranges[i].end_byte,
                .start_point = Point{
                    .row = c_ranges[i].start_point.row,
                    .column = c_ranges[i].start_point.column,
                },
                .end_point = Point{
                    .row = c_ranges[i].end_point.row,
                    .column = c_ranges[i].end_point.column,
                },
            });
        }
        return result;
    }
};

pub const Node = struct {
    inner: c.TSNode,

    fn init(node: c.TSNode) ?Node {
        if (node.id == c.NULL) {
            return null;
        }
        return Node{
            .inner = node,
        };
    }

    pub fn id(self: Node) usize {
        return @intFromPtr(self.inner.id);
    }

    pub fn kind_id(self: Node) TSSymbol {
        return c.ts_node_symbol(self.inner);
    }

    pub fn grammar_id(self: Node) TSSymbol {
        return c.ts_node_grammar_symbol(self.inner);
    }

    pub fn kind(self: Node) []const u8 {
        return std.mem.span(c.ts_node_type(self.inner));
    }

    pub fn grammar_name(self: Node) []const u8 {
        return std.mem.span(c.ts_node_grammar_type(self.inner));
    }

    pub fn language(self: Node) ?Language {
        return Language.from_language_pointer("", c.ts_node_language(self.inner));
    }

    pub fn is_named(self: Node) bool {
        return c.ts_node_is_named(self.inner);
    }

    pub fn is_extra(self: Node) bool {
        return c.ts_node_is_extra(self.inner);
    }

    pub fn has_changes(self: Node) bool {
        return c.ts_node_has_changes(self.inner);
    }

    pub fn has_error(self: Node) bool {
        return c.ts_node_has_error(self.inner);
    }

    pub fn is_error(self: Node) bool {
        return c.ts_node_is_error(self.inner);
    }

    pub fn parse_state(self: Node) TSStateId {
        return c.ts_node_parse_state(self.inner);
    }

    pub fn next_parse_state(self: Node) TSStateId {
        return c.ts_node_next_parse_state(self.inner);
    }

    pub fn is_missing(self: Node) bool {
        return c.ts_node_is_missing(self.inner);
    }

    pub fn start_byte(self: Node) usize {
        return c.ts_node_start_byte(self.inner);
    }

    pub fn end_byte(self: Node) usize {
        return c.ts_node_end_byte(self.inner);
    }

    pub fn byte_range(self: Node) [2]usize {
        return [2]usize{
            self.start_byte(),
            self.end_byte(),
        };
    }

    pub fn range(self: Node) Range {
        return Range{
            .start_byte = @intCast(self.start_byte()),
            .end_byte = @intCast(self.end_byte()),
            .start_point = self.start_position(),
            .end_point = self.end_position(),
        };
    }

    pub fn start_position(self: Node) Point {
        const ts_point = c.ts_node_start_point(self.inner);
        return Point{
            .row = ts_point.row,
            .column = ts_point.column,
        };
    }

    pub fn end_position(self: Node) Point {
        const ts_point = c.ts_node_end_point(self.inner);
        return Point{
            .row = ts_point.row,
            .column = ts_point.column,
        };
    }

    pub fn child(self: Node, i: usize) ?Node {
        const node_child = c.ts_node_child(self.inner, @intCast(i));
        return Node.init(node_child);
    }

    pub fn child_count(self: Node) usize {
        return c.ts_node_child_count(self.inner);
    }

    pub fn named_child(self: Node, i: usize) ?Node {
        const node_child = c.ts_node_named_child(self.inner, @intCast(i));
        return Node.init(node_child);
    }

    pub fn named_child_count(self: Node) usize {
        return c.ts_node_named_child_count(self.inner);
    }

    pub fn child_by_field_name(self: Node, field_name: []const u8) ?Node {
        const node_child = c.ts_node_child_by_field_name(self.inner, field_name.ptr, @intCast(field_name.len));
        return Node.init(node_child);
    }

    pub fn child_by_field_id(self: Node, field_id: TSFieldId) ?Node {
        const node_child = c.ts_node_child_by_field_id(self.inner, field_id);
        return Node.init(node_child);
    }

    pub fn field_name_for_child(self: Node, child_index: u32) ?[]const u8 {
        const c_field_name = c.ts_node_field_name_for_child(self.inner, child_index);
        if (c_field_name == null) {
            return null;
        }
        return std.mem.span(c_field_name);
    }

    pub fn children(self: Node, allocator: std.mem.Allocator, cursor: *TreeCursor) std.mem.Allocator.Error!std.ArrayList(Node) {
        cursor.reset(self);
        _ = cursor.goto_first_child();
        var nodes = std.ArrayList(Node).init(allocator);
        for (0..self.child_count()) |_| {
            try nodes.append(cursor.node());
            _ = cursor.goto_next_sibling();
        }
        return nodes;
    }

    pub fn named_children(self: Node, allocator: std.mem.Allocator, cursor: *TreeCursor) std.mem.Allocator.Error!std.ArrayList(Node) {
        cursor.reset(self);
        _ = cursor.goto_first_child();
        var nodes = std.ArrayList(Node).init(allocator);
        for (0..self.named_child_count()) |_| {
            while (!cursor.node().is_named()) {
                if (!cursor.goto_next_sibling()) {
                    break;
                }
            }
            try nodes.append(cursor.node());
            _ = cursor.goto_next_sibling();
        }
        return nodes;
    }

    pub fn children_by_field_name(self: Node, allocator: std.mem.Allocator, field_name: []const u8, cursor: *TreeCursor) std.mem.Allocator.Error!std.ArrayList(Node) {
        const field_id = self.language().?.field_id_for_name(field_name);
        var done = field_id == 0;
        if (!done) {
            cursor.reset(self);
            _ = cursor.goto_first_child();
        }

        var nodes = std.ArrayList(Node).init(allocator);
        while (!done) {
            while (cursor.field_id() != field_id) {
                if (!cursor.goto_next_sibling()) {
                    return nodes;
                }
            }
            const result = cursor.node();
            if (!cursor.goto_next_sibling()) {
                done = true;
            }
            try nodes.append(result);
        }

        return nodes;
    }

    pub fn parent(self: Node) ?Node {
        return Node.init(c.ts_node_parent(self.inner));
    }

    pub fn next_sibling(self: Node) ?Node {
        return Node.init(c.ts_node_next_sibling(self.inner));
    }

    pub fn prev_sibling(self: Node) ?Node {
        return Node.init(c.ts_node_prev_sibling(self.inner));
    }

    pub fn next_named_sibling(self: Node) ?Node {
        return Node.init(c.ts_node_next_named_sibling(self.inner));
    }

    pub fn prev_named_sibling(self: Node) ?Node {
        return Node.init(c.ts_node_prev_named_sibling(self.inner));
    }

    pub fn descendant_count(self: Node) usize {
        return @intCast(c.ts_node_descendant_count(self.inner));
    }

    pub fn descendant_for_byte_range(self: Node, start: usize, end: usize) ?Node {
        return Node.init(c.ts_node_descendant_for_byte_range(self.inner, @intCast(start), @intCast(end)));
    }

    pub fn named_descendant_for_byte_range(self: Node, start: usize, end: usize) ?Node {
        return Node.init(c.ts_node_named_descendant_for_byte_range(self.inner, @intCast(start), @intCast(end)));
    }

    pub fn descendant_for_point_range(self: Node, start: Point, end: Point) ?Node {
        return Node.init(c.ts_node_descendant_for_point_range(self.inner, start.into(), end.into()));
    }

    pub fn named_descendant_for_point_range(self: Node, start: Point, end: Point) ?Node {
        return Node.init(c.ts_node_named_descendant_for_point_range(self.inner, start.into(), end.into()));
    }

    pub fn to_sexp(self: Node) []const u8 {
        return std.mem.span(c.ts_node_string(self.inner));
    }

    pub fn utf8_text(self: Node, input: []const u8) ?[]const u8 {
        if (self.start_byte() >= input.len or self.end_byte() > input.len) {
            return null;
        }

        return input[self.start_byte()..self.end_byte()];
    }

    pub fn utf16_text(self: Node, input: []const u16) ?[]const u16 {
        if (self.start_byte() >= input.len or self.end_byte() > input.len) {
            return null;
        }

        return input[self.start_byte()..self.end_byte()];
    }

    pub fn walk(self: Node) TreeCursor {
        return TreeCursor.init(c.ts_tree_cursor_new(self.inner));
    }

    pub fn edit(self: *Node, input_edit: InputEdit) void {
        c.ts_node_edit(self.inner, input_edit.inner);
    }
};

pub const TreeCursor = struct {
    inner: c.TSTreeCursor,

    fn init(cursor: c.TSTreeCursor) TreeCursor {
        return TreeCursor{
            .inner = cursor,
        };
    }

    pub fn deinit(self: TreeCursor) void {
        c.ts_tree_cursor_delete(self.inner);
    }

    pub fn node(self: TreeCursor) Node {
        return Node.init(c.ts_tree_cursor_current_node(&self.inner)).?;
    }

    pub fn field_id(self: TreeCursor) TSFieldId {
        return c.ts_tree_cursor_current_field_id(&self.inner);
    }

    pub fn field_name(self: TreeCursor) ?[]const u8 {
        const c_field_name = c.ts_tree_cursor_current_field_name(&self.inner);
        if (c_field_name == null) {
            return null;
        }
        return std.mem.span(c_field_name);
    }

    pub fn depth(self: TreeCursor) u32 {
        return c.ts_tree_cursor_current_depth(&self.inner);
    }

    pub fn descendant_index(self: TreeCursor) u32 {
        return c.ts_tree_cursor_current_descendant_index(self.inner);
    }

    pub fn goto_first_child(self: *TreeCursor) bool {
        return c.ts_tree_cursor_goto_first_child(&self.inner);
    }

    pub fn goto_last_child(self: *TreeCursor) bool {
        return c.ts_tree_cursor_goto_last_child(&self.inner);
    }

    pub fn goto_parent(self: *TreeCursor) bool {
        return c.ts_tree_cursor_goto_parent(&self.inner);
    }

    pub fn goto_next_sibling(self: *TreeCursor) bool {
        return c.ts_tree_cursor_goto_next_sibling(&self.inner);
    }

    pub fn goto_descendant(self: *TreeCursor, descendant_idx: usize) void {
        return c.ts_tree_cursor_goto_descendant(&self.inner, @intCast(descendant_idx));
    }

    pub fn goto_previous_sibling(self: *TreeCursor) bool {
        return c.ts_tree_cursor_goto_previous_sibling(self.inner);
    }

    pub fn goto_first_child_for_byte(self: *TreeCursor, index: usize) ?i64 {
        const result = c.ts_tree_cursor_goto_first_child_for_byte(self.inner, @intCast(index));
        return if (result >= 0) result else null;
    }

    pub fn goto_first_child_for_point(self: *TreeCursor, point: Point) ?i64 {
        const result = c.ts_tree_cursor_goto_first_child_for_point(self.inner, point.into());
        return if (result >= 0) result else null;
    }

    pub fn reset(self: *TreeCursor, start_node: Node) void {
        c.ts_tree_cursor_reset(&self.inner, start_node.inner);
    }

    pub fn reset_to(self: *TreeCursor, other: TreeCursor) void {
        c.ts_tree_cursor_reset_to(&self.inner, other.inner);
    }
};

pub const InputEdit = struct {
    inner: c.TSInputEdit,

    pub fn init(start_byte: usize, old_end_byte: usize, new_end_byte: usize, start_position: Point, old_end_position: Point, new_end_position: Point) InputEdit {
        return InputEdit{
            .inner = c.TSInputEdit{
                .start_byte = @intCast(start_byte),
                .old_end_byte = @intCast(old_end_byte),
                .new_end_byte = @intCast(new_end_byte),
                .start_point = start_position.into(),
                .old_end_point = old_end_position.into(),
                .new_end_point = new_end_position.into(),
            },
        };
    }
};

pub const Range = struct {
    start_byte: u32,
    end_byte: u32,
    start_point: Point,
    end_point: Point,
};

pub const Point = struct {
    row: u32,
    column: u32,

    pub fn into(self: Point) c.TSPoint {
        return c.TSPoint{
            .row = self.row,
            .column = self.column,
        };
    }
};

test "Node.field_name_for_child()" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const C = try Language.init(allocator, "c", LanguageOpts{ .lib_path = "./c.so" });

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
