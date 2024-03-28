
const std = @import("std");
const pom = @import("pom");

fn expectEqual(expect: anytype, actual: anytype) !void {
    if (@TypeOf(actual) == []const u8 or @TypeOf(actual) == []u8) {
        return std.testing.expectEqualStrings(expect, actual);
    } else {
        return std.testing.expectEqual(expect, actual);
    }
}

const Json = union(enum) {
    Null,
    Boolean: bool,
    String: std.ArrayList(u8),

    pub fn deinit(self: Json) void {
        switch (self) {
            .String => |s| s.deinit(),
            else => {},
        }
    }
};

/// literal <- "null" | "true" | "false"
const literal: pom.Parser(Json) = pom.Choice(Json)
    .with(pom.terminal.literal("null").map(Json, struct { fn f(_: void, _: std.mem.Allocator) ?Json {
        return Json { .Null = {} };
    }}.f))
    .with(pom.terminal.literal("true").map(Json, struct { fn f(_: void, _: std.mem.Allocator) ?Json {
        return Json { .Boolean = true };
    }}.f))
    .with(pom.terminal.literal("false").map(Json, struct { fn f(_: void, _: std.mem.Allocator) ?Json {
        return Json { .Boolean = false };
    }}.f))
    .build()
;

test "literal" {
    const r1 = literal.parse("null", std.testing.allocator);
    try expectEqual(Json { .Null = {} }, try r1.rst);

    const r2 = literal.parse("true", std.testing.allocator);
    try expectEqual(Json { .Boolean = true }, try r2.rst);

    const r3 = literal.parse("false", std.testing.allocator);
    try expectEqual(Json { .Boolean = false }, try r3.rst);
}

const dec: pom.Parser(void) = pom.terminal.digit(.Dec);
const dec_0: pom.Parser(void) = pom.terminal.literal("0");

/// int <- '-'? (('0' !dec) / (!'0' dec+))
const int: pom.Parser(void) = pom.Choice(void)
    .with(dec_0.suffix(dec.pred(false)))
    .with(dec.oneMore().mapVoid().prefix(dec_0.pred(false)))
    .build()
    .prefix(pom.terminal.literal("-").optional())
;

test "int" {
    inline for (comptime .{
        "0", "123", "-0", "-123"
    }) |input| {
        var r = int.parse(input, std.testing.allocator);
        try expectEqual(true, r.isOk());
        try expectEqual(input.len, r.mov);
    }

    inline for (comptime .{
        "0123", "+123"
    }) |input| {
        var r1 = int.parse(input, std.testing.allocator);
        try expectEqual(false, r1.isOk());
    }
}

const dec_one_more: pom.Parser(void) = dec.oneMore().mapVoid();

/// frac <- '.' dec+
const frac: pom.Parser(void) =
    dec_one_more.prefix(pom.terminal.literal("."))
;

/// exp <- ('e' / 'E') ('+' / '-')? dec+
const exp: pom.Parser(void) = pom.Sequence(void)
    .with(pom.Choice(void)
        .with(pom.terminal.literal("e"))
        .with(pom.terminal.literal("E"))
        .build())
    .with(pom.Choice(void)
        .with(pom.terminal.literal("+"))
        .with(pom.terminal.literal("-"))
        .build()
        .optional())
    .with(dec_one_more)
    .build()
    .mapVoid()
;

/// number <- int frac? exp?
const number: pom.Parser(f64) = pom.Sequence(void)
    .with(int)
    .with(frac.optional())
    .with(exp.optional())
    .build()
    .slice()
    .map(f64, struct { fn f(s: []const u8, _: std.mem.Allocator) ?f64 {
        return if (std.fmt.parseFloat(f64, s)) |v| v else |_| null;
    }}.f)
;

test "number" {
    inline for (comptime .{
        .{"123.23e2", 12323.0},
    }) |i| {
        var r = number.parse(i[0], std.testing.allocator);
        try expectEqual(true, r.isOk());
        try expectEqual(i[0].len, r.mov);
        try expectEqual(i[1], try r.rst);
    }

    inline for (comptime .{
        "0123.23e2",
    }) |i| {
        var r = number.parse(i, std.testing.allocator);
        try expectEqual(false, r.isOk());
        try expectEqual(0, r.mov);
    }
}

// TODO 支持转义
const char: pom.Parser(void) =
    pom.terminal.anychar
    .prefix(pom.terminal.literal("\"").pred(false))
;

/// string <- '"' char+? '"'
const string: pom.Parser(std.ArrayList(u8)) = char.oneMore().optional().slice()
    .prefix(pom.terminal.literal("\""))
    .suffix(pom.terminal.literal("\""))
    .map(std.ArrayList(u8), struct { fn f(s: []const u8, allocator: std.mem.Allocator) ?std.ArrayList(u8) {
        var str = std.ArrayList(u8).init(allocator);
        str.appendSlice(s) catch {
            return null;
        };
        return str;
    }}.f)
;

test "string" {
    const r1 = string.parse("\"string\"", std.testing.allocator);
    if (r1.rst) |ok| {
        defer ok.deinit();
        try expectEqual("string", ok.items);
    } else |err| {
        std.debug.print("{any} {d}\n", .{err, r1.mov});
        try expectEqual(false, true);
    }
}