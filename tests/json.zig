
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
    Number: f64,
    String: std.ArrayList(u8),
    Array: std.ArrayList(Json),

    pub fn deinit(self: Json) void {
        switch (self) {
            .String => |s| {
                // std.debug.print("string deinit\n", .{});
                s.deinit();
            },
            .Array => |a| {
                // std.debug.print("array deinit\n", .{});
                for (a.items) |i| i.deinit();
                a.deinit();
            },
            else => {},
        }
    }

    pub fn clone(self: Json) ?Json {
        switch (self) {
            .String => |s| {
                const cloned = s.clone() catch return null;
                return Json { .String = cloned };
            },
            .Array => |a| {
                const cloned = a.clone() catch return null;
                return Json { .Array = cloned };
            },
            else => return self,
        }
    }

    /// TODO 转移所有权，避免拷贝
    pub fn move(self: Json) Json {
        switch (self) {
            .String => |s| {
                // s.capacity = 0;
                return Json { .String = std.ArrayList(u8).fromOwnedSlice(s.allocator, s.items) };
            },
            .Array => |_| {
                return self;
            },
            else => return self,
        }
    }
};

const whitespace: pom.PVoid = pom.terminal.cU8Choice("\x20\x09\x0A\x0D");

/// literal <- "null" | "true" | "false"
const literal: pom.Parser(Json) = pom.Choice(Json)
    .with(pom.terminal.tU8s("null").map(Json, struct { fn f(_: void, _: std.mem.Allocator) ?Json {
        return Json { .Null = {} };
    }}.f))
    .with(pom.terminal.tU8s("true").map(Json, struct { fn f(_: void, _: std.mem.Allocator) ?Json {
        return Json { .Boolean = true };
    }}.f))
    .with(pom.terminal.tU8s("false").map(Json, struct { fn f(_: void, _: std.mem.Allocator) ?Json {
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

const dec: pom.PVoid = pom.terminal.digit(.Dec);
const dec_0: pom.PVoid = pom.terminal.tU8('0');

/// int <- '-'? (('0' !dec) / (!'0' dec+))
const int: pom.PVoid = pom.Choice(void)
    .with(dec_0.suffix(dec.pred(false)))
    .with(dec.oneMore().mapVoid().prefix(dec_0.pred(false)))
    .build()
    .prefix(pom.terminal.tU8('-').optional())
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

const dec_one_more: pom.PVoid = dec.oneMore().mapVoid();

/// frac <- '.' dec+
const frac: pom.PVoid =
    dec_one_more.prefix(pom.terminal.tU8('.'))
;

/// exp <- ('e' / 'E') ('+' / '-')? dec+
const exp: pom.PVoid = pom.Sequence(void)
    .with(pom.Choice(void)
        .with(pom.terminal.tU8('e'))
        .with(pom.terminal.tU8('E'))
        .build())
    .with(pom.Choice(void)
        .with(pom.terminal.tU8('+'))
        .with(pom.terminal.tU8('-'))
        .build()
        .optional())
    .with(dec_one_more)
    .build()
    .mapVoid()
;

/// number <- int frac? exp?
const number: pom.Parser(Json) = pom.Sequence(void)
    .with(int)
    .with(frac.optional())
    .with(exp.optional())
    .build()
    .slice()
    .map(Json, struct { fn f(s: []const u8, _: std.mem.Allocator) ?Json {
        return if (std.fmt.parseFloat(f64, s)) |v| Json { .Number = v } else |_| null;
    }}.f)
;

test "number" {
    inline for (comptime .{
        .{"123.23e2", 12323.0},
    }) |i| {
        var r = number.parse(i[0], std.testing.allocator);
        try expectEqual(true, r.isOk());
        try expectEqual(i[0].len, r.mov);
        const v = try r.rst;
        try expectEqual(i[1], v.Number);
    }

    inline for (comptime .{
        "0123.23e2",
    }) |i| {
        var r = number.parse(i, std.testing.allocator);
        try expectEqual(false, r.isOk());
        try expectEqual(0, r.mov);
    }
}

const single_quotation: pom.PVoid = pom.terminal.tU8('\"');

// TODO 支持转义
const char: pom.PVoid =
    pom.terminal.anychar
    .prefix(single_quotation.pred(false))
;

/// string <- '"' char+? '"'
const string: pom.Parser(Json) = char.oneMore().optional().slice()
    .prefix(single_quotation)
    .suffix(single_quotation)
    .map(Json, struct { fn f(s: []const u8, allocator: std.mem.Allocator) ?Json {
        var str = std.ArrayList(u8).init(allocator);
        str.appendSlice(s) catch {
            return null;
        };
        return Json { .String = str };
    }}.f)
;

test "string" {
    var r1 = string.parse("\"string\"", std.testing.allocator);
    defer r1.drop();
    if (r1.rst) |ok| {
        try expectEqual("string", ok.String.items);
    } else |err| {
        std.debug.print("{any} {d}\n", .{err, r1.mov});
        try expectEqual(false, true);
    }
}

const array: pom.Parser(Json) = value
    .prefix(pom.terminal.tU8('['))
    .suffix(pom.terminal.tU8(']'))
    .map(Json, struct { fn f(j: Json, allocator: std.mem.Allocator) ?Json {
        var arr = std.ArrayList(Json).init(allocator);
        // TODO 实现 move
        const cloned = j.clone() orelse return null;
        arr.append(cloned) catch {
            arr.deinit();
            return null;
        };
        return Json { .Array = arr };
    }}.f)
;

const value: pom.Parser(Json) = pom.Choice(Json)
    .with(literal)
    .with(number)
    .with(string)
    .build()
;

test "array" {
    var r1 = array.parse("[\"string\"]", std.testing.allocator);
    defer r1.drop();
    try expectEqual(true, r1.isOk());
}