
const std = @import("std");
const pom = @import("pom");

fn expectEqual(expect: anytype, actual: anytype) !void {
    if (@TypeOf(actual) == []const u8 or @TypeOf(actual) == []u8) {
        return std.testing.expectEqualStrings(expect, actual);
    } else {
        return std.testing.expectEqual(expect, actual);
    }
}

// ================================================================
const whitespaceOpt: pom.Void = pom.U8.choice("\x20\x09\x0A\x0D")
    .oneMore()
    .optional()
;

test "whitespaceOpt" {
    inline for (.{
        " ", "\n", "\r", "\t", "\x20\x09\x0A\x0D"
    }) |i| {
        const r = whitespaceOpt.parse(i, std.testing.allocator);
        try std.testing.expect(r.isOk());
    }
}

const dec: pom.Void = pom.U8.asciiDigit(10);

/// integer <- '-'? (('0' !dec) / (!'0' dec+))
const integer: pom.Void = pom.Choice(void)
    .with(pom.U8.one('0').suffix(dec.pred(false)))
    .with(dec.oneMore().discard().prefix(pom.U8.one('0').pred(false)))
    .build()
    .prefix(pom.U8.one('-').optional())
;

/// fraction <- '.' dec+
const fraction: pom.Void =
    dec.oneMore().discard().prefix(pom.U8.one('.'))
;

/// exponent <- ('e' / 'E') ('+' / '-')? dec+
const exponent: pom.Void = pom.Sequence(void)
    .with(pom.Choice(void)
        .with(pom.U8.one('e'))
        .with(pom.U8.one('E'))
        .build())
    .with(pom.Choice(void)
        .with(pom.U8.one('+'))
        .with(pom.U8.one('-'))
        .build()
        .optional())
    .with(dec.oneMore().discard())
    .build()
    .discard()
;

const colon:     pom.Void = pom.U8.one(':');
const comma:     pom.Void = pom.U8.one(',');
const lbracket:  pom.Void = pom.U8.one('[');
const rbracket:  pom.Void = pom.U8.one(']');
const lcurly:    pom.Void = pom.U8.one('{');
const rcurly:    pom.Void = pom.U8.one('}');
const tnull:     pom.Void = pom.U8.seq("null");
const ttrue:     pom.Void = pom.U8.seq("true");
const tfalse:    pom.Void = pom.U8.seq("false");
/// TODO 支持转义、utf-8
const tstring:   pom.Void = pom.U8.any.prefix(pom.U8.one('\"').pred(false))
    .oneMore().discard()
    .prefix(pom.U8.one('\"'))
    .suffix(pom.U8.one('\"'))
;
const tnumber:   pom.Void = pom.Sequence(void)
    .with(integer)
    .with(fraction.optional())
    .with(exponent.optional())
    .build()
    .discard()
;

// ================================================================
const Json = union(enum) {
    Null,
    Boolean: bool,
    Number: f64,
    String: std.ArrayList(u8),
    Array: std.ArrayList(Json),
    Object: std.StringHashMap(Json),

    pub fn deinit(self: *Json) void {
        switch (self.*) {
            .String => |s| s.deinit(),
            .Array  => |a| { for (a.items) |*i| i.deinit(); a.deinit(); },
            .Object => |*o| {
                var kIter = o.keyIterator();
                while (kIter.next()) |key| {
                    const k: []const u8 = key.*;
                    var v: Json = o.get(k).?;
                    v.deinit();
                    o.allocator.free(k);
                }
                o.deinit();
            },
            else => {},
        }
    }

    pub fn encode(self: Json, allocator: std.mem.Allocator) !std.ArrayList(u8) {
        var str = std.ArrayList(u8).init(allocator);
        switch (self) {
            .Null => |_| {
                try str.appendSlice("null");
                return str;
            },
            .Boolean => |v| {
                try str.appendSlice(if (v) "true" else "false");
                return str;
            },
            .Number => |v| {
                const num = try std.fmt.allocPrint(std.testing.allocator, "{any}", .{v});
                defer std.testing.allocator.free(num);
                try str.appendSlice(num);
                return str;
            },
            .String => |v| {
                try str.appendSlice("\"");
                try str.appendSlice(v.items);
                try str.appendSlice("\"");
                return str;
            },
            .Array => |arr| {
                try str.appendSlice("[");
                for (arr.items, 0..) |i, idx| {
                    const r = try i.encode(allocator);
                    defer r.deinit();
                    try str.appendSlice(r.items);
                    if (idx != arr.items.len-1) {
                        try str.appendSlice(", ");
                    }
                }
                try str.appendSlice("]");
                return str;
            },
            .Object => |obj| {
                try str.appendSlice("{");
                var kIter = obj.keyIterator();
                while (kIter.next()) |key| {
                    const k = key.*;
                    const r = try obj.get(k).?.encode(allocator);
                    defer r.deinit();
                    try str.appendSlice("\"");
                    try str.appendSlice(k);
                    try str.appendSlice("\" : ");
                    try str.appendSlice(r.items);
                    // TODO
                    try str.appendSlice(", ");
                }
                try str.appendSlice("}");
                return str;
            }
        }
    }
};

fn testJsonParser(p: pom.Parser(Json), input: []const u8) !void {
    var r = p.parse(input, std.testing.allocator);
    defer r.discard();
    if (r.rst) |ok| {
        const s = try ok.encode(std.testing.allocator);
        defer s.deinit();
        std.debug.print("{s}\n", .{s.items});
        try expectEqual(input.len, r.mov);
    } else |err| {
        std.debug.print("{any} {s}\n", .{err, input[0..r.mov]});
        try std.testing.expect(false);
    }
}

/// literal <- (tnull | ttrue | tfalse) whitespaceOpt
const literal: pom.Parser(Json) = pom.Choice(Json)
    .with(tnull.map(Json, struct { fn f(_: void, _: std.mem.Allocator) ?Json {
        return Json { .Null = {} };
    }}.f))
    .with(ttrue.map(Json, struct { fn f(_: void, _: std.mem.Allocator) ?Json {
        return Json { .Boolean = true };
    }}.f))
    .with(tfalse.map(Json, struct { fn f(_: void, _: std.mem.Allocator) ?Json {
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

/// number <- integer fraction? exponent?
const number: pom.Parser(Json) = tnumber
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

const string: pom.Parser(Json) = tstring
    .slice()
    .map(Json, struct { fn f(s: []const u8, allocator: std.mem.Allocator) ?Json {
        var str = std.ArrayList(u8).init(allocator);
        str.appendSlice(s[1..(s.len - 1)]) catch {
            return null;
        };
        return Json { .String = str };
    }}.f)
;

test "string" {
    var r1 = string.parse("\"string\"", std.testing.allocator);
    defer r1.discard();
    if (r1.rst) |ok| {
        try expectEqual("string", ok.String.items);
    } else |err| {
        std.debug.print("{any} {d}\n", .{err, r1.mov});
        try expectEqual(false, true);
    }
}

const value: pom.Parser(Json) = pom.Choice(Json)
    .with(literal)
    .with(number)
    .with(string)
    .with(array)
    .build()
;

fn valueRef() pom.Parser(Json) {
    return value;
}

/// arrayValue <- whitespace? value whitespace?
const arrayValue: pom.Parser(Json) = pom.ref(Json, valueRef)
    .prefix(whitespaceOpt)
    .suffix(whitespaceOpt)
;

/// arrayOne <- arrayValue
const arrayOne: pom.Parser(Json) = arrayValue
    .map(Json, struct { fn f(j: Json, allocator: std.mem.Allocator) ?Json {
        var arr = std.ArrayList(Json).init(allocator);
        arr.append(j) catch {
            arr.deinit();
            return null;
        };
        return Json { .Array = arr };
    }}.f)
;

/// arrayMany <- (arrayValue ',')+ arrayValue
const arrayMany: pom.Parser(Json) = pom.Sequence(Json)
    .with(arrayValue
        .suffix(pom.U8.one(','))
        .oneMore()
        .map(Json, struct { fn f(l: pom.List(Json), allocator: std.mem.Allocator) ?Json {
            var arr = std.ArrayList(Json).init(allocator);
            for (l.getItems()) |i| {
                arr.append(i) catch {
                    arr.deinit();
                    l.deinit();
                    return null;
                };
            }
            l._list.deinit();
            return Json { .Array = arr };
        }}.f))
    .with(arrayValue)
    .build()
    .map(Json, struct { fn f(l: pom.List(Json), _: std.mem.Allocator) ?Json {
        const items = l.getItems();
        std.debug.assert(l.getItems().len == 2);
        var arr = items[0];
        const sfx = items[1];
        arr.Array.append(sfx) catch {
            l.deinit();
            return null;
        };
        l._list.deinit();
        return arr;
    }}.f)
;

/// array <- '[' (arrayMany / arrayOne) ']'
const array: pom.Parser(Json) = pom.Choice(Json)
    .with(arrayMany)
    .with(arrayOne)
    .build()
    .prefix(pom.U8.one('['))
    .suffix(pom.U8.one(']'))
;

test "array" {
    try testJsonParser(array, "[ [\n\"string string\"\t]   ]");
    try testJsonParser(array, "[  null, true, false, 123e-3, \"string\"  ]");
    try testJsonParser(array,
        \\[  null, true, false, 123e-3, "string",
        \\ [ null, true, false, 123e-3, "string" ] ]
    );
}

/// kv <- string whitespace? ':' whitespace? value
const kv: pom.Parser(pom.List(Json)) = pom.Sequence(Json)
    .with(string.suffix(whitespaceOpt).suffix(pom.U8.one(':')))
    .with(pom.ref(Json, valueRef).prefix(whitespaceOpt))
    .build()
;
test "key value" {
    var r = kv.parse("\"key\" : [9376, null, true]", std.testing.allocator);
    defer r.discard();
    try std.testing.expect(r.isOk());
    const v = try r.rst;
    try expectEqual(2, v.getItems().len);
    const s1 = try v.getItems()[0].encode(std.testing.allocator);
    defer s1.deinit();
    const s2 = try v.getItems()[1].encode(std.testing.allocator);
    defer s2.deinit();
    std.debug.print("{s}: {s}\n", .{s1.items, s2.items});
}

// objectValue <- whitespace? kv whitespace?
// objectOne <- objectValue
// objectMany <- (objectValue ',')+ objectValue
// object <- '{' (objectMany / objectOne) '}'