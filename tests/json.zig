
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

    pub fn deinit(self: Json) void {
        switch (self) {
            .String => |s| s.deinit(),
            .Array  => |a| { for (a.items) |i| i.deinit(); a.deinit(); },
            else    => {},
        }
    }

    // pub fn format(self: Json, allocator: std.mem.Allocator) std.ArrayList(u8) {
    //     _ = self;
    //     _ = allocator;
    // } 
};

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

/// arrayMany <- (arrayOne ',')+ arrayOne
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



/// array <- '[' arrayOne ']'
const array: pom.Parser(Json) = pom.Choice(Json)
    .with(arrayMany)
    .with(arrayOne)
    .build()
    .prefix(pom.U8.one('['))
    .suffix(pom.U8.one(']'))
;

test "array" {
    var r1 = array.parse("[ [\n\"string string\"\t]   ]", std.testing.allocator);
    defer r1.discard();
    try expectEqual(true, r1.isOk());
    // const v1 = try r1.rst;
    // std.debug.print("{s}\n", .{v1.Array.items[0].Array.items[0].String.items});

    var r2 = array.parse("[  null, true, false, 123e-3, \"string\"  ]", std.testing.allocator);
    // var r2 = array.parse(
    //     \\[ null, true, false, 123e-3, \"string\",
    //     \\ [null, true, false, 123e-3, \"string\"] 
    //     \\]
    // , std.testing.allocator);
    // var r2 = array.parse(
    //     \\[ null, true, false, 123e-3, \"string\"
    //     \\]
    // , std.testing.allocator);
    defer r2.discard();
    try expectEqual(true, r2.isOk());
    // const v2 = try r2.rst;
    // std.debug.print("{any}\n", .{v2.Array.items[0]});
}