
const std = @import("std");
const pom = @import("pom");

// ================================================================
const Json = union(enum) {
    Null,
    Boolean: bool,
    Number: f64,
    String: std.ArrayList(u8),
    Array: std.ArrayList(Json),
    Object: std.StringArrayHashMap(Json),

    pub fn deinit(self: *Json) void {
        switch (self.*) {
            .String => |s| s.deinit(),
            .Array  => |a| { for (a.items) |*i| i.deinit(); a.deinit(); },
            .Object => |*o| {
                var iter = o.iterator();
                while (iter.next()) |entry| {
                    const k = entry.key_ptr.*;
                    var   v = entry.value_ptr.*;
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
                const num = try std.fmt.allocPrint(allocator, "{any}", .{v});
                defer allocator.free(num);
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
            .Object => |*obj| {
                try str.appendSlice("{");
                if (obj.count() > 0) {
                    var iter = obj.iterator();
                    while (iter.next()) |entry| {
                        const k = entry.key_ptr.*;
                        const v = entry.value_ptr.*;

                        const v_str = try v.encode(allocator);
                        defer v_str.deinit();

                        try str.appendSlice("\"");
                        try str.appendSlice(k);
                        try str.appendSlice("\" : ");
                        try str.appendSlice(v_str.items);
                        try str.appendSlice(", ");
                    }
                    _ = str.pop();
                    _ = str.pop();
                }
                try str.appendSlice("}");
                return str;
            }
        }
    }

    pub fn decode(input: []const u8, allocator: std.mem.Allocator) !Json {
        const r = json.parse(input, allocator);
        return if (r.rst) |ok| ok else |err| err;
    }
};

// ================================================================
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

fn expectEqual(expect: anytype, actual: anytype) !void {
    if (@TypeOf(actual) == []const u8 or @TypeOf(actual) == []u8) {
        return std.testing.expectEqualStrings(expect, actual);
    } else {
        return std.testing.expectEqual(expect, actual);
    }
}

// ================================================================
const ws:        pom.Void = pom.U8.choice("\x20\x09\x0A\x0D");
const tnull:     pom.Void = pom.U8.seq("null");
const ttrue:     pom.Void = pom.U8.seq("true");
const tfalse:    pom.Void = pom.U8.seq("false");
const tdec:      pom.Void = pom.U8.asciiDigit(10);

/// integer <- '-'? (('0' !tdec) / (!'0' tdec+))
const integer: pom.Void = pom.Choice(void)
    .with(pom.U8.one('0').suffix(tdec.pred(false)))
    .with(tdec.oneMore(null).discard().prefix(pom.U8.one('0').pred(false)))
    .build()
    .prefix(pom.U8.one('-').optional())
;

/// fraction <- '.' tdec+
const fraction: pom.Void =
    tdec.oneMore(null).discard().prefix(pom.U8.one('.'))
;

/// exponent <- ('e' / 'E') ('+' / '-')? tdec+
const exponent: pom.Void = pom.Sequence(void)
    .with(pom.U8.choice("eE"))
    .with(pom.U8.choice("+-").optional())
    .with(tdec.oneMore(null).discard())
    .build()
    .discard()
;

/// tnumber <- initeger fraction? exponent?
const tnumber: pom.Void = pom.Sequence(void)
    .with(integer)
    .with(fraction.optional())
    .with(exponent.optional())
    .build()
    .discard()
;

/// tstring = '"' (!'"' [any])* '"'
const tstring: pom.Void =
    pom.U8.any.prefix(pom.U8.one('\"').pred(false))
    .zeroMore(null).discard()
    .prefix(pom.U8.one('\"'))
    .suffix(pom.U8.one('\"'))
;

// =================================================================
fn _literal(comptime Opt: enum { Null, True, False }) fn(void, std.mem.Allocator) ?Json  {
    return struct {
        fn f(_: void, _: std.mem.Allocator) ?Json {
            return switch (comptime Opt) {
                .Null  => Json { .Null = {} },
                .True  => Json { .Boolean = true },
                .False => Json { .Boolean = false },
            };
        }
    }.f;
}

/// literal <- tnull / ttrue / tfalse
const literal: pom.Parser(Json) = pom.Choice(Json)
    .with(tnull.map(Json, _literal(.Null)))
    .with(ttrue.map(Json, _literal(.True)))
    .with(tfalse.map(Json, _literal(.False)))
    .build()
;

test "literal" {
    try testJsonParser(literal, "null");
    try testJsonParser(literal, "true");
    try testJsonParser(literal, "false");
}

/// number <- tnumber
const number: pom.Parser(Json) = tnumber
    .slice()
    .map(Json, struct { fn f(s: []const u8, _: std.mem.Allocator) ?Json {
        return if (std.fmt.parseFloat(f64, s)) |v| Json { .Number = v } else |_| null;
    }}.f)
;

test "number" {
    try testJsonParser(number, "123.23e2");
    try testJsonParser(number, "0.23e2");
}

const string: pom.Parser(Json) =
    tstring.slice()
    .map(Json, struct { fn f(s: []const u8, allocator: std.mem.Allocator) ?Json {
        var str = std.ArrayList(u8).init(allocator);
        str.appendSlice(s[1..(s.len - 1)]) catch {
            return null;
        };
        return Json { .String = str };
    }}.f)
;

test "string" {
    try testJsonParser(string, "\"Json string\"");
}

/// value <- literal / number / string / array / object
const value: pom.Parser(Json) = pom.Choice(Json)
    .with(literal)
    .with(number)
    .with(string)
    .with(array)
    .with(object)
    .build()
;

fn valueRef() pom.Parser(Json) {
    return value;
}

/// wsZeroMore <- ws*
const wsZeroMore: pom.Void = ws.zeroMore(null).discard();

// TODO 如何使用 PEG 描述 `a, b, c` 中最后一个元素后没有 sep
/// array <- '[' ws* (value (ws* ',' ws*))* ws* ']'
const array: pom.Parser(Json) =
    pom.ref(Json, valueRef)
    .zeroMore(pom.U8.one(',').prefix(wsZeroMore).suffix(wsZeroMore))
        .prefix(pom.U8.one('[').suffix(wsZeroMore))
        .suffix(pom.U8.one(']').prefix(wsZeroMore))
    .map(Json, struct { fn f(l: pom.List(Json), allocator: std.mem.Allocator) ?Json {
        var arr = std.ArrayList(Json).init(allocator);
        for (l.items()) |i| {
            arr.append(i) catch {
                arr.deinit();
                l.deinit();
                return null;
            };
        }
        l.rawDeinit();
        return Json { .Array = arr };
    }}.f)
;

test "array" {
    try testJsonParser(array, "[]");
    try testJsonParser(array, "[ ]");
    try testJsonParser(array,
        \\[  null, true, false, 123e-3, "string",
        \\ [ null, true, false, 123e-3, "string" ] ]
    );
}

/// kv <- string ws* ':' ws* value
const kv: pom.Parser(pom.List(Json)) = pom.Sequence(Json)
    .with(string.suffix(wsZeroMore).suffix(pom.U8.one(':')))
    .with(pom.ref(Json, valueRef).prefix(wsZeroMore))
    .build()
;

/// object <- '{' ws* (kv (ws* ',' ws*))* ws* '}'
const object: pom.Parser(Json) =
    kv.zeroMore(pom.U8.one(',').prefix(wsZeroMore).suffix(wsZeroMore))
        .prefix(pom.U8.one('{').suffix(wsZeroMore))
        .suffix(pom.U8.one('}').prefix(wsZeroMore))
    .map(Json, struct { fn f(l: pom.List(pom.List(Json)), allocator: std.mem.Allocator) ?Json {
        var obj = std.StringArrayHashMap(Json).init(allocator);
        const kvs = l.items();
        for (kvs) |*_kv| {
            const key: []u8 = allocator.dupe(u8, _kv.items()[0].String.items) catch {
                l.deinit();
                obj.deinit();
                return null;
            };
            obj.putNoClobber(key, _kv.items()[1]) catch {
                allocator.free(key);
                l.deinit();
                obj.deinit();
                return null;
            };
            _kv.items()[0].String.deinit();
            _kv.rawDeinit();
        }
        l.rawDeinit();
        return Json { .Object = obj };
    }}.f)
;

test "object" {
    try testJsonParser(object, "{}");
    try testJsonParser(object, "{   }");
    try testJsonParser(object, 
        \\{
        \\    "1": true, "2": false, "3": [true, false, null], "4": { "1": "Nested" }
        \\}
    );
}

/// json <- ws* object ws*
const json: pom.Parser(Json) =
    object.prefix(wsZeroMore).suffix(wsZeroMore)
;

test "json" {
    try testJsonParser(json, "    {}  ");
    try testJsonParser(json, "   { }   ");
    try testJsonParser(json, json_input);
}

const json_input: []const u8 =
\\         
\\{
\\    "array": [
\\        true, false, null, "string", 123e-9, [true, false, null, "string"], {
\\                "array": [true, false, null, "string", 123e-9, [true, false, null, 123e-9, "string"]]
\\            }
\\    ],
\\    "object": {
\\        "array": [
\\            true, false, null, "string", 123e-9, [true, false, null, 123e-9, "string"], {
\\                    "array": [true, false, null, "string", [true, false, null, 123e-9, "string"]]
\\                }
\\        ],
\\        "object": { "1": null, "2": 1234 }
\\    }
\\}         
;


// =================================================================
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (gpa.deinit() == .leak) @panic("Memory Leak");
    const allocator = gpa.allocator();

    var res = Json.decode(json_input, allocator);
    if (res) |*ok| {
        defer ok.deinit();
        const s = try ok.encode(allocator);
        defer s.deinit();
        std.debug.print("{s}\n", .{s.items});
    } else |err| {
        std.debug.print("{any}\n", .{err});
    }
}