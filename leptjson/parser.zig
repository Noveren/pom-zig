
const std = @import("std");
const Allocator = std.mem.Allocator;

const pom = @import("pom");

const leptjson = @import("root.zig");
const Value = leptjson.Value;
const Error = leptjson.Error;
const String = leptjson.String;



// ================================================================
const ws: pom.Parser(void, pom.Error) =
    pom.U8.set("\x20\x09\x0A\x0D")
;

const @"ws*": pom.Parser(void, Error) =
    ws.zeroMore(null).mapErr(Error.NOP)
;

test "whitespace" {
    const rst1 = ws.parse(" ", std.testing.allocator);
    try std.testing.expect(rst1.isOk());
    
    const rst2 = @"ws*".parse("\n \t \r          ", std.testing.allocator);
    try std.testing.expect(rst2.isOk());

    const rst3 = @"ws*".parse("", std.testing.allocator);
    try std.testing.expect(rst3.isOk());
}

// ================================================================
pub fn parse(input: []const u8, allocator: Allocator) pom.Result(Value, Error) {
    const json: pom.Parser(Value, Error) =
        comptime value
            .prefix(@"ws*")
            .suffix(@"ws*")
            .suffix(pom.U8.any.pred(false).mapErr(Error.RootNotSingular))
    ;
    return json.parse(input, allocator);
}

const value: pom.Parser(Value, Error) = pom.ChoicePrefix(Value, Error)
    .with(literal, pom.U8.set("ntf"))
    .build()
    .mapErr(Error.ExpectValue)
;

// ================================================================
// literal
const literal: pom.Parser(Value, Error) = pom.Choice(Value)
    .with(pom.U8.seq("null").map(Value { .Null = {} }))
    .with(pom.U8.seq("true").map(Value { .Boolean = true }))
    .with(pom.U8.seq("false").map(Value { .Boolean = false }))
    .build()
    .mapErr(Error.InvalidValue)
;

test "literal" {
    const testLiteral = struct {
        fn f(input: []const u8, comptime mode: enum { Null, True, False }) !void {
            const rst = parse(input, std.testing.allocator);
            try std.testing.expect(rst.isOk());
            const v = try rst.value;
            try std.testing.expect(switch (mode) {
                .Null  => v.getType() == .Null,
                .True  => v.getType() == .Boolean and v.Boolean == true,
                .False => v.getType() == .Boolean and v.Boolean == false,
            });
        }
    }.f;

    try testLiteral("null", .Null);
    try testLiteral("true", .True);
    try testLiteral("false", .False);
}