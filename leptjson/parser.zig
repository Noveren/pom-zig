
const std = @import("std");
const Allocator = std.mem.Allocator;

const pom = @import("pom");

const leptjson = @import("root.zig");
const Value = leptjson.Value;
const Error = leptjson.Error;

pub fn parse(input: []const u8, allocator: Allocator) pom.Result(Value, Error||pom.Error) {
    const json: pom.Parser(Value, Error||pom.Error) =
        comptime value
            .prefix(wsZeroMore)
            .suffix(wsZeroMore)
            .suffix(pom.U8.any.pred(false).castErr(Error.RootNotSingular))
    ;
    return json.parse(input, allocator);
}

// ================================================================
const ws: pom.VoidE = pom.U8.set("\x20\x09\x0A\x0D");
const wsZeroMore: pom.Void(pom.Error) = ws.zeroMoreVoid(null);

test "whitespace" {
    const rst1 = wsZeroMore.parse("     ", std.testing.allocator);
    try std.testing.expect(rst1.isOk());
}

// ================================================================

const value: pom.Parser(Value, Error) = pom.Choice(Value, Error)
    .withPrefix(literal, pom.U8.set("ntf"))
    .build(Error.ExpectValue)
;

// ================================================================
fn genLiteral(comptime Opt: enum { Null, True, False }) fn(*void, std.mem.Allocator) ?Value  {
    return struct {
        fn f(_: *void, _: std.mem.Allocator) ?Value {
            return switch (comptime Opt) {
                .Null  => Value { .Null = {} },
                .True  => Value { .Boolean = true },
                .False => Value { .Boolean = false },
            };
        }
    }.f;
}

/// literal <- tnull / ttrue / tfalse
const literal: pom.Parser(Value, Error) = pom.Choice(Value, pom.Error)
    .with(pom.U8.seq("null").map(Value, genLiteral(.Null)))
    .with(pom.U8.seq("true").map(Value, genLiteral(.True)))
    .with(pom.U8.seq("false").map(Value, genLiteral(.False)))
    .build()
    .castErr(Error.InvalidValue) // pom.Error.FailedToChoice -> Error.InvalidValue
;

// ================================================================
test "literal" {
    const testLiteral = struct {
        fn f(input: []const u8, comptime mode: enum { Null, True, False }) !void {
            const rst = parse(input, std.testing.allocator);
            try std.testing.expect(rst.isOk());
            const v: Value = try rst.value;
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

    const rst2 = literal.parse("nul", std.testing.allocator);
    try std.testing.expectError(Error.InvalidValue, rst2.value);
    try std.testing.expect(rst2.offset == 0);

    const rst3 = value.parse("nul", std.testing.allocator);
    try std.testing.expectError(Error.InvalidValue, rst3.value);
}

fn testError(comptime err: Error, input: []const u8) !void {
    const rst = parse(input, std.testing.allocator);
    try std.testing.expectError(err, rst.value);
}

test "expect value" {
    try testError(Error.ExpectValue, "");
    try testError(Error.ExpectValue, "  \n  \t  \r    ");
}

test "invalid value" {
    try testError(Error.InvalidValue, "nul");
    // try testError(Error.InvalidValue, "?");
}

test "rootNotSingular" {
    try testError(Error.RootNotSingular, "null ?");
}