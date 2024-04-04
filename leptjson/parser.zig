
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
    .withPrefix(number, pom.U8.any)
    .build(Error.ExpectValue)
;

// ================================================================
// literal
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

/// `literal <- "null" / "true" / "false"`
const literal: pom.Parser(Value, Error) = pom.Choice(Value, pom.Error)
    .with(pom.U8.seq("null").map(Value, genLiteral(.Null)))
    .with(pom.U8.seq("true").map(Value, genLiteral(.True)))
    .with(pom.U8.seq("false").map(Value, genLiteral(.False)))
    .build()
    .castErr(Error.InvalidValue) // pom.Error.FailedToChoice -> Error.InvalidValue
;

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
}

// ================================================================
// number
const dec: pom.Void(pom.Error) = pom.U8.asciiDigit(10);
const decOneMore: pom.Void(pom.Error) = dec.oneMoreVoid(null);

/// `integer <- '-'? (('0' !dec) / (!'0' dec+))`
const integer = pom.Choice(void, pom.Error)
    .with(pom.U8.one('0').suffix(dec.pred(false)))
    .with(decOneMore.prefix(pom.U8.one('0').pred(false)))
    .build()
    .prefix(pom.U8.one('-').optional())
;

/// `fraction <- '.' dec+`
const fraction: pom.Void(pom.Error) =
    decOneMore.prefix(pom.U8.one('.'))
;

// TODO Sequence
/// `exponent <- ('e' / 'E') ('+' / '-')? dec+`
const exponent: pom.Void(pom.Error) =
    decOneMore.prefix(pom.U8.set("+-").optional()).prefix(pom.U8.set("eE"))
;

/// `tnumber <- initeger fraction? exponent?`
const tnumber: pom.Void(pom.Error) =
    integer
    .suffix(pom.Choice(void, pom.Error)
        .withPrefix(fraction, pom.U8.one('.'))
        // TODO optional
        .withPrefix(pom.nop, pom.nop)
        .build(pom.Error.FailedToChoice)
    )
    .suffix(pom.Choice(void, pom.Error)
        .withPrefix(exponent, pom.U8.set("eE"))
        // TODO optional
        .withPrefix(pom.nop, pom.nop)
        .build(pom.Error.FailedToChoice)
    )
;

const number: pom.Parser(Value, Error) =
    tnumber
    .slice()
    .castErr(Error.InvalidValue)
    .map(Value, struct { fn f(s: *[]const u8, _: std.mem.Allocator) ?Value {
        return if (std.fmt.parseFloat(f64, s.*)) |v| blk: {
            break: blk if (std.math.isInf(v)) null else Value { .Number = v };
        } else |_| null;
    }}.f)
    .mapErr(Error, struct { fn f(e: (pom.Error||Error)) Error {
        return switch (e) {
            pom.Error.FailedToMap => Error.NumberTooBig,
            else => @errorCast(e)
        };
    }}.f)
;

test "number" {
    const testNumber = struct {
        fn f(input: []const u8) !void {
            const rst = number.parse(input, std.testing.allocator);
            try std.testing.expect(rst.isOk());
            // const v = try rst.value;
            // std.debug.print("{any}\n", .{v.Number});
        }
    }.f;

    try testNumber("0");
    try testNumber("-0");
    try testNumber("-0.0");
    try testNumber("1");
    try testNumber("-1");
    try testNumber("1.5");
    try testNumber("-1.5");
    try testNumber("3.1416");
    try testNumber("1E10");
    try testNumber("1e10");
    try testNumber("1E+10");
    try testNumber("1E-10");
    try testNumber("-1E10");
    try testNumber("-1e10");
    try testNumber("-1E+10");
    try testNumber("-1E-10");
    try testNumber("1.234E+10");
    try testNumber("1.234E-10");
    try testNumber("1e-10000"); // must underflow 0e0

    try testNumber("1.0000000000000002"); // the smallest number > 1

    // 与教程不同 expect 4.9406564584124654e-324 actual 5e-324
    try testNumber("4.9406564584124654e-324"); // minimum denormal
    try testNumber("-4.9406564584124654e-324");

    try testNumber("2.2250738585072009e-308");  // Max subnormal double
    try testNumber("-2.2250738585072009e-308");
    try testNumber("2.2250738585072014e-308");  // Min normal positive double
    try testNumber("-2.2250738585072014e-308");
    try testNumber("1.7976931348623157e+308");  // Max double
    try testNumber("-1.7976931348623157e+308");

}



// ================================================================
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
    try testError(Error.InvalidValue, "?");

    // ============================================================
    // number
    try testError(Error.InvalidValue, "+0");
    try testError(Error.InvalidValue, "+1");
    try testError(Error.InvalidValue, ".123");
    try testError(Error.InvalidValue, "1.");
    try testError(Error.InvalidValue, "1e");
    try testError(Error.InvalidValue, "INF");
    try testError(Error.InvalidValue, "inf");
    try testError(Error.InvalidValue, "NAN");
    try testError(Error.InvalidValue, "nan");

    try testError(Error.InvalidValue, "0123");
}

test "root not singular" {
    try testError(Error.RootNotSingular, "null ?");

    try testError(Error.RootNotSingular, "0x0");
    try testError(Error.RootNotSingular, "0x123");
}

test "number too big" {
    try testError(Error.NumberTooBig, "1e309");
    try testError(Error.NumberTooBig, "-1e309");
}