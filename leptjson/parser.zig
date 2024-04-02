
const std = @import("std");
const Allocator = std.mem.Allocator;

const pom = @import("pom");

const leptjson = @import("root.zig");
const Value = leptjson.Value;
const Error = leptjson.Error;

// pub fn parse(input: []const u8, allocator: Allocator) !Value {
//     _ = input;
//     _ = allocator;
// }

// ================================================================
// const ws: pom.Void = pom.U8.choice("\x20\x09\x0A\x0D");
// const wsZeroMore: pom.Void = ws.zeroMore(null).discard();

// ================================================================
const value: pom.Parser(Value) = pom.Choice(Value)
    .with(literal)
    .build()
;

// ================================================================
fn genLiteral(comptime Opt: enum { Null, True, False }) fn(void, std.mem.Allocator) ?Value  {
    return struct {
        fn f(_: void, _: std.mem.Allocator) ?Value {
            return switch (comptime Opt) {
                .Null  => Value { .Null = {} },
                .True  => Value { .Boolean = true },
                .False => Value { .Boolean = false },
            };
        }
    }.f;
}

/// literal <- tnull / ttrue / tfalse
const literal: pom.Parser(Value) = pom.Choice(Value)
    .with(pom.U8.seq("null").map(Value, genLiteral(.Null)))
    .with(pom.U8.seq("true").map(Value, genLiteral(.True)))
    .with(pom.U8.seq("false").map(Value, genLiteral(.False)))
    .build()
;

// ================================================================
test "null" {
    try std.testing.expect(true);
}