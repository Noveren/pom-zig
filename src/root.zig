
const std = @import("std");

const parser = @import("parser.zig");
pub const Parser = parser.Parser;
pub const Result = parser.Result;

pub const ErrorNOP = error {};
pub const ErrorEOF = error { EOF };
pub const ErrorFailedToMatchLiteral = error { FailedToMatchLiteral };
pub const ErrorFailedToPred = error { FailedToPred };
pub const ErrorFailedToMap = error { FailedToMap };
pub const ErrorFailedToChoice = error { FailedToChoice };
pub const Error = 
       ErrorNOP
    || ErrorEOF
    || ErrorFailedToMatchLiteral
    || ErrorFailedToPred
    || ErrorFailedToMap
    || ErrorFailedToChoice
;

pub const U8 = struct {
    pub const any = Parser(void, ErrorEOF) { .parse = struct { const R = Result(void, ErrorEOF);
        fn f(input: []const u8, _: std.mem.Allocator) R {
            if (input.len > 0) {
                return R { .offset = 1, .value = {} };
            } else {
                return R { .value = @errorCast(Error.EOF) };
            }
        }
    }.f };

    pub fn one(comptime ch: u8) Parser(void, ErrorEOF||ErrorFailedToMatchLiteral) {
        return Parser(void, ErrorEOF||ErrorFailedToMatchLiteral) { .parse = struct { const R = Result(void, ErrorEOF||ErrorFailedToMatchLiteral);
            fn f(input: []const u8, _: std.mem.Allocator) R {
                if (input.len == 0) {
                    return R { .value = @errorCast(Error.EOF) };
                }
                if (input[0] == ch) {
                    return R { .offset = 1, .value = {} };
                } else {
                    return R { .value = @errorCast(Error.FailedToMatchLiteral) };
                }
            }
        }.f};
    }

    pub fn set(comptime u8s: []const u8) Parser(void, ErrorEOF||ErrorFailedToMatchLiteral) {
        return Parser(void, ErrorEOF||ErrorFailedToMatchLiteral) { .parse = struct { const R = Result(void, ErrorEOF||ErrorFailedToMatchLiteral);
            fn f(input: []const u8, _: std.mem.Allocator) R {
                if (input.len == 0) {
                    return R { .value = @errorCast(Error.EOF) };
                }
                for (u8s) |ch| {
                    if (input[0] == ch) return R { .offset = 1, .value = {} };
                }
                return R { .value = @errorCast(Error.FailedToMatchLiteral) };
            }
        }.f};
    }

    pub fn seq(comptime str: []const u8) Parser(void, ErrorEOF||ErrorFailedToMatchLiteral) {
        return Parser(void, ErrorEOF||ErrorFailedToMatchLiteral) { .parse = struct { const R = Result(void, ErrorEOF||ErrorFailedToMatchLiteral);
            fn f(input: []const u8, _: std.mem.Allocator) R {
                if (input.len == 0) {
                    return R { .value = @errorCast(Error.EOF) };
                }
                if (std.mem.startsWith(u8, input, str)) {
                    return R { .offset = str.len, .value = {} };
                } else {
                    return R { .value = @errorCast(Error.FailedToMatchLiteral) };
                }
            }
        }.f };
    }
};

test "U8" {
    const testU8Ok = struct { fn f(p: anytype, input: []const u8) !void {
        var rst = p.parse(input, std.testing.allocator);
        defer rst.discard();
        errdefer std.debug.print("{any}\n", .{rst.value});
        try std.testing.expect(rst.isOk());
    }}.f;

    const testU8Err = struct { fn f(p: anytype, err: anyerror, input: []const u8) !void {
        var rst = p.parse(input, std.testing.allocator);
        defer rst.discard();
        errdefer std.debug.print("{any}\n", .{rst.value});
        try std.testing.expectError(err, rst.value);
    }}.f;

    try testU8Ok(U8.any, "a");
    try testU8Err(U8.any, Error.EOF, "");

    try testU8Ok(U8.one('a'), "a");
    try testU8Err(U8.one('a'), Error.EOF, "");
    try testU8Err(U8.one('a'), Error.FailedToMatchLiteral, "b");

    try testU8Ok(U8.set("abc"), "b");
    try testU8Err(U8.set("abc"), Error.EOF, "");
    try testU8Err(U8.set("abc"), Error.FailedToMatchLiteral, "d");

    try testU8Ok(U8.seq("abc"), "abc");
    try testU8Err(U8.seq("abc"), Error.EOF, "");
    try testU8Err(U8.seq("abc"), Error.FailedToMatchLiteral, "bc");
}