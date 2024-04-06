
const std = @import("std");

const parser = @import("parser.zig");
const Parser = parser.Parser;
const Result = parser.Result;

const pom = @import("root.zig");

pub const any = Parser(void, pom.Error) { .parse = struct { const R = Result(void, pom.Error);
    fn f(input: []const u8, _: std.mem.Allocator) R {
        if (input.len > 0) {
            return R { .offset = 1, .value = {} };
        } else {
            return R { .value = pom.Error.EOF };
        }
    }
}.f };

pub fn one(comptime ch: u8) Parser(void, pom.Error) {
    return Parser(void, pom.Error) { .parse = struct { const R = Result(void, pom.Error);
        fn f(input: []const u8, _: std.mem.Allocator) R {
            if (input.len == 0) {
                return R { .value = pom.Error.EOF };
            }
            if (input[0] == ch) {
                return R { .offset = 1, .value = {} };
            } else {
                return R { .value = pom.Error.FailedToMatchLiteral };
            }
        }
    }.f};
}

pub fn set(comptime u8s: []const u8) Parser(void, pom.Error) {
    return Parser(void, pom.Error) { .parse = struct { const R = Result(void, pom.Error);
        fn f(input: []const u8, _: std.mem.Allocator) R {
            if (input.len == 0) {
                return R { .value = pom.Error.EOF };
            }
            for (u8s) |ch| {
                if (input[0] == ch) return R { .offset = 1, .value = {} };
            }
            return R { .value = pom.Error.FailedToMatchLiteral };
        }
    }.f};
}

pub fn seq(comptime str: []const u8) Parser(void, pom.Error) {
    return Parser(void, pom.Error) { .parse = struct { const R = Result(void, pom.Error);
        fn f(input: []const u8, _: std.mem.Allocator) R {
            if (input.len == 0) {
                return R { .value = pom.Error.EOF };
            }
            if (std.mem.startsWith(u8, input, str)) {
                return R { .offset = str.len, .value = {} };
            } else {
                return R { .value = pom.Error.FailedToMatchLiteral };
            }
        }
    }.f };
}

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

    try testU8Ok(any, "a");
    try testU8Err(any, pom.Error.EOF, "");

    try testU8Ok(one('a'), "a");
    try testU8Err(one('a'), pom.Error.EOF, "");
    try testU8Err(one('a'), pom.Error.FailedToMatchLiteral, "b");

    try testU8Ok(set("abc"), "b");
    try testU8Err(set("abc"), pom.Error.EOF, "");
    try testU8Err(set("abc"), pom.Error.FailedToMatchLiteral, "d");

    try testU8Ok(seq("abc"), "abc");
    try testU8Err(seq("abc"), pom.Error.EOF, "");
    try testU8Err(seq("abc"), pom.Error.FailedToMatchLiteral, "bc");
}