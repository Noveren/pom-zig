
const std = @import("std");
const Allocator = std.mem.Allocator;

fn isErrorSet(comptime T: type) bool {
    const info: std.builtin.Type = @typeInfo(T);
    return switch (info) { else => false, .ErrorSet => true };
}

fn Result(comptime T: type, comptime E: type) type {
    comptime if (!isErrorSet(E)) @compileError("Expected error set type, found " ++ @typeName(E));
    return struct {
        value: E!T,
        offset: usize = 0,
        const Self = @This();

        pub inline fn isOk(self: Self) bool {
            return if (self.value) |_| true else |_| false;
        }

        pub fn discard(self: *Self) void {
            const info: std.builtin.Type = @typeInfo(T);
            switch (info) {
                else => {},
                .Struct, .Union, .Enum => {
                    if (comptime std.meta.hasMethod(T, "deinit")) {
                        if (self.value) |*ok| { ok.deinit(); } else |_| {}
                    }
                }
            }
        }
    };
}

pub const Error = error {
    EOF,
    ExpectForLiteral,
    FailedToMap,
    FailedToChoice,
    FailedToPred,
    FailedToParse,
} || std.mem.Allocator.Error;

pub fn ParserE(comptime T: type) type { return Parser(T, Error); }
pub const Void = Parser(void, Error);

pub fn Parser(comptime T: type, comptime E: type) type {
    comptime if (!isErrorSet(E)) @compileError("Expected error set type, found " ++ @typeName(E));
    return struct {
        parse: *const fn([]const u8, std.mem.Allocator) Result(T, E),
        const Self = @This();

        pub fn Val(comptime _: Self) type { return T; }
        pub fn Err(comptime _: Self) type { return E; }

        pub fn map(comptime self: Self, comptime M: type, comptime map_fn: fn(*T, std.mem.Allocator) ?M) Parser(M, Error||E) {
            return Parser(M, Error||E) { .parse = struct { const R = Result(M, Error||E);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    var rst = self.parse(input, allocator);
                    return R {
                        .offset = rst.offset,
                        .value  = if (rst.value) |*ok| map_fn(ok, allocator) orelse Error.FailedToMap else |err| err,
                    };
                }
            }.f };
        }

        pub fn mapErr(comptime self: Self, comptime NewE: type, comptime map_err_fn: fn(E) NewE) Parser(T, E||NewE) {
            return Parser(T, E||NewE) { .parse = struct { const R = Result(T, E||NewE);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    const rst = self.parse(input, allocator);
                    return R {
                        .offset = rst.offset,
                        .value  = if (rst.value) |ok| ok else |err| map_err_fn(err),
                    };
                }
            }.f };
        }
    };
}

pub const U8 = struct {
    pub fn one(comptime ch: u8) Void {
        return Void { .parse = struct { const R = Result(void, Error);
            fn f(input: []const u8, _: std.mem.Allocator) R {
                if (input.len == 0) {
                    return R { .value = Error.EOF };
                }
                if (input[0] == ch) {
                    return R { .offset = 1, .value = {} };
                } else {
                    return R { .value = Error.ExpectForLiteral };
                }
            }
        }.f};
    }
};

test "map and mapErr" {
    // const T: type = Parser(u8, u8);
    // _ = T;
    const p1: ParserE(u8) = comptime U8.one('1')
        .map(u8, struct { fn f(_: *void, _: Allocator) ?u8 {
            return 1;
        }}.f)
    ;

    const p2: ParserE(u8) = comptime U8.one('1')
        .map(u8, struct { fn f(_: *void, _: Allocator) ?u8 {
            return null;
        }}.f)
    ;

    const NewError = error {
        IsOne,
    };

    const p3: Parser(u8, NewError||p2.Err()) = comptime p2
        .mapErr(NewError, struct { fn f(_: p2.Err()) NewError {
            return NewError.IsOne;
        }}.f)
    ;

    const rst1 = U8.one('1').parse("1", std.testing.allocator);
    try std.testing.expect(rst1.isOk());

    const rst2 = U8.one('1').parse("", std.testing.allocator);
    try std.testing.expectError(Error.EOF, rst2.value);

    const rst3 = U8.one('1').parse("2", std.testing.allocator);
    try std.testing.expectError(Error.ExpectForLiteral, rst3.value);

    const rst4 = p1.parse("1", std.testing.allocator);
    const v = try rst4.value;
    try std.testing.expect(v == 1);

    const rst5 = p2.parse("1", std.testing.allocator);
    try std.testing.expectError(Error.FailedToMap, rst5.value);

    const rst6 = p3.parse("1", std.testing.allocator);
    try std.testing.expectError(NewError.IsOne, rst6.value);
}