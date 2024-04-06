
const std = @import("std");

const typeCheck = @import("typeCheck.zig");

const parser = @import("parser.zig");
const Parser = parser.Parser;
const Result = parser.Result;

const pom = @import("root.zig");

pub fn Choice(comptime T: type) type {
    return struct {
        /// `p: Parser(T, AnyError)`
        pub fn with(comptime p: anytype) ChoiceN(T, 1) {
            comptime parser.assertParserT(@TypeOf(p), T);
            return ChoiceN(T, 1) {
                ._parsers = [1]Parser(T, anyerror) { p.castSupE(anyerror) },
            };
        }
    };
}

fn ChoiceN(comptime T: type, comptime N: usize) type {
    return struct {
        _parsers: [N]Parser(T, anyerror),

        /// `p: Parser(T, AnyError)`
        pub fn with(comptime self: @This(), comptime p: anytype) ChoiceN(T, N + 1) {
            comptime parser.assertParserT(@TypeOf(p), T);
            return ChoiceN(T, N + 1) {
                ._parsers = self._parsers ++ [1]Parser(T, anyerror) { p.castSupE(anyerror) },
            };
        }

        /// `pom.Error.FailedToChoice`
        pub fn build(comptime self: @This()) Parser(T, pom.Error) {
            return Parser(T, pom.Error) { .parse = struct { const R = Result(T, pom.Error);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    for (self._parsers) |p| {
                        const rst = p.parse(input, allocator);
                        if (rst.isOk()) {
                            return R { .offset = rst.offset, .value = @errorCast(rst.value) };
                        }
                    }
                    return R { .value = pom.Error.FailedToChoice };
                }
            }.f };
        }
    };
}

pub fn ChoicePrefix(comptime T: type, comptime E: type) type {
    return struct {
        /// `p: Parser(T, SubErrorSet_E)`
        pub fn with(comptime p: anytype, comptime pfx: anytype) ChoicePrefixN(T, E, 1) {
            comptime { parser.assertParserT(@TypeOf(p), T); typeCheck.assertSubErrorSet(@TypeOf(p).TypeE, E); }
            comptime parser.assertParserT(@TypeOf(pfx), void);
            return ChoicePrefixN(T, E, 1) {
                ._parsers = [1]Parser(T, E) { p.castSupE(E) },
                ._prefixs = [1]Parser(void, anyerror) { pfx.castSupE(anyerror) },
            };
        }
    };
}

fn ChoicePrefixN(comptime T: type, comptime E: type, comptime N: usize) type {
    comptime typeCheck.assertTypeClass(.ErrorSet, E);
    return struct {
        _parsers: [N]Parser(T, E),
        _prefixs: [N]Parser(void, anyerror),
        const Self = @This();

        pub fn with(comptime self: Self, comptime p: anytype, comptime pfx: anyerror) ChoicePrefixN(T, E, N + 1) {
            comptime { parser.assertParserT(@TypeOf(p), T); typeCheck.assertSubErrorSet(@TypeOf(p).TypeE, E); }
            comptime parser.assertParserT(@TypeOf(pfx), void);
            return ChoicePrefixN(T, E, N + 1) {
                ._parsers = self._parsers ++ [1]Parser(T, E) { p },
                ._prefixs = self._prefixs ++ [1]Parser(void, anyerror) { pfx.castSupE(anyerror) },
            };
        }

        pub fn build(comptime self: Self) Parser(T, E||pom.Error) {
            return Parser(T, E||pom.Error) { .parse = struct { const R = Result(T, E||pom.Error);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    for (self._parsers, self._prefixs) |p, pfx| {
                        const rst_pfx = pfx.parse(input, allocator);
                        if (rst_pfx.isOk()) {
                            const rst = p.parse(input, allocator);
                            return R { .offset = rst.offset, .value = rst.value };
                        }
                    }
                    return R { .value = pom.Error.FailedToChoice };
                }
            }.f };
        }
    };
}