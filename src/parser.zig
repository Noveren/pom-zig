
const std = @import("std");
const Allocator = std.mem.Allocator;

const typeCheck = @import("typeCheck.zig");
const pom = @import("root.zig");
const List = @import("List.zig").List;

/// `T: anytype, E: ErrorSet`;
pub fn Result(comptime T: type, comptime E: type) type {
    comptime typeCheck.assertTypeClass(.ErrorSet, E);
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

pub fn isParser(comptime P: type) bool {
    return @typeInfo(P) == .Struct
        and @hasDecl(P, "TypeT")
        and @hasDecl(P, "TypeE")
        and P == Parser(P.TypeT, P.TypeE)
    ;
}

pub fn assertParser(comptime P: type) void {
    const err_msg = "Expected pom.Parser(T, E), found '" ++ @typeName(P) ++ "'";
    if (!isParser(P)) @compileError(err_msg);
}

pub fn assertParserT(comptime P: type, comptime T: type) void {
    const err_msg = "Expected pom.Parser(void, " ++ @typeName(T) ++ "), found '" ++ @typeName(P) ++ "'";
    if (!isParser(P) or P.TypeT != T) @compileError(err_msg);
}

/// `T: anytype, E: ErrorSet`;
pub fn Parser(comptime T: type, comptime E: type) type {
    comptime typeCheck.assertTypeClass(.ErrorSet, E);
return struct {

parse: *const fn([]const u8, std.mem.Allocator) Result(T, E),

const Self = @This();

pub const TypeT: type = T;
pub const TypeE: type = E;

// ==========================================================================

pub fn discard(comptime self: Self) Parser(void, E) {
    return Parser(void, E) { .parse = struct { const R = Result(void, E);
        fn f(input: []const u8, allocator: std.mem.Allocator) R {
            var rst = self.parse(input, allocator);
            return R {
                .offset = rst.offset,
                .value = if (rst.value) |_| rst.discard() else |err| err,
            };
        }
    }.f };
}


pub fn slice(comptime self: Self) Parser([]const u8, E) {
    return Parser([]const u8, E) { .parse = struct { const R = Result([]const u8, E);
        fn f(input: []const u8, allocator: std.mem.Allocator) R {
            var rst = self.parse(input, allocator);
            return R {
                .offset = rst.offset,
                .value = if (rst.value) |_| blk: {
                    rst.discard();
                    break :blk input[0..rst.offset];
                } else |err| err,
            };
        }
    }.f };
}

/// `pfx: Parser(void, OtherError)`
pub fn prefix(comptime self: Self, comptime pfx: anytype) Parser(T, E||@TypeOf(pfx).TypeE) {
    comptime assertParserT(@TypeOf(pfx), void);
    return Parser(T, E||@TypeOf(pfx).TypeE) { .parse = struct { const R = Result(T, E||@TypeOf(pfx).TypeE);
        fn f(input: []const u8, allocator: std.mem.Allocator) R {
            const rst_pfx = pfx.parse(input, allocator);
            if (rst_pfx.value) |_| {
                const rst = self.parse(input[rst_pfx.offset..], allocator);
                return R { .offset = rst_pfx.offset + rst.offset, .value = rst.value };
            } else |err| {
                return R { .offset = rst_pfx.offset, .value = err };
            }
        }
    }.f };
}

/// `sfx: Parser(void, OtherError)`
pub fn suffix(comptime self: Self, comptime sfx: anytype) Parser(T, E||@TypeOf(sfx).TypeE) {
    comptime assertParserT(@TypeOf(sfx), void);
    return Parser(T, E||@TypeOf(sfx).TypeE) { .parse = struct { const R = Result(T, E||@TypeOf(sfx).TypeE);
        fn f(input: []const u8, allocator: std.mem.Allocator) R {
            var rst = self.parse(input, allocator);
            if (rst.value) |ok| {
                const rst_sfx = sfx.parse(input[rst.offset..], allocator);
                return R {
                    .offset = rst.offset + rst_sfx.offset,
                    .value = if (rst_sfx.value) |_| ok else |err| blk: {
                        rst.discard();
                        break: blk err;
                    }
                };
            } else |err| {
                return R { .offset = rst.offset, .value = err };
            }
        }
    }.f };
}

pub fn pred(comptime self: Self, comptime COND: bool) Parser(void, E||pom.ErrorFailedToPred) {
    return Parser(void, E||pom.ErrorFailedToPred) { .parse = struct { const R = Result(void, E||pom.ErrorFailedToPred);
        fn f(input: []const u8, allocator: std.mem.Allocator) R {
            var rst = self.parse(input, allocator);
            rst.discard();
            if (rst.value) |_| {
                return R { .value = if (comptime COND) { } else @errorCast(pom.Error.FailedToPred) };
            } else |err| {
                return R { .value = if (comptime COND) err else { } };
            }
        }
    }.f };
}

pub fn optional(comptime self: Self) Parser(void, pom.ErrorNOP) {
    return Parser(void, pom.ErrorNOP) { .parse = struct { const R = Result(void, pom.ErrorNOP);
        fn f(input: []const u8, allocator: std.mem.Allocator) R {
            var rst = self.parse(input, allocator);
            rst.discard();
            return R { .offset = if (rst.isOk()) rst.offset else 0, .value = {} };
        }
    }.f };
}

// ==========================================================================
/// Map the `result.value: T` of `self` by `map_fn` if `self` parsed `intput` successfully. 
/// You have to deinit the `resutl.value` in `map_fn` if it won't be accessed anymore.
pub fn mapFn(comptime self: Self, comptime M: type, comptime map_fn: fn(*T, std.mem.Allocator) ?M) Parser(M, E||pom.ErrorFailedToMap) {
    return Parser(M, E||pom.ErrorFailedToMap) { .parse = struct { const R = Result(M, E||pom.ErrorFailedToMap);
        fn f(input: []const u8, allocator: std.mem.Allocator) R {
            var rst = self.parse(input, allocator);
            return R {
                .offset = rst.offset,
                .value  = if (rst.value) |*ok| map_fn(ok, allocator) orelse pom.ErrorFailedToMap else |err| err,
            };
        }
    }.f };
}

/// Map the `result.value: T` into `comptime new_ok` if `self` parsed `intput` successfully. 
pub fn map(comptime self: Self, comptime new_ok: anytype) Parser(@TypeOf(new_ok), E) {
    return Parser(@TypeOf(new_ok), E) { .parse = struct { const R = Result(@TypeOf(new_ok), E);
        fn f(input: []const u8, allocator: std.mem.Allocator) R {
            var rst = self.parse(input, allocator);
            defer rst.discard();
            return R {
                .offset = rst.offset,
                .value  = if (rst.value) |_| new_ok else |err| err,
            };
        }
    }.f };
}

/// Map the `result.value: E` of `self` by `map_err_fn` if `self` failed to parse `input`. 
/// You can check the `result.value: E` and then return a new error.
pub fn mapErrFn(comptime self: Self, comptime NewE: type, comptime map_err_fn: fn(E) NewE) Parser(T, NewE) {
    comptime typeCheck.assertTypeClass(.ErrorSet, NewE);
    return Parser(T, NewE) { .parse = struct { const R = Result(T, NewE);
        fn f(input: []const u8, allocator: std.mem.Allocator) R {
            const rst = self.parse(input, allocator);
            return R {
                .offset = rst.offset,
                .value  = if (rst.value) |ok| ok else |err| map_err_fn(err),
            };
        }
    }.f };
}

/// Map the `result.value: E` of `self` directly if `self` failed to parse `input`.
pub fn mapErr(comptime self: Self, comptime new_err: anytype) Parser(T, @TypeOf(new_err)) {
    const NewE: type = @TypeOf(new_err);
    comptime typeCheck.assertTypeClass(.ErrorSet, NewE);
    return Parser(T, NewE) { .parse = struct { const R = Result(T, NewE);
        fn f(input: []const u8, allocator: std.mem.Allocator) R {
            const rst = self.parse(input, allocator);
            return R {
                .offset = rst.offset,
                .value  = if (rst.value) |ok| ok else |_| new_err,
            };
        }
    }.f };
}

pub fn castSupE(comptime self: Self, comptime SupE: type) Parser(T, SupE) {
    comptime typeCheck.assertSubErrorSet(E, SupE);
    return Parser(T, SupE) { .parse = struct { const R = Result(T, SupE);
        fn f(input: []const u8, allocator: std.mem.Allocator) R {
            const rst = self.parse(input, allocator);
            return R {
                .offset = rst.offset,
                .value  = if (rst.value) |ok| ok else |err| err,
            };
        }
    }.f };
}

// ==========================================================================
/// `sep: ?Parser(void, AnyError)`
fn manyCollect(comptime self: Self, comptime sep: anytype, comptime mode: u8, comptime N: usize) Parser(List(T), Allocator.Error||E) {
    comptime var sep_is_not_null: bool = false;
    comptime switch (@typeInfo(@TypeOf(sep))) {
        else => { assertParserT(@TypeOf(sep), void); sep_is_not_null = true; },
        .Optional => if (sep != null) unreachable,
    };
    return Parser(List(T), Allocator.Error||E) { .parse = struct { const R = Result(List(T), Allocator.Error||E);
        fn f(input: []const u8, allocator: std.mem.Allocator) R {
            var list = List(T).init(allocator);
            var offset: usize = 0;
            var hasSepBefore: bool = false;
            var rst: Result(T, E) = self.parse(input, allocator);

            if (comptime mode < 2) {
                if (rst.isOk() and rst.offset == 0) { @panic("TODO Infity Loop\n"); }
            }

            while (rst.value) |ok| : (rst = self.parse(input[offset..], allocator)) {
                list.append(ok) catch {
                    rst.discard();
                    list.deinit();
                    return R { .offset = offset, .value = Allocator.Error.OutOfMemory };
                };
                offset += rst.offset;
                
                if (comptime mode > 1) { if (list.len() == N) break; }
                
                if (comptime sep_is_not_null) {
                    const r_sep = sep.parse(input[offset..], allocator);
                    if (r_sep.value) |_| {
                        offset += r_sep.offset;
                        hasSepBefore = true;
                    } else |_| break;
                }
            } else |err| {
                const failed: bool = switch (comptime mode) {
                    0 => hasSepBefore,
                    1 => hasSepBefore or (list.len() == 0),
                    else => list.len() != N,
                };
                if (failed) {
                    offset += rst.offset;
                    list.deinit();
                    return R { .offset = offset, .value = err };
                }
            }
            return R { .offset = offset, .value = list };
        }
    }.f };
}

/// `sep: ?Parser(void, AnyError)`
fn many(comptime self: Self, comptime sep: anytype, comptime mode: u8, comptime N: usize) Parser(void, E) {
    comptime var sep_is_not_null: bool = false;
    comptime if (@TypeOf(sep) != @TypeOf(null)) {
        assertParserT(@TypeOf(sep), void); sep_is_not_null = true; 
    };
    return Parser(void, E) { .parse = struct { const R = Result(void, E);
        fn f(input: []const u8, allocator: std.mem.Allocator) R {
            var offset: usize = 0;
            var counter: usize = 0;
            var hasSepBefore: bool = false;
            var rst: Result(T, E) = self.parse(input, allocator);

            if (comptime mode < 2) {
                if (rst.isOk() and rst.offset == 0) { @panic("TODO Infity Loop\n"); }
            }

            while (rst.value) |_| : (rst = self.parse(input[offset..], allocator)) {
                counter += 1;
                offset += rst.offset;
                rst.discard();
                
                if (comptime mode > 1) { if (counter == N) break; }
                
                if (comptime sep_is_not_null) {
                    const r_sep = sep.parse(input[offset..], allocator);
                    if (r_sep.value) |_| {
                        offset += r_sep.offset;
                        hasSepBefore = true;
                    } else |_| break;
                }
            } else |err| {
                const failed: bool = switch (comptime mode) {
                    0 => hasSepBefore,
                    1 => hasSepBefore or (counter == 0),
                    else => counter != N,
                };
                if (failed) {
                    offset += rst.offset;
                    return R { .offset = offset, .value = err };
                }
            }
            return R { .offset = offset, .value = {} };
        }
    }.f };
}

/// `sep: ?Parser(void, AnyError)`
pub fn zeroMore(comptime self: Self, comptime sep: anytype) Parser(void, E) {
    return Parser(void, E) { .parse = struct { const R = Result(void, E);
        fn f(input: []const u8, allocator: std.mem.Allocator) R {
            return self.many(sep, 0, 0).parse(input, allocator);
        }
    }.f };
}

/// `sep: ?Parser(void, AnyError)`
pub fn oneMore(comptime self: Self, comptime sep: anytype) Parser(void, E) {
    return Parser(void, E) { .parse = struct { const R = Result(void, E);
        fn f(input: []const u8, allocator: std.mem.Allocator) R {
            return self.many(sep, 1, 0).parse(input, allocator);
        }
    }.f };
}

/// `sep: ?Parser(void, AnyError)`
pub fn times(comptime self: Self, comptime N: usize, comptime sep: anytype) Parser(void, E) {
    return Parser(void, E) { .parse = struct { const R = Result(void, E);
        fn f(input: []const u8, allocator: std.mem.Allocator) R {
            return self.many(sep, 2, N).parse(input, allocator);
        }
    }.f };
}

/// `sep: ?Parser(void, AnyError)`
pub fn zeroMoreCollect(comptime self: Self, comptime sep: anytype) Parser(List(T), Allocator.Error||E) {
    return Parser(List(T), Allocator.Error||E) { .parse = struct { const R = Result(List(T), Allocator.Error||E);
        fn f(input: []const u8, allocator: std.mem.Allocator) R {
            return self.manyCollect(sep, 0, 0).parse(input, allocator);
        }
    }.f };
}

/// `sep: ?Parser(void, AnyError)`
pub fn oneMoreCollect(comptime self: Self, comptime sep: anytype) Parser(List(T), Allocator.Error||E) {
    return Parser(List(T), Allocator.Error||E) { .parse = struct { const R = Result(List(T), Allocator.Error||E);
        fn f(input: []const u8, allocator: std.mem.Allocator) R {
            return self.manyCollect(sep, 1, 0).parse(input, allocator);
        }
    }.f };
}

/// `sep: ?Parser(void, AnyError)`
pub fn timesCollect(comptime self: Self, comptime N: usize, comptime sep: anytype) Parser(List(T), Allocator.Error||E) {
    return Parser(List(T), Allocator.Error||E) { .parse = struct { const R = Result(List(T), Allocator.Error||E);
        fn f(input: []const u8, allocator: std.mem.Allocator) R {
            return self.manyCollect(sep, 2, N).parse(input, allocator);
        }
    }.f };
}

// ==========================================================================
pub fn Choice() type {
    return struct {
        pub fn with(comptime p: anytype) ChoiceN(1) {
            comptime { assertParserT(@TypeOf(p), T); typeCheck.assertSubErrorSet(@TypeOf(p).TypeE, E); }
            return ChoiceN(1) {
                ._parsers = [1]Parser(T, E) { p.castSupE(E) },
            };
        }

        pub fn withPrefix(comptime p: anytype, comptime pfx: Parser(void, pom.ErrorFailedToPred)) ChoicePrefixN(1) {
            comptime { assertParserT(@TypeOf(p), T); typeCheck.assertSubErrorSet(@TypeOf(p).TypeE, E); }
            return ChoicePrefixN(1) {
                ._parsers = [1]Parser(T, E) { p.castSupE(E) },
                ._prefixs = [1]Parser(void, pom.ErrorFailedToPred) { pfx }
            };
        }
    };
}

fn ChoiceN(comptime N: usize) type {
    return struct {
        _parsers: [N]Parser(T, E),

        pub fn with(comptime self: @This(), comptime p: anytype) ChoiceN(N + 1) {
            comptime { assertParserT(@TypeOf(p), T); typeCheck.assertSubErrorSet(@TypeOf(p).TypeE, E); }
            return ChoiceN(N + 1) {
                ._parsers = self._parsers ++ [1]Parser(T, E) { p.castSupE(E) },
            };
        }

        pub fn build(comptime self: @This()) Parser(T, E||pom.ErrorFailedToChoice) {
            return Parser(T, E||pom.ErrorFailedToChoice) { .parse = struct { const R = Result(T, E||pom.ErrorFailedToChoice);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    for (self._parsers) |parser| {
                        const rst = parser.parse(input, allocator);
                        if (rst.isOk()) {
                            return R { .offset = rst.offset, .value = rst.value };
                        }
                    }
                    return R { .value = @errorCast(pom.Error.FailedToChoice) };
                }
            }.f };
        }
    };
}

fn ChoicePrefixN(comptime N: usize) type {
    return struct {
        _parsers: [N]Parser(T, E),
        _prefixs: [N]Parser(void, pom.ErrorFailedToPred),
        const Self = @This();

        pub fn withPrefix(comptime self: @This(), comptime p: anytype, comptime pfx: Parser(void, pom.ErrorFailedToPred)) ChoicePrefixN(N + 1) {
            comptime { assertParserT(@TypeOf(p), T); typeCheck.assertSubErrorSet(@TypeOf(p).TypeE, E); }
            return ChoicePrefixN(N + 1) {
                ._parsers = self._parsers ++ [1]Parser(T, E) { p.castSupE(E) },
                ._prefixs = self._prefixs ++ [1]Parser(void, pom.ErrorFailedToPred) { pfx }
            };
        }

        pub fn build(comptime self: @This()) Parser(T, E||pom.Error.FailedToChoice) {
            return Parser(T, E||pom.Error.FailedToChoice) { .parse = struct { const R = Result(T, E||pom.Error.FailedToChoice);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    for (self._parsers, self._prefixs) |parser, pfx| {
                        const rst_pfx = pfx.parse(input, allocator);
                        if (rst_pfx.isOk()) {
                            const rst = parser.parse(input, allocator);
                            return R { .offset = rst.offset, .value = rst.value };
                        }
                    }
                    return R { .value = @errorCast(pom.Error.FailedToChoice) };
                }
            }.f };
        }
    };
}

};}

test "one" {
    var rst = comptime pom.U8.one('1').timesCollect(3, pom.U8.one(',')).slice().parse("1,1,1", std.testing.allocator);
    defer rst.discard();
    try std.testing.expect(rst.isOk());
    const v = try rst.value;
    try std.testing.expectEqualStrings("1,1,1", v);
}
// TODO 将 Choice 和 ChoicePrefix 提出 Parser
const pp = Parser(void, pom.Error).Choice()
    .with(pom.U8.one('1'))
    .with(pom.U8.one('2'))
    .build()
;

test "choice" {
    var rst = pp.parse("1", std.testing.allocator);
    defer rst.discard();
}