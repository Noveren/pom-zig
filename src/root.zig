
const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn List(comptime T: type) type {
    return struct {
        memory: std.ArrayList(T),
        const Self = @This();

        pub fn init(allocator: std.mem.Allocator) Self {
            return Self { .memory = std.ArrayList(T).init(allocator) };
        }

        /// `deinit` the items in the list and the list
        pub fn deinit(self: Self) void {
            if (comptime std.meta.hasMethod(T, "deinit")) {
                for (self.memory.items) |*i| {
                    i.deinit();
                }
            }
            self.memory.deinit();
        }

        /// just `deinit` the list, maybe lead to memory leak
        pub inline fn rawDeinit(self: Self) void {
            self.memory.deinit();
        }

        pub inline fn append(self: *Self, item: T) std.mem.Allocator.Error!void {
            try self.memory.append(item);
        }

        pub inline fn items(self: Self) []const T {
            return self.memory.items;
        }

        pub inline fn len(self: Self) usize {
            return self.memory.items.len;
        }
    };
}

pub fn Result(comptime T: type, comptime E: type) type {
    comptime TypeAssert.isErrorSet(E);
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

pub const ErrorNop = error {};
pub const Error = error {
    EOF,
    ExpectForLiteral,
    FailedToMap,
    FailedToChoice,
    FailedToPred,
    FailedToParse,
} || ErrorNop;

// fn typeCheckParser(comptime P: type) void {
//     const err_msg = "Expected pom.Parser(T, E), found '" ++ @typeName(P) ++ "'";
//     if (@typeInfo(P) != .Struct) @compileError(err_msg);
//     if (!@hasDecl(P, "Val")) @compileError(err_msg);
//     if (!@hasDecl(P, "Err") or )
// }

const TypeAssert = struct {
    fn isErrorSet(comptime T: type) void {
        const info: std.builtin.Type = @typeInfo(T);
        switch (info) {
            else => @compileError("Expected error set type, found '" ++ @typeName(T) ++ "'"),
            .ErrorSet => {}
        }
    }

    fn isParserBase(comptime P: type) bool {
        return @typeInfo(P) == .Struct
           and @hasDecl(P, "Val")
           and @hasDecl(P, "Err")
           and P == Parser(P.Val, P.Err)
        ;
    }

    fn isParser(comptime P: type) void {
        if (!isParserBase(P)) @compileError("Expected pom.Parser(T, E), found '" ++ @typeName(P) ++ "'");
    }

    fn isVoid(comptime P: type) void {
        if (!isParserBase(P) or P.Val != void) @compileError("Expected pom.Parser(void, E), found '" ++ @typeName(P) ++ "'");
    }
};

test "Type Assert" {
    const TempError = error { Temp };
    comptime TypeAssert.isErrorSet(TempError);
    // comptime TypeAssert.isErrorSet(u8);

    comptime TypeAssert.isParser(Parser(void, TempError));
    // comptime TypeAssert.isParser(u8);
    // comptime TypeAssert.isParser(struct {});
    // comptime TypeAssert.isParser(struct { pub const Val = void; pub const Err = TempError; });

    // comptime TypeAssert.isVoid(Parser(u8, TempError));
}

pub fn ParserE(comptime T: type) type { return Parser(T, Error); }
pub fn Void(comptime E: type) type { return Parser(void, E); }
pub const VoidE = Void(Error);

pub fn Parser(comptime T: type, comptime E: type) type {
    comptime TypeAssert.isErrorSet(E);
    return struct {
        parse: *const fn([]const u8, std.mem.Allocator) Result(T, E),
        const Self = @This();
        pub const Val: type = T;
        pub const Err: type = E;

        pub fn discard(comptime self: Self) Void(E) {
            return Void(E) { .parse = struct { const R = Result(void, E);
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

        pub fn map(comptime self: Self, comptime M: type, comptime map_fn: fn(*T, std.mem.Allocator) ?M) Parser(M, Error||E) {
            return Parser(M, E||Error) { .parse = struct { const R = Result(M, E||Error);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    var rst = self.parse(input, allocator);
                    return R {
                        .offset = rst.offset,
                        .value  = if (rst.value) |*ok| map_fn(ok, allocator) orelse Error.FailedToMap else |err| err,
                    };
                }
            }.f };
        }

        pub fn mapErr(comptime self: Self, comptime NewE: type, comptime map_err_fn: fn(E) NewE) Parser(T, NewE) {
            comptime TypeAssert.isErrorSet(NewE);
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

        pub fn castErr(comptime self: Self, comptime new_err: anytype) Parser(T, @TypeOf(new_err)) {
            const NewE: type = @TypeOf(new_err);
            comptime TypeAssert.isErrorSet(NewE);
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

        // ========================================================
        /// mode: 0 - zeroMore, 1 - oneMore, 2 - times(N)
        /// TODO T == void => many -> Parser(void, E)
        /// sep: ?Void(E)
        fn many(comptime self: Self, comptime sep: ?Void(Error), comptime mode: u8, comptime N: usize) Parser(List(T), Allocator.Error||E) {
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
                        // TODO 是否要限制最大次数
                        if (comptime mode > 1) { if (list.len() == N) break; }
                        
                        if (comptime sep) |p_sep| {
                            const r_sep = p_sep.parse(input[offset..], allocator);
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

        pub fn zeroMore(comptime self: Self, comptime sep: ?Void(Error)) Parser(List(T), Allocator.Error||E) {
            return Parser(List(T), Allocator.Error||E) { .parse = struct { const R = Result(List(T), Allocator.Error||E);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    return self.many(sep, 0, 0).parse(input, allocator);
                }
            }.f };
        }

        pub fn oneMore(comptime self: Self, comptime sep: ?Void(Error)) Parser(List(T), Allocator.Error||E) {
            return Parser(List(T), Allocator.Error||E) { .parse = struct { const R = Result(List(T), Allocator.Error||E);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    return self.many(sep, 1, 0).parse(input, allocator);
                }
            }.f };
        }

        pub fn times(comptime self: Self, comptime N: usize, comptime sep: ?Void(Error)) Parser(List(T), Allocator.Error||E) {
            if (comptime N == 0) @compileError("Times N == 0");
            return Parser(List(T), Allocator.Error||E) { .parse = struct { const R = Result(List(T), Allocator.Error||E);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    return self.many(sep, 2, N).parse(input, allocator);
                }
            }.f };
        }

        fn manyVoid(comptime self: Self, comptime sep: ?Void(Error), comptime mode: u8, comptime N: usize) Void(E) {
            return Void(E) { .parse = struct { const R = Result(void, E);
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
                        // TODO 是否要限制最大次数
                        if (comptime mode > 1) { if (counter == N) break; }
                        
                        if (comptime sep) |p_sep| {
                            const r_sep = p_sep.parse(input[offset..], allocator);
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

        /// if sep == null, zeroMoreVoid 总是成功
        pub fn zeroMoreVoid(comptime self: Self, comptime sep: ?Void(Error)) Void(E) {
            return Void(E) { .parse = struct { const R = Result(void, E);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    return self.manyVoid(sep, 0, 0).parse(input, allocator);
                }
            }.f };
        }

        pub fn oneMoreVoid(comptime self: Self, comptime sep: ?Void(Error)) Void(E) {
            return Void(E) { .parse = struct { const R = Result(void, E);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    return self.manyVoid(sep, 1, 0).parse(input, allocator);
                }
            }.f };
        }

        pub fn timesVoid(comptime self: Self, comptime N: usize, comptime sep: ?Void(Error)) Void(E) {
            if (comptime N == 0) @compileError("Times N == 0");
            return Void(E) { .parse = struct { const R = Result(void, E);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    return self.manyVoid(sep, 2, N).parse(input, allocator);
                }
            }.f };
        }

        /// `pfx: Void(E)`
        pub fn prefix(comptime self: Self, comptime pfx: anytype) Parser(T, E||@TypeOf(pfx).Err) {
            comptime TypeAssert.isVoid(@TypeOf(pfx));
            return Parser(T, E||@TypeOf(pfx).Err) { .parse = struct { const R = Result(T, E||@TypeOf(pfx).Err);
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

        /// `sfx: Void(E)`
        pub fn suffix(comptime self: Self, comptime sfx: anytype) Parser(T, E||@TypeOf(sfx).Err) {
            comptime TypeAssert.isVoid(@TypeOf(sfx));
            return Parser(T, E||@TypeOf(sfx).Err) { .parse = struct { const R = Result(T, E||@TypeOf(sfx).Err);
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

        pub fn pred(comptime self: Self, comptime COND: bool) Void(E||Error) {
            return Void(E||Error) { .parse = struct { const R = Result(void, E||Error);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    var rst = self.parse(input, allocator);
                    rst.discard();
                    if (rst.value) |_| {
                        return R { .value = if (comptime COND) { } else Err.FailedToPred };
                    } else |err| {
                        return R { .value = if (comptime COND) err else { } };
                    }
                }
            }.f };
        }

        pub fn optional(comptime self: Self) Void(ErrorNop) {
            return Void(ErrorNop) { .parse = struct { const R = Result(void, ErrorNop);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    var rst = self.parse(input, allocator);
                    rst.discard();
                    return R { .offset = if (rst.isOk()) rst.offset else 0, .value = {} };
                }
            }.f };
        }
    };
}

pub fn Choice(comptime T: type, comptime E: type) type {
    comptime TypeAssert.isErrorSet(E);
    return struct {
        pub fn with(comptime p: Parser(T, E)) ChoiceN(T, E, 1) {
            return ChoiceN(T, E, 1) {
                ._parsers = [1]Parser(T, E) { p },
            };
        }

        pub fn withPrefix(comptime p: Parser(T, E), comptime pfx: Void(Error)) ChoicePrefixN(T, E, 1) {
            return ChoicePrefixN(T, E, 1) {
                ._parsers = [1]Parser(T, E) { p },
                ._prefixs = [1]Void(Error) { pfx }
            };
        }
    };
}

fn ChoiceN(comptime T: type, comptime E: type, comptime N: usize) type {
    comptime TypeAssert.isErrorSet(E);
    return struct {
        _parsers: [N]Parser(T, E),
        const Self = @This();

        pub fn with(comptime self: Self, comptime p: Parser(T, E)) ChoiceN(T, E, N + 1) {
            return ChoiceN(T, E, N + 1) {
                ._parsers = self._parsers ++ [1]Parser(T, E) { p },
            };
        }

        pub fn build(comptime self: Self) Parser(T, Error) {
            return Parser(T, Error) { .parse = struct { const R = Result(T, Error);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    for (self._parsers) |parser| {
                        const rst = parser.parse(input, allocator);
                        if (rst.isOk()) {
                            return R { .offset = rst.offset, .value = rst.value };
                        }
                    }
                    return R { .value = Error.FailedToChoice };
                }
            }.f };
        }
    };
}

fn ChoicePrefixN(comptime T: type, comptime E: type, comptime N: usize) type {
    comptime TypeAssert.isErrorSet(E);
    return struct {
        _parsers: [N]Parser(T, E),
        _prefixs: [N]Void(Error),
        const Self = @This();

        pub fn withPrefix(comptime self: Self, comptime p: Parser(T, E), comptime pfx: Void(Error)) ChoicePrefixN(T, E, N + 1) {
            return ChoicePrefixN(T, E, N + 1) {
                ._parsers = self._parsers ++ [1]Parser(T, E) { p },
                ._prefixs = self._prefixs ++ [1]Void(Error) { pfx }
            };
        }

        pub fn build(comptime self: Self, comptime new_err: anytype) Parser(T, E||@TypeOf(new_err)) {
            const NewE: type = @TypeOf(new_err);
            comptime TypeAssert.isErrorSet(NewE);
            return Parser(T, E||@TypeOf(new_err)) { .parse = struct { const R = Result(T, E||@TypeOf(new_err));
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    for (self._parsers, self._prefixs) |parser, pfx| {
                        const rst_pfx = pfx.parse(input, allocator);
                        if (rst_pfx.isOk()) {
                            const rst = parser.parse(input, allocator);
                            return R { .offset = rst.offset, .value = rst.value }; // T, E
                        }
                    }
                    return R { .value = new_err }; // NewE
                }
            }.f };
        }
    };
}


pub const U8 = struct {
    pub const any = Void(Error) { .parse = struct { const R = Result(void, Error);
        fn f(input: []const u8, _: std.mem.Allocator) R {
            if (input.len > 0) {
                return R { .offset = 1, .value = {} };
            } else {
                return R { .value = Error.EOF };
            }
        }
    }.f };

    pub fn one(comptime ch: u8) Void(Error) {
        return Void(Error) { .parse = struct { const R = Result(void, Error);
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

    pub fn set(comptime u8s: []const u8) Void(Error) {
        return Void(Error) { .parse = struct { const R = Result(void, Error);
            fn f(input: []const u8, _: std.mem.Allocator) R {
                if (input.len == 0) {
                    return R { .value = Error.EOF };
                }
                for (u8s) |ch| {
                    if (input[0] == ch) return R { .offset = 1, .value = {} };
                }
                return R { .value = Error.ExpectForLiteral };
            }
        }.f};
    }

    pub fn seq(comptime str: []const u8) Void(Error) {
        return Void(Error) { .parse = struct { const R = Result(void, Error);
            fn f(input: []const u8, _: std.mem.Allocator) R {
                if (input.len == 0) {
                    return R { .value = Error.EOF };
                }
                if (std.mem.startsWith(u8, input, str)) {
                    return R { .offset = str.len, .value = {} };
                } else {
                    return R { .value = Error.ExpectForLiteral };
                }
            }
        }.f };
    }

    /// `2 <= sys and sys <= 16`
    pub fn asciiDigit(comptime sys: u8) Void(Error) {
        if (comptime !(2 <= sys and sys <= 16)) unreachable;
        return Void(Error) { .parse = struct { const R = Result(void, Error);
            fn f(input: []const u8, _: std.mem.Allocator) R {
                if (input.len == 0) return R { .value = Error.EOF };
                const v: u8 = switch (sys) {
                    2...10 => if ('0' <= input[0] and input[0] <= (comptime '0' + sys - 1)) input[0] else 0,
                    else => switch (input[0]) {
                        '0'...'9', 'a'...('a' + sys - 11), 'A'...('A' + sys - 11) => input[0],
                        else => 0,
                    }
                };

                if (v > 0) {
                    return R { .offset = 1, .value = {} };
                } else {
                    return R { .value = Error.ExpectForLiteral };
                }
            }
        }.f };
    }
};

pub const nop = Void(Error) { .parse = struct { const R = Result(void, Error);
    fn f(_: []const u8, _: std.mem.Allocator) R {
        return R { .value = {} };
    }
}.f };

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

    const p3: Parser(u8, NewError) = comptime p2
        .mapErr(NewError, struct { fn f(_: Error) NewError {
            return NewError.IsOne;
        }}.f)
    ;

    const p4 = comptime p2.castErr(NewError.IsOne);

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

    const rst7 = p4.parse("1", std.testing.allocator);
    try std.testing.expectError(NewError.IsOne, rst7.value);
}