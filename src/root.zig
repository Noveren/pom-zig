
const std = @import("std");
const Allocator = std.mem.Allocator;

fn isErrorSet(comptime T: type) bool {
    const info: std.builtin.Type = @typeInfo(T);
    return switch (info) { else => false, .ErrorSet => true };
}

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

        // pub fn Val(comptime _: Self) type { return T; }
        // pub fn Err(comptime _: Self) type { return E; }

        pub fn discard(comptime self: Self) Void {
            return Void { .parse = struct { const R = Result(void, E);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    var rst = self.parse(input, allocator);
                    return R {
                        .offset = rst.offset,
                        .value = if (rst.value) |_| rst.discard() else |err| err,
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
            comptime if (!isErrorSet(E)) @compileError("Expected error set type, found " ++ @typeName(NewE));
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
            comptime if (!isErrorSet(NewE)) @compileError("Expected error set type, found " ++ @typeName(NewE));
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
        fn many(comptime self: Self, comptime sep: ?Void, comptime mode: u8, comptime N: usize) Parser(List(T), Error||E) {
            return Parser(List(T), Error||E) { .parse = struct { const R = Result(List(T), Error||E);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    var list = List(T).init(allocator);
                    var offset: usize = 0;
                    var hasSepBefore: bool = false;
                    var rst: Result(T, E) = self.parse(input, allocator);

                    if (comptime mode < 2) {
                        if (rst.isOk() and rst.offset == 0) { @panic("TODO Infty Loop\n"); }
                    }

                    while (rst.value) |ok| : (rst = self.parse(input[offset..], allocator)) {
                        list.append(ok) catch {
                            rst.discard();
                            list.deinit();
                            return R { .offset = offset, .value = Error.OutOfMemory };
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

        pub fn zeroMore(comptime self: Self, comptime sep: ?Void) Parser(List(T), Error||E) {
            return Parser(List(T), Error||E) { .parse = struct { const R = Result(List(T), Error||E);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    return self.many(sep, 0, 0).parse(input, allocator);
                }
            }.f };
        }

        pub fn oneMore(comptime self: Self, comptime sep: ?Void) Parser(List(T), Error||E) {
            return Parser(List(T), Error||E) { .parse = struct { const R = Result(List(T), Error||E);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    return self.many(sep, 1, 0).parse(input, allocator);
                }
            }.f };
        }

        pub fn times(comptime self: Self, comptime N: usize, comptime sep: ?Void) Parser(List(T), E) {
            if (comptime N == 0) @compileError("Times N == 0");
            return Parser(List(T), E) { .parse = struct { const R = Result(List(T), Error||E);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    return self.many(sep, 2, N).parse(input, allocator);
                }
            }.f };
        }        
    };
}

pub fn Choice(comptime T: type, comptime E: type) type {
    comptime if (!isErrorSet(E)) @compileError("Expected error set type, found " ++ @typeName(E));
    return struct {
        pub fn with(comptime p: Parser(T, E)) ChoiceN(T, E, 1) {
            return ChoiceN(T, E, 1) {
                ._parsers = [1]Parser(T, E) { p }
            };
        }
    };
}

fn ChoiceN(comptime T: type, comptime E: type, comptime N: usize) type {
    comptime if (!isErrorSet(E)) @compileError("Expected error set type, found " ++ @typeName(E));
    return struct {
        _parsers: [N]Parser(T, E),
        const Self = @This();

        pub fn with(comptime self: Self, comptime p: Parser(T, E)) ChoiceN(T, E, N + 1) {
            return ChoiceN(T, E, N + 1) {
                ._parsers = self._parsers ++ [1]Parser(T, E) { p }
            };
        }

        pub fn build(comptime self: Self) Parser(T, Error||E) {
            return Parser(T, Error||E) { .parse = struct { const R = Result(T, Error||E);
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

    pub fn set(comptime u8s: []const u8) Void {
        return Void { .parse = struct { const R = Result(void, Error);
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

    pub fn seq(comptime str: []const u8) Void {
        return Void { .parse = struct { const R = Result(void, Error);
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