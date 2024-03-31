
const std = @import("std");

fn expectEqual(expect: anytype, actual: anytype) !void {
    if (@TypeOf(actual) == []const u8) {
        return std.testing.expectEqualStrings(expect, actual);
    } else {
        return std.testing.expectEqual(expect, actual);
    }
}

pub fn List(comptime T: type) type {
    return struct {
        // std.ArrayList 能够自动处理 void, 添加 void 不申请内存
        _list: std.ArrayList(T),

        pub fn init(allocator: std.mem.Allocator) List(T) {
            return List(T) { ._list = std.ArrayList(T).init(allocator) };
        }

        pub fn deinit(l: List(T)) void {
            defer l._list.deinit();
            if (comptime std.meta.hasMethod(T, "deinit")) {
                for (l._list.items) |*i| {
                    i.deinit();
                }
            }
        }

        pub fn append(l: *List(T), item: T) std.mem.Allocator.Error!void {
            try l._list.append(item);
        }

        pub fn getItems(l: List(T)) []const T {
            return l._list.items;
        }
    };
}

pub const Err = error {
    EOF,
    ExpectForLiteral,
    FailedToMap,
    FailedToChoice,
    FailedToPred,
    FailedToParse,
} || std.mem.Allocator.Error;

pub fn Result(comptime Ok: type) type {
    return struct {
        mov: usize = 0,
        rst: Err!Ok,

        pub inline fn isOk(self: @This()) bool {
            return if (self.rst) |_| true else |_| false;
        }

        pub fn discard(self: *@This()) void {
            const info_Ok: std.builtin.Type = @typeInfo(Ok);
            switch (info_Ok) {
                else => {},
                .Struct, .Union, .Enum => {
                    if (comptime std.meta.hasMethod(Ok, "deinit")) {
                        if (self.rst) |*ok| { ok.deinit(); } else |_| {}
                    }
                }
            }
        }
    };
}

pub const Void = Parser(void);
pub fn Parser(comptime O: type) type {
    return struct {
        parse: *const fn([]const u8, std.mem.Allocator) Result(O),
        const Self = @This();

        /// Use `self` to parse `input: []const u8`, 
        /// then get slice `input[0..r.mov]` and `discard` the result if it parses successfully.
        /// ```zig
        /// // TODO
        /// ```
        pub fn slice(comptime self: Self) Parser([]const u8) {
            return Parser([]const u8) { .parse = struct { const R = Result([]const u8);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    var r = self.parse(input, allocator);
                    if (r.rst) |_| {
                        r.discard();
                        return R { .mov = r.mov, .rst = input[0..r.mov] };
                    } else |err| {
                        return R { .mov = r.mov, .rst = err };
                    }
                }
            }.f };
        }

        /// Use `map_fn` to map the result of `self` if it parses successfully.
        /// You have to manage the ownership of the result of `self` like `deinit`
        pub fn map(comptime self: Self, comptime M: type, comptime map_fn: fn(O, std.mem.Allocator) ?M) Parser(M) {
            return Parser(M) { .parse = struct { const R = Result(M);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    const r = self.parse(input, allocator);
                    if (r.rst) |ok| {
                        return R { .mov = r.mov, .rst = map_fn(ok, allocator) orelse Err.FailedToMap };
                    } else |err| {
                        return R { .mov = r.mov, .rst = err };
                    }
                }
            }.f };
        }

        /// Dsicard the result of `self` if it parses successfully.
        /// This method is used to map `Parser(O)` to `Parser(void)`.
        pub fn discard(comptime self: Self) Void {
            return Void { .parse = struct { const R = Result(void);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    var r = self.parse(input, allocator);
                    if (r.rst) |_| {
                        r.discard();
                        return R { .mov = r.mov, .rst = {} };
                    } else |err| {
                        return R { .mov = r.mov, .rst = err };
                    }
                }
            }.f };
        }

        /// use `pfx: Parser(void)` to check and consume the prefix
        pub fn prefix(comptime self: Self, comptime pfx: Void) Parser(O) {
            return Parser(O) { .parse = struct { const R = Result(O);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    const r_pfx = pfx.parse(input, allocator);
                    if (r_pfx.rst) |_| {
                        const r = self.parse(input[r_pfx.mov..], allocator);
                        return R { .mov = r_pfx.mov + r.mov, .rst = r.rst };
                    } else |err| {
                        return R { .mov = r_pfx.mov, .rst = err };
                    }
                }
            }.f };
        }

        /// use `sfx: Parser(void)` to check and consume the rest of input parsed by `self`
        pub fn suffix(comptime self: Self, comptime sfx: Parser(void)) Parser(O) {
            return Parser(O) { .parse = struct { const R = Result(O);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    var r = self.parse(input, allocator);
                    if (r.rst) |ok| {
                        const r_sfx = sfx.parse(input[r.mov..], allocator);
                        if (r_sfx.rst) |_| {
                            return R { .mov = r.mov + r_sfx.mov, .rst = ok };
                        } else |err| {
                            r.discard();
                            return R { .mov = r.mov + r_sfx.mov, .rst = err };
                        }
                    } else |err| {
                        return R { .mov = r.mov, .rst = err };
                    }
                }
            }.f };
        }
        // ========================================================
        pub fn zeroMore(comptime self: Self) Parser(List(O)) {
            return Parser(List(O)) { .parse = struct { const R = Result(List(O));
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    var list = List(O).init(allocator);
                    var mov: usize = 0;
                    var r: Result(O) = self.parse(input, allocator);
                    while (r.rst) |ok| : (r = self.parse(input[mov..], allocator)) {
                        list.append(ok) catch {
                            r.discard();
                            list.deinit();
                            return R { .mov = mov, .rst = Err.FailedToParse };
                        };
                        mov += r.mov;
                    } else |_| {}
                    return R { .mov = mov, .rst = list };
                }
            }.f };
        }

        pub fn oneMore(comptime self: Self) Parser(List(O)) {
            return Parser(List(O)) { .parse = struct { const R = Result(List(O));
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    var list = List(O).init(allocator);
                    var mov: usize = 0;
                    var r: Result(O) = self.parse(input, allocator);
                    while (r.rst) |ok| : (r = self.parse(input[mov..], allocator)) {
                        list.append(ok) catch {
                            r.discard();
                            list.deinit();
                            return R { .mov = mov, .rst = Err.FailedToParse };
                        };
                        mov += r.mov;
                    } else |_| {}

                    if (list.getItems().len == 0) {
                        list.deinit();
                        return R { .mov = mov, .rst = Err.FailedToParse };
                    }
                    return R { .mov = mov, .rst = list };
                }
            }.f };
        }

        pub fn times(comptime self: Self, comptime N: usize) Parser(List(O)) {
            if (comptime N == 0) unreachable;
            return Parser(List(O)) { .parse = struct { const R = Result(List(O));
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    var list = List(O).init(allocator);
                    var mov: usize = 0;
                    var r: Result(O) = undefined;
                    for (0..N) |_| {
                        r = self.parse(input[mov..], allocator);
                        if (r.rst) |ok| {
                            list.append(ok) catch {
                                r.discard();
                                list.deinit();
                                return R { .mov = mov, .rst = Err.FailedToParse };
                            };
                            mov += r.mov;
                        } else |err| {
                            list.deinit();
                            return R { .mov = mov, .rst = err };
                        }
                    }
                    return R { .mov = mov, .rst = list };
                }
            }.f };
        }

        pub fn pred(comptime self: Self, comptime COND: bool) Void {
            return Void { .parse = struct { const R = Result(void);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    var r = self.parse(input, allocator);
                    r.discard();
                    if (r.rst) |_| {
                        return R { .rst = if (comptime COND) { } else Err.FailedToPred };
                    } else |err| {
                        return R { .rst = if (comptime COND) err else { } };
                    }
                }
            }.f };
        }

        pub fn optional(comptime self: Self) Void {
            return Void { .parse = struct { const R = Result(void);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    var r = self.parse(input, allocator);
                    r.discard();
                    return R { .mov = if (r.isOk()) r.mov else 0, .rst = {} };
                }
            }.f };
        }
    };
}

test "ascii tU8s and slice" {
    const p1 = comptime U8.seq("null");
    var r1 = p1.parse("null", std.testing.allocator);
    defer r1.discard();
    try expectEqual(true, r1.isOk());
    try expectEqual(4, r1.mov);

    const p2 = comptime U8.seq("null").slice();
    var r2 = p2.parse("null", std.testing.allocator);
    defer r2.discard();
    try expectEqual(true, r2.isOk());
    try expectEqual(4, r2.mov);
    try expectEqual("null", try r2.rst);
}

pub const nop = Void { .parse = struct { const R = Result(void);
    fn f(_: []const u8, _: std.mem.Allocator) R {
        return R { .rst = {} };
    }
}.f };

pub fn ref(comptime T: type, comptime func: fn() Parser(T)) Parser(T) {
    return Parser(T) { .parse = struct {
        fn f(input: []const u8, allocator: std.mem.Allocator) Result(T) {
            return func().parse(input, allocator);
        }
    }.f };
}

pub fn Sequence(comptime T: type) type {
    return struct {
        pub fn with(comptime p: Parser(T)) SequenceN(T, 1) {
            return SequenceN(T, 1) {
                ._parsers = [1]Parser(T) { p }
            };
        }
    };
}

fn SequenceN(comptime O: type, comptime N: usize) type {
    return struct {
        _parsers: [N]Parser(O),
        const Self = @This();

        pub fn with(comptime self: Self, comptime p: Parser(O)) SequenceN(O, N + 1) {
            return SequenceN(O, N + 1) {
                ._parsers = self._parsers ++ [1]Parser(O) { p }
            };
        }

        pub fn build(comptime self: Self) Parser(List(O)) {
            return Parser(List(O)) { .parse = struct { const R = Result(List(O));
                fn f(input: []const u8, allocator: std.mem.Allocator) R {        
                    var list = List(O).init(allocator);
                    var mov: usize = 0;
                    var r: Result(O) = undefined;
                    for (self._parsers) |parser| {
                        r = parser.parse(input[mov..], allocator);
                        if (r.rst) |ok| {
                            list.append(ok) catch {
                                r.discard();
                                list.deinit();
                                return R { .mov = mov, .rst = Err.FailedToParse };
                            };
                            mov += r.mov;
                        } else |err| {
                            list.deinit();
                            return R { .mov = mov, .rst = err };
                        }
                    }
                    return R { .mov = mov, .rst = list };
                }
            }.f };
        }
    };
}

pub fn Choice(comptime O: type) type {
    return struct {
        pub fn with(comptime p: Parser(O)) ChoiceN(O, 1) {
            return ChoiceN(O, 1) {
                ._parsers = [1]Parser(O) { p }
            };
        }
    };
}

fn ChoiceN(comptime O: type, comptime N: usize) type {
    return struct {
        _parsers: [N]Parser(O),
        const Self = @This();

        pub fn with(comptime self: Self, comptime p: Parser(O)) ChoiceN(O, N + 1) {
            return ChoiceN(O, N + 1) {
                ._parsers = self._parsers ++ [1]Parser(O) { p }
            };
        }

        pub fn build(comptime self: Self) Parser(O) {
            return Parser(O) { .parse = struct { const R = Result(O);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    for (self._parsers) |p| {
                        const r = p.parse(input, allocator);
                        if (r.isOk()) {
                            return r;
                        }
                    }
                    return R { .rst = Err.FailedToChoice };
                }
            }.f };
        }
    };
}

pub const U8 = struct {
    pub const any = Parser(void) { .parse = struct { const R = Result(void);
        fn f(input: []const u8, _: std.mem.Allocator) R {
            if (input.len > 0) {
                return R { .mov = 1, .rst = {} };
            } else {
                return R { .rst = Err.EOF };
            }
        }
    }.f };

    pub fn one(comptime ch: u8) Void {
        return Void { .parse = struct { const R = Result(void);
            fn f(input: []const u8, _: std.mem.Allocator) R {
                if (input.len == 0) {
                    return R { .rst = Err.EOF };
                }
                if (input[0] == ch) {
                    return R { .mov = 1, .rst = {} };
                } else {
                    return R { .rst = Err.ExpectForLiteral };
                }
            }
        }.f};
    }

    pub fn choice(comptime set: []const u8) Void {
        return Void { .parse = struct { const R = Result(void);
            fn f(input: []const u8, _: std.mem.Allocator) R {
                if (input.len == 0) {
                    return R { .rst = Err.EOF };
                }
                for (set) |ch| {
                    if (input[0] == ch) return R { .mov = 1, .rst = {} };
                }
                return R { .rst = Err.ExpectForLiteral };
            }
        }.f};
    }

    pub fn seq(comptime str: []const u8) Parser(void) {
        return Parser(void) { .parse = struct { const R = Result(void);
            fn f(input: []const u8, _: std.mem.Allocator) R {
                if (input.len == 0) {
                    return R { .rst = Err.EOF };
                }

                if (std.mem.startsWith(u8, input, str)) {
                    return R { .mov = str.len, .rst = {} };
                } else {
                    return R { .rst = Err.ExpectForLiteral };
                }
            }
        }.f };
    }

    /// `2 <= sys and sys <= 16`
    pub fn asciiDigit(comptime sys: u8) Void {
        if (comptime !(2 <= sys and sys <= 16)) unreachable;
        return Parser(void) { .parse = struct { const R = Result(void);
            fn f(input: []const u8, _: std.mem.Allocator) R {
                if (input.len == 0) return R { .rst = Err.EOF };
                const v: u8 = switch (sys) {
                    2...10 => if ('0' <= input[0] and input[0] <= (comptime '0' + sys - 1)) input[0] else 0,
                    else => switch (input[0]) {
                        '0'...'9', 'a'...('a' + sys - 11), 'A'...('A' + sys - 11) => input[0],
                        else => 0,
                    }
                };

                if (v > 0) {
                    return R { .mov = 1, .rst = {} };
                } else {
                    return R { .rst = Err.ExpectForLiteral };
                }
            }
        }.f };
    }
};

