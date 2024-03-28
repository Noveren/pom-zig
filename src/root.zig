
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
                for (l._list.items) |i| {
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

        pub fn drop(self: *@This()) void {
            const info_Ok: std.builtin.Type = @typeInfo(Ok);
            switch (info_Ok) {
                else => {},
                .Struct, .Union, .Enum => {
                    if (comptime std.meta.hasMethod(Ok, "deinit")) {
                        if (self.rst) |ok| { ok.deinit(); } else |_| {}
                    }
                }
            }
        }
    };
}

// TODO 支持 utf-8
pub const terminal = struct {
    pub const anychar = Parser(void) { .parse = struct {
        const R = Result(void);
        fn f(input: []const u8, _: std.mem.Allocator) R {
            if (input.len > 0) {
                return R { .mov = 1, .rst = {} };
            } else {
                return R { .rst = Err.EOF };
            }
        }
    }.f };

    pub fn tU8(comptime ch: u8) PVoid {
        return PVoid { .parse = struct {
            const R = Result(void);
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

    pub fn tU8Choice(comptime choice: []const u8) PVoid {
        return PVoid { .parse = struct {
            const R = Result(void);
            fn f(input: []const u8, _: std.mem.Allocator) R {
                if (input.len == 0) {
                    return R { .rst = Err.EOF };
                }
                for (choice) |ch| {
                    if (input[0] == ch) {
                        return R { .mov = 1, .rst = {} };
                    }
                } else {
                    return R { .rst = Err.ExpectForLiteral };
                }
            }
        }.f};
    }

    pub fn tU8s(comptime str: []const u8) Parser(void) {
        return Parser(void) { .parse = struct {
            const R = Result(void);
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

    pub const DigitSystem = enum {
        Bin, Dec, Oct, Hex,
    };

    pub fn digit(comptime ds: DigitSystem) Parser(void) {
        return Parser(void) { .parse = struct {
            const R = Result(void);
            fn f(input: []const u8, _: std.mem.Allocator) R {
                if (input.len == 0) {
                    return R { .rst = Err.EOF };
                }

                const v: u8 = switch (ds) {
                    .Bin => if ('0' <= input[0] and input[0] <= '1') input[0] else 0,
                    .Oct => if ('0' <= input[0] and input[0] <= '7') input[0] else 0,
                    .Dec => if ('0' <= input[0] and input[0] <= '9') input[0] else 0,
                    .Hex => switch (input[0]) {
                        '0'...'9', 'a'...'f', 'A'...'F' => input[0],
                        else => 0,
                    },
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

pub const PVoid = Parser(void);
pub fn Parser(comptime O: type) type {
    return struct {
        parse: *const fn([]const u8, std.mem.Allocator) Result(O),
        const Self = @This();

        pub fn slice(comptime self: Self) Parser([]const u8) {
            return Parser([]const u8) { .parse = struct {
                const R = Result([]const u8);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    var r = self.parse(input, allocator);
                    if (r.rst) |_| {
                        r.drop();
                        return R { .mov = r.mov, .rst = input[0..r.mov] };
                    } else |err| {
                        return R { .mov = r.mov, .rst = err };
                    }
                }
            }.f };
        }

        pub fn map(comptime self: Self, comptime M: type, comptime map_fn: fn(O, std.mem.Allocator) ?M) Parser(M) {
            return Parser(M) { .parse = struct {
                const R = Result(M);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    var r = self.parse(input, allocator);
                    if (r.rst) |ok| {
                        defer r.drop();
                        return R { .mov = r.mov, .rst = map_fn(ok, allocator) orelse Err.FailedToMap };
                    } else |err| {
                        return R { .mov = r.mov, .rst = err };
                    }
                }
            }.f };
        }

        pub fn mapVoid(comptime self: Self) Parser(void) {
            return Parser(void) { .parse = struct {
                const R = Result(void);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    var r = self.parse(input, allocator);
                    if (r.rst) |_| {
                        r.drop();
                        return R { .mov = r.mov, .rst = {} };
                    } else |err| {
                        return R { .mov = r.mov, .rst = err };
                    }
                }
            }.f };
        }

        pub fn prefix(comptime self: Self, comptime pfx: Parser(void)) Parser(O) {
            return Parser(O) { .parse = struct {
                const R = Result(O);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    var r_pfx = pfx.parse(input, allocator);
                    if (r_pfx.rst) |_| {
                        r_pfx.drop();
                        const r = self.parse(input[r_pfx.mov..], allocator);
                        return R { .mov = r_pfx.mov + r.mov, .rst = r.rst };
                    } else |err| {
                        return R { .mov = r_pfx.mov, .rst = err };
                    }
                }
            }.f };
        }

        pub fn suffix(comptime self: Self, comptime sfx: Parser(void)) Parser(O) {
            return Parser(O) { .parse = struct {
                const R = Result(O);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    var r = self.parse(input, allocator);
                    if (r.rst) |ok| {
                        const r_sfx = sfx.parse(input[r.mov..], allocator);
                        if (r_sfx.rst) |_| {
                            return R { .mov = r.mov + r_sfx.mov, .rst = ok };
                        } else |err| {
                            r.drop();
                            return R { .mov = r.mov + r_sfx.mov, .rst = err };
                        }
                    } else |err| {
                        return R { .mov = r.mov, .rst = err };
                    }
                }
            }.f };
        }

        // ========================================================

        pub fn oneMore(comptime self: Self) Parser(List(O)) {
            return self.times(0);
        }

        pub fn times(comptime self: Self, comptime N: usize) Parser(List(O)) {
            return Parser(List(O)) { .parse = struct {
                const R = Result(List(O));
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    if (N == 0) {
                        // OneMore
                        var list = List(O).init(allocator);

                        var ok_counter: usize = 0;
                        var offset: usize = 0;
                        var r: Result(O) = self.parse(input, allocator);
                        while (r.rst) |ok| : (r = self.parse(input[offset..], allocator)) {
                            if (comptime O != void) list.append(ok) catch {
                                r.drop();
                                list.deinit();
                                return R { .mov = offset, .rst = Err.FailedToParse };
                            };
                            ok_counter += 1;
                            offset += r.mov;
                        } else |_| {}

                        if (ok_counter == 0) {
                            list.deinit();
                            return R { .mov = offset, .rst = Err.FailedToParse };
                        }
                        return R { .mov = offset, .rst = list };
                    } else {
                        // N Times
                        var list = List(O).init(allocator);

                        var offset: usize = 0;
                        var r: Result(O) = undefined;
                        for (0..N) |_| {
                            r = self.parse(input[offset..], allocator);
                            if (r.rst) |ok| {
                                if (comptime O != void) list.append(ok) catch {
                                    r.drop();
                                    list.deinit();
                                    return R { .mov = offset, .rst = Err.FailedToParse };
                                };
                                offset += r.mov;
                            } else |err| {
                                list.deinit();
                                return R { .mov = offset, .rst = err };
                            }
                        }
                        return R { .mov = offset, .rst = list };
                    }
                }
            }.f };
        }

        pub fn pred(comptime self: Self, comptime COND: bool) Parser(void) {
            return Parser(void) { .parse = struct {
                const R = Result(void);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    var r = self.parse(input, allocator);
                    r.drop();
                    if (r.rst) |_| {
                        if (COND) {
                            return R { .rst = {} };
                        } else {
                            return R { .rst = Err.FailedToPred };
                        }
                    } else |err| {
                        if (COND) {
                            return R { .rst = err };
                        } else {
                            return R { .rst = {} };
                        }
                    }
                }
            }.f };
        }

        pub fn optional(comptime self: Self) Parser(void) {
            return Parser(void) { .parse = struct {
                const R = Result(void);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    var r = self.parse(input, allocator);
                    defer r.drop();
                    if (r.rst) |_| {
                        return R { .mov = r.mov, .rst = {} };
                    } else |_| {
                        return R { .rst = {} };
                    }
                }
            }.f };
        }
    };
}

pub const nop = Parser(void) { .parse = struct {
    const R = Result(void);
    fn f(_: []const u8, _: std.mem.Allocator) R {
        return R { .rst = {} };
    }
}.f };

test "ascii tU8s and slice" {
    const p1 = comptime terminal.tU8s("null");
    var r1 = p1.parse("null", std.testing.allocator);
    defer r1.drop();
    try expectEqual(true, r1.isOk());
    try expectEqual(4, r1.mov);

    const p2 = comptime terminal.tU8s("null").slice();
    var r2 = p2.parse("null", std.testing.allocator);
    defer r2.drop();
    try expectEqual(true, r2.isOk());
    try expectEqual(4, r2.mov);
    try expectEqual("null", try r2.rst);
}

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
// TODO SequenceVoid
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
            return Parser(List(O)) { .parse = struct {
                const R = Result(List(O));
                fn f(input: []const u8, allocator: std.mem.Allocator) R {        
                    var list = List(O).init(allocator);
                
                    var offset: usize = 0;
                    var r: Result(O) = undefined;
                    for (self._parsers) |parser| {
                        r = parser.parse(input[offset..], allocator);
                        if (r.rst) |ok| {
                            if (comptime O != void) list.append(ok) catch {
                                r.drop();
                                list.deinit();
                                return R { .mov = offset, .rst = Err.FailedToParse };
                            };
                            offset += r.mov;
                        } else |err| {
                            list.deinit();
                            return R { .mov = offset, .rst = err };
                        }
                    }
                    return R { .mov = offset, .rst = list };
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
            return Parser(O) { .parse = struct {
                const R = Result(O);
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

