
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
        memory: std.ArrayList(T),
        const Self = @This();

        pub fn init(allocator: std.mem.Allocator) Self {
            return Self { .memory = std.ArrayList(T).init(allocator) };
        }

        pub fn deinit(self: Self) void {
            defer self.memory.deinit();
            if (comptime std.meta.hasMethod(T, "deinit")) {
                for (self.memory.items) |*i| {
                    i.deinit();
                }
            }
        }

        pub fn append(self: *Self, item: T) std.mem.Allocator.Error!void {
            try self.memory.append(item);
        }

        pub fn items(self: Self) []const T {
            return self.memory.items;
        }

        pub inline fn len(self: Self) usize {
            return self.memory.items.len;
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

        // 需要的：用于在组合器失败时，将解析器解析成功的结果进行释放
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
pub fn Parser(comptime T: type) type {
    return struct {
        parse: *const fn([]const u8, std.mem.Allocator) Result(T),
        const Self = @This();

        pub fn discard(comptime self: Self) Void {
            return Void { .parse = struct { const R = Result(void);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    var r = self.parse(input, allocator);
                    return R {
                        .mov = r.mov,
                        .rst = if (r.rst) |_| r.discard() else |err| err,
                     };
                }
            }.f };
        }

        pub fn slice(comptime self: Self) Parser([]const u8) {
            return Parser([]const u8) { .parse = struct { const R = Result([]const u8);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    var r = self.parse(input, allocator);
                    return R {
                        .mov = r.mov,
                        .rst = if (r.rst) |_| blk: {
                            r.discard();
                            break :blk input[0..r.mov];
                        } else |err| err,
                    };
                }
            }.f };
        }

        pub fn map(comptime self: Self, comptime M: type, comptime map_fn: fn(T, std.mem.Allocator) ?M) Parser(M) {
            return Parser(M) { .parse = struct { const R = Result(M);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    const r = self.parse(input, allocator);
                    return R {
                        .mov = r.mov,
                        .rst = if (r.rst) |ok| map_fn(ok, allocator) orelse Err.FailedToMap else |err| err,
                    };
                }
            }.f };
        }

        pub fn prefix(comptime self: Self, comptime pfx: Void) Parser(T) {
            return Parser(T) { .parse = struct { const R = Result(T);
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

        pub fn suffix(comptime self: Self, comptime sfx: Parser(void)) Parser(T) {
            return Parser(T) { .parse = struct { const R = Result(T);
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    var r = self.parse(input, allocator);
                    if (r.rst) |ok| {
                        const r_sfx = sfx.parse(input[r.mov..], allocator);
                        return R {
                            .mov = r.mov + r_sfx.mov,
                            .rst = if (r_sfx.rst) |_| ok else |err| blk: {
                                r.discard();
                                break: blk err;
                            }
                        };
                    } else |err| {
                        return R { .mov = r.mov, .rst = err };
                    }
                }
            }.f };
        }
        // ========================================================
        /// mode: 0 - zeroMore, 1 - oneMore, 2 - times(N)
        fn many(comptime self: Self, comptime sep: ?Void, comptime mode: u8, comptime N: usize) Parser(List(T)) {
            return Parser(List(T)) { .parse = struct { const R = Result(List(T));
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    var list = List(T).init(allocator);
                    var mov: usize = 0;
                    var hasSepBefore: bool = false;
                    var r: Result(T) = self.parse(input, allocator);
                    
                    if (comptime mode < 2) {
                        if (r.isOk() and r.mov == 0) { @panic("TODO Infty Loop\n"); }
                    }

                    while (r.rst) |ok| : (r = self.parse(input[mov..], allocator)) {
                        mov += r.mov;
                        list.append(ok) catch {
                            r.discard();
                            list.deinit();
                            return R { .mov = mov, .rst = Err.OutOfMemory };
                        };
                        // TODO 是否要限制最大次数
                        if (comptime mode > 1) { if (list.len() == N) break; }
                        
                        if (comptime sep) |p_sep| {
                            const r_sep = p_sep.parse(input[mov..], allocator);
                            if (r_sep.rst) |_| {
                                mov += r_sep.mov;
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
                            list.deinit();
                            return R { .mov = mov, .rst = err };
                        }
                    }
                    return R { .mov = mov, .rst = list };
                }
            }.f };
        }

        pub fn zeroMore(comptime self: Self, comptime sep: ?Void) Parser(List(T)) {
            return Parser(List(T)) { .parse = struct { const R = Result(List(T));
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    return self.many(sep, 0, 0).parse(input, allocator);
                }
            }.f };
        }

        pub fn oneMore(comptime self: Self, comptime sep: ?Void) Parser(List(T)) {
            return Parser(List(T)) { .parse = struct { const R = Result(List(T));
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    return self.many(sep, 1, 0).parse(input, allocator);
                }
            }.f };
        }

        pub fn times(comptime self: Self, comptime N: usize, comptime sep: ?Void) Parser(List(T)) {
            if (comptime N == 0) @compileError("Times N == 0");
            return Parser(List(T)) { .parse = struct { const R = Result(List(T));
                fn f(input: []const u8, allocator: std.mem.Allocator) R {
                    return self.many(sep, 2, N).parse(input, allocator);
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

test "oneMore" {
    const p1 = comptime U8.one('1').oneMore(U8.one(','));

    try std.testing.expect(p1.parse("1", std.testing.allocator).isOk());
    try std.testing.expect(!p1.parse("", std.testing.allocator).isOk());
    try std.testing.expect(p1.parse("1,1,1,1,1,1", std.testing.allocator).isOk());
    try std.testing.expect(!p1.parse("1,1,1,1,2", std.testing.allocator).isOk());

    const p2 = comptime U8.one('2').oneMore(null);
    try std.testing.expect(p2.parse("2", std.testing.allocator).isOk());
    try std.testing.expect(p2.parse("22222222222", std.testing.allocator).isOk());
    try std.testing.expect(!p2.parse("", std.testing.allocator).isOk());
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
// TODO 逻辑好像有问题
fn SequenceN(comptime T: type, comptime N: usize) type {
    return struct {
        _parsers: [N]Parser(T),
        const Self = @This();

        pub fn with(comptime self: Self, comptime p: Parser(T)) SequenceN(T, N + 1) {
            return SequenceN(T, N + 1) {
                ._parsers = self._parsers ++ [1]Parser(T) { p }
            };
        }

        pub fn build(comptime self: Self) Parser(List(T)) {
            return Parser(List(T)) { .parse = struct { const R = Result(List(T));
                fn f(input: []const u8, allocator: std.mem.Allocator) R {        
                    var list = List(T).init(allocator);
                    var mov: usize = 0;
                    var r: Result(T) = undefined;
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

pub fn Choice(comptime T: type) type {
    return struct {
        pub fn with(comptime p: Parser(T)) ChoiceN(T, 1) {
            return ChoiceN(T, 1) {
                ._parsers = [1]Parser(T) { p }
            };
        }
    };
}

fn ChoiceN(comptime T: type, comptime N: usize) type {
    return struct {
        _parsers: [N]Parser(T),
        const Self = @This();

        pub fn with(comptime self: Self, comptime p: Parser(T)) ChoiceN(T, N + 1) {
            return ChoiceN(T, N + 1) {
                ._parsers = self._parsers ++ [1]Parser(T) { p }
            };
        }

        pub fn build(comptime self: Self) Parser(T) {
            return Parser(T) { .parse = struct { const R = Result(T);
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

