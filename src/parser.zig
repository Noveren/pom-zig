
const std = @import("std");
const Allocator = std.mem.Allocator;

const Result = @import("Result.zig");
const typeCheck = @import("typeCheck.zig");

pub const ErrorFailedToMap = error { FailedToMap };
pub const Error = 
    ErrorFailedToMap
;

/// `T: anytype, E: ErrorSet`;
pub fn Parser(comptime T: type, comptime E: type) type {
    comptime typeCheck.assertType(.ErrorSet, E);
return struct {

parse: *const fn([]const u8, std.mem.Allocator) Result(T, E),

const Self = @This();

pub const Val: type = T;
pub const Err: type = E;

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

/// Map the `result.value: T` of `self` by `map_fn` if `self` parsed `intput` successfully. 
pub fn mapFn(comptime self: Self, comptime M: type, comptime map_fn: fn(*T, std.mem.Allocator) ?M) Parser(M, E||ErrorFailedToMap) {
    return Parser(M, E||ErrorFailedToMap) { .parse = struct { const R = Result(M, E||ErrorFailedToMap);
        fn f(input: []const u8, allocator: std.mem.Allocator) R {
            var rst = self.parse(input, allocator);
            return R {
                .offset = rst.offset,
                .value  = if (rst.value) |*ok| map_fn(ok, allocator) orelse @errorCast(Error.FailedToMap) else |err| err,
            };
        }
    }.f };
}

/// Map the `result.value: E` of `self` by `map_err_fn` if `self` failed to parse `input`. 
/// You can check the `result.value: E` and then return a new error.
pub fn mapErrFn(comptime self: Self, comptime NewE: type, comptime map_err_fn: fn(E) NewE) Parser(T, NewE) {
    comptime typeCheck.assertType(.ErrorSet, NewE);
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
    comptime typeCheck.assertType(.ErrorSet, NewE);
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

fn manyCollect(comptime self: Self, comptime _sep: ?Void(Error), comptime mode: u8, comptime N: usize) Parser(List(T), Allocator.Error||E) {
    comptime {
        const T_sep = @TypeOf(sep);
        typeCheck.assertType(.Optional, T_sep);
    }
    const sep: ?Parser(void, T_sep.Err) = _sep;
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


};}