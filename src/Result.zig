
const std = @import("std");
const Allocator = std.mem.Allocator;

const typeCheck = @import("typeCheck.zig");

/// `T: anytype, E: ErrorSet`;
pub fn Result(comptime T: type, comptime E: type) type {
    comptime typeCheck.assertErrorSet(E);
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