
const std = @import("std");

const Type = enum {
    ErrorSet,
    Optional,
};

pub fn assertType(comptime Mode: Type, comptime T: type) void {
    const info: std.builtin.Type = @typeInfo(T);
    const failed: bool = switch (Mode) {
        .ErrorSet => switch (info) { else => true, .ErrorSet => false },
        .Optional => switch (info) { else => true, .Optional => false },
    };
    if (failed) {
        const err_msg = "'" ++ @typeName(T) ++ "' is not '" ++ @tagName(Mode) ++ "'";
        @compileError(err_msg);
    }
}

test "assert" {
    assertType(.ErrorSet, error{E});
    // assertType(.ErrorSet, u8);
}