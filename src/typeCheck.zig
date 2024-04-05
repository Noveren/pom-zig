
const std = @import("std");

const BuiltinTypeClass = enum {
    ErrorSet,
    Optional,
};

pub fn isTypeClass(comptime Mode: BuiltinTypeClass, comptime T: type) bool {
    const info: std.builtin.Type = @typeInfo(T);
    return switch (Mode) {
        .ErrorSet => switch (info) { else => false, .ErrorSet => true },
        .Optional => switch (info) { else => false, .Optional => true },
    };
}

pub fn assertTypeClass(comptime Mode: BuiltinTypeClass, comptime T: type) void {
    if (!isTypeClass(Mode, T)) {
        const err_msg = "'" ++ @typeName(T) ++ "' is not in TypeClass '" ++ @tagName(Mode) ++ "'";
        @compileError(err_msg);
    }
}

pub fn isSubErrorSet(comptime SUB: type, comptime SUP: type) bool {
    if (!isTypeClass(.ErrorSet, SUB)) unreachable;
    if (!isTypeClass(.ErrorSet, SUP)) unreachable;
    // assertTypeClass(.ErrorSet, SUB); // Comptime Bug
    // assertTypeClass(.ErrorSet, SUP);
    const COM = SUB || SUP;
    return COM == SUP;
}

pub fn assertSubErrorSet(comptime SUB: type, comptime SUP: type) void {
    if (!isSubErrorSet(SUB, SUP)) {
        const err_msg = "'" ++ @typeName(SUB) ++ "' is not sub ErrorSet of '" ++ @typeName(SUP) ++ "'";
        @compileError(err_msg);
    }
}

test "assert" {
    try std.testing.expect(isTypeClass(.ErrorSet, error{E}));
    try std.testing.expect(!isTypeClass(.ErrorSet, u8));

    try std.testing.expect(isSubErrorSet(error {}, error {A}));
    try std.testing.expect(isSubErrorSet(error {B}, error {A, B}));
    try std.testing.expect(!isSubErrorSet(error {A}, error {}));
    try std.testing.expect(!isSubErrorSet(error {B}, error {A}));

    comptime assertSubErrorSet(error{}, error{A});
    // comptime assertSubErrorSet(error{B}, error{A});
}