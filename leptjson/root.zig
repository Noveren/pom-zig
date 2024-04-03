
const std = @import("std");

const parser = @import("parser.zig");

test {
    std.testing.refAllDecls(parser);
    _ = parser;
}

pub const Type = enum {
    Null, Boolean,
};

pub const Value = union(Type) {
    Null,
    Boolean: bool,
    const Self = @This();

    pub fn getType(self: Self) Type {
        return switch (self) {
            .Null => Type.Null,
            .Boolean => Type.Boolean,
        };
    }
};

pub const Error = error {
    ExpectValue,
    InvalidValue,
    RootNotSingular,
    NumberTooBig,
    MissQuotationMask,
    InvalidStringEscape,
    InvalidStringChar,
    InvalidUnicodeHex,
    InvalidUnicodeSurrogate,
    MissCommaOrSquareBracket,
    MissKey,
    MissColon,
    MissCommaOrCurlyBracket,
};