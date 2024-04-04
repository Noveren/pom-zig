
const std = @import("std");

const parser = @import("parser.zig");

pub const Type = enum {
    Null, Boolean, Number
};

pub const Value = union(Type) {
    Null,
    Boolean: bool,
    Number: f64,
    const Self = @This();

    pub fn getType(self: Self) Type {
        return switch (self) {
            .Null => Type.Null,
            .Boolean => Type.Boolean,
            .Number => Type.Number,
        };
    }
};

pub const Error = error {
    ExpectValue,
    InvalidValue,
    RootNotSingular,
    NumberTooBig,
    // MissQuotationMask,
    // InvalidStringEscape,
    // InvalidStringChar,
    // InvalidUnicodeHex,
    // InvalidUnicodeSurrogate,
    // MissCommaOrSquareBracket,
    // MissKey,
    // MissColon,
    // MissCommaOrCurlyBracket,
};