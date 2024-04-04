
const std = @import("std");

const parser = @import("parser.zig");
const utils = @import("utils.zig");

pub const String = utils.String;

pub const Type = enum {
    Null, Boolean, Number, String
};

pub const Value = union(Type) {
    Null,
    Boolean: bool,
    Number: f64,
    String: String,
    const Self = @This();

    pub fn getType(self: Self) Type {
        return switch (self) {
            .Null => Type.Null,
            .Boolean => Type.Boolean,
            .Number => Type.Number,
            .String => Type.String,
        };
    }

    pub fn deinit(self: *Self) void {
        switch (self.*) {
            else => {},
            .String => |*v| v.deinit(),
        }
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
    // InvalidUnicodeHex,
    // InvalidUnicodeSurrogate,
    // MissCommaOrSquareBracket,
    // MissKey,
    // MissColon,
    // MissCommaOrCurlyBracket,
};