
const std = @import("std");

const rgb = @import("rgb.zig");

test {
    std.testing.refAllDecls(rgb);
    _ = rgb;
}