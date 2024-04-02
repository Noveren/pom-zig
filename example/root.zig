
const std = @import("std");

const rgb = @import("rgb.zig");
const json = @import("json.zig");

test {
    std.testing.refAllDecls(rgb);
    _ = rgb;

    std.testing.refAllDecls(json);
    _ = json;
}