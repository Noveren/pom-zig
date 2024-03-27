
const std = @import("std");
const pom = @import("pom");

fn expectEqual(expect: anytype, actual: anytype) !void {
    if (@TypeOf(actual) == []const u8) {
        return std.testing.expectEqualStrings(expect, actual);
    } else {
        return std.testing.expectEqual(expect, actual);
    }
}

const RGB = struct {
    r: u8, g: u8, b: u8
};

fn hex2u8(buf: []const u8, _: std.mem.Allocator) ?u8 {
    return std.fmt.parseInt(u8, buf, 16) catch null;
}

fn toRGB(l: pom.List(u8), _: std.mem.Allocator) ?RGB {
    const items = l.getItems();
    return RGB {
        .r = items[0], .g = items[1], .b = items[2], 
    };
}

const digit = pom.terminal.digit(.Hex);

const color2: pom.Parser(u8) =
    digit.times(2).slice().map(u8, hex2u8)
;
const rgb6: pom.Parser(RGB) =
    color2.times(3).map(RGB, toRGB)
;

const color1: pom.Parser(u8) =
    digit.slice().map(u8, hex2u8)
;
const rgb3: pom.Parser(RGB) =
    color1.times(3).map(RGB, toRGB)
;

const rgb: pom.Parser(RGB) = pom.Choice(RGB)
    .with(rgb6)
    .with(rgb3)
    .build()
    .prefix(pom.terminal.literal("#"))
    .suffix(pom.terminal.anychar.pred(false))
;

test "rgb6" {
    var r1 = rgb6.parse("09AF35", std.testing.allocator);    
    defer r1.drop();
    if (r1.rst) |ok| {
        try expectEqual(RGB { .r = 0x09, .g = 0xAF, .b = 0x35 }, ok);
        try expectEqual(6, r1.mov);
    } else |err| {
        std.debug.print("{any}\n", .{err});
        try expectEqual(true, r1.isOk());
    }
}

test "rgb3" {
    var r1 = rgb3.parse("9F5", std.testing.allocator);    
    defer r1.drop();
    if (r1.rst) |ok| {
        try expectEqual(RGB { .r = 0x9, .g = 0xF, .b = 0x5 }, ok);
        try expectEqual(3, r1.mov);
    } else |err| {
        std.debug.print("{any}\n", .{err});
        try expectEqual(true, r1.isOk());
    }
}

test "rgb" {
    var r1 = rgb.parse("#9F5", std.testing.allocator);    
    defer r1.drop();
    if (r1.rst) |ok| {
        try expectEqual(RGB { .r = 0x9, .g = 0xF, .b = 0x5 }, ok);
        try expectEqual(4, r1.mov);
    } else |err| {
        std.debug.print("{any}\n", .{err});
        try expectEqual(true, r1.isOk());
    }

    var r2 = rgb.parse("#09AF35", std.testing.allocator);    
    defer r2.drop();
    if (r2.rst) |ok| {
        try expectEqual(RGB { .r = 0x09, .g = 0xAF, .b = 0x35 }, ok);
        try expectEqual(7, r2.mov);
    } else |err| {
        std.debug.print("{any}\n", .{err});
        try expectEqual(true, r2.isOk());
    }

    var r3 = rgb.parse("09AF35", std.testing.allocator);
    defer r3.drop();
    try std.testing.expectError(pom.Err.ExpectForLiteral, r3.rst);

    var r4 = rgb.parse("#09AF35 ", std.testing.allocator);
    defer r4.drop();
    try std.testing.expectError(pom.Err.FailedToPred, r4.rst);
}
    