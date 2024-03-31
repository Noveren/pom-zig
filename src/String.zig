
const std = @import("std");

const Self = @This();

view: []u8,
capacity: usize,
allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator) Self {
    return Self {
        .view = &[_]u8{},
        .capacity = 0,
        .allocator = allocator,
    };
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.view);
}

pub fn from(str: []const u8, allocator: std.mem.Allocator) std.mem.Allocator.Error!Self {
    const _view = try allocator.alloc(u8, str.len);
    return Self {
        .view = _view,
    };
}