
const std = @import("std");
const Allocator = std.mem.Allocator;

pub const String = struct {
    const Self = @This();
    const List = std.ArrayList(u8);

    memory: List,

    pub inline fn init(allocator: Allocator) Self {
        return Self { .memory = List.init(allocator) };
    }

    pub inline fn deinit(self: *Self) void {
        self.memory.deinit();
    }

    pub fn from(allocator: Allocator, str: []const u8) std.mem.Allocator.Error!Self {
        var memory = List.init(allocator);
        try memory.appendSlice(str);
        return Self { .memory = memory };
    }

    pub fn format(self: Self, actual_fmt: anytype, options: anytype, writer: anytype) !void {
        _ = actual_fmt;
        _ = options;
        try std.fmt.format(writer, "{s}", .{ self.memory.items });
    }

    pub inline fn append(self: *Self, str: []const u8) std.mem.Allocator.Error!void {
        try self.memory.appendSlice(str);
    }
};

test "String" {
    var string = try String.from(std.testing.allocator, "Hello");
    defer string.deinit();
    try string.append(" World!");
    std.debug.print("{s}\n", .{string});
}