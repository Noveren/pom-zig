
const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn List(comptime T: type) type {
    return struct {
        memory: std.ArrayList(T),
        const Self = @This();

        pub fn init(allocator: std.mem.Allocator) Self {
            return Self { .memory = std.ArrayList(T).init(allocator) };
        }

        /// `deinit` the items in the list and the list
        pub fn deinit(self: Self) void {
            if (comptime std.meta.hasMethod(T, "deinit")) {
                for (self.memory.items) |*i| {
                    i.deinit();
                }
            }
            self.memory.deinit();
        }

        /// just `deinit` the list, maybe lead to memory leak
        pub inline fn rawDeinit(self: Self) void {
            self.memory.deinit();
        }

        pub inline fn append(self: *Self, item: T) std.mem.Allocator.Error!void {
            try self.memory.append(item);
        }

        pub inline fn items(self: Self) []const T {
            return self.memory.items;
        }

        pub inline fn len(self: Self) usize {
            return self.memory.items.len;
        }
    };
}