// https://ziglang.org/documentation/0.7.1/
// https://github.com/ssloy/tinyrenderer/wiki/Lesson-0:-getting-started
// https://ziglang.org/documentation/master/std/#std;ArrayList
// https://github.com/ssloy/tinyrenderer/blob/909fe20934ba5334144d2c748805690a1fa4c89f/tgaimage.h
// https://gsquire.github.io/static/post/a-brief-exploration-of-zig/

const std = @import("std");
const Allocator = std.mem.Allocator;
const fs = std.fs;

const Format = enum {
    RGB,
    RGBA,

    pub fn toNumBits(self: Format) u8 {
        return switch (self) {
            Format.RGB => 3 * 8,
            Format.RGBA => 4 * 8,
        };
    }
};

const Color = struct {
    b: u8,
    g: u8,
    r: u8,
    a: u8,
    bytespp: i32,

    pub fn empty() Color {
        return Color{ .b = 0, .g = 0, .r = 0, .a = 0, .bytespp = 1 };
    }

    pub fn rgba(r: u8, g: u8, b: u8, a: u8) Color {
        return Color{ .b = b, .g = g, .r = r, .a = a, .bytespp = 4 };
    }

    pub fn rgb(r: u8, g: u8, b: u8) Color {
        return Color{ .b = b, .g = g, .r = r, .a = 255, .bytespp = 4 };
    }
};

// top-left is (0, 0)
const Image = struct {
    width: usize,
    height: usize,
    bytespp: Format,
    data: []Color,

    pub fn init(width: usize, height: usize, bytespp: Format) !Image {
        const allocator = std.heap.page_allocator;
        var black = try allocator.alloc(Color, @intCast(usize, width * height));
        var i : usize = 0;
        while(i < width * height) {
            black[i] = Color.rgba(0, 0, 0, 0);
            i += 1;
        }

        return Image{
            .width = width,
            .height = height,
            .bytespp = bytespp,
            .data = black
        };
    }

    // s for set.
    pub fn s(self: Image, x: usize, y: usize, color: Color) void { self.data[x*self.height+y] = color; }

    // g for get.
    pub fn g(self: Image, x: usize, y: usize) Color { return self.data[x*height+y]; }

    // https://github.com/ssloy/tinyrenderer/blob/909fe20934ba5334144d2c748805690a1fa4c89f/tgaimage.cpp#L148
    // https://github.com/bollu/smallpths/blob/cf91240b7f1db8f15e1567f724c1f97cad1f13d1/smallpt-hs.hs#L475-L479
    pub fn write_ppm_file(self: Image, name: []const u8) !void {
        // Note the `try` keyword here.
        var f = try fs.cwd().createFile(name, fs.File.CreateFlags{ .truncate = true });
        try f.outStream().print("P3\n {} {} {}\n", .{self.width, self.height, 255});

        var x : usize = 0; 
        while (x < self.width) {
            var y : usize = 0; 
            while(y < self.height) {
                const c : Color = self.data[x+y*self.width];
                try f.outStream().print("{} {} {} ", .{c.r, c.g, c.b});
                y += 1;
            }
            x += 1;
        }
        defer f.close();


    }
};

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const white = Color.rgba(255, 255, 255, 255);
    const red = Color.rgba(255, 0, 0, 255);
    var image = try Image.init(100, 100, Format.RGB);
    var i : usize = 0;
    while(i < 25) {
        image.s(i, i, Color.rgb(255, 0, 0));
        i += 1;
    }
    // image.s(52, 41, red);
    try stdout.print("Trying to write...\n", .{});
    try image.write_ppm_file("output.ppm");
    try stdout.print("Hello, {s}!\n", .{"world"});
}
