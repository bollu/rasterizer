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
};

const TGAHeader = packed struct {
    idlength: u8,
    colormaptype: u8,
    datatypecode: u8,
    colormaporigin: u16,
    colormaplength: u16,
    colormapdepth: u8,
    x_origin: u16,
    y_origin: u16,
    width: u16,
    height: u16,
    bitsperpixel: u8,
    imagedescriptor: u8,
};

const Image = struct {
    width: i32,
    height: i32,
    bytespp: Format,
    data: []u8,

    pub fn init(width: i32, height: i32, bytespp: Format) !Image {
        const allocator = std.heap.page_allocator;

        return Image{
            .width = width,
            .height = height,
            .bytespp = bytespp,
            .data = try allocator.alloc(u8, @intCast(usize, width * height)),
        };
    }

    // s for set
    pub fn s(self: Image, x: i32, y: i32, ix: Color) void {
        return;
    }

    // https://github.com/ssloy/tinyrenderer/blob/909fe20934ba5334144d2c748805690a1fa4c89f/tgaimage.cpp#L148
    pub fn write_tga_file(self: Image, name: []const u8) !void {
        // Note the `try` keyword here.
        var f = try fs.cwd().createFile(name, fs.File.CreateFlags{ .truncate = true });
        defer f.close();
        var header = TGAHeader{
            .idlength = 0,
            .colormaptype = 0,
            .datatypecode = 0,
            .colormaporigin = 0,
            .colormaplength = 0,
            .colormapdepth = 0,
            .x_origin = 0,
            .y_origin = 0,
            .width = @intCast(u16, self.width),
            .height = @intCast(u16, self.height),
            .bitsperpixel = self.bytespp.toNumBits(),
            .imagedescriptor = 0,
        };
        try f.writeAll(&@bitCast([18]u8, header));
    }
};

pub fn main() !void {
    // const TGAColor white = TGAColor(255, 255, 255, 255);
    // const TGAColor red   = TGAColor(255, 0,   0,   255);
    // TGAImage image(100, 100, TGAImage::RGB);
    // image.set(52, 41, red);
    // image.flip_vertically(); // i want to have the origin at the left bottom corner of the image
    // image.write_tga_file("output.tga");
    const stdout = std.io.getStdOut().writer();
    const white = Color.rgba(255, 255, 255, 255);
    const red = Color.rgba(255, 0, 0, 255);
    var image = try Image.init(100, 100, Format.RGB);
    // image.s(52, 41, red);
    try stdout.print("Trying to write...\n", .{});
    try image.write_tga_file("output.tga");
    try stdout.print("Hello, {s}!\n", .{"world"});
}
