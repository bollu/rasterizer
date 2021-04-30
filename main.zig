// https://ziglang.org/documentation/0.7.1/
// https://github.com/ssloy/tinyrenderer/wiki/Lesson-0:-getting-started
// https://ziglang.org/documentation/master/std/#std;ArrayList
// https://github.com/ssloy/tinyrenderer/blob/909fe20934ba5334144d2c748805690a1fa4c89f/tgaimage.h
// https://gsquire.github.io/static/post/a-brief-exploration-of-zig/

const std = @import("std");
const Allocator = std.mem.Allocator;
const fs = std.fs;

const Color = struct {
    b: u8,
    g: u8,
    r: u8,

    pub fn empty() Color {
        return Color{ .b = 0, .g = 0, .r = 0 };
    }

    pub fn rgb(r: u8, g: u8, b: u8) Color {
        return Color{ .b = b, .g = g, .r = r};
    }
};

// top-left is (0, 0)
const Image = struct {
    width: usize,
    height: usize,
    data: []Color,

    pub fn init(width: usize, height: usize) !Image {
        const allocator = std.heap.page_allocator;
        var black = try allocator.alloc(Color, @intCast(usize, width * height));
        var i : usize = 0;
        while(i < width * height) {
            black[i] = Color.rgb(0, 0, 0);
            i += 1;
        }

        return Image{
            .width = width,
            .height = height,
            .data = black
        };
    }

    // s for set.
    pub fn s(self: Image, x: usize, y: usize, color: Color) void { self.data[x*self.height+y] = color; }

    // g for get.
    pub fn g(self: Image, x: usize, y: usize) Color { return self.data[x*self.height+y]; }

    // https://github.com/ssloy/tinyrenderer/blob/909fe20934ba5334144d2c748805690a1fa4c89f/tgaimage.cpp#L148
    // https://github.com/bollu/smallpths/blob/cf91240b7f1db8f15e1567f724c1f97cad1f13d1/smallpt-hs.hs#L475-L479
    pub fn write_ppm_file(self: Image, name: []const u8) !void {
        // Note the `try` keyword here.
        var f = try fs.cwd().createFile(name, fs.File.CreateFlags{ .truncate = true });
        defer f.close();
        try f.outStream().print("P3\n {} {} {}\n", .{self.width, self.height, 255});

        var y : usize = 0; 
        while(y < self.height) {
            var x : usize = 0;
            while (x < self.width) {
                const c : Color = self.g(x, y);
                try f.outStream().print("{} {} {} ", .{c.r, c.g, c.b});
                x += 1;
            }
            y += 1;
        }


    }
};


fn swap(comptime T: type, a: *T, b: *T) void {
    const olda = a.*;
    a.* = b.*;
    b.* = olda;
}


fn line(x0: usize, y0:usize, x1:usize, y1:usize, color: Color, image: *Image) !void { 

    var xl = x0; var xr = x1; var yl = y0; var yr = y1;
    // left-to-right.
    if (xl > xr) {
        swap(usize, &xl, &xr);
        swap(usize, &yl, &yr);
    }

    const Dx : i32 = @intCast(i32, xr) - @intCast(i32, xl);
    const Dy : i32 = @intCast(i32, yr) - @intCast(i32, yl);
    
    if ((try std.math.absInt(Dx)) > (try std.math.absInt(Dy))) {
         const M : f32 = @intToFloat(f32, Dy) / @intToFloat(f32, Dx);
         var x = xl;
         var y = yl;
         var m : f32 = 0.0;
         while(x <= xr) {
             image.s(x, y, color);
             x += 1;
             m += M;
             while (m >= 1) { y += 1; m -= 1; }
         }
         
     }  else {
     }
}


pub fn main() !void {


    const a : usize = 10;
    const b : usize = 20;

    const Dx : i32 = try std.math.absInt(@intCast(i32, a) - @intCast(i32, b));

    const stdout = std.io.getStdOut().writer();
    const white = Color.rgb(255, 255, 255);
    const red = Color.rgb(255, 0, 0);
    var image = try Image.init(800, 600);
    try line(0, 100, 200, 100, red, &image);
    image.s(52, 41, red);
    try stdout.print("Trying to write...\n", .{});
    try image.write_ppm_file("output.ppm");
    try stdout.print("Hello, {s}!\n", .{"world"});
}
