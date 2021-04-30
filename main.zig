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
    width: i32,
    height: i32,
    data: []Color,

    pub fn init(width: i32, height: i32) !Image {
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
    pub fn s(self: Image, x: i32, y: i32, color: Color) void { self.data[@intCast(usize, x*self.height+y)] = color; }

    // g for get.
    pub fn g(self: Image, x: i32, y: i32) Color { return self.data[@intCast(usize, x*self.height+y)]; }

    // https://github.com/ssloy/tinyrenderer/blob/909fe20934ba5334144d2c748805690a1fa4c89f/tgaimage.cpp#L148
    // https://github.com/bollu/smallpths/blob/cf91240b7f1db8f15e1567f724c1f97cad1f13d1/smallpt-hs.hs#L475-L479
    pub fn write_ppm_file(self: Image, name: []const u8) !void {
        // Note the `try` keyword here.
        var f = try fs.cwd().createFile(name, fs.File.CreateFlags{ .truncate = true });
        defer f.close();
        try f.outStream().print("P3\n {} {} {}\n", .{self.width, self.height, 255});

        var y : i32 = 0; 
        while(y < self.height) {
            var x : i32 = 0;
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

fn sgn(comptime T: type, a: T) T {
  if (a >= 0) { return 1; } else { return -1; }
}


fn line(x0: i32, y0:i32, x1:i32, y1:i32, color: Color, image: *Image) !void { 

    
    if ((try std.math.absInt(x1 - x0)) > (try std.math.absInt(y1 - y0))) {
        var xl = x0; var xr = x1; var yl = y0; var yr = y1;
        // left-to-right.
        if (xl > xr) {
            swap(i32, &xl, &xr);
            swap(i32, &yl, &yr);
        }

         const M : f32 = @intToFloat(f32, yr - yl) / @intToFloat(f32, xr - xl);
         var x = xl;
         var y = yl;
         var m : f32 = 0.0;
         while(x <= xr) {
             image.s(x, y, color);
             x += 1;
             m += std.math.fabs(M);
             while (m >= 1) { y += sgn(i32, yr - yl); m -= 1; }
         }
         
     }  else {
        var xb = x0; var xt = x1; var yb = y0; var yt = y1;
        // bottom-to-top
        if (yb > yt) {
            swap(i32, &xb, &xt);
            swap(i32, &yb, &yt);
        }

         const M : f32 = @intToFloat(f32, xt - xb) / @intToFloat(f32, yt - yb);
         var x = xb;
         var y = yb;
         var m : f32 = 0.0;
         while(y <= yt) {
             image.s(x, y, color);
             y += 1;
             m += std.math.fabs(M);
             while (m >= 1) { x += sgn(i32, xt - xb); m -= 1; }
         }

     }
}


pub fn main() !void {


    const stdout = std.io.getStdOut().writer();
    const white = Color.rgb(255, 255, 255);
    const red = Color.rgb(255, 0, 0);
    const blue = Color.rgb(0, 0, 255);
    var image = try Image.init(800, 600);
    // try line(0, 100, 200, 100, red, &image);
    // try line(300, 200, 0, 200, blue, &image);

    var i : i32 = 0;
    while(i < 360) {
        i += 1;
        const theta : f32 = @intToFloat(f32, i) / 3.14;
        try line(300, 300, 
            300 + @floatToInt(i32, 250 * std.math.cos(theta)), 
            300 + @floatToInt(i32, 250 * std.math.sin(theta)), red, &image);
    }

    try stdout.print("Trying to write...\n", .{});
    try image.write_ppm_file("output.ppm");
    try stdout.print("Hello, {s}!\n", .{"world"});
}
