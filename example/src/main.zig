const std = @import("std");
const c = @cImport({
    @cInclude("blend2d/blend2d.h");
});

inline fn check(result: c.BLResult) void {
    std.debug.assert(result == c.BL_SUCCESS);
}

const BlContext = struct {
    const Self = @This();
    const BlFormat = enum(c_int) {
        prgb32 = c.BL_FORMAT_PRGB32,
        xrgb32 = c.BL_FORMAT_XRGB32,
        a8 = c.BL_FORMAT_A8,
    };
    const BlImageOptions = struct {
        width: u32,
        height: u32,
        format: BlFormat,
    };

    const BlContextOptions = c.BLContextCreateInfo;

    pub const BlOptions = struct {
        image: BlImageOptions,
        context: BlContextOptions,
    };
    ctx: c.BLContextCore,
    img: c.BLImageCore,

    pub fn init(options: BlOptions) BlContext {
        var ctx: c.BLContextCore = undefined;
        var img: c.BLImageCore = undefined;

        check(c.blImageInit(&img));
        check(c.blImageCreate(&img, @intCast(options.image.width), @intCast(options.image.height), @intFromEnum(options.image.format)));

        check(c.blContextInitAs(&ctx, &img, &options.context));

        return Self{
            .ctx = ctx,
            .img = img,
        };
    }

    pub fn deinit(self: *Self) void {
        check(c.blImageDestroy(&self.img));
        check(c.blContextDestroy(&self.ctx));
    }

    pub fn clear(self: *Self) void {
        check(c.blContextClearAll(&self.ctx));
    }

    const Path = struct {
        path: c.BLPathCore,

        pub fn init() Path {
            var p: c.BLPathCore = undefined;
            check(c.blPathInit(&p));
            return Path{
                .path = p,
            };
        }

        pub fn deinit(self: *Path) void {
            check(c.blPathDestroy(&self.path));
        }

        pub fn clear(self: *Path) void {
            check(c.blPathClear(&self.path));
        }

        pub fn moveTo(self: *Path, x: f64, y: f64) void {
            check(c.blPathMoveTo(&self.path, x, y));
        }

        pub fn cubicTo(self: *Path, x1: f64, y1: f64, x2: f64, y2: f64, x3: f64, y3: f64) void {
            check(c.blPathCubicTo(&self.path, x1, y1, x2, y2, x3, y3));
        }

        /// color is 0xAARRGGBB
        pub fn fill(self: *Path, ctx: *BlContext, color: u32) void {
            check(c.blContextFillPathDRgba32(ctx, .{
                .x = 0,
                .y = 0,
            }, &self.path, color));
        }
    };

    pub fn path(self: *Self) Path {
        _ = self; // autofix
        return Path.init();
    }

    pub fn fillRect(self: *Self, rect: c.BLRect, color: u32) void {
        check(c.blContextFillRectDRgba32(&self.ctx, &rect, color));
    }

    pub fn fillRoundedRect(self: *Self, rect: c.BLRoundRect, color: u32) void {
        check(c.blContextFillGeometryRgba32(&self.ctx, c.BL_GEOMETRY_TYPE_ROUND_RECT, &rect, color));
    }

    pub fn end(self: *Self) void {
        check(c.blContextEnd(&self.ctx));
    }

    pub fn writeToFile(self: *Self, file_path: [:0]const u8) void {
        var codec: c.BLImageCodecCore = undefined;
        check(c.blImageCodecInit(&codec));
        check(c.blImageCodecFindByName(&codec, "PNG", c.SIZE_MAX, null));
        check(c.blImageWriteToFile(&self.img, file_path, &codec));
    }
};

pub fn main() !void {
    var ctx = BlContext.init(.{
        .image = .{
            .width = 800,
            .height = 800,
            .format = .prgb32,
        },
        .context = .{},
    });
    defer ctx.deinit();

    ctx.clear();
    ctx.fillRoundedRect(.{
        .w = 200,
        .h = 200,
        .rx = 20,
        .ry = 20,
        .x = 0,
        .y = 0,
    }, 0xFF00FF00);

    ctx.fillRoundedRect(.{
        .w = 200,
        .h = 200,
        .rx = 20,
        .ry = 20,
        .x = 300,
        .y = 300,
    }, 0xFFFFFFFF);
    ctx.end();

    ctx.writeToFile("something.png");
}
