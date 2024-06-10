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

    const BlFont = struct {
        inner: c.BLFontCore,
        size: u32,
        face_name: [:0]const u8,

        pub fn textBoundsNoWrap(self: *const BlFont, text: [:0]const u8) c.BLSize {
            var glyphbox: c.BLGlyphBufferCore = undefined;
            check(c.blGlyphBufferInit(&glyphbox));
            defer check(c.blGlyphBufferDestroy(&glyphbox));

            check(c.blGlyphBufferSetText(&glyphbox, text.ptr, text.len, c.BL_TEXT_ENCODING_UTF8));
            check(c.blFontShape(&self.inner, &glyphbox));

            var font_metrics: c.BLTextMetrics = .{};
            check(c.blFontGetTextMetrics(&self.inner, &glyphbox, &font_metrics));

            std.log.debug("{any}", .{font_metrics});

            return .{
                // xmin, ymin, xmax, ymax
                .w = font_metrics.boundingBox.x1 - font_metrics.boundingBox.x0,
                .h = font_metrics.boundingBox.y1 - font_metrics.boundingBox.y0,
            };
        }
    };

    allocator: std.mem.Allocator,
    ctx: c.BLContextCore,
    img: c.BLImageCore,
    faces: std.StringArrayHashMapUnmanaged(c.BLFontFaceCore) = .{},
    // fonts: std.AutoArrayHashMapUnmanaged(BlFont, c.BLFontCore) = .{},

    pub fn init(allocator: std.mem.Allocator, options: BlOptions) BlContext {
        var ctx: c.BLContextCore = undefined;
        var img: c.BLImageCore = undefined;

        check(c.blImageInit(&img));
        check(c.blImageCreate(&img, @intCast(options.image.width), @intCast(options.image.height), @intFromEnum(options.image.format)));

        check(c.blContextInitAs(&ctx, &img, &options.context));

        return Self{
            .ctx = ctx,
            .img = img,
            .allocator = allocator,
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

    pub fn fillText(self: *Self, font: BlFont, text: [:0]const u8, x: f64, y: f64, color: u32) void {
        const origin = c.BLPoint{
            .x = x,
            .y = y,
        };
        check(c.blContextFillUtf8TextDRgba32(&self.ctx, &origin, &font.inner, text.ptr, text.len, color));
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

    pub fn loadFontFace(self: *Self, file_path: [:0]const u8, id: [:0]const u8) ![:0]const u8 {
        var face: c.BLFontFaceCore = undefined;
        check(c.blFontFaceInit(&face));
        check(c.blFontFaceCreateFromFile(&face, file_path, 0));

        try self.faces.put(self.allocator, id, face);

        return id;
    }

    pub fn unloadFontFace(self: *Self, id: [:0]const u8) void {
        const face = self.faces.remove(id) orelse return;
        check(c.blFontFaceDestroy(face));
        self.faces.remove(id);
    }

    pub fn loadFont(self: *Self, face_id: [:0]const u8, size: u32) ?BlFont {
        var font: c.BLFontCore = undefined;
        check(c.blFontInit(&font));
        const face = self.faces.getPtr(face_id) orelse return null;
        check(c.blFontCreateFromFace(&font, face, @floatFromInt(size)));

        return BlFont{
            .inner = font,
            .size = size,
            .face_name = face_id,
        };
    }
};

pub fn main() !void {
    var ctx = BlContext.init(std.heap.c_allocator, .{
        .image = .{
            .width = 800,
            .height = 800,
            .format = .prgb32,
        },
        .context = .{},
    });
    defer ctx.deinit();

    const id = try ctx.loadFontFace("./Inter-Regular.ttf", "inter-regular");
    const font = ctx.loadFont(id, 90).?;

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

    ctx.fillText(font, "Hello world!", 40, 200, 0xFFFF0000);
    std.debug.print("{any}\n", .{font.textBoundsNoWrap("Hello world!!!!!!!!!!!!!!!!!!!!!!!!1")});
    ctx.end();

    ctx.writeToFile("something.png");

    // while (true) {
    //     std.time.sleep(std.time.ns_per_s * 1);
    // }

    // ctx.writeToFile("something.png");
}
