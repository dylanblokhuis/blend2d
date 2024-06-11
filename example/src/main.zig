const std = @import("std");
const c = @import("blend2d").c;
const BlContext = @import("blend2d").BlContext;

pub fn main() !void {
    var ctx = BlContext.init(std.heap.c_allocator, .{
        .image = .{
            .width = 600,
            .height = 600,
            .format = .prgb32,
        },
        .context = .{},
    });
    defer ctx.deinit();

    const id = try ctx.loadFontFace("./Inter-Regular.ttf", "inter-regular");
    // const id2 = try ctx.loadFontFace("./Inter-Regular.ttf", "inter-regular");
    // _ = id2; // autofix

    ctx.clear();
    // ctx.fillRoundedRect(.{
    //     .w = 200,
    //     .h = 200,
    //     .rx = 20,
    //     .ry = 20,
    //     .x = 0,
    //     .y = 0,
    // }, 0xFF00FF00);

    {
        const font = ctx.loadFont(id, 16).?;
        const options: BlContext.TextWrapOptions = .{
            .line_height = 2.0,
        };
        const textboudns = font.textBoundsWrap("Hello world! this is something you can get after or what?", 200, options);

        ctx.fillRect(.{
            .w = textboudns.w,
            .h = textboudns.h,
            .x = 40,
            .y = 40,
        }, 0xFFFFFFFF);

        // ctx.fillText(font, "Hello world! this", 0, 0, 0xFFFF0000);
        ctx.fillTextWrap(font, "Hello world! this is something you can get after or what?", 40, 40, 200, 0xFFFF0000, options);
    }

    {
        const font = ctx.loadFont(id, 16).?;
        const options: BlContext.TextWrapOptions = .{
            .line_height = 1.5,
        };
        const textboudns = font.textBoundsWrap("Hello world! this is something you can get after or what?", 200, options);

        ctx.fillRect(.{
            .w = textboudns.w,
            .h = textboudns.h,
            .x = 40,
            .y = 200,
        }, 0xFFFFFFFF);

        // ctx.fillText(font, "Hello world! this", 0, 0, 0xFFFF0000);
        ctx.fillTextWrap(font, "Hello world! this is something you can get after or what?", 40, 200, 200, 0xFFFF0000, options);
    }

    ctx.end();

    ctx.writeToFile("something.png");

    // while (true) {
    //     std.time.sleep(std.time.ns_per_s * 1);
    // }

    // ctx.writeToFile("something.png");
}
