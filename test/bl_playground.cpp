#include <blend2d.h>

int main(int argc, char* argv[]) {
  BLImage img(480, 480, BL_FORMAT_PRGB32);
  BLContext ctx(img);

  ctx.clearAll();
  ctx.fillRect(100, 100, 150, 150, BLRgba32(0xFFFFFFFF));

  ctx.end();

  img.writeToFile("bl_playground.png");
  return 0;
}
