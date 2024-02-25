// This file is part of Blend2D project <https://blend2d.com>
//
// See blend2d.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../../api-build_p.h"
#if !defined(BL_BUILD_NO_JIT)

#include "../../pipeline/jit/pipecompiler_p.h"
#include "../../support/intops_p.h"

namespace bl {
namespace Pipeline {
namespace JIT {

// bl::Pipeline::PipeCompiler - Predicate Helpers
// ==============================================

#if defined(BL_JIT_ARCH_X86)
void PipeCompiler::x_make_predicate_v32(const Vec& vmask, const Gp& count) noexcept {
  x86::Mem maskPtr = _getMemConst(commonTable.loadstore16_lo8_msk8());
  maskPtr._setIndex(cc->_gpSignature.regType(), count.id());
  maskPtr.setShift(3);
  cc->vpmovsxbd(vmask, maskPtr);
}

void PipeCompiler::x_ensure_predicate_8(PixelPredicate& predicate, uint32_t maxWidth) noexcept {
  BL_ASSERT(!predicate.empty());

  blUnused(maxWidth);

  if (hasAVX512()) {
    if (!predicate.k.isValid()) {
      x86::Mem mem = _getMemConst(commonTable.k_msk16_data);
      predicate.k = cc->newKq("mask_k");
      mem._setIndex(cc->_gpSignature.regType(), predicate.count.id());
      mem.setShift(1);
      cc->kmovw(predicate.k, mem);
    }
  }
  else {
    BL_NOT_REACHED();
  }
}

void PipeCompiler::x_ensure_predicate_32(PixelPredicate& predicate, uint32_t maxWidth) noexcept {
  BL_ASSERT(!predicate.empty());

  if (hasAVX512()) {
    if (!predicate.k.isValid()) {
      x86::Mem mem = _getMemConst(commonTable.k_msk16_data);
      predicate.k = cc->newKq("mask_k");
      mem._setIndex(cc->_gpSignature.regType(), predicate.count.id());
      mem.setShift(1);
      cc->kmovw(predicate.k, mem);
    }
  }
  else {
    if (!predicate.v32.isValid()) {
      if (maxWidth <= 4)
        predicate.v32 = newV128("mask_v32");
      else
        predicate.v32 = newV256("mask_v32");
      x_make_predicate_v32(predicate.v32, predicate.count);
    }
  }
}
#endif // BL_JIT_ARCH_X86

// bl::Pipeline::PipeCompiler - Fetch Helpers
// ==========================================

void PipeCompiler::x_fetch_mask_a8_advance(VecArray& vm, PixelCount n, PixelType pixelType, const Gp& mPtr, const Vec& globalAlpha) noexcept {
  Mem m = mem_ptr(mPtr);

  switch (pixelType) {
    case PixelType::kA8: {
      BL_ASSERT(n != 1u);

      SimdWidth simdWidth = simdWidthOf(DataWidth::k16, n);
      uint32_t regCount = regCountOf(DataWidth::k16, n);

      newVecArray(vm, regCount, simdWidth, "vm");

      switch (n.value()) {
        case 2:
#if defined(BL_JIT_ARCH_X86)
          if (hasAVX2())
          {
            v_broadcast_u16(vm[0], m);
          }
          else
#endif // BL_JIT_ARCH_X86
          {
            v_loadu16(vm[0], m);
          }
          v_cvt_u8_to_u16(vm[0], vm[0]);
          break;

        case 4:
          v_loada32(vm[0], m);
          v_cvt_u8_to_u16(vm[0], vm[0]);
          break;

        case 8:
          v_cvt_u8_to_u16(vm[0], m);
          break;

        default: {
          for (uint32_t i = 0; i < regCount; i++) {
            v_cvt_u8_to_u16(vm[i], m);
            m.addOffsetLo32(vm[i].size() / 2u);
          }
          break;
        }
      }

      add(mPtr, mPtr, n.value());

      if (globalAlpha.isValid()) {
        v_mul_i16(vm, vm, globalAlpha.cloneAs(vm[0]));
        v_div255_u16(vm);
      }

      break;
    }

    case PixelType::kRGBA32: {
      SimdWidth simdWidth = simdWidthOf(DataWidth::k64, n);
      uint32_t regCount = regCountOf(DataWidth::k64, n);

      newVecArray(vm, regCount, simdWidth, "vm");

      switch (n.value()) {
        case 1: {
          BL_ASSERT(regCount == 1);

#if defined(BL_JIT_ARCH_X86)
          if (!hasAVX2()) {
            v_load8(vm[0], m);
            add(mPtr, mPtr, n.value());
            v_swizzle_lo_u16x4(vm[0], vm[0], swizzle(0, 0, 0, 0));
          }
          else
#endif // BL_JIT_ARCH_X86
          {
            v_broadcast_u8(vm[0], m);
            add(mPtr, mPtr, n.value());
            v_cvt_u8_to_u16(vm[0], vm[0]);
          }

          if (globalAlpha.isValid()) {
            v_mul_i16(vm[0], vm[0], globalAlpha.cloneAs(vm[0]));
            v_div255_u16(vm[0]);
          }
          break;
        }

        case 2: {
          BL_ASSERT(regCount == 1);

#if defined(BL_JIT_ARCH_X86)
          if (!hasAVX2()) {
            v_loadu16(vm[0], m);
            add(mPtr, mPtr, n.value());
            v_swizzle_lo_u16x4(vm[0], vm[0], swizzle(0, 0, 0, 0));
            // TODO: Obviously wrong!
          }
          else
#endif // BL_JIT_ARCH_X86
          {
            v_loadu16_u8_to_u64(vm[0], m);
            add(mPtr, mPtr, n.value());
            v_swizzlev_u8(vm[0], vm[0], simdConst(&ct.pshufb_xxxxxxx1xxxxxxx0_to_z1z1z1z1z0z0z0z0, Bcst::kNA, vm[0]));
          }

          if (globalAlpha.isValid()) {
            v_mul_i16(vm[0], vm[0], globalAlpha.cloneAs(vm[0]));
            v_div255_u16(vm[0]);
          }
          break;
        }

        case 4: {
#if defined(BL_JIT_ARCH_X86)
          if (simdWidth >= SimdWidth::k256) {
            v_loadu32_u8_to_u64(vm[0], m);
            add(mPtr, mPtr, n.value());
            v_swizzlev_u8(vm[0], vm[0], simdConst(&ct.pshufb_xxxxxxx1xxxxxxx0_to_z1z1z1z1z0z0z0z0, Bcst::kNA, vm[0]));

            if (globalAlpha.isValid()) {
              v_mul_i16(vm[0], vm[0], globalAlpha.cloneAs(vm[0]));
              v_div255_u16(vm[0]);
            }
          }
          else
#endif // BL_JIT_ARCH_X86
          {
            v_loada32(vm[0], m);
            add(mPtr, mPtr, n.value());
            v_cvt_u8_to_u16(vm[0], vm[0]);

            if (globalAlpha.isValid()) {
              v_mul_i16(vm[0], vm[0], globalAlpha.cloneAs(vm[0]));
              v_div255_u16(vm[0]);
            }

            v_interleave_lo_u16(vm[0], vm[0], vm[0]);           // vm[0] = [M3 M3 M2 M2 M1 M1 M0 M0]
            v_swizzle_u32x4(vm[1], vm[0], swizzle(3, 3, 2, 2)); // vm[1] = [M3 M3 M3 M3 M2 M2 M2 M2]
            v_swizzle_u32x4(vm[0], vm[0], swizzle(1, 1, 0, 0)); // vm[0] = [M1 M1 M1 M1 M0 M0 M0 M0]
          }
          break;
        }

        default: {
#if defined(BL_JIT_ARCH_X86)
          if (simdWidth >= SimdWidth::k256) {
            for (uint32_t i = 0; i < regCount; i++) {
              v_loaduvec_u8_to_u64(vm[i], m);
              m.addOffsetLo32(vm[i].size() / 8u);
            }

            add(mPtr, mPtr, n.value());

            if (globalAlpha.isValid()) {
              if (hasOptFlag(PipeOptFlags::kFastVpmulld)) {
                v_mul_i32(vm, vm, globalAlpha.cloneAs(vm[0]));
                v_div255_u16(vm);
                v_swizzle_u32x4(vm, vm, swizzle(2, 2, 0, 0));
              }
              else {
                v_mul_i16(vm, vm, globalAlpha.cloneAs(vm[0]));
                v_div255_u16(vm);
                v_swizzlev_u8(vm, vm, simdConst(&ct.pshufb_xxxxxxx1xxxxxxx0_to_z1z1z1z1z0z0z0z0, Bcst::kNA, vm[0]));
              }
            }
            else {
              v_swizzlev_u8(vm, vm, simdConst(&ct.pshufb_xxxxxxx1xxxxxxx0_to_z1z1z1z1z0z0z0z0, Bcst::kNA, vm[0]));
            }
          }
          else
#endif // BL_JIT_ARCH_X86
          {
            // Maximum pixels for 128-bit SIMD is 8 - there are no registers for more...
            BL_ASSERT(n == 8);

            v_cvt_u8_to_u16(vm[0], m);

            if (globalAlpha.isValid()) {
              v_mul_i16(vm[0], vm[0], globalAlpha.cloneAs(vm[0]));
              v_div255_u16(vm[0]);
            }

            add(mPtr, mPtr, n.value());

            v_interleave_hi_u16(vm[2], vm[0], vm[0]);           // vm[2] = [M7 M7 M6 M6 M5 M5 M4 M4]
            v_interleave_lo_u16(vm[0], vm[0], vm[0]);           // vm[0] = [M3 M3 M2 M2 M1 M1 M0 M0]
            v_swizzle_u32x4(vm[3], vm[2], swizzle(3, 3, 2, 2)); // vm[3] = [M7 M7 M7 M7 M6 M6 M6 M6]
            v_swizzle_u32x4(vm[1], vm[0], swizzle(3, 3, 2, 2)); // vm[1] = [M3 M3 M3 M3 M2 M2 M2 M2]
            v_swizzle_u32x4(vm[0], vm[0], swizzle(1, 1, 0, 0)); // vm[0] = [M1 M1 M1 M1 M0 M0 M0 M0]
            v_swizzle_u32x4(vm[2], vm[2], swizzle(1, 1, 0, 0)); // vm[2] = [M5 M5 M5 M5 M4 M4 M4 M4]
          }
          break;
        }
      }

      break;
    }

    default:
      BL_NOT_REACHED();
  }
}

void PipeCompiler::x_fetch_pixel(Pixel& p, PixelCount n, PixelFlags flags, FormatExt format, const Mem& src_, Alignment alignment) noexcept {
  PixelPredicate noPredicate;
  x_fetch_pixel(p, n, flags, format, src_, alignment, noPredicate);
}

void PipeCompiler::x_fetch_pixel(Pixel& p, PixelCount n, PixelFlags flags, FormatExt format, const Mem& src_, Alignment alignment, PixelPredicate& predicate) noexcept  {
  switch (p.type()) {
    case PixelType::kA8:
      _x_fetch_pixel_a8(p, n, flags, format, src_, alignment, predicate);
      break;

    case PixelType::kRGBA32:
      _x_fetch_pixel_rgba32(p, n, flags, format, src_, alignment, predicate);
      break;

    default:
      BL_NOT_REACHED();
  }
}

void PipeCompiler::_x_fetch_pixel_a8(Pixel& p, PixelCount n, PixelFlags flags, FormatExt format, const Mem& src_, Alignment alignment, PixelPredicate& predicate) noexcept {
  BL_ASSERT(p.isA8());

  blUnused(predicate);

  Mem src(src_);

  p.setCount(n);

#if defined(BL_JIT_ARCH_X86)
  SimdWidth paWidth = simdWidthOf(DataWidth::k8, n);
  SimdWidth uaWidth = simdWidthOf(DataWidth::k16, n);
#endif // BL_JIT_ARCH_X86

  // It's forbidden to use PA in single-pixel case (scalar mode) and SA in multiple-pixel case (vector mode).
  BL_ASSERT(uint32_t(n.value() != 1) ^ uint32_t(blTestFlag(flags, PixelFlags::kSA)));

  // It's forbidden to request both - PA and UA.
  BL_ASSERT((flags & (PixelFlags::kPA | PixelFlags::kUA)) != (PixelFlags::kPA | PixelFlags::kUA));

  switch (format) {
    case FormatExt::kPRGB32: {
      Vec predicatedPixel;

#if defined(BL_JIT_ARCH_X86)
      SimdWidth p32Width = simdWidthOf(DataWidth::k32, n);
      uint32_t p32RegCount = SimdWidthUtils::regCountOf(p32Width, DataWidth::k32, n);

      if (!predicate.empty()) {
        // TODO: [JIT] Do we want to support masked loading of more that 1 register?
        BL_ASSERT(n.value() > 1);
        BL_ASSERT(regCountOf(DataWidth::k32, n) == 1);

        predicatedPixel = newVec(p32Width, p.name(), "pred");
        x_ensure_predicate_32(predicate, n.value());
        v_load_predicated_v32(predicatedPixel, predicate, src);
      }
#endif // BL_JIT_ARCH_X86

      auto fetch4Shifted = [](PipeCompiler* pc, const Vec& dst, const Mem& src, Alignment alignment, const Vec& predicatedPixel) noexcept {
#if defined(BL_JIT_ARCH_X86)
        if (predicatedPixel.isValid()) {
          pc->v_srli_u32(dst, predicatedPixel, 24);
        }
        else if (pc->hasAVX512()) {
          pc->v_srli_u32(dst, src, 24);
        }
        else
#endif // BL_JIT_ARCH_X86
        {
          blUnused(predicatedPixel);
          pc->v_loada128(dst, src, alignment);
          pc->v_srli_u32(dst, dst, 24);
        }
      };

      switch (n.value()) {
        case 1: {
          p.sa = newGp32("a");
          src.addOffset(3);
          load_u8(p.sa, src);
          break;
        }

        case 4: {
          if (blTestFlag(flags, PixelFlags::kPA)) {
            newVecArray(p.pa, 1, SimdWidth::k128, p.name(), "pa");
            Vec a = p.pa[0];

            fetch4Shifted(this, a, src, alignment, predicatedPixel);
#if defined(BL_JIT_ARCH_X86)
            if (hasAVX512()) {
              cc->vpmovdb(a, a);
            }
            else
#endif // BL_JIT_ARCH_X86
            {
              v_packs_i32_i16(a, a, a);
              v_packs_i16_u8(a, a, a);
            }

            p.pa.init(a);
          }
          else {
            newVecArray(p.ua, 1, SimdWidth::k128, p.name(), "ua");
            Vec a = p.ua[0];

            fetch4Shifted(this, a, src, alignment, predicatedPixel);
            v_packs_i32_i16(a, a, a);

            p.ua.init(a);
          }

          break;
        }

        case 8: {
          Vec a0 = newV128("pa");

#if defined(BL_JIT_ARCH_X86)
          if (hasAVX512()) {
            Vec aTmp = newV256("a.tmp");
            v_srli_u32(aTmp, src, 24);

            if (blTestFlag(flags, PixelFlags::kPA)) {
              cc->vpmovdb(a0, aTmp);
              p.pa.init(a0);
              rename(p.pa, p.name(), "pa");
            }
            else {
              cc->vpmovdw(a0, aTmp);
              p.ua.init(a0);
              rename(p.ua, p.name(), "ua");
            }
          }
          else
#endif // BL_JIT_ARCH_X86
          {
            Vec a1 = newV128("paHi");

            fetch4Shifted(this, a0, src, alignment, predicatedPixel);
            src.addOffsetLo32(16);
            fetch4Shifted(this, a1, src, alignment, predicatedPixel);
            v_packs_i32_i16(a0, a0, a1);

            if (blTestFlag(flags, PixelFlags::kPA)) {
              v_packs_i16_u8(a0, a0, a0);
              p.pa.init(a0);
              rename(p.pa, p.name(), "pa");
            }
            else {
              p.ua.init(a0);
              rename(p.ua, p.name(), "ua");
            }
          }
          break;
        }

        case 16:
        case 32:
        case 64: {
#if defined(BL_JIT_ARCH_X86)
          if (hasAVX512()) {
            VecArray p32;
            newVecArray(p32, p32RegCount, p32Width, p.name(), "p32");

            auto multiVecUnpack = [](PipeCompiler* pc, VecArray& dst, VecArray src, uint32_t srcWidth) noexcept {
              uint32_t dstVecSize = dst[0].size();

              // Number of bytes in dst registers after this is done.
              uint32_t dstWidth = blMin<uint32_t>(dst.size() * dstVecSize, src.size() * srcWidth) / dst.size();

              for (;;) {
                VecArray out;
                BL_ASSERT(srcWidth < dstWidth);

                bool isLastStep = (srcWidth * 2u == dstWidth);
                uint32_t outRegCount = blMax<uint32_t>(src.size() / 2u, 1u);

                switch (srcWidth) {
                  case 4:
                    if (isLastStep)
                      out = dst.xmm();
                    else
                      pc->newV128Array(out, outRegCount, "tmp");
                    pc->v_interleave_lo_u32(out, src.even(), src.odd());
                    break;

                  case 8:
                    if (isLastStep)
                      out = dst.xmm();
                    else
                      pc->newV128Array(out, outRegCount, "tmp");
                    pc->v_interleave_lo_u64(out, src.even(), src.odd());
                    break;

                  case 16:
                    if (isLastStep)
                      out = dst.ymm();
                    else
                      pc->newV256Array(out, outRegCount, "tmp");
                    pc->v_insert_v128(out.ymm(), src.even().ymm(), src.odd().xmm(), 1);
                    break;

                  case 32:
                    BL_ASSERT(isLastStep);
                    out = dst.zmm();
                    pc->v_insert_v256(out.zmm(), src.even().zmm(), src.odd().ymm(), 1);
                    break;
                }

                srcWidth *= 2u;
                if (isLastStep)
                  break;

                src = out;
                srcWidth *= 2u;
              }
            };

            for (const Vec& v : p32) {
              if (predicatedPixel.isValid())
                v_srli_u32(v, predicatedPixel, 24);
              else
                v_srli_u32(v, src, 24);

              src.addOffset(v.size());
              if (blTestFlag(flags, PixelFlags::kPA))
                cc->vpmovdb(v.xmm(), v);
              else
                cc->vpmovdw(v.half(), v);
            }

            if (blTestFlag(flags, PixelFlags::kPA)) {
              uint32_t paRegCount = SimdWidthUtils::regCountOf(paWidth, DataWidth::k8, n);
              BL_ASSERT(paRegCount <= OpArray::kMaxSize);

              if (p32RegCount == 1) {
                p.pa.init(p32[0]);
                rename(p.pa, p.name(), "pa");
              }
              else {
                newVecArray(p.pa, paRegCount, paWidth, p.name(), "pa");
                multiVecUnpack(this, p.pa, p32, p32[0].size() / 4u);
              }
            }
            else {
              uint32_t uaRegCount = SimdWidthUtils::regCountOf(paWidth, DataWidth::k16, n);
              BL_ASSERT(uaRegCount <= OpArray::kMaxSize);

              if (p32RegCount == 1) {
                p.ua.init(p32[0]);
                rename(p.ua, p.name(), "ua");
              }
              else {
                newVecArray(p.ua, uaRegCount, uaWidth, p.name(), "ua");
                multiVecUnpack(this, p.ua, p32, p32[0].size() / 2u);
              }
            }
          }
          else
#endif // BL_JIT_ARCH_X86
          {
            // TODO:
            BL_ASSERT(false);
          }

          break;
        }

        default:
          BL_NOT_REACHED();
      }

      break;
    }

    case FormatExt::kXRGB32: {
      BL_ASSERT(predicate.empty());

      switch (n.value()) {
        case 1: {
          p.sa = newGp32("a");
          cc->mov(p.sa, 255);
          break;
        }

        default:
          BL_NOT_REACHED();
      }

      break;
    }

    case FormatExt::kA8: {
      Vec predicatedPixel;

#if defined(BL_JIT_ARCH_X86)
      if (!predicate.empty()) {
        // TODO: [JIT] Do we want to support masked loading of more that 1 register?
        BL_ASSERT(n.value() > 1);
        BL_ASSERT(regCountOf(DataWidth::k8, n) == 1);

        predicatedPixel = newVec(paWidth, p.name(), "pred");
        x_ensure_predicate_8(predicate, n.value());
        v_load_predicated_v8(predicatedPixel, predicate, src);
      }
#endif // BL_JIT_ARCH_X86

      switch (n.value()) {
        case 1: {
          p.sa = newGp32("a");
          load_u8(p.sa, src);

          break;
        }

        case 4: {
          Vec a;

          if (predicatedPixel.isValid()) {
            a = predicatedPixel;
          }
          else {
            a = newV128("a");
            src.setSize(4);
            v_loada32(a, src);
          }

          if (blTestFlag(flags, PixelFlags::kPC)) {
            p.pa.init(a);
          }
          else {
            v_cvt_u8_to_u16(a, a);
            p.ua.init(a);
          }

          break;
        }

        case 8: {
          if (predicatedPixel.isValid()) {
            Vec a = predicatedPixel;

            if (blTestFlag(flags, PixelFlags::kPA)) {
              p.pa.init(a);
            }
            else {
              v_cvt_u8_to_u16(a, a);
              p.ua.init(a);
            }
          }
          else {
            Vec a = newV128("a");
            src.setSize(8);

            if (blTestFlag(flags, PixelFlags::kPA)) {
              v_loadu64(a, src);
              p.pa.init(a);
            }
            else {
              v_loadu64_u8_to_u16(a, src);
              p.ua.init(a);
            }
          }

          break;
        }

        case 16:
        case 32:
        case 64: {
          BL_ASSERT(!predicatedPixel.isValid());

#if defined(BL_JIT_ARCH_X86)
          if (simdWidth() >= SimdWidth::k256) {
            if (blTestFlag(flags, PixelFlags::kPA)) {
              uint32_t paRegCount = SimdWidthUtils::regCountOf(paWidth, DataWidth::k8, n);
              BL_ASSERT(paRegCount <= OpArray::kMaxSize);

              newVecArray(p.pa, paRegCount, paWidth, p.name(), "pa");
              src.setSize(16u << uint32_t(paWidth));

              for (uint32_t i = 0; i < paRegCount; i++) {
                v_loadavec(p.pa[i], src, alignment);
                src.addOffsetLo32(p.pa[i].size());
              }
            }
            else {
              uint32_t uaRegCount = SimdWidthUtils::regCountOf(uaWidth, DataWidth::k16, n);
              BL_ASSERT(uaRegCount <= OpArray::kMaxSize);

              newVecArray(p.ua, uaRegCount, uaWidth, p.name(), "ua");
              src.setSize(p.ua[0].size() / 2u);

              for (uint32_t i = 0; i < uaRegCount; i++) {
                v_cvt_u8_to_u16(p.ua[i], src);
                src.addOffsetLo32(p.ua[i].size() / 2u);
              }
            }
          }
          else if (!blTestFlag(flags, PixelFlags::kPA) && hasSSE4_1()) {
            uint32_t uaRegCount = regCountOf(DataWidth::k16, n);
            BL_ASSERT(uaRegCount <= OpArray::kMaxSize);

            newV128Array(p.ua, uaRegCount, p.name(), "ua");
            src.setSize(8);

            for (uint32_t i = 0; i < uaRegCount; i++) {
              v_cvt_u8_to_u16(p.ua[i], src);
              src.addOffsetLo32(8);
            }
          }
          else
#endif // BL_JIT_ARCH_X86
          {
            uint32_t paRegCount = regCountOf(DataWidth::k8, n);
            BL_ASSERT(paRegCount <= OpArray::kMaxSize);

            newV128Array(p.pc, paRegCount, p.name(), "pc");
            src.setSize(16);

            for (uint32_t i = 0; i < paRegCount; i++) {
              v_loada128(p.pc[i], src, alignment);
              src.addOffsetLo32(16);
            }
          }

          break;
        }

        default:
          BL_NOT_REACHED();
      }

      break;
    }

    default:
      BL_NOT_REACHED();
  }

  _x_satisfy_pixel_a8(p, flags);
}

void PipeCompiler::_x_fetch_pixel_rgba32(Pixel& p, PixelCount n, PixelFlags flags, FormatExt format, const Mem& src_, Alignment alignment, PixelPredicate& predicate) noexcept  {
  BL_ASSERT(p.isRGBA32());

#if defined(BL_JIT_ARCH_A64)
  blUnused(predicate);
#endif // BL_JIT_ARCH_A64

  Mem src(src_);
  p.setCount(n);

  switch (format) {
    // RGBA32 <- PRGB32 | XRGB32.
    case FormatExt::kPRGB32:
    case FormatExt::kXRGB32: {
#if defined(BL_JIT_ARCH_X86)
      SimdWidth pcWidth = simdWidthOf(DataWidth::k32, n);
      SimdWidth ucWidth = simdWidthOf(DataWidth::k64, n);

      if (!predicate.empty()) {
        // TODO: [JIT] Do we want to support masking with more than 1 packed register?
        BL_ASSERT(regCountOf(DataWidth::k32, n) == 1);
        newVecArray(p.pc, 1, pcWidth, p.name(), "pc");

        x_ensure_predicate_32(predicate, n.value());
        v_load_predicated_v32(p.pc[0], predicate, src);
      }
      else
#endif // BL_JIT_ARCH_X86
      {
        switch (n.value()) {
          case 1: {
            newV128Array(p.pc, 1, p.name(), "pc");
            v_loada32(p.pc[0], src);

            break;
          }

          case 2: {
#if defined(BL_JIT_ARCH_X86)
            if (blTestFlag(flags, PixelFlags::kUC) && hasSSE4_1()) {
              newV128Array(p.uc, 1, p.name(), "uc");
              v_cvt_u8_to_u16(p.pc[0].xmm(), src);
            }
            else
#endif // BL_JIT_ARCH_X86
            {
              newV128Array(p.pc, 1, p.name(), "pc");
              v_loadu64(p.pc[0], src);
            }

            break;
          }

          case 4: {
#if defined(BL_JIT_ARCH_X86)
            if (!blTestFlag(flags, PixelFlags::kPC) && use256BitSimd()) {
              newV256Array(p.uc, 1, p.name(), "uc");
              v_cvt_u8_to_u16(p.uc[0].ymm(), src);
            }
            else if (!blTestFlag(flags, PixelFlags::kPC) && hasSSE4_1()) {
              newV128Array(p.uc, 2, p.name(), "uc");
              v_cvt_u8_to_u16(p.uc[0].xmm(), src);
              src.addOffsetLo32(8);
              v_cvt_u8_to_u16(p.uc[1].xmm(), src);
            }
            else
#endif // BL_JIT_ARCH_X86
            {
              newV128Array(p.pc, 1, p.name(), "pc");
              v_loada128(p.pc[0], src, alignment);
            }

            break;
          }

          case 8:
          case 16:
          case 32: {
#if defined(BL_JIT_ARCH_X86)
            if (simdWidth() >= SimdWidth::k256) {
              if (blTestFlag(flags, PixelFlags::kPC)) {
                uint32_t pcRegCount = SimdWidthUtils::regCountOf(pcWidth, DataWidth::k32, n);
                BL_ASSERT(pcRegCount <= OpArray::kMaxSize);

                newVecArray(p.pc, pcRegCount, pcWidth, p.name(), "pc");
                for (uint32_t i = 0; i < pcRegCount; i++) {
                  v_loadavec(p.pc[i], src, alignment);
                  src.addOffsetLo32(p.pc[i].size());
                }
              }
              else {
                uint32_t ucRegCount = SimdWidthUtils::regCountOf(ucWidth, DataWidth::k64, n);
                BL_ASSERT(ucRegCount <= OpArray::kMaxSize);

                newVecArray(p.uc, ucRegCount, ucWidth, p.name(), "uc");
                for (uint32_t i = 0; i < ucRegCount; i++) {
                  v_cvt_u8_to_u16(p.uc[i], src);
                  src.addOffsetLo32(p.uc[i].size() / 2u);
                }
              }
            }
            else if (!blTestFlag(flags, PixelFlags::kPC) && hasSSE4_1()) {
              uint32_t regCount = regCountOf(DataWidth::k64, n);
              BL_ASSERT(regCount <= OpArray::kMaxSize);

              newV128Array(p.uc, regCount, p.name(), "uc");
              for (uint32_t i = 0; i < regCount; i++) {
                v_cvt_u8_to_u16(p.uc[i], src);
                src.addOffsetLo32(8);
              }
            }
            else
#endif
            {
              uint32_t regCount = regCountOf(DataWidth::k32, n);
              BL_ASSERT(regCount <= OpArray::kMaxSize);

              newV128Array(p.pc, regCount, p.name(), "pc");
              v_loadavec(p.pc, src, alignment);
            }

            break;
          }

          default:
            BL_NOT_REACHED();
        }
      }

      if (format == FormatExt::kXRGB32)
        x_fill_pixel_alpha(p);

      break;
    }

    // RGBA32 <- A8.
    case FormatExt::kA8: {
      BL_ASSERT(predicate.empty());

      switch (n.value()) {
        case 1: {
          if (blTestFlag(flags, PixelFlags::kPC)) {
            newV128Array(p.pc, 1, p.name(), "pc");

#if defined(BL_JIT_ARCH_X86)
            if (!hasAVX2()) {
              Gp tmp = newGp32("tmp");
              load_u8(tmp, src);
              mul(tmp, tmp, 0x01010101u);
              s_mov_u32(p.pc[0], tmp);
            }
            else
#endif // BL_JIT_ARCH_X86
            {
              v_broadcast_u8(p.pc[0].v128(), src);
            }
          }
          else {
            newV128Array(p.uc, 1, p.name(), "uc");
#if defined(BL_JIT_ARCH_X86)
            if (!hasAVX2()) {
              v_load8(p.uc[0], src);
              v_swizzle_lo_u16x4(p.uc[0], p.uc[0], swizzle(0, 0, 0, 0));
            }
            else
#endif
            {
              v_broadcast_u8(p.uc[0], src);
              v_cvt_u8_to_u16(p.uc[0], p.uc[0]);
            }
          }

          break;
        }

        case 2: {
          if (blTestFlag(flags, PixelFlags::kPC)) {
            newV128Array(p.pc, 1, p.name(), "pc");
#if defined(BL_JIT_ARCH_X86)
            if (!hasAVX2()) {
              if (hasSSE4_1()) {
                v_loadu16_u8_to_u64(p.pc[0], src);
                v_swizzlev_u8(p.pc[0], p.pc[0], simdConst(&ct.pshufb_xxxxxxx1xxxxxxx0_to_zzzzzzzz11110000, Bcst::kNA, p.pc[0]));
              }
              else {
                Gp tmp = newGp32("tmp");
                load_u16(tmp, src);
                s_mov_u32(p.pc[0], tmp);
                v_interleave_lo_u8(p.pc[0], p.pc[0], p.pc[0]);
                v_interleave_lo_u16(p.pc[0], p.pc[0], p.pc[0]);
              }
            }
            else
#endif // BL_JIT_ARCH_X86
            {
              v_broadcast_u16(p.pc[0].v128(), src);
              v_swizzlev_u8(p.pc[0], p.pc[0], simdConst(&ct.pshufb_xxxxxxxxxxxx3210_to_3333222211110000, Bcst::kNA, p.pc[0]));
            }
          }
          else {
            // TODO: [JIT] Unfinished code.
          }

          break;
        }

        case 4: {
          if (blTestFlag(flags, PixelFlags::kPC)) {
            newV128Array(p.pc, 1, p.name(), "pc");

            v_loada32(p.pc[0], src);
#if defined(BL_JIT_ARCH_X86)
            if (!hasSSSE3()) {
              v_interleave_lo_u8(p.pc[0], p.pc[0], p.pc[0]);
              v_interleave_lo_u16(p.pc[0], p.pc[0], p.pc[0]);
            }
            else
#endif // BL_JIT_ARCH_X86
            {
              v_swizzlev_u8(p.pc[0], p.pc[0], simdConst(&ct.pshufb_xxxxxxxxxxxx3210_to_3333222211110000, Bcst::kNA, p.pc[0]));
            }
          }
          else {
#if defined(BL_JIT_ARCH_X86)
            if (use256BitSimd()) {
              newV256Array(p.uc, 1, p.name(), "uc");

              v_loadu32_u8_to_u64(p.uc, src);
              v_swizzlev_u8(p.pc[0], p.pc[0], simdConst(&ct.pshufb_xxxxxxx1xxxxxxx0_to_z1z1z1z1z0z0z0z0, Bcst::kNA, p.pc[0]));
            }
            else
#endif // BL_JIT_ARCH_X86
            {
              newV128Array(p.uc, 2, p.name(), "uc");

              v_loada32(p.uc[0], src);
              v_interleave_lo_u8(p.uc[0], p.uc[0], p.uc[0]);
              v_cvt_u8_to_u16(p.uc[0], p.uc[0]);

              v_swizzle_u32x4(p.uc[1], p.uc[0], swizzle(3, 3, 2, 2));
              v_swizzle_u32x4(p.uc[0], p.uc[0], swizzle(1, 1, 0, 0));
            }
          }

          break;
        }

        case 8:
        case 16: {
#if defined(BL_JIT_ARCH_X86)
          if (use256BitSimd()) {
            if (blTestFlag(flags, PixelFlags::kPC)) {
              uint32_t pcCount = regCountOf(DataWidth::k32, n);
              BL_ASSERT(pcCount <= OpArray::kMaxSize);

              newV256Array(p.pc, pcCount, p.name(), "pc");
              for (uint32_t i = 0; i < pcCount; i++) {
                v_cvt_u8_to_u32(p.pc[i], src);
                src.addOffsetLo32(8);
              }

              v_swizzlev_u8(p.pc, p.pc, simdConst(&ct.pshufb_xxx3xxx2xxx1xxx0_to_3333222211110000, Bcst::kNA, p.pc));
            }
            else {
              uint32_t ucCount = regCountOf(DataWidth::k64, n);
              BL_ASSERT(ucCount <= OpArray::kMaxSize);

              newV256Array(p.uc, ucCount, p.name(), "uc");
              for (uint32_t i = 0; i < ucCount; i++) {
                v_loadu32_u8_to_u64(p.uc[i], src);
                src.addOffsetLo32(4);
              }

              v_swizzlev_u8(p.uc, p.uc, simdConst(&ct.pshufb_xxxxxxx1xxxxxxx0_to_z1z1z1z1z0z0z0z0, Bcst::kNA, p.uc));
            }
          }
          else
#endif // BL_JIT_ARCH_X86
          {
            if (blTestFlag(flags, PixelFlags::kPC)) {
              uint32_t pcCount = regCountOf(DataWidth::k32, n);
              BL_ASSERT(pcCount <= OpArray::kMaxSize);

              newV128Array(p.pc, pcCount, p.name(), "pc");

              for (uint32_t i = 0; i < pcCount; i++) {
                v_loada32(p.pc[i], src);
                src.addOffsetLo32(4);
              }

#if defined(BL_JIT_ARCH_X86)
              if (!hasSSSE3()) {
                v_interleave_lo_u8(p.pc, p.pc, p.pc);
                v_interleave_lo_u16(p.pc, p.pc, p.pc);
              }
              else
#endif // BL_JIT_ARCH_X86
              {
                v_swizzlev_u8(p.uc, p.uc, simdConst(&ct.pshufb_xxx3xxx2xxx1xxx0_to_3333222211110000, Bcst::kNA, p.uc));
              }
            }
            else {
              uint32_t ucCount = regCountOf(DataWidth::k64, n);
              BL_ASSERT(ucCount == 4);

              newV128Array(p.uc, ucCount, p.name(), "uc");

              v_loada32(p.uc[0], src);
              src.addOffsetLo32(4);
              v_loada32(p.uc[2], src);

              v_interleave_lo_u8(p.uc[0], p.uc[0], p.uc[0]);
              v_interleave_lo_u8(p.uc[2], p.uc[2], p.uc[2]);

              v_cvt_u8_to_u16(p.uc[0], p.uc[0]);
              v_cvt_u8_to_u16(p.uc[2], p.uc[2]);

              v_swizzle_u32x4(p.uc[1], p.uc[0], swizzle(3, 3, 2, 2));
              v_swizzle_u32x4(p.uc[3], p.uc[2], swizzle(3, 3, 2, 2));
              v_swizzle_u32x4(p.uc[0], p.uc[0], swizzle(1, 1, 0, 0));
              v_swizzle_u32x4(p.uc[2], p.uc[2], swizzle(1, 1, 0, 0));
            }
          }

          break;
        }

        default:
          BL_NOT_REACHED();
      }

      break;
    }

    // RGBA32 <- Unknown?
    default:
      BL_NOT_REACHED();
  }

  _x_satisfy_pixel_rgba32(p, flags);
}

void PipeCompiler::x_satisfy_pixel(Pixel& p, PixelFlags flags) noexcept {
  BL_ASSERT(p.count() != 0);

  switch (p.type()) {
    case PixelType::kA8:
      _x_satisfy_pixel_a8(p, flags);
      break;

    case PixelType::kRGBA32:
      _x_satisfy_pixel_rgba32(p, flags);
      break;

    default:
      BL_NOT_REACHED();
  }
}

void PipeCompiler::_x_satisfy_pixel_a8(Pixel& p, PixelFlags flags) noexcept {
  BL_ASSERT(p.type() == PixelType::kA8);
  BL_ASSERT(p.count() != 0);

  // Scalar mode uses only SA.
  if (p.count() == 1) {
    BL_ASSERT( blTestFlag(flags, PixelFlags::kSA));
    BL_ASSERT(!blTestFlag(flags, PixelFlags::kPA | PixelFlags::kUA));

    return;
  }

  if (blTestFlag(flags, PixelFlags::kPA) && p.pa.empty()) {
    // Either PA or UA, but never both.
    BL_ASSERT(!blTestFlag(flags, PixelFlags::kUA));

    _x_pack_pixel(p.pa, p.ua, p.count().value(), p.name(), "pa");
  }
  else if (blTestFlag(flags, PixelFlags::kUA) && p.ua.empty()) {
    // Either PA or UA, but never both.
    BL_ASSERT(!blTestFlag(flags, PixelFlags::kPA));

    _x_unpack_pixel(p.ua, p.pa, p.count().value(), p.name(), "ua");
  }

  if (blTestFlag(flags, PixelFlags::kUA | PixelFlags::kUI)) {
    if (p.ua.empty()) {
      // TODO: A8 pipeline - finalize satisfy-pixel.
      BL_ASSERT(false);
    }
  }
}

void PipeCompiler::_x_satisfy_pixel_rgba32(Pixel& p, PixelFlags flags) noexcept {
  BL_ASSERT(p.type() == PixelType::kRGBA32);
  BL_ASSERT(p.count() != 0);

  // Quick reject if all flags were satisfied already or no flags were given.
  if ((!blTestFlag(flags, PixelFlags::kPC) || !p.pc.empty()) &&
      (!blTestFlag(flags, PixelFlags::kUC) || !p.uc.empty()) &&
      (!blTestFlag(flags, PixelFlags::kUA) || !p.ua.empty()) &&
      (!blTestFlag(flags, PixelFlags::kUI) || !p.ui.empty()))
    return;

  // Only fetch unpacked alpha if we already have unpacked pixels. Wait otherwise as fetch flags may contain
  // `PixelFlags::kUC`, which is handled below. This is an optimization for cases in which the caller wants
  // packed RGBA and unpacked alpha.
  if (blTestFlag(flags, PixelFlags::kUA | PixelFlags::kUI) && p.ua.empty() && !p.uc.empty()) {
    // Emit pshuflw/pshufhw sequence for every unpacked pixel.
    newVecArray(p.ua, p.uc.size(), p.uc[0], p.name(), "ua");

#if defined(BL_JIT_ARCH_X86)
    if (!hasAVX()) {
      v_expand_alpha_16(p.ua, p.uc, true);
    }
    else
#endif // BL_JIT_ARCH_X86
    {
      v_swizzlev_u8(p.ua, p.uc, simdConst(&ct.pshufb_32xxxxxx10xxxxxx_to_3232323210101010, Bcst::kNA, p.ua));
    }
  }

  // Pack or unpack sequence.
  //
  // The following code handles packing or unpacking pixels. Typically, depending on a fetcher, either
  // packed or unpacked pixels are assigned to a `Pixel`. Then, the consumer of that pixel decides which
  // format to use. So, if there is a mismatch, we have to emit a pack/unpack sequence. Unpacked pixels
  // are needed for almost everything except some special cases like SRC_COPY and PLUS without a mask.

  // Either PC or UC, but never both.
  BL_ASSERT((flags & (PixelFlags::kPC | PixelFlags::kUC)) != (PixelFlags::kPC | PixelFlags::kUC));

  if (blTestFlag(flags, PixelFlags::kPC) && p.pc.empty()) {
    _x_pack_pixel(p.pc, p.uc, p.count().value() * 4u, p.name(), "pc");
  }
  else if (blTestFlag(flags, PixelFlags::kUC) && p.uc.empty()) {
    _x_unpack_pixel(p.uc, p.pc, p.count().value() * 4, p.name(), "uc");
  }

  // Unpack alpha from either packed or unpacked pixels.
  if (blTestFlag(flags, PixelFlags::kUA | PixelFlags::kUI) && p.ua.empty()) {
    // This time we have to really fetch A8/IA8, if we haven't before.
    BL_ASSERT(!p.pc.empty() || !p.uc.empty());

    uint32_t uaCount = regCountOf(DataWidth::k64, p.count());
    BL_ASSERT(uaCount <= OpArray::kMaxSize);

    if (!p.uc.empty()) {
      newVecArray(p.ua, uaCount, p.uc[0], p.name(), "ua");
#if defined(BL_JIT_ARCH_X86)
      if (!hasAVX()) {
        v_expand_alpha_16(p.ua, p.uc, p.count() > 1);
      }
      else
#endif // BL_JIT_ARCH_X86
      {
        v_swizzlev_u8(p.ua, p.uc, simdConst(&ct.pshufb_32xxxxxx10xxxxxx_to_3232323210101010, Bcst::kNA, p.ua));
      }
    }
    else {
      if (p.count() <= 2) {
        newV128Array(p.ua, uaCount, p.name(), "ua");
#if defined(BL_JIT_ARCH_X86)
        if (hasAVX() || p.count() == 2u) {
          v_swizzlev_u8(p.ua[0], p.pc[0], simdConst(&ct.pshufb_xxxxxxxx1xxx0xxx_to_z1z1z1z1z0z0z0z0, Bcst::kNA, p.ua[0]));
        }
        else {
          // TODO: Obviously wrong!
          v_swizzle_lo_u16x4(p.ua[0], p.pc[0], swizzle(1, 1, 1, 1));
          v_srli_u16(p.ua[0], p.ua[0], 8);
        }
#else
        v_swizzlev_u8(p.ua[0], p.pc[0], simdConst(&ct.pshufb_xxxxxxxx1xxx0xxx_to_z1z1z1z1z0z0z0z0, Bcst::kNA, p.ua[0]));
#endif
      }
      else {
        SimdWidth ucWidth = simdWidthOf(DataWidth::k64, p.count());
        newVecArray(p.ua, uaCount, ucWidth, p.name(), "ua");

#if defined(BL_JIT_ARCH_X86)
        if (ucWidth == SimdWidth::k512) {
          if (uaCount == 1) {
            v_cvt_u8_to_u16(p.ua[0], p.pc[0].ymm());
          }
          else {
            v_extract_v256(p.ua.odd().ymm(), p.pc.zmm(), 1);
            v_cvt_u8_to_u16(p.ua.even(), p.pc.ymm());
            v_cvt_u8_to_u16(p.ua.odd(), p.ua.odd().ymm());
          }

          v_swizzlev_u8(p.ua, p.ua, simdConst(&ct.pshufb_32xxxxxx10xxxxxx_to_3232323210101010, Bcst::kNA, p.ua));
        }
        else if (ucWidth == SimdWidth::k256) {
          if (uaCount == 1) {
            v_cvt_u8_to_u16(p.ua[0], p.pc[0].xmm());
          }
          else {
            v_extract_v128(p.ua.odd().xmm(), p.pc.ymm(), 1);
            v_cvt_u8_to_u16(p.ua.even(), p.pc.xmm());
            v_cvt_u8_to_u16(p.ua.odd(), p.ua.odd().xmm());
          }

          v_swizzlev_u8(p.ua, p.ua, simdConst(&ct.pshufb_32xxxxxx10xxxxxx_to_3232323210101010, Bcst::kNA, p.ua));
        }
        else
#endif // BL_JIT_ARCH_X86
        {
          for (uint32_t i = 0; i < p.pc.size(); i++)
            xExtractUnpackedAFromPackedARGB32_4(p.ua[i * 2], p.ua[i * 2 + 1], p.pc[i]);
        }
      }
    }
  }

  if (blTestFlag(flags, PixelFlags::kUI) && p.ui.empty()) {
    if (hasNonDestructiveDst() || blTestFlag(flags, PixelFlags::kUA)) {
      newVecArray(p.ui, p.ua.size(), p.ua[0], p.name(), "ui");
      v_inv255_u16(p.ui, p.ua);
    }
    else {
      p.ui.init(p.ua);
      v_inv255_u16(p.ui, p.ua);

      p.ua.reset();
      rename(p.ui, p.name(), "ui");
    }
  }
}

void PipeCompiler::x_satisfy_solid(Pixel& p, PixelFlags flags) noexcept {
  BL_ASSERT(p.count() != 0);

  switch (p.type()) {
    case PixelType::kA8:
      _x_satisfy_solid_a8(p, flags);
      break;

    case PixelType::kRGBA32:
      _x_satisfy_solid_rgba32(p, flags);
      break;

    default:
      BL_NOT_REACHED();
  }
}

void PipeCompiler::_x_satisfy_solid_a8(Pixel& p, PixelFlags flags) noexcept {
  BL_ASSERT(p.type() == PixelType::kA8);
  BL_ASSERT(p.count() != 0);

  if (blTestFlag(flags, PixelFlags::kPA) && p.pa.empty()) {
    BL_ASSERT(!p.ua.empty());
    newVecArray(p.pa, 1, p.name(), "pa");
    v_packs_i16_u8(p.pa[0], p.ua[0], p.ua[0]);
  }

  // TODO: A8 pipeline - finalize solid-alpha.
}

void PipeCompiler::_x_satisfy_solid_rgba32(Pixel& p, PixelFlags flags) noexcept {
  BL_ASSERT(p.type() == PixelType::kRGBA32);
  BL_ASSERT(p.count() != 0);

  if (blTestFlag(flags, PixelFlags::kPC) && p.pc.empty()) {
    BL_ASSERT(!p.uc.empty());

    newVecArray(p.pc, 1, p.name(), "pc");
    v_mov(p.pc[0], p.uc[0]);
    v_packs_i16_u8(p.pc[0], p.pc[0], p.pc[0]);
  }

  if (blTestFlag(flags, PixelFlags::kUC) && p.uc.empty()) {
    BL_ASSERT(!p.pc.empty());

    newVecArray(p.uc, 1, p.name(), "uc");
    v_cvt_u8_to_u16(p.uc[0], p.pc[0]);
  }

  if (blTestFlag(flags, PixelFlags::kUA) && p.ua.empty()) {
    newVecArray(p.ua, 1, p.name(), "ua");

    if (!p.uc.empty()) {
      v_swizzle_lo_u16x4(p.ua[0], p.uc[0], swizzle(3, 3, 3, 3));
      v_swizzle_u32x4(p.ua[0], p.ua[0], swizzle(1, 0, 1, 0));
    }
    else {
      v_swizzle_lo_u16x4(p.ua[0], p.pc[0], swizzle(1, 1, 1, 1));
      v_swizzle_u32x4(p.ua[0], p.ua[0], swizzle(1, 0, 1, 0));
      v_srli_u16(p.ua[0], p.ua[0], 8);
    }
  }

  if (blTestFlag(flags, PixelFlags::kUI) && p.ui.empty()) {
    newVecArray(p.ui, 1, p.name(), "ui");

    if (!p.ua.empty()) {
      v_mov(p.ui[0], p.ua[0]);
    }
    else if (!p.uc.empty()) {
      v_swizzle_lo_u16x4(p.ui[0], p.uc[0], swizzle(3, 3, 3, 3));
      v_swizzle_u32x4(p.ui[0], p.ui[0], swizzle(1, 0, 1, 0));
    }
    else {
      v_swizzle_lo_u16x4(p.ui[0], p.pc[0], swizzle(1, 1, 1, 1));
      v_swizzle_u32x4(p.ui[0], p.ui[0], swizzle(1, 0, 1, 0));
      v_srli_u16(p.ui[0], p.ui[0], 8);
    }

    v_inv255_u16(p.ui[0], p.ui[0]);
  }
}

// Emits a pixel packing sequence.
void PipeCompiler::_x_pack_pixel(VecArray& px, VecArray& ux, uint32_t n, const char* prefix, const char* pxName) noexcept {
  BL_ASSERT( px.empty());
  BL_ASSERT(!ux.empty());

#if defined(BL_JIT_ARCH_X86)
  if (hasAVX512() && ux[0].type() >= asmjit::RegType::kX86_Ymm) {
    SimdWidth pxWidth = simdWidthOf(DataWidth::k8, n);
    uint32_t pxCount = regCountOf(DataWidth::k8, n);
    BL_ASSERT(pxCount <= OpArray::kMaxSize);

    newVecArray(px, pxCount, pxWidth, prefix, pxName);

    if (ux.size() == 1) {
      // Pack ZMM->YMM or YMM->XMM.
      BL_ASSERT(pxCount == 1);
      cc->vpmovwb(px[0], ux[0]);
      ux.reset();
      return;
    }
    else if (ux[0].type() >= asmjit::RegType::kX86_Zmm) {
      // Pack ZMM to ZMM.
      VecArray pxTmp;
      newV256Array(pxTmp, ux.size(), prefix, "pxTmp");

      for (uint32_t i = 0; i < ux.size(); i++)
        cc->vpmovwb(pxTmp[i].ymm(), ux[i]);

      for (uint32_t i = 0; i < ux.size(); i += 2)
        cc->vinserti32x8(px[i / 2u].zmm(), pxTmp[i].zmm(), pxTmp[i + 1u].ymm(), 1);

      ux.reset();
      return;
    }
  }

  if (hasAVX()) {
    uint32_t pxCount = regCountOf(DataWidth::k8, n);
    BL_ASSERT(pxCount <= OpArray::kMaxSize);

    if (ux[0].type() >= asmjit::RegType::kX86_Ymm) {
      if (ux.size() == 1) {
        // Pack YMM to XMM.
        BL_ASSERT(pxCount == 1);

        Vec pTmp = newV256("pTmp");
        newV128Array(px, pxCount, prefix, pxName);

        v_packs_i16_u8(pTmp, ux[0], ux[0]);
        v_swizzle_u64x4(px[0].ymm(), pTmp, swizzle(3, 1, 2, 0));
      }
      else {
        newV256Array(px, pxCount, prefix, pxName);
        v_packs_i16_u8(px, ux.even(), ux.odd());
        v_swizzle_u64x4(px, px, swizzle(3, 1, 2, 0));
      }
    }
    else {
      newV128Array(px, pxCount, prefix, pxName);
      v_packs_i16_u8(px, ux.even(), ux.odd());
    }
    ux.reset();
  }
  else {
    // NOTE: This is only used by a non-AVX pipeline. Renaming makes no sense when in AVX mode. Additionally,
    // we may need to pack to XMM register from two YMM registers, so the register types don't have to match
    // if the pipeline is using 256-bit SIMD or higher.
    px.init(ux.even());
    rename(px, prefix, pxName);

    v_packs_i16_u8(px, ux.even(), ux.odd());
    ux.reset();
  }
#else
  uint32_t pxCount = regCountOf(DataWidth::k8, n);
  BL_ASSERT(pxCount <= OpArray::kMaxSize);

  newV128Array(px, pxCount, prefix, pxName);
  v_packs_i16_u8(px, ux.even(), ux.odd());

  ux.reset();
#endif
}

// Emits a pixel unpacking sequence.
void PipeCompiler::_x_unpack_pixel(VecArray& ux, VecArray& px, uint32_t n, const char* prefix, const char* uxName) noexcept {
  BL_ASSERT( ux.empty());
  BL_ASSERT(!px.empty());

#if defined(BL_JIT_ARCH_X86)
  SimdWidth uxWidth = simdWidthOf(DataWidth::k16, n);
  uint32_t uxCount = regCountOf(DataWidth::k16, n);
  BL_ASSERT(uxCount <= OpArray::kMaxSize);

  if (hasAVX()) {
    newVecArray(ux, uxCount, uxWidth, prefix, uxName);

    if (uxWidth == SimdWidth::k512) {
      if (uxCount == 1) {
        v_cvt_u8_to_u16(ux[0], px[0].ymm());
      }
      else {
        v_extract_v256(ux.odd().ymm(), px, 1);
        v_cvt_u8_to_u16(ux.even(), px.ymm());
        v_cvt_u8_to_u16(ux.odd(), ux.odd().ymm());
      }
    }
    else if (uxWidth == SimdWidth::k256 && n >= 16) {
      if (uxCount == 1) {
        v_cvt_u8_to_u16(ux[0], px[0].xmm());
      }
      else {
        v_extract_v128(ux.odd().xmm(), px, 1);
        v_cvt_u8_to_u16(ux.even(), px.xmm());
        v_cvt_u8_to_u16(ux.odd(), ux.odd().xmm());
      }
    }
    else {
      for (uint32_t i = 0; i < uxCount; i++) {
        if (i & 1)
          v_swizzlev_u8(ux[i], px[i / 2u], simdConst(&commonTable.pshufb_76543210xxxxxxxx_to_z7z6z5z4z3z2z1z0, Bcst::kNA, ux[i]));
        else
          v_cvt_u8_to_u16(ux[i], px[i / 2u]);
      }
    }
  }
  else {
    if (n <= 8) {
      ux.init(px[0]);
      v_cvt_u8_to_u16(ux[0], ux[0]);
    }
    else {
      ux._size = px.size() * 2;
      for (uint32_t i = 0; i < px.size(); i++) {
        ux[i * 2 + 0] = px[i];
        ux[i * 2 + 1] = newV128();
        xMovzxBW_LoHi(ux[i * 2 + 0], ux[i * 2 + 1], ux[i * 2 + 0]);
      }
    }

    px.reset();
    rename(ux, prefix, uxName);
  }
#else
  uint32_t count = regCountOf(DataWidth::k16, n);
  BL_ASSERT(count <= OpArray::kMaxSize);

  newVecArray(ux, count, SimdWidth::k128, prefix, uxName);

  for (uint32_t i = 0; i < count; i++) {
    if (i & 1)
      v_swizzlev_u8(ux[i], px[i / 2u], simdConst(&commonTable.pshufb_76543210xxxxxxxx_to_z7z6z5z4z3z2z1z0, Bcst::kNA, ux[i]));
    else
      v_cvt_u8_to_u16(ux[i], px[i / 2u]);
  }
#endif
}

void PipeCompiler::x_fetch_unpacked_a8_2x(const Vec& dst, FormatExt format, const Mem& src1, const Mem& src0) noexcept {
#if defined(BL_JIT_ARCH_X86)
  Mem m0 = src0;
  Mem m1 = src1;

  if (format == FormatExt::kPRGB32) {
    m0.addOffset(3);
    m1.addOffset(3);
  }

  if (hasSSE4_1()) {
    v_load8(dst, m0);
    v_insert_u8(dst, m1, 2);
  }
  else {
    Gp aGp = newGp32("aGp");
    load_u8(aGp, m1);
    shl(aGp, aGp, 16);
    load_merge_u8(aGp, m0);
    s_mov_u32(dst, aGp);
  }
#else
  blUnused(dst, format, src1, src0);
  UNIMPLEMENTED();
#endif
}

void PipeCompiler::x_assign_unpacked_alpha_values(Pixel& p, PixelFlags flags, const Vec& vec) noexcept {
  blUnused(flags);

  BL_ASSERT(p.type() != PixelType::kNone);
  BL_ASSERT(p.count() != 0);

  Vec v0 = vec;

  if (p.isRGBA32()) {
    switch (p.count().value()) {
      case 1: {
        v_swizzle_lo_u16x4(v0, v0, swizzle(0, 0, 0, 0));

        p.uc.init(v0);
        break;
      }

      case 2: {
        v_interleave_lo_u16(v0, v0, v0);
        v_swizzle_u32x4(v0, v0, swizzle(1, 1, 0, 0));

        p.uc.init(v0);
        break;
      }

      case 4: {
        Vec v1 = newV128("@v1");

        v_interleave_lo_u16(v0, v0, v0);
        v_swizzle_u32x4(v1, v0, swizzle(3, 3, 2, 2));
        v_swizzle_u32x4(v0, v0, swizzle(1, 1, 0, 0));

        p.uc.init(v0, v1);
        break;
      }

      case 8: {
        Vec v1 = newV128("@v1");
        Vec v2 = newV128("@v2");
        Vec v3 = newV128("@v3");

        v_interleave_hi_u16(v2, v0, v0);
        v_interleave_lo_u16(v0, v0, v0);

        v_swizzle_u32x4(v1, v0, swizzle(3, 3, 2, 2));
        v_swizzle_u32x4(v0, v0, swizzle(1, 1, 0, 0));
        v_swizzle_u32x4(v3, v2, swizzle(3, 3, 2, 2));
        v_swizzle_u32x4(v2, v2, swizzle(1, 1, 0, 0));

        p.uc.init(v0, v1, v2, v3);
        break;
      }

      default:
        BL_NOT_REACHED();
    }

    rename(p.uc, "uc");
  }
  else {
    switch (p.count().value()) {
      case 1: {
        BL_ASSERT(blTestFlag(flags, PixelFlags::kSA));

        Gp sa = newGp32("sa");
        s_extract_u16(sa, vec, 0);

        p.sa = sa;
        break;
      }

      default: {
        p.ua.init(vec);
        rename(p.ua, p.name(), "ua");
        break;
      }
    }
  }
}

void PipeCompiler::x_fill_pixel_alpha(Pixel& p) noexcept {
  switch (p.type()) {
    case PixelType::kRGBA32:
      if (!p.pc.empty()) vFillAlpha255B(p.pc, p.pc);
      if (!p.uc.empty()) vFillAlpha255W(p.uc, p.uc);
      break;

    case PixelType::kA8:
      break;

    default:
      BL_NOT_REACHED();
  }
}

void PipeCompiler::x_store_pixel_advance(const Gp& dPtr, Pixel& p, PixelCount n, uint32_t bpp, Alignment alignment, PixelPredicate& predicate) noexcept {
  Mem dMem = mem_ptr(dPtr);

  // Not used by
  blUnused(predicate);

  switch (bpp) {
    case 1: {
#if defined(BL_JIT_ARCH_X86)
      if (!predicate.empty()) {
        // Predicated pixel count must be greater than 1!
        BL_ASSERT(n != 1);

        x_satisfy_pixel(p, PixelFlags::kPA | PixelFlags::kImmutable);

        x_ensure_predicate_8(predicate, n.value());
        v_store_predicated_v8(dMem, predicate, p.pa[0]);
        add(dPtr, dPtr, predicate.count.cloneAs(dPtr));
      }
      else
#endif // BL_JIT_ARCH_X86
      {
        if (n == 1) {
          x_satisfy_pixel(p, PixelFlags::kSA | PixelFlags::kImmutable);
          store_u8(dMem, p.sa);
        }
        else {
          x_satisfy_pixel(p, PixelFlags::kPA | PixelFlags::kImmutable);

          if (n <= 16) {
            v_store_iany(dMem, p.pa[0], n.value(), alignment);
          }
          else {
            x_satisfy_pixel(p, PixelFlags::kPA | PixelFlags::kImmutable);

            // TODO: AArch64 - Use v_storeavec with multiple Vec registers to take advantage of STP where possible.
            uint32_t pcIndex = 0;
            uint32_t vecSize = p.pa[0].size();
            uint32_t pixelsPerReg = vecSize;

            for (uint32_t i = 0; i < n.value(); i += pixelsPerReg) {
              v_storeavec(dMem, p.pa[pcIndex], alignment);
              if (++pcIndex >= p.pa.size())
                pcIndex = 0;
              dMem.addOffset(vecSize);
            }
          }
        }

        add(dPtr, dPtr, n.value());
      }

      break;
    }

    case 4: {
#if defined(BL_JIT_ARCH_X86)
      if (!predicate.empty()) {
        x_satisfy_pixel(p, PixelFlags::kPC | PixelFlags::kImmutable);

        if (hasOptFlag(PipeOptFlags::kFastStoreWithMask)) {
          x_ensure_predicate_32(predicate, n.value());
          v_store_predicated_v32(dMem, predicate, p.pc[0]);
          add_scaled(dPtr, predicate.count.cloneAs(dPtr), bpp);
        }
        else {
          Label L_StoreSkip1 = newLabel();

          const Gp& count = predicate.count;
          const Vec& pc0 = p.pc[0];

          if (n > 8) {
            Label L_StoreSkip8 = newLabel();
            Vec pc0YmmHigh = newV256("pc0.ymmHigh");

            v_extract_v256(pc0YmmHigh, pc0.zmm(), 1);
            j(L_StoreSkip8, bt_z(count, 3));
            v_storeu256(dMem, pc0.ymm());
            v_mov(pc0.ymm(), pc0YmmHigh);
            add(dPtr, dPtr, 8u * 4u);
            bind(L_StoreSkip8);
          }

          if (n > 4) {
            Label L_StoreSkip4 = newLabel();
            Vec pc0XmmHigh = newV128("pc0.xmmHigh");

            v_extract_v128(pc0XmmHigh, pc0.ymm(), 1);
            j(L_StoreSkip4, bt_z(count, 2));
            v_storeu128(dMem, pc0.xmm());
            v_mov(pc0.xmm(), pc0XmmHigh);
            add(dPtr, dPtr, 4u * 4u);
            bind(L_StoreSkip4);
          }

          if (n > 2) {
            Label L_StoreSkip2 = newLabel();

            j(L_StoreSkip2, bt_z(count, 1));
            v_storeu64(dMem, pc0.xmm());
            v_srlb_u128(pc0.xmm(), pc0.xmm(), 8);
            add(dPtr, dPtr, 2u * 4u);
            bind(L_StoreSkip2);
          }

          j(L_StoreSkip1, bt_z(count, 0));
          v_storea32(dMem, pc0.xmm());
          add(dPtr, dPtr, 1u * 4u);
          bind(L_StoreSkip1);
        }
      }
      else if (hasAVX512() && n >= 2 && !p.uc.empty()) {
        uint32_t ucIndex = 0;
        uint32_t vecSize = p.uc[0].size();
        uint32_t pixelsPerReg = vecSize / 8u;

        for (uint32_t i = 0; i < n.value(); i += pixelsPerReg) {
          cc->vpmovwb(dMem, p.uc[ucIndex]);
          if (++ucIndex >= p.uc.size())
            ucIndex = 0;
          dMem.addOffset(vecSize / 2u);
        }
        add(dPtr, dPtr, n.value() * 4);
      }
      else
#endif
      {
        x_satisfy_pixel(p, PixelFlags::kPC | PixelFlags::kImmutable);

        if (n <= 4) {
          v_store_iany(dMem, p.pc[0], n.value() * 4u, alignment);
        }
        else {
          // TODO: AArch64 - Use v_storeavec with multiple Vec registers to take advantage of STP where possible.
          uint32_t pcIndex = 0;
          uint32_t vecSize = p.pc[0].size();
          uint32_t pixelsPerReg = vecSize / 4u;

          for (uint32_t i = 0; i < n.value(); i += pixelsPerReg) {
            v_storeavec(dMem, p.pc[pcIndex], alignment);
            if (++pcIndex >= p.pc.size())
              pcIndex = 0;
            dMem.addOffset(vecSize);
          }
        }
        add(dPtr, dPtr, n.value() * 4);
      }

      break;
    }

    default:
      BL_NOT_REACHED();
  }
}

} // {JIT}
} // {Pipeline}
} // {bl}

#endif // !BL_BUILD_NO_JIT