// This file is part of Blend2D project <https://blend2d.com>
//
// See blend2d.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../../api-build_p.h"
#if !defined(BL_BUILD_NO_JIT)

#include "../../pipeline/jit/compoppart_p.h"
#include "../../pipeline/jit/fetchgradientpart_p.h"
#include "../../pipeline/jit/fetchutils_p.h"
#include "../../pipeline/jit/pipecompiler_p.h"

namespace bl {
namespace Pipeline {
namespace JIT {

#define REL_GRADIENT(FIELD) BL_OFFSET_OF(FetchData::Gradient, FIELD)

// bl::Pipeline::JIT::GradientDitheringContext
// ===========================================

void GradientDitheringContext::initY(const PipeFunction& fn, const Gp& x, const Gp& y) noexcept {
  _dmPosition = pc->newGp32("dm.position");
  _dmOriginX = pc->newGp32("dm.originX");
  _dmValues = pc->newVec(pc->simdWidth(), "dm.values");

  _isRectFill = x.isValid();

  pc->load_u32(_dmPosition, mem_ptr(fn.ctxData(), BL_OFFSET_OF(ContextData, pixelOrigin.y)));
  pc->load_u32(_dmOriginX, mem_ptr(fn.ctxData(), BL_OFFSET_OF(ContextData, pixelOrigin.x)));

  pc->add(_dmPosition, _dmPosition, y.r32());
  if (isRectFill())
    pc->add(_dmOriginX, _dmOriginX, x.r32());

  pc->and_(_dmPosition, _dmPosition, 15);
  if (isRectFill())
    pc->and_(_dmOriginX, _dmOriginX, 15);

  pc->shl(_dmPosition, _dmPosition, 5);
  if (isRectFill())
    pc->add(_dmPosition, _dmPosition, _dmOriginX);
}

void GradientDitheringContext::advanceY() noexcept {
  pc->add(_dmPosition, _dmPosition, 16 * 2);
  pc->and_(_dmPosition, _dmPosition, 16 * 16 * 2 - 1);
}

void GradientDitheringContext::startAtX(const Gp& x) noexcept {
  Gp dmPosition = _dmPosition;

  if (!isRectFill()) {
    // If not rectangular, we have to calculate the final position according to `x`.
    dmPosition = pc->newGp32("dm.finalPosition");

    pc->mov(dmPosition, _dmOriginX);
    pc->add(dmPosition, dmPosition, x.r32());
    pc->and_(dmPosition, dmPosition, 15);
    pc->add(dmPosition, dmPosition, _dmPosition);
  }

  Mem m;
#if defined BL_JIT_ARCH_X86
  if (pc->is32Bit()) {
    m = x86::ptr(uint64_t(uintptr_t(commonTable.bayerMatrix16x16)), dmPosition);
  }
  else {
    pc->_initCommonTablePtr();
    m = mem_ptr(pc->_commonTablePtr, dmPosition.r64(), 0, -pc->_commonTableOff);
  }
#else
  pc->_initCommonTablePtr();
  Gp ditherRow = pc->newGpPtr("@ditherRow");
  pc->add(ditherRow, pc->_commonTablePtr, -pc->_commonTableOff);
  m = mem_ptr(ditherRow, dmPosition.r64());
#endif

  if (_dmValues.isVec128())
    pc->v_loadu128(_dmValues, m);
  else
    pc->v_broadcast_v128_u32(_dmValues, m);
}

void GradientDitheringContext::advanceX(const Gp& x, const Gp& diff) noexcept {
  // FillRect never advance X as that would mean that there is a hole, which is impossible.
  BL_ASSERT(!isRectFill());

  blUnused(diff);
  startAtX(x);
}

void GradientDitheringContext::advanceXAfterFetch(uint32_t n) noexcept {
  // The compiler would optimize this to a cheap shuffle whenever possible.
  pc->v_alignr_u128(_dmValues, _dmValues, _dmValues, n & 15);
}

void GradientDitheringContext::ditherUnpackedPixels(Pixel& p) noexcept {
  SimdWidth simdWidth = SimdWidthUtils::simdWidthOf(p.uc[0]);

  Operand shufflePredicate = pc->simdConst(&commonTable.pshufb_dither_rgba64_lo, Bcst::kNA_Unique, simdWidth);
  Vec ditherPredicate = pc->newSimilarReg(p.uc[0], "ditherPredicate");
  Vec ditherThreshold = pc->newSimilarReg(p.uc[0], "ditherThreshold");

  switch (p.count().value()) {
    case 1: {
#if defined(BL_JIT_ARCH_X86)
      if (!pc->hasSSSE3()) {
        pc->v_interleave_lo_u8(ditherPredicate, _dmValues, pc->simdConst(&commonTable.i_0000000000000000, Bcst::kNA, ditherPredicate));
        pc->v_swizzle_lo_u16x4(ditherPredicate, ditherPredicate, swizzle(0, 0, 0, 0));
      }
      else
#endif // BL_JIT_ARCH_X86
      {
        pc->v_swizzlev_u8(ditherPredicate, _dmValues.cloneAs(ditherPredicate), shufflePredicate);
      }

      pc->v_swizzle_lo_u16x4(ditherThreshold, p.uc[0], swizzle(3, 3, 3, 3));
      pc->v_adds_u16(p.uc[0], p.uc[0], ditherPredicate);
      pc->v_min_u16(p.uc[0], p.uc[0], ditherThreshold);
      pc->v_srli_u16(p.uc[0], p.uc[0], 8);
      advanceXAfterFetch(1);
      break;
    }

    case 4:
    case 8:
    case 16: {
#if defined(BL_JIT_ARCH_X86)
      if (!p.uc[0].isXmm()) {
        for (uint32_t i = 0; i < p.uc.size(); i++) {
          // At least AVX2: VPSHUFB is available...
          pc->v_swizzlev_u8(ditherPredicate, _dmValues.cloneAs(ditherPredicate), shufflePredicate);
          pc->v_expand_alpha_16(ditherThreshold, p.uc[i]);
          pc->v_adds_u16(p.uc[i], p.uc[i], ditherPredicate);
          pc->v_min_u16(p.uc[i], p.uc[i], ditherThreshold);

          if (p.uc[0].isYmm())
            pc->v_swizzle_u32x4(_dmValues, _dmValues, swizzle(0, 3, 2, 1));
          else
            pc->v_swizzle_u32x4(_dmValues, _dmValues, swizzle(1, 0, 3, 2));
        }
        pc->v_srli_u16(p.uc, p.uc, 8);
      }
      else
#endif // BL_JIT_ARCH_X86
      {
        for (uint32_t i = 0; i < p.uc.size(); i++) {
          Vec dm = (i == 0) ? _dmValues.cloneAs(ditherPredicate) : ditherPredicate;

#if defined(BL_JIT_ARCH_X86)
          if (!pc->hasSSSE3()) {
            pc->v_interleave_lo_u8(ditherPredicate, dm, pc->simdConst(&commonTable.i_0000000000000000, Bcst::kNA, ditherPredicate));
            pc->v_interleave_lo_u16(ditherPredicate, ditherPredicate, ditherPredicate);
            pc->v_swizzle_u32x4(ditherPredicate, ditherPredicate, swizzle(1, 1, 0, 0));
          }
          else
#endif // BL_JIT_ARCH_X86
          {
            pc->v_swizzlev_u8(ditherPredicate, dm, shufflePredicate);
          }

          pc->v_expand_alpha_16(ditherThreshold, p.uc[i]);
          pc->v_adds_u16(p.uc[i], p.uc[i], ditherPredicate);

          if (i + 1u < p.uc.size())
            pc->v_swizzle_lo_u16x4(ditherPredicate, _dmValues.cloneAs(ditherPredicate), swizzle(0, 3, 2, 1));

          pc->v_min_u16(p.uc[i], p.uc[i], ditherThreshold);
        }

        if (p.count().value() == 4)
          pc->v_swizzle_u32x4(_dmValues, _dmValues, swizzle(0, 3, 2, 1));
        else
          pc->v_swizzle_u32x4(_dmValues, _dmValues, swizzle(1, 0, 3, 2));

        pc->v_srli_u16(p.uc, p.uc, 8);
      }
      break;
    }

    default:
      BL_NOT_REACHED();
  }
}

// bl::Pipeline::JIT::FetchGradientPart - Construction & Destruction
// =================================================================

FetchGradientPart::FetchGradientPart(PipeCompiler* pc, FetchType fetchType, FormatExt format) noexcept
  : FetchPart(pc, fetchType, format),
    _ditheringContext(pc) {

  _partFlags |= PipePartFlags::kAdvanceXNeedsDiff;
}

void FetchGradientPart::fetchSinglePixel(Pixel& dst, PixelFlags flags, const Gp& idx) noexcept {
  Mem src = mem_ptr(_tablePtr, idx, tablePtrShift());
  if (ditheringEnabled()) {
    pc->newVecArray(dst.uc, 1, SimdWidth::k128, dst.name(), "uc");
    pc->v_loadu64(dst.uc[0], src);
    _ditheringContext.ditherUnpackedPixels(dst);
  }
  else {
    pc->x_fetch_pixel(dst, PixelCount(1), flags, FormatExt::kPRGB32, src, Alignment(4));
  }
}

void FetchGradientPart::fetchMultiplePixels(Pixel& dst, PixelCount n, PixelFlags flags, const Vec& idx, IndexLayout indexLayout, InterleaveCallback cb, void* cbData) noexcept {
  Mem src = mem_ptr(_tablePtr);
  uint32_t idxShift = tablePtrShift();

  if (ditheringEnabled()) {
    dst.setType(PixelType::kRGBA64);
    FetchUtils::x_gather_pixels(pc, dst, n, FormatExt::kPRGB64, PixelFlags::kUC, src, idx, idxShift, indexLayout, cb, cbData);
    _ditheringContext.ditherUnpackedPixels(dst);

    dst.setType(PixelType::kRGBA32);
    pc->x_satisfy_pixel(dst, flags);
  }
  else {
    FetchUtils::x_gather_pixels(pc, dst, n, format(), flags, src, idx, idxShift, indexLayout, cb, cbData);
  }
}

// bl::Pipeline::JIT::FetchLinearGradientPart - Construction & Destruction
// =======================================================================

FetchLinearGradientPart::FetchLinearGradientPart(PipeCompiler* pc, FetchType fetchType, FormatExt format) noexcept
  : FetchGradientPart(pc, fetchType, format) {

  _maxSimdWidthSupported = SimdWidth::k256;

  bool dither = false;
  switch (fetchType) {
    case FetchType::kGradientLinearNNPad: _extendMode = ExtendMode::kPad; break;
    case FetchType::kGradientLinearNNRoR: _extendMode = ExtendMode::kRoR; break;
    case FetchType::kGradientLinearDitherPad: _extendMode = ExtendMode::kPad; dither = true; break;
    case FetchType::kGradientLinearDitherRoR: _extendMode = ExtendMode::kRoR; dither = true; break;
    default:
      BL_NOT_REACHED();
  }

  setDitheringEnabled(dither);
  JitUtils::resetVarStruct(&f, sizeof(f));
}

// bl::Pipeline::JIT::FetchLinearGradientPart - Prepare
// ====================================================

void FetchLinearGradientPart::preparePart() noexcept {
#if defined(BL_JIT_ARCH_X86)
  _maxPixels = uint8_t(pc->hasSSSE3() ? 8 : 4);
#else
  _maxPixels = 8;
#endif
}

// bl::Pipeline::JIT::FetchLinearGradientPart - Init & Fini
// ========================================================

void FetchLinearGradientPart::_initPart(const PipeFunction& fn, Gp& x, Gp& y) noexcept {
  // Local Registers
  // ---------------

  _tablePtr = pc->newGpPtr("f.table");                // Reg.
  f->pt = pc->newVec("f.pt");                         // Reg.
  f->dt = pc->newVec("f.dt");                         // Reg/Mem.
  f->dtN = pc->newVec("f.dtN");                       // Reg/Mem.
  f->py = pc->newVec("f.py");                         // Reg/Mem.
  f->dy = pc->newVec("f.dy");                         // Reg/Mem.
  f->maxi = pc->newVec("f.maxi");                     // Reg/Mem.
  f->rori = pc->newVec("f.rori");                     // Reg/Mem [RoR only].
  f->vIdx = pc->newVec("f.vIdx");                     // Reg/Tmp.

  // In 64-bit mode it's easier to use IMUL for 64-bit multiplication instead of SIMD, because
  // we need to multiply a scalar anyway that we then broadcast and add to our 'f.pt' vector.
  if (pc->is64Bit()) {
    f->dtGp = pc->newGp64("f.dtGp");                  // Reg/Mem.
  }

  // Part Initialization
  // -------------------

  pc->load(_tablePtr, mem_ptr(fn.fetchData(), REL_GRADIENT(lut.data)));

  if (ditheringEnabled())
    _ditheringContext.initY(fn, x, y);

  pc->s_mov_u32(f->py, y);
  pc->v_broadcast_u64(f->dy, mem_ptr(fn.fetchData(), REL_GRADIENT(linear.dy.u64)));
  pc->v_broadcast_u64(f->py, f->py);
  pc->v_mul_u64_lo_u32(f->py, f->dy, f->py);
  pc->v_broadcast_u64(f->dt, mem_ptr(fn.fetchData(), REL_GRADIENT(linear.dt.u64)));

  if (isPad()) {
    pc->v_broadcast_u16(f->maxi, mem_ptr(fn.fetchData(), REL_GRADIENT(linear.maxi)));
  }
  else {
    pc->v_broadcast_u32(f->maxi, mem_ptr(fn.fetchData(), REL_GRADIENT(linear.maxi)));
    pc->v_broadcast_u16(f->rori, mem_ptr(fn.fetchData(), REL_GRADIENT(linear.rori)));
  }

  pc->v_loadu128(f->pt, mem_ptr(fn.fetchData(), REL_GRADIENT(linear.pt)));
  pc->v_slli_i64(f->dtN, f->dt, 1u);

#if defined(BL_JIT_ARCH_X86)
  if (pc->use256BitSimd()) {
    cc->vperm2i128(f->dtN, f->dtN, f->dtN, perm2x128Imm(Perm2x128::kALo, Perm2x128::kZero));
    cc->vperm2i128(f->pt, f->pt, f->pt, perm2x128Imm(Perm2x128::kALo, Perm2x128::kALo));
    pc->v_add_i64(f->pt, f->pt, f->dtN);
    pc->v_slli_i64(f->dtN, f->dt, 2u);
  }
#endif // BL_JIT_ARCH_X86

  pc->v_add_i64(f->py, f->py, f->pt);

#if defined(BL_JIT_ARCH_X86)
  // If we cannot use PACKUSDW, which was introduced by SSE4.1 we subtract 32768 from the pointer
  // and use PACKSSDW instead. However, if we do this, we have to adjust everything else accordingly.
  if (isPad() && !pc->hasSSE4_1()) {
    pc->v_sub_i32(f->py, f->py, pc->simdConst(&ct.i_0000800000008000, Bcst::k32, f->py));
    pc->v_sub_i16(f->maxi, f->maxi, pc->simdConst(&ct.i_8000800080008000, Bcst::kNA, f->maxi));
  }
#endif // BL_JIT_ARCH_X86

  if (pc->is64Bit())
    pc->s_mov_u64(f->dtGp, f->dt);

  if (isRectFill()) {
    Vec adv = pc->newSimilarReg(f->dt, "f.adv");
    calcAdvanceX(adv, x);
    pc->v_add_i64(f->py, f->py, adv);
  }

  if (pixelGranularity() > 1)
    enterN();
}

void FetchLinearGradientPart::_finiPart() noexcept {}

// bl::Pipeline::JIT::FetchLinearGradientPart - Advance
// ====================================================

void FetchLinearGradientPart::advanceY() noexcept {
  pc->v_add_i64(f->py, f->py, f->dy);

  if (ditheringEnabled())
    _ditheringContext.advanceY();
}

void FetchLinearGradientPart::startAtX(const Gp& x) noexcept {
  if (!isRectFill()) {
    calcAdvanceX(f->pt, x);
    pc->v_add_i64(f->pt, f->pt, f->py);
  }
  else {
    pc->v_mov(f->pt, f->py);
  }

  if (ditheringEnabled())
    _ditheringContext.startAtX(x);
}

void FetchLinearGradientPart::advanceX(const Gp& x, const Gp& diff) noexcept {
  Vec adv = pc->newSimilarReg(f->pt, "f.adv");
  calcAdvanceX(adv, diff);
  pc->v_add_i64(f->pt, f->pt, adv);

  if (ditheringEnabled())
    _ditheringContext.advanceX(x, diff);
}

void FetchLinearGradientPart::calcAdvanceX(const Vec& dst, const Gp& diff) const noexcept {
  // Use imul on 64-bit targets as it's much shorter than doing a vectorized 64x32 multiply.
  if (pc->is64Bit()) {
    Gp advTmp = pc->newGp64("f.advTmp");
    pc->mul(advTmp, diff.r64(), f->dtGp);
    pc->v_broadcast_u64(dst, advTmp);
  }
  else {
    pc->v_broadcast_u32(dst, diff);
    pc->v_mul_u64_lo_u32(dst, f->dt, dst);
  }
}

// bl::Pipeline::JIT::FetchLinearGradientPart - Fetch
// ==================================================

void FetchLinearGradientPart::prefetch1() noexcept {}

void FetchLinearGradientPart::enterN() noexcept {}
void FetchLinearGradientPart::leaveN() noexcept {}

void FetchLinearGradientPart::prefetchN() noexcept {
  Vec vIdx = f->vIdx;

#if defined(BL_JIT_ARCH_X86)
  if (pc->simdWidth() >= SimdWidth::k256) {
    if (isPad()) {
      pc->v_mov(vIdx, f->pt);
      pc->v_add_i64(f->pt, f->pt, f->dtN);
      pc->v_packs_i32_u16(vIdx, vIdx, f->pt);
      pc->v_min_u16(vIdx, vIdx, f->maxi);
    }
    else {
      Vec vTmp = pc->newSimilarReg(f->vIdx, "f.vTmp");
      pc->v_and_i32(vIdx, f->pt, f->maxi);
      pc->v_add_i64(f->pt, f->pt, f->dtN);
      pc->v_and_i32(vTmp, f->pt, f->maxi);
      pc->v_packs_i32_u16(vIdx, vIdx, vTmp);
      pc->v_xor_i32(vTmp, vIdx, f->rori);
      pc->v_min_u16(vIdx, vIdx, vTmp);
    }

    pc->v_swizzle_u64x4(vIdx, vIdx, swizzle(3, 1, 2, 0));
  }
  else
#endif // BL_JIT_ARCH_X86
  {
    pc->v_mov(vIdx, f->pt);
    pc->v_add_i64(f->pt, f->pt, f->dtN);
    pc->v_interleave_shuffle_u32x4(vIdx, vIdx, f->pt, swizzle(3, 1, 3, 1));
  }
}

void FetchLinearGradientPart::postfetchN() noexcept {
  pc->v_sub_i64(f->pt, f->pt, f->dtN);
}

void FetchLinearGradientPart::fetch(Pixel& p, PixelCount n, PixelFlags flags, PixelPredicate& predicate) noexcept {
  BL_ASSERT(predicate.empty());
  blUnused(predicate);

  p.setCount(n);

  switch (n.value()) {
    case 1: {
      Gp gIdx = pc->newGp32("f.gIdx");
      Vec vIdx = pc->newV128("f.vIdx");
      uint32_t vIdxLane = 1u + uint32_t(!isPad());

      if (isPad()) {
#if defined(BL_JIT_ARCH_X86)
        if (!pc->hasSSE4_1()) {
          pc->v_packs_i32_i16(vIdx, f->pt.v128(), f->pt.v128());
          pc->v_min_i16(vIdx, vIdx, f->maxi.v128());
          pc->v_add_i16(vIdx, vIdx, pc->simdConst(&ct.i_8000800080008000, Bcst::kNA, vIdx));
        }
        else
#endif // BL_JIT_ARCH_X86
        {
          pc->v_packs_i32_u16(vIdx, f->pt.v128(), f->pt.v128());
          pc->v_min_u16(vIdx, vIdx, f->maxi.v128());
        }
      }
      else {
        Vec vTmp = pc->newV128("f.vTmp");
        pc->v_and_i32(vIdx, f->pt.v128(), f->maxi.v128());
        pc->v_xor_i32(vTmp, vIdx, f->rori.v128());
        pc->v_min_i16(vIdx, vIdx, vTmp);
      }

      pc->v_add_i64(f->pt, f->pt, f->dt);
      pc->s_extract_u16(gIdx, vIdx, vIdxLane);
      fetchSinglePixel(p, flags, gIdx);
      pc->x_satisfy_pixel(p, flags);
      break;
    }

    case 4: {
      Vec vIdx = f->vIdx;
      Vec vTmp = pc->newSimilarReg(vIdx, "f.vTmp");

#if defined(BL_JIT_ARCH_X86)
      if (pc->simdWidth() >= SimdWidth::k256) {
        Vec vWrk = pc->newSimilarReg(vIdx, "@vWrk");

        fetchMultiplePixels(p, n, flags, vIdx.v128(), IndexLayout::kUInt32Hi16, [&](uint32_t step) noexcept {
          if (isPad()) {
            switch (step) {
              case 0   : pc->v_mov(vWrk, f->pt); break;
              case 1   : pc->v_add_i64(f->pt, f->pt, f->dtN); break;
              case 2   : pc->v_packs_i32_u16(vWrk, vWrk, f->pt); break;
              case 3   : pc->v_min_u16(vWrk, vWrk, f->maxi); break;
              case 0xFF: pc->v_swizzle_u64x4(vIdx, vWrk, swizzle(3, 1, 2, 0)); break;
            }
          }
          else {
            switch (step) {
              case 0   : pc->v_and_i32(vWrk, f->pt, f->maxi);
                         pc->v_add_i64(f->pt, f->pt, f->dtN); break;
              case 1   : pc->v_and_i32(vTmp, f->pt, f->maxi);
                         pc->v_packs_i32_u16(vWrk, vWrk, vTmp); break;
              case 2   : pc->v_xor_i32(vWrk, vWrk, f->rori); break;
              case 3   : pc->v_min_u16(vWrk, vWrk, vTmp); break;
              case 0xFF: pc->v_swizzle_u64x4(vIdx, vWrk, swizzle(3, 1, 2, 0)); break;
            }
          }
        });
      }
      else
#endif // BL_JIT_ARCH_X86
      {
        IndexLayout indexLayout = IndexLayout::kUInt16;

        if (isPad()) {
#if defined(BL_JIT_ARCH_X86)
          if (!pc->hasSSE4_1()) {
            pc->v_packs_i32_i16(vIdx, vIdx, vIdx);
            pc->v_min_i16(vIdx, vIdx, f->maxi);
            pc->v_add_i16(vIdx, vIdx, pc->simdConst(&ct.i_8000800080008000, Bcst::kNA, vIdx));
          }
          else
#endif // BL_JIT_ARCH_X86
          {
            pc->v_packs_i32_u16(vIdx, vIdx, vIdx);
            pc->v_min_u16(vIdx, vIdx, f->maxi);
          }
        }
        else {
          indexLayout = IndexLayout::kUInt32Lo16;
          pc->v_and_i32(vIdx, vIdx, f->maxi);
          pc->v_xor_i32(vTmp, vIdx, f->rori);
          pc->v_min_i16(vIdx, vIdx, vTmp);
        }

        fetchMultiplePixels(p, n, flags, vIdx.v128(), indexLayout, [&](uint32_t step) noexcept {
          if (!pc->hasNonDestructiveDst()) {
            switch (step) {
              case 0   : pc->v_add_i64(f->pt, f->pt, f->dtN); break;
              case 1   : pc->v_mov(vTmp, f->pt); break;
              case 2   : pc->v_add_i64(f->pt, f->pt, f->dtN); break;
              case 0xFF: pc->v_interleave_shuffle_u32x4(vIdx, vTmp, f->pt, swizzle(3, 1, 3, 1)); break;
            }
          }
          else {
            switch (step) {
              case 0   : pc->v_add_i64(vTmp, f->pt, f->dtN); break;
              case 2   : pc->v_add_i64(f->pt, vTmp, f->dtN); break;
              case 0xFF: pc->v_interleave_shuffle_u32x4(vIdx, vTmp, f->pt, swizzle(3, 1, 3, 1)); break;
            }
          }
        });
      }

      pc->x_satisfy_pixel(p, flags);
      break;
    }

    case 8: {
      Vec vIdx = f->vIdx;
      Vec vTmp = pc->newSimilarReg(vIdx, "f.vTmp");

#if defined(BL_JIT_ARCH_X86)
      if (pc->simdWidth() >= SimdWidth::k256) {
        Vec vWrk = pc->newSimilarReg(vIdx, "@vWrk");

        fetchMultiplePixels(p, n, flags, vIdx, IndexLayout::kUInt32Hi16, [&](uint32_t step) noexcept {
          if (isPad()) {
            switch (step) {
              case 0   : pc->v_add_i64(vWrk, f->pt, f->dtN); break;
              case 1   : pc->v_add_i64(f->pt, vWrk, f->dtN); break;
              case 2   : pc->v_packs_i32_u16(vWrk, vWrk, f->pt); break;
              case 3   : pc->v_min_u16(vWrk, vWrk, f->maxi); break;
              case 0xFF: pc->v_swizzle_u64x4(vIdx, vWrk, swizzle(3, 1, 2, 0)); break;
            }
          }
          else {
            switch (step) {
              case 0   : pc->v_add_i64(f->pt, f->pt, f->dtN); break;
              case 1   : pc->v_and_i32(vWrk, f->pt, f->maxi); break;
              case 2   : pc->v_add_i64(f->pt, f->pt, f->dtN); break;
              case 3   : pc->v_and_i32(vTmp, f->pt, f->maxi); break;
              case 4   : pc->v_packs_i32_u16(vWrk, vWrk, vTmp); break;
              case 5   : pc->v_xor_i32(vTmp, vWrk, f->rori); break;
              case 6   : pc->v_min_u16(vWrk, vWrk, vTmp); break;
              case 0xFF: pc->v_swizzle_u64x4(vIdx, vWrk, swizzle(3, 1, 2, 0)); break;
            }
          }
        });
      }
      else
#endif // BL_JIT_ARCH_X86
      {
        pc->v_add_i64(f->pt, f->pt, f->dtN);
        pc->v_mov(vTmp, f->pt);
        pc->v_add_i64(f->pt, f->pt, f->dtN);
        pc->v_interleave_shuffle_u32x4(vTmp, vTmp, f->pt, swizzle(3, 1, 3, 1));

        if (isPad()) {
#if defined(BL_JIT_ARCH_X86)
          if (!pc->hasSSE4_1()) {
            pc->v_packs_i32_i16(vIdx, vIdx, vTmp);
            pc->v_min_i16(vIdx, vIdx, f->maxi);
            pc->v_add_i16(vIdx, vIdx, pc->simdConst(&ct.i_8000800080008000, Bcst::kNA, vIdx));
          }
          else
#endif // BL_JIT_ARCH_X86
          {
            pc->v_packs_i32_u16(vIdx, vIdx, vTmp);
            pc->v_min_u16(vIdx, vIdx, f->maxi);
          }
        }
        else {
          pc->v_and_i32(vIdx, vIdx, f->maxi);
          pc->v_and_i32(vTmp, vTmp, f->maxi);
          pc->v_packs_i32_i16(vIdx, vIdx, vTmp);
          pc->v_xor_i32(vTmp, vIdx, f->rori);
          pc->v_min_i16(vIdx, vIdx, vTmp);
        }

        fetchMultiplePixels(p, n, flags, vIdx, IndexLayout::kUInt16, [&](uint32_t step) noexcept {
          if (!pc->hasNonDestructiveDst()) {
            switch (step) {
              case 1   : pc->v_add_i64(f->pt, f->pt, f->dtN); break;
              case 3   : pc->v_mov(vTmp, f->pt); break;
              case 5   : pc->v_add_i64(f->pt, f->pt, f->dtN); break;
              case 0xFF: pc->v_interleave_shuffle_u32x4(vIdx, vTmp, f->pt, swizzle(3, 1, 3, 1)); break;
            }
          }
          else {
            switch (step) {
              case 1   : pc->v_add_i64(vTmp, f->pt, f->dtN); break;
              case 5   : pc->v_add_i64(f->pt, vTmp, f->dtN); break;
              case 0xFF: pc->v_interleave_shuffle_u32x4(vIdx, vTmp, f->pt, swizzle(3, 1, 3, 1)); break;
            }
          }
        });
      }

      pc->x_satisfy_pixel(p, flags);
      break;
    }

    default:
      BL_NOT_REACHED();
  }
}

// bl::Pipeline::JIT::FetchRadialGradientPart - Construction & Destruction
// =======================================================================

FetchRadialGradientPart::FetchRadialGradientPart(PipeCompiler* pc, FetchType fetchType, FormatExt format) noexcept
  : FetchGradientPart(pc, fetchType, format) {

  _partFlags |= PipePartFlags::kAdvanceXNeedsX;
  _isComplexFetch = true;

  bool dither = false;
  switch (fetchType) {
    case FetchType::kGradientRadialNNPad: _extendMode = ExtendMode::kPad; break;
    case FetchType::kGradientRadialNNRoR: _extendMode = ExtendMode::kRoR; break;
    case FetchType::kGradientRadialDitherPad: _extendMode = ExtendMode::kPad; dither = true; break;
    case FetchType::kGradientRadialDitherRoR: _extendMode = ExtendMode::kRoR; dither = true; break;
    default:
      BL_NOT_REACHED();
  }

  setDitheringEnabled(dither);
  JitUtils::resetVarStruct(&f, sizeof(f));
}

// bl::Pipeline::JIT::FetchRadialGradientPart - Prepare
// ====================================================

void FetchRadialGradientPart::preparePart() noexcept {
  _maxPixels = 4;
}

// bl::Pipeline::JIT::FetchRadialGradientPart - Init & Fini
// ========================================================

void FetchRadialGradientPart::_initPart(const PipeFunction& fn, Gp& x, Gp& y) noexcept {
  // Local Registers
  // ---------------

  _tablePtr = pc->newGpPtr("f.table");                // Reg.
  f->xx_xy = pc->newV128_F64("f.xx_xy");              // Mem.
  f->yx_yy = pc->newV128_F64("f.yx_yy");              // Mem.
  f->ax_ay = pc->newV128_F64("f.ax_ay");              // Mem.
  f->fx_fy = pc->newV128_F64("f.fx_fy");              // Mem.
  f->da_ba = pc->newV128_F64("f.da_ba");              // Mem.

  f->d_b = pc->newV128_F64("f.d_b");                  // Reg.
  f->dd_bd = pc->newV128_F64("f.dd_bd");              // Reg.
  f->ddx_ddy = pc->newV128_F64("f.ddx_ddy");          // Mem.

  f->px_py = pc->newV128_F64("f.px_py");              // Reg.
  f->scale = pc->newV128_F32("f.scale");              // Mem.
  f->ddd = pc->newV128_F64("f.ddd");                  // Mem.
  f->value = pc->newV128_F32("f.value");              // Reg/Tmp.

  f->vmaxi = pc->newV128("f.vmaxi");                  // Mem.
  f->vrori = pc->newV128("f.vrori");                  // Mem.
  f->vmaxf = pc->newV128_F64("f.vmaxf");              // Mem.

  f->d_b_prev = pc->newV128_F64("f.d_b_prev");        // Mem.
  f->dd_bd_prev = pc->newV128_F64("f.dd_bd_prev");    // Mem.

  Vec off = pc->newV128_F64("f.off");                 // Initialization only.

  // Part Initialization
  // -------------------

  pc->load(_tablePtr, mem_ptr(fn.fetchData(), REL_GRADIENT(lut.data)));

  if (ditheringEnabled())
    _ditheringContext.initY(fn, x, y);

  pc->v_loadu128_f64(f->ax_ay, mem_ptr(fn.fetchData(), REL_GRADIENT(radial.ax)));
  pc->v_loadu128_f64(f->fx_fy, mem_ptr(fn.fetchData(), REL_GRADIENT(radial.fx)));

  pc->v_loadu128_f64(f->da_ba  , mem_ptr(fn.fetchData(), REL_GRADIENT(radial.dd)));
  pc->v_loadu128_f64(f->ddx_ddy, mem_ptr(fn.fetchData(), REL_GRADIENT(radial.ddx)));

  pc->s_cvt_f64_to_f32(f->scale, mem_ptr(fn.fetchData(), REL_GRADIENT(radial.scale)));

  pc->v_loada64_f64(f->ddd, mem_ptr(fn.fetchData(), REL_GRADIENT(radial.ddd)));
  pc->v_swizzle_f32x4(f->scale, f->scale, swizzle(0, 0, 0, 0));

  pc->v_loadu128_f64(f->xx_xy, mem_ptr(fn.fetchData(), REL_GRADIENT(radial.xx)));
  pc->v_loadu128_f64(f->yx_yy, mem_ptr(fn.fetchData(), REL_GRADIENT(radial.yx)));

  pc->s_cvt_int_to_f64(f->px_py, y);
  pc->v_loadu128_f64(off, mem_ptr(fn.fetchData(), REL_GRADIENT(radial.ox)));

  pc->v_dup_lo_f64(f->px_py, f->px_py);
  pc->v_mul_f64(f->px_py, f->px_py, f->yx_yy);
  pc->v_add_f64(f->px_py, f->px_py, off);

  if (isPad()) {
    pc->v_broadcast_u16(f->vmaxi, mem_ptr(fn.fetchData(), REL_GRADIENT(radial.maxi)));
    pc->v_broadcast_u16(f->vrori, mem_ptr(fn.fetchData(), REL_GRADIENT(radial.rori)));
  }
  else {
    pc->v_broadcast_u32(f->vmaxi, mem_ptr(fn.fetchData(), REL_GRADIENT(radial.maxi)));
    pc->v_broadcast_u32(f->vrori, mem_ptr(fn.fetchData(), REL_GRADIENT(radial.rori)));
  }

  if (isPad()) {
    pc->v_cvt_i32_to_f32(f->vmaxf, f->vmaxi);
  }

  if (isRectFill()) {
    pc->s_cvt_int_to_f64(off, x);
    pc->v_dup_lo_f64(off, off);
    pc->v_mul_f64(off, off, f->xx_xy);
    pc->v_add_f64(f->px_py, f->px_py, off);
  }
}

void FetchRadialGradientPart::_finiPart() noexcept {}

// bl::Pipeline::JIT::FetchRadialGradientPart - Advance
// ====================================================

void FetchRadialGradientPart::advanceY() noexcept {
  pc->v_add_f64(f->px_py, f->px_py, f->yx_yy);

  if (ditheringEnabled())
    _ditheringContext.advanceY();
}

void FetchRadialGradientPart::startAtX(const Gp& x) noexcept {
  if (isRectFill()) {
    precalc(f->px_py);
  }
  else {
    Vec px_py = pc->newV128_F64("f.px_py");

    pc->s_cvt_int_to_f64(px_py, x);
    pc->v_dup_lo_f64(px_py, px_py);
    pc->v_mul_f64(px_py, px_py, f->xx_xy);
    pc->v_add_f64(px_py, px_py, f->px_py);

    precalc(px_py);
  }

  if (ditheringEnabled())
    _ditheringContext.startAtX(x);
}

void FetchRadialGradientPart::advanceX(const Gp& x, const Gp& diff) noexcept {
  if (isRectFill()) {
    precalc(f->px_py);
  }
  else {
    Vec px_py = pc->newV128_F64("f.px_py");

    // TODO: [PIPEGEN] Duplicated code :(
    pc->s_cvt_int_to_f64(px_py, x);
    pc->v_dup_lo_f64(px_py, px_py);
    pc->v_mul_f64(px_py, px_py, f->xx_xy);
    pc->v_add_f64(px_py, px_py, f->px_py);

    precalc(px_py);
  }

  if (ditheringEnabled())
    _ditheringContext.advanceX(x, diff);
}

// bl::Pipeline::JIT::FetchRadialGradientPart - Fetch
// ==================================================

void FetchRadialGradientPart::prefetch1() noexcept {
  pc->v_cvt_f64_to_f32_lo(f->value, f->d_b);
  pc->v_and_f32(f->value, f->value, pc->simdConst(&ct.f32_abs_lo, Bcst::kNA, f->value));
  pc->s_sqrt_f32(f->value, f->value);
}

void FetchRadialGradientPart::prefetchN() noexcept {
  Vec& d_b = f->d_b;
  Vec& dd_bd = f->dd_bd;
  Vec& ddd = f->ddd;
  Vec& value = f->value;

  Vec x0 = pc->newV128_F64("prf_x0");
  Vec x1 = pc->newV128_F64("prf_x1");
  Vec x2 = pc->newV128_F64("prf_x2");

  pc->v_mov(f->d_b_prev, f->d_b);     // Save `d_b`.
  pc->v_mov(f->dd_bd_prev, f->dd_bd); // Save `dd_bd`.

  pc->v_cvt_f64_to_f32_lo(x0, d_b);
  pc->v_add_f64(d_b, d_b, dd_bd);
  pc->v_add_f64(dd_bd, dd_bd, ddd);

  pc->v_cvt_f64_to_f32_lo(x1, d_b);
  pc->v_add_f64(d_b, d_b, dd_bd);
  pc->v_add_f64(dd_bd, dd_bd, ddd);
  pc->v_interleave_shuffle_f32x4(x0, x0, x1, swizzle(1, 0, 1, 0));

  pc->v_cvt_f64_to_f32_lo(x1, d_b);
  pc->v_add_f64(d_b, d_b, dd_bd);
  pc->v_add_f64(dd_bd, dd_bd, ddd);

  pc->v_cvt_f64_to_f32_lo(x2, d_b);
  pc->v_add_f64(d_b, d_b, dd_bd);
  pc->v_add_f64(dd_bd, dd_bd, ddd);
  pc->v_interleave_shuffle_f32x4(x1, x1, x2, swizzle(1, 0, 1, 0));

  pc->v_interleave_shuffle_f32x4(value, x0, x1, swizzle(2, 0, 2, 0));
  pc->v_abs_f32(value, value);
  pc->v_sqrt_f32(value, value);

  pc->v_interleave_shuffle_f32x4(x0, x0, x1, swizzle(3, 1, 3, 1));
  pc->v_add_f32(value, value, x0);
}

void FetchRadialGradientPart::postfetchN() noexcept {
  pc->v_mov(f->d_b, f->d_b_prev);     // Restore `d_b`.
  pc->v_mov(f->dd_bd, f->dd_bd_prev); // Restore `dd_bd`.
}

void FetchRadialGradientPart::fetch(Pixel& p, PixelCount n, PixelFlags flags, PixelPredicate& predicate) noexcept {
  BL_ASSERT(predicate.empty());
  blUnused(predicate);

  p.setCount(n);

  switch (n.value()) {
    case 1: {
      Vec x0 = pc->newV128_F32("f.x0");
      Gp gIdx = pc->newGp32("f.gIdx");

      pc->v_swizzle_u32x4(x0, f->value, swizzle(1, 1, 1, 1));
      pc->v_add_f64(f->d_b, f->d_b, f->dd_bd);

      pc->s_add_f32(x0, x0, f->value);
      pc->v_cvt_f64_to_f32_lo(f->value, f->d_b);

      pc->s_mul_f32(x0, x0, f->scale);
      pc->v_and_f32(f->value, f->value, pc->simdConst(&ct.f32_abs_lo, Bcst::kNA, f->value));

      pc->v_cvt_trunc_f32_to_i32(x0, x0);

      pc->v_add_f64(f->dd_bd, f->dd_bd, f->ddd);
      pc->s_sqrt_f32(f->value, f->value);

      Vec vIdx = pc->newV128("f.vIdx");
      if (isPad()) {
#if defined(BL_JIT_ARCH_X86)
        if (!pc->hasSSE4_1()) {
          pc->v_packs_i32_i16(vIdx, x0, x0);
          pc->v_min_i16(vIdx, vIdx, f->vmaxi.v128());
          pc->v_max_i16(vIdx, vIdx, pc->simdConst(&ct.i_0000000000000000, Bcst::kNA, vIdx));
        }
        else
#endif // BL_JIT_ARCH_X86
        {
          pc->v_packs_i32_u16(vIdx, x0, x0);
          pc->v_min_u16(vIdx, vIdx, f->vmaxi.v128());
        }
      }
      else {
        Vec vTmp = pc->newV128("f.vTmp");
        pc->v_and_i32(vIdx, x0, f->vmaxi.v128());
        pc->v_xor_i32(vTmp, vIdx, f->vrori.v128());
        pc->v_min_i16(vIdx, vIdx, vTmp);
      }

      pc->s_extract_u16(gIdx, vIdx, 0u);
      fetchSinglePixel(p, flags, gIdx);
      pc->x_satisfy_pixel(p, flags);
      break;
    }

    case 4: {
      Vec& d_b   = f->d_b;
      Vec& dd_bd = f->dd_bd;
      Vec& ddd   = f->ddd;
      Vec& value = f->value;

      Vec x0 = pc->newV128_F64("fetch_x0");
      Vec x1 = pc->newV128_F64("fetch_x1");
      Vec x2 = pc->newV128_F64("fetch_x2");
      Vec x3 = pc->newV128_F64("fetch_x3");

      pc->v_mul_f32(value, value, f->scale);
      pc->v_cvt_f64_to_f32_lo(x0, d_b);

      pc->v_mov(f->d_b_prev, d_b);     // Save `d_b_prev`.
      pc->v_mov(f->dd_bd_prev, dd_bd); // Save `dd_bd_prev`.

      pc->v_add_f64(d_b, d_b, dd_bd);
      pc->v_add_f64(dd_bd, dd_bd, ddd);

      pc->v_cvt_f64_to_f32_lo(x1, d_b);
      pc->v_add_f64(d_b, d_b, dd_bd);

      pc->v_cvt_round_f32_to_i32(x3, value);
      pc->v_add_f64(dd_bd, dd_bd, ddd);

      IndexLayout indexLayout = IndexLayout::kUInt16;
      Vec vIdx = pc->newV128("vIdx");

      if (isPad()) {
#if defined(BL_JIT_ARCH_X86)
        if (!pc->hasSSE4_1()) {
          pc->v_packs_i32_i16(vIdx, x3, x3);
          pc->v_min_i16(vIdx, vIdx, f->vmaxi.v128());
          pc->v_max_i16(vIdx, vIdx, pc->simdConst(&ct.i_0000000000000000, Bcst::kNA, vIdx));
        }
        else
#endif // BL_JIT_ARCH_X86
        {
          pc->v_packs_i32_u16(vIdx, x3, x3);
          pc->v_min_u16(vIdx, vIdx, f->vmaxi.v128());
        }
      }
      else {
        indexLayout = IndexLayout::kUInt32Lo16;
        Vec vTmp = pc->newV128("f.vTmp");

        pc->v_and_i32(vIdx, x3, f->vmaxi.v128());
        pc->v_xor_i32(vTmp, vIdx, f->vrori.v128());
        pc->v_min_i16(vIdx, vIdx, vTmp);
      }

      fetchMultiplePixels(p, n, flags, vIdx, indexLayout, [&](uint32_t step) noexcept {
        switch (step) {
          case 0: pc->v_interleave_shuffle_f32x4(x0, x0, x1, swizzle(1, 0, 1, 0));
                  pc->v_cvt_f64_to_f32_lo(x1, d_b);
                  pc->v_add_f64(d_b, d_b, dd_bd);
                  break;
          case 1: pc->v_mov(value, x0);
                  pc->v_cvt_f64_to_f32_lo(x2, d_b);
                  pc->v_add_f64(dd_bd, dd_bd, ddd);
                  break;
          case 2: pc->v_interleave_shuffle_f32x4(x1, x1, x2, swizzle(1, 0, 1, 0));
                  pc->v_interleave_shuffle_f32x4(x0, x0, x1, swizzle(2, 0, 2, 0));
                  pc->v_abs_f32(x0, x0);
                  break;
          case 3: pc->v_sqrt_f32(x0, x0);
                  pc->v_add_f64(d_b, d_b, dd_bd);
                  pc->v_interleave_shuffle_f32x4(value, value, x1, swizzle(3, 1, 3, 1));
                  pc->v_add_f64(dd_bd, dd_bd, ddd);
                  break;
        }
      });

      pc->x_satisfy_pixel(p, flags);
      pc->v_add_f32(value, value, x0);
      break;
    }

    case 8: {
      _fetch2x4(p, flags);
      break;
    }

    default:
      BL_NOT_REACHED();
  }
}

void FetchRadialGradientPart::precalc(const Vec& px_py) noexcept {
  Vec& d_b   = f->d_b;
  Vec& dd_bd = f->dd_bd;

  Vec x0 = pc->newV128_F64("f.x0");
  Vec x1 = pc->newV128_F64("f.x1");
  Vec x2 = pc->newV128_F64("f.x2");

  pc->v_mul_f64(d_b, px_py, f->ax_ay);                   // [Ay.Py           | Ax.Px                             ]
  pc->v_mul_f64(x0, px_py, f->fx_fy);                    // [Fy.Py           | Fx.Px                             ]
  pc->v_mul_f64(x1, px_py, f->ddx_ddy);                  // [Ddy.Py          | Ddx.Px                            ]
  pc->v_mul_f64(d_b, d_b, px_py);                        // [Ay.Py^2         | Ax.Px^2                           ]
  pc->v_hadd_f64(d_b, d_b, x0);                          // [Fx.Px + Fy.Py   | Ax.Px^2 + Ay.Py^2                 ]
  pc->v_swap_f64(x2, x0);
  pc->s_mul_f64(x2, x2, x0);                             // [?               | Fx.Px.Fy.Py                       ]
  pc->s_add_f64(x2, x2, x2);                             // [?               | 2.Fx.Px.Fy.Py                     ]

#if defined(BL_JIT_ARCH_X86)
  pc->v_hadd_f64(x1, x1, x1);                            // [Ddx.Px + Ddy.Py | Ddx.Px + Ddy.Py                   ]
  pc->s_add_f64(d_b, d_b, x2);                           // [Fx.Px + Fy.Py   | Ax.Px^2 + Ay.Py^2 + 2.Fx.Px.Fy.Py ]
  pc->s_add_f64(dd_bd, f->da_ba, x1);                    // [Bd              | Dd + Ddx.Px + Ddy.Py              ]
#else
  pc->v_add_f64(d_b, d_b, x2);                           // [Fx.Px + Fy.Py   | Ax.Px^2 + Ay.Py^2 + 2.Fx.Px.Fy.Py ]
  pc->v_swap_f64(x2, x1);                                // [Ddx.Px          | Ddy.Py                            ]
  pc->s_add_f64(x1, x1, x2);                             // [0               | Ddx.Px + Ddy.Py                   ]
  pc->v_add_f64(dd_bd, f->da_ba, x1);                    // [Bd              | Dd + Ddx.Px + Ddy.Py              ]
#endif
}

// bl::Pipeline::JIT::FetchConicGradientPart - Construction & Destruction
// ======================================================================

FetchConicGradientPart::FetchConicGradientPart(PipeCompiler* pc, FetchType fetchType, FormatExt format) noexcept
  : FetchGradientPart(pc, fetchType, format) {

  _partFlags |= PipePartFlags::kMaskedAccess | PipePartFlags::kAdvanceXNeedsX;
  _isComplexFetch = true;
  _maxSimdWidthSupported = SimdWidth::k512;

  setDitheringEnabled(fetchType == FetchType::kGradientConicDither);
  JitUtils::resetVarStruct(&f, sizeof(f));
}

// bl::Pipeline::JIT::FetchConicGradientPart - Prepare
// ===================================================

void FetchConicGradientPart::preparePart() noexcept {
  _maxPixels = uint8_t(4 * pc->simdMultiplier());
}

// bl::Pipeline::JIT::FetchConicGradientPart - Init & Fini
// =======================================================

void FetchConicGradientPart::_initPart(const PipeFunction& fn, Gp& x, Gp& y) noexcept {
  // Local Registers
  // ---------------

  _tablePtr = pc->newGpPtr("f.table");                // Reg.
  f->consts = pc->newGpPtr("f.consts");               // Reg.
  f->px = pc->newV128_F64("f.px");                    // Reg.
  f->xx = pc->newV128_F64("f.xx");                    // Reg/Mem.
  f->hx_hy = pc->newV128_F64("f.hx_hy");              // Reg. (TODO: Make spillable).
  f->yx_yy = pc->newV128_F64("f.yx_yy");              // Mem.
  f->ay = pc->newVec("f.ay");                         // Reg/Mem.
  f->by = pc->newVec("f.by");                         // Reg/Mem.

  f->angleOffset = pc->newVec("f.angleOffset");       // Reg/Mem.
  f->maxi = pc->newVec("f.maxi");                     // Reg/Mem.

  f->t0 = pc->newVec("f.t0");                         // Reg/Tmp.
  f->t1 = pc->newVec("f.t1");                         // Reg/Tmp.
  f->t2 = pc->newVec("f.t2");                         // Reg/Tmp.

  Vec off = pc->newV128_F64("f.off");                 // Initialization only.

  // Part Initialization
  // -------------------

  pc->load(_tablePtr, mem_ptr(fn.fetchData(), REL_GRADIENT(lut.data)));

  if (ditheringEnabled())
    _ditheringContext.initY(fn, x, y);

  pc->s_cvt_int_to_f64(f->hx_hy, y);
  pc->v_dup_lo_f64(f->xx, mem_ptr(fn.fetchData(), REL_GRADIENT(conic.xx)));
  pc->v_loadu128_f64(f->yx_yy, mem_ptr(fn.fetchData(), REL_GRADIENT(conic.yx)));
  pc->v_loadu128_f64(off, mem_ptr(fn.fetchData(), REL_GRADIENT(conic.ox)));

  pc->v_broadcast_u32(f->maxi, mem_ptr(fn.fetchData(), REL_GRADIENT(conic.maxi)));
  pc->v_broadcast_u32(f->angleOffset, mem_ptr(fn.fetchData(), REL_GRADIENT(conic.offset)));

  pc->v_dup_lo_f64(f->hx_hy, f->hx_hy);
  pc->v_mul_f64(f->hx_hy, f->hx_hy, f->yx_yy);
  pc->v_add_f64(f->hx_hy, f->hx_hy, off);

  pc->load(f->consts, mem_ptr(fn.fetchData(), REL_GRADIENT(conic.consts)));

  if (isRectFill()) {
    pc->s_cvt_int_to_f64(off, x);
    pc->s_mul_f64(off, off, f->xx);
#if defined(BL_JIT_ARCH_X86)
    // Scalar adds keep the original hi value.
    pc->s_add_f64(f->hx_hy, f->hx_hy, off);
#else
    // Scalar adds zero the original hi value, so we have to use a vector add instead.
    pc->v_add_f64(f->hx_hy, f->hx_hy, off);
#endif
  }

  // Setup constants used by 4+ pixel fetches.
  if (maxPixels() > 1) {
    f->xx_inc = pc->newV128("f.xx_inc"); // Reg/Mem.
    f->xx_off = pc->newVec("f.xx_off"); // Reg/Mem.

    pc->v_cvt_f64_to_f32_lo(f->xx_off.v128(), f->xx);

    if (maxPixels() == 4)
      pc->v_mul_f64(f->xx_inc, f->xx, pc->simdMemConst(&ct.f64_4_8, Bcst::k32, f->xx_inc));
    else
      pc->v_mul_f64(f->xx_inc, f->xx, pc->simdMemConst(&ct.f64_8_4, Bcst::k32, f->xx_inc));

    pc->v_broadcast_u32(f->xx_off, f->xx_off);
    pc->v_mul_f32(f->xx_off, f->xx_off, pc->simdMemConst(&ct.f32_increments, Bcst::kNA, f->xx_off));
  }
}

void FetchConicGradientPart::_finiPart() noexcept {}

// bl::Pipeline::JIT::FetchConicGradientPart - Advance
// ===================================================

void FetchConicGradientPart::advanceY() noexcept {
  pc->v_add_f64(f->hx_hy, f->hx_hy, f->yx_yy);

  if (ditheringEnabled())
    _ditheringContext.advanceY();
}

void FetchConicGradientPart::startAtX(const Gp& x) noexcept {
  pc->v_cvt_f64_to_f32_lo(f->by.v128(), f->hx_hy);
  pc->v_swizzle_f32x4(f->by.v128(), f->by.v128(), swizzle(1, 1, 1, 1));

  if (!f->by.isVec128()) {
    pc->v_broadcast_v128_f32(f->by, f->by.v128());
  }

  pc->v_abs_f32(f->ay, f->by);
  pc->v_srai_i32(f->by, f->by, 31);
  pc->v_and_f32(f->by, f->by, mem_ptr(f->consts, BL_OFFSET_OF(CommonTable::Conic, n_div_1)));

  advanceX(x, pc->_gpNone);
}

void FetchConicGradientPart::advanceX(const Gp& x, const Gp& diff) noexcept {
  blUnused(diff);

  if (isRectFill()) {
    pc->v_dup_lo_f64(f->px, f->hx_hy);
  }
  else {
    pc->s_cvt_int_to_f64(f->px, x);
    pc->s_mul_f64(f->px, f->px, f->xx);
    pc->s_add_f64(f->px, f->px, f->hx_hy);
  }

  recalcX();

  if (ditheringEnabled())
    _ditheringContext.startAtX(x);
}

void FetchConicGradientPart::recalcX() noexcept {
  pc->v_cvt_f64_to_f32_lo(f->t0.v128(), f->px);

  if (maxPixels() == 1) {
    Vec t0 = f->t0.v128();
    Vec t1 = f->t1.v128();
    Vec t2 = f->t2.v128();
    Vec ay = f->ay.v128();
    Vec tmp = pc->newV128("f.tmp");

    pc->v_abs_f32(t1, t0);
    pc->s_max_f32(tmp, t1, ay);
    pc->s_min_f32(t2, t1, ay);

    pc->s_cmp_eq_f32(t1, t1, t2);
    pc->s_div_f32(t2, t2, tmp);

    pc->v_srai_i32(t0, t0, 31);
    pc->v_and_f32(t1, t1, mem_ptr(f->consts, BL_OFFSET_OF(CommonTable::Conic, n_div_4)));
  }
  else {
    Vec t0 = f->t0;
    Vec t1 = f->t1;
    Vec t2 = f->t2;
    Vec ay = f->ay;
    Vec tmp = pc->newSimilarReg(f->t0, "f.tmp");

    pc->v_broadcast_u32(t0, t0);
    pc->v_add_f32(t0, t0, f->xx_off);
    pc->v_abs_f32(t1, t0);

    pc->v_max_f32(tmp, t1, ay);
    pc->v_min_f32(t2, t1, ay);
    pc->v_cmp_eq_f32(t1, t1, t2);
    pc->v_div_f32(t2, t2, tmp);
  }
}

// bl::Pipeline::JIT::FetchConicGradientPart - Fetch
// =================================================

void FetchConicGradientPart::prefetchN() noexcept {}

void FetchConicGradientPart::fetch(Pixel& p, PixelCount n, PixelFlags flags, PixelPredicate& predicate) noexcept {
  p.setCount(n);

  Gp consts = f->consts;
  Vec px = f->px;
  Vec t0 = f->t0;
  Vec t1 = f->t1;
  Vec t2 = f->t2;

  // Use 128-bit SIMD if the number of pixels is 4 or less.
  if (n.value() <= 4) {
    t0 = t0.v128();
    t1 = t1.v128();
    t2 = t2.v128();
  }

  Vec t3 = pc->newSimilarReg(t0, "f.t3");
  Vec t4 = pc->newSimilarReg(t0, "f.t4");

  switch (n.value()) {
    case 1: {
      Gp idx = pc->newGpPtr("f.idx");

      pc->s_mul_f32(t3, t2, t2);
      pc->v_srai_i32(t0, t0, 31);
      pc->v_loada32_f32(t4, mem_ptr(consts, BL_OFFSET_OF(CommonTable::Conic, q3)));

      pc->s_madd_f32(t4, t4, t3, mem_ptr(consts, BL_OFFSET_OF(CommonTable::Conic, q2)));
      pc->v_and_f32(t1, t1, mem_ptr(f->consts, BL_OFFSET_OF(CommonTable::Conic, n_div_4)));
      pc->s_madd_f32(t4, t4, t3, mem_ptr(consts, BL_OFFSET_OF(CommonTable::Conic, q1)));
      pc->v_and_f32(t0, t0, mem_ptr(consts, BL_OFFSET_OF(CommonTable::Conic, n_div_2)));
      pc->s_madd_f32(t4, t4, t3, mem_ptr(consts, BL_OFFSET_OF(CommonTable::Conic, q0)));
      pc->s_msub_f32(t4, t4, t2, t1);

      pc->v_abs_f32(t4, t4);
      pc->s_sub_f32(t4, t4, t0);
      pc->v_abs_f32(t4, t4);

      pc->s_sub_f32(t4, t4, f->by);
      pc->v_abs_f32(t4, t4);
      pc->s_add_f32(t4, t4, f->angleOffset.v128());

      pc->v_cvt_trunc_f32_to_i32(t4, t4);
      pc->s_add_f64(px, px, f->xx);
      pc->v_and_i32(t4, t4, f->maxi.v128());
      pc->s_mov_u32(idx.r32(), t4);

      recalcX();
      fetchSinglePixel(p, flags, idx);
      pc->x_satisfy_pixel(p, flags);
      break;
    }

    case 4:
    case 8:
    case 16: {
      pc->v_mul_f32(t3, t2, t2);
      pc->v_srai_i32(t0, t0, 31);
      pc->v_mov(t4, mem_ptr(consts, BL_OFFSET_OF(CommonTable::Conic, q3)));
      pc->v_and_f32(t1, t1, mem_ptr(f->consts, BL_OFFSET_OF(CommonTable::Conic, n_div_4)));

      pc->v_madd_f32(t4, t4, t3, mem_ptr(consts, BL_OFFSET_OF(CommonTable::Conic, q2)));
      pc->v_and_f32(t0, t0, mem_ptr(consts, BL_OFFSET_OF(CommonTable::Conic, n_div_2)));
      pc->v_madd_f32(t4, t4, t3, mem_ptr(consts, BL_OFFSET_OF(CommonTable::Conic, q1)));
      pc->v_madd_f32(t4, t4, t3, mem_ptr(consts, BL_OFFSET_OF(CommonTable::Conic, q0)));
      pc->v_msub_f32(t4, t4, t2, t1);

      pc->v_abs_f32(t4, t4);
      pc->v_sub_f32(t4, t4, t0);
      pc->v_abs_f32(t4, t4);

      pc->v_sub_f32(t4, t4, f->by.cloneAs(t4));
      pc->v_abs_f32(t4, t4);
      pc->v_add_f32(t4, t4, f->angleOffset.cloneAs(t4));

      pc->v_cvt_trunc_f32_to_i32(t4, t4);
      pc->v_and_i32(t4, t4, f->maxi.cloneAs(t4));

      fetchMultiplePixels(p, n, flags, t4, IndexLayout::kUInt32Lo16, [&](uint32_t step) noexcept {
        // Don't recalculate anything if this is predicated load, because this is either the end or X will be advanced.
        if (!predicate.empty())
          return;

        switch (step) {
          case 1: if (maxPixels() >= 8 && n.value() == 4) {
                    Vec tmp = pc->newV128("f.tmp");
                    pc->v_dup_hi_f64(tmp, f->xx_inc);
                    pc->s_add_f64(px, px, tmp);
                  }
                  else {
                    pc->s_add_f64(px, px, f->xx_inc);
                  }

                  if (n.value() == 16) {
                    pc->s_add_f64(px, px, f->xx_inc);
                  }
                  break;
          case 2: recalcX();
                  break;
        }
      });

      pc->x_satisfy_pixel(p, flags);
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
