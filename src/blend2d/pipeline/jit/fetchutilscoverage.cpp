// This file is part of Blend2D project <https://blend2d.com>
//
// See blend2d.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../../api-build_p.h"
#if !defined(BL_BUILD_NO_JIT)

#include "../../pipeline/jit/fetchutilscoverage_p.h"

namespace bl {
namespace Pipeline {
namespace JIT {
namespace FetchUtils {

// bl::Pipeline::JIT::FetchUtils - Init & Pass Vec Coverage
// ========================================================

// TODO: REMOVE_THIS_ADD_THIS_TO_FILLPART

static uint32_t calculateCoverageByteCount(PixelCount pixelCount, PixelType pixelType, PixelCoverageFormat coverageFormat) noexcept {
  DataWidth dataWidth = DataWidth::k8;

  switch (coverageFormat) {
    case PixelCoverageFormat::kPacked:
      dataWidth = DataWidth::k8;
      break;

    case PixelCoverageFormat::kUnpacked:
      dataWidth = DataWidth::k16;
      break;

    default:
      BL_NOT_REACHED();
  }

  uint32_t count = pixelCount.value();
  switch (pixelType) {
    case PixelType::kA8:
      break;

    case PixelType::kRGBA32:
      count *= 4u;
      break;

    default:
      BL_NOT_REACHED();
  }

  return (1u << uint32_t(dataWidth)) * count;
}

void initVecCoverage(
  PipeCompiler* pc,
  VecArray& dst,
  PixelCount maxPixelCount,
  SimdWidth maxSimdWidth,
  PixelType pixelType,
  PixelCoverageFormat coverageFormat) noexcept {

  uint32_t coverageByteCount = calculateCoverageByteCount(maxPixelCount, pixelType, coverageFormat);
  SimdWidth simdWidth = SimdWidthUtils::simdWidthForByteCount(maxSimdWidth, coverageByteCount);
  uint32_t vecCount = SimdWidthUtils::vecCountForByteCount(simdWidth, coverageByteCount);

  pc->newVecArray(dst, vecCount, simdWidth, "vm");
}

void passVecCoverage(
  VecArray& dst,
  const VecArray& src,
  PixelCount pixelCount,
  PixelType pixelType,
  PixelCoverageFormat coverageFormat) noexcept {

  uint32_t coverageByteCount = calculateCoverageByteCount(pixelCount, pixelType, coverageFormat);
  SimdWidth simdWidth = SimdWidthUtils::simdWidthForByteCount(SimdWidthUtils::simdWidthOf(src[0]), coverageByteCount);
  uint32_t vecCount = SimdWidthUtils::vecCountForByteCount(simdWidth, coverageByteCount);

  // We can use at most what was given to us, or less in case that the current
  // `pixelCount` is less than `maxPixelCount` passed to `initVecCoverage()`.
  BL_ASSERT(vecCount <= src.size());

  dst._size = vecCount;
  for (uint32_t i = 0; i < vecCount; i++) {
    dst.v[i].reset();
    dst.v[i].as<asmjit::BaseReg>().setSignatureAndId(SimdWidthUtils::signatureOf(simdWidth), src.v[i].id());
  }
}

// bl::Pipeline::JIT::FetchUtils - Expand Coverages
// ================================================

static void expandCoveragesToA8(
  PipeCompiler* pc,
  const VecArray& cov,
  PixelCount pixelCount,
  PixelCoverageFormat dstFormat,
  PixelCoverageSource srcFormat) noexcept {

  switch (dstFormat) {
    case PixelCoverageFormat::kPacked: {
      if (srcFormat == PixelCoverageSource::kPackedConsecutive) {
        return;
      }

      if (srcFormat == PixelCoverageSource::kUnpackedConsecutive || srcFormat == PixelCoverageSource::kUnpackedHalfLo128) {
        if (pixelCount <= 8u) {
          Vec v = cov[0].v128();
          pc->v_packs_i16_u8(v, v, v);
          return;
        }

        UNIMPLEMENTED();
      }

      BL_NOT_REACHED();
    }

    case PixelCoverageFormat::kUnpacked: {
      if (srcFormat == PixelCoverageSource::kPackedConsecutive) {
        if (pixelCount <= 8u) {
          Vec v = cov[0].v128();
          pc->v_cvt_u8_to_u16(v, v);
          return;
        }

        UNIMPLEMENTED();

        return;
      }

      if (srcFormat == PixelCoverageSource::kUnpackedConsecutive) {
        return;
      }

      if (srcFormat == PixelCoverageSource::kUnpackedHalfLo128) {
        if (pixelCount <= 4)
          return;

        // We have to convert from:
        //   cov = [?? ?? ?? ?? a7 a6 a5 a4|?? ?? ?? ?? a3 a2 a1 a0]
        // To:
        //   cov = [a7 a6 a5 a4 a3 a2 a1 a0|a7 a6 a5 a4 a3 a2 a1 a0]
        pc->v_swizzle_u64x4(cov, cov, swizzle(2, 0, 2, 0));
        return;
      }

      BL_NOT_REACHED();
    }

    default:
      BL_NOT_REACHED();
  }
}

static void expandCoveragesToRGBA32(
  PipeCompiler* pc,
  const VecArray& cov,
  PixelCount pixelCount,
  PixelCoverageFormat dstFormat,
  PixelCoverageSource srcFormat) noexcept {

  switch (dstFormat) {
    case PixelCoverageFormat::kPacked: {
      if (srcFormat == PixelCoverageSource::kPackedConsecutive) {
        return;
      }

      if (srcFormat == PixelCoverageSource::kUnpackedConsecutive || srcFormat == PixelCoverageSource::kUnpackedHalfLo128) {
        for (uint32_t i = 0; i < cov.size(); i++) {
          Vec v = cov[i].v128();
          pc->v_swizzlev_u8(v, v, pc->simdConst(&pc->ct.pshufb_xxx3xxx2xxx1xxx0_to_3333222211110000, Bcst::kNA, v));
        }
        return;
      }

      UNIMPLEMENTED();
    }

    case PixelCoverageFormat::kUnpacked: {
      if (srcFormat == PixelCoverageSource::kPackedConsecutive) {
        UNIMPLEMENTED();

        return;
      }

      if (srcFormat == PixelCoverageSource::kUnpackedConsecutive || srcFormat == PixelCoverageSource::kUnpackedHalfLo128) {
        if (pixelCount == 4) {
          Vec cov0_128 = cov[0].v128();

          pc->v_interleave_lo_u16(cov0_128, cov0_128, cov0_128);      //   cov[0] = [a3 a3 a2 a2 a1 a1 a0 a0]
#if defined(BL_JIT_ARCH_X86)
          if (cov[0].isVec256())
          {
            pc->v_swizzle_u64x4(cov[0], cov[0], swizzle(1, 1, 0, 0)); //   cov[0] = [a3 a3 a2 a2 a3 a3 a2 a2|a1 a1 a0 a0 a1 a1 a0 a0]
            pc->v_swizzle_u32x4(cov[0], cov[0], swizzle(1, 1, 0, 0)); //   cov[0] = [a3 a3 a3 a3 a2 a2 a2 a2|a1 a1 a1 a1 a0 a0 a0 a0]
          }
          else
#endif
          {
            pc->v_swizzle_u32x4(cov[1], cov[0], swizzle(3, 3, 2, 2)); //   cov[0] = [a3 a3 a3 a3 a2 a2 a2 a2]
            pc->v_swizzle_u32x4(cov[0], cov[0], swizzle(1, 1, 0, 0)); //   cov[0] = [a1 a1 a1 a1 a0 a0 a0 a0]
          }

          return;
        }

        if (pixelCount == 8) {
          pc->v_interleave_lo_u16(cov[0], cov[0], cov[0]);            //   cov[0] = [a7 a7 a6 a6 a5 a5 a4 a4|a3 a3 a2 a2 a1 a1 a0 a0]
          pc->v_swizzle_u64x4(cov[1], cov[0], swizzle(3, 3, 2, 2));   //   cov[1] = [a7 a7 a6 a6 a7 a7 a6 a6|a5 a5 a4 a4 a5 a5 a4 a4]
          pc->v_swizzle_u64x4(cov[0], cov[0], swizzle(1, 1, 0, 0));   //   cov[0] = [a3 a3 a2 a2 a3 a3 a2 a2|a1 a1 a0 a0 a1 a1 a0 a0]
          pc->v_interleave_lo_u32(cov[0], cov[0], cov[0]);            //   cov[0] = [a3 a3 a3 a3 a2 a2 a2 a2|a1 a1 a1 a1 a0 a0 a0 a0]
          pc->v_interleave_lo_u32(cov[1], cov[1], cov[1]);            //   cov[1] = [a7 a7 a7 a7 a6 a6 a6 a6|a5 a5 a5 a5 a4 a4 a4 a4]

          return;
        }
      }

      BL_NOT_REACHED();
    }

    default:
      BL_NOT_REACHED();
  }
}

void expandCoverages(
  PipeCompiler* pc,
  const VecArray& cov,
  PixelCount pixelCount,
  PixelType pixelType,
  PixelCoverageFormat dstFormat,
  PixelCoverageSource srcFormat) noexcept {

  switch (pixelType) {
    case PixelType::kA8: {
      expandCoveragesToA8(pc, cov, pixelCount, dstFormat, srcFormat);
      return;
    }

    case PixelType::kRGBA32: {
      expandCoveragesToRGBA32(pc, cov, pixelCount, dstFormat, srcFormat);
      return;
    }

    default:
      BL_NOT_REACHED();
  }
}

} // {FetchUtils}
} // {JIT}
} // {Pipeline}
} // {bl}

#endif // !BL_BUILD_NO_JIT
