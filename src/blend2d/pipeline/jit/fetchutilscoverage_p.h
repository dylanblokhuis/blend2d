// This file is part of Blend2D project <https://blend2d.com>
//
// See blend2d.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef BLEND2D_PIPELINE_JIT_FETCHUTILSCOVERAGE_P_H_INCLUDED
#define BLEND2D_PIPELINE_JIT_FETCHUTILSCOVERAGE_P_H_INCLUDED

#include "../../pipeline/jit/pipecompiler_p.h"
#include "../../pipeline/jit/pipeprimitives_p.h"

//! \cond INTERNAL
//! \addtogroup blend2d_pipeline_jit
//! \{

namespace bl {
namespace Pipeline {
namespace JIT {
namespace FetchUtils {

void initVecCoverage(
  PipeCompiler* pc,
  VecArray& dst,
  PixelCount maxPixelCount,
  SimdWidth maxSimdWidth,
  PixelType pixelType,
  PixelCoverageFormat coverageFormat) noexcept;

void passVecCoverage(
  VecArray& dst,
  const VecArray& src,
  PixelCount pixelCount,
  PixelType pixelType,
  PixelCoverageFormat coverageFormat) noexcept;

//! Expands coverages described by `srcFormat` into `dstFormat`, which was requested by the compositor.
void expandCoverages(
  PipeCompiler* pc,
  const VecArray& cov,
  PixelCount pixelCount,
  PixelType pixelType,
  PixelCoverageFormat dstFormat,
  PixelCoverageSource srcFormat) noexcept;

} // {FetchUtils}
} // {JIT}
} // {Pipeline}
} // {bl}

//! \}
//! \endcond

#endif // BLEND2D_PIPELINE_JIT_FETCHUTILSCOVERAGE_P_H_INCLUDED
