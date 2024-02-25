// This file is part of Blend2D project <https://blend2d.com>
//
// See blend2d.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../../api-build_p.h"
#if !defined(BL_BUILD_NO_JIT)

#include "../../pipeline/jit/fetchutils_p.h"

namespace bl {
namespace Pipeline {
namespace JIT {

// bl::Pipeline::JIT::IndexExtractor
// =================================

IndexExtractor::IndexExtractor(PipeCompiler* pc) noexcept
  : _pc(pc),
    _vec(),
    _mem(),
    _type(kTypeNone),
    _indexSize(0),
    _memSize(0) {}

void IndexExtractor::begin(uint32_t type, const Vec& vec) noexcept {
  BL_ASSERT(type != kTypeNone);
  BL_ASSERT(type < kTypeCount);

#if defined(BL_JIT_ARCH_X86)
  uint32_t vecSize = vec.size();
  Mem mem = _pc->tmpStack(vecSize);

  if (vecSize < 16)
    _pc->v_storeu256(mem, vec);
  else
    _pc->v_storea128(mem, vec);
  begin(type, mem, vec.size());
#else
  _type = type;
  _vec = vec;

  switch (_type) {
    case kTypeInt16:
    case kTypeUInt16:
      _indexSize = 2;
      break;

    case kTypeInt32:
    case kTypeUInt32:
      _indexSize = 4;
      break;

    default:
      BL_NOT_REACHED();
  }
#endif
}

void IndexExtractor::begin(uint32_t type, const Mem& mem, uint32_t memSize) noexcept {
  BL_ASSERT(type != kTypeNone);
  BL_ASSERT(type < kTypeCount);

  _type = type;
  _vec.reset();
  _mem = mem;
  _memSize = uint16_t(memSize);

  switch (_type) {
    case kTypeInt16:
    case kTypeUInt16:
      _indexSize = 2;
      break;

    case kTypeInt32:
    case kTypeUInt32:
      _indexSize = 4;
      break;

    default:
      BL_NOT_REACHED();
  }
}

void IndexExtractor::extract(const Gp& dst, uint32_t index) noexcept {
  BL_ASSERT(dst.size() >= 4);
  BL_ASSERT(_type != kTypeNone);

  if (!_vec.isValid()) {
    BL_ASSERT((index + 1u) * _indexSize <= _memSize);
    Mem m = _mem;
    m.addOffset(int(index * _indexSize));

    switch (_type) {
      case kTypeInt16 : _pc->load_i16(dst, m); return;
      case kTypeUInt16: _pc->load_u16(dst, m); return;
      case kTypeInt32 : _pc->load_i32(dst, m); return;
      case kTypeUInt32: _pc->load_u32(dst, m); return;

      default:
        BL_NOT_REACHED();
    }
  }
  else {
#if defined(BL_JIT_ARCH_X86)
    BL_NOT_REACHED();
#elif defined(BL_JIT_ARCH_A64)
    AsmCompiler* cc = _pc->cc;
    switch (_type) {
      case kTypeInt16 : cc->smov(dst      , _vec.h(index)); return;
      case kTypeUInt16: cc->umov(dst.r32(), _vec.h(index)); return;
      case kTypeInt32 : cc->smov(dst      , _vec.s(index)); return;
      case kTypeUInt32: cc->umov(dst.r32(), _vec.s(index)); return;

      default:
        BL_NOT_REACHED();
    }
#endif
  }
}

// bl::Pipeline::JIT::FetchContext
// ===============================

void FetchContext::_init(PixelCount n) noexcept {
  BL_ASSERT(n == 4 || n == 8);

  _pixel->setCount(n);
  _fetchDone = false;

  // The strategy for fetching alpha pixels is a bit different compared to fetching RGBA pixels.
  // In general we prefer to fetch into a GP accumulator and then convert it to XMM|YMM at the end.
  _a8FetchMode = _fetchFormat == FormatExt::kA8 || _pixel->isA8();

  switch (_pixel->type()) {
    case PixelType::kA8: {
      if (!blTestFlag(_fetchFlags, PixelFlags::kPA_UA_UI))
        _fetchFlags |= PixelFlags::kPA;

      if (blTestFlag(_fetchFlags, PixelFlags::kPA)) {
        _pc->newV128Array(_pixel->pa, 1, "pa");
        aTmp = _pixel->pa[0];
      }
      else {
        _pc->newV128Array(_pixel->ua, 1, "ua");
        aTmp = _pixel->ua[0];
      }
      break;
    }

    case PixelType::kRGBA32: {
      // If no flags were specified, use packed.
      if (!blTestFlag(_fetchFlags, PixelFlags::kPA_UA_UI | PixelFlags::kPC_UC))
        _fetchFlags |= PixelFlags::kPC;

#if defined(BL_JIT_ARCH_X86)
      if (!_pc->hasSSE4_1() && !_a8FetchMode) {
        // We need some temporaries if the CPU doesn't support `SSE4.1`.
        pTmp0 = _pc->newV128("@pTmp0");
        pTmp1 = _pc->newV128("@pTmp1");
      }
#endif // BL_JIT_ARCH_X86

      if (blTestFlag(_fetchFlags, PixelFlags::kPC) || _pc->use256BitSimd()) {
        _pc->newV128Array(_pixel->pc, (n.value() + 3) / 4, "pc");
        aTmp = _pixel->pc[0];
      }
      else {
        _pc->newV128Array(_pixel->uc, (n.value() + 1) / 2, "uc");
        aTmp = _pixel->uc[0];
      }
      break;
    }

    case PixelType::kRGBA64: {
      if (!blTestFlag(_fetchFlags, PixelFlags::kPA_UA_UI | PixelFlags::kPC_UC))
        _fetchFlags |= PixelFlags::kPC;

#if defined(BL_JIT_ARCH_X86)
      if (_pc->use256BitSimd() && n.value() > 2u) {
        _pc->newV256Array(_pixel->uc, (n.value() + 3) / 4, "uc");
        pTmp0 = _pc->newV128("@pTmp0");
      }
      else
#endif // BL_JIT_ARCH_X86
      {
        _pc->newV128Array(_pixel->uc, (n.value() + 1) / 2, "uc");
      }
      break;
    }

    default:
      BL_NOT_REACHED();
  }

  if (_a8FetchMode) {
    if (_pc->is64Bit() && n > 4) {
      aAcc = _pc->newGp64("@aAcc");
      _a8FetchShift = 8;
    }
    else if (_pc->is64Bit() && blTestFlag(_fetchFlags, PixelFlags::kUA | PixelFlags::kUC)) {
      aAcc = _pc->newGp64("@aAcc");
      _a8FetchShift = 16;
    }
    else {
      aAcc = _pc->newGp32("@aAcc");
      _a8FetchShift = 8;
    }
  }
}

void FetchContext::fetchPixel(const Mem& src) noexcept {
  BL_ASSERT(_fetchIndex < _pixel->count().value());

  if (_a8FetchMode) {
    Mem m(src);
    m.setSize(1);

    if (_fetchFormat == FormatExt::kPRGB32)
      m.addOffset(3);

    bool clearAcc = _fetchIndex == 0 || (_fetchIndex == 4 && aAcc.size() == 4);
    bool finalize = _fetchIndex == _pixel->count().value() - 1;

    if (clearAcc)
      _pc->load_u8(aAcc, m);
    else
      _pc->load_merge_u8(aAcc, m);
    _pc->ror(aAcc, aAcc, _a8FetchShift);

    if (finalize) {
      // The last pixel -> Convert to XMM.
      if (aAcc.size() == 8) {
        _pc->s_mov_u64(aTmp, aAcc);
      }
      else if (_fetchIndex == 7) {
#if defined(BL_JIT_ARCH_X86)
        if (!_pc->hasSSE4_1()) {
          Vec aHi = _pc->newV128("@aHi");
          _pc->s_mov_u32(aHi, aAcc);
          _pc->v_interleave_lo_u32(aTmp, aTmp, aHi);
        }
        else
#endif // BL_JIT_ARCH_X86
        {
          _pc->s_insert_u32(aTmp, aAcc, 1);
        }
      }
      else {
        _pc->s_mov_u32(aTmp, aAcc);
      }

      if (_a8FetchShift == 8 && !blTestFlag(_fetchFlags, PixelFlags::kPA | PixelFlags::kPC))
        _pc->v_cvt_u8_to_u16(aTmp, aTmp);
    }
    else if (_fetchIndex == 3 && aAcc.size() == 4) {
      // Not the last pixel, but we have to convert to XMM as we have no more
      // space in the GP accumulator. This only happens in 32-bit mode.
      _pc->s_mov_u32(aTmp, aAcc);
    }
  }
  else if (_pixel->isRGBA32()) {
#if defined(BL_JIT_ARCH_X86)
    if (_pc->use256BitSimd()) {
      Vec& pix = _pixel->pc[_fetchIndex / 4u];
      switch (_fetchIndex) {
        case 0:
        case 4: _pc->v_loada32(pix, src); break;
        case 1:
        case 5: _pc->v_insert_u32(pix, src, 1); break;
        case 2:
        case 6: _pc->v_insert_u32(pix, src, 2); break;
        case 3:
        case 7: _pc->v_insert_u32(pix, src, 3); break;

        default:
          BL_NOT_REACHED();
      }

      if (_fetchIndex == 7) {
        packedFetchDone();
      }
    }
    else
#endif // BL_JIT_ARCH_X86
    {
      bool isPC = !blTestFlag(_fetchFlags, PixelFlags::kUC);
      VecArray& uc = _pixel->uc;

      Vec p0 = isPC ? _pixel->pc[0] : uc[0];
      Vec p1;

      if (_pixel->count() > 4)
        p1 = isPC ? _pixel->pc[1] : uc[2];

#if defined(BL_JIT_ARCH_X86)
      if (!_pc->hasSSE4_1()) {
        switch (_fetchIndex) {
          case 0: _pc->v_loada32(p0, src);
                  break;
          case 1: _pc->v_loada32(pTmp0, src);
                  break;
          case 2: _pc->v_interleave_lo_u32(p0, p0, pTmp0);
                  if (isPC)
                    _pc->v_loada32(pTmp0, src);
                  else
                    _pc->v_loada32(uc[1], src);
                  break;
          case 3: _pc->v_loada32(pTmp1, src);
                  break;

          case 4: if (isPC) {
                    _pc->v_interleave_lo_u32(pTmp0, pTmp0, pTmp1);
                    _pc->v_interleave_lo_u64(p0, p0, pTmp0);
                  }
                  else {
                    _pc->v_interleave_lo_u32(uc[1], uc[1], pTmp1);
                  }
                  _pc->v_loada32(p1, src);
                  break;
          case 5: _pc->v_loada32(pTmp0, src);
                  break;
          case 6: _pc->v_interleave_lo_u32(p1, p1, pTmp0);
                  if (isPC)
                    _pc->v_loada32(pTmp0, src);
                  else
                    _pc->v_loada32(uc[3], src);
                  break;
          case 7: _pc->v_loada32(pTmp1, src);
                  break;
        }
      }
      else
#endif
      {
        switch (_fetchIndex) {
          case 0: _pc->v_loada32(p0, src);
                  break;
          case 1: _pc->v_insert_u32(p0, src, 1);
                  break;
          case 2: if (isPC)
                    _pc->v_insert_u32(p0, src, 2);
                  else
                    _pc->v_loada32(uc[1], src);
                  break;
          case 3: if (isPC)
                    _pc->v_insert_u32(p0, src, 3);
                  else
                    _pc->v_insert_u32(uc[1], src, 1);
                  break;

          case 4: _pc->v_loada32(p1, src);
                  break;
          case 5: _pc->v_insert_u32(p1, src, 1);
                  break;
          case 6: if (isPC)
                    _pc->v_insert_u32(p1, src, 2);
                  else
                    _pc->v_loada32(uc[3], src);
                  break;
          case 7: if (isPC)
                    _pc->v_insert_u32(p1, src, 3);
                  else
                    _pc->v_insert_u32(uc[3], src, 1);
                  break;
        }
      }
    }
  }
  else if (_pixel->isRGBA64()) {
#if defined(BL_JIT_ARCH_X86)
    if (_pc->use256BitSimd()) {
      Vec pix;
      if ((_fetchIndex & 0x3u) < 2u)
        pix = _pixel->uc[_fetchIndex / 4u].xmm();
      else
        pix = pTmp0;

      if ((_fetchIndex & 0x1u) == 0u)
        _pc->v_loadu64(pix, src);
      else
        _pc->v_insert_u64(pix, src, 1);

      if ((_fetchIndex & 0x3u) == 0x3u) {
        Vec pYmm = _pixel->uc[_fetchIndex / 4u].ymm();
        _pc->v_insert_v128_u32(pYmm, pYmm, pTmp0, 1);
      }
    }
    else
#endif // BL_JIT_ARCH_X86
    {
      Vec pix = _pixel->uc[_fetchIndex / 2u];
      if ((_fetchIndex & 0x1u) == 0u)
        _pc->v_loadu64(pix, src);
      else
        _pc->v_insert_u64(pix, src, 1);
    }

    _fetchDone = _fetchIndex + 1 == _pixel->count().value();
  }

  _fetchIndex++;
}

void FetchContext::_fetchAll(const Mem& src, uint32_t srcShift, IndexExtractor& extractor, const uint8_t* indexes, InterleaveCallback cb, void* cbData) noexcept {
  BL_ASSERT(_fetchIndex == 0);

  Gp idx0 = _pc->newGpPtr("@idx0");
  Gp idx1 = _pc->newGpPtr("@idx1");

  Mem src0 = src;
  Mem src1 = src;

  src0.setIndex(idx0, srcShift);
  src1.setIndex(idx1, srcShift);

  switch (_pixel->count().value()) {
    case 2: {
      extractor.extract(idx0, indexes[0]);
      extractor.extract(idx1, indexes[1]);

      cb(0, cbData);
      fetchPixel(src0);

      cb(1, cbData);
      fetchPixel(src1);

      cb(0xFF, cbData);
      break;
    }

    case 4: {
      extractor.extract(idx0, indexes[0]);
      extractor.extract(idx1, indexes[1]);

      cb(0, cbData);
      fetchPixel(src0);
      extractor.extract(idx0, indexes[2]);

      cb(1, cbData);
      fetchPixel(src1);
      extractor.extract(idx1, indexes[3]);

      cb(2, cbData);
      fetchPixel(src0);

      cb(3, cbData);
      fetchPixel(src1);

      cb(0xFF, cbData);
      break;
    }

    case 8: {
#if defined(BL_JIT_ARCH_X86)
      bool isPC = _pc->use256BitSimd() || (_pc->hasSSE4_1() && !blTestFlag(_fetchFlags, PixelFlags::kUC));
#else
      bool isPC = !blTestFlag(_fetchFlags, PixelFlags::kUC);
#endif

      if (isPC && blFormatInfo[size_t(_fetchFormat)].depth == 32) {
        Vec& pc0 = _pixel->pc[0];
        Vec& pc1 = _pixel->pc[1];

        extractor.extract(idx0, indexes[0]);
        extractor.extract(idx1, indexes[4]);

        cb(0, cbData);
        _pc->v_loada32(pc0, src0);
        extractor.extract(idx0, indexes[1]);

        cb(1, cbData);
        _pc->v_loada32(pc1, src1);
        extractor.extract(idx1, indexes[5]);

        cb(2, cbData);
        _pc->v_insert_u32(pc0, src0, 1);
        extractor.extract(idx0, indexes[2]);

        cb(3, cbData);
        _pc->v_insert_u32(pc1, src1, 1);
        extractor.extract(idx1, indexes[6]);

        cb(4, cbData);
        _pc->v_insert_u32(pc0, src0, 2);
        extractor.extract(idx0, indexes[3]);

        cb(5, cbData);
        _pc->v_insert_u32(pc1, src1, 2);
        extractor.extract(idx1, indexes[7]);

        cb(6, cbData);
        _pc->v_insert_u32(pc0, src0, 3);

        cb(7, cbData);
        _pc->v_insert_u32(pc1, src1, 3);

        cb(0xFF, cbData);
        _fetchIndex = 8;
        packedFetchDone();
      }
      else {
        extractor.extract(idx0, indexes[0]);
        extractor.extract(idx1, indexes[1]);

        cb(0, cbData);
        fetchPixel(src0);
        extractor.extract(idx0, indexes[2]);

        cb(1, cbData);
        fetchPixel(src1);
        extractor.extract(idx1, indexes[3]);

        cb(2, cbData);
        fetchPixel(src0);
        extractor.extract(idx0, indexes[4]);

        cb(3, cbData);
        fetchPixel(src1);
        extractor.extract(idx1, indexes[5]);

        cb(4, cbData);
        fetchPixel(src0);
        extractor.extract(idx0, indexes[6]);

        cb(5, cbData);
        fetchPixel(src1);
        extractor.extract(idx1, indexes[7]);

        cb(6, cbData);
        fetchPixel(src0);

        cb(7, cbData);
        fetchPixel(src1);

        cb(0xFF, cbData);
      }
      break;
    }

    default:
      BL_NOT_REACHED();
  }
}

void FetchContext::packedFetchDone() noexcept {
#if defined(BL_JIT_ARCH_X86)
  if (!_pixel->pc[0].isVec128()) {
    if (blTestFlag(_fetchFlags, PixelFlags::kPC)) {
      VecArray pc;
      _pc->newV256Array(pc, 1, _pixel->name(), "pc");
      _pc->v_insert_v128_u32(pc[0], _pixel->pc[0], _pixel->pc[1], 1);
      _pixel->pc = pc;
    }
    else {
      VecArray uc;
      _pc->newV256Array(uc, 2, _pixel->name(), "uc");
      _pc->v_cvt_u8_to_u16(uc, _pixel->pc);

      _pixel->pc.reset();
      _pixel->uc = uc;
    }
  }
#endif

  _fetchDone = true;
}

void FetchContext::end() noexcept {
  uint32_t n = _pixel->count().value();

  BL_ASSERT(n != 0);
  BL_ASSERT(n == _fetchIndex);

  if (_fetchDone)
    return;

  if (_a8FetchMode) {
    if (_pixel->isRGBA32()) {
      if (blTestFlag(_fetchFlags, PixelFlags::kPC)) {
        switch (n) {
          case 4: {
            Vec& a0 = _pixel->pc[0];

            _pc->v_interleave_lo_u8(a0, a0, a0);
            _pc->v_interleave_lo_u16(a0, a0, a0);
            break;
          }

          case 8: {
            Vec& a0 = _pixel->pc[0];
            Vec& a1 = _pixel->pc[1];

            _pc->v_interleave_hi_u8(a1, a0, a0);
            _pc->v_interleave_lo_u8(a0, a0, a0);
            _pc->v_interleave_hi_u16(a1, a1, a1);
            _pc->v_interleave_lo_u16(a0, a0, a0);
            break;
          }

          default:
            BL_NOT_REACHED();
        }
      }
      else {
        switch (n) {
          case 4: {
            Vec& a0 = _pixel->uc[0];
            Vec& a1 = _pixel->uc[1];

            _pc->v_interleave_lo_u16(a0, a0, a0);

            _pc->v_swizzle_u32x4(a1, a0, swizzle(3, 3, 2, 2));
            _pc->v_swizzle_u32x4(a0, a0, swizzle(1, 1, 0, 0));
            break;
          }

          case 8: {
            Vec& a0 = _pixel->uc[0];
            Vec& a1 = _pixel->uc[1];
            Vec& a2 = _pixel->uc[2];
            Vec& a3 = _pixel->uc[3];

            _pc->v_interleave_hi_u16(a2, a0, a0);
            _pc->v_interleave_lo_u16(a0, a0, a0);

            _pc->v_swizzle_u32x4(a3, a2, swizzle(3, 3, 2, 2));
            _pc->v_swizzle_u32x4(a1, a0, swizzle(3, 3, 2, 2));
            _pc->v_swizzle_u32x4(a2, a2, swizzle(1, 1, 0, 0));
            _pc->v_swizzle_u32x4(a0, a0, swizzle(1, 1, 0, 0));
            break;
          }

          default:
            BL_NOT_REACHED();
        }
      }
    }
    else {
      // Nothing...
    }
  }
  else {
#if defined(BL_JIT_ARCH_X86)
    if (!_pc->hasSSE4_1()) {
      if (blTestFlag(_fetchFlags, PixelFlags::kPC)) {
        const Vec& pcLast = _pixel->pc[_pixel->pc.size() - 1];
        _pc->v_interleave_lo_u32(pTmp0, pTmp0, pTmp1);
        _pc->v_interleave_lo_u64(pcLast, pcLast, pTmp0);
      }
      else {
        const Vec& ucLast = _pixel->uc[_pixel->uc.size() - 1];
        _pc->v_interleave_lo_u32(ucLast, ucLast, pTmp1);
      }
    }
#endif // BL_JIT_ARCH_X86

    if (!blTestFlag(_fetchFlags, PixelFlags::kPC)) {
      _pc->v_cvt_u8_to_u16(_pixel->uc, _pixel->uc);
    }
  }

  _fetchDone = true;
}

// bl::Pipeline::JIT::FetchUtils
// =============================

void FetchUtils::x_gather_pixels(PipeCompiler* pc, Pixel& p, PixelCount n, FormatExt format, PixelFlags flags, const Mem& src, const Vec& idx, uint32_t shift, IndexLayout indexLayout, InterleaveCallback cb, void* cbData) noexcept {
  Mem mem(src);

#if defined(BL_JIT_ARCH_X86)
  if (pc->hasOptFlag(PipeOptFlags::kFastGather)) {
    // NOTE: Gathers are provided by AVX2 and later, thus if we are here it means at least AVX2 is available.
    AsmCompiler* cc = pc->cc;
    uint32_t count = p.count().value();

    if (blFormatInfo[size_t(format)].depth == 32) {
      VecArray pixels;

      if (n <= 4u)
        pc->newV128Array(pixels, 1, p.name(), "pc");
      else if (n <= 8u)
        pc->newV256Array(pixels, 1, p.name(), "pc");
      else
        pc->newV512Array(pixels, 1, p.name(), "pc");

      Vec gatherIndex = idx.cloneAs(pixels[0]);

      switch (indexLayout) {
        case IndexLayout::kUInt16:
          gatherIndex = pc->newSimilarReg(pixels[0], "gatherIndex");
          cc->vpmovzxwd(gatherIndex, idx.xmm());
          break;

        case IndexLayout::kUInt32:
        case IndexLayout::kUInt32Lo16:
          // UInt32Lo16 expects that the high part is zero, so we can treat it as 32-bit index.
          break;

        case IndexLayout::kUInt32Hi16:
          gatherIndex = pc->newSimilarReg(pixels[0], "gatherIndex");
          pc->v_srli_u32(gatherIndex, idx.cloneAs(gatherIndex), 16);
          break;

        default:
          BL_NOT_REACHED();
      }

      mem.setIndex(gatherIndex);
      mem.setShift(shift);

      pc->v_zero_i(pixels[0]);
      if (pc->hasAVX512()) {
        KReg pred = cc->newKw("pred");
        cc->kxnorw(pred, pred, pred);
        cc->k(pred).vpgatherdd(pixels[0], mem);
      }
      else {
        Vec pred = pc->newSimilarReg(pixels[0], "pred");
        pc->v_ones_i(pred);
        cc->vpgatherdd(pixels[0], mem, pred);
      }

      for (uint32_t i = 0; i < count; i++)
        cb(i, cbData);

      x_convert_gathered_pixels(pc, p, n, flags, pixels);
      cb(0xFF, cbData);
      return;
    }

    if (blFormatInfo[size_t(format)].depth == 64) {
      VecArray pixels;

      if (n <= 4u) {
        pc->newV256Array(pixels, 1, p.name(), "pc");
      }
      else {
        if (pc->use512BitSimd()) {
          pc->newV512Array(pixels, n.value() / 8, p.name(), "pc");
        }
        else {
          pc->newV256Array(pixels, 2, p.name(), "pc");
        }
      }

      Vec gatherIndex = idx.cloneAs(pixels[0]);

      switch (indexLayout) {
        case IndexLayout::kUInt16:
          gatherIndex = pc->newSimilarReg(pixels[0], "gatherIndex");
          cc->vpmovzxwd(gatherIndex, idx.xmm());
          break;

        case IndexLayout::kUInt32:
        case IndexLayout::kUInt32Lo16:
          // UInt32Lo16 expects that the high part is zero, so we can treat it as 32-bit index.
          break;

        case IndexLayout::kUInt32Hi16:
          gatherIndex = pc->newSimilarReg(pixels[0], "gatherIndex");
          pc->v_srli_u32(gatherIndex, idx.cloneAs(gatherIndex), 16);
          break;

        default:
          BL_NOT_REACHED();
      }

      if (pc->use512BitSimd() && n.value() == 16)
        mem.setIndex(gatherIndex.ymm());
      else
        mem.setIndex(gatherIndex.xmm());
      mem.setShift(shift);

      for (uint32_t i = 0; i < pixels.size(); i++) {
        if (i == 1) {
          if (pc->use512BitSimd() && n.value() == 16) {
            Vec gi2 = pc->newSimilarReg(gatherIndex, "gatherIndex2");
            cc->vextracti32x8(gi2.ymm(), gatherIndex.zmm(), 1);
            mem.setIndex(gi2.ymm());
          }
          else {
            Vec gi2 = pc->newSimilarReg(gatherIndex, "gatherIndex2");
            cc->vextracti128(gi2.xmm(), gatherIndex.ymm(), 1);
            mem.setIndex(gi2.xmm());
          }
        }

        pc->v_zero_i(pixels[i]);
        if (pc->hasAVX512()) {
          KReg pred = cc->newKw("pred");
          cc->kxnorw(pred, pred, pred);
          cc->k(pred).vpgatherdq(pixels[i], mem);
        }
        else {
          Vec pred = pc->newSimilarReg(pixels[i], "pred");
          pc->v_ones_i(pred);
          cc->vpgatherdq(pixels[i], mem, pred);
        }

        for (uint32_t step = 0; step < 4; step++)
          cb(i * 4 + step, cbData);
      }

      x_convert_gathered_pixels(pc, p, n, flags, pixels);
      cb(0xFF, cbData);
      return;
    }
  }
#endif // BL_JIT_ARCH_X86

  uint32_t indexType = 0;
  const uint8_t* indexSequence = nullptr;

  static const uint8_t oddIndexes[] = { 1, 3, 5, 7, 9, 11, 13, 15 };
  static const uint8_t evenIndexes[] = { 0, 2, 4, 6, 8, 10, 12, 14 };
  static const uint8_t consecutiveIndexes[] = { 0, 1, 2, 3, 4, 5, 6, 7 };

  switch (indexLayout) {
    case IndexLayout::kUInt16:
      indexType = IndexExtractor::kTypeUInt16;
      indexSequence = consecutiveIndexes;
      break;

    case IndexLayout::kUInt32:
      indexType = IndexExtractor::kTypeUInt32;
      indexSequence = consecutiveIndexes;
      break;

    case IndexLayout::kUInt32Lo16:
      indexType = IndexExtractor::kTypeUInt16;
      indexSequence = evenIndexes;
      break;

    case IndexLayout::kUInt32Hi16:
      indexType = IndexExtractor::kTypeUInt16;
      indexSequence = oddIndexes;
      break;

    default:
      BL_NOT_REACHED();
  }

  IndexExtractor indexExtractor(pc);
  indexExtractor.begin(indexType, idx);

  FetchContext fCtx(pc, &p, n, format, flags);
  fCtx._fetchAll(src, shift, indexExtractor, indexSequence, cb, cbData);
  fCtx.end();
}

void FetchUtils::x_convert_gathered_pixels(PipeCompiler* pc, Pixel& p, PixelCount n, PixelFlags flags, const VecArray& gPix) noexcept {
  if (p.isA8()) {
    pc->v_srli_u32(gPix, gPix, 24);

    if (blTestFlag(flags, PixelFlags::kPA)) {
      SimdWidth paSimdWidth = pc->simdWidthOf(DataWidth::k8, n);
      uint32_t paRegCount = pc->regCountOf(DataWidth::k8, n);

      pc->newVecArray(p.pa, paRegCount, paSimdWidth, p.name(), "pa");
      BL_ASSERT(p.pa.size() == 1);

#if defined(BL_JIT_ARCH_X86)
      if (pc->hasAVX512()) {
        pc->cc->vpmovdb(p.pa[0], gPix[0]);
      }
      else
#endif // BL_JIT_ARCH_X86
      {
        pc->x_packs_i16_u8(p.pa[0].cloneAs(gPix[0]), gPix[0], gPix[0]);
        pc->x_packs_i16_u8(p.pa[0], p.pa[0], p.pa[0]);
      }
    }
    else {
      SimdWidth uaSimdWidth = pc->simdWidthOf(DataWidth::k16, n);
      uint32_t uaRegCount = pc->regCountOf(DataWidth::k16, n);

      pc->newVecArray(p.ua, uaRegCount, uaSimdWidth, p.name(), "ua");
      BL_ASSERT(p.ua.size() == 1);

#if defined(BL_JIT_ARCH_X86)
      if (pc->hasAVX512()) {
        pc->cc->vpmovdw(p.ua[0], gPix[0]);
      }
      else
#endif // BL_JIT_ARCH_X86
      {
        pc->x_packs_i16_u8(p.ua[0].cloneAs(gPix[0]), gPix[0], gPix[0]);
      }
    }
  }
  else if (p.isRGBA32()) {
    p.pc = gPix;
    pc->rename(p.pc, p.name(), "pc");
  }
  else {
#if defined(BL_JIT_ARCH_X86)
    if (!pc->use256BitSimd() && gPix[0].isVec256()) {
      Vec uc1 = pc->newV128(p.name(), "uc1");
      p.uc.init(gPix[0].xmm(), uc1);
      pc->cc->vextracti128(uc1, gPix[0], 1);
    }
    else
#endif // BL_JIT_ARCH_X86
    {
      p.uc = gPix;
      pc->rename(p.uc, p.name(), "uc");
    }
  }
}

} // {JIT}
} // {Pipeline}
} // {bl}

#endif // !BL_BUILD_NO_JIT
