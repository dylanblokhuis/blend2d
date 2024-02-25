// This file is part of Blend2D project <https://blend2d.com>
//
// See blend2d.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../../api-build_test_p.h"
#if defined(BL_TEST) && !defined(BL_BUILD_NO_JIT)

#include "../../random_p.h"
#include "../../pipeline/jit/pipecompiler_p.h"

// bl::Pipeline::JIT - Tests
// =========================

namespace bl {
namespace Pipeline {
namespace JIT {
namespace Tests {

// bl::Pipeline::JIT - Tests - Constants
// =====================================

static constexpr uint64_t kRandomSeed = 0x1234u;
static constexpr uint32_t kTestIterCount = 1000u;

static BL_INLINE_NODEBUG constexpr uint32_t vecWidthFromSimdWidth(SimdWidth simdWidth) noexcept {
  return 16u << uint32_t(simdWidth);
}

// bl::Pipeline::JIT - Tests - JIT Function Prototypes
// ===================================================

typedef uint32_t (*TestCondRRFunc)(int32_t a, int32_t b);
typedef uint32_t (*TestCondRIFunc)(int32_t a);

typedef void (*TestMFunc)(void* ptr);
typedef uintptr_t (*TestRMFunc)(uintptr_t reg, void* ptr);
typedef void (*TestMRFunc)(void* ptr, uintptr_t reg);

typedef uint32_t (*TestRRFunc)(uint32_t a);
typedef uint32_t (*TestRRRFunc)(uint32_t a, uint32_t b);
typedef uint32_t (*TestRRIFunc)(uint32_t a);

typedef void (*TestVVFunc)(void* dst, const void* src);
typedef void (*TestVVVFunc)(void* dst, const void* src1, const void* src2);
typedef void (*TestVVVVFunc)(void* dst, const void* src1, const void* src2, const void* src3);

// bl::Pipeline::JIT - Tests - JIT Context Error Handler
// =====================================================

class TestErrorHandler : public asmjit::ErrorHandler {
public:
  TestErrorHandler() noexcept {}
  ~TestErrorHandler() noexcept override {}

  void handleError(asmjit::Error err, const char* message, asmjit::BaseEmitter* origin) override {
    blUnused(origin);
    EXPECT_EQ(err, asmjit::kErrorOk)
      .message("AsmJit Error: %s", message);
  }
};
// bl::Pipeline::JIT - Tests - JIT Context for Testing
// ===================================================

class JitContext {
public:
  asmjit::JitRuntime rt;
  asmjit::CpuFeatures features {};
  PipeOptFlags optFlags {};

  asmjit::StringLogger logger;
  // asmjit::FileLogger fl;

  TestErrorHandler eh;
  asmjit::CodeHolder code;
  AsmCompiler cc;

  void prepare() noexcept {
    logger.clear();

    code.init(rt.environment());
    code.setErrorHandler(&eh);

    // fl.setFile(stdout);
    // fl.addFlags(asmjit::FormatFlags::kMachineCode);
    // code.setLogger(&fl);
    code.setLogger(&logger);

    code.attach(&cc);
    cc.addDiagnosticOptions(asmjit::DiagnosticOptions::kRAAnnotate);
    cc.addDiagnosticOptions(asmjit::DiagnosticOptions::kValidateAssembler);
    cc.addDiagnosticOptions(asmjit::DiagnosticOptions::kValidateIntermediate);
  }

  template<typename Fn>
  Fn finish() noexcept {
    Fn fn;
    EXPECT_EQ(cc.finalize(), asmjit::kErrorOk);
    EXPECT_EQ(rt.add(&fn, &code), asmjit::kErrorOk);
    code.reset();
    return fn;
  }
};

// bl::Pipeline::JIT - Tests - Conditional Operations - Functions
// ==============================================================

static TestCondRRFunc create_func_cond_rr(JitContext& ctx, OpcodeCond op, CondCode condCode, uint32_t variation) noexcept {
  ctx.prepare();
  PipeCompiler pc(&ctx.cc, ctx.features, ctx.optFlags);

  asmjit::FuncNode* node = ctx.cc.newFunc(asmjit::FuncSignature::build<uint32_t, int32_t, int32_t>());
  EXPECT_NOT_NULL(node);

  pc.initSimdWidth(SimdWidth::k128);
  pc.initFunction(node);

  Gp a = pc.newGp32("a");
  Gp b = pc.newGp32("b");
  Gp result = pc.newGp32("result");

  node->setArg(0, a);
  node->setArg(1, b);

  switch (variation) {
    case 0: {
      // Test a conditional branch based on the given condition.
      Label done = pc.newLabel();
      pc.mov(result, 1);
      pc.j(done, Condition(op, condCode, a, b));
      pc.mov(result, 0);
      pc.bind(done);
      break;
    }

    case 1: {
      // Test a cmov functionality.
      Gp trueValue = pc.newGp32("trueValue");
      pc.mov(result, 0);
      pc.mov(trueValue, 1);
      pc.cmov(result, trueValue, Condition(op, condCode, a, b));
      break;
    }

    case 2: {
      // Test a select functionality.
      Gp falseValue = pc.newGp32("falseValue");
      Gp trueValue = pc.newGp32("trueValue");
      pc.mov(falseValue, 0);
      pc.mov(trueValue, 1);
      pc.select(result, trueValue, falseValue, Condition(op, condCode, a, b));
      break;
    }
  }

  ctx.cc.ret(result);
  ctx.cc.endFunc();

  return ctx.finish<TestCondRRFunc>();
}

static TestCondRIFunc create_func_cond_ri(JitContext& ctx, OpcodeCond op, CondCode condCode, Imm bImm) noexcept {
  ctx.prepare();
  PipeCompiler pc(&ctx.cc, ctx.features, ctx.optFlags);

  asmjit::FuncNode* node = ctx.cc.newFunc(asmjit::FuncSignature::build<uint32_t, int32_t>());
  EXPECT_NOT_NULL(node);

  pc.initSimdWidth(SimdWidth::k128);
  pc.initFunction(node);

  Gp a = pc.newGp32("a");
  Gp result = pc.newGp32("result");
  Label done = pc.newLabel();

  node->setArg(0, a);
  pc.mov(result, 1);
  pc.j(done, Condition(op, condCode, a, bImm));
  pc.mov(result, 0);
  pc.bind(done);
  ctx.cc.ret(result);

  ctx.cc.endFunc();
  return ctx.finish<TestCondRIFunc>();
}

// bl::Pipeline::JIT - Tests - Conditional Operations - Runner
// ===========================================================

static BL_NOINLINE void test_conditional_op(JitContext& ctx, OpcodeCond op, CondCode condCode, int32_t a, int32_t b, bool expected) noexcept {
  for (uint32_t variation = 0; variation < 3; variation++) {
    TestCondRRFunc fn_rr = create_func_cond_rr(ctx, op, condCode, variation);
    TestCondRIFunc fn_ri = create_func_cond_ri(ctx, op, condCode, b);

    uint32_t observed_rr = fn_rr(a, b);
    EXPECT_EQ(observed_rr, uint32_t(expected))
      .message("Operation failed (RR):\n"
              "      Input #1: %d\n"
              "      Input #2: %d\n"
              "      Expected: %d\n"
              "      Observed: %d\n"
              "Assembly:\n%s",
              a,
              b,
              uint32_t(expected),
              observed_rr,
              ctx.logger.data());

    uint32_t observed_ri = fn_ri(a);
    EXPECT_EQ(observed_ri, uint32_t(expected))
      .message("Operation failed (RI):\n"
              "      Input #1: %d\n"
              "      Input #2: %d\n"
              "      Expected: %d\n"
              "      Observed: %d\n"
              "Assembly:\n%s",
              a,
              b,
              uint32_t(expected),
              observed_ri,
              ctx.logger.data());

    ctx.rt.reset();
  }
}

static BL_NOINLINE void test_cond_ops(JitContext& ctx) noexcept {
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kEqual, 0, 0, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kEqual, 1, 1, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kEqual, 1, 2, false);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kEqual, 100, 31, false);

  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kNotEqual, 0, 0, false);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kNotEqual, 1, 1, false);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kNotEqual, 1, 2, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kNotEqual, 100, 31, true);

  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kUnsignedGT, 0, 0, false);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kUnsignedGT, 1, 0, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kUnsignedGT, 111111, 0, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kUnsignedGT, 111111, 222, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kUnsignedGT, 222, 111111, false);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kUnsignedGT, 222, 111, true);

  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kUnsignedGE, 0, 0, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kUnsignedGE, 1, 0, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kUnsignedGE, 111111, 0, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kUnsignedGE, 111111, 111111, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kUnsignedGE, 111111, 222, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kUnsignedGE, 222, 111111, false);

  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kUnsignedLT, 0, 0, false);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kUnsignedLT, 1, 0, false);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kUnsignedLT, 0, 1, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kUnsignedLT, 111111, 0, false);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kUnsignedLT, 111111, 222, false);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kUnsignedLT, 222, 111111, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kUnsignedLT, 222, 111, false);

  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kUnsignedLE, 0, 0, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kUnsignedLE, 1, 0, false);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kUnsignedLE, 0, 1, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kUnsignedLE, 111111, 0, false);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kUnsignedLE, 111111, 222, false);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kUnsignedLE, 222, 111111, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kUnsignedLE, 22222, 22222, true);

  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedGT, 0, 0, false);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedGT, 1, 0, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedGT, 111111, 0, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedGT, 111111, -222, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedGT, -222, 111111, false);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedGT, -222, -111, false);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedGT, -111, -1, false);

  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedGE, 0, 0, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedGE, 1, 0, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedGE, 111111, 0, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedGE, 111111, 111111, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedGE, 111111, -222, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedGE, -222, 111111, false);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedGE, -111, -1, false);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedGE, -111, -111, true);

  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedLT, 0, 0, false);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedLT, 1, 0, false);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedLT, 111111, 0, false);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedLT, 111111, -222, false);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedLT, -222, 111111, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedLT, -222, -111, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedLT, -111, -1, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedLT, -1, -1, false);

  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedLE, 0, 0, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedLE, 1, 0, false);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedLE, 111111, 0, false);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedLE, 111111, -222, false);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedLE, -222, 111111, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedLE, -222, -111, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedLE, -111, -1, true);
  test_conditional_op(ctx, OpcodeCond::kCompare, CondCode::kSignedLE, -1, -1, true);

  test_conditional_op(ctx, OpcodeCond::kTest, CondCode::kZero, 0, 0, true);
  test_conditional_op(ctx, OpcodeCond::kTest, CondCode::kZero, 1, 0, true);
  test_conditional_op(ctx, OpcodeCond::kTest, CondCode::kZero, 111111, 0, true);
  test_conditional_op(ctx, OpcodeCond::kTest, CondCode::kZero, 111111, -222, false);
  test_conditional_op(ctx, OpcodeCond::kTest, CondCode::kZero, -222, 111111, false);

  test_conditional_op(ctx, OpcodeCond::kTest, CondCode::kNotZero, 0, 0, false);
  test_conditional_op(ctx, OpcodeCond::kTest, CondCode::kNotZero, 1, 0, false);
  test_conditional_op(ctx, OpcodeCond::kTest, CondCode::kNotZero, 111111, 0, false);
  test_conditional_op(ctx, OpcodeCond::kTest, CondCode::kNotZero, 111111, -222, true);
  test_conditional_op(ctx, OpcodeCond::kTest, CondCode::kNotZero, -222, 111111, true);

  test_conditional_op(ctx, OpcodeCond::kBitTest, CondCode::kBTZero, 0x0, 0, true);
  test_conditional_op(ctx, OpcodeCond::kBitTest, CondCode::kBTZero, 0x1, 0, false);
  test_conditional_op(ctx, OpcodeCond::kBitTest, CondCode::kBTZero, 0xFF, 7, false);
  test_conditional_op(ctx, OpcodeCond::kBitTest, CondCode::kBTZero, 0xFF, 9, true);
  test_conditional_op(ctx, OpcodeCond::kBitTest, CondCode::kBTZero, 0xFFFFFFFF, 31, false);
  test_conditional_op(ctx, OpcodeCond::kBitTest, CondCode::kBTZero, 0x7FFFFFFF, 31, true);

  test_conditional_op(ctx, OpcodeCond::kBitTest, CondCode::kBTNotZero, 0x0, 0, false);
  test_conditional_op(ctx, OpcodeCond::kBitTest, CondCode::kBTNotZero, 0x1, 0, true);
  test_conditional_op(ctx, OpcodeCond::kBitTest, CondCode::kBTNotZero, 0xFF, 7, true);
  test_conditional_op(ctx, OpcodeCond::kBitTest, CondCode::kBTNotZero, 0xFF, 9, false);
  test_conditional_op(ctx, OpcodeCond::kBitTest, CondCode::kBTNotZero, 0xFFFFFFFF, 31, true);
  test_conditional_op(ctx, OpcodeCond::kBitTest, CondCode::kBTNotZero, 0x7FFFFFFF, 31, false);

  test_conditional_op(ctx, OpcodeCond::kAssignAnd, CondCode::kZero, 0x00000000, 0x00000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignAnd, CondCode::kZero, 0x00000001, 0x00000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignAnd, CondCode::kZero, 0x000000FF, 0x00000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignAnd, CondCode::kZero, 0x000000FF, 0x000000FF, false);
  test_conditional_op(ctx, OpcodeCond::kAssignAnd, CondCode::kZero, 0xFFFFFFFF, 0xFF000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignAnd, CondCode::kZero, 0x7FFFFFFF, 0x80000000, true);

  test_conditional_op(ctx, OpcodeCond::kAssignAnd, CondCode::kNotZero, 0x00000000, 0x00000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignAnd, CondCode::kNotZero, 0x00000001, 0x00000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignAnd, CondCode::kNotZero, 0x000000FF, 0x00000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignAnd, CondCode::kNotZero, 0x000000FF, 0x000000FF, true);
  test_conditional_op(ctx, OpcodeCond::kAssignAnd, CondCode::kNotZero, 0xFFFFFFFF, 0xFF000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignAnd, CondCode::kNotZero, 0x7FFFFFFF, 0x80000000, false);

  test_conditional_op(ctx, OpcodeCond::kAssignOr, CondCode::kZero, 0x00000000, 0x00000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignOr, CondCode::kZero, 0x00000001, 0x00000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignOr, CondCode::kZero, 0x000000FF, 0x00000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignOr, CondCode::kZero, 0x000000FF, 0x000000FF, false);
  test_conditional_op(ctx, OpcodeCond::kAssignOr, CondCode::kZero, 0xFFFFFFFF, 0xFF000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignOr, CondCode::kZero, 0x7FFFFFFF, 0x80000000, false);

  test_conditional_op(ctx, OpcodeCond::kAssignOr, CondCode::kNotZero, 0x00000000, 0x00000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignOr, CondCode::kNotZero, 0x00000001, 0x00000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignOr, CondCode::kNotZero, 0x000000FF, 0x00000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignOr, CondCode::kNotZero, 0x000000FF, 0x000000FF, true);
  test_conditional_op(ctx, OpcodeCond::kAssignOr, CondCode::kNotZero, 0xFFFFFFFF, 0xFF000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignOr, CondCode::kNotZero, 0x7FFFFFFF, 0x80000000, true);

  test_conditional_op(ctx, OpcodeCond::kAssignXor, CondCode::kZero, 0x00000000, 0x00000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignXor, CondCode::kZero, 0x00000001, 0x00000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignXor, CondCode::kZero, 0x000000FF, 0x00000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignXor, CondCode::kZero, 0x000000FF, 0x000000FF, true);
  test_conditional_op(ctx, OpcodeCond::kAssignXor, CondCode::kZero, 0xFFFFFFFF, 0xFF000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignXor, CondCode::kZero, 0x7FFFFFFF, 0x80000000, false);

  test_conditional_op(ctx, OpcodeCond::kAssignXor, CondCode::kNotZero, 0x00000000, 0x00000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignXor, CondCode::kNotZero, 0x00000001, 0x00000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignXor, CondCode::kNotZero, 0x000000FF, 0x00000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignXor, CondCode::kNotZero, 0x000000FF, 0x000000FF, false);
  test_conditional_op(ctx, OpcodeCond::kAssignXor, CondCode::kNotZero, 0xFFFFFFFF, 0xFF000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignXor, CondCode::kNotZero, 0x7FFFFFFF, 0x80000000, true);

  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kZero, 0x00000000, 0x00000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kZero, 0xFF000000, 0x01000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kZero, 0x000000FF, 0x00000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kZero, 0x000000FF, 0x000000FF, false);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kZero, 0xFFFFFFFF, 0xFF000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kZero, 0x7FFFFFFF, 0x80000000, false);

  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kNotZero, 0x00000000, 0x00000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kNotZero, 0xFF000000, 0x01000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kNotZero, 0x000000FF, 0x00000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kNotZero, 0x000000FF, 0x000000FF, true);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kNotZero, 0xFFFFFFFF, 0xFF000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kNotZero, 0x7FFFFFFF, 0x80000000, true);

  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kCarry, 0x00000000, 0x00000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kCarry, 0xFF000000, 0x01000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kCarry, 0x000000FF, 0x00000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kCarry, 0x000000FF, 0x000000FF, false);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kCarry, 0xFFFFFFFF, 0xFF000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kCarry, 0x7FFFFFFF, 0x80000000, false);

  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kNotCarry, 0x00000000, 0x00000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kNotCarry, 0xFF000000, 0x01000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kNotCarry, 0x000000FF, 0x00000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kNotCarry, 0x000000FF, 0x000000FF, true);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kNotCarry, 0xFFFFFFFF, 0xFF000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kNotCarry, 0x7FFFFFFF, 0x80000000, true);

  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kSign, 0x00000000, 0x00000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kSign, 0xFF000000, 0x01000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kSign, 0x000000FF, 0x80000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kSign, 0x000000FF, 0x800000FF, true);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kSign, 0xFFFFFFFF, 0xFF000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kSign, 0x7FFFFFFF, 0x80000000, true);

  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kNotSign, 0x00000000, 0x00000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kNotSign, 0xFF000000, 0x01000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kNotSign, 0x000000FF, 0x80000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kNotSign, 0x000000FF, 0x800000FF, false);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kNotSign, 0xFFFFFFFF, 0xFF000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignAdd, CondCode::kNotSign, 0x7FFFFFFF, 0x80000000, false);

  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kZero, 0x00000000, 0x00000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kZero, 0xFF000000, 0x01000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kZero, 0x000000FF, 0x00000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kZero, 0x000000FF, 0x000000FF, true);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kZero, 0xFFFFFFFF, 0xFF000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kZero, 0x7FFFFFFF, 0x80000000, false);

  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kNotZero, 0x00000000, 0x00000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kNotZero, 0xFF000000, 0x01000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kNotZero, 0x000000FF, 0x00000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kNotZero, 0x000000FF, 0x000000FF, false);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kNotZero, 0xFFFFFFFF, 0xFF000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kNotZero, 0x7FFFFFFF, 0x80000000, true);

  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kUnsignedLT, 0x00000000, 0x00000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kUnsignedLT, 0xFF000000, 0x01000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kUnsignedLT, 0x000000FF, 0x00000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kUnsignedLT, 0x000000FF, 0x000000FF, false);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kUnsignedLT, 0xFFFFFFFF, 0xFF000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kUnsignedLT, 0x7FFFFFFF, 0x80000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kUnsignedLT, 0x00000111, 0x0000F0FF, true);

  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kUnsignedGE, 0x00000000, 0x00000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kUnsignedGE, 0xFF000000, 0x01000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kUnsignedGE, 0x000000FF, 0x00000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kUnsignedGE, 0x000000FF, 0x000000FF, true);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kUnsignedGE, 0xFFFFFFFF, 0xFF000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kUnsignedGE, 0x7FFFFFFF, 0x80000000, false);

  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kSign, 0x00000000, 0x00000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kSign, 0x00000000, 0xFFFFFFFF, false);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kSign, 0x00000000, 0x00000001, true);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kSign, 0x00000001, 0x00000010, true);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kSign, 0xFFFFFFFF, 0xFF000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kSign, 0x7FFFFFFF, 0x80000000, true);

  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kNotSign, 0x00000000, 0x00000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kNotSign, 0x00000000, 0xFFFFFFFF, true);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kNotSign, 0x00000000, 0x00000001, false);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kNotSign, 0x00000001, 0x00000010, false);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kNotSign, 0xFFFFFFFF, 0xFF000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kNotSign, 0x7FFFFFFF, 0x80000000, false);

  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kUnsignedGT, 0x00000000, 0x00000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kUnsignedGT, 0xFF000000, 0x01000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kUnsignedGT, 0x000000FF, 0x00000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kUnsignedGT, 0x000000FF, 0x000000FF, false);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kUnsignedGT, 0xFFFFFFFF, 0xFF000000, true);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kUnsignedGT, 0x7FFFFFFF, 0x80000000, false);
  test_conditional_op(ctx, OpcodeCond::kAssignSub, CondCode::kUnsignedGT, 0x00000111, 0x0000F0FF, false);

  test_conditional_op(ctx, OpcodeCond::kAssignShr, CondCode::kZero, 0x00000000, 1, true);
  test_conditional_op(ctx, OpcodeCond::kAssignShr, CondCode::kZero, 0x000000FF, 8, true);
  test_conditional_op(ctx, OpcodeCond::kAssignShr, CondCode::kZero, 0x000000FF, 7, false);
  test_conditional_op(ctx, OpcodeCond::kAssignShr, CondCode::kZero, 0xFFFFFFFF, 31, false);
  test_conditional_op(ctx, OpcodeCond::kAssignShr, CondCode::kZero, 0x7FFFFFFF, 31, true);

  test_conditional_op(ctx, OpcodeCond::kAssignShr, CondCode::kNotZero, 0x00000000, 1, false);
  test_conditional_op(ctx, OpcodeCond::kAssignShr, CondCode::kNotZero, 0x000000FF, 8, false);
  test_conditional_op(ctx, OpcodeCond::kAssignShr, CondCode::kNotZero, 0x000000FF, 7, true);
  test_conditional_op(ctx, OpcodeCond::kAssignShr, CondCode::kNotZero, 0xFFFFFFFF, 31, true);
  test_conditional_op(ctx, OpcodeCond::kAssignShr, CondCode::kNotZero, 0x7FFFFFFF, 31, false);
}

// bl::Pipeline::JIT - Tests - M Operations - Functions
// ====================================================

static TestMFunc create_func_m(JitContext& ctx, OpcodeM op) noexcept {
  ctx.prepare();
  PipeCompiler pc(&ctx.cc, ctx.features, ctx.optFlags);

  asmjit::FuncNode* node = ctx.cc.newFunc(asmjit::FuncSignature::build<void, void*>());
  EXPECT_NOT_NULL(node);

  pc.initSimdWidth(SimdWidth::k128);
  pc.initFunction(node);

  Gp ptr = pc.newGpPtr("ptr");
  node->setArg(0, ptr);
  pc.emit_m(op, mem_ptr(ptr));

  ctx.cc.endFunc();
  return ctx.finish<TestMFunc>();
}

// bl::Pipeline::JIT - Tests - M Operations - Runner
// =================================================

static BL_NOINLINE void test_m_ops(JitContext& ctx) noexcept {
  uint8_t buffer[8];

  TestMFunc fn_zero_u8 = create_func_m(ctx, OpcodeM::kStoreZeroU8);
  memcpy(buffer, "ABCDEFGH", 8);
  fn_zero_u8(buffer + 0);
  EXPECT_EQ(memcmp(buffer, "\0BCDEFGH", 8), 0);
  fn_zero_u8(buffer + 5);
  EXPECT_EQ(memcmp(buffer, "\0BCDE\0GH", 8), 0);

  TestMFunc fn_zero_u16 = create_func_m(ctx, OpcodeM::kStoreZeroU16);
  memcpy(buffer, "ABCDEFGH", 8);
  fn_zero_u16(buffer + 0);
  EXPECT_EQ(memcmp(buffer, "\0\0CDEFGH", 8), 0);
  fn_zero_u16(buffer + 4);
  EXPECT_EQ(memcmp(buffer, "\0\0CD\0\0GH", 8), 0);

  TestMFunc fn_zero_u32 = create_func_m(ctx, OpcodeM::kStoreZeroU32);
  memcpy(buffer, "ABCDEFGH", 8);
  fn_zero_u32(buffer + 0);
  EXPECT_EQ(memcmp(buffer, "\0\0\0\0EFGH", 8), 0);
  fn_zero_u32(buffer + 4);
  EXPECT_EQ(memcmp(buffer, "\0\0\0\0\0\0\0\0", 8), 0);

#if BL_TARGET_ARCH_BITS >= 64
  TestMFunc fn_zero_u64 = create_func_m(ctx, OpcodeM::kStoreZeroU64);
  memcpy(buffer, "ABCDEFGH", 8);
  fn_zero_u64(buffer + 0);
  EXPECT_EQ(memcmp(buffer, "\0\0\0\0\0\0\0\0", 8), 0);
#endif

  TestMFunc fn_zero_reg = create_func_m(ctx, OpcodeM::kStoreZeroReg);
  memcpy(buffer, "ABCDEFGH", 8);
  fn_zero_reg(buffer + 0);
#if BL_TARGET_ARCH_BITS >= 64
  EXPECT_EQ(memcmp(buffer, "\0\0\0\0\0\0\0\0", 8), 0);
#else
  EXPECT_EQ(memcmp(buffer, "\0\0\0\0EFGH", 8), 0);
#endif

  ctx.rt.reset();
}

// bl::Pipeline::JIT - Tests - RM Operations - Functions
// =====================================================

static TestRMFunc create_func_rm(JitContext& ctx, OpcodeRM op) noexcept {
  ctx.prepare();
  PipeCompiler pc(&ctx.cc, ctx.features, ctx.optFlags);

  asmjit::FuncNode* node = ctx.cc.newFunc(asmjit::FuncSignature::build<uintptr_t, uintptr_t, void*>());
  EXPECT_NOT_NULL(node);

  pc.initSimdWidth(SimdWidth::k128);
  pc.initFunction(node);

  Gp reg = pc.newGpPtr("reg");
  Gp ptr = pc.newGpPtr("ptr");

  node->setArg(0, reg);
  node->setArg(1, ptr);

  pc.emit_rm(op, reg, mem_ptr(ptr));
  ctx.cc.ret(reg);

  ctx.cc.endFunc();
  return ctx.finish<TestRMFunc>();
}

// bl::Pipeline::JIT - Tests - RM Operations - Runner
// ==================================================

static BL_NOINLINE void test_rm_ops(JitContext& ctx) noexcept {
  union Mem {
    uint8_t buffer[8];
    uint16_t u8;
    uint16_t u16;
    uint32_t u32;
    uint64_t u64;
  };

  Mem mem{};

  TestRMFunc fn_load_i8 = create_func_rm(ctx, OpcodeRM::kLoadI8);
  mem.u8 = uint8_t(int8_t(6));
  EXPECT_EQ(fn_load_i8(0, mem.buffer), uintptr_t(intptr_t(6)));

  mem.u8 = uint8_t(int8_t(-6));
  EXPECT_EQ(fn_load_i8(0, mem.buffer), uintptr_t(intptr_t(-6)));

  TestRMFunc fn_load_u8 = create_func_rm(ctx, OpcodeRM::kLoadU8);
  mem.u8 = uint8_t(0x80);
  EXPECT_EQ(fn_load_u8(0, mem.buffer), 0x80u);

  mem.u8 = uint8_t(0xFF);
  EXPECT_EQ(fn_load_u8(0, mem.buffer), 0xFFu);

  TestRMFunc fn_load_i16 = create_func_rm(ctx, OpcodeRM::kLoadI16);
  mem.u16 = uint16_t(int16_t(666));
  EXPECT_EQ(fn_load_i16(0, mem.buffer), uintptr_t(intptr_t(666)));

  mem.u16 = uint16_t(int16_t(-666));
  EXPECT_EQ(fn_load_i16(0, mem.buffer), uintptr_t(intptr_t(-666)));

  TestRMFunc fn_load_u16 = create_func_rm(ctx, OpcodeRM::kLoadU16);
  mem.u16 = uint16_t(0x8000);
  EXPECT_EQ(fn_load_u16(0, mem.buffer), 0x8000u);

  mem.u16 = uint16_t(0xFEED);
  EXPECT_EQ(fn_load_u16(0, mem.buffer), 0xFEEDu);

  TestRMFunc fn_load_i32 = create_func_rm(ctx, OpcodeRM::kLoadI32);
  mem.u32 = uint32_t(int32_t(666666));
  EXPECT_EQ(fn_load_i32(0, mem.buffer), uintptr_t(intptr_t(666666)));

  mem.u32 = uint32_t(int32_t(-666666));
  EXPECT_EQ(fn_load_i32(0, mem.buffer), uintptr_t(intptr_t(-666666)));

  TestRMFunc fn_load_u32 = create_func_rm(ctx, OpcodeRM::kLoadU32);
  mem.u32 = 0x12345678;
  EXPECT_EQ(fn_load_u32(0, mem.buffer), uint32_t(0x12345678));

#if BL_TARGET_ARCH_BITS >= 64
  TestRMFunc fn_load_i64 = create_func_rm(ctx, OpcodeRM::kLoadI64);
  mem.u64 = 0xF123456789ABCDEFu;
  EXPECT_EQ(fn_load_i64(0, mem.buffer), 0xF123456789ABCDEFu);

  TestRMFunc fn_load_u64 = create_func_rm(ctx, OpcodeRM::kLoadU64);
  mem.u64 = 0xF123456789ABCDEFu;
  EXPECT_EQ(fn_load_u64(0, mem.buffer), 0xF123456789ABCDEFu);
#endif

  TestRMFunc fn_load_reg = create_func_rm(ctx, OpcodeRM::kLoadReg);
  mem.u64 = 0xF123456789ABCDEFu;
#if BL_TARGET_ARCH_BITS >= 64
  EXPECT_EQ(fn_load_reg(0, mem.buffer), 0xF123456789ABCDEFu);
#else
  EXPECT_EQ(fn_load_reg(0, mem.buffer), 0x89ABCDEFu);
#endif

  TestRMFunc fn_load_merge_u8 = create_func_rm(ctx, OpcodeRM::kLoadMergeU8);
  mem.u8 = uint8_t(0xAA);
  EXPECT_EQ(fn_load_merge_u8(0x1F2FFF00, mem.buffer), 0x1F2FFFAAu);

  TestRMFunc fn_load_merge_u16 = create_func_rm(ctx, OpcodeRM::kLoadMergeU16);
  mem.u16 = uint16_t(0xAABB);
  EXPECT_EQ(fn_load_merge_u16(0x1F2F0000, mem.buffer), 0x1F2FAABBu);

  ctx.rt.reset();
}

// bl::Pipeline::JIT - Tests - MR Operations - Functions
// =====================================================

static TestMRFunc create_func_mr(JitContext& ctx, OpcodeMR op) noexcept {
  ctx.prepare();
  PipeCompiler pc(&ctx.cc, ctx.features, ctx.optFlags);

  asmjit::FuncNode* node = ctx.cc.newFunc(asmjit::FuncSignature::build<void, void*, uintptr_t>());
  EXPECT_NOT_NULL(node);

  pc.initSimdWidth(SimdWidth::k128);
  pc.initFunction(node);

  Gp ptr = pc.newGpPtr("ptr");
  Gp reg = pc.newGpPtr("reg");

  node->setArg(0, ptr);
  node->setArg(1, reg);

  pc.emit_mr(op, mem_ptr(ptr), reg);

  ctx.cc.endFunc();
  return ctx.finish<TestMRFunc>();
}

// bl::Pipeline::JIT - Tests - MR Operations - Runner
// ==================================================

static BL_NOINLINE void test_mr_ops(JitContext& ctx) noexcept {
  union Mem {
    uint8_t buffer[8];
    uint16_t u8;
    uint16_t u16;
    uint32_t u32;
    uint64_t u64;
  };

  Mem mem{};

  TestMRFunc fn_store_u8 = create_func_mr(ctx, OpcodeMR::kStoreU8);
  memcpy(mem.buffer, "ABCDEFGH", 8);
  fn_store_u8(mem.buffer, 0x7A);
  EXPECT_EQ(memcmp(mem.buffer, "zBCDEFGH", 8), 0);

  TestMRFunc fn_store_u16 = create_func_mr(ctx, OpcodeMR::kStoreU16);
  memcpy(mem.buffer, "ABCDEFGH", 8);
  fn_store_u16(mem.buffer, 0x7A7A);
  EXPECT_EQ(memcmp(mem.buffer, "zzCDEFGH", 8), 0);

  TestMRFunc fn_store_u32 = create_func_mr(ctx, OpcodeMR::kStoreU32);
  memcpy(mem.buffer, "ABCDEFGH", 8);
  fn_store_u32(mem.buffer, 0x7A7A7A7A);
  EXPECT_EQ(memcmp(mem.buffer, "zzzzEFGH", 8), 0);

#if BL_TARGET_ARCH_BITS >= 64
  TestMRFunc fn_store_u64 = create_func_mr(ctx, OpcodeMR::kStoreU64);
  memcpy(mem.buffer, "ABCDEFGH", 8);
  fn_store_u64(mem.buffer, 0x7A7A7A7A7A7A7A7A);
  EXPECT_EQ(memcmp(mem.buffer, "zzzzzzzz", 8), 0);
#endif

  TestMRFunc fn_store_reg = create_func_mr(ctx, OpcodeMR::kStoreReg);
  memcpy(mem.buffer, "ABCDEFGH", 8);
#if BL_TARGET_ARCH_BITS >= 64
  fn_store_reg(mem.buffer, 0x7A7A7A7A7A7A7A7A);
  EXPECT_EQ(memcmp(mem.buffer, "zzzzzzzz", 8), 0);
#else
  fn_store_reg(mem.buffer, 0x7A7A7A7A);
  EXPECT_EQ(memcmp(mem.buffer, "zzzzEFGH", 8), 0);
#endif

  TestMRFunc fn_add_u8 = create_func_mr(ctx, OpcodeMR::kAddU8);
  mem.u64 = 0;
  mem.u8 = 42;
  fn_add_u8(mem.buffer, 13);
  EXPECT_EQ(mem.u8, 55u);
  EXPECT_EQ(memcmp(mem.buffer + 1, "\0\0\0\0\0\0\0", 7), 0);

  TestMRFunc fn_add_u16 = create_func_mr(ctx, OpcodeMR::kAddU16);
  mem.u64 = 0;
  mem.u16 = 442;
  fn_add_u16(mem.buffer, 335);
  EXPECT_EQ(mem.u16, 777u);
  EXPECT_EQ(memcmp(mem.buffer + 2, "\0\0\0\0\0\0", 6), 0);

  TestMRFunc fn_add_u32 = create_func_mr(ctx, OpcodeMR::kAddU32);
  mem.u64 = 0;
  mem.u32 = 442332;
  fn_add_u32(mem.buffer, 335223);
  EXPECT_EQ(mem.u32, 777555u);
  EXPECT_EQ(memcmp(mem.buffer + 2, "\0\0\0\0\0\0", 6), 0);

#if BL_TARGET_ARCH_BITS >= 64
  TestMRFunc fn_add_u64 = create_func_mr(ctx, OpcodeMR::kAddU64);
  mem.u64 = 0xF123456789ABCDEFu;
  fn_add_u64(mem.buffer, 0x0102030405060708u);
  EXPECT_EQ(mem.u64, 0xF225486B8EB1D4F7u);
#endif

  TestMRFunc fn_add_reg = create_func_mr(ctx, OpcodeMR::kAddReg);
  mem.u64 = 0xFFFFFFFFFFFFFFFF;
#if BL_TARGET_ARCH_BITS >= 64
  fn_add_reg(mem.buffer, 1);
  EXPECT_EQ(mem.u64, 0u);
#else
  mem.u32 = 0x01020304;
  fn_add_reg(mem.buffer, 0x02030405);
  EXPECT_EQ(mem.u32, 0x03050709u);
  EXPECT_EQ(memcmp(mem.buffer + 4, "\255\255\255\255", 4), 0);
#endif

  ctx.rt.reset();
}

// bl::Pipeline::JIT - Tests - RR Operations - Functions
// =====================================================

static TestRRFunc create_func_rr(JitContext& ctx, OpcodeRR op) noexcept {
  ctx.prepare();
  PipeCompiler pc(&ctx.cc, ctx.features, ctx.optFlags);

  asmjit::FuncNode* node = ctx.cc.newFunc(asmjit::FuncSignature::build<uint32_t, uint32_t>());
  EXPECT_NOT_NULL(node);

  pc.initSimdWidth(SimdWidth::k128);
  pc.initFunction(node);

  Gp r = pc.newGp32("r");
  node->setArg(0, r);
  pc.emit_2i(op, r, r);
  ctx.cc.ret(r);

  ctx.cc.endFunc();
  return ctx.finish<TestRRFunc>();
}

// bl::Pipeline::JIT - Tests - RR Operations - Runner
// ==================================================

static BL_NOINLINE void test_rr_ops(JitContext& ctx) noexcept {
  TestRRFunc fn_abs = create_func_rr(ctx, OpcodeRR::kAbs);
  EXPECT_EQ(fn_abs(0u), 0u);
  EXPECT_EQ(fn_abs(1u), 1u);
  EXPECT_EQ(fn_abs(uint32_t(-1)), 1u);
  EXPECT_EQ(fn_abs(uint32_t(-333)), 333u);
  EXPECT_EQ(fn_abs(0x80000000u), 0x80000000u);

  TestRRFunc fn_neg = create_func_rr(ctx, OpcodeRR::kNeg);
  EXPECT_EQ(fn_neg(0u), 0u);
  EXPECT_EQ(fn_neg(1u), uint32_t(-1));
  EXPECT_EQ(fn_neg(uint32_t(-1)), 1u);
  EXPECT_EQ(fn_neg(uint32_t(-333)), 333u);
  EXPECT_EQ(fn_neg(333u), uint32_t(-333));
  EXPECT_EQ(fn_neg(0x80000000u), 0x80000000u);

  TestRRFunc fn_not = create_func_rr(ctx, OpcodeRR::kNot);
  EXPECT_EQ(fn_not(0u), 0xFFFFFFFFu);
  EXPECT_EQ(fn_not(1u), 0xFFFFFFFEu);
  EXPECT_EQ(fn_not(0xFFFFFFFF), 0u);
  EXPECT_EQ(fn_not(0x12333245), ~0x12333245u);
  EXPECT_EQ(fn_not(0x80000000u), 0x7FFFFFFFu);

  TestRRFunc fn_bswap32 = create_func_rr(ctx, OpcodeRR::kBSwap);
  EXPECT_EQ(fn_bswap32(0x11223344u), 0x44332211u);
  EXPECT_EQ(fn_bswap32(0xFFFF0000u), 0x0000FFFFu);
  EXPECT_EQ(fn_bswap32(0x00000000u), 0x00000000u);

  TestRRFunc fn_clz32 = create_func_rr(ctx, OpcodeRR::kCLZ);
  EXPECT_EQ(fn_clz32(0x80000000u), 0u);
  EXPECT_EQ(fn_clz32(0x40000000u), 1u);
  EXPECT_EQ(fn_clz32(0x00800000u), 8u);
  EXPECT_EQ(fn_clz32(0x00008000u), 16u);
  EXPECT_EQ(fn_clz32(0x00000080u), 24u);
  EXPECT_EQ(fn_clz32(0x00000001u), 31u);

  TestRRFunc fn_ctz32 = create_func_rr(ctx, OpcodeRR::kCTZ);
  EXPECT_EQ(fn_ctz32(0x80000000u), 31u);
  EXPECT_EQ(fn_ctz32(0x40000000u), 30u);
  EXPECT_EQ(fn_ctz32(0x00800000u), 23u);
  EXPECT_EQ(fn_ctz32(0x00008000u), 15u);
  EXPECT_EQ(fn_ctz32(0x00000080u), 7u);
  EXPECT_EQ(fn_ctz32(0x00000001u), 0u);

  TestRRFunc fn_reflect = create_func_rr(ctx, OpcodeRR::kReflect);
  EXPECT_EQ(fn_reflect(0x00000000u), 0x00000000u);
  EXPECT_EQ(fn_reflect(0x00FF0000u), 0x00FF0000u);
  EXPECT_EQ(fn_reflect(0x000000FFu), 0x000000FFu);
  EXPECT_EQ(fn_reflect(0x80000000u), 0x7FFFFFFFu);
  EXPECT_EQ(fn_reflect(0xFFFFFFFFu), 0x00000000u);
  EXPECT_EQ(fn_reflect(0x88FF0000u), 0x7700FFFFu);

  ctx.rt.reset();
}

// bl::Pipeline::JIT - Tests - RRR Operations - Functions
// ======================================================

static TestRRRFunc create_func_rrr(JitContext& ctx, OpcodeRRR op) noexcept {
  ctx.prepare();
  PipeCompiler pc(&ctx.cc, ctx.features, ctx.optFlags);

  asmjit::FuncNode* node = ctx.cc.newFunc(asmjit::FuncSignature::build<uint32_t, uint32_t, uint32_t>());
  EXPECT_NOT_NULL(node);

  pc.initSimdWidth(SimdWidth::k128);
  pc.initFunction(node);

  Gp a = pc.newGp32("a");
  Gp b = pc.newGp32("b");
  Gp result = pc.newGp32("result");

  node->setArg(0, a);
  node->setArg(1, b);

  pc.emit_3i(op, result, a, b);
  ctx.cc.ret(result);

  ctx.cc.endFunc();
  return ctx.finish<TestRRRFunc>();
}

static TestRRIFunc create_func_rri(JitContext& ctx, OpcodeRRR op, Imm bImm) noexcept {
  ctx.prepare();
  PipeCompiler pc(&ctx.cc, ctx.features, ctx.optFlags);

  asmjit::FuncNode* node = ctx.cc.newFunc(asmjit::FuncSignature::build<uint32_t, uint32_t>());
  EXPECT_NOT_NULL(node);

  pc.initSimdWidth(SimdWidth::k128);
  pc.initFunction(node);

  Gp a = pc.newGp32("a");
  Gp result = pc.newGp32("result");

  node->setArg(0, a);

  pc.emit_3i(op, result, a, bImm);
  ctx.cc.ret(result);

  ctx.cc.endFunc();
  return ctx.finish<TestRRIFunc>();
}

// bl::Pipeline::JIT - Tests - RRR Operations - Runner
// ===================================================

static BL_NOINLINE void test_rrr_op(JitContext& ctx, OpcodeRRR op, uint32_t a, uint32_t b, uint32_t expected) noexcept {
  TestRRRFunc fn_rrr = create_func_rrr(ctx, op);
  uint32_t observed_rrr = fn_rrr(a, b);
  EXPECT_EQ(observed_rrr, expected)
    .message("Operation failed (RRR):\n"
            "      Input #1: %d\n"
            "      Input #2: %d\n"
            "      Expected: %d\n"
            "      Observed: %d\n"
            "Assembly:\n%s",
            a,
            b,
            uint32_t(expected),
            observed_rrr,
            ctx.logger.data());

  TestRRIFunc fn_rri = create_func_rri(ctx, op, Imm(b));
  uint32_t observed_rri = fn_rri(a);
  EXPECT_EQ(observed_rri, expected)
    .message("Operation failed (RRI):\n"
            "      Input #1: %d\n"
            "      Input #2: %d\n"
            "      Expected: %d\n"
            "      Observed: %d\n"
            "Assembly:\n%s",
            a,
            b,
            uint32_t(expected),
            observed_rri,
            ctx.logger.data());

  ctx.rt.reset();
}

static BL_NOINLINE void test_rrr_ops(JitContext& ctx) noexcept {
  test_rrr_op(ctx, OpcodeRRR::kAnd, 0u, 0u, 0u);
  test_rrr_op(ctx, OpcodeRRR::kAnd, 0xFFu, 0x11u, 0x11u);
  test_rrr_op(ctx, OpcodeRRR::kAnd, 0x11u, 0xFFu, 0x11u);
  test_rrr_op(ctx, OpcodeRRR::kAnd, 0xFF11u, 0x1111u, 0x1111u);
  test_rrr_op(ctx, OpcodeRRR::kAnd, 0x1111u, 0xFF11u, 0x1111u);
  test_rrr_op(ctx, OpcodeRRR::kAnd, 0x0000FFFFu, 0xFFFF0000u, 0u);
  test_rrr_op(ctx, OpcodeRRR::kAnd, 0xFFFFFFFFu, 0xFFFF0000u, 0xFFFF0000u);
  test_rrr_op(ctx, OpcodeRRR::kAnd, 0x11111111u, 0x11223344u, 0x11001100u);

  test_rrr_op(ctx, OpcodeRRR::kOr, 0u, 0u, 0u);
  test_rrr_op(ctx, OpcodeRRR::kOr, 0xFFu, 0x11u, 0xFFu);
  test_rrr_op(ctx, OpcodeRRR::kOr, 0x11u, 0xFFu, 0xFFu);
  test_rrr_op(ctx, OpcodeRRR::kOr, 0xFF11u, 0x1111u, 0xFF11u);
  test_rrr_op(ctx, OpcodeRRR::kOr, 0x1111u, 0xFF11u, 0xFF11u);
  test_rrr_op(ctx, OpcodeRRR::kOr, 0x0000FFFFu, 0xFFFF0001u, 0xFFFFFFFFu);
  test_rrr_op(ctx, OpcodeRRR::kOr, 0xFFFFFFFFu, 0xFF000000u, 0xFFFFFFFFu);
  test_rrr_op(ctx, OpcodeRRR::kOr, 0x11111111u, 0x00223344u, 0x11333355u);

  test_rrr_op(ctx, OpcodeRRR::kXor, 0u, 0u, 0u);
  test_rrr_op(ctx, OpcodeRRR::kXor, 0xFFu, 0x11u, 0xEEu);
  test_rrr_op(ctx, OpcodeRRR::kXor, 0x11u, 0xFFu, 0xEEu);
  test_rrr_op(ctx, OpcodeRRR::kXor, 0xFF11u, 0x1111u, 0xEE00u);
  test_rrr_op(ctx, OpcodeRRR::kXor, 0x1111u, 0xFF11u, 0xEE00u);
  test_rrr_op(ctx, OpcodeRRR::kXor, 0x0000FFFFu, 0xFFFF0001u, 0xFFFFFFFEu);
  test_rrr_op(ctx, OpcodeRRR::kXor, 0xFFFFFFFFu, 0xFF000000u, 0x00FFFFFFu);
  test_rrr_op(ctx, OpcodeRRR::kXor, 0x11111111u, 0x00223344u, 0x11332255u);

  test_rrr_op(ctx, OpcodeRRR::kBic, 0u, 0u, 0u);
  test_rrr_op(ctx, OpcodeRRR::kBic, 0xFFu, 0x11u, 0xEEu);
  test_rrr_op(ctx, OpcodeRRR::kBic, 0x11u, 0xFFu, 0x00u);
  test_rrr_op(ctx, OpcodeRRR::kBic, 0xFF11u, 0x1111u, 0xEE00u);
  test_rrr_op(ctx, OpcodeRRR::kBic, 0x1111u, 0xFF11u, 0x0000u);
  test_rrr_op(ctx, OpcodeRRR::kBic, 0x0000FFFFu, 0xFFFF0000u, 0x0000FFFFu);
  test_rrr_op(ctx, OpcodeRRR::kBic, 0xFFFFFFFFu, 0xFFFF0000u, 0x0000FFFFu);
  test_rrr_op(ctx, OpcodeRRR::kBic, 0x11111111u, 0x11223344u, 0x00110011u);

  test_rrr_op(ctx, OpcodeRRR::kAdd, 0u, 0u, 0u);
  test_rrr_op(ctx, OpcodeRRR::kAdd, 1u, 2u, 3u);
  test_rrr_op(ctx, OpcodeRRR::kAdd, 0xFF000000u, 0x00FFFFFFu, 0xFFFFFFFFu);
  test_rrr_op(ctx, OpcodeRRR::kAdd, 1u, 0xFFFu, 0x1000u);
  test_rrr_op(ctx, OpcodeRRR::kAdd, 1u, 0xFFF000u, 0xFFF001u);

  test_rrr_op(ctx, OpcodeRRR::kSub, 1u, 2u, 0xFFFFFFFFu);

  test_rrr_op(ctx, OpcodeRRR::kMul, 1000u, 999u, 999000u);
  test_rrr_op(ctx, OpcodeRRR::kMul, 0xFFFFu, 0x00010001u, 0xFFFFFFFFu);

  test_rrr_op(ctx, OpcodeRRR::kUDiv, 100000u, 1000u, 100u);

  test_rrr_op(ctx, OpcodeRRR::kUMod, 1999u, 1000u, 999u);

  test_rrr_op(ctx, OpcodeRRR::kSMin, uint32_t(1111), uint32_t(0), uint32_t(0));
  test_rrr_op(ctx, OpcodeRRR::kSMin, uint32_t(-1111), uint32_t(0), uint32_t(-1111));
  test_rrr_op(ctx, OpcodeRRR::kSMin, uint32_t(1), uint32_t(22), uint32_t(1));
  test_rrr_op(ctx, OpcodeRRR::kSMin, uint32_t(1), uint32_t(0), uint32_t(0));
  test_rrr_op(ctx, OpcodeRRR::kSMin, uint32_t(100101033), uint32_t(999), uint32_t(999));
  test_rrr_op(ctx, OpcodeRRR::kSMin, uint32_t(100101033), uint32_t(112), uint32_t(112));
  test_rrr_op(ctx, OpcodeRRR::kSMin, uint32_t(112), uint32_t(1125532), uint32_t(112));
  test_rrr_op(ctx, OpcodeRRR::kSMin, uint32_t(1111), uint32_t(-1), uint32_t(-1));
  test_rrr_op(ctx, OpcodeRRR::kSMin, uint32_t(-1111), uint32_t(-1), uint32_t(-1111));
  test_rrr_op(ctx, OpcodeRRR::kSMin, uint32_t(-1), uint32_t(-22), uint32_t(-22));
  test_rrr_op(ctx, OpcodeRRR::kSMin, uint32_t(-1), uint32_t(-128), uint32_t(-128));
  test_rrr_op(ctx, OpcodeRRR::kSMin, uint32_t(-128), uint32_t(-1), uint32_t(-128));
  test_rrr_op(ctx, OpcodeRRR::kSMin, uint32_t(-128), uint32_t(9), uint32_t(-128));
  test_rrr_op(ctx, OpcodeRRR::kSMin, uint32_t(12444), uint32_t(-1), uint32_t(-1));

  test_rrr_op(ctx, OpcodeRRR::kSMax, uint32_t(1), uint32_t(22), uint32_t(22));
  test_rrr_op(ctx, OpcodeRRR::kSMax, uint32_t(1), uint32_t(0), uint32_t(1));
  test_rrr_op(ctx, OpcodeRRR::kSMax, uint32_t(100101033), uint32_t(999), uint32_t(100101033));
  test_rrr_op(ctx, OpcodeRRR::kSMax, uint32_t(100101033), uint32_t(112), uint32_t(100101033));
  test_rrr_op(ctx, OpcodeRRR::kSMax, uint32_t(112), uint32_t(1125532), uint32_t(1125532));
  test_rrr_op(ctx, OpcodeRRR::kSMax, uint32_t(1111), uint32_t(-1), uint32_t(1111));
  test_rrr_op(ctx, OpcodeRRR::kSMax, uint32_t(-1111), uint32_t(-1), uint32_t(-1));
  test_rrr_op(ctx, OpcodeRRR::kSMax, uint32_t(-1), uint32_t(-22), uint32_t(-1));
  test_rrr_op(ctx, OpcodeRRR::kSMax, uint32_t(-1), uint32_t(-128), uint32_t(-1));
  test_rrr_op(ctx, OpcodeRRR::kSMax, uint32_t(-128), uint32_t(-1), uint32_t(-1));
  test_rrr_op(ctx, OpcodeRRR::kSMax, uint32_t(-128), uint32_t(9), uint32_t(9));
  test_rrr_op(ctx, OpcodeRRR::kSMax, uint32_t(12444), uint32_t(-1), uint32_t(12444));

  test_rrr_op(ctx, OpcodeRRR::kUMin, 1, 22, 1);
  test_rrr_op(ctx, OpcodeRRR::kUMin, 22, 1, 1);
  test_rrr_op(ctx, OpcodeRRR::kUMin, 1, 255, 1);
  test_rrr_op(ctx, OpcodeRRR::kUMin, 255, 1, 1);
  test_rrr_op(ctx, OpcodeRRR::kUMin, 1023, 255, 255);
  test_rrr_op(ctx, OpcodeRRR::kUMin, 255, 1023, 255);
  test_rrr_op(ctx, OpcodeRRR::kUMin, 0xFFFFFFFFu, 255, 255);
  test_rrr_op(ctx, OpcodeRRR::kUMin, 255, 0xFFFFFFFFu, 255);
  test_rrr_op(ctx, OpcodeRRR::kUMin, 0xFFFFFFFFu, 0xFFFFFF00u, 0xFFFFFF00u);
  test_rrr_op(ctx, OpcodeRRR::kUMin, 0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu);

  test_rrr_op(ctx, OpcodeRRR::kUMax, 1, 22, 22);
  test_rrr_op(ctx, OpcodeRRR::kUMax, 22, 1, 22);
  test_rrr_op(ctx, OpcodeRRR::kUMax, 1, 255, 255);
  test_rrr_op(ctx, OpcodeRRR::kUMax, 255, 1, 255);
  test_rrr_op(ctx, OpcodeRRR::kUMax, 1023, 255, 1023);
  test_rrr_op(ctx, OpcodeRRR::kUMax, 255, 1023, 1023);
  test_rrr_op(ctx, OpcodeRRR::kUMax, 0xFFFFFFFFu, 255, 0xFFFFFFFFu);
  test_rrr_op(ctx, OpcodeRRR::kUMax, 255, 0xFFFFFFFFu, 0xFFFFFFFFu);
  test_rrr_op(ctx, OpcodeRRR::kUMax, 0xFFFFFFFFu, 0xFFFFFF00u, 0xFFFFFFFFu);
  test_rrr_op(ctx, OpcodeRRR::kUMax, 0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu);

  test_rrr_op(ctx, OpcodeRRR::kSll, 1u, 1u, 1u << 1);
  test_rrr_op(ctx, OpcodeRRR::kSll, 1u, 22u, 1u << 22);
  test_rrr_op(ctx, OpcodeRRR::kSll, 1u, 31u, 1u << 31);
  test_rrr_op(ctx, OpcodeRRR::kSll, 0x7FFFFFFFu, 1u, 0xFFFFFFFEu);

  test_rrr_op(ctx, OpcodeRRR::kSrl, 1u, 1u, 1u >> 1);
  test_rrr_op(ctx, OpcodeRRR::kSrl, 1u, 22u, 1u >> 22);
  test_rrr_op(ctx, OpcodeRRR::kSrl, 1u, 31u, 1u >> 31);
  test_rrr_op(ctx, OpcodeRRR::kSrl, 0x7FFFFFFFu, 1u, 0x7FFFFFFFu >> 1);

  test_rrr_op(ctx, OpcodeRRR::kSra, 1u, 1u, 1u >> 1);
  test_rrr_op(ctx, OpcodeRRR::kSra, 1u, 22u, 1u >> 22);
  test_rrr_op(ctx, OpcodeRRR::kSra, 1u, 31u, 1u >> 31);
  test_rrr_op(ctx, OpcodeRRR::kSra, 0x7FFFFFFFu, 1u, 0x7FFFFFFFu >> 1);
  test_rrr_op(ctx, OpcodeRRR::kSra, 0xF0000000u, 4u, 0xFF000000u);
  test_rrr_op(ctx, OpcodeRRR::kSra, 0x80000000u, 31u, 0xFFFFFFFFu);

  test_rrr_op(ctx, OpcodeRRR::kRol, 0x11223344u, 8u, 0x22334411u);
  test_rrr_op(ctx, OpcodeRRR::kRol, 0x11223344u, 16u, 0x33441122u);
  test_rrr_op(ctx, OpcodeRRR::kRol, 0xFCFFDABBu, 1u, 0xF9FFB577u);

  test_rrr_op(ctx, OpcodeRRR::kRor, 0x11223344u, 8u, 0x44112233u);
  test_rrr_op(ctx, OpcodeRRR::kRor, 0x11223344u, 16u, 0x33441122u);
  test_rrr_op(ctx, OpcodeRRR::kRor, 0xF0000000u, 1u, 0x78000000u);

  test_rrr_op(ctx, OpcodeRRR::kSBound, 0, 244u, 0);
  test_rrr_op(ctx, OpcodeRRR::kSBound, 42, 244u, 42u);
  test_rrr_op(ctx, OpcodeRRR::kSBound, 1111, 244u, 244u);
  test_rrr_op(ctx, OpcodeRRR::kSBound, 9999999, 111244u, 111244u);
  test_rrr_op(ctx, OpcodeRRR::kSBound, uint32_t(int32_t(-1)), 1000u, 0u);
  test_rrr_op(ctx, OpcodeRRR::kSBound, uint32_t(INT32_MIN), 100000u, 0u);
  test_rrr_op(ctx, OpcodeRRR::kSBound, uint32_t(INT32_MAX), 0u, 0u);
  test_rrr_op(ctx, OpcodeRRR::kSBound, uint32_t(INT32_MAX), 100000u, 100000u);
  test_rrr_op(ctx, OpcodeRRR::kSBound, uint32_t(INT32_MAX), uint32_t(INT32_MAX), uint32_t(INT32_MAX));
}

// bl::Pipeline::JIT - Tests - SIMD - Functions
// ============================================

static TestVVFunc create_func_vv(JitContext& ctx, SimdWidth simdWidth, OpcodeVV op, uint32_t variation = 0) noexcept {
  ctx.prepare();
  PipeCompiler pc(&ctx.cc, ctx.features, ctx.optFlags);

  asmjit::FuncNode* node = ctx.cc.newFunc(asmjit::FuncSignature::build<void, void*, const void*>());
  EXPECT_NOT_NULL(node);

  pc.initSimdWidth(simdWidth);
  pc.initFunction(node);

  Gp dstPtr = pc.newGpPtr("dstPtr");
  Gp srcPtr = pc.newGpPtr("srcPtr");

  node->setArg(0, dstPtr);
  node->setArg(1, srcPtr);

  Vec dstVec = pc.newVec(simdWidth, "dstVec");

  if (variation == 0) {
    Vec srcVec = pc.newVec(simdWidth, "srcVec");
    pc.v_loaduvec(srcVec, mem_ptr(srcPtr));
    pc.emit_2v(op, dstVec, srcVec);
  }
  else if (variation == 1) {
    // This is used to test broadcasts from a GP register to a vector register.
    Gp srcGp = pc.newGpPtr("srcGp");

    switch (op) {
      case OpcodeVV::kBroadcastU8:
      case OpcodeVV::kBroadcastU8Z:
        pc.load_u8(srcGp, mem_ptr(srcPtr));
        pc.emit_2v(op, dstVec, srcGp);
        break;

      case OpcodeVV::kBroadcastU16:
      case OpcodeVV::kBroadcastU16Z:
        pc.load_u16(srcGp, mem_ptr(srcPtr));
        pc.emit_2v(op, dstVec, srcGp);
        break;

      case OpcodeVV::kBroadcastU32:
      case OpcodeVV::kBroadcastF32:
        pc.load_u32(srcGp, mem_ptr(srcPtr));
        pc.emit_2v(op, dstVec, srcGp);
        break;

      case OpcodeVV::kBroadcastU64:
      case OpcodeVV::kBroadcastF64:
        // Prevent using 64-bit registers on 32-bit architectures (that would fail).
        if (pc.is64Bit()) {
          pc.load_u64(srcGp, mem_ptr(srcPtr));
          pc.emit_2v(op, dstVec, srcGp);
        }
        else {
          pc.emit_2v(op, dstVec, mem_ptr(srcPtr));
        }
        break;

      default:
        BL_NOT_REACHED();
    }
  }
  else {
    pc.emit_2v(op, dstVec, mem_ptr(srcPtr));
  }

  pc.v_storeuvec(mem_ptr(dstPtr), dstVec);

  ctx.cc.endFunc();
  return ctx.finish<TestVVFunc>();
}

static TestVVFunc create_func_vvi(JitContext& ctx, SimdWidth simdWidth, OpcodeVVI op, uint32_t imm) noexcept {
  ctx.prepare();
  PipeCompiler pc(&ctx.cc, ctx.features, ctx.optFlags);

  asmjit::FuncNode* node = ctx.cc.newFunc(asmjit::FuncSignature::build<void, void*, const void*>());
  EXPECT_NOT_NULL(node);

  pc.initSimdWidth(simdWidth);
  pc.initFunction(node);

  Gp dstPtr = pc.newGpPtr("dstPtr");
  Gp srcPtr = pc.newGpPtr("srcPtr");

  node->setArg(0, dstPtr);
  node->setArg(1, srcPtr);

  Vec dstVec = pc.newVec(simdWidth, "dstVec");
  Vec srcVec = pc.newVec(simdWidth, "srcVec");

  pc.v_loaduvec(srcVec, mem_ptr(srcPtr));
  pc.emit_2vi(op, dstVec, srcVec, imm);
  pc.v_storeuvec(mem_ptr(dstPtr), dstVec);

  ctx.cc.endFunc();
  return ctx.finish<TestVVFunc>();
}

static TestVVVFunc create_func_vvv(JitContext& ctx, SimdWidth simdWidth, OpcodeVVV op) noexcept {
  ctx.prepare();
  PipeCompiler pc(&ctx.cc, ctx.features, ctx.optFlags);

  asmjit::FuncNode* node = ctx.cc.newFunc(asmjit::FuncSignature::build<void, void*, const void*, const void*>());
  EXPECT_NOT_NULL(node);

  pc.initSimdWidth(simdWidth);
  pc.initFunction(node);

  Gp dstPtr = pc.newGpPtr("dstPtr");
  Gp src1Ptr = pc.newGpPtr("src1Ptr");
  Gp src2Ptr = pc.newGpPtr("src2Ptr");

  node->setArg(0, dstPtr);
  node->setArg(1, src1Ptr);
  node->setArg(2, src2Ptr);

  Vec dstVec = pc.newVec(simdWidth, "dstVec");
  Vec src1Vec = pc.newVec(simdWidth, "src1Vec");
  Vec src2Vec = pc.newVec(simdWidth, "src2Vec");

  pc.v_loaduvec(src1Vec, mem_ptr(src1Ptr));
  pc.v_loaduvec(src2Vec, mem_ptr(src2Ptr));
  pc.emit_3v(op, dstVec, src1Vec, src2Vec);
  pc.v_storeuvec(mem_ptr(dstPtr), dstVec);

  ctx.cc.endFunc();
  return ctx.finish<TestVVVFunc>();
}

static TestVVVFunc create_func_vvvi(JitContext& ctx, SimdWidth simdWidth, OpcodeVVVI op, uint32_t imm) noexcept {
  ctx.prepare();
  PipeCompiler pc(&ctx.cc, ctx.features, ctx.optFlags);

  asmjit::FuncNode* node = ctx.cc.newFunc(asmjit::FuncSignature::build<void, void*, const void*, const void*>());
  EXPECT_NOT_NULL(node);

  pc.initSimdWidth(simdWidth);
  pc.initFunction(node);

  Gp dstPtr = pc.newGpPtr("dstPtr");
  Gp src1Ptr = pc.newGpPtr("src1Ptr");
  Gp src2Ptr = pc.newGpPtr("src2Ptr");

  node->setArg(0, dstPtr);
  node->setArg(1, src1Ptr);
  node->setArg(2, src2Ptr);

  Vec dstVec = pc.newVec(simdWidth, "dstVec");
  Vec src1Vec = pc.newVec(simdWidth, "src1Vec");
  Vec src2Vec = pc.newVec(simdWidth, "src2Vec");

  pc.v_loaduvec(src1Vec, mem_ptr(src1Ptr));
  pc.v_loaduvec(src2Vec, mem_ptr(src2Ptr));
  pc.emit_3vi(op, dstVec, src1Vec, src2Vec, imm);
  pc.v_storeuvec(mem_ptr(dstPtr), dstVec);

  ctx.cc.endFunc();
  return ctx.finish<TestVVVFunc>();
}

static TestVVVVFunc create_func_vvvv(JitContext& ctx, SimdWidth simdWidth, OpcodeVVVV op) noexcept {
  ctx.prepare();
  PipeCompiler pc(&ctx.cc, ctx.features, ctx.optFlags);

  asmjit::FuncNode* node = ctx.cc.newFunc(asmjit::FuncSignature::build<void, void*, const void*, const void*, const void*>());
  EXPECT_NOT_NULL(node);

  pc.initSimdWidth(simdWidth);
  pc.initFunction(node);

  Gp dstPtr = pc.newGpPtr("dstPtr");
  Gp src1Ptr = pc.newGpPtr("src1Ptr");
  Gp src2Ptr = pc.newGpPtr("src2Ptr");
  Gp src3Ptr = pc.newGpPtr("src3Ptr");

  node->setArg(0, dstPtr);
  node->setArg(1, src1Ptr);
  node->setArg(2, src2Ptr);
  node->setArg(3, src3Ptr);

  Vec dstVec = pc.newVec(simdWidth, "dstVec");
  Vec src1Vec = pc.newVec(simdWidth, "src1Vec");
  Vec src2Vec = pc.newVec(simdWidth, "src2Vec");
  Vec src3Vec = pc.newVec(simdWidth, "src3Vec");

  pc.v_loaduvec(src1Vec, mem_ptr(src1Ptr));
  pc.v_loaduvec(src2Vec, mem_ptr(src2Ptr));
  pc.v_loaduvec(src3Vec, mem_ptr(src3Ptr));
  pc.emit_4v(op, dstVec, src1Vec, src2Vec, src3Vec);
  pc.v_storeuvec(mem_ptr(dstPtr), dstVec);

  ctx.cc.endFunc();
  return ctx.finish<TestVVVVFunc>();
}
// bl::Pipeline::JIT - Tests - SIMD - Vector Overlay
// =================================================

template<uint32_t kW, typename T>
union VecOverlay {
  T items[kW / sizeof(T)];

  int8_t data_i8[kW];
  uint8_t data_u8[kW];

  int16_t data_i16[kW / 2u];
  uint16_t data_u16[kW / 2u];

  int32_t data_i32[kW / 4u];
  uint32_t data_u32[kW / 4u];

  int64_t data_i64[kW / 8u];
  uint64_t data_u64[kW / 8u];
};

// bl::Pipeline::JIT - Tests - SIMD - Data Generators & Constraints
// ================================================================

// Data generator, which is used to fill the content of SIMD registers.
class DataGenInt {
public:
  BLRandom rng;
  uint32_t step;

  BL_INLINE explicit DataGenInt(uint64_t seed) noexcept
    : rng(seed),
      step(0) {}

  uint64_t nextUInt64() noexcept {
    if (++step >= 256)
      step = 0;

    // NOTE: Nothing really elaborate - sometimes we want to test also numbers
    // that random number generators won't return often, so we hardcode some.
    switch (step) {
      case   0: return 0u;
      case   1: return 0u;
      case   2: return 0u;
      case   6: return 1u;
      case   7: return 0u;
      case  10: return 0u;
      case  11: return 0xFFu;
      case  15: return 0xFFFFu;
      case  17: return 0xFFFFFFFFu;
      case  21: return 0xFFFFFFFFFFFFFFFFu;
      case  24: return 1u;
      case  40: return 0xFFu;
      case  55: return 0x8080808080808080u;
      case  66: return 0x80000080u;
      case  69: return 1u;
      case  79: return 0x7F;
      case 122: return 0xFFFFu;
      case 123: return 0xFFFFu;
      case 124: return 0xFFFFu;
      case 127: return 1u;
      case 130: return 0xFFu;
      case 142: return 0x7FFFu;
      case 143: return 0x7FFFu;
      case 144: return 0u;
      case 145: return 0x7FFFu;
      default : return rng.nextUInt64();
    }
  }
};

// Some SIMD operations are constrained, especially those higher level. So, to successfully test these we
// have to model the constraints in a way that the SIMD instruction we test actually gets the correct input.
// Note that a constraint doesn't have to be always range based, it could be anything.
struct ConstraintNone {
  template<uint32_t kW, typename T>
  static BL_INLINE_NODEBUG void apply(VecOverlay<kW, T>& v) noexcept { blUnused(v); }
};

template<typename ElementT, typename Derived>
struct ConstraintBase {
  template<uint32_t kW, typename T>
  static BL_INLINE void apply(VecOverlay<kW, T>& v) noexcept {
    ElementT elements[kW / sizeof(ElementT)];

    memcpy(elements, v.data_u8, kW);
    for (size_t i = 0; i < kW / sizeof(ElementT); i++)
      elements[i] = Derived::apply_one(elements[i]);
    memcpy(v.data_u8, elements, kW);
  }
};

template<uint8_t kMin, uint8_t kMax>
struct ConstraintRangeU8 : public ConstraintBase<uint16_t, ConstraintRangeU8<kMin, kMax>> {
  static BL_INLINE_NODEBUG uint8_t apply_one(uint8_t x) noexcept { return blClamp(x, kMin, kMax); }
};

template<uint16_t kMin, uint16_t kMax>
struct ConstraintRangeU16 : public ConstraintBase<uint16_t, ConstraintRangeU16<kMin, kMax>> {
  static BL_INLINE_NODEBUG uint16_t apply_one(uint16_t x) noexcept { return blClamp(x, kMin, kMax); }
};

template<uint32_t kMin, uint32_t kMax>
struct ConstraintRangeU32 : public ConstraintBase<uint32_t, ConstraintRangeU32<kMin, kMax>> {
  static BL_INLINE_NODEBUG uint32_t apply_one(uint32_t x) noexcept { return blClamp(x, kMin, kMax); }
};

// bl::Pipeline::JIT - Tests - Generic Operations
// ==============================================

template<typename T>
static BL_INLINE_NODEBUG typename std::make_unsigned<T>::type cast_uint(const T& x) noexcept {
  return (typename std::make_unsigned<T>::type)x;
}

template<typename T>
static BL_INLINE_NODEBUG typename std::make_signed<T>::type cast_int(const T& x) noexcept {
  return (typename std::make_signed<T>::type)x;
}

static BL_INLINE_NODEBUG int8_t saturate_i16_to_i8(int16_t x) noexcept {
  return x < int16_t(-128) ? int8_t(-128) :
         x > int16_t( 127) ? int8_t( 127) : int8_t(x & 0xFF);
}

static BL_INLINE_NODEBUG uint8_t saturate_i16_to_u8(int16_t x) noexcept {
  return x < int16_t(0x00) ? uint8_t(0x00) :
         x > int16_t(0xFF) ? uint8_t(0xFF) : uint8_t(x & 0xFF);
}

static BL_INLINE_NODEBUG int16_t saturate_i32_to_i16(int32_t x) noexcept {
  return x < int32_t(-32768) ? int16_t(-32768) :
         x > int32_t( 32767) ? int16_t( 32767) : int16_t(x & 0xFFFF);
}

static BL_INLINE_NODEBUG uint16_t saturate_i32_to_u16(int32_t x) noexcept {
  return x < int32_t(0x0000) ? uint16_t(0x0000) :
         x > int32_t(0xFFFF) ? uint16_t(0xFFFF) : uint16_t(x & 0xFFFF);
}

template<typename T, typename Derived> struct op_base_vv {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a) noexcept {
    VecOverlay<kW, T> out{};
    for (uint32_t i = 0; i < kW / sizeof(T); i++)
      out.items[i] = Derived::apply_one(a.items[i]);
    return out;
  }
};

template<typename T, typename Derived> struct op_base_vvi {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, uint32_t imm) noexcept {
    VecOverlay<kW, T> out{};
    for (uint32_t i = 0; i < kW / sizeof(T); i++)
      out.items[i] = Derived::apply_one(a.items[i], imm);
    return out;
  }
};

template<typename T, typename Derived> struct op_base_vvv {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, const VecOverlay<kW, T>& b) noexcept {
    VecOverlay<kW, T> out{};
    for (uint32_t i = 0; i < kW / sizeof(T); i++)
      out.items[i] = Derived::apply_one(a.items[i], b.items[i]);
    return out;
  }
};

template<typename T, typename Derived> struct op_base_vvvi {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, const VecOverlay<kW, T>& b, uint32_t imm) noexcept {
    VecOverlay<kW, T> out{};
    for (uint32_t i = 0; i < kW / sizeof(T); i++)
      out.items[i] = Derived::apply_one(a.items[i], b.items[i], imm);
    return out;
  }
};

template<typename T, typename Derived> struct op_base_vvvv {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, const VecOverlay<kW, T>& b, const VecOverlay<kW, T>& c) noexcept {
    VecOverlay<kW, T> out{};
    for (uint32_t i = 0; i < kW / sizeof(T); i++)
      out.items[i] = Derived::apply_one(a.items[i], b.items[i], c.items[i]);
    return out;
  }
};

// bl::Pipeline::JIT - Tests - Generic Operations - VV
// ===================================================

template<typename T> struct iop_mov : public op_base_vv<T, iop_mov<T>> {
  static BL_INLINE_NODEBUG T apply_one(const T& a) noexcept { return a; }
};

struct iop_mov_u64 {
  template<uint32_t kW, typename T>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a) noexcept {
    VecOverlay<kW, T> out{};
    out.data_u64[0] = a.data_u64[0];
    return out;
  }
};

template<typename T> struct iop_abs : public op_base_vv<T, iop_abs<T>> {
  static BL_INLINE_NODEBUG T apply_one(const T& a) noexcept { return a < 0 ? T(cast_uint(T(0)) - cast_uint(a)) : a; }
};

template<typename T> struct iop_neg : public op_base_vv<T, iop_neg<T>> {
  static BL_INLINE_NODEBUG T apply_one(const T& a) noexcept { return T(cast_uint(T(0)) - cast_uint(a)); }
};

template<typename T> struct iop_not : public op_base_vv<T, iop_not<T>> {
  static BL_INLINE_NODEBUG T apply_one(const T& a) noexcept { return T(~a); }
};

template<typename T> struct iop_broadcast_u8 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a) noexcept {
    VecOverlay<kW, T> out{};
    for (uint32_t i = 0; i < kW; i++)
      out.data_u8[i] = a.data_u8[0];
    return out;
  }
};

template<typename T> struct iop_broadcast_u16 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a) noexcept {
    VecOverlay<kW, T> out{};
    for (uint32_t i = 0; i < kW / 2u; i++)
      out.data_u16[i] = a.data_u16[0];
    return out;
  }
};

template<typename T> struct iop_broadcast_u32 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a) noexcept {
    VecOverlay<kW, T> out{};
    for (uint32_t i = 0; i < kW / 4u; i++)
      out.data_u32[i] = a.data_u32[0];
    return out;
  }
};

template<typename T> struct iop_broadcast_u64 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a) noexcept {
    VecOverlay<kW, T> out{};
    for (uint32_t i = 0; i < kW / 8u; i++)
      out.data_u64[i] = a.data_u64[0];
    return out;
  }
};

// bl::Pipeline::JIT - Tests - Generic Operations - VVI
// ====================================================

template<typename T> struct iop_slli : public op_base_vvi<T, iop_slli<T>> {
  static BL_INLINE_NODEBUG T apply_one(const T& a, uint32_t imm) noexcept { return T(cast_uint(a) << imm); }
};

template<typename T> struct iop_srli : public op_base_vvi<T, iop_srli<T>> {
  static BL_INLINE_NODEBUG T apply_one(const T& a, uint32_t imm) noexcept { return T(cast_uint(a) >> imm); }
};

template<typename T> struct iop_rsrli : public op_base_vvi<T, iop_rsrli<T>> {
  static BL_INLINE T apply_one(const T& a, uint32_t imm) noexcept {
    T add = T((a & (T(1) << (imm - 1))) != 0);
    return T((cast_uint(a) >> imm) + cast_uint(add));
  }
};

template<typename T> struct iop_srai : public op_base_vvi<T, iop_srai<T>> {
  static BL_INLINE_NODEBUG T apply_one(const T& a, uint32_t imm) noexcept { return T(cast_int(a) >> imm); }
};

template<typename T> struct iop_sllb_u128 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, uint32_t imm) noexcept {
    VecOverlay<kW, T> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      for (uint32_t i = 0; i < 16; i++) {
        out.data_u8[off + i] = i < imm ? uint8_t(0) : a.data_u8[off + i - imm];
      }
    }
    return out;
  }
};

template<typename T> struct iop_srlb_u128 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, uint32_t imm) noexcept {
    VecOverlay<kW, T> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      for (uint32_t i = 0; i < 16; i++) {
        out.data_u8[off + i] = i + imm < 16u ? a.data_u8[off + i + imm] : uint8_t(0);
      }
    }
    return out;
  }
};

template<typename T> struct iop_swizzle_u16 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, uint32_t imm) noexcept {
    uint32_t D = (imm >> 24) & 0x3;
    uint32_t C = (imm >> 16) & 0x3;
    uint32_t B = (imm >>  8) & 0x3;
    uint32_t A = (imm >>  0) & 0x3;

    VecOverlay<kW, T> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u16[off / 2 + 0] = a.data_u16[off / 2 + 0 + A];
      out.data_u16[off / 2 + 1] = a.data_u16[off / 2 + 0 + B];
      out.data_u16[off / 2 + 2] = a.data_u16[off / 2 + 0 + C];
      out.data_u16[off / 2 + 3] = a.data_u16[off / 2 + 0 + D];
      out.data_u16[off / 2 + 4] = a.data_u16[off / 2 + 4 + A];
      out.data_u16[off / 2 + 5] = a.data_u16[off / 2 + 4 + B];
      out.data_u16[off / 2 + 6] = a.data_u16[off / 2 + 4 + C];
      out.data_u16[off / 2 + 7] = a.data_u16[off / 2 + 4 + D];
    }
    return out;
  }
};

template<typename T> struct iop_swizzle_lo_u16x4 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, uint32_t imm) noexcept {
    uint32_t D = (imm >> 24) & 0x3;
    uint32_t C = (imm >> 16) & 0x3;
    uint32_t B = (imm >>  8) & 0x3;
    uint32_t A = (imm >>  0) & 0x3;

    VecOverlay<kW, T> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u16[off / 2 + 0] = a.data_u16[off / 2 + A];
      out.data_u16[off / 2 + 1] = a.data_u16[off / 2 + B];
      out.data_u16[off / 2 + 2] = a.data_u16[off / 2 + C];
      out.data_u16[off / 2 + 3] = a.data_u16[off / 2 + D];
      memcpy(out.data_u8 + off + 8, a.data_u8 + off + 8, 8);
    }
    return out;
  }
};

template<typename T> struct iop_swizzle_hi_u16x4 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, uint32_t imm) noexcept {
    uint32_t D = (imm >> 24) & 0x3;
    uint32_t C = (imm >> 16) & 0x3;
    uint32_t B = (imm >>  8) & 0x3;
    uint32_t A = (imm >>  0) & 0x3;

    VecOverlay<kW, T> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      memcpy(out.data_u8 + off, a.data_u8 + off, 8);
      out.data_u16[off / 2 + 4] = a.data_u16[off / 2 + 4 + A];
      out.data_u16[off / 2 + 5] = a.data_u16[off / 2 + 4 + B];
      out.data_u16[off / 2 + 6] = a.data_u16[off / 2 + 4 + C];
      out.data_u16[off / 2 + 7] = a.data_u16[off / 2 + 4 + D];
    }
    return out;
  }
};

template<typename T> struct iop_swizzle_u32x4 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, uint32_t imm) noexcept {
    uint32_t D = (imm >> 24) & 0x3;
    uint32_t C = (imm >> 16) & 0x3;
    uint32_t B = (imm >>  8) & 0x3;
    uint32_t A = (imm >>  0) & 0x3;

    VecOverlay<kW, T> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u32[off / 4 + 0] = a.data_u32[off / 4 + A];
      out.data_u32[off / 4 + 1] = a.data_u32[off / 4 + B];
      out.data_u32[off / 4 + 2] = a.data_u32[off / 4 + C];
      out.data_u32[off / 4 + 3] = a.data_u32[off / 4 + D];
    }
    return out;
  }
};

template<typename T> struct iop_swizzle_u64x2 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, uint32_t imm) noexcept {
    uint32_t B = (imm >>  8) & 0x1;
    uint32_t A = (imm >>  0) & 0x1;

    VecOverlay<kW, T> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u64[off / 8 + 0] = a.data_u64[off / 8 + A];
      out.data_u64[off / 8 + 1] = a.data_u64[off / 8 + B];
    }
    return out;
  }
};

// bl::Pipeline::JIT - Tests - SIMD - Generic Operations - VVV
// ===========================================================

template<typename T> struct iop_and : public op_base_vvv<T, iop_and<T>> {
  static BL_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return T(a & b); }
};

template<typename T> struct iop_or : public op_base_vvv<T, iop_or<T>> {
  static BL_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return T(a | b); }
};

template<typename T> struct iop_xor : public op_base_vvv<T, iop_xor<T>> {
  static BL_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return T(a ^ b); }
};

template<typename T> struct iop_andn : public op_base_vvv<T, iop_andn<T>> {
  static BL_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return T(~a & b); }
};

template<typename T> struct iop_bic : public op_base_vvv<T, iop_bic<T>> {
  static BL_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return T(a & ~b); }
};

template<typename T> struct iop_add : public op_base_vvv<T, iop_add<T>> {
  static BL_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return T(cast_uint(a) + cast_uint(b)); }
};

template<typename T> struct iop_adds : public op_base_vvv<T, iop_adds<T>> {
  static BL_INLINE T apply_one(const T& a, const T& b) noexcept {
    bl::OverflowFlag of{};
    T result = IntOps::addOverflow(a, b, &of);

    if (!of)
      return result;

    if (Traits::isUnsigned<T>() || b > 0)
      return Traits::maxValue<T>();
    else
      return Traits::minValue<T>();
  }
};

template<typename T> struct iop_sub : public op_base_vvv<T, iop_sub<T>> {
  static BL_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return T(cast_uint(a) - cast_uint(b)); }
};

template<typename T> struct iop_subs : public op_base_vvv<T, iop_subs<T>> {
  static BL_INLINE T apply_one(const T& a, const T& b) noexcept {
    bl::OverflowFlag of{};
    T result = IntOps::subOverflow(a, b, &of);

    if (!of)
      return result;

    if (Traits::isUnsigned<T>() || b > 0)
      return Traits::minValue<T>();
    else
      return Traits::maxValue<T>();
  }
};

template<typename T> struct iop_mul : public op_base_vvv<T, iop_mul<T>> {
  static BL_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return T((uint64_t(a) * uint64_t(b)) & ~T(0)); }
};

template<typename T> struct iop_mulhi : public op_base_vvv<T, iop_mulhi<T>> {
  static BL_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept {
    uint64_t result = uint64_t(int64_t(cast_int(a))) * uint64_t(int64_t(cast_int(b)));
    return T((result >> (sizeof(T) * 8u)) & ~T(0));
  }
};

template<typename T> struct iop_mulhu : public op_base_vvv<T, iop_mulhu<T>> {
  static BL_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept {
    uint64_t result = uint64_t(a) * uint64_t(b);
    return T((result >> (sizeof(T) * 8u)) & ~T(0));
  }
};

struct iop_mul_u64_lo_u32 : public op_base_vvv<uint64_t, iop_mul_u64_lo_u32> {
  static BL_INLINE_NODEBUG uint64_t apply_one(const uint64_t& a, const uint64_t& b) noexcept {
    return uint64_t(a) * uint64_t(b & 0xFFFFFFFFu);
  }
};

struct iop_mhadd_i16_i32 : public op_base_vvv<uint32_t, iop_mhadd_i16_i32> {
  static BL_INLINE_NODEBUG uint32_t apply_one(const uint32_t& a, const uint32_t& b) noexcept {
    uint32_t al = uint32_t(int32_t(int16_t(a & 0xFFFF)));
    uint32_t ah = uint32_t(int32_t(int16_t(a >> 16)));

    uint32_t bl = uint32_t(int32_t(int16_t(b & 0xFFFF)));
    uint32_t bh = uint32_t(int32_t(int16_t(b >> 16)));

    return al * bl + ah * bh;
  }
};

template<typename T> struct iop_min : public op_base_vvv<T, iop_min<T>> {
  static BL_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return a < b ? a : b; }
};

template<typename T> struct iop_max : public op_base_vvv<T, iop_max<T>> {
  static BL_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return a > b ? a : b; }
};

template<typename T> struct iop_cmp_eq : public op_base_vvv<T, iop_cmp_eq<T>> {
  static BL_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return a == b ? IntOps::allOnes<T>() : T(0); }
};

template<typename T> struct iop_cmp_ne : public op_base_vvv<T, iop_cmp_ne<T>> {
  static BL_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return a != b ? IntOps::allOnes<T>() : T(0); }
};

template<typename T> struct iop_cmp_gt : public op_base_vvv<T, iop_cmp_gt<T>> {
  static BL_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return a >  b ? IntOps::allOnes<T>() : T(0); }
};

template<typename T> struct iop_cmp_ge : public op_base_vvv<T, iop_cmp_ge<T>> {
  static BL_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return a >= b ? IntOps::allOnes<T>() : T(0); }
};

template<typename T> struct iop_cmp_lt : public op_base_vvv<T, iop_cmp_lt<T>> {
  static BL_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return a <  b ? IntOps::allOnes<T>() : T(0); }
};

template<typename T> struct iop_cmp_le : public op_base_vvv<T, iop_cmp_le<T>> {
  static BL_INLINE_NODEBUG T apply_one(const T& a, const T& b) noexcept { return a <= b ? IntOps::allOnes<T>() : T(0); }
};

struct iop_combine_lo_hi_u64 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, uint64_t> apply(const VecOverlay<kW, uint64_t>& a, const VecOverlay<kW, uint64_t>& b) noexcept {
    VecOverlay<kW, uint64_t> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u64[off / 8 + 0] = b.data_u64[off / 8 + 1];
      out.data_u64[off / 8 + 1] = a.data_u64[off / 8 + 0];
    }
    return out;
  }
};

struct iop_combine_hi_lo_u64 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, uint64_t> apply(const VecOverlay<kW, uint64_t>& a, const VecOverlay<kW, uint64_t>& b) noexcept {
    VecOverlay<kW, uint64_t> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u64[off / 8 + 0] = b.data_u64[off / 8 + 0];
      out.data_u64[off / 8 + 1] = a.data_u64[off / 8 + 1];
    }
    return out;
  }
};

template<typename T> struct iop_interleave_lo_u8 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, const VecOverlay<kW, T>& b) noexcept {
    VecOverlay<kW, T> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      for (uint32_t i = 0; i < 8; i++) {
        out.data_u8[off + i * 2 + 0] = a.data_u8[off + i];
        out.data_u8[off + i * 2 + 1] = b.data_u8[off + i];
      }
    }
    return out;
  }
};

template<typename T> struct iop_interleave_hi_u8 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, const VecOverlay<kW, T>& b) noexcept {
    VecOverlay<kW, T> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      for (uint32_t i = 0; i < 8; i++) {
        out.data_u8[off + i * 2 + 0] = a.data_u8[off + 8 + i];
        out.data_u8[off + i * 2 + 1] = b.data_u8[off + 8 + i];
      }
    }
    return out;
  }
};

template<typename T> struct iop_interleave_lo_u16 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, const VecOverlay<kW, T>& b) noexcept {
    VecOverlay<kW, T> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      for (uint32_t i = 0; i < 4; i++) {
        out.data_u16[off / 2 + i * 2 + 0] = a.data_u16[off / 2 + i];
        out.data_u16[off / 2 + i * 2 + 1] = b.data_u16[off / 2 + i];
      }
    }
    return out;
  }
};

template<typename T> struct iop_interleave_hi_u16 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, const VecOverlay<kW, T>& b) noexcept {
    VecOverlay<kW, T> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      for (uint32_t i = 0; i < 4; i++) {
        out.data_u16[off / 2 + i * 2 + 0] = a.data_u16[off / 2 + 4 + i];
        out.data_u16[off / 2 + i * 2 + 1] = b.data_u16[off / 2 + 4 + i];
      }
    }
    return out;
  }
};

template<typename T> struct iop_interleave_lo_u32 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, const VecOverlay<kW, T>& b) noexcept {
    VecOverlay<kW, T> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      for (uint32_t i = 0; i < 2; i++) {
        out.data_u32[off / 4 + i * 2 + 0] = a.data_u32[off / 4 + i];
        out.data_u32[off / 4 + i * 2 + 1] = b.data_u32[off / 4 + i];
      }
    }
    return out;
  }
};

template<typename T> struct iop_interleave_hi_u32 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, const VecOverlay<kW, T>& b) noexcept {
    VecOverlay<kW, T> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      for (uint32_t i = 0; i < 2; i++) {
        out.data_u32[off / 4 + i * 2 + 0] = a.data_u32[off / 4 + 2 + i];
        out.data_u32[off / 4 + i * 2 + 1] = b.data_u32[off / 4 + 2 + i];
      }
    }
    return out;
  }
};

template<typename T> struct iop_interleave_lo_u64 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, const VecOverlay<kW, T>& b) noexcept {
    VecOverlay<kW, T> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u64[off / 8 + 0] = a.data_u64[off / 8 + 0];
      out.data_u64[off / 8 + 1] = b.data_u64[off / 8 + 0];
    }
    return out;
  }
};

template<typename T> struct iop_interleave_hi_u64 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, const VecOverlay<kW, T>& b) noexcept {
    VecOverlay<kW, T> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u64[off / 8 + 0] = a.data_u64[off / 8 + 1];
      out.data_u64[off / 8 + 1] = b.data_u64[off / 8 + 1];
    }
    return out;
  }
};

// bl::Pipeline::JIT - Tests - SIMD - Generic Operations - VVVI
// ============================================================

template<typename T> struct iop_alignr_u128 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, const VecOverlay<kW, T>& b, uint32_t imm) noexcept {
    VecOverlay<kW, T> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      for (uint32_t i = 0; i < 16; i++) {
        out.data_u8[off + i] = i + imm < 16 ? b.data_u8[off + i + imm] : a.data_u8[off + i + imm - 16];
      }
    }
    return out;
  }
};

template<typename T> struct iop_interleave_shuffle_u32x4 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, const VecOverlay<kW, T>& b, uint32_t imm) noexcept {
    uint32_t D = (imm >> 24) & 0x3;
    uint32_t C = (imm >> 16) & 0x3;
    uint32_t B = (imm >>  8) & 0x3;
    uint32_t A = (imm >>  0) & 0x3;

    VecOverlay<kW, T> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u32[off / 4 + 0] = a.data_u32[off / 4 + A];
      out.data_u32[off / 4 + 1] = a.data_u32[off / 4 + B];
      out.data_u32[off / 4 + 2] = b.data_u32[off / 4 + C];
      out.data_u32[off / 4 + 3] = b.data_u32[off / 4 + D];
    }
    return out;
  }
};

template<typename T> struct iop_interleave_shuffle_u64x2 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, const VecOverlay<kW, T>& b, uint32_t imm) noexcept {
    uint32_t B = (imm >>  8) & 0x1;
    uint32_t A = (imm >>  0) & 0x1;

    VecOverlay<kW, T> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u64[off / 8 + 0] = a.data_u64[off / 8 + A];
      out.data_u64[off / 8 + 1] = b.data_u64[off / 8 + B];
    }
    return out;
  }
};

template<typename T> struct iop_packs_i16_i8 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, const VecOverlay<kW, T>& b) noexcept {
    VecOverlay<kW, T> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_i8[off +  0] = saturate_i16_to_i8(a.data_i16[off / 2 + 0]);
      out.data_i8[off +  1] = saturate_i16_to_i8(a.data_i16[off / 2 + 1]);
      out.data_i8[off +  2] = saturate_i16_to_i8(a.data_i16[off / 2 + 2]);
      out.data_i8[off +  3] = saturate_i16_to_i8(a.data_i16[off / 2 + 3]);
      out.data_i8[off +  4] = saturate_i16_to_i8(a.data_i16[off / 2 + 4]);
      out.data_i8[off +  5] = saturate_i16_to_i8(a.data_i16[off / 2 + 5]);
      out.data_i8[off +  6] = saturate_i16_to_i8(a.data_i16[off / 2 + 6]);
      out.data_i8[off +  7] = saturate_i16_to_i8(a.data_i16[off / 2 + 7]);
      out.data_i8[off +  8] = saturate_i16_to_i8(b.data_i16[off / 2 + 0]);
      out.data_i8[off +  9] = saturate_i16_to_i8(b.data_i16[off / 2 + 1]);
      out.data_i8[off + 10] = saturate_i16_to_i8(b.data_i16[off / 2 + 2]);
      out.data_i8[off + 11] = saturate_i16_to_i8(b.data_i16[off / 2 + 3]);
      out.data_i8[off + 12] = saturate_i16_to_i8(b.data_i16[off / 2 + 4]);
      out.data_i8[off + 13] = saturate_i16_to_i8(b.data_i16[off / 2 + 5]);
      out.data_i8[off + 14] = saturate_i16_to_i8(b.data_i16[off / 2 + 6]);
      out.data_i8[off + 15] = saturate_i16_to_i8(b.data_i16[off / 2 + 7]);
    }
    return out;
  }
};

template<typename T> struct iop_packs_i16_u8 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, const VecOverlay<kW, T>& b) noexcept {
    VecOverlay<kW, T> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u8[off +  0] = saturate_i16_to_u8(a.data_i16[off / 2 + 0]);
      out.data_u8[off +  1] = saturate_i16_to_u8(a.data_i16[off / 2 + 1]);
      out.data_u8[off +  2] = saturate_i16_to_u8(a.data_i16[off / 2 + 2]);
      out.data_u8[off +  3] = saturate_i16_to_u8(a.data_i16[off / 2 + 3]);
      out.data_u8[off +  4] = saturate_i16_to_u8(a.data_i16[off / 2 + 4]);
      out.data_u8[off +  5] = saturate_i16_to_u8(a.data_i16[off / 2 + 5]);
      out.data_u8[off +  6] = saturate_i16_to_u8(a.data_i16[off / 2 + 6]);
      out.data_u8[off +  7] = saturate_i16_to_u8(a.data_i16[off / 2 + 7]);
      out.data_u8[off +  8] = saturate_i16_to_u8(b.data_i16[off / 2 + 0]);
      out.data_u8[off +  9] = saturate_i16_to_u8(b.data_i16[off / 2 + 1]);
      out.data_u8[off + 10] = saturate_i16_to_u8(b.data_i16[off / 2 + 2]);
      out.data_u8[off + 11] = saturate_i16_to_u8(b.data_i16[off / 2 + 3]);
      out.data_u8[off + 12] = saturate_i16_to_u8(b.data_i16[off / 2 + 4]);
      out.data_u8[off + 13] = saturate_i16_to_u8(b.data_i16[off / 2 + 5]);
      out.data_u8[off + 14] = saturate_i16_to_u8(b.data_i16[off / 2 + 6]);
      out.data_u8[off + 15] = saturate_i16_to_u8(b.data_i16[off / 2 + 7]);
    }
    return out;
  }
};

template<typename T> struct iop_packs_i32_i16 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, const VecOverlay<kW, T>& b) noexcept {
    VecOverlay<kW, T> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_i16[off / 2 + 0] = saturate_i32_to_i16(a.data_i32[off / 4 + 0]);
      out.data_i16[off / 2 + 1] = saturate_i32_to_i16(a.data_i32[off / 4 + 1]);
      out.data_i16[off / 2 + 2] = saturate_i32_to_i16(a.data_i32[off / 4 + 2]);
      out.data_i16[off / 2 + 3] = saturate_i32_to_i16(a.data_i32[off / 4 + 3]);
      out.data_i16[off / 2 + 4] = saturate_i32_to_i16(b.data_i32[off / 4 + 0]);
      out.data_i16[off / 2 + 5] = saturate_i32_to_i16(b.data_i32[off / 4 + 1]);
      out.data_i16[off / 2 + 6] = saturate_i32_to_i16(b.data_i32[off / 4 + 2]);
      out.data_i16[off / 2 + 7] = saturate_i32_to_i16(b.data_i32[off / 4 + 3]);
    }
    return out;
  }
};

template<typename T> struct iop_packs_i32_u16 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, const VecOverlay<kW, T>& b) noexcept {
    VecOverlay<kW, T> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      out.data_u16[off / 2 + 0] = saturate_i32_to_u16(a.data_i32[off / 4 + 0]);
      out.data_u16[off / 2 + 1] = saturate_i32_to_u16(a.data_i32[off / 4 + 1]);
      out.data_u16[off / 2 + 2] = saturate_i32_to_u16(a.data_i32[off / 4 + 2]);
      out.data_u16[off / 2 + 3] = saturate_i32_to_u16(a.data_i32[off / 4 + 3]);
      out.data_u16[off / 2 + 4] = saturate_i32_to_u16(b.data_i32[off / 4 + 0]);
      out.data_u16[off / 2 + 5] = saturate_i32_to_u16(b.data_i32[off / 4 + 1]);
      out.data_u16[off / 2 + 6] = saturate_i32_to_u16(b.data_i32[off / 4 + 2]);
      out.data_u16[off / 2 + 7] = saturate_i32_to_u16(b.data_i32[off / 4 + 3]);
    }
    return out;
  }
};

// bl::Pipeline::JIT - Tests - SIMD - Generic Operations - VVVV
// ============================================================

// TODO:
template<typename T> struct iop_blendv_bits : public op_base_vvvv<T, iop_blendv_bits<T>> {
  static BL_INLINE_NODEBUG T apply_one(const T& a, const T& b, const T& c) noexcept { return T((a & ~c) | (b & c)); }
};

template<typename T> struct iop_swizzlev_u8 {
  template<uint32_t kW>
  static BL_INLINE VecOverlay<kW, T> apply(const VecOverlay<kW, T>& a, const VecOverlay<kW, T>& b) noexcept {
    VecOverlay<kW, T> out{};
    for (uint32_t off = 0; off < kW; off += 16) {
      for (uint32_t i = 0; i < 16; i++) {
        size_t sel = b.data_u8[off + i] & (0x8F); // 3 bits ignored.
        out.data_u8[off + i] = sel & 0x80 ? uint8_t(0) : a.data_u8[off + sel];
      }
    }
    return out;
  }
};

struct iop_div255_u16 : public op_base_vv<uint16_t, iop_div255_u16> {
  static BL_INLINE uint16_t apply_one(const uint16_t& a) noexcept {
    uint32_t x = a + 0x80u;
    return uint16_t((x + (x >> 8)) >> 8);
  }
};

struct iop_div65535_u32 : public op_base_vv<uint32_t, iop_div65535_u32> {
  static BL_INLINE uint32_t apply_one(const uint32_t& a) noexcept {
    uint32_t x = a + 0x8000u;
    return uint32_t((x + (x >> 16)) >> 16);
  }
};

// bl::Pipeline::JIT - Tests - SIMD - Utilities
// ============================================

template<uint32_t kW, typename T>
static void fill_random(DataGenInt& dg, VecOverlay<kW, T>& dst) noexcept {
  for (uint32_t i = 0; i < kW / 8u; i++)
    dst.data_u64[i] = dg.nextUInt64();
}

template<typename T>
static void fill_val(T* arr, T v, uint32_t count, uint32_t repeat = 1) noexcept {
  uint32_t add = 0;
  for (uint32_t i = 0; i < count; i++) {
    arr[i] = T(v + T(add));
    if (++add >= repeat)
      add = 0;
  }
}

// bl::Pipeline::JIT - Tests - SIMD - Verification
// ===============================================

template<typename T> struct TypeNameToString {};
template<> struct TypeNameToString<int8_t  > { static BL_INLINE_NODEBUG const char* get() noexcept { return "int8"; } };
template<> struct TypeNameToString<int16_t > { static BL_INLINE_NODEBUG const char* get() noexcept { return "int16"; } };
template<> struct TypeNameToString<int32_t > { static BL_INLINE_NODEBUG const char* get() noexcept { return "int32"; } };
template<> struct TypeNameToString<int64_t > { static BL_INLINE_NODEBUG const char* get() noexcept { return "int64"; } };
template<> struct TypeNameToString<uint8_t > { static BL_INLINE_NODEBUG const char* get() noexcept { return "uint8"; } };
template<> struct TypeNameToString<uint16_t> { static BL_INLINE_NODEBUG const char* get() noexcept { return "uint16"; } };
template<> struct TypeNameToString<uint32_t> { static BL_INLINE_NODEBUG const char* get() noexcept { return "uint32"; } };
template<> struct TypeNameToString<uint64_t> { static BL_INLINE_NODEBUG const char* get() noexcept { return "uint64"; } };
template<> struct TypeNameToString<float   > { static BL_INLINE_NODEBUG const char* get() noexcept { return "float32"; } };
template<> struct TypeNameToString<double  > { static BL_INLINE_NODEBUG const char* get() noexcept { return "float64"; } };

template<typename T>
static BL_NOINLINE BLString format_items(const T* items, uint32_t count) noexcept {
  BLString s;
  s.append('{');
  for (uint32_t i = 0; i < count; i++)
    s.appendFormat("%s%llu", i == 0 ? "" : ", ", (unsigned long long)items[i] & IntOps::allOnes<typename std::make_unsigned<T>::type>());
  s.append('}');
  return s;
}

template<typename T>
static bool compare_ivec(const T* observed, const T* expected, uint32_t count) noexcept {
  for (uint32_t i = 0; i < count; i++)
    if (BL_UNLIKELY(observed[i] != expected[i]))
      return false;
  return true;
}

template<typename T>
static void verify_ivec(const T* observed, const T* expected, uint32_t count, const char* assembly) noexcept {
  if (!compare_ivec(observed, expected, count)) {
    BLString observed_str = format_items(observed, count);
    BLString expected_str = format_items(expected, count);

    EXPECT_EQ(observed_str, expected_str)
      .message("Operation failed\n"
              "      Expected: %s\n"
              "      Observed: %s\n"
             "Assembly:\n%s",
              expected_str.data(),
              observed_str.data(),
              assembly);
  }
}

template<typename T>
static BL_NOINLINE void test_iop_vv_failed(const T* input1, const T* observed, const T* expected, uint32_t count, const char* assembly) noexcept {
  BLString input1_str = format_items(input1, count);
  BLString observed_str = format_items(observed, count);
  BLString expected_str = format_items(expected, count);

  EXPECT_EQ(observed_str, expected_str)
    .message("Operation failed:\n"
             "      Input #1: %s\n"
             "      Expected: %s\n"
             "      Observed: %s\n"
             "Assembly:\n%s",
             input1_str.data(),
             expected_str.data(),
             observed_str.data(),
             assembly);
}

template<typename T>
static BL_NOINLINE void test_iop_vvi_failed(const T* input1, const T* observed, const T* expected, uint32_t count, uint32_t imm, const char* assembly) noexcept {
  BLString input1_str = format_items(input1, count);
  BLString observed_str = format_items(observed, count);
  BLString expected_str = format_items(expected, count);

  EXPECT_EQ(observed_str, expected_str)
    .message("Operation failed:\n"
             "      Input #1: %s\n"
             "      ImmValue: %u (0x%08X)\n"
             "      Expected: %s\n"
             "      Observed: %s\n"
             "Assembly:\n%s",
             input1_str.data(),
             imm, imm,
             expected_str.data(),
             observed_str.data(),
             assembly);
}

template<typename T>
static BL_NOINLINE void test_iop_vvv_failed(const T* input1, const T* input2, const T* observed, const T* expected, uint32_t count, const char* assembly) noexcept {
  BLString input1_str = format_items(input1, count);
  BLString input2_str = format_items(input2, count);
  BLString observed_str = format_items(observed, count);
  BLString expected_str = format_items(expected, count);

  EXPECT_EQ(observed_str, expected_str)
    .message("Operation failed:\n"
             "      Input #1: %s\n"
             "      Input #2: %s\n"
             "      Expected: %s\n"
             "      Observed: %s\n"
             "Assembly:\n%s",
             input1_str.data(),
             input2_str.data(),
             expected_str.data(),
             observed_str.data(),
             assembly);
}

template<typename T>
static BL_NOINLINE void test_iop_vvvi_failed(const T* input1, const T* input2, const T* observed, const T* expected, uint32_t count, uint32_t imm, const char* assembly) noexcept {
  BLString input1_str = format_items(input1, count);
  BLString input2_str = format_items(input2, count);
  BLString observed_str = format_items(observed, count);
  BLString expected_str = format_items(expected, count);

  EXPECT_EQ(observed_str, expected_str)
    .message("Operation failed:\n"
             "      Input #1: %s\n"
             "      Input #2: %s\n"
             "      ImmValue: %u (0x%08X)\n"
             "      Expected: %s\n"
             "      Observed: %s\n"
             "Assembly:\n%s",
             input1_str.data(),
             input2_str.data(),
             imm, imm,
             expected_str.data(),
             observed_str.data(),
             assembly);
}

template<typename T>
static BL_NOINLINE void test_iop_vvvv_failed(const T* input1, const T* input2, const T* input3, const T* observed, const T* expected, uint32_t count, const char* assembly) noexcept {
  BLString input1_str = format_items(input1, count);
  BLString input2_str = format_items(input2, count);
  BLString input3_str = format_items(input3, count);
  BLString observed_str = format_items(observed, count);
  BLString expected_str = format_items(expected, count);

  EXPECT_EQ(observed_str, expected_str)
    .message("Operation failed\n"
             "      Input #1: %s\n"
             "      Input #2: %s\n"
             "      Input #3: %s\n"
             "      Expected: %s\n"
             "      Observed: %s\n"
             "Assembly:\n%s",
             input1_str.data(),
             input2_str.data(),
             input3_str.data(),
             expected_str.data(),
             observed_str.data(),
             assembly);
}

// bl::Pipeline::JIT - Tests - Integer Operations - VV
// ===================================================

template<SimdWidth kSimdWidth, typename T, OpcodeVV kOp, typename GenericOp, typename Constraint>
static BL_NOINLINE void test_iop_vv_constraint(JitContext& ctx, uint32_t variation = 0) noexcept {
  constexpr uint32_t kW = vecWidthFromSimdWidth(kSimdWidth);
  constexpr uint32_t kItemCount = kW / uint32_t(sizeof(T));

  TestVVFunc compiledApply = create_func_vv(ctx, kSimdWidth, kOp, variation);
  DataGenInt dg(kRandomSeed);

  for (uint32_t iter = 0; iter < kTestIterCount; iter++) {
    VecOverlay<kW, T> a {};
    VecOverlay<kW, T> observed {};
    VecOverlay<kW, T> expected {};

    fill_random(dg, a);
    Constraint::apply(a);

    compiledApply(&observed, &a);
    expected = GenericOp::apply(a);

    if (!compare_ivec(observed.items, expected.items, kItemCount))
      test_iop_vv_failed(a.items, observed.items, expected.items, kItemCount, ctx.logger.data());
  }

  ctx.rt.release(compiledApply);
}

template<SimdWidth kSimdWidth, typename T, OpcodeVV kOp, typename GenericOp>
static void test_iop_vv(JitContext& ctx, uint32_t variation = 0) noexcept {
  return test_iop_vv_constraint<kSimdWidth, T, kOp, GenericOp, ConstraintNone>(ctx, variation);
}

// bl::Pipeline::JIT - Tests - SIMD - Integer Operations - VVI
// ===========================================================

template<SimdWidth kSimdWidth, typename T, OpcodeVVI kOp, typename GenericOp, typename Constraint>
static BL_NOINLINE void test_iop_vvi_constraint(JitContext& ctx, uint32_t imm) noexcept {
  constexpr uint32_t kW = vecWidthFromSimdWidth(kSimdWidth);
  constexpr uint32_t kItemCount = kW / uint32_t(sizeof(T));

  TestVVFunc compiledApply = create_func_vvi(ctx, kSimdWidth, kOp, imm);
  DataGenInt dg(kRandomSeed);

  for (uint32_t iter = 0; iter < kTestIterCount; iter++) {
    VecOverlay<kW, T> a {};
    VecOverlay<kW, T> observed {};
    VecOverlay<kW, T> expected {};

    fill_random(dg, a);
    Constraint::apply(a);

    compiledApply(&observed, &a);
    expected = GenericOp::apply(a, imm);

    if (!compare_ivec(observed.items, expected.items, kItemCount))
      test_iop_vvi_failed(a.items, observed.items, expected.items, kItemCount, imm, ctx.logger.data());
  }

  ctx.rt.release(compiledApply);
}

template<SimdWidth kSimdWidth, typename T, OpcodeVVI kOp, typename GenericOp>
static void test_iop_vvi(JitContext& ctx, uint32_t imm) noexcept {
  return test_iop_vvi_constraint<kSimdWidth, T, kOp, GenericOp, ConstraintNone>(ctx, imm);
}

// bl::Pipeline::JIT - Tests - SIMD - Integer Operations - VVV
// ===========================================================

template<SimdWidth kSimdWidth, typename T, OpcodeVVV kOp, typename GenericOp, typename Constraint>
static BL_NOINLINE void test_iop_vvv_constraint(JitContext& ctx) noexcept {
  constexpr uint32_t kW = vecWidthFromSimdWidth(kSimdWidth);
  constexpr uint32_t kItemCount = kW / uint32_t(sizeof(T));

  TestVVVFunc compiledApply = create_func_vvv(ctx, kSimdWidth, kOp);
  DataGenInt dg(kRandomSeed);

  for (uint32_t iter = 0; iter < kTestIterCount; iter++) {
    VecOverlay<kW, T> a;
    VecOverlay<kW, T> b;
    VecOverlay<kW, T> observed;
    VecOverlay<kW, T> expected;

    fill_random(dg, a);
    fill_random(dg, b);
    Constraint::apply(a);
    Constraint::apply(b);

    compiledApply(&observed, &a, &b);
    expected = GenericOp::apply(a, b);

    if (!compare_ivec(observed.items, expected.items, kItemCount))
      test_iop_vvv_failed(a.items, b.items, observed.items, expected.items, kItemCount, ctx.logger.data());
  }

  ctx.rt.release(compiledApply);
}

template<SimdWidth kSimdWidth, typename T, OpcodeVVV kOp, typename GenericOp>
static void test_iop_vvv(JitContext& ctx) noexcept {
  return test_iop_vvv_constraint<kSimdWidth, T, kOp, GenericOp, ConstraintNone>(ctx);
}

// bl::Pipeline::JIT - Tests - SIMD - Integer Operations - VVVI
// ============================================================

template<SimdWidth kSimdWidth, typename T, OpcodeVVVI kOp, typename GenericOp, typename Constraint>
static BL_NOINLINE void test_iop_vvvi_constraint(JitContext& ctx, uint32_t imm) noexcept {
  constexpr uint32_t kW = vecWidthFromSimdWidth(kSimdWidth);
  constexpr uint32_t kItemCount = kW / uint32_t(sizeof(T));

  TestVVVFunc compiledApply = create_func_vvvi(ctx, kSimdWidth, kOp, imm);
  DataGenInt dg(kRandomSeed);

  for (uint32_t iter = 0; iter < kTestIterCount; iter++) {
    VecOverlay<kW, T> a;
    VecOverlay<kW, T> b;
    VecOverlay<kW, T> observed;
    VecOverlay<kW, T> expected;

    fill_random(dg, a);
    fill_random(dg, b);
    Constraint::apply(a);
    Constraint::apply(b);

    compiledApply(&observed, &a, &b);
    expected = GenericOp::apply(a, b, imm);

    if (!compare_ivec(observed.items, expected.items, kItemCount))
      test_iop_vvvi_failed(a.items, b.items, observed.items, expected.items, kItemCount, imm, ctx.logger.data());
  }

  ctx.rt.release(compiledApply);
}

template<SimdWidth kSimdWidth, typename T, OpcodeVVVI kOp, typename GenericOp>
static void test_iop_vvvi(JitContext& ctx, uint32_t imm) noexcept {
  return test_iop_vvvi_constraint<kSimdWidth, T, kOp, GenericOp, ConstraintNone>(ctx, imm);
}

// bl::Pipeline::JIT - Tests - SIMD - Integer Operations - VVVV
// ============================================================

template<SimdWidth kSimdWidth, typename T, OpcodeVVVV kOp, typename GenericOp, typename Constraint>
static BL_NOINLINE void test_iop_vvvv_constraint(JitContext& ctx) noexcept {
  constexpr uint32_t kW = vecWidthFromSimdWidth(kSimdWidth);
  constexpr uint32_t kItemCount = kW / uint32_t(sizeof(T));

  TestVVVVFunc compiledApply = create_func_vvvv(ctx, kSimdWidth, kOp);
  DataGenInt dg(kRandomSeed);

  for (uint32_t iter = 0; iter < kTestIterCount; iter++) {
    VecOverlay<kW, T> a;
    VecOverlay<kW, T> b;
    VecOverlay<kW, T> c;
    VecOverlay<kW, T> observed;
    VecOverlay<kW, T> expected;

    fill_random(dg, a);
    fill_random(dg, b);
    fill_random(dg, c);
    Constraint::apply(a);
    Constraint::apply(b);
    Constraint::apply(c);

    compiledApply(&observed, &a, &b, &c);
    expected = GenericOp::apply(a, b, c);

    if (!compare_ivec(observed.items, expected.items, kItemCount))
      test_iop_vvvv_failed(a.items, b.items, c.items, observed.items, expected.items, kItemCount);
  }
}

template<SimdWidth kSimdWidth, typename T, OpcodeVVVV kOp, typename GenericOp>
static void test_iop_vvvv(JitContext& ctx) noexcept {
  return test_iop_vvvv_constraint<kSimdWidth, T, kOp, GenericOp, ConstraintNone>(ctx);
}

// bl::Pipeline::JIT - Tests - SIMD - Runner
// =========================================

template<SimdWidth kSimdWidth>
static BL_NOINLINE void test_simd_ops(JitContext& ctx) noexcept {
  INFO("  Testing mov");
  {
    test_iop_vv<kSimdWidth, uint8_t , OpcodeVV::kMov, iop_mov<uint8_t>>(ctx);
    test_iop_vv<kSimdWidth, uint64_t, OpcodeVV::kMovU64, iop_mov_u64>(ctx);
  }

  INFO("  Testing broadcast");
  {
    // Test all broadcasts - vector based, GP to vector, and memory to vector.
    for (uint32_t variation = 0; variation < 3; variation++) {
      test_iop_vv<kSimdWidth, uint8_t , OpcodeVV::kBroadcastU8Z, iop_broadcast_u8<uint8_t>>(ctx, variation);
      test_iop_vv<kSimdWidth, uint16_t, OpcodeVV::kBroadcastU16Z, iop_broadcast_u16<uint16_t>>(ctx, variation);
      test_iop_vv<kSimdWidth, uint8_t , OpcodeVV::kBroadcastU8, iop_broadcast_u8<uint8_t>>(ctx, variation);
      test_iop_vv<kSimdWidth, uint16_t, OpcodeVV::kBroadcastU16, iop_broadcast_u16<uint16_t>>(ctx, variation);
      test_iop_vv<kSimdWidth, uint32_t, OpcodeVV::kBroadcastU32, iop_broadcast_u32<uint32_t>>(ctx, variation);
      test_iop_vv<kSimdWidth, uint64_t, OpcodeVV::kBroadcastU64, iop_broadcast_u64<uint64_t>>(ctx, variation);
      test_iop_vv<kSimdWidth, uint32_t, OpcodeVV::kBroadcastF32, iop_broadcast_u32<uint32_t>>(ctx, variation);
      test_iop_vv<kSimdWidth, uint64_t, OpcodeVV::kBroadcastF64, iop_broadcast_u64<uint64_t>>(ctx, variation);
    }
  }

  INFO("  Testing abs");
  {
    test_iop_vv<kSimdWidth, int8_t  , OpcodeVV::kAbsI8, iop_abs<int8_t>>(ctx);
    test_iop_vv<kSimdWidth, int16_t , OpcodeVV::kAbsI16, iop_abs<int16_t>>(ctx);
    test_iop_vv<kSimdWidth, int32_t , OpcodeVV::kAbsI32, iop_abs<int32_t>>(ctx);
    test_iop_vv<kSimdWidth, int64_t , OpcodeVV::kAbsI64, iop_abs<int64_t>>(ctx);
  }

  INFO("  Testing not");
  {
    test_iop_vv<kSimdWidth, uint32_t, OpcodeVV::kNotU32  , iop_not<uint32_t>>(ctx);
    test_iop_vv<kSimdWidth, uint64_t, OpcodeVV::kNotU64  , iop_not<uint64_t>>(ctx);
    test_iop_vv<kSimdWidth, uint32_t, OpcodeVV::kNotF32  , iop_not<uint32_t>>(ctx);
    test_iop_vv<kSimdWidth, uint64_t, OpcodeVV::kNotF64  , iop_not<uint64_t>>(ctx);
  }

  INFO("  Testing bit shift");
  {
/*
    for (uint32_t i = 1; i < 8; i++) {
      test_iop_vvi<kSimdWidth, uint8_t , OpcodeVVI::kSllU8 , iop_slli<uint8_t>>(ctx, i);
      test_iop_vvi<kSimdWidth, uint8_t , OpcodeVVI::kSrlU8 , iop_srli<uint8_t>>(ctx, i);
      test_iop_vvi<kSimdWidth, int8_t  , OpcodeVVI::kSraI8 , iop_srai<int8_t>>(ctx, i);
    }
*/
    for (uint32_t i = 1; i < 16; i++) {
      test_iop_vvi<kSimdWidth, uint16_t, OpcodeVVI::kSllU16, iop_slli<uint16_t>>(ctx, i);
      test_iop_vvi<kSimdWidth, uint16_t, OpcodeVVI::kSrlU16, iop_srli<uint16_t>>(ctx, i);
      test_iop_vvi<kSimdWidth, int16_t , OpcodeVVI::kSraI16, iop_srai<int16_t>>(ctx, i);
    }

    for (uint32_t i = 1; i < 32; i++) {
      test_iop_vvi<kSimdWidth, uint32_t, OpcodeVVI::kSllU32, iop_slli<uint32_t>>(ctx, i);
      test_iop_vvi<kSimdWidth, uint32_t, OpcodeVVI::kSrlU32, iop_srli<uint32_t>>(ctx, i);
      test_iop_vvi<kSimdWidth, int32_t , OpcodeVVI::kSraI32, iop_srai<int32_t>>(ctx, i);
    }

    for (uint32_t i = 1; i < 64; i++) {
      test_iop_vvi<kSimdWidth, uint64_t, OpcodeVVI::kSllU64, iop_slli<uint64_t>>(ctx, i);
      test_iop_vvi<kSimdWidth, uint64_t, OpcodeVVI::kSrlU64, iop_srli<uint64_t>>(ctx, i);
      test_iop_vvi<kSimdWidth, int64_t , OpcodeVVI::kSraI64, iop_srai<int64_t>>(ctx, i);
    }
  }

  INFO("  Testing sllb_u128");
  {
    for (uint32_t i = 1; i < 16; i++) {
      test_iop_vvi<kSimdWidth, uint8_t , OpcodeVVI::kSllbU128, iop_sllb_u128<uint8_t>>(ctx, i);
    }
  }

  INFO("  Testing srlb_u128");
  {
    for (uint32_t i = 0; i < 16; i++) {
      test_iop_vvi<kSimdWidth, uint8_t , OpcodeVVI::kSrlbU128, iop_srlb_u128<uint8_t>>(ctx, i);
    }
  }

  INFO("  Testing swizzle_[lo|hi]_u16x4");
  {
    for (uint32_t i = 0; i < 256; i++) {
      uint32_t imm = swizzle((i >> 6) & 3, (i >> 4) & 3, (i >> 2) & 3, i & 3).value;

      test_iop_vvi<kSimdWidth, uint16_t , OpcodeVVI::kSwizzleLoU16x4, iop_swizzle_lo_u16x4<uint16_t>>(ctx, imm);
      test_iop_vvi<kSimdWidth, uint16_t , OpcodeVVI::kSwizzleHiU16x4, iop_swizzle_hi_u16x4<uint16_t>>(ctx, imm);
      test_iop_vvi<kSimdWidth, uint16_t , OpcodeVVI::kSwizzleU16x4, iop_swizzle_u16<uint16_t>>(ctx, imm);
    }
  }

  INFO("  Testing swizzle_u32x4");
  {
    for (uint32_t i = 0; i < 256; i++) {
      uint32_t imm = swizzle((i >> 6) & 3, (i >> 4) & 3, (i >> 2) & 3, i & 3).value;

      test_iop_vvi<kSimdWidth, uint32_t, OpcodeVVI::kSwizzleU32x4, iop_swizzle_u32x4<uint32_t>>(ctx, imm);
      test_iop_vvi<kSimdWidth, uint32_t, OpcodeVVI::kSwizzleF32x4, iop_swizzle_u32x4<uint32_t>>(ctx, imm);
    }
  }

  INFO("  Testing swizzle_u64x2");
  {
    for (uint32_t i = 0; i < 4; i++) {
      uint32_t imm = swizzle((i >> 1) & 1, i & 1).value;
      test_iop_vvi<kSimdWidth, uint64_t, OpcodeVVI::kSwizzleU64x2, iop_swizzle_u64x2<uint64_t>>(ctx, imm);
      test_iop_vvi<kSimdWidth, uint64_t, OpcodeVVI::kSwizzleF64x2, iop_swizzle_u64x2<uint64_t>>(ctx, imm);
    }
  }

  INFO("  Testing logical");
  {
    test_iop_vvv<kSimdWidth, uint32_t, OpcodeVVV::kAndU32, iop_and<uint32_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint64_t, OpcodeVVV::kAndU64, iop_and<uint64_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint32_t, OpcodeVVV::kOrU32, iop_or<uint32_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint64_t, OpcodeVVV::kOrU64, iop_or<uint64_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint32_t, OpcodeVVV::kXorU32, iop_xor<uint32_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint64_t, OpcodeVVV::kXorU64, iop_xor<uint64_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint32_t, OpcodeVVV::kAndnU32, iop_andn<uint32_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint64_t, OpcodeVVV::kAndnU64, iop_andn<uint64_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint32_t, OpcodeVVV::kBicU32, iop_bic<uint32_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint64_t, OpcodeVVV::kBicU64, iop_bic<uint64_t>>(ctx);
  }

  INFO("  Testing add / adds");
  {
    test_iop_vvv<kSimdWidth, uint8_t , OpcodeVVV::kAddU8, iop_add<uint8_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint16_t, OpcodeVVV::kAddU16, iop_add<uint16_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint32_t, OpcodeVVV::kAddU32, iop_add<uint32_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint64_t, OpcodeVVV::kAddU64, iop_add<uint64_t>>(ctx);

    test_iop_vvv<kSimdWidth, int8_t  , OpcodeVVV::kAddsI8, iop_adds<int8_t>>(ctx);
    test_iop_vvv<kSimdWidth, int16_t , OpcodeVVV::kAddsI16, iop_adds<int16_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint8_t , OpcodeVVV::kAddsU8, iop_adds<uint8_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint16_t, OpcodeVVV::kAddsU16, iop_adds<uint16_t>>(ctx);
  }

  INFO("  Testing sub / subs");
  {
    test_iop_vvv<kSimdWidth, uint8_t , OpcodeVVV::kSubU8, iop_sub<uint8_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint16_t, OpcodeVVV::kSubU16, iop_sub<uint16_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint32_t, OpcodeVVV::kSubU32, iop_sub<uint32_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint64_t, OpcodeVVV::kSubU64, iop_sub<uint64_t>>(ctx);

    test_iop_vvv<kSimdWidth, int8_t  , OpcodeVVV::kSubsI8, iop_subs<int8_t>>(ctx);
    test_iop_vvv<kSimdWidth, int16_t , OpcodeVVV::kSubsI16, iop_subs<int16_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint8_t , OpcodeVVV::kSubsU8, iop_subs<uint8_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint16_t, OpcodeVVV::kSubsU16, iop_subs<uint16_t>>(ctx);
  }

  INFO("  Testing mul");
  {
    test_iop_vvv<kSimdWidth, uint16_t, OpcodeVVV::kMulU16, iop_mul<uint16_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint32_t, OpcodeVVV::kMulU32, iop_mul<uint32_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint64_t, OpcodeVVV::kMulU64, iop_mul<uint64_t>>(ctx);

    test_iop_vvv<kSimdWidth, int16_t , OpcodeVVV::kMulhI16, iop_mulhi<int16_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint16_t, OpcodeVVV::kMulhU16, iop_mulhu<uint16_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint64_t, OpcodeVVV::kMulU64_LoU32, iop_mul_u64_lo_u32>(ctx);
  }

  INFO("  Testing mhadd");
  {
    test_iop_vvv<kSimdWidth, uint32_t, OpcodeVVV::kMHAddI16_I32, iop_mhadd_i16_i32>(ctx);
  }

  INFO("  Testing min / max");
  {
    test_iop_vvv<kSimdWidth, int8_t  , OpcodeVVV::kMinI8, iop_min<int8_t>>(ctx);
    test_iop_vvv<kSimdWidth, int16_t , OpcodeVVV::kMinI16, iop_min<int16_t>>(ctx);
    test_iop_vvv<kSimdWidth, int32_t , OpcodeVVV::kMinI32, iop_min<int32_t>>(ctx);
    test_iop_vvv<kSimdWidth, int64_t , OpcodeVVV::kMinI64, iop_min<int64_t>>(ctx);

    test_iop_vvv<kSimdWidth, uint8_t , OpcodeVVV::kMinU8, iop_min<uint8_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint16_t, OpcodeVVV::kMinU16, iop_min<uint16_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint32_t, OpcodeVVV::kMinU32, iop_min<uint32_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint64_t, OpcodeVVV::kMinU64, iop_min<uint64_t>>(ctx);

    test_iop_vvv<kSimdWidth, int8_t  , OpcodeVVV::kMaxI8, iop_max<int8_t>>(ctx);
    test_iop_vvv<kSimdWidth, int16_t , OpcodeVVV::kMaxI16, iop_max<int16_t>>(ctx);
    test_iop_vvv<kSimdWidth, int32_t , OpcodeVVV::kMaxI32, iop_max<int32_t>>(ctx);
    test_iop_vvv<kSimdWidth, int64_t , OpcodeVVV::kMaxI64, iop_max<int64_t>>(ctx);

    test_iop_vvv<kSimdWidth, uint8_t , OpcodeVVV::kMaxU8, iop_max<uint8_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint16_t, OpcodeVVV::kMaxU16, iop_max<uint16_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint32_t, OpcodeVVV::kMaxU32, iop_max<uint32_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint64_t, OpcodeVVV::kMaxU64, iop_max<uint64_t>>(ctx);
  }

  INFO("  Testing cmp");
  {
    test_iop_vvv<kSimdWidth, uint8_t , OpcodeVVV::kCmpEqU8, iop_cmp_eq<uint8_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint16_t, OpcodeVVV::kCmpEqU16, iop_cmp_eq<uint16_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint32_t, OpcodeVVV::kCmpEqU32, iop_cmp_eq<uint32_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint64_t, OpcodeVVV::kCmpEqU64, iop_cmp_eq<uint64_t>>(ctx);
/*
    test_iop_vvv<kSimdWidth, uint8_t , OpcodeVVV::kCmpNeU8, iop_cmp_ne<uint8_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint16_t, OpcodeVVV::kCmpNeU16, iop_cmp_ne<uint16_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint32_t, OpcodeVVV::kCmpNeU32, iop_cmp_ne<uint32_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint64_t, OpcodeVVV::kCmpNeU64, iop_cmp_ne<uint64_t>>(ctx);
*/
    test_iop_vvv<kSimdWidth, int8_t  , OpcodeVVV::kCmpGtI8, iop_cmp_gt<int8_t>>(ctx);
    test_iop_vvv<kSimdWidth, int16_t , OpcodeVVV::kCmpGtI16, iop_cmp_gt<int16_t>>(ctx);
    test_iop_vvv<kSimdWidth, int32_t , OpcodeVVV::kCmpGtI32, iop_cmp_gt<int32_t>>(ctx);
    test_iop_vvv<kSimdWidth, int64_t , OpcodeVVV::kCmpGtI64, iop_cmp_gt<int64_t>>(ctx);

    test_iop_vvv<kSimdWidth, uint8_t , OpcodeVVV::kCmpGtU8, iop_cmp_gt<uint8_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint16_t, OpcodeVVV::kCmpGtU16, iop_cmp_gt<uint16_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint32_t, OpcodeVVV::kCmpGtU32, iop_cmp_gt<uint32_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint64_t, OpcodeVVV::kCmpGtU64, iop_cmp_gt<uint64_t>>(ctx);

    test_iop_vvv<kSimdWidth, int8_t  , OpcodeVVV::kCmpGeI8, iop_cmp_ge<int8_t>>(ctx);
    test_iop_vvv<kSimdWidth, int16_t , OpcodeVVV::kCmpGeI16, iop_cmp_ge<int16_t>>(ctx);
    test_iop_vvv<kSimdWidth, int32_t , OpcodeVVV::kCmpGeI32, iop_cmp_ge<int32_t>>(ctx);
    test_iop_vvv<kSimdWidth, int64_t , OpcodeVVV::kCmpGeI64, iop_cmp_ge<int64_t>>(ctx);

    test_iop_vvv<kSimdWidth, uint8_t , OpcodeVVV::kCmpGeU8, iop_cmp_ge<uint8_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint16_t, OpcodeVVV::kCmpGeU16, iop_cmp_ge<uint16_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint32_t, OpcodeVVV::kCmpGeU32, iop_cmp_ge<uint32_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint64_t, OpcodeVVV::kCmpGeU64, iop_cmp_ge<uint64_t>>(ctx);

    test_iop_vvv<kSimdWidth, int8_t  , OpcodeVVV::kCmpLtI8, iop_cmp_lt<int8_t>>(ctx);
    test_iop_vvv<kSimdWidth, int16_t , OpcodeVVV::kCmpLtI16, iop_cmp_lt<int16_t>>(ctx);
    test_iop_vvv<kSimdWidth, int32_t , OpcodeVVV::kCmpLtI32, iop_cmp_lt<int32_t>>(ctx);
    test_iop_vvv<kSimdWidth, int64_t , OpcodeVVV::kCmpLtI64, iop_cmp_lt<int64_t>>(ctx);

    test_iop_vvv<kSimdWidth, uint8_t , OpcodeVVV::kCmpLtU8, iop_cmp_lt<uint8_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint16_t, OpcodeVVV::kCmpLtU16, iop_cmp_lt<uint16_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint32_t, OpcodeVVV::kCmpLtU32, iop_cmp_lt<uint32_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint64_t, OpcodeVVV::kCmpLtU64, iop_cmp_lt<uint64_t>>(ctx);

    test_iop_vvv<kSimdWidth, int8_t  , OpcodeVVV::kCmpLeI8, iop_cmp_le<int8_t>>(ctx);
    test_iop_vvv<kSimdWidth, int16_t , OpcodeVVV::kCmpLeI16, iop_cmp_le<int16_t>>(ctx);
    test_iop_vvv<kSimdWidth, int32_t , OpcodeVVV::kCmpLeI32, iop_cmp_le<int32_t>>(ctx);
    test_iop_vvv<kSimdWidth, int64_t , OpcodeVVV::kCmpLeI64, iop_cmp_le<int64_t>>(ctx);

    test_iop_vvv<kSimdWidth, uint8_t , OpcodeVVV::kCmpLeU8, iop_cmp_le<uint8_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint16_t, OpcodeVVV::kCmpLeU16, iop_cmp_le<uint16_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint32_t, OpcodeVVV::kCmpLeU32, iop_cmp_le<uint32_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint64_t, OpcodeVVV::kCmpLeU64, iop_cmp_le<uint64_t>>(ctx);
  }

  INFO("  Testing combine");
  {
    test_iop_vvv<kSimdWidth, uint64_t, OpcodeVVV::kCombineLoHiU64, iop_combine_lo_hi_u64>(ctx);
    test_iop_vvv<kSimdWidth, uint64_t, OpcodeVVV::kCombineLoHiF64, iop_combine_lo_hi_u64>(ctx);
    test_iop_vvv<kSimdWidth, uint64_t, OpcodeVVV::kCombineHiLoU64, iop_combine_hi_lo_u64>(ctx);
    test_iop_vvv<kSimdWidth, uint64_t, OpcodeVVV::kCombineHiLoF64, iop_combine_hi_lo_u64>(ctx);
  }

  INFO("  Testing interleave");
  {
    test_iop_vvv<kSimdWidth, uint8_t , OpcodeVVV::kInterleaveLoU8, iop_interleave_lo_u8<uint8_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint8_t , OpcodeVVV::kInterleaveHiU8, iop_interleave_hi_u8<uint8_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint16_t, OpcodeVVV::kInterleaveLoU16, iop_interleave_lo_u16<uint16_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint16_t, OpcodeVVV::kInterleaveHiU16, iop_interleave_hi_u16<uint16_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint32_t, OpcodeVVV::kInterleaveLoU32, iop_interleave_lo_u32<uint32_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint32_t, OpcodeVVV::kInterleaveHiU32, iop_interleave_hi_u32<uint32_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint64_t, OpcodeVVV::kInterleaveLoU64, iop_interleave_lo_u64<uint64_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint64_t, OpcodeVVV::kInterleaveHiU64, iop_interleave_hi_u64<uint64_t>>(ctx);
  }

  INFO("  Testing packs");
  {
    test_iop_vvv<kSimdWidth, int8_t  , OpcodeVVV::kPacksI16_I8, iop_packs_i16_i8<int8_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint8_t , OpcodeVVV::kPacksI16_U8, iop_packs_i16_u8<uint8_t>>(ctx);
    test_iop_vvv<kSimdWidth, int16_t , OpcodeVVV::kPacksI32_I16, iop_packs_i32_i16<int16_t>>(ctx);
    test_iop_vvv<kSimdWidth, uint16_t, OpcodeVVV::kPacksI32_U16, iop_packs_i32_u16<uint16_t>>(ctx);
  }

  INFO("  Testing alignr_u128");
  {
    for (uint32_t i = 1; i < 16; i++)
      test_iop_vvvi<kSimdWidth, uint8_t , OpcodeVVVI::kAlignr_U128, iop_alignr_u128<uint8_t>>(ctx, i);
  }

  INFO("  Testing interleave_shuffle");
  {
    for (uint32_t i = 0; i < 256; i++) {
      uint32_t imm = swizzle((i >> 6) & 3, (i >> 4) & 3, (i >> 2) & 3, i & 3).value;

      test_iop_vvvi<kSimdWidth, uint32_t, OpcodeVVVI::kInterleaveShuffleU32x4, iop_interleave_shuffle_u32x4<uint32_t>>(ctx, imm);
      test_iop_vvvi<kSimdWidth, uint32_t, OpcodeVVVI::kInterleaveShuffleF32x4, iop_interleave_shuffle_u32x4<uint32_t>>(ctx, imm);
    }

    for (uint32_t i = 0; i < 4; i++) {
      uint32_t imm = swizzle((i >> 1) & 1, i & 1).value;

      test_iop_vvvi<kSimdWidth, uint64_t, OpcodeVVVI::kInterleaveShuffleU64x2, iop_interleave_shuffle_u64x2<uint64_t>>(ctx, imm);
      test_iop_vvvi<kSimdWidth, uint64_t, OpcodeVVVI::kInterleaveShuffleF64x2, iop_interleave_shuffle_u64x2<uint64_t>>(ctx, imm);
    }
  }
}

static void test_gp_ops(JitContext& ctx) noexcept {
  test_cond_ops(ctx);
  test_m_ops(ctx);
  test_rm_ops(ctx);
  test_rr_ops(ctx);
  test_rrr_ops(ctx);
}

#if defined(BL_JIT_ARCH_X86)
static void dumpFeatureList(asmjit::String& out, const asmjit::CpuFeatures& features) noexcept {
  asmjit::CpuFeatures::Iterator it = features.iterator();

  bool first = true;
  while (it.hasNext()) {
    size_t featureId = it.next();
    if (!first)
      out.append(' ');
    asmjit::Formatter::formatFeature(out, asmjit::Arch::kHost, uint32_t(featureId));
    first = false;
  }
}

static void test_x86_ops(JitContext& ctx, const asmjit::CpuFeatures& hostFeatures) noexcept {
  using Ext = asmjit::CpuFeatures::X86;
  using CpuFeatures = asmjit::CpuFeatures;

  asmjit::String sss;
  dumpFeatureList(sss, hostFeatures);
  INFO("Available features: %s", sss.data());

  // Features that must always be available;
  CpuFeatures base;
  base.add(Ext::kI486, Ext::kCMOV, Ext::kCMPXCHG8B, Ext::kFPU, Ext::kSSE, Ext::kSSE2);

  // To verify that JIT implements ALL features with ALL possible CPU flags, we use profiles to select features
  // that the JIT compiler will be allowed to use. The features are gradually increased similarly to how new CPU
  // generations introduced them. We cannot cover ALL possible CPUs, but that's not even necessary as we test
  // individual operations where instructions can be selected on the features available.

  // GP variations.
  {
    CpuFeatures profiles[4] {};
    profiles[0] = base;

    profiles[1] = profiles[0];
    profiles[1].add(Ext::kADX, Ext::kBMI);

    profiles[2] = profiles[1];
    profiles[2].add(Ext::kBMI2, Ext::kLZCNT, Ext::kMOVBE, Ext::kPOPCNT);

    profiles[3] = hostFeatures;

    bool first = true;
    CpuFeatures lastFiltered;

    for (const CpuFeatures& profile : profiles) {
      CpuFeatures filtered = profile;

      for (uint32_t i = 0; i < CpuFeatures::kNumBitWords; i++)
        filtered.data()._bits[i] &= hostFeatures.data()._bits[i];

      if (!first && filtered == lastFiltered)
        continue;

      asmjit::String s;
      if (filtered == hostFeatures)
        s.assign("[ALL]");
      else
        dumpFeatureList(s, filtered);

      ctx.features = filtered;

      INFO("Testing GP ops with: %s", s.data());
      test_gp_ops(ctx);

      first = false;
      lastFiltered = filtered;
    }
  }

  // SIMD variations covering SSE2+, AVX+, and AVX512+ cases.
  {
    CpuFeatures profiles[15] {};
    profiles[0] = base;

    profiles[1] = profiles[0];
    profiles[1].add(Ext::kSSE3);

    profiles[2] = profiles[1];
    profiles[2].add(Ext::kSSSE3);

    profiles[3] = profiles[2];
    profiles[3].add(Ext::kSSE4_1, Ext::kADX, Ext::kBMI, Ext::kBMI2, Ext::kLZCNT, Ext::kMOVBE, Ext::kPOPCNT);

    profiles[4] = profiles[3];
    profiles[4].add(Ext::kSSE4_2);

    profiles[5] = profiles[4];
    profiles[5].add(Ext::kPCLMULQDQ);

    profiles[6] = profiles[5];
    profiles[6].add(Ext::kAVX);

    profiles[7] = profiles[6];
    profiles[7].add(Ext::kAVX2);

    profiles[8] = profiles[7];
    profiles[8].add(Ext::kF16C, Ext::kFMA, Ext::kVAES, Ext::kVPCLMULQDQ);

    profiles[9] = profiles[8];
    profiles[9].add(Ext::kAVX_IFMA, Ext::kAVX_NE_CONVERT, Ext::kAVX_VNNI, Ext::kAVX_VNNI_INT8, Ext::kAVX_VNNI_INT16);

    // We start deliberately from a profile that doesn't contains AVX_xxx
    // extensions as these didn't exist when the first AVX512 CPUs were shipped.
    profiles[10] = profiles[7];
    profiles[10].add(Ext::kAVX512_F, Ext::kAVX512_BW, Ext::kAVX512_DQ, Ext::kAVX512_CD, Ext::kAVX512_VL);

    profiles[11] = profiles[11];
    profiles[11].add(Ext::kAVX512_IFMA, Ext::kAVX512_VBMI);

    profiles[12] = profiles[11];
    profiles[12].add(Ext::kAVX512_BITALG, Ext::kAVX512_VBMI2, Ext::kAVX512_VNNI, Ext::kAVX512_VPOPCNTDQ);

    profiles[13] = profiles[12];
    profiles[13].add(Ext::kAVX512_BF16, Ext::kAVX512_FP16);

    profiles[14] = hostFeatures;

    bool first = true;
    CpuFeatures lastFiltered;

    for (const CpuFeatures& profile : profiles) {
      CpuFeatures filtered = profile;

      for (uint32_t i = 0; i < CpuFeatures::kNumBitWords; i++)
        filtered.data()._bits[i] &= hostFeatures.data()._bits[i];

      if (!first && filtered == lastFiltered)
        continue;

      asmjit::String s;
      if (filtered == hostFeatures)
        s.assign("[ALL]");
      else
        dumpFeatureList(s, filtered);

      ctx.features = filtered;

      INFO("Testing 128-bit SIMD ops with: %s", s.data());
      test_simd_ops<SimdWidth::k128>(ctx);

      if (filtered.x86().hasAVX2()) {
        INFO("Testing 256-bit SIMD ops with: %s", s.data());
        test_simd_ops<SimdWidth::k256>(ctx);
      }

      if (filtered.x86().hasAVX512_F()) {
        INFO("Testing 512-bit SIMD ops with: %s", s.data());
        test_simd_ops<SimdWidth::k512>(ctx);
      }

      first = false;
      lastFiltered = filtered;
    }
  }
}
#endif // BL_JIT_ARCH_X86

#if defined(BL_JIT_ARCH_A64)
static void test_a64_ops(JitContext& ctx, const asmjit::CpuFeatures& hostFeatures) noexcept {
  ctx.features = hostFeatures;

  test_gp_ops(ctx);
  test_simd_ops<SimdWidth::k128>(ctx);
}
#endif // BL_JIT_ARCH_A64

UNIT(pipecompiler, /*BL_TEST_GROUP_PIPELINE_JIT_COMPILER*/ -999) {
  JitContext ctx;
  asmjit::CpuFeatures hostFeatures = asmjit::CpuInfo::host().features();

#if defined(BL_JIT_ARCH_X86)
  test_x86_ops(ctx, hostFeatures);
#elif defined(BL_JIT_ARCH_A64)
  test_a64_ops(ctx, hostFeatures);
#endif
}

} // {Tests}
} // {JIT}
} // {Pipeline}
} // {bl}

#endif // BL_TEST && !BL_BUILD_NO_JIT
