const std = @import("std");
const builtin = @import("builtin");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const nojit = b.option(bool, "nojit", "Disable JIT compilation") orelse false;
    const nofutex = b.option(bool, "nofutex", "Disable Futex") orelse false;
    const notls = b.option(bool, "notls", "Disable Thread-Local Storage") orelse false;
    const nostdcxx = b.option(bool, "nostdcxx", "Disable C++ Standard Library") orelse true;

    const lib = b.addSharedLibrary(.{
        .name = "blend2d",
        .target = target,
        .optimize = if (optimize != .Debug) .ReleaseSmall else .Debug,
    });

    if (optimize == .Debug) {
        lib.defineCMacro("BL_BUILD_DEBUG", null);
    } else {
        lib.defineCMacro("NDEBUG", null);
        lib.defineCMacro("BL_BUILD_RELEASE", null);
    }

    if (nojit) {
        lib.defineCMacro("BL_BUILD_NO_JIT", null);
    }
    if (nostdcxx) {
        lib.defineCMacro("BL_BUILD_NO_STDCXX", null);
    }
    if (nofutex) {
        lib.defineCMacro("BL_BUILD_NO_FUTEX", null);
    }
    if (notls) {
        lib.defineCMacro("BL_BUILD_NO_TLS", null);
    }

    if (std.Target.x86.featureSetHas(builtin.cpu.features, .avx512f) and std.Target.x86.featureSetHas(builtin.cpu.features, .avx512bw) and std.Target.x86.featureSetHas(builtin.cpu.features, .avx512dq) and std.Target.x86.featureSetHas(builtin.cpu.features, .avx512cd) and std.Target.x86.featureSetHas(builtin.cpu.features, .avx512vl)) {
        lib.defineCMacro("BL_BUILD_OPT_AVX512", null);
    }

    if (std.Target.x86.featureSetHas(builtin.cpu.features, .avx2)) {
        lib.defineCMacro("BL_BUILD_OPT_AVX2", null);
    }

    if (std.Target.x86.featureSetHas(builtin.cpu.features, .avx)) {
        lib.defineCMacro("BL_BUILD_OPT_AVX", null);
    }

    if (std.Target.x86.featureSetHas(builtin.cpu.features, .sse4_2)) {
        lib.defineCMacro("BL_BUILD_OPT_SSE4_2", null);
    }

    if (std.Target.x86.featureSetHas(builtin.cpu.features, .sse4_1)) {
        lib.defineCMacro("BL_BUILD_OPT_SSE4_1", null);
    }

    if (std.Target.x86.featureSetHas(builtin.cpu.features, .ssse3)) {
        lib.defineCMacro("BL_BUILD_OPT_SSSE3", null);
    }

    if (std.Target.x86.featureSetHas(builtin.cpu.features, .sse3)) {
        lib.defineCMacro("BL_BUILD_OPT_SSE3", null);
    }

    if (std.Target.x86.featureSetHas(builtin.cpu.features, .sse2)) {
        lib.defineCMacro("BL_BUILD_OPT_SSE2", null);
    }

    if (std.Target.aarch64.featureSetHas(builtin.cpu.features, .neon)) {
        lib.defineCMacro("BL_BUILD_OPT_ASIMD", null);
    }

    lib.linkLibC();
    lib.linkLibCpp();
    lib.addIncludePath(b.path("./src"));
    lib.addIncludePath(b.path("./asmjit/src"));
    lib.installHeadersDirectory(b.path("./src"), "blend2d", .{});
    lib.installHeadersDirectory(b.path("./asmjit/src"), "asmjit", .{});
    lib.addCSourceFiles(.{
        .files = &src_files,
        .flags = &.{
            "-fvisibility=hidden",
            "-fno-exceptions",
            "-fno-rtti",
            "-fno-math-errno",
            "-fno-semantic-interposition",
            "-fno-threadsafe-statics",
            "-fmerge-all-constants",
            "-ftree-vectorize",
        },
    });

    b.installArtifact(lib);
}

const src_files = [_][]const u8{
    "src/blend2d/api-globals.cpp",
    "src/blend2d/api-nocxx.cpp",
    "src/blend2d/array.cpp",
    "src/blend2d/array_test.cpp",
    "src/blend2d/bitarray.cpp",
    "src/blend2d/bitarray_test.cpp",
    "src/blend2d/bitset.cpp",
    "src/blend2d/bitset_test.cpp",
    "src/blend2d/compopinfo.cpp",
    "src/blend2d/context.cpp",
    "src/blend2d/context_test.cpp",
    "src/blend2d/filesystem.cpp",
    "src/blend2d/font.cpp",
    "src/blend2d/font_test.cpp",
    "src/blend2d/fontdata.cpp",
    "src/blend2d/fontface.cpp",
    "src/blend2d/fontfeaturesettings.cpp",
    "src/blend2d/fontfeaturesettings_test.cpp",
    "src/blend2d/fontmanager.cpp",
    "src/blend2d/fonttagdataids.cpp",
    "src/blend2d/fonttagdataids_test.cpp",
    "src/blend2d/fonttagdatainfo.cpp",
    "src/blend2d/fonttagdatainfo_test.cpp",
    "src/blend2d/fonttagset.cpp",
    "src/blend2d/fontvariationsettings.cpp",
    "src/blend2d/fontvariationsettings_test.cpp",
    "src/blend2d/format.cpp",
    "src/blend2d/geometry.cpp",
    "src/blend2d/glyphbuffer.cpp",
    "src/blend2d/gradient.cpp",
    "src/blend2d/gradient_test.cpp",
    "src/blend2d/image.cpp",
    "src/blend2d/image_test.cpp",
    "src/blend2d/imagecodec.cpp",
    "src/blend2d/imagecodec_test.cpp",
    "src/blend2d/imagedecoder.cpp",
    "src/blend2d/imageencoder.cpp",
    "src/blend2d/imagescale.cpp",
    "src/blend2d/matrix.cpp",
    "src/blend2d/matrix_avx.cpp",
    "src/blend2d/matrix_sse2.cpp",
    "src/blend2d/matrix_test.cpp",
    "src/blend2d/object.cpp",
    "src/blend2d/path.cpp",
    "src/blend2d/path_test.cpp",
    "src/blend2d/pathstroke.cpp",
    "src/blend2d/pattern.cpp",
    "src/blend2d/pixelconverter.cpp",
    "src/blend2d/pixelconverter_avx2.cpp",
    "src/blend2d/pixelconverter_sse2.cpp",
    "src/blend2d/pixelconverter_ssse3.cpp",
    "src/blend2d/pixelconverter_test.cpp",
    "src/blend2d/random.cpp",
    "src/blend2d/random_test.cpp",
    "src/blend2d/rgba_test.cpp",
    "src/blend2d/runtime.cpp",
    "src/blend2d/string.cpp",
    "src/blend2d/string_test.cpp",
    "src/blend2d/trace.cpp",
    "src/blend2d/var.cpp",
    "src/blend2d/var_test.cpp",

    "src/blend2d/codec/bmpcodec.cpp",
    "src/blend2d/codec/jpegcodec.cpp",
    "src/blend2d/codec/jpeghuffman.cpp",
    "src/blend2d/codec/jpegops.cpp",
    "src/blend2d/codec/jpegops_sse2.cpp",
    "src/blend2d/codec/pngcodec.cpp",
    "src/blend2d/codec/pngops.cpp",
    "src/blend2d/codec/pngops_sse2.cpp",

    "src/blend2d/compression/checksum.cpp",
    "src/blend2d/compression/checksum_test.cpp",
    "src/blend2d/compression/deflatedecoder.cpp",
    "src/blend2d/compression/deflateencoder.cpp",

    "src/blend2d/opentype/otcff.cpp",
    "src/blend2d/opentype/otcff_test.cpp",
    "src/blend2d/opentype/otcmap.cpp",
    "src/blend2d/opentype/otcore.cpp",
    "src/blend2d/opentype/otface.cpp",
    "src/blend2d/opentype/otglyf.cpp",
    "src/blend2d/opentype/otglyf_asimd.cpp",
    "src/blend2d/opentype/otglyf_avx2.cpp",
    "src/blend2d/opentype/otglyf_sse4_2.cpp",
    "src/blend2d/opentype/otglyfsimddata.cpp",
    "src/blend2d/opentype/otkern.cpp",
    "src/blend2d/opentype/otlayout.cpp",
    "src/blend2d/opentype/otmetrics.cpp",
    "src/blend2d/opentype/otname.cpp",

    "src/blend2d/pipeline/pipedefs.cpp",
    "src/blend2d/pipeline/piperuntime.cpp",

    "src/blend2d/pipeline/jit/compoppart.cpp",
    "src/blend2d/pipeline/jit/fetchgradientpart.cpp",
    "src/blend2d/pipeline/jit/fetchpart.cpp",
    "src/blend2d/pipeline/jit/fetchpatternpart.cpp",
    "src/blend2d/pipeline/jit/fetchpixelptrpart.cpp",
    "src/blend2d/pipeline/jit/fetchsolidpart.cpp",
    "src/blend2d/pipeline/jit/fetchutils.cpp",
    "src/blend2d/pipeline/jit/fillpart.cpp",
    "src/blend2d/pipeline/jit/pipecompiler_x86.cpp",
    "src/blend2d/pipeline/jit/pipegencore.cpp",
    "src/blend2d/pipeline/jit/pipegenruntime.cpp",
    "src/blend2d/pipeline/jit/pipepart.cpp",

    "src/blend2d/pipeline/reference/fixedpiperuntime.cpp",

    "src/blend2d/pixelops/interpolation.cpp",
    "src/blend2d/pixelops/interpolation_avx2.cpp",
    "src/blend2d/pixelops/interpolation_sse2.cpp",
    "src/blend2d/pixelops/funcs.cpp",
    "src/blend2d/pixelops/scalar_test.cpp",

    "src/blend2d/raster/analyticrasterizer_test.cpp",
    "src/blend2d/raster/rastercontext.cpp",
    "src/blend2d/raster/rastercontextops.cpp",
    "src/blend2d/raster/renderfetchdata.cpp",
    "src/blend2d/raster/rendertargetinfo.cpp",
    "src/blend2d/raster/workdata.cpp",
    "src/blend2d/raster/workermanager.cpp",
    "src/blend2d/raster/workerproc.cpp",
    "src/blend2d/raster/workersynchronization.cpp",

    "src/blend2d/simd/simd_test.cpp",
    "src/blend2d/simd/simdarm_test_asimd.cpp",
    "src/blend2d/simd/simdx86_test_avx.cpp",
    "src/blend2d/simd/simdx86_test_avx2.cpp",
    "src/blend2d/simd/simdx86_test_avx512.cpp",
    "src/blend2d/simd/simdx86_test_sse2.cpp",
    "src/blend2d/simd/simdx86_test_sse4_1.cpp",
    "src/blend2d/simd/simdx86_test_sse4_2.cpp",
    "src/blend2d/simd/simdx86_test_ssse3.cpp",

    "src/blend2d/support/algorithm_test.cpp",
    "src/blend2d/support/arenaallocator.cpp",
    "src/blend2d/support/arenabitarray_test.cpp",
    "src/blend2d/support/arenahashmap.cpp",
    "src/blend2d/support/arenahashmap_test.cpp",
    "src/blend2d/support/arenalist_test.cpp",
    "src/blend2d/support/arenatree_test.cpp",
    "src/blend2d/support/bitops_test.cpp",
    "src/blend2d/support/intops_test.cpp",
    "src/blend2d/support/math.cpp",
    "src/blend2d/support/math_test.cpp",
    "src/blend2d/support/memops_test.cpp",
    "src/blend2d/support/ptrops_test.cpp",
    "src/blend2d/support/scopedallocator.cpp",
    "src/blend2d/support/zeroallocator.cpp",
    "src/blend2d/support/zeroallocator_test.cpp",

    "src/blend2d/tables/tables.cpp",
    "src/blend2d/tables/tables_test.cpp",

    "src/blend2d/threading/futex.cpp",
    "src/blend2d/threading/thread.cpp",
    "src/blend2d/threading/threadpool.cpp",
    "src/blend2d/threading/threadpool_test.cpp",
    "src/blend2d/threading/uniqueidgenerator.cpp",

    "src/blend2d/unicode/unicode.cpp",
    "src/blend2d/unicode/unicode_test.cpp",

    "asmjit/src/asmjit/core/archtraits.cpp",
    "asmjit/src/asmjit/core/assembler.cpp",
    "asmjit/src/asmjit/core/builder.cpp",
    "asmjit/src/asmjit/core/codeholder.cpp",
    "asmjit/src/asmjit/core/codewriter.cpp",
    "asmjit/src/asmjit/core/compiler.cpp",
    "asmjit/src/asmjit/core/constpool.cpp",
    "asmjit/src/asmjit/core/cpuinfo.cpp",
    "asmjit/src/asmjit/core/emithelper.cpp",
    "asmjit/src/asmjit/core/emitter.cpp",
    "asmjit/src/asmjit/core/emitterutils.cpp",
    "asmjit/src/asmjit/core/environment.cpp",
    "asmjit/src/asmjit/core/errorhandler.cpp",
    "asmjit/src/asmjit/core/formatter.cpp",
    "asmjit/src/asmjit/core/func.cpp",
    "asmjit/src/asmjit/core/funcargscontext.cpp",
    "asmjit/src/asmjit/core/globals.cpp",
    "asmjit/src/asmjit/core/inst.cpp",
    "asmjit/src/asmjit/core/instdb.cpp",
    "asmjit/src/asmjit/core/jitallocator.cpp",
    "asmjit/src/asmjit/core/jitruntime.cpp",
    "asmjit/src/asmjit/core/logger.cpp",
    "asmjit/src/asmjit/core/operand.cpp",
    "asmjit/src/asmjit/core/osutils.cpp",
    "asmjit/src/asmjit/core/ralocal.cpp",
    "asmjit/src/asmjit/core/rapass.cpp",
    "asmjit/src/asmjit/core/rastack.cpp",
    "asmjit/src/asmjit/core/string.cpp",
    "asmjit/src/asmjit/core/support.cpp",
    "asmjit/src/asmjit/core/target.cpp",
    "asmjit/src/asmjit/core/type.cpp",
    "asmjit/src/asmjit/core/virtmem.cpp",
    "asmjit/src/asmjit/core/zone.cpp",
    "asmjit/src/asmjit/core/zonehash.cpp",
    "asmjit/src/asmjit/core/zonelist.cpp",
    "asmjit/src/asmjit/core/zonestack.cpp",
    "asmjit/src/asmjit/core/zonetree.cpp",
    "asmjit/src/asmjit/core/zonevector.cpp",

    "asmjit/src/asmjit/arm/armformatter.cpp",
    "asmjit/src/asmjit/arm/a64assembler.cpp",
    "asmjit/src/asmjit/arm/a64builder.cpp",
    "asmjit/src/asmjit/arm/a64compiler.cpp",
    "asmjit/src/asmjit/arm/a64emithelper.cpp",
    "asmjit/src/asmjit/arm/a64formatter.cpp",
    "asmjit/src/asmjit/arm/a64func.cpp",
    "asmjit/src/asmjit/arm/a64instapi.cpp",
    "asmjit/src/asmjit/arm/a64instdb.cpp",
    "asmjit/src/asmjit/arm/a64operand.cpp",
    "asmjit/src/asmjit/arm/a64rapass.cpp",

    "asmjit/src/asmjit/x86/x86assembler.cpp",
    "asmjit/src/asmjit/x86/x86builder.cpp",
    "asmjit/src/asmjit/x86/x86compiler.cpp",
    "asmjit/src/asmjit/x86/x86emithelper.cpp",
    "asmjit/src/asmjit/x86/x86formatter.cpp",
    "asmjit/src/asmjit/x86/x86func.cpp",
    "asmjit/src/asmjit/x86/x86instdb.cpp",
    "asmjit/src/asmjit/x86/x86instapi.cpp",
    "asmjit/src/asmjit/x86/x86operand.cpp",
    "asmjit/src/asmjit/x86/x86rapass.cpp",
};
