#ifndef SLANG_H
#define SLANG_H
#ifndef SLANG_COMPILER
#define SLANG_COMPILER
#if defined(_MSC_VER)
#if _MSC_VER >= 1900
#define SLANG_VC 14
#elif _MSC_VER >= 1800
#define SLANG_VC 12
#elif _MSC_VER >= 1700
#define SLANG_VC 11
#elif _MSC_VER >= 1600
#define SLANG_VC 10
#elif _MSC_VER >= 1500
#define SLANG_VC 9
#else
#error "unknown version of Visual C++ compiler"
#endif
#elif defined(__clang__)
#define SLANG_CLANG 1
#elif defined(__SNC__)
#define SLANG_SNC 1
#elif defined(__ghs__)
#define SLANG_GHS 1
#elif defined(__GNUC__)
#define SLANG_GCC 1
#else
#error "unknown compiler"
#endif
#ifndef SLANG_VC
#define SLANG_VC 0
#endif
#ifndef SLANG_CLANG
#define SLANG_CLANG 0
#endif
#ifndef SLANG_SNC
#define SLANG_SNC 0
#endif
#ifndef SLANG_GHS
#define SLANG_GHS 0
#endif
#ifndef SLANG_GCC
#define SLANG_GCC 0
#endif
#endif
#ifndef SLANG_PLATFORM
#define SLANG_PLATFORM
#if defined(WINAPI_FAMILY) && WINAPI_FAMILY == WINAPI_PARTITION_APP
#define SLANG_WINRT 1
#elif defined(XBOXONE)
#define SLANG_XBOXONE 1
#elif defined(_WIN64)
#define SLANG_WIN64 1
#elif defined(_M_PPC)
#define SLANG_X360 1
#elif defined(_WIN32)
#define SLANG_WIN32 1
#elif defined(__ANDROID__)
#define SLANG_ANDROID 1
#elif defined(__linux__) || defined(__CYGWIN__)
#define SLANG_LINUX 1
#elif defined(__APPLE__)
#include "TargetConditionals.h"
#if TARGET_OS_MAC
#define SLANG_OSX 1
#else
#define SLANG_IOS 1
#endif
#elif defined(__CELLOS_LV2__)
#define SLANG_PS3 1
#elif defined(__ORBIS__)
#define SLANG_PS4 1
#elif defined(__SNC__) && defined(__arm__)
#define SLANG_PSP2 1
#elif defined(__ghs__)
#define SLANG_WIIU 1
#elif defined(__EMSCRIPTEN__)
#define SLANG_WASM 1
#else
#error "unknown target platform"
#endif
#ifndef SLANG_WINRT
#define SLANG_WINRT 0
#endif
#ifndef SLANG_XBOXONE
#define SLANG_XBOXONE 0
#endif
#ifndef SLANG_WIN64
#define SLANG_WIN64 0
#endif
#ifndef SLANG_X360
#define SLANG_X360 0
#endif
#ifndef SLANG_WIN32
#define SLANG_WIN32 0
#endif
#ifndef SLANG_ANDROID
#define SLANG_ANDROID 0
#endif
#ifndef SLANG_LINUX
#define SLANG_LINUX 0
#endif
#ifndef SLANG_IOS
#define SLANG_IOS 0
#endif
#ifndef SLANG_OSX
#define SLANG_OSX 0
#endif
#ifndef SLANG_PS3
#define SLANG_PS3 0
#endif
#ifndef SLANG_PS4
#define SLANG_PS4 0
#endif
#ifndef SLANG_PSP2
#define SLANG_PSP2 0
#endif
#ifndef SLANG_WIIU
#define SLANG_WIIU 0
#endif
#endif
#define SLANG_GCC_FAMILY (SLANG_CLANG || SLANG_SNC || SLANG_GHS || SLANG_GCC)
#define SLANG_WINDOWS_FAMILY (SLANG_WINRT || SLANG_WIN32 || SLANG_WIN64)
#define SLANG_MICROSOFT_FAMILY (SLANG_XBOXONE || SLANG_X360 || SLANG_WINDOWS_FAMILY)
#define SLANG_LINUX_FAMILY (SLANG_LINUX || SLANG_ANDROID)
#define SLANG_APPLE_FAMILY (SLANG_IOS || SLANG_OSX)
#define SLANG_UNIX_FAMILY \
(SLANG_LINUX_FAMILY || SLANG_APPLE_FAMILY)
#if !defined(SLANG_CONFIG_DX_ON_VK) || !SLANG_CONFIG_DX_ON_VK
#define SLANG_ENABLE_DXVK 0
#define SLANG_ENABLE_VKD3D 0
#else
#define SLANG_ENABLE_DXVK 1
#define SLANG_ENABLE_VKD3D 1
#endif
#if SLANG_WINDOWS_FAMILY
#define SLANG_ENABLE_DIRECTX 1
#define SLANG_ENABLE_DXGI_DEBUG 1
#define SLANG_ENABLE_DXBC_SUPPORT 1
#define SLANG_ENABLE_PIX 1
#elif SLANG_LINUX_FAMILY
#define SLANG_ENABLE_DIRECTX (SLANG_ENABLE_DXVK || SLANG_ENABLE_VKD3D)
#define SLANG_ENABLE_DXGI_DEBUG 0
#define SLANG_ENABLE_DXBC_SUPPORT 0
#define SLANG_ENABLE_PIX 0
#else
#define SLANG_ENABLE_DIRECTX 0
#define SLANG_ENABLE_DXGI_DEBUG 0
#define SLANG_ENABLE_DXBC_SUPPORT 0
#define SLANG_ENABLE_PIX 0
#endif
#ifndef SLANG_NO_THROW
#if SLANG_WINDOWS_FAMILY && !defined(SLANG_DISABLE_EXCEPTIONS)
#define SLANG_NO_THROW __declspec(nothrow)
#endif
#endif
#ifndef SLANG_NO_THROW
#define SLANG_NO_THROW
#endif
#ifndef SLANG_STDCALL
#if SLANG_MICROSOFT_FAMILY
#define SLANG_STDCALL __stdcall
#else
#define SLANG_STDCALL
#endif
#endif
#ifndef SLANG_MCALL
#define SLANG_MCALL SLANG_STDCALL
#endif
#if !defined(SLANG_STATIC) && !defined(SLANG_DYNAMIC)
#define SLANG_DYNAMIC
#endif
#if defined(_MSC_VER)
#define SLANG_DLL_EXPORT __declspec(dllexport)
#else
#if SLANG_WINDOWS_FAMILY
#define SLANG_DLL_EXPORT \
__attribute__((dllexport)) __attribute__((__visibility__("default")))
#else
#define SLANG_DLL_EXPORT __attribute__((__visibility__("default")))
#endif
#endif
#if defined(SLANG_DYNAMIC)
#if defined(_MSC_VER)
#ifdef SLANG_DYNAMIC_EXPORT
#define SLANG_API SLANG_DLL_EXPORT
#else
#define SLANG_API __declspec(dllimport)
#endif
#else
#define SLANG_API SLANG_DLL_EXPORT
#endif
#endif
#ifndef SLANG_API
#define SLANG_API
#endif
#if SLANG_GCC_FAMILY
#define SLANG_NO_INLINE __attribute__((noinline))
#define SLANG_FORCE_INLINE inline __attribute__((always_inline))
#define SLANG_BREAKPOINT(id) __builtin_trap();
#define SLANG_ALIGN_OF(T) __alignof__(T)
#endif
#if SLANG_GCC_FAMILY || defined(__clang__)
#define SLANG_OFFSET_OF(T, ELEMENT) __builtin_offsetof(T, ELEMENT)
#endif
#ifndef SLANG_OFFSET_OF
#define SLANG_OFFSET_OF(T, ELEMENT) (size_t(&((T*)1)->ELEMENT) - 1)
#endif
#if SLANG_VC
#define SLANG_NO_INLINE __declspec(noinline)
#define SLANG_FORCE_INLINE __forceinline
#define SLANG_BREAKPOINT(id) __debugbreak();
#define SLANG_ALIGN_OF(T) __alignof(T)
#define SLANG_INT64(x) (x##i64)
#define SLANG_UINT64(x) (x##ui64)
#endif
#ifndef SLANG_FORCE_INLINE
#define SLANG_FORCE_INLINE inline
#endif
#ifndef SLANG_NO_INLINE
#define SLANG_NO_INLINE
#endif
#ifndef SLANG_COMPILE_TIME_ASSERT
#define SLANG_COMPILE_TIME_ASSERT(x) static_assert(x)
#endif
#ifndef SLANG_BREAKPOINT
#define SLANG_BREAKPOINT(id) (*((int*)0) = int(id));
#endif
#define SLANG_COUNT_OF(x) (SlangSSizeT(sizeof(x) / sizeof(0 [x])))
#define SLANG_INLINE inline
#if !defined(SLANG_HAS_EXCEPTIONS) && defined(SLANG_DISABLE_EXCEPTIONS)
#define SLANG_HAS_EXCEPTIONS 0
#endif
#ifndef SLANG_HAS_EXCEPTIONS
#define SLANG_HAS_EXCEPTIONS 1
#endif
#define SLANG_STRINGIZE_HELPER(X) #X
#define SLANG_STRINGIZE(X) SLANG_STRINGIZE_HELPER(X)
#define SLANG_CONCAT_HELPER(X, Y) X##Y
#define SLANG_CONCAT(X, Y) SLANG_CONCAT_HELPER(X, Y)
#ifndef SLANG_UNUSED
#define SLANG_UNUSED(v) (void)v;
#endif
#if defined(__llvm__)
#define SLANG_MAYBE_UNUSED [[maybe_unused]]
#else
#define SLANG_MAYBE_UNUSED
#endif
#ifndef SLANG_INT64
#define SLANG_INT64(x) (x##ll)
#endif
#ifndef SLANG_UINT64
#define SLANG_UINT64(x) (x##ull)
#endif
#ifdef __cplusplus
#define SLANG_EXTERN_C extern "C"
#else
#define SLANG_EXTERN_C
#endif
#ifdef __cplusplus
#if SLANG_CLANG
#if (__clang_major__ * 10 + __clang_minor__) >= 33
#define SLANG_HAS_MOVE_SEMANTICS 1
#define SLANG_HAS_ENUM_CLASS 1
#define SLANG_OVERRIDE override
#endif
#elif SLANG_GCC_FAMILY
#if (__cplusplus >= 201103L)
#if (__GNUC__ * 100 + __GNUC_MINOR__) >= 405
#define SLANG_HAS_MOVE_SEMANTICS 1
#endif
#if (__GNUC__ * 100 + __GNUC_MINOR__) >= 406
#define SLANG_HAS_ENUM_CLASS 1
#endif
#if (__GNUC__ * 100 + __GNUC_MINOR__) >= 407
#define SLANG_OVERRIDE override
#endif
#endif
#endif
#if SLANG_VC
#if _MSC_VER < 1700
#pragma warning(disable : 4481)
#endif
#define SLANG_OVERRIDE override
#if _MSC_VER >= 1600
#define SLANG_HAS_MOVE_SEMANTICS 1
#endif
#if _MSC_VER >= 1700
#define SLANG_HAS_ENUM_CLASS 1
#endif
#endif
#ifndef SLANG_OVERRIDE
#define SLANG_OVERRIDE
#endif
#ifndef SLANG_HAS_ENUM_CLASS
#define SLANG_HAS_ENUM_CLASS 0
#endif
#ifndef SLANG_HAS_MOVE_SEMANTICS
#define SLANG_HAS_MOVE_SEMANTICS 0
#endif
#endif
#if defined(_M_ARM) || defined(__ARM_EABI__)
#define SLANG_PROCESSOR_ARM 1
#elif defined(__i386__) || defined(_M_IX86)
#define SLANG_PROCESSOR_X86 1
#elif defined(_M_AMD64) || defined(_M_X64) || defined(__amd64) || defined(__x86_64)
#define SLANG_PROCESSOR_X86_64 1
#elif defined(_PPC_) || defined(__ppc__) || defined(__POWERPC__) || defined(_M_PPC)
#if defined(__powerpc64__) || defined(__ppc64__) || defined(__PPC64__) || \
defined(__64BIT__) || defined(_LP64) || defined(__LP64__)
#define SLANG_PROCESSOR_POWER_PC_64 1
#else
#define SLANG_PROCESSOR_POWER_PC 1
#endif
#elif defined(__arm__)
#define SLANG_PROCESSOR_ARM 1
#elif defined(_M_ARM64) || defined(__aarch64__)
#define SLANG_PROCESSOR_ARM_64 1
#elif defined(__EMSCRIPTEN__)
#define SLANG_PROCESSOR_WASM 1
#endif
#ifndef SLANG_PROCESSOR_ARM
#define SLANG_PROCESSOR_ARM 0
#endif
#ifndef SLANG_PROCESSOR_ARM_64
#define SLANG_PROCESSOR_ARM_64 0
#endif
#ifndef SLANG_PROCESSOR_X86
#define SLANG_PROCESSOR_X86 0
#endif
#ifndef SLANG_PROCESSOR_X86_64
#define SLANG_PROCESSOR_X86_64 0
#endif
#ifndef SLANG_PROCESSOR_POWER_PC
#define SLANG_PROCESSOR_POWER_PC 0
#endif
#ifndef SLANG_PROCESSOR_POWER_PC_64
#define SLANG_PROCESSOR_POWER_PC_64 0
#endif
#define SLANG_PROCESSOR_FAMILY_X86 (SLANG_PROCESSOR_X86_64 | SLANG_PROCESSOR_X86)
#define SLANG_PROCESSOR_FAMILY_ARM (SLANG_PROCESSOR_ARM | SLANG_PROCESSOR_ARM_64)
#define SLANG_PROCESSOR_FAMILY_POWER_PC (SLANG_PROCESSOR_POWER_PC_64 | SLANG_PROCESSOR_POWER_PC)
#define SLANG_PTR_IS_64 \
(SLANG_PROCESSOR_ARM_64 | SLANG_PROCESSOR_X86_64 | SLANG_PROCESSOR_POWER_PC_64)
#define SLANG_PTR_IS_32 (SLANG_PTR_IS_64 ^ 1)
#if SLANG_PROCESSOR_FAMILY_X86
#define SLANG_LITTLE_ENDIAN 1
#define SLANG_UNALIGNED_ACCESS 1
#elif SLANG_PROCESSOR_FAMILY_ARM
#if defined(__ARMEB__)
#define SLANG_BIG_ENDIAN 1
#else
#define SLANG_LITTLE_ENDIAN 1
#endif
#elif SLANG_PROCESSOR_FAMILY_POWER_PC
#define SLANG_BIG_ENDIAN 1
#elif SLANG_WASM
#define SLANG_LITTLE_ENDIAN 1
#endif
#ifndef SLANG_LITTLE_ENDIAN
#define SLANG_LITTLE_ENDIAN 0
#endif
#ifndef SLANG_BIG_ENDIAN
#define SLANG_BIG_ENDIAN 0
#endif
#ifndef SLANG_UNALIGNED_ACCESS
#define SLANG_UNALIGNED_ACCESS 0
#endif
#if ((SLANG_BIG_ENDIAN | SLANG_LITTLE_ENDIAN) == 0)
#error "Couldn't determine endianness"
#endif
#ifndef SLANG_NO_INTTYPES
#include <inttypes.h>
#endif
#ifndef SLANG_NO_STDDEF
#include <stddef.h>
#endif
#ifdef __cplusplus
extern "C"
{
#endif
typedef uint32_t SlangUInt32;
typedef int32_t SlangInt32;
#if SLANG_PTR_IS_64
typedef int64_t SlangInt;
typedef uint64_t SlangUInt;
typedef int64_t SlangSSizeT;
typedef uint64_t SlangSizeT;
#else
typedef int32_t SlangInt;
typedef uint32_t SlangUInt;
typedef int32_t SlangSSizeT;
typedef uint32_t SlangSizeT;
#endif
typedef bool SlangBool;
typedef int SlangSeverityIntegral;
enum SlangSeverity : SlangSeverityIntegral
{
SLANG_SEVERITY_DISABLED = 0,
SLANG_SEVERITY_NOTE,
SLANG_SEVERITY_WARNING,
SLANG_SEVERITY_ERROR,
SLANG_SEVERITY_FATAL,
SLANG_SEVERITY_INTERNAL,
};
typedef int SlangDiagnosticFlags;
enum
{
SLANG_DIAGNOSTIC_FLAG_VERBOSE_PATHS = 0x01,
SLANG_DIAGNOSTIC_FLAG_TREAT_WARNINGS_AS_ERRORS = 0x02
};
typedef int SlangBindableResourceIntegral;
enum SlangBindableResourceType : SlangBindableResourceIntegral
{
SLANG_NON_BINDABLE = 0,
SLANG_TEXTURE,
SLANG_SAMPLER,
SLANG_UNIFORM_BUFFER,
SLANG_STORAGE_BUFFER,
};
typedef int SlangCompileTargetIntegral;
enum SlangCompileTarget : SlangCompileTargetIntegral
{
SLANG_TARGET_UNKNOWN,
SLANG_TARGET_NONE,
SLANG_GLSL,
SLANG_GLSL_VULKAN_DEPRECATED,
SLANG_GLSL_VULKAN_ONE_DESC_DEPRECATED,
SLANG_HLSL,
SLANG_SPIRV,
SLANG_SPIRV_ASM,
SLANG_DXBC,
SLANG_DXBC_ASM,
SLANG_DXIL,
SLANG_DXIL_ASM,
SLANG_C_SOURCE,
SLANG_CPP_SOURCE,
SLANG_HOST_EXECUTABLE,
SLANG_SHADER_SHARED_LIBRARY,
SLANG_SHADER_HOST_CALLABLE,
SLANG_CUDA_SOURCE,
SLANG_PTX,
SLANG_CUDA_OBJECT_CODE,
SLANG_OBJECT_CODE,
SLANG_HOST_CPP_SOURCE,
SLANG_HOST_HOST_CALLABLE,
SLANG_CPP_PYTORCH_BINDING,
SLANG_METAL,
SLANG_METAL_LIB,
SLANG_METAL_LIB_ASM,
SLANG_HOST_SHARED_LIBRARY,
SLANG_WGSL,
SLANG_WGSL_SPIRV_ASM,
SLANG_WGSL_SPIRV,
SLANG_TARGET_COUNT_OF,
};
typedef int SlangContainerFormatIntegral;
enum SlangContainerFormat : SlangContainerFormatIntegral
{
SLANG_CONTAINER_FORMAT_NONE,
SLANG_CONTAINER_FORMAT_SLANG_MODULE,
};
typedef int SlangPassThroughIntegral;
enum SlangPassThrough : SlangPassThroughIntegral
{
SLANG_PASS_THROUGH_NONE,
SLANG_PASS_THROUGH_FXC,
SLANG_PASS_THROUGH_DXC,
SLANG_PASS_THROUGH_GLSLANG,
SLANG_PASS_THROUGH_SPIRV_DIS,
SLANG_PASS_THROUGH_CLANG,
SLANG_PASS_THROUGH_VISUAL_STUDIO,
SLANG_PASS_THROUGH_GCC,
SLANG_PASS_THROUGH_GENERIC_C_CPP,
SLANG_PASS_THROUGH_NVRTC,
SLANG_PASS_THROUGH_LLVM,
SLANG_PASS_THROUGH_SPIRV_OPT,
SLANG_PASS_THROUGH_METAL,
SLANG_PASS_THROUGH_TINT,
SLANG_PASS_THROUGH_COUNT_OF,
};
typedef int SlangArchiveTypeIntegral;
enum SlangArchiveType : SlangArchiveTypeIntegral
{
SLANG_ARCHIVE_TYPE_UNDEFINED,
SLANG_ARCHIVE_TYPE_ZIP,
SLANG_ARCHIVE_TYPE_RIFF,
SLANG_ARCHIVE_TYPE_RIFF_DEFLATE,
SLANG_ARCHIVE_TYPE_RIFF_LZ4,
SLANG_ARCHIVE_TYPE_COUNT_OF,
};
typedef unsigned int SlangCompileFlags;
enum
{
SLANG_COMPILE_FLAG_NO_MANGLING = 1 << 3,
SLANG_COMPILE_FLAG_NO_CODEGEN = 1 << 4,
SLANG_COMPILE_FLAG_OBFUSCATE = 1 << 5,
SLANG_COMPILE_FLAG_NO_CHECKING = 0,
SLANG_COMPILE_FLAG_SPLIT_MIXED_TYPES = 0,
};
typedef unsigned int SlangTargetFlags;
enum
{
SLANG_TARGET_FLAG_PARAMETER_BLOCKS_USE_REGISTER_SPACES = 1 << 4,
SLANG_TARGET_FLAG_GENERATE_WHOLE_PROGRAM = 1 << 8,
SLANG_TARGET_FLAG_DUMP_IR = 1 << 9,
SLANG_TARGET_FLAG_GENERATE_SPIRV_DIRECTLY = 1 << 10,
};
constexpr static SlangTargetFlags kDefaultTargetFlags =
SLANG_TARGET_FLAG_GENERATE_SPIRV_DIRECTLY;
typedef unsigned int SlangFloatingPointModeIntegral;
enum SlangFloatingPointMode : SlangFloatingPointModeIntegral
{
SLANG_FLOATING_POINT_MODE_DEFAULT = 0,
SLANG_FLOATING_POINT_MODE_FAST,
SLANG_FLOATING_POINT_MODE_PRECISE,
};
typedef unsigned int SlangLineDirectiveModeIntegral;
enum SlangLineDirectiveMode : SlangLineDirectiveModeIntegral
{
SLANG_LINE_DIRECTIVE_MODE_DEFAULT =
0,
SLANG_LINE_DIRECTIVE_MODE_NONE,
SLANG_LINE_DIRECTIVE_MODE_STANDARD,
SLANG_LINE_DIRECTIVE_MODE_GLSL,
SLANG_LINE_DIRECTIVE_MODE_SOURCE_MAP,
};
typedef int SlangSourceLanguageIntegral;
enum SlangSourceLanguage : SlangSourceLanguageIntegral
{
SLANG_SOURCE_LANGUAGE_UNKNOWN,
SLANG_SOURCE_LANGUAGE_SLANG,
SLANG_SOURCE_LANGUAGE_HLSL,
SLANG_SOURCE_LANGUAGE_GLSL,
SLANG_SOURCE_LANGUAGE_C,
SLANG_SOURCE_LANGUAGE_CPP,
SLANG_SOURCE_LANGUAGE_CUDA,
SLANG_SOURCE_LANGUAGE_SPIRV,
SLANG_SOURCE_LANGUAGE_METAL,
SLANG_SOURCE_LANGUAGE_WGSL,
SLANG_SOURCE_LANGUAGE_COUNT_OF,
};
typedef unsigned int SlangProfileIDIntegral;
enum SlangProfileID : SlangProfileIDIntegral
{
SLANG_PROFILE_UNKNOWN,
};
typedef SlangInt32 SlangCapabilityIDIntegral;
enum SlangCapabilityID : SlangCapabilityIDIntegral
{
SLANG_CAPABILITY_UNKNOWN = 0,
};
typedef unsigned int SlangMatrixLayoutModeIntegral;
enum SlangMatrixLayoutMode : SlangMatrixLayoutModeIntegral
{
SLANG_MATRIX_LAYOUT_MODE_UNKNOWN = 0,
SLANG_MATRIX_LAYOUT_ROW_MAJOR,
SLANG_MATRIX_LAYOUT_COLUMN_MAJOR,
};
typedef SlangUInt32 SlangStageIntegral;
enum SlangStage : SlangStageIntegral
{
SLANG_STAGE_NONE,
SLANG_STAGE_VERTEX,
SLANG_STAGE_HULL,
SLANG_STAGE_DOMAIN,
SLANG_STAGE_GEOMETRY,
SLANG_STAGE_FRAGMENT,
SLANG_STAGE_COMPUTE,
SLANG_STAGE_RAY_GENERATION,
SLANG_STAGE_INTERSECTION,
SLANG_STAGE_ANY_HIT,
SLANG_STAGE_CLOSEST_HIT,
SLANG_STAGE_MISS,
SLANG_STAGE_CALLABLE,
SLANG_STAGE_MESH,
SLANG_STAGE_AMPLIFICATION,
SLANG_STAGE_COUNT,
SLANG_STAGE_PIXEL = SLANG_STAGE_FRAGMENT,
};
typedef SlangUInt32 SlangDebugInfoLevelIntegral;
enum SlangDebugInfoLevel : SlangDebugInfoLevelIntegral
{
SLANG_DEBUG_INFO_LEVEL_NONE = 0,
SLANG_DEBUG_INFO_LEVEL_MINIMAL,
SLANG_DEBUG_INFO_LEVEL_STANDARD,
SLANG_DEBUG_INFO_LEVEL_MAXIMAL,
};
typedef SlangUInt32 SlangDebugInfoFormatIntegral;
enum SlangDebugInfoFormat : SlangDebugInfoFormatIntegral
{
SLANG_DEBUG_INFO_FORMAT_DEFAULT,
SLANG_DEBUG_INFO_FORMAT_C7,
SLANG_DEBUG_INFO_FORMAT_PDB,
SLANG_DEBUG_INFO_FORMAT_STABS,
SLANG_DEBUG_INFO_FORMAT_COFF,
SLANG_DEBUG_INFO_FORMAT_DWARF,
SLANG_DEBUG_INFO_FORMAT_COUNT_OF,
};
typedef SlangUInt32 SlangOptimizationLevelIntegral;
enum SlangOptimizationLevel : SlangOptimizationLevelIntegral
{
SLANG_OPTIMIZATION_LEVEL_NONE = 0,
SLANG_OPTIMIZATION_LEVEL_DEFAULT,
SLANG_OPTIMIZATION_LEVEL_HIGH,
SLANG_OPTIMIZATION_LEVEL_MAXIMAL,
};
enum SlangEmitSpirvMethod
{
SLANG_EMIT_SPIRV_DEFAULT = 0,
SLANG_EMIT_SPIRV_VIA_GLSL,
SLANG_EMIT_SPIRV_DIRECTLY,
};
namespace slang
{
enum class CompilerOptionName
{
MacroDefine,
DepFile,
EntryPointName,
Specialize,
Help,
HelpStyle,
Include,
Language,
MatrixLayoutColumn,
MatrixLayoutRow,
ZeroInitialize,
IgnoreCapabilities,
RestrictiveCapabilityCheck,
ModuleName,
Output,
Profile,
Stage,
Target,
Version,
WarningsAsErrors,
DisableWarnings,
EnableWarning,
DisableWarning,
DumpWarningDiagnostics,
InputFilesRemain,
EmitIr,
ReportDownstreamTime,
ReportPerfBenchmark,
ReportCheckpointIntermediates,
SkipSPIRVValidation,
SourceEmbedStyle,
SourceEmbedName,
SourceEmbedLanguage,
DisableShortCircuit,
MinimumSlangOptimization,
DisableNonEssentialValidations,
DisableSourceMap,
UnscopedEnum,
PreserveParameters,
Capability,
DefaultImageFormatUnknown,
DisableDynamicDispatch,
DisableSpecialization,
FloatingPointMode,
DebugInformation,
LineDirectiveMode,
Optimization,
Obfuscate,
VulkanBindShift,
VulkanBindGlobals,
VulkanInvertY,
VulkanUseDxPositionW,
VulkanUseEntryPointName,
VulkanUseGLLayout,
VulkanEmitReflection,
GLSLForceScalarLayout,
EnableEffectAnnotations,
EmitSpirvViaGLSL,
EmitSpirvDirectly,
SPIRVCoreGrammarJSON,
IncompleteLibrary,
CompilerPath,
DefaultDownstreamCompiler,
DownstreamArgs,
PassThrough,
DumpRepro,
DumpReproOnError,
ExtractRepro,
LoadRepro,
LoadReproDirectory,
ReproFallbackDirectory,
DumpAst,
DumpIntermediatePrefix,
DumpIntermediates,
DumpIr,
DumpIrIds,
PreprocessorOutput,
OutputIncludes,
ReproFileSystem,
SerialIr,
SkipCodeGen,
ValidateIr,
VerbosePaths,
VerifyDebugSerialIr,
NoCodeGen,
FileSystem,
Heterogeneous,
NoMangle,
NoHLSLBinding,
NoHLSLPackConstantBufferElements,
ValidateUniformity,
AllowGLSL,
EnableExperimentalPasses,
BindlessSpaceIndex,
ArchiveType,
CompileCoreModule,
Doc,
IrCompression,
LoadCoreModule,
ReferenceModule,
SaveCoreModule,
SaveCoreModuleBinSource,
TrackLiveness,
LoopInversion,
ParameterBlocksUseRegisterSpaces,
CountOfParsableOptions,
DebugInformationFormat,
VulkanBindShiftAll,
GenerateWholeProgram,
UseUpToDateBinaryModule,
EmbedDownstreamIR,
ForceDXLayout,
EmitSpirvMethod,
EmitReflectionJSON,
SaveGLSLModuleBinSource,
CountOf,
};
enum class CompilerOptionValueKind
{
Int,
String
};
struct CompilerOptionValue
{
CompilerOptionValueKind kind = CompilerOptionValueKind::Int;
int32_t intValue0 = 0;
int32_t intValue1 = 0;
const char* stringValue0 = nullptr;
const char* stringValue1 = nullptr;
};
struct CompilerOptionEntry
{
CompilerOptionName name;
CompilerOptionValue value;
};
}
typedef int32_t SlangResult;
#define SLANG_FAILED(status) ((status) < 0)
#define SLANG_SUCCEEDED(status) ((status) >= 0)
#define SLANG_GET_RESULT_FACILITY(r) ((int32_t)(((r) >> 16) & 0x7fff))
#define SLANG_GET_RESULT_CODE(r) ((int32_t)((r) & 0xffff))
#define SLANG_MAKE_ERROR(fac, code) \
((((int32_t)(fac)) << 16) | ((int32_t)(code)) | int32_t(0x80000000))
#define SLANG_MAKE_SUCCESS(fac, code) ((((int32_t)(fac)) << 16) | ((int32_t)(code)))
#define SLANG_FACILITY_WIN_GENERAL 0
#define SLANG_FACILITY_WIN_INTERFACE 4
#define SLANG_FACILITY_WIN_API 7
#define SLANG_FACILITY_BASE 0x200
#define SLANG_FACILITY_CORE SLANG_FACILITY_BASE
#define SLANG_FACILITY_INTERNAL SLANG_FACILITY_BASE + 1
#define SLANG_FACILITY_EXTERNAL_BASE 0x210
#define SLANG_OK 0
#define SLANG_FAIL SLANG_MAKE_ERROR(SLANG_FACILITY_WIN_GENERAL, 0x4005)
#define SLANG_MAKE_WIN_GENERAL_ERROR(code) SLANG_MAKE_ERROR(SLANG_FACILITY_WIN_GENERAL, code)
#define SLANG_E_NOT_IMPLEMENTED SLANG_MAKE_WIN_GENERAL_ERROR(0x4001)
#define SLANG_E_NO_INTERFACE SLANG_MAKE_WIN_GENERAL_ERROR(0x4002)
#define SLANG_E_ABORT SLANG_MAKE_WIN_GENERAL_ERROR(0x4004)
#define SLANG_E_INVALID_HANDLE SLANG_MAKE_ERROR(SLANG_FACILITY_WIN_API, 6)
#define SLANG_E_INVALID_ARG SLANG_MAKE_ERROR(SLANG_FACILITY_WIN_API, 0x57)
#define SLANG_E_OUT_OF_MEMORY SLANG_MAKE_ERROR(SLANG_FACILITY_WIN_API, 0xe)
#define SLANG_MAKE_CORE_ERROR(code) SLANG_MAKE_ERROR(SLANG_FACILITY_CORE, code)
#define SLANG_E_BUFFER_TOO_SMALL SLANG_MAKE_CORE_ERROR(1)
#define SLANG_E_UNINITIALIZED SLANG_MAKE_CORE_ERROR(2)
#define SLANG_E_PENDING SLANG_MAKE_CORE_ERROR(3)
#define SLANG_E_CANNOT_OPEN SLANG_MAKE_CORE_ERROR(4)
#define SLANG_E_NOT_FOUND SLANG_MAKE_CORE_ERROR(5)
#define SLANG_E_INTERNAL_FAIL SLANG_MAKE_CORE_ERROR(6)
#define SLANG_E_NOT_AVAILABLE SLANG_MAKE_CORE_ERROR(7)
#define SLANG_E_TIME_OUT SLANG_MAKE_CORE_ERROR(8)
struct SlangUUID
{
uint32_t data1;
uint16_t data2;
uint16_t data3;
uint8_t data4[8];
};
#define SLANG_COM_INTERFACE(a, b, c, d0, d1, d2, d3, d4, d5, d6, d7) \
public:                                                              \
SLANG_FORCE_INLINE constexpr static SlangUUID getTypeGuid()      \
{                                                                \
return {a, b, c, d0, d1, d2, d3, d4, d5, d6, d7};            \
}
#define SLANG_CLASS_GUID(a, b, c, d0, d1, d2, d3, d4, d5, d6, d7) \
SLANG_FORCE_INLINE constexpr static SlangUUID getTypeGuid()   \
{                                                             \
return {a, b, c, d0, d1, d2, d3, d4, d5, d6, d7};         \
}
#define SLANG_IID_PPV_ARGS(ppType)                                                         \
std::decay_t<decltype(**(ppType))>::getTypeGuid(),                                     \
(                                                                                  \
(void)[] {                                                                     \
static_assert(                                                             \
std::is_base_of_v<ISlangUnknown, std::decay_t<decltype(**(ppType))>>); \
},                                                                             \
reinterpret_cast<void**>(ppType))
struct ISlangUnknown
{
SLANG_COM_INTERFACE(
0x00000000,
0x0000,
0x0000,
{0xC0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x46})
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
queryInterface(SlangUUID const& uuid, void** outObject) = 0;
virtual SLANG_NO_THROW uint32_t SLANG_MCALL addRef() = 0;
virtual SLANG_NO_THROW uint32_t SLANG_MCALL release() = 0;
SlangResult QueryInterface(struct _GUID const& uuid, void** outObject)
{
return queryInterface(*(SlangUUID const*)&uuid, outObject);
}
uint32_t AddRef() { return addRef(); }
uint32_t Release() { return release(); }
};
#define SLANG_UUID_ISlangUnknown ISlangUnknown::getTypeGuid()
class ISlangCastable : public ISlangUnknown
{
SLANG_COM_INTERFACE(
0x87ede0e1,
0x4852,
0x44b0,
{0x8b, 0xf2, 0xcb, 0x31, 0x87, 0x4d, 0xe2, 0x39});
virtual SLANG_NO_THROW void* SLANG_MCALL castAs(const SlangUUID& guid) = 0;
};
class ISlangClonable : public ISlangCastable
{
SLANG_COM_INTERFACE(
0x1ec36168,
0xe9f4,
0x430d,
{0xbb, 0x17, 0x4, 0x8a, 0x80, 0x46, 0xb3, 0x1f});
SLANG_NO_THROW virtual void* SLANG_MCALL clone(const SlangUUID& guid) = 0;
};
struct ISlangBlob : public ISlangUnknown
{
SLANG_COM_INTERFACE(
0x8BA5FB08,
0x5195,
0x40e2,
{0xAC, 0x58, 0x0D, 0x98, 0x9C, 0x3A, 0x01, 0x02})
virtual SLANG_NO_THROW void const* SLANG_MCALL getBufferPointer() = 0;
virtual SLANG_NO_THROW size_t SLANG_MCALL getBufferSize() = 0;
};
#define SLANG_UUID_ISlangBlob ISlangBlob::getTypeGuid()
struct SlangTerminatedChars
{
SLANG_CLASS_GUID(
0xbe0db1a8,
0x3594,
0x4603,
{0xa7, 0x8b, 0xc4, 0x86, 0x84, 0x30, 0xdf, 0xbb});
operator const char*() const { return chars; }
char chars[1];
};
struct ISlangFileSystem : public ISlangCastable
{
SLANG_COM_INTERFACE(
0x003A09FC,
0x3A4D,
0x4BA0,
{0xAD, 0x60, 0x1F, 0xD8, 0x63, 0xA9, 0x15, 0xAB})
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
loadFile(char const* path, ISlangBlob** outBlob) = 0;
};
#define SLANG_UUID_ISlangFileSystem ISlangFileSystem::getTypeGuid()
typedef void (*SlangFuncPtr)(void);
struct ISlangSharedLibrary_Dep1 : public ISlangUnknown
{
SLANG_COM_INTERFACE(
0x9c9d5bc5,
0xeb61,
0x496f,
{0x80, 0xd7, 0xd1, 0x47, 0xc4, 0xa2, 0x37, 0x30})
virtual SLANG_NO_THROW void* SLANG_MCALL findSymbolAddressByName(char const* name) = 0;
};
#define SLANG_UUID_ISlangSharedLibrary_Dep1 ISlangSharedLibrary_Dep1::getTypeGuid()
struct ISlangSharedLibrary : public ISlangCastable
{
SLANG_COM_INTERFACE(
0x70dbc7c4,
0xdc3b,
0x4a07,
{0xae, 0x7e, 0x75, 0x2a, 0xf6, 0xa8, 0x15, 0x55})
SLANG_FORCE_INLINE SlangFuncPtr findFuncByName(char const* name)
{
return (SlangFuncPtr)findSymbolAddressByName(name);
}
virtual SLANG_NO_THROW void* SLANG_MCALL findSymbolAddressByName(char const* name) = 0;
};
#define SLANG_UUID_ISlangSharedLibrary ISlangSharedLibrary::getTypeGuid()
struct ISlangSharedLibraryLoader : public ISlangUnknown
{
SLANG_COM_INTERFACE(
0x6264ab2b,
0xa3e8,
0x4a06,
{0x97, 0xf1, 0x49, 0xbc, 0x2d, 0x2a, 0xb1, 0x4d})
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
loadSharedLibrary(const char* path, ISlangSharedLibrary** sharedLibraryOut) = 0;
};
#define SLANG_UUID_ISlangSharedLibraryLoader ISlangSharedLibraryLoader::getTypeGuid()
typedef unsigned int SlangPathTypeIntegral;
enum SlangPathType : SlangPathTypeIntegral
{
SLANG_PATH_TYPE_DIRECTORY,
SLANG_PATH_TYPE_FILE,
};
typedef void (
*FileSystemContentsCallBack)(SlangPathType pathType, const char* name, void* userData);
enum class OSPathKind : uint8_t
{
None,
Direct,
OperatingSystem,
};
enum class PathKind
{
Simplified,
Canonical,
Display,
OperatingSystem,
CountOf,
};
struct ISlangFileSystemExt : public ISlangFileSystem
{
SLANG_COM_INTERFACE(
0x5fb632d2,
0x979d,
0x4481,
{0x9f, 0xee, 0x66, 0x3c, 0x3f, 0x14, 0x49, 0xe1})
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
getFileUniqueIdentity(const char* path, ISlangBlob** outUniqueIdentity) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL calcCombinedPath(
SlangPathType fromPathType,
const char* fromPath,
const char* path,
ISlangBlob** pathOut) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
getPathType(const char* path, SlangPathType* pathTypeOut) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
getPath(PathKind kind, const char* path, ISlangBlob** outPath) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL clearCache() = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL enumeratePathContents(
const char* path,
FileSystemContentsCallBack callback,
void* userData) = 0;
virtual SLANG_NO_THROW OSPathKind SLANG_MCALL getOSPathKind() = 0;
};
#define SLANG_UUID_ISlangFileSystemExt ISlangFileSystemExt::getTypeGuid()
struct ISlangMutableFileSystem : public ISlangFileSystemExt
{
SLANG_COM_INTERFACE(
0xa058675c,
0x1d65,
0x452a,
{0x84, 0x58, 0xcc, 0xde, 0xd1, 0x42, 0x71, 0x5})
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
saveFile(const char* path, const void* data, size_t size) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
saveFileBlob(const char* path, ISlangBlob* dataBlob) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL remove(const char* path) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL createDirectory(const char* path) = 0;
};
#define SLANG_UUID_ISlangMutableFileSystem ISlangMutableFileSystem::getTypeGuid()
typedef unsigned int SlangWriterChannelIntegral;
enum SlangWriterChannel : SlangWriterChannelIntegral
{
SLANG_WRITER_CHANNEL_DIAGNOSTIC,
SLANG_WRITER_CHANNEL_STD_OUTPUT,
SLANG_WRITER_CHANNEL_STD_ERROR,
SLANG_WRITER_CHANNEL_COUNT_OF,
};
typedef unsigned int SlangWriterModeIntegral;
enum SlangWriterMode : SlangWriterModeIntegral
{
SLANG_WRITER_MODE_TEXT,
SLANG_WRITER_MODE_BINARY,
};
struct ISlangWriter : public ISlangUnknown
{
SLANG_COM_INTERFACE(
0xec457f0e,
0x9add,
0x4e6b,
{0x85, 0x1c, 0xd7, 0xfa, 0x71, 0x6d, 0x15, 0xfd})
virtual SLANG_NO_THROW char* SLANG_MCALL beginAppendBuffer(size_t maxNumChars) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
endAppendBuffer(char* buffer, size_t numChars) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
write(const char* chars, size_t numChars) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL flush() = 0;
virtual SLANG_NO_THROW SlangBool SLANG_MCALL isConsole() = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL setMode(SlangWriterMode mode) = 0;
};
#define SLANG_UUID_ISlangWriter ISlangWriter::getTypeGuid()
struct ISlangProfiler : public ISlangUnknown
{
SLANG_COM_INTERFACE(
0x197772c7,
0x0155,
0x4b91,
{0x84, 0xe8, 0x66, 0x68, 0xba, 0xff, 0x06, 0x19})
virtual SLANG_NO_THROW size_t SLANG_MCALL getEntryCount() = 0;
virtual SLANG_NO_THROW const char* SLANG_MCALL getEntryName(uint32_t index) = 0;
virtual SLANG_NO_THROW long SLANG_MCALL getEntryTimeMS(uint32_t index) = 0;
virtual SLANG_NO_THROW uint32_t SLANG_MCALL getEntryInvocationTimes(uint32_t index) = 0;
};
#define SLANG_UUID_ISlangProfiler ISlangProfiler::getTypeGuid()
namespace slang
{
struct IGlobalSession;
struct ICompileRequest;
}
typedef slang::IGlobalSession SlangSession;
typedef struct SlangProgramLayout SlangProgramLayout;
typedef struct slang::ICompileRequest SlangCompileRequest;
typedef void (*SlangDiagnosticCallback)(char const* message, void* userData);
SLANG_API const char* spGetBuildTagString();
typedef struct SlangProgramLayout SlangProgramLayout;
typedef struct SlangEntryPoint SlangEntryPoint;
typedef struct SlangEntryPointLayout SlangEntryPointLayout;
typedef struct SlangReflectionDecl SlangReflectionDecl;
typedef struct SlangReflectionModifier SlangReflectionModifier;
typedef struct SlangReflectionType SlangReflectionType;
typedef struct SlangReflectionTypeLayout SlangReflectionTypeLayout;
typedef struct SlangReflectionVariable SlangReflectionVariable;
typedef struct SlangReflectionVariableLayout SlangReflectionVariableLayout;
typedef struct SlangReflectionTypeParameter SlangReflectionTypeParameter;
typedef struct SlangReflectionUserAttribute SlangReflectionUserAttribute;
typedef SlangReflectionUserAttribute SlangReflectionAttribute;
typedef struct SlangReflectionFunction SlangReflectionFunction;
typedef struct SlangReflectionGeneric SlangReflectionGeneric;
union SlangReflectionGenericArg
{
SlangReflectionType* typeVal;
int64_t intVal;
bool boolVal;
};
enum SlangReflectionGenericArgType
{
SLANG_GENERIC_ARG_TYPE = 0,
SLANG_GENERIC_ARG_INT = 1,
SLANG_GENERIC_ARG_BOOL = 2
};
typedef SlangProgramLayout SlangReflection;
typedef SlangEntryPointLayout SlangReflectionEntryPoint;
typedef unsigned int SlangTypeKindIntegral;
enum SlangTypeKind : SlangTypeKindIntegral
{
SLANG_TYPE_KIND_NONE,
SLANG_TYPE_KIND_STRUCT,
SLANG_TYPE_KIND_ARRAY,
SLANG_TYPE_KIND_MATRIX,
SLANG_TYPE_KIND_VECTOR,
SLANG_TYPE_KIND_SCALAR,
SLANG_TYPE_KIND_CONSTANT_BUFFER,
SLANG_TYPE_KIND_RESOURCE,
SLANG_TYPE_KIND_SAMPLER_STATE,
SLANG_TYPE_KIND_TEXTURE_BUFFER,
SLANG_TYPE_KIND_SHADER_STORAGE_BUFFER,
SLANG_TYPE_KIND_PARAMETER_BLOCK,
SLANG_TYPE_KIND_GENERIC_TYPE_PARAMETER,
SLANG_TYPE_KIND_INTERFACE,
SLANG_TYPE_KIND_OUTPUT_STREAM,
SLANG_TYPE_KIND_MESH_OUTPUT,
SLANG_TYPE_KIND_SPECIALIZED,
SLANG_TYPE_KIND_FEEDBACK,
SLANG_TYPE_KIND_POINTER,
SLANG_TYPE_KIND_DYNAMIC_RESOURCE,
SLANG_TYPE_KIND_COUNT,
};
typedef unsigned int SlangScalarTypeIntegral;
enum SlangScalarType : SlangScalarTypeIntegral
{
SLANG_SCALAR_TYPE_NONE,
SLANG_SCALAR_TYPE_VOID,
SLANG_SCALAR_TYPE_BOOL,
SLANG_SCALAR_TYPE_INT32,
SLANG_SCALAR_TYPE_UINT32,
SLANG_SCALAR_TYPE_INT64,
SLANG_SCALAR_TYPE_UINT64,
SLANG_SCALAR_TYPE_FLOAT16,
SLANG_SCALAR_TYPE_FLOAT32,
SLANG_SCALAR_TYPE_FLOAT64,
SLANG_SCALAR_TYPE_INT8,
SLANG_SCALAR_TYPE_UINT8,
SLANG_SCALAR_TYPE_INT16,
SLANG_SCALAR_TYPE_UINT16,
SLANG_SCALAR_TYPE_INTPTR,
SLANG_SCALAR_TYPE_UINTPTR
};
typedef unsigned int SlangDeclKindIntegral;
enum SlangDeclKind : SlangDeclKindIntegral
{
SLANG_DECL_KIND_UNSUPPORTED_FOR_REFLECTION,
SLANG_DECL_KIND_STRUCT,
SLANG_DECL_KIND_FUNC,
SLANG_DECL_KIND_MODULE,
SLANG_DECL_KIND_GENERIC,
SLANG_DECL_KIND_VARIABLE,
SLANG_DECL_KIND_NAMESPACE
};
#ifndef SLANG_RESOURCE_SHAPE
#define SLANG_RESOURCE_SHAPE
typedef unsigned int SlangResourceShapeIntegral;
enum SlangResourceShape : SlangResourceShapeIntegral
{
SLANG_RESOURCE_BASE_SHAPE_MASK = 0x0F,
SLANG_RESOURCE_NONE = 0x00,
SLANG_TEXTURE_1D = 0x01,
SLANG_TEXTURE_2D = 0x02,
SLANG_TEXTURE_3D = 0x03,
SLANG_TEXTURE_CUBE = 0x04,
SLANG_TEXTURE_BUFFER = 0x05,
SLANG_STRUCTURED_BUFFER = 0x06,
SLANG_BYTE_ADDRESS_BUFFER = 0x07,
SLANG_RESOURCE_UNKNOWN = 0x08,
SLANG_ACCELERATION_STRUCTURE = 0x09,
SLANG_TEXTURE_SUBPASS = 0x0A,
SLANG_RESOURCE_EXT_SHAPE_MASK = 0xF0,
SLANG_TEXTURE_FEEDBACK_FLAG = 0x10,
SLANG_TEXTURE_SHADOW_FLAG = 0x20,
SLANG_TEXTURE_ARRAY_FLAG = 0x40,
SLANG_TEXTURE_MULTISAMPLE_FLAG = 0x80,
SLANG_TEXTURE_1D_ARRAY = SLANG_TEXTURE_1D | SLANG_TEXTURE_ARRAY_FLAG,
SLANG_TEXTURE_2D_ARRAY = SLANG_TEXTURE_2D | SLANG_TEXTURE_ARRAY_FLAG,
SLANG_TEXTURE_CUBE_ARRAY = SLANG_TEXTURE_CUBE | SLANG_TEXTURE_ARRAY_FLAG,
SLANG_TEXTURE_2D_MULTISAMPLE = SLANG_TEXTURE_2D | SLANG_TEXTURE_MULTISAMPLE_FLAG,
SLANG_TEXTURE_2D_MULTISAMPLE_ARRAY =
SLANG_TEXTURE_2D | SLANG_TEXTURE_MULTISAMPLE_FLAG | SLANG_TEXTURE_ARRAY_FLAG,
SLANG_TEXTURE_SUBPASS_MULTISAMPLE = SLANG_TEXTURE_SUBPASS | SLANG_TEXTURE_MULTISAMPLE_FLAG,
};
#endif
typedef unsigned int SlangResourceAccessIntegral;
enum SlangResourceAccess : SlangResourceAccessIntegral
{
SLANG_RESOURCE_ACCESS_NONE,
SLANG_RESOURCE_ACCESS_READ,
SLANG_RESOURCE_ACCESS_READ_WRITE,
SLANG_RESOURCE_ACCESS_RASTER_ORDERED,
SLANG_RESOURCE_ACCESS_APPEND,
SLANG_RESOURCE_ACCESS_CONSUME,
SLANG_RESOURCE_ACCESS_WRITE,
SLANG_RESOURCE_ACCESS_FEEDBACK,
SLANG_RESOURCE_ACCESS_UNKNOWN = 0x7FFFFFFF,
};
typedef unsigned int SlangParameterCategoryIntegral;
enum SlangParameterCategory : SlangParameterCategoryIntegral
{
SLANG_PARAMETER_CATEGORY_NONE,
SLANG_PARAMETER_CATEGORY_MIXED,
SLANG_PARAMETER_CATEGORY_CONSTANT_BUFFER,
SLANG_PARAMETER_CATEGORY_SHADER_RESOURCE,
SLANG_PARAMETER_CATEGORY_UNORDERED_ACCESS,
SLANG_PARAMETER_CATEGORY_VARYING_INPUT,
SLANG_PARAMETER_CATEGORY_VARYING_OUTPUT,
SLANG_PARAMETER_CATEGORY_SAMPLER_STATE,
SLANG_PARAMETER_CATEGORY_UNIFORM,
SLANG_PARAMETER_CATEGORY_DESCRIPTOR_TABLE_SLOT,
SLANG_PARAMETER_CATEGORY_SPECIALIZATION_CONSTANT,
SLANG_PARAMETER_CATEGORY_PUSH_CONSTANT_BUFFER,
SLANG_PARAMETER_CATEGORY_REGISTER_SPACE,
SLANG_PARAMETER_CATEGORY_GENERIC,
SLANG_PARAMETER_CATEGORY_RAY_PAYLOAD,
SLANG_PARAMETER_CATEGORY_HIT_ATTRIBUTES,
SLANG_PARAMETER_CATEGORY_CALLABLE_PAYLOAD,
SLANG_PARAMETER_CATEGORY_SHADER_RECORD,
SLANG_PARAMETER_CATEGORY_EXISTENTIAL_TYPE_PARAM,
SLANG_PARAMETER_CATEGORY_EXISTENTIAL_OBJECT_PARAM,
SLANG_PARAMETER_CATEGORY_SUB_ELEMENT_REGISTER_SPACE,
SLANG_PARAMETER_CATEGORY_SUBPASS,
SLANG_PARAMETER_CATEGORY_METAL_ARGUMENT_BUFFER_ELEMENT,
SLANG_PARAMETER_CATEGORY_METAL_ATTRIBUTE,
SLANG_PARAMETER_CATEGORY_METAL_PAYLOAD,
SLANG_PARAMETER_CATEGORY_COUNT,
SLANG_PARAMETER_CATEGORY_METAL_BUFFER = SLANG_PARAMETER_CATEGORY_CONSTANT_BUFFER,
SLANG_PARAMETER_CATEGORY_METAL_TEXTURE = SLANG_PARAMETER_CATEGORY_SHADER_RESOURCE,
SLANG_PARAMETER_CATEGORY_METAL_SAMPLER = SLANG_PARAMETER_CATEGORY_SAMPLER_STATE,
SLANG_PARAMETER_CATEGORY_VERTEX_INPUT = SLANG_PARAMETER_CATEGORY_VARYING_INPUT,
SLANG_PARAMETER_CATEGORY_FRAGMENT_OUTPUT = SLANG_PARAMETER_CATEGORY_VARYING_OUTPUT,
SLANG_PARAMETER_CATEGORY_COUNT_V1 = SLANG_PARAMETER_CATEGORY_SUBPASS,
};
typedef SlangUInt32 SlangBindingTypeIntegral;
enum SlangBindingType : SlangBindingTypeIntegral
{
SLANG_BINDING_TYPE_UNKNOWN = 0,
SLANG_BINDING_TYPE_SAMPLER,
SLANG_BINDING_TYPE_TEXTURE,
SLANG_BINDING_TYPE_CONSTANT_BUFFER,
SLANG_BINDING_TYPE_PARAMETER_BLOCK,
SLANG_BINDING_TYPE_TYPED_BUFFER,
SLANG_BINDING_TYPE_RAW_BUFFER,
SLANG_BINDING_TYPE_COMBINED_TEXTURE_SAMPLER,
SLANG_BINDING_TYPE_INPUT_RENDER_TARGET,
SLANG_BINDING_TYPE_INLINE_UNIFORM_DATA,
SLANG_BINDING_TYPE_RAY_TRACING_ACCELERATION_STRUCTURE,
SLANG_BINDING_TYPE_VARYING_INPUT,
SLANG_BINDING_TYPE_VARYING_OUTPUT,
SLANG_BINDING_TYPE_EXISTENTIAL_VALUE,
SLANG_BINDING_TYPE_PUSH_CONSTANT,
SLANG_BINDING_TYPE_MUTABLE_FLAG = 0x100,
SLANG_BINDING_TYPE_MUTABLE_TETURE =
SLANG_BINDING_TYPE_TEXTURE | SLANG_BINDING_TYPE_MUTABLE_FLAG,
SLANG_BINDING_TYPE_MUTABLE_TYPED_BUFFER =
SLANG_BINDING_TYPE_TYPED_BUFFER | SLANG_BINDING_TYPE_MUTABLE_FLAG,
SLANG_BINDING_TYPE_MUTABLE_RAW_BUFFER =
SLANG_BINDING_TYPE_RAW_BUFFER | SLANG_BINDING_TYPE_MUTABLE_FLAG,
SLANG_BINDING_TYPE_BASE_MASK = 0x00FF,
SLANG_BINDING_TYPE_EXT_MASK = 0xFF00,
};
typedef SlangUInt32 SlangLayoutRulesIntegral;
enum SlangLayoutRules : SlangLayoutRulesIntegral
{
SLANG_LAYOUT_RULES_DEFAULT,
SLANG_LAYOUT_RULES_METAL_ARGUMENT_BUFFER_TIER_2,
};
typedef SlangUInt32 SlangModifierIDIntegral;
enum SlangModifierID : SlangModifierIDIntegral
{
SLANG_MODIFIER_SHARED,
SLANG_MODIFIER_NO_DIFF,
SLANG_MODIFIER_STATIC,
SLANG_MODIFIER_CONST,
SLANG_MODIFIER_EXPORT,
SLANG_MODIFIER_EXTERN,
SLANG_MODIFIER_DIFFERENTIABLE,
SLANG_MODIFIER_MUTATING,
SLANG_MODIFIER_IN,
SLANG_MODIFIER_OUT,
SLANG_MODIFIER_INOUT
};
typedef SlangUInt32 SlangImageFormatIntegral;
enum SlangImageFormat : SlangImageFormatIntegral
{
#define SLANG_FORMAT(NAME, DESC) SLANG_IMAGE_FORMAT_##NAME,
#include "slang-image-format-defs.h"
#undef SLANG_FORMAT
};
#define SLANG_UNBOUNDED_SIZE (~size_t(0))
typedef SlangReflectionVariableLayout SlangReflectionParameter;
#ifdef __cplusplus
}
#endif
#ifdef __cplusplus
namespace slang
{
struct ISession;
}
#endif
#include "slang-deprecated.h"
#ifdef __cplusplus
namespace slang
{
struct BufferReflection;
struct DeclReflection;
struct TypeLayoutReflection;
struct TypeReflection;
struct VariableLayoutReflection;
struct VariableReflection;
struct FunctionReflection;
struct GenericReflection;
union GenericArgReflection
{
TypeReflection* typeVal;
int64_t intVal;
bool boolVal;
};
struct Attribute
{
char const* getName()
{
return spReflectionUserAttribute_GetName((SlangReflectionAttribute*)this);
}
uint32_t getArgumentCount()
{
return (uint32_t)spReflectionUserAttribute_GetArgumentCount(
(SlangReflectionAttribute*)this);
}
TypeReflection* getArgumentType(uint32_t index)
{
return (TypeReflection*)spReflectionUserAttribute_GetArgumentType(
(SlangReflectionAttribute*)this,
index);
}
SlangResult getArgumentValueInt(uint32_t index, int* value)
{
return spReflectionUserAttribute_GetArgumentValueInt(
(SlangReflectionAttribute*)this,
index,
value);
}
SlangResult getArgumentValueFloat(uint32_t index, float* value)
{
return spReflectionUserAttribute_GetArgumentValueFloat(
(SlangReflectionAttribute*)this,
index,
value);
}
const char* getArgumentValueString(uint32_t index, size_t* outSize)
{
return spReflectionUserAttribute_GetArgumentValueString(
(SlangReflectionAttribute*)this,
index,
outSize);
}
};
typedef Attribute UserAttribute;
struct TypeReflection
{
enum class Kind
{
None = SLANG_TYPE_KIND_NONE,
Struct = SLANG_TYPE_KIND_STRUCT,
Array = SLANG_TYPE_KIND_ARRAY,
Matrix = SLANG_TYPE_KIND_MATRIX,
Vector = SLANG_TYPE_KIND_VECTOR,
Scalar = SLANG_TYPE_KIND_SCALAR,
ConstantBuffer = SLANG_TYPE_KIND_CONSTANT_BUFFER,
Resource = SLANG_TYPE_KIND_RESOURCE,
SamplerState = SLANG_TYPE_KIND_SAMPLER_STATE,
TextureBuffer = SLANG_TYPE_KIND_TEXTURE_BUFFER,
ShaderStorageBuffer = SLANG_TYPE_KIND_SHADER_STORAGE_BUFFER,
ParameterBlock = SLANG_TYPE_KIND_PARAMETER_BLOCK,
GenericTypeParameter = SLANG_TYPE_KIND_GENERIC_TYPE_PARAMETER,
Interface = SLANG_TYPE_KIND_INTERFACE,
OutputStream = SLANG_TYPE_KIND_OUTPUT_STREAM,
Specialized = SLANG_TYPE_KIND_SPECIALIZED,
Feedback = SLANG_TYPE_KIND_FEEDBACK,
Pointer = SLANG_TYPE_KIND_POINTER,
DynamicResource = SLANG_TYPE_KIND_DYNAMIC_RESOURCE,
};
enum ScalarType : SlangScalarTypeIntegral
{
None = SLANG_SCALAR_TYPE_NONE,
Void = SLANG_SCALAR_TYPE_VOID,
Bool = SLANG_SCALAR_TYPE_BOOL,
Int32 = SLANG_SCALAR_TYPE_INT32,
UInt32 = SLANG_SCALAR_TYPE_UINT32,
Int64 = SLANG_SCALAR_TYPE_INT64,
UInt64 = SLANG_SCALAR_TYPE_UINT64,
Float16 = SLANG_SCALAR_TYPE_FLOAT16,
Float32 = SLANG_SCALAR_TYPE_FLOAT32,
Float64 = SLANG_SCALAR_TYPE_FLOAT64,
Int8 = SLANG_SCALAR_TYPE_INT8,
UInt8 = SLANG_SCALAR_TYPE_UINT8,
Int16 = SLANG_SCALAR_TYPE_INT16,
UInt16 = SLANG_SCALAR_TYPE_UINT16,
};
Kind getKind() { return (Kind)spReflectionType_GetKind((SlangReflectionType*)this); }
unsigned int getFieldCount()
{
return spReflectionType_GetFieldCount((SlangReflectionType*)this);
}
VariableReflection* getFieldByIndex(unsigned int index)
{
return (
VariableReflection*)spReflectionType_GetFieldByIndex((SlangReflectionType*)this, index);
}
bool isArray() { return getKind() == TypeReflection::Kind::Array; }
TypeReflection* unwrapArray()
{
TypeReflection* type = this;
while (type->isArray())
{
type = type->getElementType();
}
return type;
}
size_t getElementCount()
{
return spReflectionType_GetElementCount((SlangReflectionType*)this);
}
size_t getTotalArrayElementCount()
{
if (!isArray())
return 0;
size_t result = 1;
TypeReflection* type = this;
for (;;)
{
if (!type->isArray())
return result;
result *= type->getElementCount();
type = type->getElementType();
}
}
TypeReflection* getElementType()
{
return (TypeReflection*)spReflectionType_GetElementType((SlangReflectionType*)this);
}
unsigned getRowCount() { return spReflectionType_GetRowCount((SlangReflectionType*)this); }
unsigned getColumnCount()
{
return spReflectionType_GetColumnCount((SlangReflectionType*)this);
}
ScalarType getScalarType()
{
return (ScalarType)spReflectionType_GetScalarType((SlangReflectionType*)this);
}
TypeReflection* getResourceResultType()
{
return (TypeReflection*)spReflectionType_GetResourceResultType((SlangReflectionType*)this);
}
SlangResourceShape getResourceShape()
{
return spReflectionType_GetResourceShape((SlangReflectionType*)this);
}
SlangResourceAccess getResourceAccess()
{
return spReflectionType_GetResourceAccess((SlangReflectionType*)this);
}
char const* getName() { return spReflectionType_GetName((SlangReflectionType*)this); }
SlangResult getFullName(ISlangBlob** outNameBlob)
{
return spReflectionType_GetFullName((SlangReflectionType*)this, outNameBlob);
}
unsigned int getUserAttributeCount()
{
return spReflectionType_GetUserAttributeCount((SlangReflectionType*)this);
}
UserAttribute* getUserAttributeByIndex(unsigned int index)
{
return (UserAttribute*)spReflectionType_GetUserAttribute((SlangReflectionType*)this, index);
}
UserAttribute* findAttributeByName(char const* name)
{
return (UserAttribute*)spReflectionType_FindUserAttributeByName(
(SlangReflectionType*)this,
name);
}
UserAttribute* findUserAttributeByName(char const* name) { return findAttributeByName(name); }
TypeReflection* applySpecializations(GenericReflection* generic)
{
return (TypeReflection*)spReflectionType_applySpecializations(
(SlangReflectionType*)this,
(SlangReflectionGeneric*)generic);
}
GenericReflection* getGenericContainer()
{
return (GenericReflection*)spReflectionType_GetGenericContainer((SlangReflectionType*)this);
}
};
enum ParameterCategory : SlangParameterCategoryIntegral
{
None = SLANG_PARAMETER_CATEGORY_NONE,
Mixed = SLANG_PARAMETER_CATEGORY_MIXED,
ConstantBuffer = SLANG_PARAMETER_CATEGORY_CONSTANT_BUFFER,
ShaderResource = SLANG_PARAMETER_CATEGORY_SHADER_RESOURCE,
UnorderedAccess = SLANG_PARAMETER_CATEGORY_UNORDERED_ACCESS,
VaryingInput = SLANG_PARAMETER_CATEGORY_VARYING_INPUT,
VaryingOutput = SLANG_PARAMETER_CATEGORY_VARYING_OUTPUT,
SamplerState = SLANG_PARAMETER_CATEGORY_SAMPLER_STATE,
Uniform = SLANG_PARAMETER_CATEGORY_UNIFORM,
DescriptorTableSlot = SLANG_PARAMETER_CATEGORY_DESCRIPTOR_TABLE_SLOT,
SpecializationConstant = SLANG_PARAMETER_CATEGORY_SPECIALIZATION_CONSTANT,
PushConstantBuffer = SLANG_PARAMETER_CATEGORY_PUSH_CONSTANT_BUFFER,
RegisterSpace = SLANG_PARAMETER_CATEGORY_REGISTER_SPACE,
GenericResource = SLANG_PARAMETER_CATEGORY_GENERIC,
RayPayload = SLANG_PARAMETER_CATEGORY_RAY_PAYLOAD,
HitAttributes = SLANG_PARAMETER_CATEGORY_HIT_ATTRIBUTES,
CallablePayload = SLANG_PARAMETER_CATEGORY_CALLABLE_PAYLOAD,
ShaderRecord = SLANG_PARAMETER_CATEGORY_SHADER_RECORD,
ExistentialTypeParam = SLANG_PARAMETER_CATEGORY_EXISTENTIAL_TYPE_PARAM,
ExistentialObjectParam = SLANG_PARAMETER_CATEGORY_EXISTENTIAL_OBJECT_PARAM,
SubElementRegisterSpace = SLANG_PARAMETER_CATEGORY_SUB_ELEMENT_REGISTER_SPACE,
InputAttachmentIndex = SLANG_PARAMETER_CATEGORY_SUBPASS,
MetalBuffer = SLANG_PARAMETER_CATEGORY_CONSTANT_BUFFER,
MetalTexture = SLANG_PARAMETER_CATEGORY_METAL_TEXTURE,
MetalArgumentBufferElement = SLANG_PARAMETER_CATEGORY_METAL_ARGUMENT_BUFFER_ELEMENT,
MetalAttribute = SLANG_PARAMETER_CATEGORY_METAL_ATTRIBUTE,
MetalPayload = SLANG_PARAMETER_CATEGORY_METAL_PAYLOAD,
VertexInput = SLANG_PARAMETER_CATEGORY_VERTEX_INPUT,
FragmentOutput = SLANG_PARAMETER_CATEGORY_FRAGMENT_OUTPUT,
};
enum class BindingType : SlangBindingTypeIntegral
{
Unknown = SLANG_BINDING_TYPE_UNKNOWN,
Sampler = SLANG_BINDING_TYPE_SAMPLER,
Texture = SLANG_BINDING_TYPE_TEXTURE,
ConstantBuffer = SLANG_BINDING_TYPE_CONSTANT_BUFFER,
ParameterBlock = SLANG_BINDING_TYPE_PARAMETER_BLOCK,
TypedBuffer = SLANG_BINDING_TYPE_TYPED_BUFFER,
RawBuffer = SLANG_BINDING_TYPE_RAW_BUFFER,
CombinedTextureSampler = SLANG_BINDING_TYPE_COMBINED_TEXTURE_SAMPLER,
InputRenderTarget = SLANG_BINDING_TYPE_INPUT_RENDER_TARGET,
InlineUniformData = SLANG_BINDING_TYPE_INLINE_UNIFORM_DATA,
RayTracingAccelerationStructure = SLANG_BINDING_TYPE_RAY_TRACING_ACCELERATION_STRUCTURE,
VaryingInput = SLANG_BINDING_TYPE_VARYING_INPUT,
VaryingOutput = SLANG_BINDING_TYPE_VARYING_OUTPUT,
ExistentialValue = SLANG_BINDING_TYPE_EXISTENTIAL_VALUE,
PushConstant = SLANG_BINDING_TYPE_PUSH_CONSTANT,
MutableFlag = SLANG_BINDING_TYPE_MUTABLE_FLAG,
MutableTexture = SLANG_BINDING_TYPE_MUTABLE_TETURE,
MutableTypedBuffer = SLANG_BINDING_TYPE_MUTABLE_TYPED_BUFFER,
MutableRawBuffer = SLANG_BINDING_TYPE_MUTABLE_RAW_BUFFER,
BaseMask = SLANG_BINDING_TYPE_BASE_MASK,
ExtMask = SLANG_BINDING_TYPE_EXT_MASK,
};
struct TypeLayoutReflection
{
TypeReflection* getType()
{
return (TypeReflection*)spReflectionTypeLayout_GetType((SlangReflectionTypeLayout*)this);
}
TypeReflection::Kind getKind()
{
return (TypeReflection::Kind)spReflectionTypeLayout_getKind(
(SlangReflectionTypeLayout*)this);
}
size_t getSize(SlangParameterCategory category)
{
return spReflectionTypeLayout_GetSize((SlangReflectionTypeLayout*)this, category);
}
size_t getStride(SlangParameterCategory category)
{
return spReflectionTypeLayout_GetStride((SlangReflectionTypeLayout*)this, category);
}
int32_t getAlignment(SlangParameterCategory category)
{
return spReflectionTypeLayout_getAlignment((SlangReflectionTypeLayout*)this, category);
}
size_t getSize(slang::ParameterCategory category = slang::ParameterCategory::Uniform)
{
return spReflectionTypeLayout_GetSize(
(SlangReflectionTypeLayout*)this,
(SlangParameterCategory)category);
}
size_t getStride(slang::ParameterCategory category = slang::ParameterCategory::Uniform)
{
return spReflectionTypeLayout_GetStride(
(SlangReflectionTypeLayout*)this,
(SlangParameterCategory)category);
}
int32_t getAlignment(slang::ParameterCategory category = slang::ParameterCategory::Uniform)
{
return spReflectionTypeLayout_getAlignment(
(SlangReflectionTypeLayout*)this,
(SlangParameterCategory)category);
}
unsigned int getFieldCount()
{
return spReflectionTypeLayout_GetFieldCount((SlangReflectionTypeLayout*)this);
}
VariableLayoutReflection* getFieldByIndex(unsigned int index)
{
return (VariableLayoutReflection*)spReflectionTypeLayout_GetFieldByIndex(
(SlangReflectionTypeLayout*)this,
index);
}
SlangInt findFieldIndexByName(char const* nameBegin, char const* nameEnd = nullptr)
{
return spReflectionTypeLayout_findFieldIndexByName(
(SlangReflectionTypeLayout*)this,
nameBegin,
nameEnd);
}
VariableLayoutReflection* getExplicitCounter()
{
return (VariableLayoutReflection*)spReflectionTypeLayout_GetExplicitCounter(
(SlangReflectionTypeLayout*)this);
}
bool isArray() { return getType()->isArray(); }
TypeLayoutReflection* unwrapArray()
{
TypeLayoutReflection* typeLayout = this;
while (typeLayout->isArray())
{
typeLayout = typeLayout->getElementTypeLayout();
}
return typeLayout;
}
size_t getElementCount() { return getType()->getElementCount(); }
size_t getTotalArrayElementCount() { return getType()->getTotalArrayElementCount(); }
size_t getElementStride(SlangParameterCategory category)
{
return spReflectionTypeLayout_GetElementStride((SlangReflectionTypeLayout*)this, category);
}
TypeLayoutReflection* getElementTypeLayout()
{
return (TypeLayoutReflection*)spReflectionTypeLayout_GetElementTypeLayout(
(SlangReflectionTypeLayout*)this);
}
VariableLayoutReflection* getElementVarLayout()
{
return (VariableLayoutReflection*)spReflectionTypeLayout_GetElementVarLayout(
(SlangReflectionTypeLayout*)this);
}
VariableLayoutReflection* getContainerVarLayout()
{
return (VariableLayoutReflection*)spReflectionTypeLayout_getContainerVarLayout(
(SlangReflectionTypeLayout*)this);
}
ParameterCategory getParameterCategory()
{
return (ParameterCategory)spReflectionTypeLayout_GetParameterCategory(
(SlangReflectionTypeLayout*)this);
}
unsigned int getCategoryCount()
{
return spReflectionTypeLayout_GetCategoryCount((SlangReflectionTypeLayout*)this);
}
ParameterCategory getCategoryByIndex(unsigned int index)
{
return (ParameterCategory)spReflectionTypeLayout_GetCategoryByIndex(
(SlangReflectionTypeLayout*)this,
index);
}
unsigned getRowCount() { return getType()->getRowCount(); }
unsigned getColumnCount() { return getType()->getColumnCount(); }
TypeReflection::ScalarType getScalarType() { return getType()->getScalarType(); }
TypeReflection* getResourceResultType() { return getType()->getResourceResultType(); }
SlangResourceShape getResourceShape() { return getType()->getResourceShape(); }
SlangResourceAccess getResourceAccess() { return getType()->getResourceAccess(); }
char const* getName() { return getType()->getName(); }
SlangMatrixLayoutMode getMatrixLayoutMode()
{
return spReflectionTypeLayout_GetMatrixLayoutMode((SlangReflectionTypeLayout*)this);
}
int getGenericParamIndex()
{
return spReflectionTypeLayout_getGenericParamIndex((SlangReflectionTypeLayout*)this);
}
TypeLayoutReflection* getPendingDataTypeLayout()
{
return (TypeLayoutReflection*)spReflectionTypeLayout_getPendingDataTypeLayout(
(SlangReflectionTypeLayout*)this);
}
VariableLayoutReflection* getSpecializedTypePendingDataVarLayout()
{
return (VariableLayoutReflection*)
spReflectionTypeLayout_getSpecializedTypePendingDataVarLayout(
(SlangReflectionTypeLayout*)this);
}
SlangInt getBindingRangeCount()
{
return spReflectionTypeLayout_getBindingRangeCount((SlangReflectionTypeLayout*)this);
}
BindingType getBindingRangeType(SlangInt index)
{
return (BindingType)spReflectionTypeLayout_getBindingRangeType(
(SlangReflectionTypeLayout*)this,
index);
}
bool isBindingRangeSpecializable(SlangInt index)
{
return (bool)spReflectionTypeLayout_isBindingRangeSpecializable(
(SlangReflectionTypeLayout*)this,
index);
}
SlangInt getBindingRangeBindingCount(SlangInt index)
{
return spReflectionTypeLayout_getBindingRangeBindingCount(
(SlangReflectionTypeLayout*)this,
index);
}
SlangInt getFieldBindingRangeOffset(SlangInt fieldIndex)
{
return spReflectionTypeLayout_getFieldBindingRangeOffset(
(SlangReflectionTypeLayout*)this,
fieldIndex);
}
SlangInt getExplicitCounterBindingRangeOffset()
{
return spReflectionTypeLayout_getExplicitCounterBindingRangeOffset(
(SlangReflectionTypeLayout*)this);
}
TypeLayoutReflection* getBindingRangeLeafTypeLayout(SlangInt index)
{
return (TypeLayoutReflection*)spReflectionTypeLayout_getBindingRangeLeafTypeLayout(
(SlangReflectionTypeLayout*)this,
index);
}
VariableReflection* getBindingRangeLeafVariable(SlangInt index)
{
return (VariableReflection*)spReflectionTypeLayout_getBindingRangeLeafVariable(
(SlangReflectionTypeLayout*)this,
index);
}
SlangImageFormat getBindingRangeImageFormat(SlangInt index)
{
return spReflectionTypeLayout_getBindingRangeImageFormat(
(SlangReflectionTypeLayout*)this,
index);
}
SlangInt getBindingRangeDescriptorSetIndex(SlangInt index)
{
return spReflectionTypeLayout_getBindingRangeDescriptorSetIndex(
(SlangReflectionTypeLayout*)this,
index);
}
SlangInt getBindingRangeFirstDescriptorRangeIndex(SlangInt index)
{
return spReflectionTypeLayout_getBindingRangeFirstDescriptorRangeIndex(
(SlangReflectionTypeLayout*)this,
index);
}
SlangInt getBindingRangeDescriptorRangeCount(SlangInt index)
{
return spReflectionTypeLayout_getBindingRangeDescriptorRangeCount(
(SlangReflectionTypeLayout*)this,
index);
}
SlangInt getDescriptorSetCount()
{
return spReflectionTypeLayout_getDescriptorSetCount((SlangReflectionTypeLayout*)this);
}
SlangInt getDescriptorSetSpaceOffset(SlangInt setIndex)
{
return spReflectionTypeLayout_getDescriptorSetSpaceOffset(
(SlangReflectionTypeLayout*)this,
setIndex);
}
SlangInt getDescriptorSetDescriptorRangeCount(SlangInt setIndex)
{
return spReflectionTypeLayout_getDescriptorSetDescriptorRangeCount(
(SlangReflectionTypeLayout*)this,
setIndex);
}
SlangInt getDescriptorSetDescriptorRangeIndexOffset(SlangInt setIndex, SlangInt rangeIndex)
{
return spReflectionTypeLayout_getDescriptorSetDescriptorRangeIndexOffset(
(SlangReflectionTypeLayout*)this,
setIndex,
rangeIndex);
}
SlangInt getDescriptorSetDescriptorRangeDescriptorCount(SlangInt setIndex, SlangInt rangeIndex)
{
return spReflectionTypeLayout_getDescriptorSetDescriptorRangeDescriptorCount(
(SlangReflectionTypeLayout*)this,
setIndex,
rangeIndex);
}
BindingType getDescriptorSetDescriptorRangeType(SlangInt setIndex, SlangInt rangeIndex)
{
return (BindingType)spReflectionTypeLayout_getDescriptorSetDescriptorRangeType(
(SlangReflectionTypeLayout*)this,
setIndex,
rangeIndex);
}
ParameterCategory getDescriptorSetDescriptorRangeCategory(
SlangInt setIndex,
SlangInt rangeIndex)
{
return (ParameterCategory)spReflectionTypeLayout_getDescriptorSetDescriptorRangeCategory(
(SlangReflectionTypeLayout*)this,
setIndex,
rangeIndex);
}
SlangInt getSubObjectRangeCount()
{
return spReflectionTypeLayout_getSubObjectRangeCount((SlangReflectionTypeLayout*)this);
}
SlangInt getSubObjectRangeBindingRangeIndex(SlangInt subObjectRangeIndex)
{
return spReflectionTypeLayout_getSubObjectRangeBindingRangeIndex(
(SlangReflectionTypeLayout*)this,
subObjectRangeIndex);
}
SlangInt getSubObjectRangeSpaceOffset(SlangInt subObjectRangeIndex)
{
return spReflectionTypeLayout_getSubObjectRangeSpaceOffset(
(SlangReflectionTypeLayout*)this,
subObjectRangeIndex);
}
VariableLayoutReflection* getSubObjectRangeOffset(SlangInt subObjectRangeIndex)
{
return (VariableLayoutReflection*)spReflectionTypeLayout_getSubObjectRangeOffset(
(SlangReflectionTypeLayout*)this,
subObjectRangeIndex);
}
};
struct Modifier
{
enum ID : SlangModifierIDIntegral
{
Shared = SLANG_MODIFIER_SHARED,
NoDiff = SLANG_MODIFIER_NO_DIFF,
Static = SLANG_MODIFIER_STATIC,
Const = SLANG_MODIFIER_CONST,
Export = SLANG_MODIFIER_EXPORT,
Extern = SLANG_MODIFIER_EXTERN,
Differentiable = SLANG_MODIFIER_DIFFERENTIABLE,
Mutating = SLANG_MODIFIER_MUTATING,
In = SLANG_MODIFIER_IN,
Out = SLANG_MODIFIER_OUT,
InOut = SLANG_MODIFIER_INOUT
};
};
struct VariableReflection
{
char const* getName() { return spReflectionVariable_GetName((SlangReflectionVariable*)this); }
TypeReflection* getType()
{
return (TypeReflection*)spReflectionVariable_GetType((SlangReflectionVariable*)this);
}
Modifier* findModifier(Modifier::ID id)
{
return (Modifier*)spReflectionVariable_FindModifier(
(SlangReflectionVariable*)this,
(SlangModifierID)id);
}
unsigned int getUserAttributeCount()
{
return spReflectionVariable_GetUserAttributeCount((SlangReflectionVariable*)this);
}
Attribute* getUserAttributeByIndex(unsigned int index)
{
return (UserAttribute*)spReflectionVariable_GetUserAttribute(
(SlangReflectionVariable*)this,
index);
}
Attribute* findAttributeByName(SlangSession* globalSession, char const* name)
{
return (UserAttribute*)spReflectionVariable_FindUserAttributeByName(
(SlangReflectionVariable*)this,
globalSession,
name);
}
Attribute* findUserAttributeByName(SlangSession* globalSession, char const* name)
{
return findAttributeByName(globalSession, name);
}
bool hasDefaultValue()
{
return spReflectionVariable_HasDefaultValue((SlangReflectionVariable*)this);
}
SlangResult getDefaultValueInt(int64_t* value)
{
return spReflectionVariable_GetDefaultValueInt((SlangReflectionVariable*)this, value);
}
GenericReflection* getGenericContainer()
{
return (GenericReflection*)spReflectionVariable_GetGenericContainer(
(SlangReflectionVariable*)this);
}
VariableReflection* applySpecializations(GenericReflection* generic)
{
return (VariableReflection*)spReflectionVariable_applySpecializations(
(SlangReflectionVariable*)this,
(SlangReflectionGeneric*)generic);
}
};
struct VariableLayoutReflection
{
VariableReflection* getVariable()
{
return (VariableReflection*)spReflectionVariableLayout_GetVariable(
(SlangReflectionVariableLayout*)this);
}
char const* getName() { return getVariable()->getName(); }
Modifier* findModifier(Modifier::ID id) { return getVariable()->findModifier(id); }
TypeLayoutReflection* getTypeLayout()
{
return (TypeLayoutReflection*)spReflectionVariableLayout_GetTypeLayout(
(SlangReflectionVariableLayout*)this);
}
ParameterCategory getCategory() { return getTypeLayout()->getParameterCategory(); }
unsigned int getCategoryCount() { return getTypeLayout()->getCategoryCount(); }
ParameterCategory getCategoryByIndex(unsigned int index)
{
return getTypeLayout()->getCategoryByIndex(index);
}
size_t getOffset(SlangParameterCategory category)
{
return spReflectionVariableLayout_GetOffset((SlangReflectionVariableLayout*)this, category);
}
size_t getOffset(slang::ParameterCategory category = slang::ParameterCategory::Uniform)
{
return spReflectionVariableLayout_GetOffset(
(SlangReflectionVariableLayout*)this,
(SlangParameterCategory)category);
}
TypeReflection* getType() { return getVariable()->getType(); }
unsigned getBindingIndex()
{
return spReflectionParameter_GetBindingIndex((SlangReflectionVariableLayout*)this);
}
unsigned getBindingSpace()
{
return spReflectionParameter_GetBindingSpace((SlangReflectionVariableLayout*)this);
}
size_t getBindingSpace(SlangParameterCategory category)
{
return spReflectionVariableLayout_GetSpace((SlangReflectionVariableLayout*)this, category);
}
size_t getBindingSpace(slang::ParameterCategory category)
{
return spReflectionVariableLayout_GetSpace(
(SlangReflectionVariableLayout*)this,
(SlangParameterCategory)category);
}
char const* getSemanticName()
{
return spReflectionVariableLayout_GetSemanticName((SlangReflectionVariableLayout*)this);
}
size_t getSemanticIndex()
{
return spReflectionVariableLayout_GetSemanticIndex((SlangReflectionVariableLayout*)this);
}
SlangStage getStage()
{
return spReflectionVariableLayout_getStage((SlangReflectionVariableLayout*)this);
}
VariableLayoutReflection* getPendingDataLayout()
{
return (VariableLayoutReflection*)spReflectionVariableLayout_getPendingDataLayout(
(SlangReflectionVariableLayout*)this);
}
};
struct FunctionReflection
{
char const* getName() { return spReflectionFunction_GetName((SlangReflectionFunction*)this); }
TypeReflection* getReturnType()
{
return (TypeReflection*)spReflectionFunction_GetResultType((SlangReflectionFunction*)this);
}
unsigned int getParameterCount()
{
return spReflectionFunction_GetParameterCount((SlangReflectionFunction*)this);
}
VariableReflection* getParameterByIndex(unsigned int index)
{
return (VariableReflection*)spReflectionFunction_GetParameter(
(SlangReflectionFunction*)this,
index);
}
unsigned int getUserAttributeCount()
{
return spReflectionFunction_GetUserAttributeCount((SlangReflectionFunction*)this);
}
Attribute* getUserAttributeByIndex(unsigned int index)
{
return (
Attribute*)spReflectionFunction_GetUserAttribute((SlangReflectionFunction*)this, index);
}
Attribute* findAttributeByName(SlangSession* globalSession, char const* name)
{
return (Attribute*)spReflectionFunction_FindUserAttributeByName(
(SlangReflectionFunction*)this,
globalSession,
name);
}
Attribute* findUserAttributeByName(SlangSession* globalSession, char const* name)
{
return findAttributeByName(globalSession, name);
}
Modifier* findModifier(Modifier::ID id)
{
return (Modifier*)spReflectionFunction_FindModifier(
(SlangReflectionFunction*)this,
(SlangModifierID)id);
}
GenericReflection* getGenericContainer()
{
return (GenericReflection*)spReflectionFunction_GetGenericContainer(
(SlangReflectionFunction*)this);
}
FunctionReflection* applySpecializations(GenericReflection* generic)
{
return (FunctionReflection*)spReflectionFunction_applySpecializations(
(SlangReflectionFunction*)this,
(SlangReflectionGeneric*)generic);
}
FunctionReflection* specializeWithArgTypes(unsigned int argCount, TypeReflection* const* types)
{
return (FunctionReflection*)spReflectionFunction_specializeWithArgTypes(
(SlangReflectionFunction*)this,
argCount,
(SlangReflectionType* const*)types);
}
bool isOverloaded()
{
return spReflectionFunction_isOverloaded((SlangReflectionFunction*)this);
}
unsigned int getOverloadCount()
{
return spReflectionFunction_getOverloadCount((SlangReflectionFunction*)this);
}
FunctionReflection* getOverload(unsigned int index)
{
return (FunctionReflection*)spReflectionFunction_getOverload(
(SlangReflectionFunction*)this,
index);
}
};
struct GenericReflection
{
DeclReflection* asDecl()
{
return (DeclReflection*)spReflectionGeneric_asDecl((SlangReflectionGeneric*)this);
}
char const* getName() { return spReflectionGeneric_GetName((SlangReflectionGeneric*)this); }
unsigned int getTypeParameterCount()
{
return spReflectionGeneric_GetTypeParameterCount((SlangReflectionGeneric*)this);
}
VariableReflection* getTypeParameter(unsigned index)
{
return (VariableReflection*)spReflectionGeneric_GetTypeParameter(
(SlangReflectionGeneric*)this,
index);
}
unsigned int getValueParameterCount()
{
return spReflectionGeneric_GetValueParameterCount((SlangReflectionGeneric*)this);
}
VariableReflection* getValueParameter(unsigned index)
{
return (VariableReflection*)spReflectionGeneric_GetValueParameter(
(SlangReflectionGeneric*)this,
index);
}
unsigned int getTypeParameterConstraintCount(VariableReflection* typeParam)
{
return spReflectionGeneric_GetTypeParameterConstraintCount(
(SlangReflectionGeneric*)this,
(SlangReflectionVariable*)typeParam);
}
TypeReflection* getTypeParameterConstraintType(VariableReflection* typeParam, unsigned index)
{
return (TypeReflection*)spReflectionGeneric_GetTypeParameterConstraintType(
(SlangReflectionGeneric*)this,
(SlangReflectionVariable*)typeParam,
index);
}
DeclReflection* getInnerDecl()
{
return (DeclReflection*)spReflectionGeneric_GetInnerDecl((SlangReflectionGeneric*)this);
}
SlangDeclKind getInnerKind()
{
return spReflectionGeneric_GetInnerKind((SlangReflectionGeneric*)this);
}
GenericReflection* getOuterGenericContainer()
{
return (GenericReflection*)spReflectionGeneric_GetOuterGenericContainer(
(SlangReflectionGeneric*)this);
}
TypeReflection* getConcreteType(VariableReflection* typeParam)
{
return (TypeReflection*)spReflectionGeneric_GetConcreteType(
(SlangReflectionGeneric*)this,
(SlangReflectionVariable*)typeParam);
}
int64_t getConcreteIntVal(VariableReflection* valueParam)
{
return spReflectionGeneric_GetConcreteIntVal(
(SlangReflectionGeneric*)this,
(SlangReflectionVariable*)valueParam);
}
GenericReflection* applySpecializations(GenericReflection* generic)
{
return (GenericReflection*)spReflectionGeneric_applySpecializations(
(SlangReflectionGeneric*)this,
(SlangReflectionGeneric*)generic);
}
};
struct EntryPointReflection
{
char const* getName()
{
return spReflectionEntryPoint_getName((SlangReflectionEntryPoint*)this);
}
char const* getNameOverride()
{
return spReflectionEntryPoint_getNameOverride((SlangReflectionEntryPoint*)this);
}
unsigned getParameterCount()
{
return spReflectionEntryPoint_getParameterCount((SlangReflectionEntryPoint*)this);
}
FunctionReflection* getFunction()
{
return (FunctionReflection*)spReflectionEntryPoint_getFunction(
(SlangReflectionEntryPoint*)this);
}
VariableLayoutReflection* getParameterByIndex(unsigned index)
{
return (VariableLayoutReflection*)spReflectionEntryPoint_getParameterByIndex(
(SlangReflectionEntryPoint*)this,
index);
}
SlangStage getStage()
{
return spReflectionEntryPoint_getStage((SlangReflectionEntryPoint*)this);
}
void getComputeThreadGroupSize(SlangUInt axisCount, SlangUInt* outSizeAlongAxis)
{
return spReflectionEntryPoint_getComputeThreadGroupSize(
(SlangReflectionEntryPoint*)this,
axisCount,
outSizeAlongAxis);
}
void getComputeWaveSize(SlangUInt* outWaveSize)
{
return spReflectionEntryPoint_getComputeWaveSize(
(SlangReflectionEntryPoint*)this,
outWaveSize);
}
bool usesAnySampleRateInput()
{
return 0 != spReflectionEntryPoint_usesAnySampleRateInput((SlangReflectionEntryPoint*)this);
}
VariableLayoutReflection* getVarLayout()
{
return (VariableLayoutReflection*)spReflectionEntryPoint_getVarLayout(
(SlangReflectionEntryPoint*)this);
}
TypeLayoutReflection* getTypeLayout() { return getVarLayout()->getTypeLayout(); }
VariableLayoutReflection* getResultVarLayout()
{
return (VariableLayoutReflection*)spReflectionEntryPoint_getResultVarLayout(
(SlangReflectionEntryPoint*)this);
}
bool hasDefaultConstantBuffer()
{
return spReflectionEntryPoint_hasDefaultConstantBuffer((SlangReflectionEntryPoint*)this) !=
0;
}
};
typedef EntryPointReflection EntryPointLayout;
struct TypeParameterReflection
{
char const* getName()
{
return spReflectionTypeParameter_GetName((SlangReflectionTypeParameter*)this);
}
unsigned getIndex()
{
return spReflectionTypeParameter_GetIndex((SlangReflectionTypeParameter*)this);
}
unsigned getConstraintCount()
{
return spReflectionTypeParameter_GetConstraintCount((SlangReflectionTypeParameter*)this);
}
TypeReflection* getConstraintByIndex(int index)
{
return (TypeReflection*)spReflectionTypeParameter_GetConstraintByIndex(
(SlangReflectionTypeParameter*)this,
index);
}
};
enum class LayoutRules : SlangLayoutRulesIntegral
{
Default = SLANG_LAYOUT_RULES_DEFAULT,
MetalArgumentBufferTier2 = SLANG_LAYOUT_RULES_METAL_ARGUMENT_BUFFER_TIER_2,
};
typedef struct ShaderReflection ProgramLayout;
typedef enum SlangReflectionGenericArgType GenericArgType;
struct ShaderReflection
{
unsigned getParameterCount() { return spReflection_GetParameterCount((SlangReflection*)this); }
unsigned getTypeParameterCount()
{
return spReflection_GetTypeParameterCount((SlangReflection*)this);
}
slang::ISession* getSession() { return spReflection_GetSession((SlangReflection*)this); }
TypeParameterReflection* getTypeParameterByIndex(unsigned index)
{
return (TypeParameterReflection*)spReflection_GetTypeParameterByIndex(
(SlangReflection*)this,
index);
}
TypeParameterReflection* findTypeParameter(char const* name)
{
return (
TypeParameterReflection*)spReflection_FindTypeParameter((SlangReflection*)this, name);
}
VariableLayoutReflection* getParameterByIndex(unsigned index)
{
return (VariableLayoutReflection*)spReflection_GetParameterByIndex(
(SlangReflection*)this,
index);
}
static ProgramLayout* get(SlangCompileRequest* request)
{
return (ProgramLayout*)spGetReflection(request);
}
SlangUInt getEntryPointCount()
{
return spReflection_getEntryPointCount((SlangReflection*)this);
}
EntryPointReflection* getEntryPointByIndex(SlangUInt index)
{
return (
EntryPointReflection*)spReflection_getEntryPointByIndex((SlangReflection*)this, index);
}
SlangUInt getGlobalConstantBufferBinding()
{
return spReflection_getGlobalConstantBufferBinding((SlangReflection*)this);
}
size_t getGlobalConstantBufferSize()
{
return spReflection_getGlobalConstantBufferSize((SlangReflection*)this);
}
TypeReflection* findTypeByName(const char* name)
{
return (TypeReflection*)spReflection_FindTypeByName((SlangReflection*)this, name);
}
FunctionReflection* findFunctionByName(const char* name)
{
return (FunctionReflection*)spReflection_FindFunctionByName((SlangReflection*)this, name);
}
FunctionReflection* findFunctionByNameInType(TypeReflection* type, const char* name)
{
return (FunctionReflection*)spReflection_FindFunctionByNameInType(
(SlangReflection*)this,
(SlangReflectionType*)type,
name);
}
VariableReflection* findVarByNameInType(TypeReflection* type, const char* name)
{
return (VariableReflection*)spReflection_FindVarByNameInType(
(SlangReflection*)this,
(SlangReflectionType*)type,
name);
}
TypeLayoutReflection* getTypeLayout(
TypeReflection* type,
LayoutRules rules = LayoutRules::Default)
{
return (TypeLayoutReflection*)spReflection_GetTypeLayout(
(SlangReflection*)this,
(SlangReflectionType*)type,
SlangLayoutRules(rules));
}
EntryPointReflection* findEntryPointByName(const char* name)
{
return (
EntryPointReflection*)spReflection_findEntryPointByName((SlangReflection*)this, name);
}
TypeReflection* specializeType(
TypeReflection* type,
SlangInt specializationArgCount,
TypeReflection* const* specializationArgs,
ISlangBlob** outDiagnostics)
{
return (TypeReflection*)spReflection_specializeType(
(SlangReflection*)this,
(SlangReflectionType*)type,
specializationArgCount,
(SlangReflectionType* const*)specializationArgs,
outDiagnostics);
}
GenericReflection* specializeGeneric(
GenericReflection* generic,
SlangInt specializationArgCount,
GenericArgType const* specializationArgTypes,
GenericArgReflection const* specializationArgVals,
ISlangBlob** outDiagnostics)
{
return (GenericReflection*)spReflection_specializeGeneric(
(SlangReflection*)this,
(SlangReflectionGeneric*)generic,
specializationArgCount,
(SlangReflectionGenericArgType const*)specializationArgTypes,
(SlangReflectionGenericArg const*)specializationArgVals,
outDiagnostics);
}
bool isSubType(TypeReflection* subType, TypeReflection* superType)
{
return spReflection_isSubType(
(SlangReflection*)this,
(SlangReflectionType*)subType,
(SlangReflectionType*)superType);
}
SlangUInt getHashedStringCount() const
{
return spReflection_getHashedStringCount((SlangReflection*)this);
}
const char* getHashedString(SlangUInt index, size_t* outCount) const
{
return spReflection_getHashedString((SlangReflection*)this, index, outCount);
}
TypeLayoutReflection* getGlobalParamsTypeLayout()
{
return (TypeLayoutReflection*)spReflection_getGlobalParamsTypeLayout(
(SlangReflection*)this);
}
VariableLayoutReflection* getGlobalParamsVarLayout()
{
return (VariableLayoutReflection*)spReflection_getGlobalParamsVarLayout(
(SlangReflection*)this);
}
SlangResult toJson(ISlangBlob** outBlob)
{
return spReflection_ToJson((SlangReflection*)this, nullptr, outBlob);
}
};
struct DeclReflection
{
enum class Kind
{
Unsupported = SLANG_DECL_KIND_UNSUPPORTED_FOR_REFLECTION,
Struct = SLANG_DECL_KIND_STRUCT,
Func = SLANG_DECL_KIND_FUNC,
Module = SLANG_DECL_KIND_MODULE,
Generic = SLANG_DECL_KIND_GENERIC,
Variable = SLANG_DECL_KIND_VARIABLE,
Namespace = SLANG_DECL_KIND_NAMESPACE,
};
char const* getName() { return spReflectionDecl_getName((SlangReflectionDecl*)this); }
Kind getKind() { return (Kind)spReflectionDecl_getKind((SlangReflectionDecl*)this); }
unsigned int getChildrenCount()
{
return spReflectionDecl_getChildrenCount((SlangReflectionDecl*)this);
}
DeclReflection* getChild(unsigned int index)
{
return (DeclReflection*)spReflectionDecl_getChild((SlangReflectionDecl*)this, index);
}
TypeReflection* getType()
{
return (TypeReflection*)spReflection_getTypeFromDecl((SlangReflectionDecl*)this);
}
VariableReflection* asVariable()
{
return (VariableReflection*)spReflectionDecl_castToVariable((SlangReflectionDecl*)this);
}
FunctionReflection* asFunction()
{
return (FunctionReflection*)spReflectionDecl_castToFunction((SlangReflectionDecl*)this);
}
GenericReflection* asGeneric()
{
return (GenericReflection*)spReflectionDecl_castToGeneric((SlangReflectionDecl*)this);
}
DeclReflection* getParent()
{
return (DeclReflection*)spReflectionDecl_getParent((SlangReflectionDecl*)this);
}
template<Kind K>
struct FilteredList
{
unsigned int count;
DeclReflection* parent;
struct FilteredIterator
{
DeclReflection* parent;
unsigned int count;
unsigned int index;
DeclReflection* operator*() { return parent->getChild(index); }
void operator++()
{
index++;
while (index < count && !(parent->getChild(index)->getKind() == K))
{
index++;
}
}
bool operator!=(FilteredIterator const& other) { return index != other.index; }
};
FilteredIterator begin()
{
unsigned int index = 0;
while (index < count && !(parent->getChild(index)->getKind() == K))
{
index++;
}
return FilteredIterator{parent, count, index};
}
FilteredIterator end() { return FilteredIterator{parent, count, count}; }
};
template<Kind K>
FilteredList<K> getChildrenOfKind()
{
return FilteredList<K>{getChildrenCount(), (DeclReflection*)this};
}
struct IteratedList
{
unsigned int count;
DeclReflection* parent;
struct Iterator
{
DeclReflection* parent;
unsigned int count;
unsigned int index;
DeclReflection* operator*() { return parent->getChild(index); }
void operator++() { index++; }
bool operator!=(Iterator const& other) { return index != other.index; }
};
IteratedList::Iterator begin() { return IteratedList::Iterator{parent, count, 0}; }
IteratedList::Iterator end() { return IteratedList::Iterator{parent, count, count}; }
};
IteratedList getChildren() { return IteratedList{getChildrenCount(), (DeclReflection*)this}; }
};
typedef uint32_t CompileCoreModuleFlags;
struct CompileCoreModuleFlag
{
enum Enum : CompileCoreModuleFlags
{
WriteDocumentation = 0x1,
};
};
typedef ISlangBlob IBlob;
struct IComponentType;
struct ITypeConformance;
struct IGlobalSession;
struct IModule;
struct SessionDesc;
struct SpecializationArg;
struct TargetDesc;
enum class BuiltinModuleName
{
Core,
GLSL
};
struct IGlobalSession : public ISlangUnknown
{
SLANG_COM_INTERFACE(0xc140b5fd, 0xc78, 0x452e, {0xba, 0x7c, 0x1a, 0x1e, 0x70, 0xc7, 0xf7, 0x1c})
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
createSession(SessionDesc const& desc, ISession** outSession) = 0;
virtual SLANG_NO_THROW SlangProfileID SLANG_MCALL findProfile(char const* name) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL
setDownstreamCompilerPath(SlangPassThrough passThrough, char const* path) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL
setDownstreamCompilerPrelude(SlangPassThrough passThrough, const char* preludeText) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL
getDownstreamCompilerPrelude(SlangPassThrough passThrough, ISlangBlob** outPrelude) = 0;
virtual SLANG_NO_THROW const char* SLANG_MCALL getBuildTagString() = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL setDefaultDownstreamCompiler(
SlangSourceLanguage sourceLanguage,
SlangPassThrough defaultCompiler) = 0;
virtual SlangPassThrough SLANG_MCALL
getDefaultDownstreamCompiler(SlangSourceLanguage sourceLanguage) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL
setLanguagePrelude(SlangSourceLanguage sourceLanguage, const char* preludeText) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL
getLanguagePrelude(SlangSourceLanguage sourceLanguage, ISlangBlob** outPrelude) = 0;
[[deprecated]] virtual SLANG_NO_THROW SlangResult SLANG_MCALL
createCompileRequest(slang::ICompileRequest** outCompileRequest) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL
addBuiltins(char const* sourcePath, char const* sourceString) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL
setSharedLibraryLoader(ISlangSharedLibraryLoader* loader) = 0;
virtual SLANG_NO_THROW ISlangSharedLibraryLoader* SLANG_MCALL getSharedLibraryLoader() = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
checkCompileTargetSupport(SlangCompileTarget target) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
checkPassThroughSupport(SlangPassThrough passThrough) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
compileCoreModule(CompileCoreModuleFlags flags) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
loadCoreModule(const void* coreModule, size_t coreModuleSizeInBytes) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
saveCoreModule(SlangArchiveType archiveType, ISlangBlob** outBlob) = 0;
virtual SLANG_NO_THROW SlangCapabilityID SLANG_MCALL findCapability(char const* name) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL setDownstreamCompilerForTransition(
SlangCompileTarget source,
SlangCompileTarget target,
SlangPassThrough compiler) = 0;
virtual SLANG_NO_THROW SlangPassThrough SLANG_MCALL
getDownstreamCompilerForTransition(SlangCompileTarget source, SlangCompileTarget target) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL
getCompilerElapsedTime(double* outTotalTime, double* outDownstreamTime) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL setSPIRVCoreGrammar(char const* jsonPath) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL parseCommandLineArguments(
int argc,
const char* const* argv,
SessionDesc* outSessionDesc,
ISlangUnknown** outAuxAllocation) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
getSessionDescDigest(SessionDesc* sessionDesc, ISlangBlob** outBlob) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
compileBuiltinModule(BuiltinModuleName module, CompileCoreModuleFlags flags) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
loadBuiltinModule(BuiltinModuleName module, const void* moduleData, size_t sizeInBytes) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL saveBuiltinModule(
BuiltinModuleName module,
SlangArchiveType archiveType,
ISlangBlob** outBlob) = 0;
};
#define SLANG_UUID_IGlobalSession IGlobalSession::getTypeGuid()
struct TargetDesc
{
size_t structureSize = sizeof(TargetDesc);
SlangCompileTarget format = SLANG_TARGET_UNKNOWN;
SlangProfileID profile = SLANG_PROFILE_UNKNOWN;
SlangTargetFlags flags = kDefaultTargetFlags;
SlangFloatingPointMode floatingPointMode = SLANG_FLOATING_POINT_MODE_DEFAULT;
SlangLineDirectiveMode lineDirectiveMode = SLANG_LINE_DIRECTIVE_MODE_DEFAULT;
bool forceGLSLScalarBufferLayout = false;
CompilerOptionEntry* compilerOptionEntries = nullptr;
uint32_t compilerOptionEntryCount = 0;
};
typedef uint32_t SessionFlags;
enum
{
kSessionFlags_None = 0
};
struct PreprocessorMacroDesc
{
const char* name;
const char* value;
};
struct SessionDesc
{
size_t structureSize = sizeof(SessionDesc);
TargetDesc const* targets = nullptr;
SlangInt targetCount = 0;
SessionFlags flags = kSessionFlags_None;
SlangMatrixLayoutMode defaultMatrixLayoutMode = SLANG_MATRIX_LAYOUT_ROW_MAJOR;
char const* const* searchPaths = nullptr;
SlangInt searchPathCount = 0;
PreprocessorMacroDesc const* preprocessorMacros = nullptr;
SlangInt preprocessorMacroCount = 0;
ISlangFileSystem* fileSystem = nullptr;
bool enableEffectAnnotations = false;
bool allowGLSLSyntax = false;
CompilerOptionEntry* compilerOptionEntries = nullptr;
uint32_t compilerOptionEntryCount = 0;
};
enum class ContainerType
{
None,
UnsizedArray,
StructuredBuffer,
ConstantBuffer,
ParameterBlock
};
struct ISession : public ISlangUnknown
{
SLANG_COM_INTERFACE(0x67618701, 0xd116, 0x468f, {0xab, 0x3b, 0x47, 0x4b, 0xed, 0xce, 0xe, 0x3d})
virtual SLANG_NO_THROW IGlobalSession* SLANG_MCALL getGlobalSession() = 0;
virtual SLANG_NO_THROW IModule* SLANG_MCALL
loadModule(const char* moduleName, IBlob** outDiagnostics = nullptr) = 0;
virtual SLANG_NO_THROW IModule* SLANG_MCALL loadModuleFromSource(
const char* moduleName,
const char* path,
slang::IBlob* source,
slang::IBlob** outDiagnostics = nullptr) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL createCompositeComponentType(
IComponentType* const* componentTypes,
SlangInt componentTypeCount,
IComponentType** outCompositeComponentType,
ISlangBlob** outDiagnostics = nullptr) = 0;
virtual SLANG_NO_THROW TypeReflection* SLANG_MCALL specializeType(
TypeReflection* type,
SpecializationArg const* specializationArgs,
SlangInt specializationArgCount,
ISlangBlob** outDiagnostics = nullptr) = 0;
virtual SLANG_NO_THROW TypeLayoutReflection* SLANG_MCALL getTypeLayout(
TypeReflection* type,
SlangInt targetIndex = 0,
LayoutRules rules = LayoutRules::Default,
ISlangBlob** outDiagnostics = nullptr) = 0;
virtual SLANG_NO_THROW TypeReflection* SLANG_MCALL getContainerType(
TypeReflection* elementType,
ContainerType containerType,
ISlangBlob** outDiagnostics = nullptr) = 0;
virtual SLANG_NO_THROW TypeReflection* SLANG_MCALL getDynamicType() = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
getTypeRTTIMangledName(TypeReflection* type, ISlangBlob** outNameBlob) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL getTypeConformanceWitnessMangledName(
TypeReflection* type,
TypeReflection* interfaceType,
ISlangBlob** outNameBlob) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL getTypeConformanceWitnessSequentialID(
slang::TypeReflection* type,
slang::TypeReflection* interfaceType,
uint32_t* outId) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
createCompileRequest(SlangCompileRequest** outCompileRequest) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL createTypeConformanceComponentType(
slang::TypeReflection* type,
slang::TypeReflection* interfaceType,
ITypeConformance** outConformance,
SlangInt conformanceIdOverride,
ISlangBlob** outDiagnostics) = 0;
virtual SLANG_NO_THROW IModule* SLANG_MCALL loadModuleFromIRBlob(
const char* moduleName,
const char* path,
slang::IBlob* source,
slang::IBlob** outDiagnostics = nullptr) = 0;
virtual SLANG_NO_THROW SlangInt SLANG_MCALL getLoadedModuleCount() = 0;
virtual SLANG_NO_THROW IModule* SLANG_MCALL getLoadedModule(SlangInt index) = 0;
virtual SLANG_NO_THROW bool SLANG_MCALL
isBinaryModuleUpToDate(const char* modulePath, slang::IBlob* binaryModuleBlob) = 0;
virtual SLANG_NO_THROW IModule* SLANG_MCALL loadModuleFromSourceString(
const char* moduleName,
const char* path,
const char* string,
slang::IBlob** outDiagnostics = nullptr) = 0;
};
#define SLANG_UUID_ISession ISession::getTypeGuid()
struct IMetadata : public ISlangCastable
{
SLANG_COM_INTERFACE(0x8044a8a3, 0xddc0, 0x4b7f, {0xaf, 0x8e, 0x2, 0x6e, 0x90, 0x5d, 0x73, 0x32})
virtual SlangResult isParameterLocationUsed(
SlangParameterCategory category,
SlangUInt spaceIndex,
SlangUInt registerIndex,
bool& outUsed) = 0;
};
#define SLANG_UUID_IMetadata IMetadata::getTypeGuid()
struct IComponentType : public ISlangUnknown
{
SLANG_COM_INTERFACE(0x5bc42be8, 0x5c50, 0x4929, {0x9e, 0x5e, 0xd1, 0x5e, 0x7c, 0x24, 0x1, 0x5f})
virtual SLANG_NO_THROW ISession* SLANG_MCALL getSession() = 0;
virtual SLANG_NO_THROW ProgramLayout* SLANG_MCALL
getLayout(SlangInt targetIndex = 0, IBlob** outDiagnostics = nullptr) = 0;
virtual SLANG_NO_THROW SlangInt SLANG_MCALL getSpecializationParamCount() = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL getEntryPointCode(
SlangInt entryPointIndex,
SlangInt targetIndex,
IBlob** outCode,
IBlob** outDiagnostics = nullptr) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL getResultAsFileSystem(
SlangInt entryPointIndex,
SlangInt targetIndex,
ISlangMutableFileSystem** outFileSystem) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL
getEntryPointHash(SlangInt entryPointIndex, SlangInt targetIndex, IBlob** outHash) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL specialize(
SpecializationArg const* specializationArgs,
SlangInt specializationArgCount,
IComponentType** outSpecializedComponentType,
ISlangBlob** outDiagnostics = nullptr) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
link(IComponentType** outLinkedComponentType, ISlangBlob** outDiagnostics = nullptr) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL getEntryPointHostCallable(
int entryPointIndex,
int targetIndex,
ISlangSharedLibrary** outSharedLibrary,
slang::IBlob** outDiagnostics = 0) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
renameEntryPoint(const char* newName, IComponentType** outEntryPoint) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL linkWithOptions(
IComponentType** outLinkedComponentType,
uint32_t compilerOptionEntryCount,
CompilerOptionEntry* compilerOptionEntries,
ISlangBlob** outDiagnostics = nullptr) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
getTargetCode(SlangInt targetIndex, IBlob** outCode, IBlob** outDiagnostics = nullptr) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL getTargetMetadata(
SlangInt targetIndex,
IMetadata** outMetadata,
IBlob** outDiagnostics = nullptr) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL getEntryPointMetadata(
SlangInt entryPointIndex,
SlangInt targetIndex,
IMetadata** outMetadata,
IBlob** outDiagnostics = nullptr) = 0;
};
#define SLANG_UUID_IComponentType IComponentType::getTypeGuid()
struct IEntryPoint : public IComponentType
{
SLANG_COM_INTERFACE(0x8f241361, 0xf5bd, 0x4ca0, {0xa3, 0xac, 0x2, 0xf7, 0xfa, 0x24, 0x2, 0xb8})
virtual SLANG_NO_THROW FunctionReflection* SLANG_MCALL getFunctionReflection() = 0;
};
#define SLANG_UUID_IEntryPoint IEntryPoint::getTypeGuid()
struct ITypeConformance : public IComponentType
{
SLANG_COM_INTERFACE(0x73eb3147, 0xe544, 0x41b5, {0xb8, 0xf0, 0xa2, 0x44, 0xdf, 0x21, 0x94, 0xb})
};
#define SLANG_UUID_ITypeConformance ITypeConformance::getTypeGuid()
struct IModule : public IComponentType
{
SLANG_COM_INTERFACE(0xc720e64, 0x8722, 0x4d31, {0x89, 0x90, 0x63, 0x8a, 0x98, 0xb1, 0xc2, 0x79})
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
findEntryPointByName(char const* name, IEntryPoint** outEntryPoint) = 0;
virtual SLANG_NO_THROW SlangInt32 SLANG_MCALL getDefinedEntryPointCount() = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
getDefinedEntryPoint(SlangInt32 index, IEntryPoint** outEntryPoint) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL serialize(ISlangBlob** outSerializedBlob) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL writeToFile(char const* fileName) = 0;
virtual SLANG_NO_THROW const char* SLANG_MCALL getName() = 0;
virtual SLANG_NO_THROW const char* SLANG_MCALL getFilePath() = 0;
virtual SLANG_NO_THROW const char* SLANG_MCALL getUniqueIdentity() = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL findAndCheckEntryPoint(
char const* name,
SlangStage stage,
IEntryPoint** outEntryPoint,
ISlangBlob** outDiagnostics) = 0;
virtual SLANG_NO_THROW SlangInt32 SLANG_MCALL getDependencyFileCount() = 0;
virtual SLANG_NO_THROW char const* SLANG_MCALL getDependencyFilePath(SlangInt32 index) = 0;
virtual SLANG_NO_THROW DeclReflection* SLANG_MCALL getModuleReflection() = 0;
};
#define SLANG_UUID_IModule IModule::getTypeGuid()
struct IModulePrecompileService_Experimental : public ISlangUnknown
{
SLANG_COM_INTERFACE(
0x8e12e8e3,
0x5fcd,
0x433e,
{0xaf, 0xcb, 0x13, 0xa0, 0x88, 0xbc, 0x5e, 0xe5})
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
precompileForTarget(SlangCompileTarget target, ISlangBlob** outDiagnostics) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL getPrecompiledTargetCode(
SlangCompileTarget target,
IBlob** outCode,
IBlob** outDiagnostics = nullptr) = 0;
virtual SLANG_NO_THROW SlangInt SLANG_MCALL getModuleDependencyCount() = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL getModuleDependency(
SlangInt dependencyIndex,
IModule** outModule,
IBlob** outDiagnostics = nullptr) = 0;
};
#define SLANG_UUID_IModulePrecompileService_Experimental \
IModulePrecompileService_Experimental::getTypeGuid()
struct SpecializationArg
{
enum class Kind : int32_t
{
Unknown,
Type,
};
Kind kind;
union
{
TypeReflection* type;
};
static SpecializationArg fromType(TypeReflection* inType)
{
SpecializationArg rs;
rs.kind = Kind::Type;
rs.type = inType;
return rs;
}
};
}
#define SLANG_API_VERSION 0
enum SlangLanguageVersion
{
SLANG_LANGUAGE_VERSION_2025 = 2025
};
struct SlangGlobalSessionDesc
{
uint32_t structureSize = sizeof(SlangGlobalSessionDesc);
uint32_t apiVersion = SLANG_API_VERSION;
uint32_t languageVersion = SLANG_LANGUAGE_VERSION_2025;
bool enableGLSL = false;
uint32_t reserved[16] = {};
};
SLANG_EXTERN_C SLANG_API SlangResult
slang_createGlobalSession(SlangInt apiVersion, slang::IGlobalSession** outGlobalSession);
SLANG_EXTERN_C SLANG_API SlangResult slang_createGlobalSession2(
const SlangGlobalSessionDesc* desc,
slang::IGlobalSession** outGlobalSession);
SLANG_EXTERN_C SLANG_API SlangResult slang_createGlobalSessionWithoutCoreModule(
SlangInt apiVersion,
slang::IGlobalSession** outGlobalSession);
SLANG_API ISlangBlob* slang_getEmbeddedCoreModule();
SLANG_EXTERN_C SLANG_API void slang_shutdown();
SLANG_EXTERN_C SLANG_API const char* slang_getLastInternalErrorMessage();
namespace slang
{
inline SlangResult createGlobalSession(slang::IGlobalSession** outGlobalSession)
{
SlangGlobalSessionDesc defaultDesc = {};
return slang_createGlobalSession2(&defaultDesc, outGlobalSession);
}
inline SlangResult createGlobalSession(
const SlangGlobalSessionDesc* desc,
slang::IGlobalSession** outGlobalSession)
{
return slang_createGlobalSession2(desc, outGlobalSession);
}
inline void shutdown()
{
slang_shutdown();
}
inline const char* getLastInternalErrorMessage()
{
return slang_getLastInternalErrorMessage();
}
}
#endif
#define SLANG_ERROR_INSUFFICIENT_BUFFER SLANG_E_BUFFER_TOO_SMALL
#define SLANG_ERROR_INVALID_PARAMETER SLANG_E_INVALID_ARG
#endif