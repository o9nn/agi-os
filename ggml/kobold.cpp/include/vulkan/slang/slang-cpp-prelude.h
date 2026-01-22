#ifndef SLANG_CPP_PRELUDE_H
#define SLANG_CPP_PRELUDE_H
#ifdef SLANG_LLVM
#include "slang-llvm.h"
#else
#if SLANG_GCC_FAMILY && __GNUC__ < 6
#include <cmath>
#define SLANG_PRELUDE_STD std::
#else
#include <math.h>
#define SLANG_PRELUDE_STD
#endif
#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#endif
#if defined(_MSC_VER)
#define SLANG_PRELUDE_SHARED_LIB_EXPORT __declspec(dllexport)
#else
#define SLANG_PRELUDE_SHARED_LIB_EXPORT __attribute__((__visibility__("default")))
#endif
#ifdef __cplusplus
#define SLANG_PRELUDE_EXTERN_C extern "C"
#define SLANG_PRELUDE_EXTERN_C_START \
extern "C"                       \
{
#define SLANG_PRELUDE_EXTERN_C_END }
#else
#define SLANG_PRELUDE_EXTERN_C
#define SLANG_PRELUDE_EXTERN_C_START
#define SLANG_PRELUDE_EXTERN_C_END
#endif
#define SLANG_PRELUDE_EXPORT SLANG_PRELUDE_EXTERN_C SLANG_PRELUDE_SHARED_LIB_EXPORT
#define SLANG_PRELUDE_EXPORT_START SLANG_PRELUDE_EXTERN_C_START SLANG_PRELUDE_SHARED_LIB_EXPORT
#define SLANG_PRELUDE_EXPORT_END SLANG_PRELUDE_EXTERN_C_END
#ifndef INFINITY
#define INFINITY float(1e+300 * 1e+300)
#endif
#ifndef SLANG_INFINITY
#define SLANG_INFINITY INFINITY
#endif
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
#elif defined(__APPLE__) && !defined(SLANG_LLVM)
#include "TargetConditionals.h"
#if TARGET_OS_MAC
#define SLANG_OSX 1
#else
#define SLANG_IOS 1
#endif
#elif defined(__APPLE__)
#define SLANG_OSX 1
#elif defined(__CELLOS_LV2__)
#define SLANG_PS3 1
#elif defined(__ORBIS__)
#define SLANG_PS4 1
#elif defined(__SNC__) && defined(__arm__)
#define SLANG_PSP2 1
#elif defined(__ghs__)
#define SLANG_WIIU 1
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
#if SLANG_GCC_FAMILY
#define SLANG_ALIGN_OF(T) __alignof__(T)
#define SLANG_BREAKPOINT(id) __builtin_trap()
#define SLANG_OFFSET_OF(T, ELEMENT) (size_t(&((T*)1)->ELEMENT) - 1)
#endif
#if SLANG_VC
#define SLANG_ALIGN_OF(T) __alignof(T)
#define SLANG_BREAKPOINT(id) __debugbreak();
#endif
#ifndef SLANG_OFFSET_OF
#define SLANG_OFFSET_OF(X, Y) offsetof(X, Y)
#endif
#ifndef SLANG_BREAKPOINT
#define SLANG_BREAKPOINT(id) (*((int*)0) = int(id));
#endif
#ifndef SLANG_H
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
#ifndef SLANG_FORCE_INLINE
#define SLANG_FORCE_INLINE inline
#endif
struct SlangUUID
{
uint32_t data1;
uint16_t data2;
uint16_t data3;
uint8_t data4[8];
};
typedef int32_t SlangResult;
struct ISlangUnknown
{
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
queryInterface(SlangUUID const& uuid, void** outObject) = 0;
virtual SLANG_NO_THROW uint32_t SLANG_MCALL addRef() = 0;
virtual SLANG_NO_THROW uint32_t SLANG_MCALL release() = 0;
};
#define SLANG_COM_INTERFACE(a, b, c, d0, d1, d2, d3, d4, d5, d6, d7)             \
public:                                                                          \
SLANG_FORCE_INLINE static const SlangUUID& getTypeGuid()                     \
{                                                                            \
static const SlangUUID guid = {a, b, c, d0, d1, d2, d3, d4, d5, d6, d7}; \
return guid;                                                             \
}
#endif
#include "slang-cpp-scalar-intrinsics.h"
#include "slang-cpp-types.h"
#if defined(_MSC_VER)
#pragma warning(disable : 4700)
#endif
#ifndef SLANG_UNROLL
#define SLANG_UNROLL
#endif
#endif