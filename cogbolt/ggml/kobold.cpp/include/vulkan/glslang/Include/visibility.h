#ifdef GLSLANG_IS_SHARED_LIBRARY
#ifdef _WIN32
#ifdef GLSLANG_EXPORTING
#define GLSLANG_EXPORT __declspec(dllexport)
#else
#define GLSLANG_EXPORT __declspec(dllimport)
#endif
#elif __GNUC__ >= 4
#define GLSLANG_EXPORT __attribute__((visibility("default")))
#endif
#endif
#ifndef GLSLANG_EXPORT
#define GLSLANG_EXPORT
#endif
#define GLSLANG_EXPORT_FOR_TESTS GLSLANG_EXPORT