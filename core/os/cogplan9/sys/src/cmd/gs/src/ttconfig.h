#ifndef TTCONFIG_H
#define TTCONFIG_H
#include "ttconf.h"
#define ALIGNMENT 8
#define SECURE_COMPUTATIONS
#define IGNORE_FILL_FLOW
#ifndef HAVE_PRINT_FUNCTION
#define Print( format, ap ) vfprintf( stderr, (format), (ap) )
#endif
#define FT_BIG_ENDIAN 4321
#define FT_LITTLE_ENDIAN 1234
#ifdef WORDS_BIGENDIAN
#define FT_BYTE_ORDER FT_BIG_ENDIAN
#else
#define FT_BYTE_ORDER FT_LITTLE_ENDIAN
#endif
#if FT_BYTE_ORDER == FT_BIG_ENDIAN
#ifndef BUS_ERROR
#define LOOSE_ACCESS
#endif
#endif
#undef TT_CONFIG_THREAD_SAFE
#undef TT_CONFIG_REENTRANT
#if defined(TT_CONFIG_THREAD_SAFE) || defined(TT_CONFIG_REENTRANT)
#define TT_CONFIG_THREADS
#endif
#undef TT_STATIC_INTERPRETER
#undef TT_STATIC_RASTER
#define TT_EXTEND_ENGINE
#endif