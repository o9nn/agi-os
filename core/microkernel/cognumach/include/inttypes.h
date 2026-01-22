#ifndef _INTTYPES_H_
#define _INTTYPES_H_
#include <stdint.h>
#ifdef __x86_64__
#define __64PREFIX "l"
#define __PTRPREFIX "l"
#else
#define __64PREFIX "ll"
#define __PTRPREFIX
#endif
#define PRId8 "d"
#define PRId16 "d"
#define PRId32 "d"
#define PRId64 __64PREFIX"d"
#define PRIdPTR __PTRPREFIX"d"
#define PRIi8 "i"
#define PRIi16 "i"
#define PRIi32 "i"
#define PRIi64 __64PREFIX"i"
#define PRIiPTR __PTRPREFIX"i"
#define PRIu8 "u"
#define PRIu16 "u"
#define PRIu32 "u"
#define PRIu64 __64PREFIX"u"
#define PRIuPTR __PTRPREFIX"u"
#define PRIx8 "x"
#define PRIx16 "x"
#define PRIx32 "x"
#define PRIx64 __64PREFIX"x"
#define PRIxPTR __PTRPREFIX"x"
#define PRIo8 "o"
#define PRIo16 "o"
#define PRIo32 "o"
#define PRIo64 __64PREFIX"o"
#define PRIoPTR __PTRPREFIX"o"
#endif