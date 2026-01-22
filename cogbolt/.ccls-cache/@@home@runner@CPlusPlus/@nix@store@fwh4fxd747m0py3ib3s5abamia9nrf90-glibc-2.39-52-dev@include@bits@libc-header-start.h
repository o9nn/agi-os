#ifndef __GLIBC_INTERNAL_STARTING_HEADER_IMPLEMENTATION
# error "Never include <bits/libc-header-start.h> directly."
#endif
#undef __GLIBC_INTERNAL_STARTING_HEADER_IMPLEMENTATION
#include <features.h>
#undef __GLIBC_USE_LIB_EXT2
#if (defined __USE_GNU							\
|| (defined __STDC_WANT_LIB_EXT2__ && __STDC_WANT_LIB_EXT2__ > 0))
# define __GLIBC_USE_LIB_EXT2 1
#else
# define __GLIBC_USE_LIB_EXT2 0
#endif
#undef __GLIBC_USE_IEC_60559_BFP_EXT
#if defined __USE_GNU || defined __STDC_WANT_IEC_60559_BFP_EXT__
# define __GLIBC_USE_IEC_60559_BFP_EXT 1
#else
# define __GLIBC_USE_IEC_60559_BFP_EXT 0
#endif
#undef __GLIBC_USE_IEC_60559_BFP_EXT_C2X
#if __GLIBC_USE (IEC_60559_BFP_EXT) || __GLIBC_USE (ISOC2X)
# define __GLIBC_USE_IEC_60559_BFP_EXT_C2X 1
#else
# define __GLIBC_USE_IEC_60559_BFP_EXT_C2X 0
#endif
#undef __GLIBC_USE_IEC_60559_EXT
#if __GLIBC_USE (IEC_60559_BFP_EXT) || defined __STDC_WANT_IEC_60559_EXT__
# define __GLIBC_USE_IEC_60559_EXT 1
#else
# define __GLIBC_USE_IEC_60559_EXT 0
#endif
#undef __GLIBC_USE_IEC_60559_FUNCS_EXT
#if defined __USE_GNU || defined __STDC_WANT_IEC_60559_FUNCS_EXT__
# define __GLIBC_USE_IEC_60559_FUNCS_EXT 1
#else
# define __GLIBC_USE_IEC_60559_FUNCS_EXT 0
#endif
#undef __GLIBC_USE_IEC_60559_FUNCS_EXT_C2X
#if __GLIBC_USE (IEC_60559_FUNCS_EXT) || __GLIBC_USE (ISOC2X)
# define __GLIBC_USE_IEC_60559_FUNCS_EXT_C2X 1
#else
# define __GLIBC_USE_IEC_60559_FUNCS_EXT_C2X 0
#endif
#undef __GLIBC_USE_IEC_60559_TYPES_EXT
#if defined __USE_GNU || defined __STDC_WANT_IEC_60559_TYPES_EXT__
# define __GLIBC_USE_IEC_60559_TYPES_EXT 1
#else
# define __GLIBC_USE_IEC_60559_TYPES_EXT 0
#endif