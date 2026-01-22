#ifndef	_STDC_PREDEF_H
#define	_STDC_PREDEF_H	1
#ifdef __GCC_IEC_559
# if __GCC_IEC_559 > 0
#  define __STDC_IEC_559__		1
#  define __STDC_IEC_60559_BFP__ 	201404L
# endif
#else
# define __STDC_IEC_559__		1
# define __STDC_IEC_60559_BFP__ 	201404L
#endif
#ifdef __GCC_IEC_559_COMPLEX
# if __GCC_IEC_559_COMPLEX > 0
#  define __STDC_IEC_559_COMPLEX__	1
#  define __STDC_IEC_60559_COMPLEX__	201404L
# endif
#else
# define __STDC_IEC_559_COMPLEX__	1
# define __STDC_IEC_60559_COMPLEX__	201404L
#endif
#define __STDC_ISO_10646__		201706L
#endif