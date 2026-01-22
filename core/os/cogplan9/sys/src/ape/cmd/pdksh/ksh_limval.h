#ifdef HAVE_VALUES_H
# include <values.h>
#endif
#ifndef DMAXEXP
# define DMAXEXP 128
#endif
#ifndef BITSPERBYTE
# ifdef CHAR_BIT
# define BITSPERBYTE CHAR_BIT
# else
# define BITSPERBYTE 8
# endif
#endif
#ifndef BITS
# define BITS(t) (BITSPERBYTE * sizeof(t))
#endif