#include <bits/wordsize.h>
#if defined __x86_64__ && defined __ILP32__
# define __TIMESIZE	64
#else
# define __TIMESIZE	__WORDSIZE
#endif