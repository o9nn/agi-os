#ifndef KSH_TIMES_H
# define KSH_TIMES_H
#include "ksh_time.h"
#include <sys/times.h>
#ifdef TIMES_BROKEN
extern clock_t	ksh_times ARGS((struct tms *));
#else
# define ksh_times times
#endif
#ifdef HAVE_TIMES
extern clock_t	times ARGS((struct tms *));
#endif
#endif