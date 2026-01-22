#ifndef KSH_TIME_H
# define KSH_TIME_H
#ifdef TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif
#ifndef TIME_DECLARED
extern time_t time ARGS((time_t *));
#endif
#ifndef CLK_TCK
# define CLK_TCK 60
#endif
#endif