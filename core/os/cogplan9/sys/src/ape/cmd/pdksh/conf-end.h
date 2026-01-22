#if defined(EMACS) || defined(VI)
# define EDIT
#else
# undef EDIT
#endif
#if defined(EDIT) && !defined(HISTORY)
# define HISTORY
#endif
#if defined(HISTORY) && (!defined(COMPLEX_HISTORY) || !defined(HAVE_MMAP) || !defined(HAVE_FLOCK))
# undef COMPLEX_HISTORY
# define EASY_HISTORY
#endif
#if (defined(HAVE_WAITPID) || defined(HAVE_WAIT3)) \
&& (defined(POSIX_SIGNALS) || defined(BSD42_SIGNALS))
# define JOB_SIGS
#endif
#if !defined(JOB_SIGS) || !(defined(POSIX_PGRP) || defined(BSD_PGRP))
# undef JOBS
#endif
#ifdef SIGNALS_DONT_INTERRUPT
# error pdksh needs interruptable system calls.
#endif
#ifdef HAVE_GCC_FUNC_ATTR
# define GCC_FUNC_ATTR(x) __attribute__((x))
# define GCC_FUNC_ATTR2(x,y) __attribute__((x,y))
#else
# define GCC_FUNC_ATTR(x)
# define GCC_FUNC_ATTR2(x,y)
#endif