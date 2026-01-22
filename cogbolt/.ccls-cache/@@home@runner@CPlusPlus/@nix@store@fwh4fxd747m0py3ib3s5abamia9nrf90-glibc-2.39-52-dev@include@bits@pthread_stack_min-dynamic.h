#ifndef PTHREAD_STACK_MIN
# if defined __USE_DYNAMIC_STACK_SIZE && __USE_DYNAMIC_STACK_SIZE
# ifndef __ASSEMBLER__
# define __SC_THREAD_STACK_MIN_VALUE 75
__BEGIN_DECLS
extern long int __sysconf (int __name) __THROW;
__END_DECLS
# define PTHREAD_STACK_MIN __sysconf (__SC_THREAD_STACK_MIN_VALUE)
# endif
# else
# include <bits/pthread_stack_min.h>
# endif
#endif