#if !defined(__fail_h)
#define __fail_h 1
#undef fail
#if defined(NDEBUG)
#define fail(e)	((void)0)
#else
#define fail(e)	((e) ? __fail(#e, __FILE__, __LINE__) : (void)0)
#endif
extern void	__fail(char *, char *, int);
#endif