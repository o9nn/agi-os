#ifndef	_ALLOCA_H
#define	_ALLOCA_H	1
#include <features.h>
#define	__need_size_t
#include <stddef.h>
__BEGIN_DECLS
#undef	alloca
extern void *alloca (size_t __size) __THROW;
#ifdef	__GNUC__
# define alloca(size)	__builtin_alloca (size)
#endif
__END_DECLS
#endif