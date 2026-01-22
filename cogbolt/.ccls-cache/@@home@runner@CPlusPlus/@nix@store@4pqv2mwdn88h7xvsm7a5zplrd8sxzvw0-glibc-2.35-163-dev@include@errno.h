#ifndef	_ERRNO_H
#define	_ERRNO_H 1
#include <features.h>
#include <bits/errno.h>
#ifndef __ASSEMBLER__
__BEGIN_DECLS
extern int *__errno_location (void) __THROW __attribute_const__;
# define errno (*__errno_location ())
# ifdef __USE_GNU
extern char *program_invocation_name;
extern char *program_invocation_short_name;
#include <bits/types/error_t.h>
# endif
__END_DECLS
#endif
#endif