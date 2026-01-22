#include <u.h>
#include <libc.h>
# if defined(OPT_SPEED) && defined(OPT_ACCURACY)
# error "cannot optimize for both speed and accuracy"
# endif
# if defined(OPT_SPEED) && !defined(OPT_SSO)
# define OPT_SSO
# endif