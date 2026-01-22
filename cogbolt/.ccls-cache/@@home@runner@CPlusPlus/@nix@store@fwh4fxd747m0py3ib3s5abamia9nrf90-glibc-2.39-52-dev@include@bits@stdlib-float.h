#ifndef _STDLIB_H
# error "Never use <bits/stdlib-float.h> directly; include <stdlib.h> instead."
#endif
#ifdef __USE_EXTERN_INLINES
__extern_inline double
__NTH (atof (const char *__nptr))
{
return strtod (__nptr, (char **) NULL);
}
#endif