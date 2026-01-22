#include <u.h>
#include <libc.h>
#include "String.h"
extern String*	_s_alloc(void);
extern String *
s_array(char *cp, int len)
{
String *sp = _s_alloc();
sp->base = sp->ptr = cp;
sp->end = sp->base + len;
sp->fixed = 1;
return sp;
}