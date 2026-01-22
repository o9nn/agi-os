#include <u.h>
#include <libc.h>
#include "String.h"
extern String*
s_copy(char *cp)
{
String *sp;
int len;
len = strlen(cp)+1;
sp = s_newalloc(len);
setmalloctag(sp, getcallerpc(&cp));
strcpy(sp->base, cp);
sp->ptr = sp->base + len - 1;
return sp;
}