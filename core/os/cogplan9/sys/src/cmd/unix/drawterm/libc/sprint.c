#include <u.h>
#include <libc.h>
#include "fmtdef.h"
int
sprint(char *buf, char *fmt, ...)
{
int n;
uint len;
va_list args;
len = 1<<30;
if((uintptr)buf+len < (uintptr)buf)
len = -(uintptr)buf-1;
va_start(args, fmt);
n = vsnprint(buf, len, fmt, args);
va_end(args);
return n;
}