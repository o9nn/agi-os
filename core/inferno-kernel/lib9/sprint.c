#include "lib9.h"
int
sprint(char *buf, char *fmt, ...)
{
int n;
uint len;
va_list args;
len = 1<<30;
if((uintptr)buf+len < (uintptr)buf)
len = -(uint)buf-1;
va_start(args, fmt);
n = vsnprint(buf, len, fmt, args);
va_end(args);
return n;
}