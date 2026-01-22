#include "stdinc.h"
#include "9.h"
int
consVPrint(char* fmt, va_list args)
{
int len, ret;
char buf[256];
len = vsnprint(buf, sizeof(buf), fmt, args);
ret = consWrite(buf, len);
while (len-- > 0 && buf[len] == '\n')
buf[len] = '\0';
if (0)
syslog(0, "fossil", "%s", buf);
return ret;
}
int
consPrint(char* fmt, ...)
{
int ret;
va_list args;
va_start(args, fmt);
ret = consVPrint(fmt, args);
va_end(args);
return ret;
}