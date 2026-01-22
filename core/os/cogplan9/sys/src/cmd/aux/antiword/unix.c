#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "antiword.h"
void
werr(int iFatal, const char *szFormat, ...)
{
va_list tArg;
va_start(tArg, szFormat);
(void)vfprintf(stderr, szFormat, tArg);
va_end(tArg);
fprintf(stderr, "\n");
switch (iFatal) {
case 0:
return;
case 1:
exit(EXIT_FAILURE);
default:
exit(iFatal);
}
}
void
Hourglass_On(void)
{
}
void
Hourglass_Off(void)
{
}