#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include "global.h"
#include "error.h"
#include "lexxer.h"
static const char *program;
int errors = 0;
void
fatal(const char *format, ...)
{
va_list pvar;
va_start(pvar, format);
fprintf(stderr, "%s: fatal: ", program);
(void) vfprintf(stderr, format, pvar);
fprintf(stderr, "\n");
va_end(pvar);
exit(1);
}
void
warn(const char *format, ...)
{
va_list pvar;
va_start(pvar, format);
if (!BeQuiet && (errors == 0))
{
fprintf(stderr, "\"%s\", line %d: warning: ", inname, lineno-1);
(void) vfprintf(stderr, format, pvar);
fprintf(stderr, "\n");
}
va_end(pvar);
}
void
error(const char *format, ...)
{
va_list pvar;
va_start(pvar, format);
fprintf(stderr, "\"%s\", line %d: ", inname, lineno-1);
(void) vfprintf(stderr, format, pvar);
fprintf(stderr, "\n");
va_end(pvar);
errors++;
}
const char *
unix_error_string(int error_num)
{
static char buffer[256];
const char *error_mess;
error_mess = strerror (error_num);
sprintf(buffer, "%s (%d)", error_mess, error_num);
return buffer;
}
void
set_program_name(const char *name)
{
program = name;
}