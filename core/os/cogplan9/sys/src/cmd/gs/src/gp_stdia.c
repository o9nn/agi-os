#include "stdio_.h"
#include "time_.h"
#include "unistd_.h"
#include "gx.h"
#include "gp.h"
int gp_stdin_read(char *buf, int len, int interactive, FILE *f)
{
return read(fileno(f), buf, len);
}