#include "stdio_.h"
#include "gx.h"
#include "gp.h"
int gp_stdin_read(char *buf, int len, int interactive, FILE *f)
{
return fread(buf, 1, interactive ? 1 : len, f);
}