#include "pipe_.h"
#include "string_.h"
#include "time_.h"
#include "gx.h"
#include "gp.h"
#include <signal.h>
#include <stdlib.h>
int interrupted;
private void signalhandler(int);
private FILE *rbfopen(char *, char *);
void
gp_init(void)
{
intercept(signalhandler);
}
void
gp_exit(int exit_status, int code)
{
}
void
gp_do_exit(int exit_status)
{
exit(exit_status);
}
private void
signalhandler(int sig)
{
clearerr(stdin);
switch (sig) {
case SIGINT:
case SIGQUIT:
interrupted = 1;
break;
case SIGFPE:
interrupted = 2;
break;
default:
break;
}
}
#define PS_YEAR_0 80
#define PS_MONTH_0 1
#define PS_DAY_0 1
void
gp_get_realtime(long *pdt)
{
long date, time, pstime, psdate, tick;
short day;
_sysdate(0, &time, &date, &day, &tick);
_julian(&time, &date);
pstime = 0;
psdate = (PS_YEAR_0 << 16) + (PS_MONTH_0 << 8) + PS_DAY_0;
_julian(&pstime, &psdate);
pdt[0] = (date - psdate) * 86400 + time;
pdt[1] = 0;
#ifdef DEBUG_CLOCK
printf("pdt[0] = %ld  pdt[1] = %ld\n", pdt[0], pdt[1]);
#endif
}
void
gp_get_usertime(long *pdt)
{
return gp_get_realtime(pdt);
}
int gp_cache_insert(int type, byte *key, int keylen, void *buffer, int buflen)
{
return 0;
}
int gp_cache_query(int type, byte* key, int keylen, void **buffer,
gp_cache_alloc alloc, void *userdata)
{
return -1;
}
FILE *
gp_open_printer(char fname[gp_file_name_sizeof], int binary_mode)
{
return
(strlen(fname) == 0 ? 0 :
fname[0] == '|' ? popen(fname + 1, "w") :
rbfopen(fname, "w"));
}
FILE *
rbfopen(char *fname, char *perm)
{
FILE *file = fopen(fname, perm);
file->_flag |= _RBF;
return file;
}
void
gp_close_printer(FILE * pfile, const char *fname)
{
if (fname[0] == '|')
pclose(pfile);
else
fclose(pfile);
}
int
gp_setmode_binary(FILE * pfile, bool binary)
{
if (binary)
file->_flag |= _RBF;
else
file->_flag &= ~_RBF;
return 0;
}
void *gp_enumerate_fonts_init(gs_memory_t *mem)
{
return NULL;
}
int gp_enumerate_fonts_next(void *enum_state, char **fontname, char **path)
{
return 0;
}
void gp_enumerate_fonts_free(void *enum_state)
{
}