#include "pipe_.h"
#include "string_.h"
#include "time_.h"
#include "gx.h"
#include "gsexit.h"
#include "gp.h"
#ifdef __PROTOTYPES__
# include <stdlib.h>
#else
extern void exit(int);
extern char *getenv(const char *);
#endif
void
gp_init(void)
{
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
const char *
gp_strerror(int errnum)
{
return NULL;
}
int
gp_read_macresource(byte *buf, const char *filename,
const uint type, const ushort id)
{
return 0;
}
void
gp_get_realtime(long *pdt)
{
struct timeval tp;
#if gettimeofday_no_timezone
{
if (gettimeofday(&tp) == -1) {
lprintf("Ghostscript: gettimeofday failed!\n");
tp.tv_sec = tp.tv_usec = 0;
}
}
#else
{
struct timezone tzp;
if (gettimeofday(&tp, &tzp) == -1) {
lprintf("Ghostscript: gettimeofday failed!\n");
tp.tv_sec = tp.tv_usec = 0;
}
}
#endif
pdt[0] = tp.tv_sec;
pdt[1] = tp.tv_usec >= 0 && tp.tv_usec < 1000000 ? tp.tv_usec * 1000 : 0;
#ifdef DEBUG_CLOCK
printf("tp.tv_sec = %d  tp.tv_usec = %d  pdt[0] = %ld  pdt[1] = %ld\n",
tp.tv_sec, tp.tv_usec, pdt[0], pdt[1]);
#endif
}
void
gp_get_usertime(long *pdt)
{
#if use_times_for_usertime
struct tms tms;
long ticks;
const long ticks_per_sec = CLK_TCK;
times(&tms);
ticks = tms.tms_utime + tms.tms_stime + tms.tms_cutime + tms.tms_cstime;
pdt[0] = ticks / ticks_per_sec;
pdt[1] = (ticks % ticks_per_sec) * (1000000000 / ticks_per_sec);
#else
gp_get_realtime(pdt);
#endif
}
const char *
gp_getenv_display(void)
{
return getenv("DISPLAY");
}
FILE *
gp_open_printer(char fname[gp_file_name_sizeof], int binary_mode)
{
const char *fmode = (binary_mode ? "wb" : "w");
return (strlen(fname) == 0 ? 0 : fopen(fname, fmode));
}
void
gp_close_printer(FILE * pfile, const char *fname)
{
if (fname[0] == '|')
pclose(pfile);
else
fclose(pfile);
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