#include "string_.h"
#include "gx.h"
#include "gsexit.h"
#include "gp.h"
#include "time_.h"
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
return strerror(errnum);
}
void
gp_get_realtime(long *pdt)
{
struct timeval tp;
struct timezone tzp;
if (gettimeofday(&tp, &tzp) == -1) {
lprintf("Ghostscript: gettimeofday failed!\n");
tp.tv_sec = tp.tv_usec = 0;
}
pdt[0] = tp.tv_sec;
pdt[1] = tp.tv_usec * 1000;
#ifdef DEBUG_CLOCK
printf("tp.tv_sec = %d  tp.tv_usec = %d  pdt[0] = %ld  pdt[1] = %ld\n",
tp.tv_sec, tp.tv_usec, pdt[0], pdt[1]);
#endif
}
void
gp_get_usertime(long *pdt)
{
gp_get_realtime(pdt);
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
extern void gp_set_file_binary(int, int);
FILE *
gp_open_printer(char fname[gp_file_name_sizeof], int binary_mode)
{
if (strlen(fname) == 0 || !strcmp(fname, "PRN")) {
if (binary_mode)
gp_set_file_binary(fileno(stdprn), 1);
stdprn->_flag = _IOWRT;
return stdprn;
} else
return fopen(fname, (binary_mode ? "wb" : "w"));
}
void
gp_close_printer(FILE * pfile, const char *fname)
{
if (pfile == stdprn)
fflush(pfile);
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