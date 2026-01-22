#include "dos_.h"
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include "stat_.h"
#include "string_.h"
#include "gx.h"
#include "gp.h"
#include "gpmisc.h"
extern char *mktemp(char *);
private FILE *gs_stdprn;
private void handle_FPE(int);
void
gp_init(void)
{
gs_stdprn = 0;
signal(SIGFPE, handle_FPE);
}
private void
handle_FPE(int sig)
{
eprintf("Numeric exception:\n");
exit(1);
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
FILE *pfile;
if (strlen(fname) == 0 || !strcmp(fname, "PRN")) {
#ifdef stdprn
if (!binary_mode)
return stdprn;
if (gs_stdprn == 0) {
int fno = dup(fileno(stdprn));
setmode(fno, O_BINARY);
gs_stdprn = fdopen(fno, "wb");
}
pfile = gs_stdprn;
#else
pfile = fopen("PRN", (binary_mode ? "wb" : "w"));
if (pfile == NULL)
return NULL;
#endif
} else {
pfile = fopen(fname, (binary_mode ? "wb" : "w"));
if (pfile == NULL)
return NULL;
}
gp_set_file_binary(fileno(pfile), binary_mode);
return pfile;
}
void
gp_close_printer(FILE * pfile, const char *fname)
{
#ifdef stdprn
if (pfile != stdprn)
#endif
fclose(pfile);
if (pfile == gs_stdprn)
gs_stdprn = 0;
}
FILE *
gp_open_scratch_file(const char *prefix, char *fname, const char *mode)
{
int prefix_length = strlen(prefix);
int len = gp_file_name_sizeof - prefix_length - 7;
FILE *f;
if (gp_file_name_is_absolute(prefix, prefix_length) ||
gp_gettmpdir(fname, &len) != 0
)
*fname = 0;
else {
char *temp;
for (temp = fname; *temp; temp++)
*temp = tolower(*temp);
if (strlen(fname) && (fname[strlen(fname) - 1] != '\\'))
strcat(fname, "\\");
}
if (strlen(fname) + prefix_length + 7 >= gp_file_name_sizeof)
return 0;
strcat(fname, prefix);
strcat(fname, "XXXXXX");
mktemp(fname);
f = gp_fopentemp(fname, mode);
if (f == NULL)
eprintf1("**** Could not open temporary file %s\n", fname);
return f;
}
FILE *
gp_fopen(const char *fname, const char *mode)
{
return fopen(fname, mode);
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