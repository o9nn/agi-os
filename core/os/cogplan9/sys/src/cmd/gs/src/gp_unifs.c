#include "memory_.h"
#include "string_.h"
#include "stdio_.h"
#include "gx.h"
#include "gp.h"
#include "gpmisc.h"
#include "gsstruct.h"
#include "gsutil.h"
#include "stat_.h"
#include "dirent_.h"
#include "unistd_.h"
#include <stdlib.h>
#ifdef FILENAME_MAX
#  if FILENAME_MAX < 80
#    undef FILENAME_MAX
#  endif
#endif
#ifndef FILENAME_MAX
#  define FILENAME_MAX 1024
#endif
extern char *mktemp(char *);
const char gp_scratch_file_name_prefix[] = "gs_";
const char gp_null_file_name[] = "/dev/null";
const char gp_current_directory_name[] = ".";
FILE *
gp_open_scratch_file(const char *prefix, char fname[gp_file_name_sizeof],
const char *mode)
{
int prefix_length = strlen(prefix);
int len = gp_file_name_sizeof - prefix_length - 8;
FILE *fp;
if (gp_file_name_is_absolute(prefix, prefix_length))
*fname = 0;
else if (gp_gettmpdir(fname, &len) != 0)
strcpy(fname, "/tmp/");
else {
if (strlen(fname) != 0 && fname[strlen(fname) - 1] != '/')
strcat(fname, "/");
}
if (strlen(fname) + prefix_length + 8 >= gp_file_name_sizeof)
return 0;
strcat(fname, prefix);
if (*fname != 0 && fname[strlen(fname) - 1] == 'X')
strcat(fname, "-");
strcat(fname, "XXXXXX");
#ifdef HAVE_MKSTEMP
{
int file;
char ofname[gp_file_name_sizeof];
memcpy(ofname, fname, gp_file_name_sizeof);
file = mkstemp(fname);
if (file < -1) {
eprintf1("**** Could not open temporary file %s\n", ofname);
return NULL;
}
fp = fdopen(file, mode);
if (fp == NULL)
close(file);
}
#else
mktemp(fname);
fp = gp_fopentemp(fname, mode);
#endif
if (fp == NULL)
eprintf1("**** Could not open temporary file %s\n", fname);
return fp;
}
FILE *
gp_fopen(const char *fname, const char *mode)
{
return fopen(fname, mode);
}
int
gp_setmode_binary(FILE * pfile, bool mode)
{
return 0;
}
typedef struct dirstack_s dirstack;
struct dirstack_s {
dirstack *next;
DIR *entry;
};
gs_private_st_ptrs1(st_dirstack, dirstack, "dirstack",
dirstack_enum_ptrs, dirstack_reloc_ptrs, next);
struct file_enum_s {
DIR *dirp;
char *pattern;
char *work;
int worklen;
dirstack *dstack;
int patlen;
int pathead;
bool first_time;
gs_memory_t *memory;
};
gs_private_st_ptrs3(st_file_enum, struct file_enum_s, "file_enum",
file_enum_enum_ptrs, file_enum_reloc_ptrs, pattern, work, dstack);
#ifdef DEBUG
private bool
wmatch(const byte * str, uint len, const byte * pstr, uint plen,
const string_match_params * psmp)
{
bool match = string_match(str, len, pstr, plen, psmp);
if (gs_debug_c('e')) {
int i;
dlputs("[e]string_match(\"");
for (i=0; i<len; i++)
errprintf("%c", str[i]);
dputs("\", \"");
for (i=0; i<plen; i++)
errprintf("%c", pstr[i]);
dprintf1("\") = %s\n", (match ? "TRUE" : "false"));
}
return match;
}
#define string_match wmatch
#endif
private char *
rchr(char *str, char ch, int len)
{
register char *p = str + len;
while (p > str)
if (*--p == ch)
return p;
return 0;
}
private bool
popdir(file_enum * pfen)
{
dirstack *d = pfen->dstack;
if (d == 0)
return false;
pfen->dirp = d->entry;
pfen->dstack = d->next;
gs_free_object(pfen->memory, d, "gp_enumerate_files(popdir)");
return true;
}
file_enum *
gp_enumerate_files_init(const char *pat, uint patlen, gs_memory_t * mem)
{
file_enum *pfen;
char *p;
char *work;
if (patlen > FILENAME_MAX)
return 0;
{
const char *p1;
for (p1 = pat; p1 < pat + patlen; p1++)
if (*p1 == 0)
return 0;
}
pfen = gs_alloc_struct(mem, file_enum, &st_file_enum,
"gp_enumerate_files");
if (pfen == 0)
return 0;
pfen->pattern =
(char *)gs_alloc_bytes(mem, patlen + 1,
"gp_enumerate_files(pattern)");
if (pfen->pattern == 0)
return 0;
memcpy(pfen->pattern, pat, patlen);
pfen->pattern[patlen] = 0;
work = (char *)gs_alloc_bytes(mem, FILENAME_MAX + 1,
"gp_enumerate_files(work)");
if (work == 0)
return 0;
pfen->work = work;
p = work;
memcpy(p, pat, patlen);
p += patlen;
*p = 0;
p = pfen->work;
while (!(*p == '*' || *p == '?' || *p == 0))
p++;
while (!(*p == '/' || *p == 0))
p++;
if (*p == '/')
*p = 0;
pfen->pathead = p - work;
p = rchr(work, '/', p - work);
if (!p) {
work[0] = 0;
pfen->worklen = 0;
} else {
if (p == work) {
p++;
}
*p = 0;
pfen->worklen = p - work;
}
pfen->memory = mem;
pfen->dstack = 0;
pfen->first_time = true;
pfen->patlen = patlen;
return pfen;
}
uint
gp_enumerate_files_next(file_enum * pfen, char *ptr, uint maxlen)
{
const dir_entry *de;
char *work = pfen->work;
int worklen = pfen->worklen;
char *pattern = pfen->pattern;
int pathead = pfen->pathead;
int len;
struct stat stbuf;
if (pfen->first_time) {
pfen->dirp = ((worklen == 0) ? opendir(".") : opendir(work));
if_debug1('e', "[e]file_enum:First-Open '%s'\n", work);
pfen->first_time = false;
if (pfen->dirp == 0) {
gp_enumerate_files_close(pfen);
return ~(uint) 0;
}
}
top:de = readdir(pfen->dirp);
if (de == 0) {
char *p;
if_debug0('e', "[e]file_enum:Closedir\n");
closedir(pfen->dirp);
p = rchr(work, '/', worklen);
if (p != 0) {
if (p == work)
p++;
*p = 0;
worklen = p - work;
} else
worklen = 0;
p = rchr(pattern, '/', pathead);
if (p != 0)
pathead = p - pattern;
else
pathead = 0;
if (popdir(pfen)) {
if_debug1('e', "[e]file_enum:Dir popped '%s'\n", work);
goto top;
} else {
if_debug0('e', "[e]file_enum:Dirstack empty\n");
gp_enumerate_files_close(pfen);
return ~(uint) 0;
}
}
len = strlen(de->d_name);
if (len <= 2 && (!strcmp(de->d_name, ".") || !strcmp(de->d_name, "..")))
goto top;
if (len + worklen + 1 > FILENAME_MAX)
goto top;
if (worklen == 0) {
memcpy(work, de->d_name, len + 1);
} else if (worklen == 1 && work[0] == '/') {
memcpy(work + 1, de->d_name, len + 1);
len = len + 1;
} else {
work[worklen] = '/';
memcpy(work + worklen + 1, de->d_name, len + 1);
len = worklen + 1 + len;
}
if (!string_match((byte *) work, len, (byte *) pattern, pathead, NULL))
goto top;
if (pathead < pfen->patlen) {
DIR *dp;
if (((stat(work, &stbuf) >= 0)
? !stat_is_dir(stbuf)
: 0))
goto top;
if (pfen->patlen == pathead + 1) {
if (!stat_is_dir(stbuf)) {
dp = opendir(work);
if (!dp)
goto top;
closedir(dp);
}
work[len++] = '/';
goto winner;
}
dp = opendir(work);
#ifdef DEBUG
{
char save_end = pattern[pathead];
pattern[pathead] = 0;
if_debug2('e', "[e]file_enum:fname='%s', p='%s'\n",
work, pattern);
pattern[pathead] = save_end;
}
#endif
if (!dp)
goto top;
else {
char *p;
dirstack *d;
for (p = pattern + pathead + 1;; p++) {
if (*p == 0) {
pathead = pfen->patlen;
break;
} else if (*p == '/') {
pathead = p - pattern;
break;
}
}
d = gs_alloc_struct(pfen->memory, dirstack,
&st_dirstack,
"gp_enumerate_files(pushdir)");
if (d != 0) {
d->next = pfen->dstack;
d->entry = pfen->dirp;
pfen->dstack = d;
} else
DO_NOTHING;
if_debug1('e', "[e]file_enum:Dir pushed '%s'\n",
work);
worklen = len;
pfen->dirp = dp;
goto top;
}
}
winner:
pfen->worklen = worklen;
pfen->pathead = pathead;
memcpy(ptr, work, len);
return len;
}
void
gp_enumerate_files_close(file_enum * pfen)
{
gs_memory_t *mem = pfen->memory;
if_debug0('e', "[e]file_enum:Cleanup\n");
while (popdir(pfen))
DO_NOTHING;
gs_free_object(mem, (byte *) pfen->work,
"gp_enumerate_close(work)");
gs_free_object(mem, (byte *) pfen->pattern,
"gp_enumerate_files_close(pattern)");
gs_free_object(mem, pfen, "gp_enumerate_files_close");
}