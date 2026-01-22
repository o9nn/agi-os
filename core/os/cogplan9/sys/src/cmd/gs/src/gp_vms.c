#include "string_.h"
#include "memory_.h"
#include "gx.h"
#include "gp.h"
#include "gpmisc.h"
#include "gsstruct.h"
#include <stat.h>
#include <stdlib.h>
#include <errno.h>
#include <unixio.h>
extern char *getenv(const char *);
#ifdef VMS
# define fopen_VMS fopen
#else
# define fopen_VMS(name, mode, m1, m2) fopen(name, mode)
#endif
#define DSC$K_DTYPE_T 14
#define DSC$K_CLASS_S 1
struct dsc$descriptor_s {
unsigned short dsc$w_length;
unsigned char dsc$b_dtype;
unsigned char dsc$b_class;
char *dsc$a_pointer;
};
typedef struct dsc$descriptor_s descrip;
#define RMS_IS_ERROR_OR_NMF(rmsv) (((rmsv) & 1) == 0)
#define RMS$_NMF 99018
#define RMS$_NORMAL 65537
#define NAM$C_MAXRSS 255
struct file_enum_s {
uint context, length;
descrip pattern;
gs_memory_t *memory;
};
gs_private_st_ptrs1(st_file_enum, struct file_enum_s, "file_enum",
file_enum_enum_ptrs, file_enum_reloc_ptrs, pattern.dsc$a_pointer);
extern uint
LIB$FIND_FILE(descrip *, descrip *, uint *, descrip *, descrip *,
uint *, uint *),
LIB$FIND_FILE_END(uint *),
SYS$FILESCAN(descrip *, uint *, uint *),
SYS$PUTMSG(uint *, int (*)(), descrip *, uint);
private uint
strlength(char *str, uint maxlen, char term)
{
uint i = 0;
while (i < maxlen && str[i] != term)
i++;
return i;
}
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
switch (exit_status) {
case 0:
exit(exit_OK);
case 1:
exit(exit_FAILED);
}
exit(exit_status);
}
void
gp_get_realtime(long *pdt)
{
struct {
uint _l0, _l1;
} binary_date, now, difference;
long LIB$EDIV(), LIB$SUBX(), SYS$BINTIM(), SYS$GETTIM();
long units_per_second = 10000000;
char *jan_1_1980 = "1-JAN-1980 00:00:00.00";
descrip str_desc;
str_desc.dsc$w_length = strlen(jan_1_1980);
str_desc.dsc$a_pointer = jan_1_1980;
(void)SYS$BINTIM(&str_desc, &binary_date);
(void)SYS$GETTIM(&now);
(void)LIB$SUBX(&now, &binary_date, &difference);
(void)LIB$EDIV(&units_per_second, &difference, &pdt[0], &pdt[1]);
pdt[1] *= 100;
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
const char *
gp_getenv_display(void)
{
return getenv("DECW$DISPLAY");
}
FILE *
gp_open_printer(char fname[gp_file_name_sizeof], int binary_mode)
{
if (strlen(fname) == 0)
return 0;
if (binary_mode) {
return fopen_VMS(fname, "w", "rfm = udf", "ctx = stm");
} else {
return fopen_VMS(fname, "w", "rfm = var", "rat = cr");
}
}
void
gp_close_printer(FILE * pfile, const char *fname)
{
fclose(pfile);
}
const char gp_file_name_list_separator = ',';
const char gp_scratch_file_name_prefix[] = "_temp_";
const char gp_null_file_name[] = "NLA0:";
const char gp_current_directory_name[] = "[]";
const char gp_fmode_binary_suffix[] = "";
const char gp_fmode_rb[] = "r";
const char gp_fmode_wb[] = "w";
FILE *
gp_open_scratch_file(const char *prefix, char fname[gp_file_name_sizeof],
const char *mode)
{
FILE *f;
char tmpdir[gp_file_name_sizeof];
int tdlen = gp_file_name_sizeof;
int flen[1];
if (!gp_file_name_is_absolute(prefix, strlen(prefix)) &&
gp_gettmpdir(tmpdir, &tdlen) == 0) {
flen[0] = gp_file_name_sizeof;
if (gp_file_name_combine(tmpdir, tdlen, prefix, strlen(prefix),
false, fname, flen ) != gp_combine_success ) {
return NULL;
}
fname[ *flen ] = 0;
} else {
strcpy(fname, prefix);
}
if (strlen(fname) + 6 >= gp_file_name_sizeof)
return 0;
strcat(fname, "XXXXXX");
mktemp(fname);
f = fopen(fname, mode);
if (f == NULL)
eprintf1("**** Could not open temporary file %s\n", fname);
return f;
}
FILE *
gp_fopen(const char *fname, const char *mode)
{
#ifdef __DECC
#define FAB$C_FIX 1
stat_t buffer;
if (stat((char *)fname, &buffer) == 0)
if (buffer.st_fab_rfm == FAB$C_FIX)
return fopen(fname, mode, "rfm=stmlf", "ctx=stm");
#endif
return fopen(fname, mode);
}
int
gp_setmode_binary(FILE * pfile, bool binary)
{
return 0;
}
private void
gp_free_enumeration(file_enum * pfen)
{
if (pfen) {
LIB$FIND_FILE_END(&pfen->context);
gs_free_object(pfen->memory, pfen->pattern.dsc$a_pointer,
"GP_ENUM(pattern)");
gs_free_object(pfen->memory, pfen,
"GP_ENUM(file_enum)");
}
}
file_enum *
gp_enumerate_files_init(const char *pat, uint patlen, gs_memory_t * mem)
{
file_enum *pfen;
uint i, len;
char *c, *newpat;
bool dot_in_filename = false;
pfen = gs_alloc_struct(mem, file_enum, &st_file_enum,
"GP_ENUM(file_enum)");
newpat = (char *)gs_alloc_bytes(mem, patlen + 2, "GP_ENUM(pattern)");
if (pfen == 0 || newpat == 0) {
gs_free_object(mem, newpat, "GP_ENUM(pattern)");
gs_free_object(mem, pfen, "GP_ENUM(file_enum)");
return (file_enum *) 0;
}
c = newpat;
for (i = 0; i < patlen; pat++, i++)
switch (*pat) {
case '?':
*c++ = '%';
break;
case '\\':
i++;
if (i < patlen)
*c++ = *++pat;
break;
case '.':
case ']':
dot_in_filename = *pat == '.';
default:
*c++ = *pat;
break;
}
if (pat[-1] == '*' && !dot_in_filename) {
*c++ = '.';
*c++ = '*';
}
len = c - newpat;
if (len > 255) {
gs_free_object(mem, newpat, "GP_ENUM(pattern)");
gs_free_object(mem, pfen, "GP_ENUM(file_enum)");
return (file_enum *) 0;
}
pfen->context = 0;
pfen->length = patlen;
pfen->pattern.dsc$w_length = len;
pfen->pattern.dsc$b_dtype = DSC$K_DTYPE_T;
pfen->pattern.dsc$b_class = DSC$K_CLASS_S;
pfen->pattern.dsc$a_pointer = newpat;
pfen->memory = mem;
return pfen;
}
uint
gp_enumerate_files_next(file_enum * pfen, char *ptr, uint maxlen)
{
char *c, filnam[NAM$C_MAXRSS];
descrip result =
{NAM$C_MAXRSS, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
uint i, len;
result.dsc$a_pointer = filnam;
i = LIB$FIND_FILE(&pfen->pattern, &result, &pfen->context,
(descrip *) 0, (descrip *) 0, (uint *) 0, (uint *) 0);
if (RMS_IS_ERROR_OR_NMF(i)) {
gp_free_enumeration(pfen);
return (uint)(-1);
} else if ((len = strlength(filnam, NAM$C_MAXRSS, ' ')) > maxlen)
return maxlen + 1;
c = ptr;
for (i = 0; i < len; i++)
*c++ = filnam[i];
return len;
}
void
gp_enumerate_files_close(file_enum * pfen)
{
gp_free_enumeration(pfen);
}
const char *
gp_strerror(int errnum)
{
return NULL;
}
uint gp_file_name_root(const char *fname, uint len)
{
int i, j;
if (len == 0)
return 0;
for (i = 0; i < len; i++)
if (fname[i] == ':')
break;
if (i == len)
return 0;
if (fname[i] == ':')
i++;
if (i == len || fname[i] != '[')
return i;
i++;
for (j = i; j < len; j++)
if (fname[j] == ']')
break;
if (j == len)
return i;
j++;
if (j == len)
return i;
if (fname[j] != '[')
return i;
return j + 1;
}
uint gs_file_name_check_separator(const char *fname, int len, const char *item)
{
if (len > 0) {
if (fname[0] == ']')
return 1;
if (fname[0] == '.')
return 1;
if (fname[0] == '-') {
if (fname == item + 1 && item[0] == '-')
return 1;
return 1;
}
} else if (len < 0) {
if (fname[-1] == '.' || fname[-1] == ':' || fname[-1] == '[')
return 1;
}
return 0;
}
bool gp_file_name_is_parent(const char *fname, uint len)
{
return len == 1 && fname[0] == '-';
}
bool gp_file_name_is_current(const char *fname, uint len)
{
return len == 0;
}
const char *gp_file_name_separator(void)
{ return "]";
}
const char *gp_file_name_directory_separator(void)
{ return ".";
}
const char *gp_file_name_parent(void)
{ return "-";
}
const char *gp_file_name_current(void)
{ return "";
}
bool gp_file_name_is_partent_allowed(void)
{ return false;
}
bool gp_file_name_is_empty_item_meanful(void)
{ return true;
}
gp_file_name_combine_result
gp_file_name_combine(const char *prefix, uint plen, const char *fname, uint flen,
bool no_sibling, char *buffer, uint *blen)
{
uint rlen, flen1 = flen, plen1 = plen;
const char *fname1 = fname;
if ( plen > 0 && prefix[plen-1] == '\0' )
plen--;
if (plen == 0 && flen == 0) {
if (*blen == 0)
return gp_combine_small_buffer;
buffer[0] = '.';
*blen = 1;
}
rlen = gp_file_name_root(fname, flen);
if (rlen > 0 || plen == 0 || flen == 0) {
if (rlen == 0 && plen != 0) {
fname1 = prefix;
flen1 = plen;
}
if (flen1 + 1 > *blen)
return gp_combine_small_buffer;
memcpy(buffer, fname1, flen1);
buffer[flen1] = 0;
*blen = flen1;
return gp_combine_success;
}
if ( prefix[plen - 1] == ']' && fname[ 0 ] == '-' )
{
memcpy(buffer, prefix, plen - 1 );
fname1 = fname + 1;
flen1 = flen - 1;
memcpy(buffer + plen - 1 , fname1, flen1);
memcpy(buffer + plen + flen1 - 1 , "]" , 1 );
buffer[plen + flen1] = 0;
*blen = plen + flen1;
return gp_combine_success;
}
if ( prefix[plen - 1] == ':' || (prefix[plen - 1] == ']' &&
memchr(fname, ']', flen) == 0) )
{
if (plen + flen + 1 > *blen)
return gp_combine_small_buffer;
memcpy(buffer, prefix, plen);
memcpy(buffer + plen, fname, flen);
buffer[plen + flen] = 0;
*blen = plen + flen;
return gp_combine_success;
}
if ( memchr( prefix , '[' , plen ) == 0 &&
memchr( prefix , '.' , plen ) == 0 )
{
char* tmp_prefix;
int tmp_plen;
if ( prefix[0] == '/' )
{
tmp_prefix = prefix + 1;
tmp_plen = plen - 1;
}
else
{
tmp_prefix = prefix;
tmp_plen = plen;
}
if ( tmp_plen + flen + 2 > *blen)
return gp_combine_small_buffer;
memcpy(buffer, tmp_prefix, tmp_plen);
memcpy(buffer + tmp_plen , ":" , 1 );
memcpy(buffer + tmp_plen + 1, fname, flen);
if ( memchr( fname , '.' , flen ) != 0 )
{
buffer[ tmp_plen + flen + 1] = 0;
*blen = tmp_plen + flen + 1;
}
else
{
memcpy(buffer + tmp_plen + flen + 1 , "." , 1 );
buffer[ tmp_plen + flen + 2] = 0;
*blen = tmp_plen + flen + 2;
}
return gp_combine_success;
}
if (prefix[plen - 1] != ']' && fname[0] == '[')
return gp_combine_cant_handle;
if (fname[0] == '[') {
fname1 = fname + 1;
flen1 = flen - 1;
}
if (prefix[plen - 1] == ']')
plen1 = plen - 1;
return gp_file_name_combine_generic(prefix, plen1,
fname1, flen1, no_sibling, buffer, blen);
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