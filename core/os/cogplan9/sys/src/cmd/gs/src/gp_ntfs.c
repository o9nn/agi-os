#include "stdio_.h"
#include <fcntl.h>
#include <io.h>
#include <stdio.h>
#include "memory_.h"
#include "string_.h"
#include "gstypes.h"
#include "gsmemory.h"
#include "gsstruct.h"
#include "gp.h"
#include "gpmisc.h"
#include "gsutil.h"
#include "windows_.h"
private int
setmode_binary(int fno, bool binary)
{
#if defined(__STDC__) && !defined(__WATCOMC__)
return _setmode(fno, binary ? _O_BINARY : _O_TEXT);
#else
return setmode(fno, binary ? O_BINARY : O_TEXT);
#endif
}
void
gp_set_file_binary(int prnfno, int binary)
{
DISCARD(setmode_binary(prnfno, binary != 0));
}
int
gp_setmode_binary(FILE * pfile, bool binary)
{
#if defined(__STDC__) && !defined(__WATCOMC__)
int code = setmode_binary(_fileno(pfile), binary);
#else
int code = setmode_binary(fileno(pfile), binary);
#endif
return (code == -1 ? -1 : 0);
}
const char gp_file_name_list_separator = ';';
const char gp_fmode_binary_suffix[] = "b";
const char gp_fmode_rb[] = "rb";
const char gp_fmode_wb[] = "wb";
struct file_enum_s {
WIN32_FIND_DATA find_data;
HANDLE find_handle;
char *pattern;
int patlen;
int pat_size;
int head_size;
int first_time;
gs_memory_t *memory;
};
gs_private_st_ptrs1(st_file_enum, struct file_enum_s, "file_enum",
file_enum_enum_ptrs, file_enum_reloc_ptrs, pattern);
file_enum *
gp_enumerate_files_init(const char *pat, uint patlen, gs_memory_t * mem)
{
file_enum *pfen = gs_alloc_struct(mem, file_enum, &st_file_enum, "gp_enumerate_files");
int pat_size = 2 * patlen + 1;
char *pattern;
int hsize = 0;
int i, j;
if (pfen == 0)
return 0;
pattern = (char *)gs_alloc_bytes(mem, pat_size,
"gp_enumerate_files(pattern)");
if (pattern == 0)
return 0;
for (i = 0, j=0; i < patlen; i++) {
if (pat[i] == '\\') {
i++;
if (i == patlen)
break;
}
pattern[j++]=pat[i];
}
for (i = 0; i < j; i++) {
if(pattern[i] == '/' || pattern[i] == '\\' || pattern[i] == ':')
hsize = i+1;
}
pattern[j] = 0;
pfen->pattern = pattern;
pfen->patlen = j;
pfen->pat_size = pat_size;
pfen->head_size = hsize;
pfen->memory = mem;
pfen->first_time = 1;
memset(&pfen->find_data, 0, sizeof(pfen->find_data));
pfen->find_handle = INVALID_HANDLE_VALUE;
return pfen;
}
uint
gp_enumerate_files_next(file_enum * pfen, char *ptr, uint maxlen)
{
int code = 0;
uint len;
for(;;)
{ if (pfen->first_time)
{ pfen->find_handle = FindFirstFile(pfen->pattern, &(pfen->find_data));
if (pfen->find_handle == INVALID_HANDLE_VALUE)
{ code = -1;
break;
}
pfen->first_time = 0;
}
else
{ if (!FindNextFile(pfen->find_handle, &(pfen->find_data)))
{ code = -1;
break;
}
}
if ( strcmp(".",  pfen->find_data.cFileName)
&& strcmp("..", pfen->find_data.cFileName)
&& (pfen->find_data.dwFileAttributes != FILE_ATTRIBUTE_DIRECTORY))
break;
}
if (code != 0) {
gp_enumerate_files_close(pfen);
return ~(uint) 0;
}
len = strlen(pfen->find_data.cFileName);
if (pfen->head_size + len < maxlen) {
memcpy(ptr, pfen->pattern, pfen->head_size);
strcpy(ptr + pfen->head_size, pfen->find_data.cFileName);
return pfen->head_size + len;
}
if (pfen->head_size >= maxlen)
return 0;
memcpy(ptr, pfen->pattern, pfen->head_size);
strncpy(ptr + pfen->head_size, pfen->find_data.cFileName,
maxlen - pfen->head_size - 1);
return maxlen;
}
void
gp_enumerate_files_close(file_enum * pfen)
{
gs_memory_t *mem = pfen->memory;
if (pfen->find_handle != INVALID_HANDLE_VALUE)
FindClose(pfen->find_handle);
gs_free_object(mem, pfen->pattern,
"gp_enumerate_files_close(pattern)");
gs_free_object(mem, pfen, "gp_enumerate_files_close");
}
uint gp_file_name_root(const char *fname, uint len)
{   int i = 0;
if (len == 0)
return 0;
if (len > 1 && fname[0] == '\\' && fname[1] == '\\') {
int k = 0;
for (i = 2; i < len; i++)
if (fname[i] == '\\' || fname[i] == '/')
if (k++) {
i++;
break;
}
} else if (fname[0] == '/' || fname[0] == '\\') {
i = 1;
} else if (len > 1 && fname[1] == ':') {
i = (len > 2 && (fname[2] == '/' || fname[2] == '\\') ? 3 : 2);
}
return i;
}
uint gs_file_name_check_separator(const char *fname, int len, const char *item)
{   if (len > 0) {
if (fname[0] == '/' || fname[0] == '\\')
return 1;
} else if (len < 0) {
if (fname[-1] == '/' || fname[-1] == '\\')
return 1;
}
return 0;
}
bool gp_file_name_is_parent(const char *fname, uint len)
{   return len == 2 && fname[0] == '.' && fname[1] == '.';
}
bool gp_file_name_is_current(const char *fname, uint len)
{   return len == 1 && fname[0] == '.';
}
const char *gp_file_name_separator(void)
{   return "/";
}
const char *gp_file_name_directory_separator(void)
{   return "/";
}
const char *gp_file_name_parent(void)
{   return "..";
}
const char *gp_file_name_current(void)
{   return ".";
}
bool gp_file_name_is_partent_allowed(void)
{   return true;
}
bool gp_file_name_is_empty_item_meanful(void)
{   return false;
}
gp_file_name_combine_result
gp_file_name_combine(const char *prefix, uint plen, const char *fname, uint flen,
bool no_sibling, char *buffer, uint *blen)
{
return gp_file_name_combine_generic(prefix, plen,
fname, flen, no_sibling, buffer, blen);
}