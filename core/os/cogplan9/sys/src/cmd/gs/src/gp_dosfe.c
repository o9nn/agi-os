#include "stdio_.h"
#include <fcntl.h>
#include "dos_.h"
#include "memory_.h"
#include "string_.h"
#include "gstypes.h"
#include "gsmemory.h"
#include "gsstruct.h"
#include "gp.h"
#include "gsutil.h"
struct file_enum_s {
ff_struct_t ffblk;
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
char *p;
int hsize = 0;
int i;
int dot = 0;
if (pfen == 0)
return 0;
pattern = (char *)gs_alloc_bytes(mem, pat_size,
"gp_enumerate_files(pattern)");
if (pattern == 0)
return 0;
memcpy(pattern, pat, patlen);
p = pattern + patlen;
for (i = 0; i < patlen; i++) {
switch (pat[i]) {
case '*':
*p++ = '*';
while (i < patlen && pat[i] != '.')
i++;
if (i == patlen && !dot) {
*p++ = '.';
*p++ = '*';
}
i--;
continue;
case '.':
dot = 1;
break;
case '\\':
if (i + 1 < patlen && pat[i + 1] == '\\')
i++;
case ':':
case '/':
hsize = p + 1 - (pattern + patlen);
dot = 0;
}
*p++ = pat[i];
}
*p = 0;
pfen->pattern = pattern;
pfen->patlen = patlen;
pfen->pat_size = pat_size;
pfen->head_size = hsize;
pfen->memory = mem;
pfen->first_time = 1;
return pfen;
}
private const string_match_params smp_file =
{'*', '?', -1, true, true};
uint
gp_enumerate_files_next(file_enum * pfen, char *ptr, uint maxlen)
{
int code;
char *p, *q;
uint len;
const char *fpat = pfen->pattern + pfen->patlen;
top:if (pfen->first_time) {
code = dos_findfirst(fpat, &pfen->ffblk);
pfen->first_time = 0;
} else
code = dos_findnext(&pfen->ffblk);
if (code != 0) {
gp_enumerate_files_close(pfen);
return ~(uint) 0;
}
if (maxlen < 13 + pfen->head_size)
return maxlen + 1;
memcpy(ptr, fpat, pfen->head_size);
for (p = &pfen->ffblk.ff_name[0], q = ptr + pfen->head_size; *p; p++)
if (*p != ' ')
*q++ = *p;
len = q - ptr;
if (!string_match(ptr, len, pfen->pattern, pfen->patlen, &smp_file))
goto top;
return len;
}
void
gp_enumerate_files_close(file_enum * pfen)
{
gs_memory_t *mem = pfen->memory;
gs_free_object(mem, pfen->pattern,
"gp_enumerate_files_close(pattern)");
gs_free_object(mem, pfen, "gp_enumerate_files_close");
}