#include "unistd_.h"
#include "fcntl_.h"
#include "stdio_.h"
#include "stat_.h"
#include "memory_.h"
#include "string_.h"
#include "gp.h"
#include "gpgetenv.h"
#include "gpmisc.h"
int
gp_gettmpdir(char *ptr, int *plen)
{
int max_len = *plen;
int code = gp_getenv("TMPDIR", ptr, plen);
if (code != 1)
return code;
*plen = max_len;
return gp_getenv("TEMP", ptr, plen);
}
FILE *
gp_fopentemp(const char *fname, const char *mode)
{
int flags = O_EXCL;
const char *p = mode;
int fildes;
FILE *file;
while (*p)
switch (*p++) {
case 'a':
flags |= O_CREAT | O_APPEND;
break;
case 'r':
flags |= O_RDONLY;
break;
case 'w':
flags |= O_CREAT | O_WRONLY | O_TRUNC;
break;
#ifdef O_BINARY
case 'b':
flags |= O_BINARY;
break;
#endif
case '+':
flags = (flags & ~(O_RDONLY | O_WRONLY)) | O_RDWR;
break;
default:
break;
}
fildes = open(fname, flags, S_IRUSR | S_IWUSR);
if (fildes < 0)
return 0;
file = fdopen(fildes, (char *)mode);
if (file == 0)
close(fildes);
return file;
}
private inline bool
append(char **bp, const char *bpe, const char **ip, uint len)
{
if (bpe - *bp < len)
return false;
memcpy(*bp, *ip, len);
*bp += len;
*ip += len;
return true;
}
private inline uint
search_separator(const char **ip, const char *ipe, const char *item, int direction)
{   uint slen = 0;
for (slen = 0; (*ip - ipe) * direction < 0; (*ip) += direction)
if((slen = gs_file_name_check_separator(*ip, ipe - *ip, item)) != 0)
break;
return slen;
}
gp_file_name_combine_result
gp_file_name_combine_generic(const char *prefix, uint plen, const char *fname, uint flen,
bool no_sibling, char *buffer, uint *blen)
{
char *bp = buffer, *bpe = buffer + *blen;
const char *ip, *ipe;
uint slen;
uint infix_type = 0;
uint infix_len = 0;
uint rlen = gp_file_name_root(fname, flen);
if (rlen != 0) {
ip = fname;
ipe = fname + flen;
} else {
ip = prefix;
ipe = prefix + plen;
rlen = gp_file_name_root(prefix, plen);
}
if (!append(&bp, bpe, &ip, rlen))
return gp_combine_small_buffer;
slen = gs_file_name_check_separator(bp, buffer - bp, bp);
if (rlen != 0 && slen == 0) {
const char *sep = gp_file_name_directory_separator();
slen = strlen(sep);
if (!append(&bp, bpe, &sep, slen))
return gp_combine_small_buffer;
rlen += slen;
}
for (;;) {
const char *item = ip;
uint ilen;
slen = search_separator(&ip, ipe, item, 1);
ilen = ip - item;
if (ilen == 0 && !gp_file_name_is_empty_item_meanful()) {
ip += slen;
slen = 0;
} else if (gp_file_name_is_current(item, ilen)) {
if (bp == buffer) {
if (!append(&bp, bpe, &item, ilen))
return gp_combine_small_buffer;
infix_type = 1;
infix_len = ilen;
} else {
ip += slen;
slen = 0;
}
} else if (!gp_file_name_is_parent(item, ilen)) {
if (!append(&bp, bpe, &item, ilen))
return gp_combine_small_buffer;
} else if (bp == buffer + rlen + infix_len) {
if (rlen != 0)
return gp_combine_cant_handle;
switch (infix_type) {
case 1:
bp = buffer + rlen;
infix_len = 0;
case 0:
if ((no_sibling && ipe == fname + flen && flen != 0) ||
!gp_file_name_is_partent_allowed())
return gp_combine_cant_handle;
case 2:
DO_NOTHING;
}
if (!append(&bp, bpe, &item, ilen))
return gp_combine_small_buffer;
infix_type = 2;
infix_len += ilen;
slen = gs_file_name_check_separator(ip, ipe - ip, ip);
} else {
uint slen1 = gs_file_name_check_separator(bp, buffer + rlen - bp, bp);
char *bie = bp - slen1;
bp = bie;
DISCARD(search_separator((const char **)&bp, buffer + rlen, bp, -1));
ip += gs_file_name_check_separator(ip, ipe - ip, ip);
if (no_sibling) {
const char *p = ip;
DISCARD(search_separator(&p, ipe, ip, 1));
if (p - ip != bie - bp || memcmp(ip, bp, p - ip))
return gp_combine_cant_handle;
}
slen = 0;
}
if (slen) {
if (bp == buffer + rlen + infix_len)
infix_len += slen;
if (!append(&bp, bpe, &ip, slen))
return gp_combine_small_buffer;
}
if (ip == ipe) {
if (ipe == fname + flen) {
const char *zero="";
if (bp == buffer) {
const char *current = gp_file_name_current();
int clen = strlen(current);
if (!append(&bp, bpe, &current, clen))
return gp_combine_small_buffer;
}
*blen = bp - buffer;
if (!append(&bp, bpe, &zero, 1))
return gp_combine_small_buffer;
return gp_combine_success;
} else {
ip = fname;
ipe = fname + flen;
if (slen == 0) {
const char *sep;
slen = search_separator(&ip, ipe, fname, 1);
sep = (slen != 0 ? gp_file_name_directory_separator()
: gp_file_name_separator());
slen = strlen(sep);
if (bp == buffer + rlen + infix_len)
infix_len += slen;
if (!append(&bp, bpe, &sep, slen))
return gp_combine_small_buffer;
ip = fname;
}
}
}
}
}
gp_file_name_combine_result
gp_file_name_reduce(const char *fname, uint flen, char *buffer, uint *blen)
{
return gp_file_name_combine(fname, flen, fname + flen, 0, false, buffer, blen);
}
bool
gp_file_name_is_absolute(const char *fname, uint flen)
{
return (gp_file_name_root(fname, flen) > 0);
}
private uint
gp_file_name_prefix(const char *fname, uint flen,
bool (*test)(const char *fname, uint flen))
{
uint plen = gp_file_name_root(fname, flen), slen;
const char *ip, *ipe;
const char *item = fname;
if (plen > 0)
return 0;
ip = fname + plen;
ipe = fname + flen;
for (; ip < ipe; ) {
item = ip;
slen = search_separator(&ip, ipe, item, 1);
if (!(*test)(item, ip - item))
break;
ip += slen;
}
return item - fname;
}
uint
gp_file_name_parents(const char *fname, uint flen)
{
return gp_file_name_prefix(fname, flen, gp_file_name_is_parent);
}
uint
gp_file_name_cwds(const char *fname, uint flen)
{
return gp_file_name_prefix(fname, flen, gp_file_name_is_current);
}