#include "stdio_.h"
#include "memory_.h"
#include <stdlib.h>
#include "gdebug.h"
#include "strimpl.h"
#include "sfilter.h"
#include "sbwbs.h"
private_st_buffered_state();
private void
s_buffered_set_defaults(stream_state * st)
{
stream_buffered_state *const ss = (stream_buffered_state *) st;
ss->buffer = 0;
}
private int
s_buffered_no_block_init(stream_state * st)
{
stream_buffered_state *const ss = (stream_buffered_state *) st;
ss->buffer = 0;
ss->filling = true;
ss->bpos = 0;
return 0;
}
private int
s_buffered_block_init(stream_state * st)
{
stream_buffered_state *const ss = (stream_buffered_state *) st;
s_buffered_no_block_init(st);
ss->buffer = gs_alloc_bytes(st->memory, ss->BlockSize, "buffer");
if (ss->buffer == 0)
return ERRC;
return 0;
}
private int
s_buffered_process(stream_state * st, stream_cursor_read * pr, bool last)
{
stream_buffered_state *const ss = (stream_buffered_state *) st;
register const byte *p = pr->ptr;
const byte *rlimit = pr->limit;
uint count = rlimit - p;
uint left = ss->bsize - ss->bpos;
if (!ss->filling)
return 1;
if (left < count)
count = left;
if_debug3('w', "[w]buffering %d bytes to position %d, last = %s\n",
count, ss->bpos, (last ? "true" : "false"));
memcpy(ss->buffer + ss->bpos, p + 1, count);
pr->ptr = p += count;
ss->bpos += count;
if (ss->bpos == ss->bsize || (p == rlimit && last)) {
ss->filling = false;
return 1;
}
return 0;
}
private void
s_buffered_release(stream_state * st)
{
stream_buffered_state *const ss = (stream_buffered_state *) st;
gs_free_object(st->memory, ss->buffer, "buffer");
}
private_st_BWBS_state();
private void s_BWBS_release(stream_state *);
private void
s_BWBS_set_defaults(stream_state * st)
{
stream_BWBS_state *const ss = (stream_BWBS_state *) st;
s_buffered_set_defaults(st);
ss->offsets = 0;
}
private int
bwbs_init(stream_state * st, uint osize)
{
stream_BWBS_state *const ss = (stream_BWBS_state *) st;
int code;
ss->bsize = ss->BlockSize;
code = s_buffered_block_init(st);
if (code != 0)
return code;
ss->offsets = (void *)gs_alloc_bytes(st->memory, osize,
"BWBlockSort offsets");
if (ss->offsets == 0) {
s_BWBS_release(st);
return ERRC;
}
ss->I = -1;
return 0;
}
private void
s_BWBS_release(stream_state * st)
{
stream_BWBS_state *const ss = (stream_BWBS_state *) st;
gs_free_object(st->memory, ss->offsets, "BWBlockSort offsets");
s_buffered_release(st);
}
private int
s_BWBSE_init(stream_state * st)
{
stream_BWBS_state *const ss = (stream_BWBS_state *) st;
return bwbs_init(st, ss->BlockSize * sizeof(int));
}
private stream_BWBS_state *bwbs_compare_ss;
private int
bwbs_compare_rotations(const void *p1, const void *p2)
{
const byte *buffer = bwbs_compare_ss->buffer;
const byte *s1 = buffer + *(const int *)p1;
const byte *s2 = buffer + *(const int *)p2;
const byte *start1;
const byte *end;
int swap;
if (*s1 != *s2)
return (*s1 < *s2 ? -1 : 1);
if (s1 < s2)
swap = 1;
else {
const byte *t = s1;
s1 = s2;
s2 = t;
swap = -1;
}
start1 = s1;
end = buffer + bwbs_compare_ss->N;
for (s1++, s2++; s2 < end; s1++, s2++)
if (*s1 != *s2)
return (*s1 < *s2 ? -swap : swap);
s2 = buffer;
for (; s1 < end; s1++, s2++)
if (*s1 != *s2)
return (*s1 < *s2 ? -swap : swap);
s1 = buffer;
for (; s1 < start1; s1++, s2++)
if (*s1 != *s2)
return (*s1 < *s2 ? -swap : swap);
return 0;
}
private void
bwbse_sort(const byte * buffer, uint * indices, int N)
{
offsets_full Cs;
#define C Cs.v
uint sum = 0, j, ch;
memset(C, 0, sizeof(Cs));
for (j = 0; j < N; j++)
C[buffer[j]]++;
for (ch = 0; ch <= 255; ch++) {
sum += C[ch];
C[ch] = sum - C[ch];
}
for (j = 0; j < N; j++)
indices[C[buffer[j]]++] = j;
sum = 0;
for (ch = 0; ch <= 255; sum = C[ch], ch++)
qsort(indices + sum, C[ch] - sum,
sizeof(*indices),
bwbs_compare_rotations);
#undef C
}
private int
s_BWBSE_process(stream_state * st, stream_cursor_read * pr,
stream_cursor_write * pw, bool last)
{
stream_BWBS_state *const ss = (stream_BWBS_state *) st;
register byte *q = pw->ptr;
byte *wlimit = pw->limit;
uint wcount = wlimit - q;
uint *indices = ss->offsets;
if (ss->filling) {
int status, j, N;
byte *buffer = ss->buffer;
if (wcount < sizeof(int) * 2)
return 1;
status = s_buffered_process(st, pr, last);
if (!status)
return 0;
ss->N = N = ss->bpos;
for (j = N / 2 - 1; j >= 0; j--) {
byte *p0 = &buffer[j];
byte *p1 = &buffer[N - 1 - j];
byte b = *p0;
*p0 = *p1;
*p1 = b;
}
bwbs_compare_ss = ss;
bwbse_sort(buffer, indices, N);
for (j = 0; j < N; j++)
if (indices[j] == 0) {
ss->I = j;
break;
}
for (j = sizeof(int); --j >= 0;)
*++q = (byte) (N >> (j * 8));
for (j = sizeof(int); --j >= 0;)
*++q = (byte) (ss->I >> (j * 8));
ss->bpos = 0;
}
while (q < wlimit && ss->bpos < ss->N) {
int i = indices[ss->bpos++];
*++q = ss->buffer[(i == 0 ? ss->N - 1 : i - 1)];
}
if (ss->bpos == ss->N) {
ss->filling = true;
ss->bpos = 0;
}
pw->ptr = q;
if (q == wlimit)
return 1;
return 0;
}
const stream_template s_BWBSE_template = {
&st_BWBS_state, s_BWBSE_init, s_BWBSE_process, sizeof(int) * 2, 1,
s_BWBS_release, s_BWBS_set_defaults
};
#define SHORT_OFFSETS
#ifdef SHORT_OFFSETS
typedef struct {
bits16 v[256];
} offsets_4k;
#if arch_sizeof_int > 2
#  define ceil_64k(n) (((n) + 0xffff) >> 16)
#else
#  define ceil_64k(n) 1
#endif
#define ceil_4k(n) (((n) + 0xfff) >> 12)
#define offset_space(bsize)\
(ceil_64k(bsize) * sizeof(offsets_full) +\
ceil_4k(bsize) * sizeof(offsets_4k) +\
((bsize + 1) >> 1) * 3)
#else
#define offset_space(bsize)\
(bsize * sizeof(int))
#endif
private int
s_BWBSD_init(stream_state * st)
{
stream_BWBS_state *const ss = (stream_BWBS_state *) st;
uint bsize = ss->BlockSize;
return bwbs_init(st, offset_space(bsize));
}
#ifdef SHORT_OFFSETS
private void
bwbsd_construct_offsets(stream_BWBS_state * sst, offsets_full * po64k,
offsets_4k * po4k, byte * po1, int N)
{
offsets_full Cs;
#define C Cs.v
uint i1;
byte *b = sst->buffer;
offsets_full *p2 = po64k - 1;
offsets_4k *p1 = po4k;
byte *p0 = po1;
memset(C, 0, sizeof(Cs));
for (i1 = 0; i1 < ceil_4k(N); i1++, p1++) {
int j;
if (!(i1 & 15))
*++p2 = Cs;
for (j = 0; j < 256; j++)
p1->v[j] = C[j] - p2->v[j];
j = (N + 1 - (i1 << 12)) >> 1;
if (j > 4096 / 2)
j = 4096 / 2;
for (; j > 0; j--, b += 2, p0 += 3) {
byte b0 = b[0];
uint d0 = C[b0]++ - (p1->v[b0] + p2->v[b0]);
byte b1 = b[1];
uint d1 = C[b1]++ - (p1->v[b1] + p2->v[b1]);
p0[0] = d0 >> 4;
p0[1] = (byte) ((d0 << 4) + (d1 >> 8));
p0[2] = (byte) d1;
}
}
if (N & 1)
C[sst->buffer[N]]--;
{
int sum = 0, ch;
for (ch = 0; ch <= 255; ch++) {
sum += C[ch];
C[ch] = sum - C[ch];
}
}
{
int i2, ch;
for (p2 = po64k, i2 = ceil_64k(N); i2 > 0; p2++, i2--)
for (ch = 0; ch < 256; ch++)
p2->v[ch] += C[ch];
}
#undef C
}
#else
private void
bwbsd_construct_offsets(stream_BWBS_state * sst, int *po, int N)
{
offsets_full Cs;
#define C Cs.v
uint i;
byte *b = sst->buffer;
int *p = po;
memset(C, 0, sizeof(Cs));
for (i = 0; i < N; i++, p++, b++)
*p = C[*b]++;
{
int sum = 0, ch;
for (ch = 0; ch <= 255; ch++) {
sum += C[ch];
C[ch] = sum - C[ch];
}
}
for (i = 0, b = sst->buffer, p = po; i < N; i++, b++, p++)
*p += C[*b];
#undef C
}
#endif
private int
s_BWBSD_process(stream_state * st, stream_cursor_read * pr,
stream_cursor_write * pw, bool last)
{
stream_BWBS_state *const ss = (stream_BWBS_state *) st;
register const byte *p = pr->ptr;
const byte *rlimit = pr->limit;
uint count = rlimit - p;
register byte *q = pw->ptr;
byte *wlimit = pw->limit;
#ifdef SHORT_OFFSETS
uint BlockSize = ss->BlockSize;
offsets_full *po64k = ss->offsets;
offsets_4k *po4k = (offsets_4k *) (po64k + ceil_64k(BlockSize));
byte *po1 = (byte *) (po4k + ceil_4k(BlockSize));
#else
int *po = ss->offsets;
#endif
if (ss->I < 0) {
int I, N, j;
if (count < sizeof(int) * 2)
return 0;
for (N = 0, j = 0; j < sizeof(int); j++)
N = (N << 8) + *++p;
for (I = 0, j = 0; j < sizeof(int); j++)
I = (I << 8) + *++p;
ss->N = N;
ss->I = I;
if_debug2('w', "[w]N=%d I=%d\n", N, I);
pr->ptr = p;
if (N < 0 || N > ss->BlockSize || I < 0 || I >= N)
return ERRC;
if (N == 0)
return EOFC;
count -= sizeof(int) * 2;
ss->bpos = 0;
ss->bsize = N;
}
if (ss->filling) {
if (!s_buffered_process(st, pr, last))
return 0;
#ifdef SHORT_OFFSETS
bwbsd_construct_offsets(ss, po64k, po4k, po1, ss->bsize);
#else
bwbsd_construct_offsets(ss, po, ss->bsize);
#endif
ss->bpos = 0;
ss->i = ss->I;
}
while (q < wlimit && ss->bpos < ss->bsize) {
int i = ss->i;
byte b = ss->buffer[i];
#ifdef SHORT_OFFSETS
uint d;
const byte *pd = &po1[(i >> 1) + i];
*++q = b;
if (!(i & 1))
d = ((uint) pd[0] << 4) + (pd[1] >> 4);
else
d = ((pd[0] & 0xf) << 8) + pd[1];
ss->i = po64k[i >> 16].v[b] + po4k[i >> 12].v[b] + d;
#else
*++q = b;
ss->i = po[i];
#endif
ss->bpos++;
}
if (ss->bpos == ss->bsize) {
ss->I = -1;
ss->filling = true;
}
pw->ptr = q;
if (q == wlimit)
return 1;
return 0;
}
const stream_template s_BWBSD_template = {
&st_BWBS_state, s_BWBSD_init, s_BWBSD_process, 1, sizeof(int) * 2,
s_BWBS_release, s_BWBS_set_defaults
};