#include "memory_.h"
#include "gserrors.h"
#include "gserror.h"
#include "strimpl.h"
#include "sarc4.h"
private_st_arcfour_state();
int
s_arcfour_set_key(stream_arcfour_state * state, const unsigned char *key,
int keylength)
{
unsigned int x, y;
unsigned char s, *S = state->S;
if (keylength < 1)
return_error(gs_error_rangecheck);
for (x = 0; x < 256; x++)
S[x] = x;
y = 0;
for (x = 0; x < 256; x++) {
y = (y + S[x] + key[x % keylength]) & 0xFF;
s = S[x];
S[x] = S[y];
S[y] = s;
}
state->x = 0;
state->y = 0;
return 0;
}
private int
s_arcfour_process(stream_state * ss, stream_cursor_read * pr,
stream_cursor_write * pw, bool last)
{
stream_arcfour_state *const state = (stream_arcfour_state *) ss;
unsigned int x = state->x;
unsigned int y = state->y;
unsigned char s, *S = state->S;
unsigned char z;
const unsigned char *limit;
int status;
if ((pr->limit - pr->ptr) > (pw->limit - pw->ptr)) {
limit = pr->ptr + (pw->limit - pw->ptr);
status = 1;
} else {
limit = pr->limit;
status = last ? EOFC : 0;
}
while (pr->ptr < limit) {
x = (x + 1) & 0xFF;
y = (y + S[x]) & 0xFF;
s = S[x];
S[x] = S[y];
S[y] = s;
z = S[(S[x] + S[y]) & 0xFF];
*++pw->ptr = (*++pr->ptr) ^ z;
}
state->x = x;
state->y = y;
return status;
}
const stream_template s_arcfour_template = {
&st_arcfour_state, NULL, s_arcfour_process, 1, 1
};
int
s_arcfour_process_buffer(stream_arcfour_state *ss, byte *buf, int buf_size)
{
stream_cursor_read r;
stream_cursor_write w;
const bool unused = false;
r.ptr = w.ptr = buf - 1;
r.limit = w.limit = buf - 1 + buf_size;
return s_arcfour_process((stream_state *)ss, &r, &w, unused);
}