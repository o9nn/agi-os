#include "stdio_.h"
#include "memory_.h"
#include "gdebug.h"
#include "gpcheck.h"
#include "stream.h"
#include "strimpl.h"
private int sreadbuf(stream *, stream_cursor_write *);
private int swritebuf(stream *, stream_cursor_read *, bool);
private void stream_compact(stream *, bool);
public_st_stream();
public_st_stream_state();
private
ENUM_PTRS_WITH(stream_enum_ptrs, stream *st) return 0;
case 0:
if (st->foreign)
ENUM_RETURN(NULL);
else if (st->cbuf_string.data != 0)
ENUM_RETURN_STRING_PTR(stream, cbuf_string);
else
ENUM_RETURN(st->cbuf);
ENUM_PTR3(1, stream, strm, prev, next);
ENUM_PTR(4, stream, state);
case 5: return ENUM_CONST_STRING(&st->file_name);
ENUM_PTRS_END
private RELOC_PTRS_WITH(stream_reloc_ptrs, stream *st)
{
byte *cbuf_old = st->cbuf;
if (cbuf_old != 0 && !st->foreign) {
long reloc;
if (st->cbuf_string.data != 0) {
RELOC_STRING_VAR(st->cbuf_string);
st->cbuf = st->cbuf_string.data;
} else
RELOC_VAR(st->cbuf);
reloc = cbuf_old - st->cbuf;
st->srptr -= reloc;
st->srlimit -= reloc;
st->swlimit -= reloc;
}
RELOC_VAR(st->strm);
RELOC_VAR(st->prev);
RELOC_VAR(st->next);
RELOC_VAR(st->state);
RELOC_CONST_STRING_VAR(st->file_name);
}
RELOC_PTRS_END
private void
stream_finalize(void *vptr)
{
stream *const st = vptr;
if_debug2('u', "[u]%s 0x%lx\n",
(!s_is_valid(st) ? "already closed:" :
st->is_temp ? "is_temp set:" :
st->file == 0 ? "not file:" :
"closing file:"), (ulong) st);
if (s_is_valid(st) && !st->is_temp && st->file != 0) {
st->cbuf = 0;
st->cbuf_string.data = 0;
sclose(st);
}
}
private const stream_template s_no_template = {
&st_stream_state, 0, 0, 1, 1, 0
};
void
s_init(stream *s, gs_memory_t * mem)
{
s->memory = mem;
s->report_error = s_no_report_error;
s->min_left = 0;
s->error_string[0] = 0;
s->prev = s->next = 0;
s->file_name.data = 0;
s->file_name.size = 0;
s->close_strm = false;
s->close_at_eod = true;
}
stream *
s_alloc(gs_memory_t * mem, client_name_t cname)
{
stream *s = gs_alloc_struct(mem, stream, &st_stream, cname);
if_debug2('s', "[s]alloc(%s) = 0x%lx\n",
client_name_string(cname), (ulong) s);
if (s == 0)
return 0;
s_init(s, mem);
return s;
}
void
s_init_state(stream_state *st, const stream_template *template,
gs_memory_t *mem)
{
st->template = template;
st->memory = mem;
st->report_error = s_no_report_error;
st->min_left = 0;
}
stream_state *
s_alloc_state(gs_memory_t * mem, gs_memory_type_ptr_t stype,
client_name_t cname)
{
stream_state *st = gs_alloc_struct(mem, stream_state, stype, cname);
if_debug3('s', "[s]alloc_state %s(%s) = 0x%lx\n",
client_name_string(cname),
client_name_string(stype->sname),
(ulong) st);
if (st)
s_init_state(st, NULL, mem);
return st;
}
void
s_std_init(register stream * s, byte * ptr, uint len, const stream_procs * pp,
int modes)
{
s->template = &s_no_template;
s->cbuf = ptr;
s->srptr = s->srlimit = s->swptr = ptr - 1;
s->swlimit = ptr - 1 + len;
s->end_status = 0;
s->foreign = 0;
s->modes = modes;
s->cbuf_string.data = 0;
s->position = 0;
s->bsize = s->cbsize = len;
s->strm = 0;
s->is_temp = 0;
s->procs = *pp;
s->state = (stream_state *) s;
s->file = 0;
s->file_name.data = 0;
s->file_name.size = 0;
if_debug4('s', "[s]init 0x%lx, buf=0x%lx, len=%u, modes=%d\n",
(ulong) s, (ulong) ptr, len, modes);
}
int
ssetfilename(stream *s, const byte *data, uint size)
{
byte *str =
(s->file_name.data == 0 ?
gs_alloc_string(s->memory, size + 1, "ssetfilename") :
gs_resize_string(s->memory,
(byte *)s->file_name.data,
s->file_name.size,
size + 1, "ssetfilename"));
if (str == 0)
return -1;
memcpy(str, data, size);
str[size] = 0;
s->file_name.data = str;
s->file_name.size = size + 1;
return 0;
}
int
sfilename(stream *s, gs_const_string *pfname)
{
pfname->data = s->file_name.data;
if (pfname->data == 0) {
pfname->size = 0;
return -1;
}
pfname->size = s->file_name.size - 1;
return 0;
}
int
s_std_null(stream * s)
{
return 0;
}
void
s_std_read_reset(stream * s)
{
s->srptr = s->srlimit = s->cbuf - 1;
}
void
s_std_write_reset(stream * s)
{
s->swptr = s->cbuf - 1;
}
int
s_std_read_flush(stream * s)
{
while (1) {
s->srptr = s->srlimit = s->cbuf - 1;
if (s->end_status)
break;
s_process_read_buf(s);
}
return (s->end_status == EOFC ? 0 : s->end_status);
}
int
s_std_write_flush(stream * s)
{
return s_process_write_buf(s, false);
}
int
s_std_noavailable(stream * s, long *pl)
{
*pl = -1;
return 0;
}
int
s_std_noseek(stream * s, long pos)
{
return ERRC;
}
int
s_std_close(stream * s)
{
return 0;
}
int
s_std_switch_mode(stream * s, bool writing)
{
return ERRC;
}
void
s_disable(register stream * s)
{
s->cbuf = 0;
s->bsize = 0;
s->end_status = EOFC;
s->modes = 0;
s->cbuf_string.data = 0;
s->cursor.r.ptr = s->cursor.r.limit = 0;
s->cursor.w.limit = 0;
s->procs.close = s_std_null;
s->strm = 0;
s->state = (stream_state *) s;
s->template = &s_no_template;
if (s->file_name.data) {
gs_free_const_string(s->memory, s->file_name.data, s->file_name.size,
"s_disable(file_name)");
s->file_name.data = 0;
s->file_name.size = 0;
}
if_debug1('s', "[s]disable 0x%lx\n", (ulong) s);
}
int
s_filter_write_flush(register stream * s)
{
int status = s_process_write_buf(s, false);
if (status != 0)
return status;
return sflush(s->strm);
}
int
s_filter_close(register stream * s)
{
int status;
bool close = s->close_strm;
stream *stemp = s->strm;
if (s_is_writing(s)) {
int status = s_process_write_buf(s, true);
if (status != 0 && status != EOFC)
return status;
status = sflush(stemp);
if (status != 0 && status != EOFC)
return status;
}
status = s_std_close(s);
if (status != 0 && status != EOFC)
return status;
if (close && stemp != 0)
return sclose(stemp);
return status;
}
int
s_no_report_error(stream_state * st, const char *str)
{
return 0;
}
const stream_procs s_filter_read_procs = {
s_std_noavailable, s_std_noseek, s_std_read_reset,
s_std_read_flush, s_filter_close
};
const stream_procs s_filter_write_procs = {
s_std_noavailable, s_std_noseek, s_std_write_reset,
s_filter_write_flush, s_filter_close
};
int
savailable(stream * s, long *pl)
{
return (*(s)->procs.available) (s, pl);
}
long
stell(stream * s)
{
const byte *ptr = (s_is_writing(s) ? s->swptr : s->srptr);
return (ptr == 0 ? 0 : ptr + 1 - s->cbuf) + s->position;
}
int
spseek(stream * s, long pos)
{
if_debug3('s', "[s]seek 0x%lx to %ld, position was %ld\n",
(ulong) s, pos, stell(s));
return (*(s)->procs.seek) (s, pos);
}
int
sswitch(register stream * s, bool writing)
{
if (s->procs.switch_mode == 0)
return ERRC;
return (*s->procs.switch_mode) (s, writing);
}
int
sclose(register stream * s)
{
stream_state *st;
int status = (*s->procs.close) (s);
if (status < 0)
return status;
st = s->state;
if (st != 0) {
stream_proc_release((*release)) = st->template->release;
if (release != 0)
(*release) (st);
if (st != (stream_state *) s && st->memory != 0)
gs_free_object(st->memory, st, "s_std_close");
s->state = (stream_state *) s;
}
s_disable(s);
return status;
}
int
spgetcc(register stream * s, bool close_at_eod)
{
int status, left;
int min_left = sbuf_min_left(s);
while (status = s->end_status,
left = s->srlimit - s->srptr,
left <= min_left && status >= 0
)
s_process_read_buf(s);
if (left <= min_left &&
(left == 0 || (status != EOFC && status != ERRC))
) {
stream_compact(s, true);
if (status == EOFC && close_at_eod && s->close_at_eod) {
status = sclose(s);
if (status == 0)
status = EOFC;
s->end_status = status;
}
return status;
}
return *++(s->srptr);
}
int
spputc(register stream * s, byte b)
{
for (;;) {
if (s->end_status)
return s->end_status;
if (!sendwp(s)) {
*++(s->swptr) = b;
return b;
}
s_process_write_buf(s, false);
}
}
int
sungetc(register stream * s, byte c)
{
if (!s_is_reading(s) || s->srptr < s->cbuf || *(s->srptr) != c)
return ERRC;
s->srptr--;
return 0;
}
int
sgets(stream * s, byte * buf, uint nmax, uint * pn)
{
stream_cursor_write cw;
int status = 0;
int min_left = sbuf_min_left(s);
cw.ptr = buf - 1;
cw.limit = cw.ptr + nmax;
while (cw.ptr < cw.limit) {
int left;
if ((left = s->srlimit - s->srptr) > min_left) {
s->srlimit -= min_left;
stream_move(&s->cursor.r, &cw);
s->srlimit += min_left;
} else {
uint wanted = cw.limit - cw.ptr;
int c;
stream_state *st;
if (wanted >= s->bsize >> 2 &&
(st = s->state) != 0 &&
wanted >= st->template->min_out_size &&
s->end_status == 0 &&
left == 0
) {
byte *wptr = cw.ptr;
cw.limit -= min_left;
status = sreadbuf(s, &cw);
cw.limit += min_left;
stream_compact(s, true);
s->srptr = s->srlimit = s->cbuf - 1;
s->position += cw.ptr - wptr;
if (status != 1 || cw.ptr == cw.limit)
break;
}
c = spgetc(s);
if (c < 0) {
status = c;
break;
}
*++(cw.ptr) = c;
}
}
*pn = cw.ptr + 1 - buf;
return (status >= 0 ? 0 : status);
}
int
sputs(register stream * s, const byte * str, uint wlen, uint * pn)
{
uint len = wlen;
int status = s->end_status;
if (status >= 0)
while (len > 0) {
uint count = s->swlimit - s->swptr;
if (count > 0) {
if (count > len)
count = len;
memcpy(s->swptr + 1, str, count);
s->swptr += count;
str += count;
len -= count;
} else {
byte ch = *str++;
status = sputc(s, ch);
if (status < 0)
break;
len--;
}
}
*pn = wlen - len;
return (status >= 0 ? 0 : status);
}
int
spskip(register stream * s, long nskip, long *pskipped)
{
long n = nskip;
int min_left;
if (nskip < 0 || !s_is_reading(s)) {
*pskipped = 0;
return ERRC;
}
if (s_can_seek(s)) {
long pos = stell(s);
int status = sseek(s, pos + n);
*pskipped = stell(s) - pos;
return status;
}
min_left = sbuf_min_left(s);
while (sbufavailable(s) < n + min_left) {
int status;
n -= sbufavailable(s);
s->srptr = s->srlimit;
if (s->end_status) {
*pskipped = nskip - n;
return s->end_status;
}
status = sgetc(s);
if (status < 0) {
*pskipped = nskip - n;
return status;
}
--n;
}
s->srptr += n;
*pskipped = nskip;
return 0;
}
int
sreadline(stream *s_in, stream *s_out, void *readline_data,
gs_const_string *prompt, gs_string * buf,
gs_memory_t * bufmem, uint * pcount, bool *pin_eol,
bool (*is_stdin)(const stream *))
{
uint count = *pcount;
#if '\n' == '\r'
#  define LF 0xa
#else
#  define LF '\n'
#endif
if (count == 0 && s_out && prompt) {
uint ignore_n;
int ch = sputs(s_out, prompt->data, prompt->size, &ignore_n);
if (ch < 0)
return ch;
}
top:
if (*pin_eol) {
int ch = spgetcc(s_in, false);
if (ch == EOFC) {
*pin_eol = false;
return 0;
} else if (ch < 0)
return ch;
else if (ch != LF)
sputback(s_in);
*pin_eol = false;
return 0;
}
for (;;) {
int ch = sgetc(s_in);
if (ch < 0) {
*pcount = count;
return ch;
}
switch (ch) {
case '\r':
{
#if '\n' == '\r'
if (!is_stdin(s_in))
#endif
{
*pcount = count;
*pin_eol = true;
goto top;
}
}
case LF:
#undef LF
*pcount = count;
return 0;
}
if (count >= buf->size) {
if (!bufmem) {
sputback(s_in);
*pcount = count;
return 1;
}
{
uint nsize = count + max(count, 20);
byte *ndata = gs_resize_string(bufmem, buf->data, buf->size,
nsize, "sreadline(buffer)");
if (ndata == 0)
return ERRC;
buf->data = ndata;
buf->size = nsize;
}
}
buf->data[count++] = ch;
}
}
int
s_process_read_buf(stream * s)
{
int status;
stream_compact(s, false);
status = sreadbuf(s, &s->cursor.w);
s->end_status = (status >= 0 ? 0 : status);
return 0;
}
int
s_process_write_buf(stream * s, bool last)
{
int status = swritebuf(s, &s->cursor.r, last);
stream_compact(s, false);
return (status >= 0 ? 0 : status);
}
#define MOVE_BACK(curr, prev)\
BEGIN\
stream *back = prev->strm;\
prev->strm = curr; curr = prev; prev = back;\
END
#define MOVE_AHEAD(curr, prev)\
BEGIN\
stream *ahead = curr->strm;\
curr->strm = prev; prev = curr; curr = ahead;\
END
private int
sreadbuf(stream * s, stream_cursor_write * pbuf)
{
stream *prev = 0;
stream *curr = s;
int status;
for (;;) {
stream *strm;
stream_cursor_write *pw;
byte *oldpos;
for (;;) {
stream_cursor_read cr;
stream_cursor_read *pr;
int left;
bool eof;
strm = curr->strm;
if (strm == 0) {
cr.ptr = 0, cr.limit = 0;
pr = &cr;
left = 0;
eof = false;
} else {
pr = &strm->cursor.r;
left = sbuf_min_left(strm);
left = min(left, pr->limit - pr->ptr);
pr->limit -= left;
eof = strm->end_status == EOFC;
}
pw = (prev == 0 ? pbuf : &curr->cursor.w);
if_debug4('s', "[s]read process 0x%lx, nr=%u, nw=%u, eof=%d\n",
(ulong) curr, (uint) (pr->limit - pr->ptr),
(uint) (pw->limit - pw->ptr), eof);
oldpos = pw->ptr;
status = (*curr->procs.process) (curr->state, pr, pw, eof);
pr->limit += left;
if_debug5('s', "[s]after read 0x%lx, nr=%u, nw=%u, status=%d, position=%d\n",
(ulong) curr, (uint) (pr->limit - pr->ptr),
(uint) (pw->limit - pw->ptr), status, s->position);
if (strm == 0 || status != 0)
break;
if (strm->end_status < 0) {
if (strm->end_status != EOFC || pw->ptr == oldpos)
status = strm->end_status;
break;
}
MOVE_AHEAD(curr, prev);
stream_compact(curr, false);
}
if ((strm != 0 || curr->file) && status == EOFC &&
curr->cursor.r.ptr >= curr->cursor.r.limit &&
curr->close_at_eod
) {
int cstat = sclose(curr);
if (cstat != 0)
status = cstat;
}
curr->end_status = (status >= 0 ? 0 : status);
if (prev == 0)
return status;
MOVE_BACK(curr, prev);
}
}
private int
swritebuf(stream * s, stream_cursor_read * pbuf, bool last)
{
stream *prev = 0;
stream *curr = s;
int depth = 0;
int status;
for (;;) {
for (;;) {
stream *strm = curr->strm;
stream_cursor_write cw;
stream_cursor_read *pr;
stream_cursor_write *pw;
bool end = last &&
(prev == 0 ||
(depth <= 1 && prev->end_status == EOFC));
if (strm == 0)
cw.ptr = 0, cw.limit = 0, pw = &cw;
else
pw = &strm->cursor.w;
if (prev == 0)
pr = pbuf;
else
pr = &curr->cursor.r;
if_debug5('s',
"[s]write process 0x%lx(%s), nr=%u, nw=%u, end=%d\n",
(ulong)curr,
gs_struct_type_name(curr->state->template->stype),
(uint)(pr->limit - pr->ptr),
(uint)(pw->limit - pw->ptr), end);
status = curr->end_status;
if (status >= 0) {
status = (*curr->procs.process)(curr->state, pr, pw, end);
if_debug5('s',
"[s]after write 0x%lx, nr=%u, nw=%u, end=%d, status=%d\n",
(ulong) curr, (uint) (pr->limit - pr->ptr),
(uint) (pw->limit - pw->ptr), end, status);
if (status == 0 && end)
status = EOFC;
if (status == EOFC || status == ERRC)
curr->end_status = status;
}
if (strm == 0 || (status < 0 && status != EOFC))
break;
if (status != 1) {
if (!end || !strm->is_temp)
break;
}
status = strm->end_status;
if (status < 0)
break;
if (!curr->is_temp)
++depth;
if_debug1('s', "[s]moving ahead, depth = %d\n", depth);
MOVE_AHEAD(curr, prev);
stream_compact(curr, false);
}
curr->end_status = (status >= 0 ? 0 : status);
if (status < 0 || prev == 0) {
while (prev) {
if_debug0('s', "[s]unwinding\n");
MOVE_BACK(curr, prev);
if (status >= 0)
curr->end_status = 0;
else if (status == ERRC)
curr->end_status = ERRC;
}
return status;
}
MOVE_BACK(curr, prev);
if (!curr->is_temp)
--depth;
if_debug1('s', "[s]moving back, depth = %d\n", depth);
}
}
int
stream_move(stream_cursor_read * pr, stream_cursor_write * pw)
{
uint rcount = pr->limit - pr->ptr;
uint wcount = pw->limit - pw->ptr;
uint count;
int status;
if (rcount <= wcount)
count = rcount, status = 0;
else
count = wcount, status = 1;
memmove(pw->ptr + 1, pr->ptr + 1, count);
pr->ptr += count;
pw->ptr += count;
return status;
}
private void
stream_compact(stream * s, bool always)
{
if (s->cursor.r.ptr >= s->cbuf && (always || s->end_status >= 0)) {
uint dist = s->cursor.r.ptr + 1 - s->cbuf;
memmove(s->cbuf, s->cursor.r.ptr + 1,
(uint) (s->cursor.r.limit - s->cursor.r.ptr));
s->cursor.r.ptr = s->cbuf - 1;
s->cursor.r.limit -= dist;
s->position += dist;
}
}
private int
s_string_available(stream *, long *),
s_string_read_seek(stream *, long),
s_string_write_seek(stream *, long),
s_string_read_process(stream_state *, stream_cursor_read *,
stream_cursor_write *, bool),
s_string_write_process(stream_state *, stream_cursor_read *,
stream_cursor_write *, bool);
void
sread_string(register stream *s, const byte *ptr, uint len)
{
static const stream_procs p = {
s_string_available, s_string_read_seek, s_std_read_reset,
s_std_read_flush, s_std_null, s_string_read_process
};
s_std_init(s, (byte *)ptr, len, &p, s_mode_read + s_mode_seek);
s->cbuf_string.data = (byte *)ptr;
s->cbuf_string.size = len;
s->end_status = EOFC;
s->srlimit = s->swlimit;
}
private void
s_string_reusable_reset(stream *s)
{
s->srptr = s->cbuf - 1;
s->srlimit = s->srptr + s->bsize;
}
private int
s_string_reusable_flush(stream *s)
{
s->srptr = s->srlimit = s->cbuf + s->bsize - 1;
return 0;
}
void
sread_string_reusable(stream *s, const byte *ptr, uint len)
{
static const stream_procs p = {
s_string_available, s_string_read_seek, s_string_reusable_reset,
s_string_reusable_flush, s_std_null, s_string_read_process
};
sread_string(s, ptr, len);
s->procs = p;
s->close_at_eod = false;
}
private int
s_string_available(stream *s, long *pl)
{
*pl = sbufavailable(s);
if (*pl == 0 && s->close_at_eod)
*pl = -1;
return 0;
}
private int
s_string_read_seek(register stream * s, long pos)
{
if (pos < 0 || pos > s->bsize)
return ERRC;
s->srptr = s->cbuf + pos - 1;
s->srlimit = s->cbuf + s->bsize - 1;
s->position = 0;
return 0;
}
void
swrite_string(register stream * s, byte * ptr, uint len)
{
static const stream_procs p = {
s_std_noavailable, s_string_write_seek, s_std_write_reset,
s_std_null, s_std_null, s_string_write_process
};
s_std_init(s, ptr, len, &p, s_mode_write + s_mode_seek);
s->cbuf_string.data = ptr;
s->cbuf_string.size = len;
}
private int
s_string_write_seek(register stream * s, long pos)
{
if (pos < 0 || pos > s->bsize)
return ERRC;
s->swptr = s->cbuf + pos - 1;
return 0;
}
private int
s_string_read_process(stream_state * st, stream_cursor_read * ignore_pr,
stream_cursor_write * pw, bool last)
{
return EOFC;
}
private int
s_string_write_process(stream_state * st, stream_cursor_read * pr,
stream_cursor_write * ignore_pw, bool last)
{
return (last ? EOFC : ERRC);
}
private int
s_write_position_process(stream_state *, stream_cursor_read *,
stream_cursor_write *, bool);
void
swrite_position_only(stream *s)
{
static byte discard_buf[50];
swrite_string(s, discard_buf, sizeof(discard_buf));
s->procs.process = s_write_position_process;
}
private int
s_write_position_process(stream_state * st, stream_cursor_read * pr,
stream_cursor_write * ignore_pw, bool last)
{
pr->ptr = pr->limit;
return 0;
}
int
s_init_filter(stream *fs, stream_state *fss, byte *buf, uint bsize,
stream *target)
{
const stream_template *template = fss->template;
if (bsize < template->min_in_size)
return ERRC;
s_std_init(fs, buf, bsize, &s_filter_write_procs, s_mode_write);
fs->procs.process = template->process;
fs->state = fss;
if (template->init) {
fs->end_status = (template->init)(fss);
if (fs->end_status < 0)
return fs->end_status;
}
fs->strm = target;
return 0;
}
stream *
s_add_filter(stream **ps, const stream_template *template,
stream_state *ss, gs_memory_t *mem)
{
stream *es;
stream_state *ess;
uint bsize = max(template->min_in_size, 256);
byte *buf;
if (bsize > (*ps)->bsize && template->process != s_NullE_template.process) {
stream_template null_template;
null_template = s_NullE_template;
null_template.min_in_size = bsize;
if (s_add_filter(ps, &null_template, NULL, mem) == 0)
return 0;
}
es = s_alloc(mem, "s_add_filter(stream)");
buf = gs_alloc_bytes(mem, bsize, "s_add_filter(buf)");
if (es == 0 || buf == 0) {
gs_free_object(mem, buf, "s_add_filter(buf)");
gs_free_object(mem, es, "s_add_filter(stream)");
return 0;
}
ess = (ss == 0 ? (stream_state *)es : ss);
ess->template = template;
ess->memory = mem;
es->memory = mem;
if (s_init_filter(es, ess, buf, bsize, *ps) < 0)
return 0;
*ps = es;
return es;
}
int
s_close_filters(stream **ps, stream *target)
{
while (*ps != target) {
stream *s = *ps;
gs_memory_t *mem = s->state->memory;
byte *sbuf = s->cbuf;
stream *next = s->strm;
int status = sclose(s);
stream_state *ss = s->state;
if (status < 0)
return status;
if (mem) {
gs_free_object(mem, sbuf, "s_close_filters(buf)");
gs_free_object(mem, s, "s_close_filters(stream)");
if (ss != (stream_state *)s)
gs_free_object(mem, ss, "s_close_filters(state)");
}
*ps = next;
}
return 0;
}
private int
s_Null_process(stream_state * st, stream_cursor_read * pr,
stream_cursor_write * pw, bool last)
{
return stream_move(pr, pw);
}
const stream_template s_NullE_template = {
&st_stream_state, NULL, s_Null_process, 1, 1
};
const stream_template s_NullD_template = {
&st_stream_state, NULL, s_Null_process, 1, 1
};