#include "stdio_.h"
#include "errno_.h"
#include "memory_.h"
#include "unistd_.h"
#include "gdebug.h"
#include "gpcheck.h"
#include "stream.h"
#include "strimpl.h"
#ifdef KEEP_FILENO_API
void
sread_fileno(stream *, FILE *, byte *, uint),
swrite_fileno(stream *, FILE *, byte *, uint),
sappend_fileno(stream *, FILE *, byte *, uint);
#else
#  define sread_fileno sread_file
#  define swrite_fileno swrite_file
#  define sappend_fileno sappend_file
#endif
private int
s_fileno_available(stream *, long *),
s_fileno_read_seek(stream *, long),
s_fileno_read_close(stream *),
s_fileno_read_process(stream_state *, stream_cursor_read *,
stream_cursor_write *, bool);
private int
s_fileno_write_seek(stream *, long),
s_fileno_write_flush(stream *),
s_fileno_write_close(stream *),
s_fileno_write_process(stream_state *, stream_cursor_read *,
stream_cursor_write *, bool);
private int
s_fileno_switch(stream *, bool);
inline private int
sfileno(const stream *s)
{
return fileno(s->file);
}
inline private long
ltell(int fd)
{
return lseek(fd, 0L, SEEK_CUR);
}
private bool
errno_is_retry(int errn)
{
switch (errn) {
#ifdef EINTR
case EINTR: return true;
#endif
#if defined(EAGAIN) && (!defined(EINTR) || EAGAIN != EINTR)
case EAGAIN: return true;
#endif
#if defined(EWOULDBLOCK) && (!defined(EINTR) || EWOULDBLOCK != EINTR) && (!defined(EAGAIN) || EWOULDBLOCK != EAGAIN)
case EWOULDBLOCK: return true;
#endif
default: return false;
}
}
void
sread_fileno(register stream * s, FILE * file, byte * buf, uint len)
{
static const stream_procs p = {
s_fileno_available, s_fileno_read_seek, s_std_read_reset,
s_std_read_flush, s_fileno_read_close, s_fileno_read_process,
s_fileno_switch
};
int fd = fileno(file);
long curpos = ltell(fd);
bool seekable = (curpos != -1L && lseek(fd, curpos, SEEK_SET) != -1L);
s_std_init(s, buf, len, &p,
(seekable ? s_mode_read + s_mode_seek : s_mode_read));
if_debug2('s', "[s]read file=0x%lx, fd=%d\n", (ulong) file,
fileno(file));
s->file = file;
s->file_modes = s->modes;
s->file_offset = 0;
s->file_limit = max_long;
}
#ifndef KEEP_FILENO_API
int
sread_subfile(stream *s, long start, long length)
{
if (s->file == 0 || s->modes != s_mode_read + s_mode_seek ||
s->file_offset != 0 || s->file_limit != max_long ||
((s->position < start || s->position > start + length) &&
sseek(s, start) < 0)
)
return ERRC;
s->position -= start;
s->file_offset = start;
s->file_limit = length;
return 0;
}
#endif
private int
s_fileno_available(register stream * s, long *pl)
{
long max_avail = s->file_limit - stell(s);
long buf_avail = sbufavailable(s);
int fd = sfileno(s);
*pl = min(max_avail, buf_avail);
if (sseekable(s)) {
long pos, end;
pos = ltell(fd);
if (pos < 0)
return ERRC;
end = lseek(fd, 0L, SEEK_END);
if (lseek(fd, pos, SEEK_SET) < 0 || end < 0)
return ERRC;
buf_avail += end - pos;
*pl = min(max_avail, buf_avail);
if (*pl == 0)
*pl = -1;
} else {
if (*pl == 0)
*pl = -1;
}
return 0;
}
private int
s_fileno_read_seek(register stream * s, long pos)
{
uint end = s->srlimit - s->cbuf + 1;
long offset = pos - s->position;
if (offset >= 0 && offset <= end) {
s->srptr = s->cbuf + offset - 1;
return 0;
}
if (pos < 0 || pos > s->file_limit ||
lseek(sfileno(s), s->file_offset + pos, SEEK_SET) < 0
)
return ERRC;
s->srptr = s->srlimit = s->cbuf - 1;
s->end_status = 0;
s->position = pos;
return 0;
}
private int
s_fileno_read_close(stream * s)
{
FILE *file = s->file;
if (file != 0) {
s->file = 0;
return (fclose(file) ? ERRC : 0);
}
return 0;
}
private int
s_fileno_read_process(stream_state * st, stream_cursor_read * ignore_pr,
stream_cursor_write * pw, bool last)
{
stream *s = (stream *)st;
int fd = sfileno(s);
uint max_count;
int nread, status;
again:
max_count = pw->limit - pw->ptr;
status = 1;
if (s->file_limit < max_long) {
long limit_count = s->file_offset + s->file_limit - ltell(fd);
if (max_count > limit_count)
max_count = limit_count, status = EOFC;
}
nread = read(fd, (void *)(pw->ptr + 1), max_count);
if (nread > 0)
pw->ptr += nread;
else if (nread == 0)
status = EOFC;
else if (errno_is_retry(errno))
goto again;
else
status = ERRC;
process_interrupts(s->memory);
return status;
}
void
swrite_fileno(register stream * s, FILE * file, byte * buf, uint len)
{
static const stream_procs p = {
s_std_noavailable, s_fileno_write_seek, s_std_write_reset,
s_fileno_write_flush, s_fileno_write_close, s_fileno_write_process,
s_fileno_switch
};
s_std_init(s, buf, len, &p,
(file == stdout ? s_mode_write : s_mode_write + s_mode_seek));
if_debug2('s', "[s]write file=0x%lx, fd=%d\n", (ulong) file,
fileno(file));
s->file = file;
s->file_modes = s->modes;
s->file_offset = 0;
s->file_limit = max_long;
}
void
sappend_fileno(register stream * s, FILE * file, byte * buf, uint len)
{
swrite_fileno(s, file, buf, len);
s->modes = s_mode_write + s_mode_append;
s->file_modes = s->modes;
s->position = lseek(fileno(file), 0L, SEEK_END);
}
private int
s_fileno_write_seek(stream * s, long pos)
{
int code = sflush(s);
if (code < 0)
return code;
if (lseek(sfileno(s), pos, SEEK_SET) < 0)
return ERRC;
s->position = pos;
return 0;
}
private int
s_fileno_write_flush(register stream * s)
{
int result = s_process_write_buf(s, false);
discard(fsync(sfileno(s)));
return result;
}
private int
s_fileno_write_close(register stream * s)
{
s_process_write_buf(s, true);
return s_fileno_read_close(s);
}
private int
s_fileno_write_process(stream_state * st, stream_cursor_read * pr,
stream_cursor_write * ignore_pw, bool last)
{
int nwrite, status;
uint count;
again:
count = pr->limit - pr->ptr;
if (count == 0) {
process_interrupts((stream*)st->memory);
return 0;
}
nwrite = write(sfileno((stream *)st), (const void *)(pr->ptr + 1), count);
if (nwrite >= 0) {
pr->ptr += nwrite;
status = 0;
} else if (errno_is_retry(errno))
goto again;
else
status = ERRC;
process_interrupts((stream *)st->memory);
return status;
}
private int
s_fileno_switch(stream * s, bool writing)
{
uint modes = s->file_modes;
int fd = sfileno(s);
long pos;
if (writing) {
if (!(s->file_modes & s_mode_write))
return ERRC;
pos = stell(s);
if_debug2('s', "[s]switch 0x%lx to write at %ld\n",
(ulong) s, pos);
lseek(fd, pos, SEEK_SET);
if (modes & s_mode_append) {
sappend_file(s, s->file, s->cbuf, s->cbsize);
} else {
swrite_file(s, s->file, s->cbuf, s->cbsize);
s->position = pos;
}
s->modes = modes;
} else {
if (!(s->file_modes & s_mode_read))
return ERRC;
pos = stell(s);
if_debug2('s', "[s]switch 0x%lx to read at %ld\n",
(ulong) s, pos);
if (sflush(s) < 0)
return ERRC;
lseek(fd, 0L, SEEK_CUR);
sread_file(s, s->file, s->cbuf, s->cbsize);
s->modes |= modes & s_mode_append;
s->position = pos;
}
s->file_modes = modes;
return 0;
}