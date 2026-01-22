#include "stdio_.h"
#include "memory_.h"
#include "gdebug.h"
#include "gpcheck.h"
#include "stream.h"
#include "strimpl.h"
private int
s_file_available(stream *, long *),
s_file_read_seek(stream *, long),
s_file_read_close(stream *),
s_file_read_process(stream_state *, stream_cursor_read *,
stream_cursor_write *, bool);
private int
s_file_write_seek(stream *, long),
s_file_write_flush(stream *),
s_file_write_close(stream *),
s_file_write_process(stream_state *, stream_cursor_read *,
stream_cursor_write *, bool);
private int
s_file_switch(stream *, bool);
void
sread_file(register stream * s, FILE * file, byte * buf, uint len)
{
static const stream_procs p = {
s_file_available, s_file_read_seek, s_std_read_reset,
s_std_read_flush, s_file_read_close, s_file_read_process,
s_file_switch
};
int had_error = ferror(file);
long curpos = ftell(file);
bool seekable = (curpos != -1L && fseek(file, curpos, SEEK_SET) == 0);
if (!had_error)
clearerr(file);
s_std_init(s, buf, len, &p,
(seekable ? s_mode_read + s_mode_seek : s_mode_read));
if_debug1('s', "[s]read file=0x%lx\n", (ulong) file);
s->file = file;
s->file_modes = s->modes;
s->file_offset = 0;
s->file_limit = max_long;
}
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
private int
s_file_available(register stream * s, long *pl)
{
long max_avail = s->file_limit - stell(s);
long buf_avail = sbufavailable(s);
*pl = min(max_avail, buf_avail);
if (sseekable(s)) {
long pos, end;
pos = ftell(s->file);
if (fseek(s->file, 0L, SEEK_END))
return ERRC;
end = ftell(s->file);
if (fseek(s->file, pos, SEEK_SET))
return ERRC;
buf_avail += end - pos;
*pl = min(max_avail, buf_avail);
if (*pl == 0)
*pl = -1;
} else {
if (*pl == 0 && feof(s->file))
*pl = -1;
}
return 0;
}
private int
s_file_read_seek(register stream * s, long pos)
{
uint end = s->srlimit - s->cbuf + 1;
long offset = pos - s->position;
if (offset >= 0 && offset <= end) {
s->srptr = s->cbuf + offset - 1;
return 0;
}
if (pos < 0 || pos > s->file_limit ||
fseek(s->file, s->file_offset + pos, SEEK_SET) != 0
)
return ERRC;
s->srptr = s->srlimit = s->cbuf - 1;
s->end_status = 0;
s->position = pos;
return 0;
}
private int
s_file_read_close(stream * s)
{
FILE *file = s->file;
if (file != 0) {
s->file = 0;
return (fclose(file) ? ERRC : 0);
}
return 0;
}
private int
s_file_read_process(stream_state * st, stream_cursor_read * ignore_pr,
stream_cursor_write * pw, bool last)
{
stream *s = (stream *)st;
FILE *file = s->file;
uint max_count = pw->limit - pw->ptr;
int status = 1;
int count;
if (s->file_limit < max_long) {
long limit_count = s->file_offset + s->file_limit - ftell(file);
if (max_count > limit_count)
max_count = limit_count, status = EOFC;
}
count = fread(pw->ptr + 1, 1, max_count, file);
if (count < 0)
count = 0;
pw->ptr += count;
process_interrupts(s->memory);
return (ferror(file) ? ERRC : feof(file) ? EOFC : status);
}
void
swrite_file(register stream * s, FILE * file, byte * buf, uint len)
{
static const stream_procs p = {
s_std_noavailable, s_file_write_seek, s_std_write_reset,
s_file_write_flush, s_file_write_close, s_file_write_process,
s_file_switch
};
s_std_init(s, buf, len, &p,
(file == stdout ? s_mode_write : s_mode_write + s_mode_seek));
if_debug1('s', "[s]write file=0x%lx\n", (ulong) file);
s->file = file;
s->file_modes = s->modes;
s->file_offset = 0;
s->file_limit = max_long;
}
void
sappend_file(register stream * s, FILE * file, byte * buf, uint len)
{
swrite_file(s, file, buf, len);
s->modes = s_mode_write + s_mode_append;
s->file_modes = s->modes;
fseek(file, 0L, SEEK_END);
s->position = ftell(file);
}
private int
s_file_write_seek(stream * s, long pos)
{
int code = sflush(s);
if (code < 0)
return code;
if (fseek(s->file, pos, SEEK_SET) != 0)
return ERRC;
s->position = pos;
return 0;
}
private int
s_file_write_flush(register stream * s)
{
int result = s_process_write_buf(s, false);
fflush(s->file);
return result;
}
private int
s_file_write_close(register stream * s)
{
s_process_write_buf(s, true);
return s_file_read_close(s);
}
private int
s_file_write_process(stream_state * st, stream_cursor_read * pr,
stream_cursor_write * ignore_pw, bool last)
{
uint count = pr->limit - pr->ptr;
if (count != 0) {
FILE *file = ((stream *) st)->file;
int written = fwrite(pr->ptr + 1, 1, count, file);
if (written < 0)
written = 0;
pr->ptr += written;
process_interrupts(NULL);
return (ferror(file) ? ERRC : 0);
} else {
process_interrupts(NULL);
return 0;
}
}
private int
s_file_switch(stream * s, bool writing)
{
uint modes = s->file_modes;
FILE *file = s->file;
long pos;
if (writing) {
if (!(s->file_modes & s_mode_write))
return ERRC;
pos = stell(s);
if_debug2('s', "[s]switch 0x%lx to write at %ld\n",
(ulong) s, pos);
fseek(file, pos, SEEK_SET);
if (modes & s_mode_append) {
sappend_file(s, file, s->cbuf, s->cbsize);
} else {
swrite_file(s, file, s->cbuf, s->cbsize);
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
fseek(file, 0L, SEEK_CUR);
sread_file(s, file, s->cbuf, s->cbsize);
s->modes |= modes & s_mode_append;
s->position = pos;
}
s->file_modes = modes;
return 0;
}