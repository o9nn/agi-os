#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <errno.h>
#include "ps.h"
#include "common.h"
#define iscntl(ch) ((unsigned)(ch) < 32)
static int
flush (const char **beg, const char *new, FILE *s)
{
const char *b = *beg;
if (new > b)
*beg = new;
if (new - 1 > b)
{
size_t len = new - 1 - b;
int ret = fwrite (b, 1, len, s);
if (ret < 0 || (size_t) ret < len)
return 1;
}
return 0;
}
error_t
noise_write (const char *t, ssize_t max, FILE *s)
{
int ch;
const char *ok = t;
size_t len = 0;
while ((ch = *t++) && (max < 0 || len < (size_t) max))
if (isgraph (ch) || ch == ' ')
len++;
else
{
int is_cntl = iscntl (ch);
if (flush (&ok, t, s))
return errno;
len += (is_cntl ? 2 : 4);
if (max >= 0 && len > (size_t) max)
break;
if (is_cntl)
fprintf (s, "^%c", ch + 'A');
else
fprintf (s, "\\%03o", ch);
}
if (flush (&ok, t, s))
return errno;
return 0;
}
size_t
noise_len (const char *t, ssize_t max)
{
int ch;
size_t len = 0;
while ((ch = *t++) && (max == 0 || len < max))
if (isgraph (ch) || ch == ' ')
len++;
else
{
size_t rep_len = iscntl (ch) ? 2 : 4;
if (max >= 0 && rep_len + len > (size_t) max)
break;
len += rep_len;
}
return len;
}
error_t
ps_stream_write (struct ps_stream *stream, const char *string, ssize_t max_len)
{
size_t len = noise_len (string, max_len);
if (len > 0)
{
error_t err;
ssize_t spaces_needed = stream->spaces;
stream->spaces = 0;
while (spaces_needed > 0)
{
static char spaces[] = "                                ";
#define spaces_len (sizeof(spaces) - 1)
size_t chunk = spaces_needed > spaces_len ? spaces_len : spaces_needed;
error_t err =
ps_stream_write (stream, spaces + spaces_len - chunk, chunk);
if (err)
return err;
spaces_needed -= chunk;
}
stream->spaces = spaces_needed;
err = noise_write (string, len, stream->stream);
if (err)
return err;
stream->pos += len;
}
return 0;
}
error_t
ps_stream_space (struct ps_stream *stream, ssize_t num)
{
stream->spaces += num;
return 0;
}
error_t
ps_stream_pad (struct ps_stream *stream, ssize_t sofar, ssize_t width)
{
return ps_stream_space (stream, ABS (width) - sofar);
}
error_t
ps_stream_newline (struct ps_stream *stream)
{
putc ('\n', stream->stream);
stream->spaces = 0;
stream->pos = 0;
return 0;
}
error_t
_ps_stream_write_field (struct ps_stream *stream,
const char *buf, size_t max_width,
int width)
{
error_t err;
size_t len;
while (isspace (*buf))
buf++;
if (stream->spaces < 0 && max_width >= 0)
max_width += stream->spaces;
len = noise_len (buf, max_width);
if (width > 0)
{
err = ps_stream_write (stream, buf, len);
if (!err)
err = ps_stream_space (stream, width - len);
}
else if (width < 0)
{
err = ps_stream_space (stream, -width - len);
if (!err)
err = ps_stream_write (stream, buf, len);
}
else
err = ps_stream_write (stream, buf, len);
return err;
}
error_t
ps_stream_write_field (struct ps_stream *stream, const char *buf, int width)
{
return _ps_stream_write_field (stream, buf, -1, width);
}
error_t
ps_stream_write_trunc_field (struct ps_stream *stream,
const char *buf, int width)
{
return _ps_stream_write_field (stream, buf, width ? ABS (width) : -1, width);
}
error_t
ps_stream_write_int_field (struct ps_stream *stream, int value, int width)
{
char buf[20];
sprintf (buf, "%d", value);
return ps_stream_write_field (stream, buf, width);
}
error_t
ps_stream_create (FILE *dest, struct ps_stream **stream)
{
*stream = malloc (sizeof (struct ps_stream));
if (! *stream)
return ENOMEM;
(*stream)->stream = dest;
(*stream)->spaces = 0;
(*stream)->pos = 0;
return 0;
}
void
ps_stream_free (struct ps_stream *stream)
{
free (stream);
}