#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "mailstream_compress.h"
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#if HAVE_ZLIB
#include <zlib.h>
#endif
#include <assert.h>
#include "mailstream_low.h"
#include "mailstream_cancel.h"
#define CHUNK_SIZE 1024
#ifndef MIN
#define MIN(x, y) ((x) < (y) ? (x) : (y))
#endif
static ssize_t mailstream_low_compress_read(mailstream_low * s, void * buf, size_t count);
static ssize_t mailstream_low_compress_write(mailstream_low * s, const void * buf, size_t count);
static int mailstream_low_compress_close(mailstream_low * s);
static int mailstream_low_compress_get_fd(mailstream_low * s);
static struct mailstream_cancel * mailstream_low_compress_get_cancel(mailstream_low * s);
static void mailstream_low_compress_free(mailstream_low * s);
static void mailstream_low_compress_cancel(mailstream_low * s);
static carray * mailstream_low_compress_get_certificate_chain(mailstream_low * s);
static int mailstream_low_compress_setup_idle(mailstream_low * low);
static int mailstream_low_compress_unsetup_idle(mailstream_low * low);
static int mailstream_low_compress_interrupt_idle(mailstream_low * low);
#if HAVE_ZLIB
typedef struct mailstream_compress_data {
mailstream_low * ms;
z_stream *compress_stream;
z_stream *decompress_stream;
unsigned char input_buf[CHUNK_SIZE];
unsigned char output_buf[CHUNK_SIZE];
} compress_data;
#endif
static mailstream_low_driver local_mailstream_compress_driver = {
mailstream_low_compress_read,
mailstream_low_compress_write,
mailstream_low_compress_close,
mailstream_low_compress_get_fd,
mailstream_low_compress_free,
mailstream_low_compress_cancel,
mailstream_low_compress_get_cancel,
mailstream_low_compress_get_certificate_chain,
mailstream_low_compress_setup_idle,
mailstream_low_compress_unsetup_idle,
mailstream_low_compress_interrupt_idle,
};
mailstream_low_driver * mailstream_compress_driver = &local_mailstream_compress_driver;
mailstream_low * mailstream_low_compress_open(mailstream_low * ms)
{
#if HAVE_ZLIB
mailstream_low * s;
struct mailstream_compress_data * compress_data = calloc(1, sizeof(* compress_data));
if (compress_data == NULL)
goto err;
compress_data->compress_stream = NULL;
compress_data->decompress_stream = NULL;
compress_data->compress_stream = malloc(sizeof(z_stream));
compress_data->compress_stream->zalloc = Z_NULL;
compress_data->compress_stream->zfree = Z_NULL;
compress_data->compress_stream->opaque = Z_NULL;
int ret = deflateInit2(compress_data->compress_stream, Z_BEST_SPEED, Z_DEFLATED, -15, 8, Z_DEFAULT_STRATEGY);
if (ret != Z_OK) {
goto free_compress_data;
}
compress_data->compress_stream->avail_in = 0;
compress_data->compress_stream->avail_out = 0;
compress_data->decompress_stream = malloc(sizeof(z_stream));
compress_data->decompress_stream->zalloc = Z_NULL;
compress_data->decompress_stream->zfree = Z_NULL;
compress_data->decompress_stream->opaque = Z_NULL;
ret = inflateInit2(compress_data->decompress_stream, -15);
if (ret != Z_OK) {
goto free_compress_data;
}
compress_data->decompress_stream->avail_in = 0;
compress_data->decompress_stream->avail_out = 0;
compress_data->ms = ms;
s = mailstream_low_new(compress_data, mailstream_compress_driver);
if (s == NULL)
goto free_compress_data;
return s;
free_compress_data:
if (compress_data->compress_stream) {
deflateEnd(compress_data->compress_stream);
free(compress_data->compress_stream);
}
if (compress_data->decompress_stream) {
inflateEnd(compress_data->decompress_stream);
free(compress_data->decompress_stream);
}
free(compress_data);
err:
return NULL;
#else
return NULL;
#endif
}
static ssize_t mailstream_low_compress_read(mailstream_low * s, void * buf, size_t count)
{
#if HAVE_ZLIB
compress_data * data = s->data;
data->ms->timeout = s->timeout;
z_stream * strm = data->decompress_stream;
int zr;
do {
if (strm->avail_in == 0) {
int read = (int) data->ms->driver->mailstream_read(data->ms, data->input_buf, CHUNK_SIZE);
if (read <= 0) {
return read;
}
strm->avail_in = read;
strm->next_in = data->input_buf;
}
strm->next_out = buf;
strm->avail_out = (int) count;
zr = inflate(strm, Z_NO_FLUSH);
}
while (zr == Z_OK && strm->avail_in == 0 && strm->avail_out == count);
if (zr < 0) {
return -1;
}
return count - strm->avail_out;
#else
return -1;
#endif
}
static ssize_t mailstream_low_compress_write(mailstream_low * s, const void * buf, size_t count) {
#if HAVE_ZLIB
int zr;
compress_data * data = s->data;
data->ms->timeout = s->timeout;
z_stream * strm = data->compress_stream;
strm->next_in = (Bytef *)buf;
int compress_len = MIN((int) count, CHUNK_SIZE);
strm->avail_in = compress_len;
strm->avail_out = CHUNK_SIZE;
strm->next_out = data->output_buf;
zr = deflate(strm, Z_PARTIAL_FLUSH);
if (zr < 0) {
return -1;
}
unsigned char * p = data->output_buf;
size_t remaining = CHUNK_SIZE - strm->avail_out;
while (remaining > 0) {
ssize_t wr = data->ms->driver->mailstream_write(data->ms, p, remaining);
if (wr < 0) {
return -1;
}
p += wr;
remaining -= wr;
}
return compress_len - strm->avail_in;
#else
return -1;
#endif
}
static int mailstream_low_compress_close(mailstream_low * s)
{
#if HAVE_ZLIB
compress_data * data = s->data;
return mailstream_low_close(data->ms);
#else
return 0;
#endif
}
static int mailstream_low_compress_get_fd(mailstream_low * s)
{
#if HAVE_ZLIB
compress_data * data = s->data;
return data->ms->driver->mailstream_get_fd(data->ms);
#else
return -1;
#endif
}
static struct mailstream_cancel * mailstream_low_compress_get_cancel(mailstream_low * s)
{
#if HAVE_ZLIB
compress_data * data = s->data;
return data->ms->driver->mailstream_get_cancel(data->ms);
#else
return NULL;
#endif
}
static void mailstream_low_compress_free(mailstream_low * s)
{
#if HAVE_ZLIB
compress_data * data = s->data;
mailstream_low_free(data->ms);
if (data->compress_stream) {
deflateEnd(data->compress_stream);
free(data->compress_stream);
}
if (data->decompress_stream) {
inflateEnd(data->decompress_stream);
free(data->decompress_stream);
}
free(data);
free(s);
#endif
}
static void mailstream_low_compress_cancel(mailstream_low * s)
{
#if HAVE_ZLIB
compress_data * data = s->data;
data->ms->driver->mailstream_cancel(data->ms);
#endif
}
static carray * mailstream_low_compress_get_certificate_chain(mailstream_low * s)
{
#if HAVE_ZLIB
compress_data * data = s->data;
return data->ms->driver->mailstream_get_certificate_chain(data->ms);
#else
return NULL;
#endif
}
int mailstream_low_compress_wait_idle(mailstream_low * low,
struct mailstream_cancel * idle,
int max_idle_delay)
{
#if HAVE_ZLIB
compress_data * data = low->data;
return mailstream_low_wait_idle(data->ms, idle, max_idle_delay);
#else
return MAILSTREAM_IDLE_ERROR;
#endif
}
static int mailstream_low_compress_setup_idle(mailstream_low * low)
{
#if HAVE_ZLIB
compress_data * data = low->data;
return mailstream_low_setup_idle(data->ms);
#else
return -1;
#endif
}
static int mailstream_low_compress_unsetup_idle(mailstream_low * low)
{
#if HAVE_ZLIB
compress_data * data = low->data;
return mailstream_low_unsetup_idle(data->ms);
#else
return -1;
#endif
}
static int mailstream_low_compress_interrupt_idle(mailstream_low * low)
{
#if HAVE_ZLIB
compress_data * data = low->data;
return mailstream_low_interrupt_idle(data->ms);
#else
return -1;
#endif
}