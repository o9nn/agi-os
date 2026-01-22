#include "netpgp/config-netpgp.h"
#ifdef HAVE_SYS_CDEFS_H
#include <sys/cdefs.h>
#endif
#if defined(__NetBSD__)
__COPYRIGHT("@(#) Copyright (c) 2009 The NetBSD Foundation, Inc. All rights reserved.");
__RCSID("$NetBSD$");
#endif
#ifdef HAVE_ZLIB_H
#include <zlib.h>
#endif
#ifdef HAVE_BZLIB_H
#include <bzlib.h>
#endif
#include <string.h>
#include "netpgp/packet-parse.h"
#include "netpgp/errors.h"
#include "netpgp/netpgpdefs.h"
#include "netpgp/crypto.h"
#include "netpgp/memory.h"
#include "netpgp/writer.h"
#define DECOMPRESS_BUFFER 1024
typedef struct {
pgp_compression_type_t type;
pgp_region_t *region;
uint8_t in[DECOMPRESS_BUFFER];
uint8_t out[DECOMPRESS_BUFFER];
z_stream zstream;
size_t offset;
int inflate_ret;
} z_decompress_t;
#ifdef HAVE_BZLIB_H
typedef struct {
pgp_compression_type_t type;
pgp_region_t *region;
char in[DECOMPRESS_BUFFER];
char out[DECOMPRESS_BUFFER];
bz_stream bzstream;
size_t offset;
int inflate_ret;
} bz_decompress_t;
#endif
typedef struct {
z_stream stream;
uint8_t *src;
uint8_t *dst;
} compress_t;
static int
zlib_compressed_data_reader(pgp_stream_t *stream, void *dest, size_t length,
pgp_error_t **errors,
pgp_reader_t *readinfo,
pgp_cbdata_t *cbinfo)
{
z_decompress_t *z = pgp_reader_get_arg(readinfo);
size_t len;
size_t cc;
char *cdest = dest;
if (z->type != PGP_C_ZIP && z->type != PGP_C_ZLIB) {
(void) fprintf(stderr,
"zlib_compressed_data_reader: weird type %d\n",
z->type);
return 0;
}
if (z->inflate_ret == Z_STREAM_END &&
z->zstream.next_out == &z->out[z->offset]) {
return 0;
}
if (pgp_get_debug_level(__FILE__)) {
(void) fprintf(stderr,
"zlib_compressed_data_reader: length %" PRIsize "d\n",
length);
}
for (cc = 0 ; cc < length ; cc += len) {
if (&z->out[z->offset] == z->zstream.next_out) {
int ret;
z->zstream.next_out = z->out;
z->zstream.avail_out = sizeof(z->out);
z->offset = 0;
if (z->zstream.avail_in == 0) {
unsigned n = z->region->length;
if (!z->region->indeterminate) {
n -= z->region->readc;
if (n > sizeof(z->in)) {
n = sizeof(z->in);
}
} else {
n = sizeof(z->in);
}
if (!pgp_stacked_limited_read(stream, z->in, n,
z->region,
errors, readinfo, cbinfo)) {
return -1;
}
z->zstream.next_in = z->in;
z->zstream.avail_in = (z->region->indeterminate) ?
z->region->last_read : n;
}
ret = inflate(&z->zstream, Z_SYNC_FLUSH);
if (ret == Z_STREAM_END) {
if (!z->region->indeterminate &&
z->region->readc != z->region->length) {
PGP_ERROR_1(cbinfo->errors,
PGP_E_P_DECOMPRESSION_ERROR,
"%s",
"Compressed stream ended before packet end.");
}
} else if (ret != Z_OK) {
(void) fprintf(stderr, "ret=%d\n", ret);
PGP_ERROR_1(cbinfo->errors,
PGP_E_P_DECOMPRESSION_ERROR, "%s",
z->zstream.msg);
}
z->inflate_ret = ret;
}
if (z->zstream.next_out <= &z->out[z->offset]) {
(void) fprintf(stderr, "Out of memory in buffer\n");
return 0;
}
len = (size_t)(z->zstream.next_out - &z->out[z->offset]);
size_t left_in_cdest = length - cc;
if (len > left_in_cdest) {
len = left_in_cdest;
}
(void) memcpy(&cdest[cc], &z->out[z->offset], len);
z->offset += len;
}
return (int)length;
}
#ifdef HAVE_BZLIB_H
static int
bzip2_compressed_data_reader(pgp_stream_t *stream, void *dest, size_t length,
pgp_error_t **errors,
pgp_reader_t *readinfo,
pgp_cbdata_t *cbinfo)
{
bz_decompress_t *bz = pgp_reader_get_arg(readinfo);
size_t len;
size_t cc;
char *cdest = dest;
if (bz->type != PGP_C_BZIP2) {
(void) fprintf(stderr, "Weird type %d\n", bz->type);
return 0;
}
if (bz->inflate_ret == BZ_STREAM_END &&
bz->bzstream.next_out == &bz->out[bz->offset]) {
return 0;
}
for (cc = 0 ; cc < length ; cc += len) {
if (&bz->out[bz->offset] == bz->bzstream.next_out) {
int ret;
bz->bzstream.next_out = (char *) bz->out;
bz->bzstream.avail_out = sizeof(bz->out);
bz->offset = 0;
if (bz->bzstream.avail_in == 0) {
unsigned n = bz->region->length;
if (!bz->region->indeterminate) {
n -= bz->region->readc;
if (n > sizeof(bz->in))
n = sizeof(bz->in);
} else
n = sizeof(bz->in);
if (!pgp_stacked_limited_read(stream,
(uint8_t *) bz->in,
n, bz->region,
errors, readinfo, cbinfo))
return -1;
bz->bzstream.next_in = bz->in;
bz->bzstream.avail_in =
(bz->region->indeterminate) ?
bz->region->last_read : n;
}
ret = BZ2_bzDecompress(&bz->bzstream);
if (ret == BZ_STREAM_END) {
if (!bz->region->indeterminate &&
bz->region->readc != bz->region->length)
PGP_ERROR_1(cbinfo->errors,
PGP_E_P_DECOMPRESSION_ERROR,
"%s",
"Compressed stream ended before packet end.");
} else if (ret != BZ_OK) {
PGP_ERROR_1(cbinfo->errors,
PGP_E_P_DECOMPRESSION_ERROR,
"Invalid return %d from BZ2_bzDecompress", ret);
}
bz->inflate_ret = ret;
}
if (bz->bzstream.next_out <= &bz->out[bz->offset]) {
(void) fprintf(stderr, "Out of bz memroy\n");
return 0;
}
len = (size_t)(bz->bzstream.next_out - &bz->out[bz->offset]);
if (len > length) {
len = length;
}
(void) memcpy(&cdest[cc], &bz->out[bz->offset], len);
bz->offset += len;
}
return (int)length;
}
#endif
int
pgp_decompress(pgp_region_t *region, pgp_stream_t *stream,
pgp_compression_type_t type)
{
z_decompress_t z;
#ifdef HAVE_BZLIB_H
bz_decompress_t bz;
#endif
const int printerrors = 1;
int ret;
switch (type) {
case PGP_C_ZIP:
case PGP_C_ZLIB:
(void) memset(&z, 0x0, sizeof(z));
z.region = region;
z.offset = 0;
z.type = type;
z.zstream.next_in = Z_NULL;
z.zstream.avail_in = 0;
z.zstream.next_out = z.out;
z.zstream.zalloc = Z_NULL;
z.zstream.zfree = Z_NULL;
z.zstream.opaque = Z_NULL;
break;
#ifdef HAVE_BZLIB_H
case PGP_C_BZIP2:
(void) memset(&bz, 0x0, sizeof(bz));
bz.region = region;
bz.offset = 0;
bz.type = type;
bz.bzstream.next_in = NULL;
bz.bzstream.avail_in = 0;
bz.bzstream.next_out = bz.out;
bz.bzstream.bzalloc = NULL;
bz.bzstream.bzfree = NULL;
bz.bzstream.opaque = NULL;
#endif
break;
default:
PGP_ERROR_1(&stream->errors,
PGP_E_ALG_UNSUPPORTED_COMPRESS_ALG,
"Compression algorithm %d is not yet supported", type);
return 0;
}
switch (type) {
case PGP_C_ZIP:
ret = (int)inflateInit2(&z.zstream, -15);
break;
case PGP_C_ZLIB:
ret = (int)inflateInit(&z.zstream);
break;
#ifdef HAVE_BZLIB_H
case PGP_C_BZIP2:
ret = BZ2_bzDecompressInit(&bz.bzstream, 1, 0);
break;
#endif
default:
return 0;
}
switch (type) {
case PGP_C_ZIP:
case PGP_C_ZLIB:
if (ret != Z_OK) {
PGP_ERROR_1(&stream->errors,
PGP_E_P_DECOMPRESSION_ERROR,
"Cannot initialise ZIP or ZLIB stream for decompression: error=%d", ret);
return 0;
}
pgp_reader_push(stream, zlib_compressed_data_reader,
NULL, &z);
break;
#ifdef HAVE_BZLIB_H
case PGP_C_BZIP2:
if (ret != BZ_OK) {
PGP_ERROR_1(&stream->errors,
PGP_E_P_DECOMPRESSION_ERROR,
"Cannot initialise BZIP2 stream for decompression: error=%d", ret);
return 0;
}
pgp_reader_push(stream, bzip2_compressed_data_reader,
NULL, &bz);
break;
#endif
default:
return 0;
}
ret = pgp_parse(stream, !printerrors);
pgp_reader_pop(stream);
switch (type) {
case PGP_C_ZIP:
case PGP_C_ZLIB:
inflateEnd(&z.zstream);
break;
#ifdef HAVE_BZLIB_H
case PGP_C_BZIP2:
BZ2_bzDecompressEnd(&bz.bzstream);
break;
#endif
default:
return 0;
}
return ret;
}
unsigned
pgp_writez(pgp_output_t *out, const uint8_t *data, const unsigned len)
{
compress_t *zip;
size_t sz_in;
size_t sz_out;
int ret;
int r = 0;
const int level = Z_DEFAULT_COMPRESSION;
if ((zip = calloc(1, sizeof(*zip))) == NULL) {
(void) fprintf(stderr, "pgp_writez: bad alloc\n");
return 0;
}
zip->stream.zalloc = Z_NULL;
zip->stream.zfree = Z_NULL;
zip->stream.opaque = NULL;
if ((int)deflateInit(&zip->stream, level) != Z_OK) {
(void) fprintf(stderr, "pgp_writez: can't initialise\n");
return 0;
}
if (zip->src != NULL || zip->dst != NULL) {
(void) fprintf(stderr, "pgp_writez: non-null streams\n");
return 0;
}
sz_in = len * sizeof(uint8_t);
sz_out = ((101 * sz_in) / 100) + 12;
if ((zip->src = calloc(1, sz_in)) == NULL) {
free(zip);
(void) fprintf(stderr, "pgp_writez: bad alloc2\n");
return 0;
}
if ((zip->dst = calloc(1, sz_out)) == NULL) {
free(zip->src);
free(zip);
(void) fprintf(stderr, "pgp_writez: bad alloc3\n");
return 0;
}
(void) memcpy(zip->src, data, len);
zip->stream.next_in = zip->src;
zip->stream.avail_in = (unsigned)sz_in;
zip->stream.total_in = 0;
zip->stream.next_out = zip->dst;
zip->stream.avail_out = (unsigned)sz_out;
zip->stream.total_out = 0;
do {
r = deflate(&zip->stream, Z_FINISH);
} while (r != Z_STREAM_END);
ret = pgp_write_ptag(out, PGP_PTAG_CT_COMPRESSED) &&
pgp_write_length(out, (unsigned)(zip->stream.total_out + 1))&&
pgp_write_scalar(out, PGP_C_ZLIB, 1) &&
pgp_write(out, zip->dst, (unsigned)zip->stream.total_out);
deflateEnd(&zip->stream);
free(zip->src);
free(zip->dst);
free(zip);
return ret;
}