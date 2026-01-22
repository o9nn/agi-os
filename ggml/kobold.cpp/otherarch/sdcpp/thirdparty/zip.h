#pragma once
#ifndef ZIP_H
#define ZIP_H
#include <stdint.h>
#include <string.h>
#include <sys/types.h>
#ifndef ZIP_SHARED
#define ZIP_EXPORT
#else
#ifdef _WIN32
#ifdef ZIP_BUILD_SHARED
#define ZIP_EXPORT __declspec(dllexport)
#else
#define ZIP_EXPORT __declspec(dllimport)
#endif
#else
#define ZIP_EXPORT __attribute__((visibility("default")))
#endif
#endif
#ifdef __cplusplus
extern "C" {
#endif
#if !defined(_POSIX_C_SOURCE) && defined(_MSC_VER)
#ifdef _WIN64
typedef long long ssize_t;
#else
typedef long ssize_t;
#endif
#endif
#define ZIP_DEFAULT_COMPRESSION_LEVEL 6
#define ZIP_ENOINIT -1
#define ZIP_EINVENTNAME -2
#define ZIP_ENOENT -3
#define ZIP_EINVMODE -4
#define ZIP_EINVLVL -5
#define ZIP_ENOSUP64 -6
#define ZIP_EMEMSET -7
#define ZIP_EWRTENT -8
#define ZIP_ETDEFLINIT -9
#define ZIP_EINVIDX -10
#define ZIP_ENOHDR -11
#define ZIP_ETDEFLBUF -12
#define ZIP_ECRTHDR -13
#define ZIP_EWRTHDR -14
#define ZIP_EWRTDIR -15
#define ZIP_EOPNFILE -16
#define ZIP_EINVENTTYPE -17
#define ZIP_EMEMNOALLOC -18
#define ZIP_ENOFILE -19
#define ZIP_ENOPERM -20
#define ZIP_EOOMEM -21
#define ZIP_EINVZIPNAME -22
#define ZIP_EMKDIR -23
#define ZIP_ESYMLINK -24
#define ZIP_ECLSZIP -25
#define ZIP_ECAPSIZE -26
#define ZIP_EFSEEK -27
#define ZIP_EFREAD -28
#define ZIP_EFWRITE -29
#define ZIP_ERINIT -30
#define ZIP_EWINIT -31
#define ZIP_EWRINIT -32
extern ZIP_EXPORT const char *zip_strerror(int errnum);
struct zip_t;
extern ZIP_EXPORT struct zip_t *zip_open(const char *zipname, int level,
char mode);
extern ZIP_EXPORT struct zip_t *
zip_openwitherror(const char *zipname, int level, char mode, int *errnum);
extern ZIP_EXPORT void zip_close(struct zip_t *zip);
extern ZIP_EXPORT int zip_is64(struct zip_t *zip);
extern ZIP_EXPORT int zip_entry_open(struct zip_t *zip, const char *entryname);
extern ZIP_EXPORT int zip_entry_opencasesensitive(struct zip_t *zip,
const char *entryname);
extern ZIP_EXPORT int zip_entry_openbyindex(struct zip_t *zip, size_t index);
extern ZIP_EXPORT int zip_entry_close(struct zip_t *zip);
extern ZIP_EXPORT const char *zip_entry_name(struct zip_t *zip);
extern ZIP_EXPORT ssize_t zip_entry_index(struct zip_t *zip);
extern ZIP_EXPORT int zip_entry_isdir(struct zip_t *zip);
extern ZIP_EXPORT unsigned long long zip_entry_size(struct zip_t *zip);
extern ZIP_EXPORT unsigned long long zip_entry_uncomp_size(struct zip_t *zip);
extern ZIP_EXPORT unsigned long long zip_entry_comp_size(struct zip_t *zip);
extern ZIP_EXPORT unsigned int zip_entry_crc32(struct zip_t *zip);
extern ZIP_EXPORT int zip_entry_write(struct zip_t *zip, const void *buf,
size_t bufsize);
extern ZIP_EXPORT int zip_entry_fwrite(struct zip_t *zip, const char *filename);
extern ZIP_EXPORT ssize_t zip_entry_read(struct zip_t *zip, void **buf,
size_t *bufsize);
extern ZIP_EXPORT ssize_t zip_entry_noallocread(struct zip_t *zip, void *buf,
size_t bufsize);
extern ZIP_EXPORT int zip_entry_fread(struct zip_t *zip, const char *filename);
extern ZIP_EXPORT int
zip_entry_extract(struct zip_t *zip,
size_t (*on_extract)(void *arg, uint64_t offset,
const void *data, size_t size),
void *arg);
extern ZIP_EXPORT ssize_t zip_entries_total(struct zip_t *zip);
extern ZIP_EXPORT ssize_t zip_entries_delete(struct zip_t *zip,
char *const entries[], size_t len);
extern ZIP_EXPORT int
zip_stream_extract(const char *stream, size_t size, const char *dir,
int (*on_extract)(const char *filename, void *arg),
void *arg);
extern ZIP_EXPORT struct zip_t *zip_stream_open(const char *stream, size_t size,
int level, char mode);
extern ZIP_EXPORT struct zip_t *zip_stream_openwitherror(const char *stream,
size_t size, int level,
char mode,
int *errnum);
extern ZIP_EXPORT ssize_t zip_stream_copy(struct zip_t *zip, void **buf,
size_t *bufsize);
extern ZIP_EXPORT void zip_stream_close(struct zip_t *zip);
extern ZIP_EXPORT int zip_create(const char *zipname, const char *filenames[],
size_t len);
extern ZIP_EXPORT int zip_extract(const char *zipname, const char *dir,
int (*on_extract_entry)(const char *filename,
void *arg),
void *arg);
#ifdef __cplusplus
}
#endif
#endif