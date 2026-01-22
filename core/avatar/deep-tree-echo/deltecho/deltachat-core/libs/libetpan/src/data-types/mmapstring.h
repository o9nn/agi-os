#ifndef __MMAP_STRING_H__
#define __MMAP_STRING_H__
#include <sys/types.h>
#ifndef LIBETPAN_CONFIG_H
#	include <libetpan/libetpan-config.h>
#endif
#ifdef __cplusplus
extern "C" {
#endif
typedef struct _MMAPString MMAPString;
struct _MMAPString
{
char * str;
size_t len;
size_t allocated_len;
int fd;
size_t mmapped_size;
};
LIBETPAN_EXPORT
void mmap_string_set_tmpdir(const char * directory);
LIBETPAN_EXPORT
MMAPString * mmap_string_new (const char * init);
LIBETPAN_EXPORT
MMAPString * mmap_string_new_len (const char * init,
size_t len);
LIBETPAN_EXPORT
MMAPString * mmap_string_sized_new (size_t dfl_size);
LIBETPAN_EXPORT
void mmap_string_free (MMAPString * string);
LIBETPAN_EXPORT
MMAPString * mmap_string_assign (MMAPString * string,
const char * rval);
LIBETPAN_EXPORT
MMAPString * mmap_string_truncate (MMAPString *string,
size_t len);
LIBETPAN_EXPORT
MMAPString * mmap_string_set_size (MMAPString * string,
size_t len);
LIBETPAN_EXPORT
MMAPString * mmap_string_insert_len (MMAPString * string,
size_t pos,
const char * val,
size_t len);
LIBETPAN_EXPORT
MMAPString * mmap_string_append (MMAPString * string,
const char * val);
LIBETPAN_EXPORT
MMAPString * mmap_string_append_len (MMAPString * string,
const char * val,
size_t len);
LIBETPAN_EXPORT
MMAPString * mmap_string_append_c (MMAPString * string,
char c);
LIBETPAN_EXPORT
MMAPString * mmap_string_prepend (MMAPString * string,
const char * val);
LIBETPAN_EXPORT
MMAPString * mmap_string_prepend_c (MMAPString * string,
char c);
LIBETPAN_EXPORT
MMAPString * mmap_string_prepend_len (MMAPString * string,
const char * val,
size_t len);
LIBETPAN_EXPORT
MMAPString * mmap_string_insert (MMAPString * string,
size_t pos,
const char * val);
LIBETPAN_EXPORT
MMAPString * mmap_string_insert_c (MMAPString *string,
size_t pos,
char c);
LIBETPAN_EXPORT
MMAPString * mmap_string_erase(MMAPString * string,
size_t pos,
size_t len);
LIBETPAN_EXPORT
void mmap_string_set_ceil(size_t ceil);
int mmap_string_ref(MMAPString * string);
int mmap_string_unref(char * str);
#ifdef __cplusplus
}
#endif
#endif