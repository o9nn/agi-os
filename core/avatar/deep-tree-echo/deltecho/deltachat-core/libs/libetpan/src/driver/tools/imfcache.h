#ifndef IMFCACHE_H
#define IMFCACHE_H
#include <stdio.h>
#include "mailimf.h"
#include "maildriver_types.h"
#include "mmapstring.h"
#ifdef __cplusplus
extern "C" {
#endif
int mail_serialize_clear(MMAPString * mmapstr, size_t * indx);
int mail_serialize_write(MMAPString * mmapstr, size_t * indx,
char * buf, size_t size);
int mail_serialize_read(MMAPString * mmapstr, size_t * indx,
char * buf, size_t size);
int mailimf_cache_int_write(MMAPString * mmapstr, size_t * indx,
uint32_t value);
int mailimf_cache_string_write(MMAPString * mmapstr, size_t * indx,
char * str, size_t length);
int mailimf_cache_int_read(MMAPString * mmapstr, size_t * indx,
uint32_t * result);
int mailimf_cache_string_read(MMAPString * mmapstr, size_t * indx,
char ** result);
int mailimf_cache_fields_write(MMAPString * mmapstr, size_t * indx,
struct mailimf_fields * fields);
int mailimf_cache_fields_read(MMAPString * mmapstr, size_t * indx,
struct mailimf_fields ** result);
#ifdef __cplusplus
}
#endif
#endif