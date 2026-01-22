#ifndef	_MACH_DEBUG_HASH_INFO_H_
#define _MACH_DEBUG_HASH_INFO_H_
typedef struct hash_info_bucket {
unsigned int hib_count;
} hash_info_bucket_t;
typedef hash_info_bucket_t *hash_info_bucket_array_t;
#endif