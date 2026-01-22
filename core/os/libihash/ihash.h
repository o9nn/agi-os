#ifndef _HURD_IHASH_H
#define _HURD_IHASH_H	1
#include <errno.h>
#include <sys/types.h>
#include <limits.h>
#include <stdint.h>
#include <stddef.h>
typedef void *hurd_ihash_value_t;
#define _HURD_IHASH_EMPTY	((hurd_ihash_value_t) 0)
#define _HURD_IHASH_DELETED	((hurd_ihash_value_t) -1)
static inline int
hurd_ihash_value_valid (hurd_ihash_value_t value)
{
return value != _HURD_IHASH_EMPTY && value != _HURD_IHASH_DELETED;
}
typedef uintptr_t hurd_ihash_key_t;
typedef hurd_ihash_value_t *hurd_ihash_locp_t;
typedef hurd_ihash_key_t (*hurd_ihash_fct_hash_t) (const void *);
typedef int (*hurd_ihash_fct_cmp_t) (const void *, const void *);
typedef void (*hurd_ihash_cleanup_t) (hurd_ihash_value_t value, void *arg);
struct _hurd_ihash_item
{
hurd_ihash_value_t value;
hurd_ihash_key_t key;
};
typedef struct _hurd_ihash_item *_hurd_ihash_item_t;
struct hurd_ihash
{
size_t nr_items;
_hurd_ihash_item_t items;
size_t size;
intptr_t locp_offset;
unsigned int max_load;
hurd_ihash_cleanup_t cleanup;
void *cleanup_data;
hurd_ihash_fct_hash_t fct_hash;
hurd_ihash_fct_cmp_t fct_cmp;
size_t nr_free;
};
typedef struct hurd_ihash *hurd_ihash_t;
#define HURD_IHASH_MIN_SIZE	32
#define HURD_IHASH_MAX_LOAD_DEFAULT 96
#define HURD_IHASH_NO_LOCP	INTPTR_MIN
#define HURD_IHASH_INITIALIZER(locp_offs)				\
{ .nr_items = 0, .size = 0, .cleanup = (hurd_ihash_cleanup_t) 0,	\
.max_load = HURD_IHASH_MAX_LOAD_DEFAULT,				\
.locp_offset = (locp_offs)}
#define HURD_IHASH_INITIALIZER_GKI(locp_offs, f_clean, f_clean_data,	\
f_hash, f_compare)			\
{ .nr_items = 0, .size = 0,						\
.cleanup = (f_clean),						\
.cleanup_data = (f_clean_data),					\
.max_load = HURD_IHASH_MAX_LOAD_DEFAULT,				\
.locp_offset = (locp_offs),						\
.fct_hash = (f_hash),						\
.fct_cmp = (f_compare)}						\
void hurd_ihash_init (hurd_ihash_t ht, intptr_t locp_offs);
void hurd_ihash_destroy (hurd_ihash_t ht);
error_t hurd_ihash_create (hurd_ihash_t *ht, intptr_t locp_offs);
void hurd_ihash_free (hurd_ihash_t ht);
void hurd_ihash_set_cleanup (hurd_ihash_t ht, hurd_ihash_cleanup_t cleanup,
void *cleanup_data);
void hurd_ihash_set_gki (hurd_ihash_t ht,
hurd_ihash_fct_hash_t fct_hash,
hurd_ihash_fct_cmp_t fct_cmp);
void hurd_ihash_set_max_load (hurd_ihash_t ht, unsigned int max_load);
static inline unsigned int
hurd_ihash_get_load (hurd_ihash_t ht)
{
int d = __builtin_ctzl (ht->size) - 7;
return d >= 0 ? ht->nr_items >> d : ht->nr_items << -d;
}
static inline unsigned int
hurd_ihash_get_effective_load (hurd_ihash_t ht)
{
int d = __builtin_ctzl (ht->size) - 7;
return
d >= 0 ? (ht->size - ht->nr_free) >> d : (ht->size - ht->nr_free) << -d;
}
error_t hurd_ihash_add (hurd_ihash_t ht, hurd_ihash_key_t key,
hurd_ihash_value_t item);
error_t hurd_ihash_locp_add (hurd_ihash_t ht, hurd_ihash_locp_t locp,
hurd_ihash_key_t key, hurd_ihash_value_t value);
hurd_ihash_value_t hurd_ihash_find (hurd_ihash_t ht, hurd_ihash_key_t key);
hurd_ihash_value_t hurd_ihash_locp_find (hurd_ihash_t ht,
hurd_ihash_key_t key,
hurd_ihash_locp_t *slot);
#define HURD_IHASH_ITERATE(ht, val)					\
for (hurd_ihash_value_t val,						\
*_hurd_ihash_valuep = (ht)->size ? &(ht)->items[0].value : 0;	\
(ht)->size							\
&& (size_t) ((_hurd_ihash_item_t) _hurd_ihash_valuep		\
- &(ht)->items[0])				\
< (ht)->size						\
&& (val = *_hurd_ihash_valuep, 1);				\
_hurd_ihash_valuep = (hurd_ihash_value_t *)			\
(((_hurd_ihash_item_t) _hurd_ihash_valuep) + 1))		\
if (val != _HURD_IHASH_EMPTY && val != _HURD_IHASH_DELETED)
#define HURD_IHASH_ITERATE_ITEMS(ht, item)                              \
for (_hurd_ihash_item_t item = (ht)->size? &(ht)->items[0]: 0;	\
(ht)->size && item - &(ht)->items[0] < (ht)->size;               \
item++)                                                          \
if (item->value != _HURD_IHASH_EMPTY &&                             \
item->value != _HURD_IHASH_DELETED)
int hurd_ihash_remove (hurd_ihash_t ht, hurd_ihash_key_t key);
void hurd_ihash_locp_remove (hurd_ihash_t ht, hurd_ihash_locp_t locp);
uint32_t hurd_ihash_hash32 (const void *buf, size_t len, uint32_t seed);
#endif