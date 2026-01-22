#if HAVE_CONFIG_H
#include <config.h>
#endif
#include <errno.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <assert-backtrace.h>
#include "ihash.h"
static inline hurd_ihash_key_t
hash (hurd_ihash_t ht, hurd_ihash_key_t k)
{
return ht->fct_hash ? ht->fct_hash ((const void *) k) : k;
}
static inline int
compare (hurd_ihash_t ht, hurd_ihash_key_t a, hurd_ihash_key_t b)
{
return
ht->fct_cmp ? (a && ht->fct_cmp ((const void *) a, (const void *) b))
: a == b;
}
static inline int
index_empty (hurd_ihash_t ht, unsigned int idx)
{
return ! hurd_ihash_value_valid (ht->items[idx].value);
}
static inline int
index_valid (hurd_ihash_t ht, unsigned int idx, hurd_ihash_key_t key)
{
return !index_empty (ht, idx) && compare (ht, ht->items[idx].key, key);
}
static inline int
find_index (hurd_ihash_t ht, hurd_ihash_key_t key)
{
unsigned int idx;
unsigned int up_idx;
unsigned int first_deleted = 0;
int first_deleted_set = 0;
unsigned int mask = ht->size - 1;
idx = hash (ht, key) & mask;
up_idx = idx;
do
{
if (ht->items[up_idx].value == _HURD_IHASH_EMPTY)
return first_deleted_set ? first_deleted : up_idx;
if (compare (ht, ht->items[up_idx].key, key))
return up_idx;
if (! first_deleted_set
&& ht->items[up_idx].value == _HURD_IHASH_DELETED)
first_deleted = up_idx, first_deleted_set = 1;
up_idx = (up_idx + 1) & mask;
}
while (up_idx != idx);
return first_deleted;
}
static inline void
locp_remove (hurd_ihash_t ht, hurd_ihash_locp_t locp)
{
struct _hurd_ihash_item *item = (struct _hurd_ihash_item *) locp;
assert_backtrace (hurd_ihash_value_valid (item->value));
if (ht->cleanup)
(*ht->cleanup) (item->value, ht->cleanup_data);
item->value = _HURD_IHASH_DELETED;
item->key = 0;
ht->nr_items--;
}
void
hurd_ihash_init (hurd_ihash_t ht, intptr_t locp_offs)
{
ht->nr_items = 0;
ht->size = 0;
ht->locp_offset = locp_offs;
ht->max_load = HURD_IHASH_MAX_LOAD_DEFAULT;
ht->cleanup = 0;
ht->fct_hash = NULL;
ht->fct_cmp = NULL;
ht->nr_free = 0;
}
void
hurd_ihash_destroy (hurd_ihash_t ht)
{
if (ht->cleanup)
{
hurd_ihash_cleanup_t cleanup = ht->cleanup;
void *cleanup_data = ht->cleanup_data;
HURD_IHASH_ITERATE (ht, value)
(*cleanup) (value, cleanup_data);
}
if (ht->size > 0)
free (ht->items);
}
error_t
hurd_ihash_create (hurd_ihash_t *ht, intptr_t locp_offs)
{
*ht = malloc (sizeof (struct hurd_ihash));
if (*ht == NULL)
return ENOMEM;
hurd_ihash_init (*ht, locp_offs);
return 0;
}
void
hurd_ihash_free (hurd_ihash_t ht)
{
hurd_ihash_destroy (ht);
free (ht);
}
void
hurd_ihash_set_cleanup (hurd_ihash_t ht, hurd_ihash_cleanup_t cleanup,
void *cleanup_data)
{
ht->cleanup = cleanup;
ht->cleanup_data = cleanup_data;
}
void
hurd_ihash_set_gki (hurd_ihash_t ht,
hurd_ihash_fct_hash_t fct_hash,
hurd_ihash_fct_cmp_t fct_cmp)
{
assert (ht->size == 0 || !"called after insertion");
assert (fct_hash);
assert (fct_cmp);
ht->fct_hash = fct_hash;
ht->fct_cmp = fct_cmp;
}
void
hurd_ihash_set_max_load (hurd_ihash_t ht, unsigned int max_load)
{
ht->max_load = max_load;
}
static inline int
add_one (hurd_ihash_t ht, hurd_ihash_key_t key, hurd_ihash_value_t value)
{
unsigned int idx;
idx = find_index (ht, key);
if (index_valid (ht, idx, key))
locp_remove (ht, &ht->items[idx].value);
if (index_empty (ht, idx))
{
ht->nr_items++;
if (ht->items[idx].value == _HURD_IHASH_EMPTY)
{
assert (ht->nr_free > 0);
ht->nr_free--;
}
ht->items[idx].value = value;
ht->items[idx].key = key;
if (ht->locp_offset != HURD_IHASH_NO_LOCP)
*((hurd_ihash_locp_t *) (((char *) value) + ht->locp_offset))
= &ht->items[idx].value;
return 1;
}
return 0;
}
error_t
hurd_ihash_locp_add (hurd_ihash_t ht, hurd_ihash_locp_t locp,
hurd_ihash_key_t key, hurd_ihash_value_t value)
{
struct _hurd_ihash_item *item = (struct _hurd_ihash_item *) locp;
if (ht->size == 0
|| item == NULL
|| (hurd_ihash_value_valid (item->value)
&& ! compare (ht, item->key, key))
|| hurd_ihash_get_effective_load (ht) > ht->max_load)
return hurd_ihash_add (ht, key, value);
if (! hurd_ihash_value_valid (item->value))
{
item->key = key;
ht->nr_items += 1;
if (item->value == _HURD_IHASH_EMPTY)
{
assert (ht->nr_free > 0);
ht->nr_free -= 1;
}
}
else
{
assert (compare (ht, item->key, key));
if (ht->cleanup)
(*ht->cleanup) (locp, ht->cleanup_data);
}
item->value = value;
if (ht->locp_offset != HURD_IHASH_NO_LOCP)
*((hurd_ihash_locp_t *) (((char *) value) + ht->locp_offset))
= locp;
return 0;
}
error_t
hurd_ihash_add (hurd_ihash_t ht, hurd_ihash_key_t key, hurd_ihash_value_t item)
{
struct hurd_ihash old_ht = *ht;
int was_added;
int fatal = 0;
unsigned int i;
if (ht->size)
{
if (hurd_ihash_get_effective_load (ht) <= ht->max_load)
add_one:
if (add_one (ht, key, item))
return 0;
}
ht->nr_items = 0;
if (ht->size == 0)
ht->size = HURD_IHASH_MIN_SIZE;
else if (hurd_ihash_get_load (&old_ht) > ht->max_load)
ht->size <<= 1;
ht->nr_free = ht->size;
ht->items = calloc (ht->size, sizeof (struct _hurd_ihash_item));
if (ht->items == NULL)
{
*ht = old_ht;
if (fatal || ht->size == 0)
return ENOMEM;
fatal = 1;
goto add_one;
}
for (i = 0; i < old_ht.size; i++)
if (!index_empty (&old_ht, i))
{
was_added = add_one (ht, old_ht.items[i].key, old_ht.items[i].value);
assert (was_added);
}
was_added = add_one (ht, key, item);
assert (was_added);
if (old_ht.size > 0)
free (old_ht.items);
return 0;
}
hurd_ihash_value_t
hurd_ihash_find (hurd_ihash_t ht, hurd_ihash_key_t key)
{
if (ht->size == 0)
return NULL;
else
{
int idx = find_index (ht, key);
return index_valid (ht, idx, key) ? ht->items[idx].value : NULL;
}
}
hurd_ihash_value_t
hurd_ihash_locp_find (hurd_ihash_t ht,
hurd_ihash_key_t key,
hurd_ihash_locp_t *slot)
{
int idx;
if (ht->size == 0)
{
*slot = NULL;
return NULL;
}
idx = find_index (ht, key);
*slot = &ht->items[idx].value;
return index_valid (ht, idx, key) ? ht->items[idx].value : NULL;
}
int
hurd_ihash_remove (hurd_ihash_t ht, hurd_ihash_key_t key)
{
if (ht->size != 0)
{
int idx = find_index (ht, key);
if (index_valid (ht, idx, key))
{
locp_remove (ht, &ht->items[idx].value);
return 1;
}
}
return 0;
}
void
hurd_ihash_locp_remove (hurd_ihash_t ht, hurd_ihash_locp_t locp)
{
locp_remove (ht, locp);
}