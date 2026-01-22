#ifndef _MEMORY_POOL_H
#define _MEMORY_POOL_H
#include <stddef.h>
#include "link-includes.h"
#include "error.h"
#include "utilities.h"
#ifndef D_MEMPOOL
#define D_MEMPOOL (D_SPEC+4)
#endif
#define MIN_ALIGNMENT sizeof(void *)
#define MAX_ALIGNMENT 64
typedef struct Pool_desc_s Pool_desc;
Pool_desc *pool_new(const char *, const char *, size_t, size_t, bool, bool, bool);
void *pool_alloc_vec(Pool_desc *, size_t) GNUC_MALLOC;
void pool_reuse(Pool_desc *);
#ifndef DEBUG
void pool_delete(Pool_desc *);
#else
void pool_delete(const char *func, Pool_desc *);
#define pool_delete(...) pool_delete (__func__, __VA_ARGS__)
#endif
#ifdef POOL_FREE
void pool_free(Pool_desc *, void *e);
#endif
#ifndef POOL_ALLOCATOR
#define POOL_ALLOCATOR 1
#endif
#if !POOL_ALLOCATOR
typedef union
{
struct{
char *next;
size_t size;
};
max_align_t dummy;
} alloc_attr;
#endif
#define FLDSIZE_NEXT sizeof(char *)
#define POOL_NEXT_BLOCK(blk, offset_next) (*(char **)((blk)+(offset_next)))
struct  Pool_desc_s
{
char *ring;
char *alloc_next;
#ifdef POOL_FREE
char *free_list;
#endif
size_t block_size;
size_t data_size;
size_t alignment;
size_t num_elements;
char *chain;
size_t element_size;
const char *name;
const char *func;
size_t issued_elements;
size_t alloced_elements;
size_t alloced_bytes;
bool zero_out;
#ifdef POOL_EXACT
bool exact;
#endif
};
typedef struct
{
char *current_element;
char *block_end;
size_t element_number;
} Pool_location;
static inline void *pool_alloc(Pool_desc *mp)
{
return pool_alloc_vec(mp, 1);
}
static inline void *pool_next(Pool_desc *mp, Pool_location *l)
{
#ifdef POOL_FREE
assert(mp->free_list == NULL, "Cannot be called after pool_free()");
#endif
if (l->element_number == mp->issued_elements) return NULL;
if (l->element_number == 0)
{
l->element_number = 1;
#if POOL_ALLOCATOR
l->current_element = mp->chain;
l->block_end = mp->chain + mp->data_size;
#else
l->current_element = mp->chain + sizeof(alloc_attr);
#endif
return l->current_element;
}
#if POOL_ALLOCATOR
l->current_element += mp->element_size;
if (l->current_element == l->block_end)
{
l->current_element = *(char **)l->block_end;
dassert(l->current_element != NULL, "Truncated memory pool");
l->block_end = l->current_element + mp->data_size;
}
#else
alloc_attr *at = (alloc_attr *)(l->current_element - sizeof(alloc_attr));
l->current_element = at->next + sizeof(alloc_attr);
#endif
l->element_number++;
return l->current_element;
}
static inline size_t pool_num_elements_issued(Pool_desc *mp)
{
if (mp) return mp->issued_elements;
return 0;
}
static inline size_t pool_size(Pool_desc *mp)
{
if (mp) return mp->alloced_elements;
return 0;
}
static inline size_t pool_bytes(Pool_desc *mp)
{
if (mp) return mp->alloced_bytes;
return 0;
}
#if !defined(__has_feature)
#define __has_feature(x) 0
#endif
#if __has_feature(address_sanitizer) || defined(__SANITIZE_ADDRESS__)
#include <sanitizer/asan_interface.h>
#define ASAN_POISON_MEMORY_REGION(addr, size) \
__asan_poison_memory_region((addr), (size))
#define ASAN_UNPOISON_MEMORY_REGION(addr, size) \
__asan_unpoison_memory_region((addr), (size))
#define ASAN_ADDRESS_IS_POISONED(addr) \
__asan_address_is_poisoned(addr)
#else
#define ASAN_POISON_MEMORY_REGION(addr, size) \
((void)(addr), (void)(size))
#define ASAN_UNPOISON_MEMORY_REGION(addr, size) \
((void)(addr), (void)(size))
#define ASAN_ADDRESS_IS_POISONED(addr) \
((void)(addr), false)
#endif
#endif