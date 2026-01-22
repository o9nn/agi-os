#include <errno.h>
#include "error.h"
#include "memory-pool.h"
#include "utilities.h"
static size_t align_size(size_t element_size)
{
if (element_size < MAX_ALIGNMENT)
{
size_t s = next_power_of_two_up(element_size);
if (s != element_size)
element_size = ALIGN(element_size, s);
}
else
{
element_size = ALIGN(element_size, MIN_ALIGNMENT);
}
return element_size;
}
Pool_desc *pool_new(const char *func, const char *name,
size_t num_elements, size_t element_size,
bool zero_out, bool align, bool exact)
{
Pool_desc *mp = malloc(sizeof(Pool_desc));
mp->func = func;
mp->name = name;
if (align)
{
mp->element_size = align_size(element_size);
mp->alignment = MAX(MIN_ALIGNMENT, mp->element_size);
mp->alignment = MIN(MAX_ALIGNMENT, mp->alignment);
}
else
{
mp->element_size = element_size;
mp->alignment = MIN_ALIGNMENT;
}
mp->data_size = ALIGN(num_elements * mp->element_size, FLDSIZE_NEXT);
mp->block_size = ALIGN(mp->data_size + FLDSIZE_NEXT, mp->alignment);
mp->zero_out = zero_out;
#ifdef POOL_EXACT
mp->exact = exact;
#endif
mp->alloc_next = NULL;
mp->chain = NULL;
mp->ring = NULL;
#ifdef POOL_FREE
mp->free_list = NULL;
#endif
mp->issued_elements = 0;
mp->alloced_elements = 0;
mp->num_elements = num_elements;
mp->alloced_bytes = 0;
lgdebug(+D_MEMPOOL, "%sElement size %zu, alignment %zu (pool '%s' created in %s())\n",
POOL_ALLOCATOR?"":"(Fake pool allocator) ",
mp->element_size, mp->alignment, mp->name, mp->func);
return mp;
}
#if POOL_ALLOCATOR
#undef pool_delete
#ifndef DEBUG
void pool_delete (Pool_desc *mp)
#else
void pool_delete (const char *func, Pool_desc *mp)
#endif
{
if (NULL == mp) return;
const char *from_func = "";
#ifdef DEBUG
from_func = func;
#endif
lgdebug(+D_MEMPOOL, "Used %zu (%zu) elements (%s deleted pool '%s' created in %s())\n",
mp->issued_elements, mp->num_elements, from_func, mp->name, mp->func);
size_t alloc_size = mp->data_size;
char *c_next;
for (char *c = mp->chain; c != NULL; c = c_next)
{
c_next = POOL_NEXT_BLOCK(c, alloc_size);
aligned_free(c);
}
free(mp);
}
void *pool_alloc_vec(Pool_desc *mp, size_t vecsize)
{
dassert(vecsize < mp->num_elements,
"Pool %s: num_elements is too small %zu >= %zu)",
mp->name, vecsize, mp->num_elements);
if (vecsize >= mp->num_elements)
{
prt_error("Warning: Pool %s: num_elements is too small %zu >= %zu)\n",
mp->name, vecsize, mp->num_elements);
return NULL;
}
mp->issued_elements += vecsize;
#ifdef POOL_FREE
if ((NULL != mp->free_list) && (vecsize == 1))
{
void *alloc_next = mp->free_list;
ASAN_UNPOISON_MEMORY_REGION(alloc_next, mp->element_size);
mp->free_list = *(char **)mp->free_list;
if (mp->zero_out) memset(alloc_next, 0, mp->element_size);
return alloc_next;
}
#endif
size_t alloc_size = mp->element_size * vecsize;
if ((NULL == mp->alloc_next) ||
(mp->alloc_next + alloc_size > mp->ring + mp->data_size))
{
#ifdef POOL_EXACT
assert(!mp->exact || (NULL == mp->alloc_next),
"Too many elements %zu>%zu (pool '%s' created in %s())",
mp->issued_elements, mp->num_elements, mp->name, mp->func);
#endif
char *prev = mp->ring;
if (NULL != mp->ring)
{
mp->ring = POOL_NEXT_BLOCK(mp->ring, mp->data_size);
}
if (NULL == mp->ring)
{
mp->ring = aligned_alloc(mp->alignment, mp->block_size);
mp->alloced_elements += mp->num_elements;
mp->alloced_bytes += mp->block_size;
assert(NULL != mp->ring, "Aligned_alloc(%zu, %zu): %s",
mp->block_size, mp->element_size, syserror_msg(errno));
if (NULL == mp->alloc_next)
mp->chain = mp->ring;
else
POOL_NEXT_BLOCK(prev, mp->data_size) = mp->ring;
POOL_NEXT_BLOCK(mp->ring, mp->data_size) = NULL;
}
if (mp->zero_out) memset(mp->ring, 0, mp->data_size);
mp->alloc_next = mp->ring;
}
void *alloc_next = mp->alloc_next;
mp->alloc_next +=  alloc_size;
return alloc_next;
}
void pool_reuse(Pool_desc *mp)
{
lgdebug(+D_MEMPOOL, "Reuse %zu elements (pool '%s' created in %s())\n",
mp->issued_elements, mp->name, mp->func);
mp->ring = mp->chain;
mp->alloc_next = mp->ring;
if ((mp->ring != NULL) && (mp->zero_out)) memset(mp->ring, 0, mp->data_size);
mp->issued_elements = 0;
#ifdef POOL_FREE
mp->free_list = NULL;
#endif
}
#ifdef POOL_FREE
void pool_free(Pool_desc *mp, void *e)
{
assert(mp->element_size >= FLDSIZE_NEXT);
if (NULL == e) return;
mp->issued_elements--;
char *next = mp->free_list;
mp->free_list = e;
*(char **)e = next;
ASAN_POISON_MEMORY_REGION(e, mp->element_size);
}
#endif
#else
void *pool_alloc_vec(Pool_desc *mp, size_t vecsize)
{
dassert(vecsize < mp->num_elements, "Pool block is too small %zu > %zu)",
vecsize, mp->num_elements);
mp->issued_elements += vecsize;
mp->alloced_elements += vecsize;
size_t alloc_size = mp->element_size * vecsize;
#ifdef POOL_EXACT
assert(!mp->exact || mp->issued_elements <= mp->num_elements,
"Too many elements (%zu>%zu) (pool '%s' created in %s())",
mp->issued_elements, mp->num_elements, mp->name, mp->func);
#endif
char *next = mp->chain;
size_t totsz = sizeof(alloc_attr) + alloc_size;
mp->chain = malloc(totsz);
mp->alloced_bytes += totsz;
alloc_attr *at = (alloc_attr *)mp->chain;
at->next = next;
at->size = alloc_size;
char *alloc_next = mp->chain + sizeof(alloc_attr);
if (mp->zero_out) memset(alloc_next, 0, alloc_size);
return alloc_next;
}
void pool_reuse(Pool_desc *mp)
{
if (NULL == mp) return;
lgdebug(+D_MEMPOOL, "Reuse %zu elements (pool '%s' created in %s())\n",
mp->issued_elements, mp->name, mp->func);
char *c_next;
for (char *c = mp->chain; c != NULL; c = c_next)
{
alloc_attr *at = (alloc_attr *)c;
#ifdef POOL_FREE
ASAN_UNPOISON_MEMORY_REGION(c,  sizeof(alloc_attr) + at->size);
#endif
c_next = at->next;
free(c);
}
mp->chain = NULL;
mp->issued_elements = 0;
}
#undef pool_delete
#ifndef DEBUG
void pool_delete (Pool_desc *mp)
#else
void pool_delete (const char *func, Pool_desc *mp)
#endif
{
if (NULL == mp) return;
const char *from_func = "";
#ifdef DEBUG
from_func = func;
#endif
lgdebug(+D_MEMPOOL, "Used %zu (%zu) elements (%s deleted pool '%s' created in %s())\n",
mp->issued_elements, mp->num_elements, from_func, mp->name, mp->func);
char *c_next;
for (char *c = mp->chain; c != NULL; c = c_next)
{
alloc_attr *at = (alloc_attr *)c;
#ifdef POOL_FREE
ASAN_UNPOISON_MEMORY_REGION(c, sizeof(alloc_attr) + at->size);
#endif
c_next = at->next;
free(c);
}
free(mp);
}
#ifdef POOL_FREE
void pool_free(Pool_desc *mp, void *e)
{
mp->issued_elements--;
assert(!ASAN_ADDRESS_IS_POISONED(e), "Double pool free of %p\n", e);
ASAN_POISON_MEMORY_REGION(e, sizeof(alloc_attr) + ((alloc_attr *)e)->size);
}
#endif
#endif