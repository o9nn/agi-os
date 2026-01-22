#ifndef _VM_BLOCK_CACHE_H_
#define _VM_BLOCK_CACHE_H_
#include <mach/boolean.h>
#include <vm/vm_types.h>
#include <kern/queue.h>
#include <kern/lock.h>
#include <vm/vm_object.h>
#include <vm/vm_page.h>
#define BLOCK_CACHE_MIN_BLOCK_SIZE	512
#define BLOCK_CACHE_MAX_BLOCK_SIZE	65536
#define BLOCK_CACHE_DEFAULT_BLOCKS	1024
#define BLOCK_CACHE_HASH_SIZE		256
typedef enum {
BLOCK_CACHE_CLEAN = 0,
BLOCK_CACHE_DIRTY = 1,
BLOCK_CACHE_READING = 2,
BLOCK_CACHE_WRITING = 3,
BLOCK_CACHE_ERROR = 4
} block_cache_state_t;
struct block_cache_entry {
queue_chain_t	hash_link;
queue_chain_t	lru_link;
queue_chain_t	object_link;
vm_object_t	object;
vm_offset_t	block_offset;
vm_size_t	block_size;
block_cache_state_t state;
unsigned int	ref_count;
unsigned int	access_time;
unsigned int	access_frequency;
boolean_t	read_ahead_hint;
boolean_t	write_cluster_hint;
unsigned int	page_count;
vm_page_t	*pages;
simple_lock_data_t lock;
int		waiters;
};
typedef struct block_cache_entry *block_cache_entry_t;
struct block_cache {
vm_size_t	block_size;
unsigned int	max_blocks;
unsigned int	total_blocks;
unsigned int	hits;
unsigned int	misses;
unsigned int	reads;
unsigned int	writes;
queue_head_t	hash_buckets[BLOCK_CACHE_HASH_SIZE];
queue_head_t	lru_queue;
queue_head_t	block_list;
simple_lock_data_t lock;
vm_object_t	object;
};
typedef struct block_cache *block_cache_t;
void vm_block_cache_init(void);
block_cache_t block_cache_create(vm_object_t object, vm_size_t block_size);
void block_cache_destroy(block_cache_t cache);
kern_return_t block_cache_read(block_cache_t cache, vm_offset_t offset,
vm_size_t size, void **data);
kern_return_t block_cache_write(block_cache_t cache, vm_offset_t offset,
vm_size_t size, void *data);
kern_return_t block_cache_flush(block_cache_t cache, vm_offset_t offset,
vm_size_t size);
void block_cache_set_read_ahead(block_cache_t cache, boolean_t enable);
void block_cache_set_write_clustering(block_cache_t cache, boolean_t enable);
void block_cache_memory_pressure(block_cache_t cache, vm_size_t target_free);
void block_cache_get_stats(block_cache_t cache,
unsigned int *hits, unsigned int *misses,
unsigned int *total_blocks);
kern_return_t vm_object_enable_block_cache(vm_object_t object,
vm_size_t block_size);
void vm_object_disable_block_cache(vm_object_t object);
boolean_t vm_object_has_block_cache(vm_object_t object);
kern_return_t vm_page_to_block_read(vm_object_t object, vm_offset_t offset,
vm_page_t *pages, unsigned int page_count);
kern_return_t vm_page_to_block_write(vm_object_t object, vm_offset_t offset,
vm_page_t *pages, unsigned int page_count);
static inline unsigned int
block_cache_hash(vm_offset_t offset)
{
return (offset >> PAGE_SHIFT) % BLOCK_CACHE_HASH_SIZE;
}
static inline boolean_t
block_cache_offset_aligned(vm_offset_t offset, vm_size_t block_size)
{
return (offset & (block_size - 1)) == 0;
}
static inline vm_size_t
block_cache_round_to_block(vm_size_t size, vm_size_t block_size)
{
return (size + block_size - 1) & ~(block_size - 1);
}
#endif