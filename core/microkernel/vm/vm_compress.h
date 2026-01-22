#ifndef _VM_COMPRESS_H_
#define _VM_COMPRESS_H_
#include <mach/boolean.h>
#include <vm/vm_types.h>
#include <vm/vm_object.h>
#include <vm/vm_page.h>
#include <kern/queue.h>
#include <kern/lock.h>
struct vm_compressed_page {
queue_chain_t		hash_link;
vm_object_t		object;
vm_offset_t		offset;
void			*compressed_data;
vm_size_t		compressed_size;
unsigned int		access_time;
};
struct vm_compress_stats {
unsigned int		compressed_pages;
unsigned int		decompressed_pages;
unsigned int		average_compression_ratio;
};
void vm_compress_init(void);
kern_return_t vm_page_compress(vm_object_t object, vm_offset_t offset, vm_page_t page);
kern_return_t vm_page_decompress(vm_object_t object, vm_offset_t offset, vm_page_t page);
kern_return_t vm_page_compress_remove(vm_object_t object, vm_offset_t offset);
void vm_compress_get_stats(struct vm_compress_stats *stats);
#endif