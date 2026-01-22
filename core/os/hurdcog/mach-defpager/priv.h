#ifndef __MACH_DEFPAGER_PRIV_H__
#define __MACH_DEFPAGER_PRIV_H__
#include <mach.h>
#include <queue.h>
#include <hurd/ihash.h>
typedef unsigned int	bm_entry_t;
#define	NB_BM		32
#define	BM_MASK		0xffffffff
#define	howmany(a,b)	(((a) + (b) - 1)/(b))
#define	NO_BLOCK	((vm_offset_t)-1)
struct part {
pthread_mutex_t	p_lock;
char		*name;
vm_size_t	total_size;
vm_size_t	free;
unsigned int	id;
bm_entry_t	*bitmap;
boolean_t	going_away;
struct file_direct *file;
};
typedef	struct part	*partition_t;
struct partitions {
pthread_mutex_t	lock;
int		n_partitions;
partition_t	*partition_list;
};
extern struct partitions all_partitions;
typedef unsigned char	p_index_t;
#define	P_INDEX_INVALID	((p_index_t)-1)
#define	no_partition(x)	((x) == P_INDEX_INVALID)
#define	DEBUG_READER_CONFLICTS	0
#if	DEBUG_READER_CONFLICTS
int	default_pager_read_conflicts = 0;
#endif
union dp_map {
struct {
unsigned int	p_offset : 24,
p_index : 8;
} block;
union dp_map		*indirect;
};
typedef union dp_map	*dp_map_t;
#define	no_block(e)		((e).indirect == (dp_map_t)NO_BLOCK)
#define	invalidate_block(e)	((e).indirect = (dp_map_t)NO_BLOCK)
struct dpager {
pthread_mutex_t	lock;
#if	DEBUG_READER_CONFLICTS
int		readers;
boolean_t	writer;
#endif
dp_map_t	map;
vm_size_t	size;
vm_size_t	limit;
vm_size_t	byte_limit;
p_index_t	cur_partition;
#ifdef	CHECKSUM
vm_offset_t	*checksum;
#define	NO_CHECKSUM	((vm_offset_t)-1)
#endif
};
typedef struct dpager	*dpager_t;
#define	PAGEMAP_ENTRIES		64
#define	PAGEMAP_SIZE(npgs)	((npgs)*sizeof(vm_offset_t))
#define	INDIRECT_PAGEMAP_ENTRIES(npgs) \
((((npgs)-1)/PAGEMAP_ENTRIES) + 1)
#define	INDIRECT_PAGEMAP_SIZE(npgs) \
(INDIRECT_PAGEMAP_ENTRIES(npgs) * sizeof(vm_offset_t *))
#define	INDIRECT_PAGEMAP(size)	\
(size > PAGEMAP_ENTRIES)
#define	ROUNDUP_TO_PAGEMAP(npgs) \
(((npgs) + PAGEMAP_ENTRIES - 1) & ~(PAGEMAP_ENTRIES - 1))
struct dstruct {
hurd_ihash_locp_t htable_locp;
queue_chain_t	links;
pthread_mutex_t	lock;
pthread_cond_t
waiting_seqno,
waiting_read,
waiting_write,
waiting_refs;
memory_object_t	pager;
mach_port_seqno_t seqno;
mach_port_t	pager_request;
mach_port_urefs_t request_refs;
mach_port_t	pager_name;
mach_port_urefs_t name_refs;
boolean_t	external;
unsigned int	readers;
unsigned int	writers;
mach_port_t	lock_request;
unsigned int	errors;
struct dpager	dpager;
};
typedef struct dstruct *	default_pager_t;
#define	DEFAULT_PAGER_NULL	((default_pager_t)0)
struct pager_port {
struct hurd_ihash	htable;
pthread_mutex_t	lock;
queue_head_t	leak_queue;
};
extern struct pager_port all_pagers;
#endif