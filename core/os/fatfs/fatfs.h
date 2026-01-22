#include <sys/types.h>
#include <sys/mman.h>
#include <hurd/diskfs.h>
#include <hurd/diskfs-pager.h>
#include <hurd/store.h>
#include "fat.h"
#include "virt-inode.h"
struct disknode
{
cluster_t start_cluster;
inode_t inode;
struct node *dirnode;
pthread_rwlock_t dirent_lock;
char *link_target;
size_t translen;
char *translator;
pthread_rwlock_t alloc_lock;
pthread_spinlock_t chain_extension_lock;
struct cluster_chain *first;
struct cluster_chain *last;
cluster_t length_of_chain;
int chain_complete;
struct pager *pager;
int dir_idx;
};
struct lookup_context
{
inode_t inode;
vm_address_t buf;
struct node *dir;
};
struct user_pager_info
{
struct node *node;
enum pager_type
{
FAT,
FILE_DATA,
} type;
vm_prot_t max_prot;
};
extern struct store *store;
extern uid_t fs_uid;
extern gid_t fs_gid;
extern void *fat_image;
extern vm_address_t zerocluster;
extern struct dirrect dr_root_node;
#define LOG2_BLOCKS_PER_CLUSTER \
(log2_bytes_per_cluster - store->log2_block_size)
#define round_cluster(offs) \
((((offs) + bytes_per_cluster - 1) \
>> log2_bytes_per_cluster) << log2_bytes_per_cluster)
#define FAT_FIRST_CLUSTER_BLOCK(cluster) \
(((cluster - 2) << LOG2_BLOCKS_PER_CLUSTER) + \
(first_data_byte >> store->log2_block_size))
void drop_pager_softrefs (struct node *);
void allow_pager_softrefs (struct node *);
void create_fat_pager (void);
error_t inhibit_fat_pager (void);
void resume_fat_pager (void);
void flush_node_pager (struct node *node);
void write_all_disknodes (void);
error_t fat_get_next_cluster (cluster_t cluster, cluster_t *next_cluster);
void fat_to_unix_filename (const char *, char *);
error_t diskfs_cached_lookup_in_dirbuf (int cache_id, struct node **npp,
vm_address_t buf);
void refresh_node_stats (void);