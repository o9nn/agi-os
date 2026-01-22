#ifndef _RAID5_H
#define _RAID5_H
#ifdef __KERNEL__
#include <linux/md.h>
#include <asm/atomic.h>
struct disk_info {
kdev_t dev;
int operational;
int number;
int raid_disk;
int write_only;
int spare;
};
struct stripe_head {
struct stripe_head *hash_next, **hash_pprev;
struct stripe_head *free_next;
struct buffer_head *buffer_pool;
struct buffer_head *bh_pool;
struct raid5_data *raid_conf;
struct buffer_head *bh_old[MD_SB_DISKS];
struct buffer_head *bh_new[MD_SB_DISKS];
struct buffer_head *bh_copy[MD_SB_DISKS];
struct buffer_head *bh_req[MD_SB_DISKS];
int cmd_new[MD_SB_DISKS];
int new[MD_SB_DISKS];
unsigned long sector;
int size;
int pd_idx;
int nr_pending;
unsigned long state;
int cmd;
int count;
int write_method;
int phase;
struct wait_queue *wait;
};
#define PHASE_BEGIN 0
#define PHASE_READ_OLD 1
#define PHASE_WRITE 2
#define PHASE_READ 3
#define PHASE_COMPLETE 4
#define METHOD_NONE 0
#define RECONSTRUCT_WRITE 1
#define READ_MODIFY_WRITE 2
#define STRIPE_LOCKED 0
#define STRIPE_ERROR 1
#define STRIPE_NONE 0
#define STRIPE_WRITE 1
#define STRIPE_READ 2
struct raid5_data {
struct stripe_head **stripe_hashtbl;
struct md_dev *mddev;
struct md_thread *thread, *resync_thread;
struct disk_info disks[MD_SB_DISKS];
struct disk_info *spare;
int buffer_size;
int chunk_size, level, algorithm;
int raid_disks, working_disks, failed_disks;
int sector_count;
unsigned long next_sector;
atomic_t nr_handle;
struct stripe_head *next_free_stripe;
int nr_stripes;
int resync_parity;
int max_nr_stripes;
int clock;
int nr_hashed_stripes;
int nr_locked_stripes;
int nr_pending_stripes;
int nr_cached_stripes;
int nr_free_sh;
struct stripe_head *free_sh_list;
struct wait_queue *wait_for_stripe;
};
#endif
#define ALGORITHM_LEFT_ASYMMETRIC 0
#define ALGORITHM_RIGHT_ASYMMETRIC 1
#define ALGORITHM_LEFT_SYMMETRIC 2
#define ALGORITHM_RIGHT_SYMMETRIC 3
#endif