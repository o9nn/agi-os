#ifndef _MD_H
#define _MD_H
#include <linux/major.h>
#include <linux/ioctl.h>
#include <linux/types.h>
#define MD_MAJOR_VERSION		0
#define MD_MINOR_VERSION		36
#define MD_PATCHLEVEL_VERSION		6
#define MD_DEFAULT_DISK_READAHEAD	(256 * 1024)
#define REGISTER_DEV 		_IO (MD_MAJOR, 1)
#define START_MD     		_IO (MD_MAJOR, 2)
#define STOP_MD      		_IO (MD_MAJOR, 3)
#define REGISTER_DEV_NEW	_IO (MD_MAJOR, 4)
#define FAULT_SHIFT       8
#define PERSONALITY_SHIFT 16
#define FACTOR_MASK       0x000000FFUL
#define FAULT_MASK        0x0000FF00UL
#define PERSONALITY_MASK  0x00FF0000UL
#define MD_RESERVED       0
#define LINEAR            (1UL << PERSONALITY_SHIFT)
#define STRIPED           (2UL << PERSONALITY_SHIFT)
#define RAID0             STRIPED
#define RAID1             (3UL << PERSONALITY_SHIFT)
#define RAID5             (4UL << PERSONALITY_SHIFT)
#define MAX_PERSONALITY   5
#define MD_RESERVED_BYTES		(64 * 1024)
#define MD_RESERVED_SECTORS		(MD_RESERVED_BYTES / 512)
#define MD_RESERVED_BLOCKS		(MD_RESERVED_BYTES / BLOCK_SIZE)
#define MD_NEW_SIZE_SECTORS(x)		((x & ~(MD_RESERVED_SECTORS - 1)) - MD_RESERVED_SECTORS)
#define MD_NEW_SIZE_BLOCKS(x)		((x & ~(MD_RESERVED_BLOCKS - 1)) - MD_RESERVED_BLOCKS)
#define MD_SB_BYTES			4096
#define MD_SB_WORDS			(MD_SB_BYTES / 4)
#define MD_SB_BLOCKS			(MD_SB_BYTES / BLOCK_SIZE)
#define MD_SB_SECTORS			(MD_SB_BYTES / 512)
#define	MD_SB_GENERIC_OFFSET		0
#define MD_SB_PERSONALITY_OFFSET	64
#define MD_SB_DISKS_OFFSET		128
#define MD_SB_DESCRIPTOR_OFFSET		992
#define MD_SB_GENERIC_CONSTANT_WORDS	32
#define MD_SB_GENERIC_STATE_WORDS	32
#define MD_SB_GENERIC_WORDS		(MD_SB_GENERIC_CONSTANT_WORDS + MD_SB_GENERIC_STATE_WORDS)
#define MD_SB_PERSONALITY_WORDS		64
#define MD_SB_DISKS_WORDS		384
#define MD_SB_DESCRIPTOR_WORDS		32
#define MD_SB_RESERVED_WORDS		(1024 - MD_SB_GENERIC_WORDS - MD_SB_PERSONALITY_WORDS - MD_SB_DISKS_WORDS - MD_SB_DESCRIPTOR_WORDS)
#define MD_SB_EQUAL_WORDS		(MD_SB_GENERIC_WORDS + MD_SB_PERSONALITY_WORDS + MD_SB_DISKS_WORDS)
#define MD_SB_DISKS			(MD_SB_DISKS_WORDS / MD_SB_DESCRIPTOR_WORDS)
#define MD_FAULTY_DEVICE		0
#define MD_ACTIVE_DEVICE		1
#define MD_SYNC_DEVICE			2
typedef struct md_device_descriptor_s {
__u32 number;
__u32 major;
__u32 minor;
__u32 raid_disk;
__u32 state;
__u32 reserved[MD_SB_DESCRIPTOR_WORDS - 5];
} md_descriptor_t;
#define MD_SB_MAGIC		0xa92b4efc
#define MD_SB_CLEAN		0
#define MD_SB_ERRORS		1
typedef struct md_superblock_s {
__u32 md_magic;
__u32 major_version;
__u32 minor_version;
__u32 patch_version;
__u32 gvalid_words;
__u32 set_magic;
__u32 ctime;
__u32 level;
__u32 size;
__u32 nr_disks;
__u32 raid_disks;
__u32 gstate_creserved[MD_SB_GENERIC_CONSTANT_WORDS - 11];
__u32 utime;
__u32 state;
__u32 active_disks;
__u32 working_disks;
__u32 failed_disks;
__u32 spare_disks;
__u32 gstate_sreserved[MD_SB_GENERIC_STATE_WORDS - 6];
__u32 parity_algorithm;
__u32 chunk_size;
__u32 pstate_reserved[MD_SB_PERSONALITY_WORDS - 2];
md_descriptor_t disks[MD_SB_DISKS];
__u32 reserved[MD_SB_RESERVED_WORDS];
md_descriptor_t descriptor;
} md_superblock_t;
#ifdef __KERNEL__
#include <linux/mm.h>
#include <linux/fs.h>
#include <linux/blkdev.h>
#include <asm/semaphore.h>
#define SUPPORT_RECONSTRUCTION	0
#define MAX_REAL     8
#define MAX_MD_DEV   4
#define FACTOR(a)         ((a)->repartition & FACTOR_MASK)
#define MAX_FAULT(a)      (((a)->repartition & FAULT_MASK)>>8)
#define PERSONALITY(a)    ((a)->repartition & PERSONALITY_MASK)
#define FACTOR_SHIFT(a) (PAGE_SHIFT + (a) - 10)
struct real_dev
{
kdev_t dev;
int size;
int offset;
struct inode *inode;
md_superblock_t *sb;
u32 sb_offset;
};
struct md_dev;
#define SPARE_INACTIVE	0
#define SPARE_WRITE	1
#define SPARE_ACTIVE	2
struct md_personality
{
char *name;
int (*map)(struct md_dev *mddev, kdev_t *rdev,
unsigned long *rsector, unsigned long size);
int (*make_request)(struct md_dev *mddev, int rw, struct buffer_head * bh);
void (*end_request)(struct buffer_head * bh, int uptodate);
int (*run)(int minor, struct md_dev *mddev);
int (*stop)(int minor, struct md_dev *mddev);
int (*status)(char *page, int minor, struct md_dev *mddev);
int (*ioctl)(struct inode *inode, struct file *file,
unsigned int cmd, unsigned long arg);
int max_invalid_dev;
int (*error_handler)(struct md_dev *mddev, kdev_t dev);
int (*hot_add_disk) (struct md_dev *mddev, kdev_t dev);
int (*hot_remove_disk) (struct md_dev *mddev, kdev_t dev);
int (*mark_spare) (struct md_dev *mddev, md_descriptor_t *descriptor, int state);
};
struct md_dev
{
struct real_dev	devices[MAX_REAL];
struct md_personality	*pers;
md_superblock_t	*sb;
int			sb_dirty;
int			repartition;
int			busy;
int			nb_dev;
void			*private;
};
struct md_thread {
void			(*run) (void *data);
void			*data;
struct wait_queue	*wqueue;
unsigned long           flags;
struct semaphore	*sem;
struct task_struct	*tsk;
};
#define THREAD_WAKEUP  0
extern struct md_dev md_dev[MAX_MD_DEV];
extern int md_size[MAX_MD_DEV];
extern int md_maxreadahead[MAX_MD_DEV];
extern char *partition_name (kdev_t dev);
extern int register_md_personality (int p_num, struct md_personality *p);
extern int unregister_md_personality (int p_num);
extern struct md_thread *md_register_thread (void (*run) (void *data), void *data);
extern void md_unregister_thread (struct md_thread *thread);
extern void md_wakeup_thread(struct md_thread *thread);
extern int md_update_sb (int minor);
extern int md_do_sync(struct md_dev *mddev);
#endif __KERNEL__
#endif _MD_H