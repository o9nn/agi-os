#ifndef _tmpfs_h
#define _tmpfs_h 1
#include <hurd/diskfs.h>
#include <sys/types.h>
#include <dirent.h>
#include <stdint.h>
struct disknode
{
uint_fast8_t type;
unsigned int gen;
off_t size;
mode_t mode;
nlink_t nlink;
uid_t uid, author;
gid_t gid;
struct timespec atime, mtime, ctime;
unsigned int flags;
char *trans;
size_t translen;
union
{
char *lnk;
struct
{
mach_port_t memobj, ro_memobj;
vm_address_t memref;
unsigned int allocpages;
} reg;
struct
{
struct tmpfs_dirent *entries;
struct disknode *dotdot;
} dir;
dev_t chr, blk;
} u;
struct node *hnext, **hprevp;
};
struct tmpfs_dirent
{
struct tmpfs_dirent *next;
struct disknode *dn;
uint8_t namelen;
char name[0];
};
extern off_t tmpfs_page_limit;
extern mach_port_t default_pager;
extern unsigned int num_files;
extern off_t tmpfs_space_used;
static inline void
adjust_used (off_t change)
{
__atomic_add_fetch (&num_files, change, __ATOMIC_RELAXED);
}
static inline off_t
get_used (void)
{
return __atomic_load_n (&num_files, __ATOMIC_RELAXED);
}
#endif