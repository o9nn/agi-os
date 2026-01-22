#include <string.h>
#include <error.h>
#include "fatfs.h"
#include "libdiskfs/fs_S.h"
#ifndef UF_APPEND
#define UF_APPEND 0
#endif
#ifndef UF_NODUMP
#define UF_NODUMP 0
#endif
#ifndef UF_IMMUTABLE
#define UF_IMMUTABLE 0
#endif
error_t
diskfs_user_make_node (struct node **npp, struct lookup_context *ctx)
{
struct node *np;
struct disknode *dn;
np = diskfs_make_node_alloc (sizeof *dn);
if (np == NULL)
return ENOMEM;
dn = np->dn;
dn->pager = 0;
dn->first = 0;
dn->last = 0;
dn->length_of_chain = 0;
dn->chain_complete = 0;
dn->chain_extension_lock = PTHREAD_SPINLOCK_INITIALIZER;
pthread_rwlock_init (&dn->alloc_lock, NULL);
pthread_rwlock_init (&dn->dirent_lock, NULL);
dn->inode = ctx->inode;
dn->dirnode = ctx->dir;
*npp = np;
return 0;
}
error_t
diskfs_cached_lookup_in_dirbuf (int inum, struct node **npp, vm_address_t buf)
{
struct lookup_context ctx = { buf: buf, inode: vi_lookup (inum) };
return diskfs_cached_lookup_context (inum, npp, &ctx);
}
void
diskfs_node_norefs (struct node *np)
{
struct cluster_chain *last = np->dn->first;
while (last)
{
struct cluster_chain *next = last->next;
free(last);
last = next;
}
if (np->dn->translator)
free (np->dn->translator);
if (np->dn->dirnode)
diskfs_nrele (np->dn->dirnode);
assert_backtrace (!np->dn->pager);
free (np);
}
void
diskfs_user_try_dropping_softrefs (struct node *np)
{
drop_pager_softrefs (np);
}
void
diskfs_lost_hardrefs (struct node *np)
{
}
void
diskfs_new_hardrefs (struct node *np)
{
allow_pager_softrefs (np);
}
error_t
diskfs_user_read_node (struct node *np, struct lookup_context *ctx)
{
error_t err;
struct stat *st = &np->dn_stat;
struct disknode *dn = np->dn;
struct dirrect *dr;
struct node *dp = 0;
struct vi_key vk = vi_key(np->dn->inode);
vm_prot_t prot = VM_PROT_READ;
memory_object_t memobj;
vm_address_t buf = ctx->buf;
vm_size_t buflen = 0;
int our_buf = 0;
st->st_fstype = FSTYPE_MSLOSS;
st->st_fsid = getpid ();
st->st_ino = np->cache_id;
st->st_gen = 0;
st->st_rdev = 0;
st->st_nlink = 1;
st->st_uid = fs_uid;
st->st_gid = fs_gid;
st->st_rdev = 0;
np->dn->translator = 0;
np->dn->translen = 0;
st->st_flags = 0;
if (vk.dir_inode == 0 && vk.dir_offset == 2)
return 0;
if (vk.dir_inode == 0)
dr = &dr_root_node;
else
{
if (buf == 0)
{
dp = diskfs_cached_ifind (vk.dir_inode);
assert_backtrace (dp);
memobj = diskfs_get_filemap (dp, prot);
if (memobj == MACH_PORT_NULL)
return errno;
buflen = round_page (dp->dn_stat.st_size);
err = vm_map (mach_task_self (),
&buf, buflen, 0, 1, memobj, 0, 0, prot, prot, 0);
mach_port_deallocate (mach_task_self (), memobj);
our_buf = 1;
}
dr = (struct dirrect *) (buf + vk.dir_offset);
}
np->dn->dirnode = dp;
if (dp)
refcounts_ref (&dp->refcounts, NULL);
pthread_rwlock_rdlock (&np->dn->dirent_lock);
dn->start_cluster = (read_word (dr->first_cluster_high) << 16)
+ read_word (dr->first_cluster_low);
if (dr->attribute & FAT_DIR_ATTR_DIR)
{
st->st_mode = S_IFDIR | 0777;
if ((diskfs_root_node == 0 || np == diskfs_root_node)
&& (fat_type == FAT12 || fat_type == FAT16))
{
st->st_size = read_dword (dr->file_size);
np->allocsize = nr_of_root_dir_sectors << log2_bytes_per_sector;
}
else
{
np->allocsize = 0;
pthread_rwlock_rdlock (&dn->alloc_lock);
err = fat_extend_chain (np, FAT_EOC, 0);
pthread_rwlock_unlock (&dn->alloc_lock);
if (err)
{
if (our_buf && buf)
munmap ((caddr_t) buf, buflen);
return err;
}
st->st_size = np->allocsize;
}
}
else
{
unsigned offset;
st->st_mode = S_IFREG | 0666;
st->st_size = read_dword (dr->file_size);
np->allocsize = np->dn_stat.st_size;
offset = np->allocsize & (bytes_per_cluster - 1);
if (offset > 0)
np->allocsize += bytes_per_cluster - offset;
}
if (dr->attribute & FAT_DIR_ATTR_RDONLY)
st->st_mode &= ~0222;
{
struct timespec ts;
fat_to_epoch (dr->write_date, dr->write_time, &ts);
st->st_ctim = st->st_mtim = st->st_atim = ts;
}
st->st_blksize = bytes_per_sector;
st->st_blocks = (st->st_size - 1) / bytes_per_sector + 1;
pthread_rwlock_unlock (&np->dn->dirent_lock);
if (our_buf && buf)
munmap ((caddr_t) buf, buflen);
return 0;
}
error_t
diskfs_validate_owner_change (struct node *np, uid_t uid)
{
if (uid != 0)
return EINVAL;
return 0;
}
error_t
diskfs_validate_group_change (struct node *np, gid_t gid)
{
if (gid != 0)
return EINVAL;
return 0;
}
error_t
diskfs_validate_mode_change (struct node *np, mode_t mode)
{
return 0;
}
error_t
diskfs_validate_author_change (struct node *np, uid_t author)
{
return (author == np->dn_stat.st_uid) ? 0 : EINVAL;
}
error_t
diskfs_validate_flags_change (struct node *np, int flags)
{
if (flags & ~(UF_NODUMP | UF_IMMUTABLE | UF_APPEND))
return EINVAL;
else
return 0;
}
void
write_node (struct node *np)
{
error_t err;
struct stat *st = &np->dn_stat;
struct dirrect *dr;
struct node *dp;
struct vi_key vk = vi_key(np->dn->inode);
vm_prot_t prot = VM_PROT_READ | VM_PROT_WRITE;
memory_object_t memobj;
vm_address_t buf = 0;
vm_size_t buflen;
if (vk.dir_inode == 0 || np == diskfs_root_node)
return;
assert_backtrace (!np->dn_set_ctime && !np->dn_set_atime && !np->dn_set_mtime);
if (np->dn_stat_dirty)
{
assert_backtrace (!diskfs_readonly);
dp = np->dn->dirnode;
assert_backtrace (dp);
pthread_mutex_lock (&dp->lock);
memobj = diskfs_get_filemap (dp, prot);
if (memobj == MACH_PORT_NULL)
{
pthread_mutex_unlock (&dp->lock);
return;
}
buflen = round_page (dp->dn_stat.st_size);
err = vm_map (mach_task_self (),
&buf, buflen, 0, 1, memobj, 0, 0, prot, prot, 0);
mach_port_deallocate (mach_task_self (), memobj);
if (err)
{
pthread_mutex_unlock (&dp->lock);
error (1, err, "Could not map memory");
}
dr = (struct dirrect *) (buf + vk.dir_offset);
pthread_rwlock_wrlock (&np->dn->dirent_lock);
write_word (dr->first_cluster_low, np->dn->start_cluster & 0xffff);
write_word (dr->first_cluster_high, np->dn->start_cluster >> 16);
write_dword (dr->file_size, st->st_size);
fat_from_epoch ((unsigned char *) &dr->write_date,
(unsigned char *) &dr->write_time, &st->st_mtime);
pthread_rwlock_unlock (&np->dn->dirent_lock);
np->dn_stat_dirty = 0;
munmap ((caddr_t) buf, buflen);
pthread_mutex_unlock (&dp->lock);
}
}
error_t
diskfs_node_reload (struct node *node)
{
struct cluster_chain *last = node->dn->first;
static struct lookup_context ctx = { buf: 0 };
while (last)
{
struct cluster_chain *next = last->next;
free(last);
last = next;
}
flush_node_pager (node);
return diskfs_user_read_node (node, &ctx);
}
void
write_all_disknodes (void)
{
error_t write_one_disknode (struct node *node)
{
diskfs_set_node_times (node);
write_node (node);
return 0;
}
diskfs_node_iterate (write_one_disknode);
}
void
refresh_node_stats (void)
{
error_t refresh_one_node_stat (struct node *node)
{
node->dn_stat.st_uid = fs_uid;
node->dn_stat.st_gid = fs_gid;
return 0;
}
diskfs_node_iterate (refresh_one_node_stat);
}
void
diskfs_write_disknode (struct node *np, int wait)
{
write_node (np);
}
error_t
diskfs_set_statfs (struct statfs *st)
{
st->f_type = FSTYPE_MSLOSS;
st->f_bsize = bytes_per_sector;
st->f_blocks = total_sectors;
st->f_bfree = fat_get_freespace () * sectors_per_cluster;
st->f_bavail = st->f_bfree;
st->f_files = 0;
st->f_ffree = 0;
st->f_fsid = getpid ();
st->f_namelen = 0;
st->f_favail = st->f_ffree;
st->f_frsize = bytes_per_cluster;
return 0;
}
error_t
diskfs_set_translator (struct node *node,
const char *name, mach_msg_type_number_t namelen,
struct protid *cred)
{
assert_backtrace (!diskfs_readonly);
return EOPNOTSUPP;
}
error_t
diskfs_get_translator (struct node *node, char **namep, mach_msg_type_number_t *namelen)
{
assert_backtrace (0);
}
void
diskfs_shutdown_soft_ports (void)
{
}
error_t
diskfs_truncate (struct node *node, loff_t length)
{
error_t err;
loff_t offset;
diskfs_check_readonly ();
assert_backtrace (!diskfs_readonly);
if (length >= node->dn_stat.st_size)
return 0;
offset = length & (bytes_per_cluster - 1);
if (offset > 0)
{
diskfs_node_rdwr (node, (void *)zerocluster, length, bytes_per_cluster - offset,
1, 0, 0);
diskfs_file_update (node, 1);
}
pthread_rwlock_wrlock (&node->dn->alloc_lock);
node->dn_stat.st_size = length;
node->dn_set_mtime = 1;
node->dn_set_ctime = 1;
diskfs_node_update (node, 1);
err = diskfs_catch_exception ();
if (!err)
{
fat_truncate_node(node, round_cluster(length) >> log2_bytes_per_cluster);
node->allocsize = round_cluster(length);
diskfs_end_catch_exception ();
}
node->dn_set_mtime = 1;
node->dn_set_ctime = 1;
node->dn_stat_dirty = 1;
pthread_rwlock_unlock (&node->dn->alloc_lock);
return err;
}
kern_return_t
diskfs_S_file_get_storage_info (struct protid *cred,
mach_port_t **ports,
mach_msg_type_name_t *ports_type,
mach_msg_type_number_t *num_ports,
int **ints, mach_msg_type_number_t *num_ints,
loff_t **offsets,
mach_msg_type_number_t *num_offsets,
data_t *data, mach_msg_type_number_t *data_len)
{
return EOPNOTSUPP;
}
void
diskfs_free_node (struct node *np, mode_t old_mode)
{
assert_backtrace (!diskfs_readonly);
vi_free(np->dn->inode);
}
error_t
diskfs_alloc_node (struct node *dir, mode_t mode, struct node **node)
{
error_t err;
ino_t inum;
inode_t inode;
struct node *np;
struct lookup_context ctx = { dir: dir };
assert_backtrace (!diskfs_readonly);
err = vi_new((struct vi_key) {0,2}, &inum, &inode);
if (err)
return err;
err = diskfs_cached_lookup_context (inum, &np, &ctx);
if (err)
return err;
refcounts_ref (&dir->refcounts, NULL);
*node = np;
return 0;
}