#include <string.h>
#include <error.h>
#include <argp.h>
#include <argz.h>
#include <limits.h>
#include <version.h>
#include "fatfs.h"
#include "libdiskfs/fsys_S.h"
struct node *diskfs_root_node;
struct store *store = 0;
struct store_parsed *store_parsed = 0;
char *diskfs_disk_name = 0;
char *diskfs_server_name = "fatfs";
char *diskfs_server_version = HURD_VERSION;
char *diskfs_extra_version = "GNU Hurd";
int diskfs_synchronous = 0;
int diskfs_link_max = 1;
int diskfs_name_max = FAT_NAME_MAX;
int diskfs_maxsymlinks = 8;
vm_address_t zerocluster;
struct dirrect dr_root_node;
uid_t default_fs_uid;
gid_t default_fs_gid;
uid_t fs_uid;
gid_t fs_gid;
static const struct argp_option options[] =
{
{ "uid", 'U', "uid", 0, "Default uid for files" },
{ "gid", 'G', "gid", 0, "Default gid for files" },
{ 0 }
};
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
switch (key)
{
case 'U':
if (arg)
fs_uid = atoi (arg);
refresh_node_stats ();
break;
case 'G':
if (arg)
fs_gid = atoi (arg);
refresh_node_stats ();
break;
case ARGP_KEY_INIT:
state->child_inputs[0] = state->input;
break;
case ARGP_KEY_SUCCESS:
break;
default:
return ARGP_ERR_UNKNOWN;
}
return 0;
}
static const struct argp_child startup_children[] =
{ { &diskfs_store_startup_argp }, { 0 } };
static struct argp startup_argp =
{ options, parse_opt, 0, 0, startup_children };
static const struct argp_child runtime_children[] =
{ { &diskfs_std_runtime_argp }, { 0 } };
static struct argp runtime_argp =
{ options, parse_opt, 0, 0, runtime_children };
struct argp *diskfs_runtime_argp = (struct argp *) &runtime_argp;
error_t
diskfs_append_args (char **argz, size_t *argz_len)
{
error_t err;
char buf[100];
err = diskfs_append_std_options (argz, argz_len);
if (!err && fs_uid != default_fs_uid)
{
snprintf (buf, sizeof buf, "--uid=%d", fs_uid);
err = argz_add (argz, argz_len, buf);
}
if (!err && fs_gid != default_fs_gid)
{
snprintf (buf, sizeof buf, "--gid=%d", fs_gid);
err = argz_add (argz, argz_len, buf);
}
if (! err)
err = store_parsed_append_args (store_parsed, argz, argz_len);
return err;
}
static void
fetch_root (void)
{
error_t err;
ino_t inum;
struct lookup_context ctx;
memset (&dr_root_node, 0, sizeof(struct dirrect));
dr_root_node.attribute = FAT_DIR_ATTR_DIR;
if (fat_type == FAT32)
{
dr_root_node.first_cluster_high[1]
= sblock->compat.fat32.root_cluster[3];
dr_root_node.first_cluster_high[0]
= sblock->compat.fat32.root_cluster[2];
dr_root_node.first_cluster_low[1] = sblock->compat.fat32.root_cluster[1];
dr_root_node.first_cluster_low[0] = sblock->compat.fat32.root_cluster[0];
}
switch (fat_type)
{
case FAT12:
case FAT16:
write_dword(dr_root_node.file_size, nr_of_root_dir_sectors
<< log2_bytes_per_sector);
break;
case FAT32:
{
cluster_t rootdir;
int cs = 0;
rootdir = (cluster_t) *sblock->compat.fat32.root_cluster;
while (rootdir != FAT_EOC)
{
fat_get_next_cluster (rootdir, &rootdir);
cs++;
}
write_dword (dr_root_node.file_size, cs << log2_bytes_per_cluster);
}
break;
default:
assert_backtrace (!"don't know how to set size of root dir");
};
err = vi_new ((struct vi_key) {0, 1}, &inum, &ctx.inode);
assert_perror_backtrace (err);
if (!err)
err = diskfs_cached_lookup_context (inum, &diskfs_root_node, &ctx);
assert_perror_backtrace (err);
pthread_mutex_unlock (&diskfs_root_node->lock);
}
int
main (int argc, char **argv)
{
mach_port_t bootstrap;
default_fs_uid = getuid ();
default_fs_gid = getgid ();
fs_uid = default_fs_uid;
fs_gid = default_fs_gid;
diskfs_readonly = 1;
diskfs_hard_readonly = 1;
store = diskfs_init_main (&startup_argp, argc, argv, &store_parsed,
&bootstrap);
fat_read_sblock ();
create_fat_pager ();
zerocluster = (vm_address_t) mmap (0, bytes_per_cluster, PROT_READ|PROT_WRITE,
MAP_ANON, 0, 0);
fetch_root ();
diskfs_startup_diskfs (bootstrap, 0);
pthread_exit (NULL);
return 0;
}
error_t
diskfs_reload_global_state (void)
{
return 0;
}
error_t
diskfs_set_hypermetadata (int wait, int clean)
{
return 0;
}
void
diskfs_readonly_changed (int readonly)
{
abort ();
}
kern_return_t
diskfs_S_fsys_getfile (struct diskfs_control *pt,
mach_port_t reply, mach_msg_type_name_t reply_type,
const uid_t *uids, mach_msg_type_number_t nuids,
const gid_t *gids, mach_msg_type_number_t ngids,
const_data_t handle, mach_msg_type_number_t handle_len,
mach_port_t *file, mach_msg_type_name_t *file_type)
{
return EOPNOTSUPP;
}