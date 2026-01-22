#ifndef ACPIFS_H
#define ACPIFS_H
#include <hurd/netfs.h>
#include <pthread.h>
#include <maptime.h>
#include <netfs_impl.h>
#include "myacpi.h"
#ifndef NAME_SIZE
#define NAME_SIZE 8
#endif
#define NODE_CACHE_MAX 16
struct acpifs_perm
{
int32_t uid;
int32_t gid;
};
struct acpifs_dirent
{
char name[NAME_SIZE];
struct acpifs_dirent *parent;
io_statbuf_t stat;
struct acpifs_dir *dir;
struct node *node;
struct acpi_table *acpitable;
};
struct acpifs_dir
{
uint16_t num_entries;
struct acpifs_dirent **entries;
};
struct acpifs
{
struct node *root;
struct node *node_cache_mru, *node_cache_lru;
size_t node_cache_len;
size_t node_cache_max;
pthread_mutex_t node_cache_lock;
mach_port_t next_task;
struct acpifs_perm perm;
struct acpifs_dirent *entries;
size_t num_entries;
};
extern struct acpifs *fs;
extern volatile struct mapped_time_value *acpifs_maptime;
#define UPDATE_TIMES(e, what) (\
{\
fshelp_touch (&e->stat, what, acpifs_maptime);\
if(e->node)\
fshelp_touch (&e->node->nn_stat, what, acpifs_maptime);\
}\
)
#define UPDATE_OWNER(e, uid) (\
{\
e->stat.st_uid = uid;\
if(e->node)\
e->node->nn_stat.st_uid = uid;\
}\
)
#define UPDATE_GROUP(e, gid) (\
{\
e->stat.st_gid = gid;\
if(e->node)\
e->node->nn_stat.st_gid = gid;\
}\
)
error_t alloc_file_system (struct acpifs **fs);
error_t init_file_system (struct acpifs *fs);
error_t init_root_node (file_t underlying_node);
error_t create_fs_tree (struct acpifs *fs);
error_t fs_set_permissions (struct acpifs *fs);
error_t entry_check_perms (struct iouser *user, struct acpifs_dirent *e,
int flags);
#endif