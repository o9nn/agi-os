#ifndef PCIFS_H
#define PCIFS_H
#include <hurd/netfs.h>
#include <pthread.h>
#include <maptime.h>
#include <pciaccess.h>
#define PCI_CONFIG_SIZE 256
#include <netfs_impl.h>
#ifndef NAME_SIZE
#define NAME_SIZE 16
#endif
#define NODE_CACHE_MAX 16
struct pcifs_dirent
{
int32_t domain;
int16_t bus;
int16_t dev;
int8_t func;
int32_t device_class;
char name[NAME_SIZE];
struct pcifs_dirent *parent;
io_statbuf_t stat;
struct pcifs_dir *dir;
struct node *node;
struct pci_device *device;
void *region_maps[6];
void *rom_map;
};
struct pcifs_dir
{
uint16_t num_entries;
struct pcifs_dirent **entries;
};
struct pcifs_perm
{
int32_t domain;
int16_t bus;
int16_t dev;
int8_t func;
int16_t d_class;
int16_t d_subclass;
int32_t uid;
int32_t gid;
};
struct pcifs_params
{
size_t node_cache_max;
mach_port_t next_task;
struct pcifs_perm *perms;
size_t num_perms;
};
struct pcifs
{
struct node *root;
struct pcifs_params params;
struct node *node_cache_mru, *node_cache_lru;
size_t node_cache_len;
pthread_mutex_t node_cache_lock;
pthread_mutex_t pci_conf_lock;
struct pcifs_dirent *entries;
size_t num_entries;
};
extern struct pcifs *fs;
extern volatile struct mapped_time_value *pcifs_maptime;
#define UPDATE_TIMES(e, what) (\
{\
fshelp_touch (&e->stat, what, pcifs_maptime);\
if(e->node)\
fshelp_touch (&e->node->nn_stat, what, pcifs_maptime);\
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
error_t alloc_file_system (struct pcifs **fs);
error_t init_root_node (file_t underlying_node);
error_t init_file_system (struct pcifs *fs);
error_t create_fs_tree (struct pcifs *fs);
error_t fs_set_permissions (struct pcifs *fs);
error_t entry_check_perms (struct iouser *user, struct pcifs_dirent *e,
int flags);
#endif