#ifndef NETFS_IMPL
#define NETFS_IMPL
#include <hurd.h>
#include <mach.h>
#include "vdev.h"
struct netnode
{
struct lnode *ln;
char *name;
};
struct lnode
{
struct vether_device vdev;
struct stat st;
struct node *n;
};
extern file_t root_file;
extern volatile struct mapped_time_value *multiplexer_maptime;
error_t new_node (struct lnode *ln, struct node **np);
#endif