#ifndef NETFS_IMPL_H
#define NETFS_IMPL_H
#include <hurd/netfs.h>
#include <acpifs.h>
struct netnode
{
struct acpifs_dirent *ln;
struct node *ncache_next, *ncache_prev;
};
#endif