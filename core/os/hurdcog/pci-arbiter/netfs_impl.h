#ifndef NETFS_IMPL_H
#define NETFS_IMPL_H
#include <hurd/netfs.h>
#include "pcifs.h"
struct netnode
{
struct pcifs_dirent *ln;
struct node *ncache_next, *ncache_prev;
};
#endif