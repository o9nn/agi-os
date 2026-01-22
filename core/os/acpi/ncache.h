#ifndef NCACHE_H
#define NCACHE_H
#include <hurd/netfs.h>
#include <acpifs.h>
void node_cache (struct node *node);
void node_unlink (struct node *node, struct acpifs *fs);
#endif