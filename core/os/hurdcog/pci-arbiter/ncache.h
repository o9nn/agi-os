#ifndef NCACHE_H
#define NCACHE_H
#include <hurd/netfs.h>
#include "pcifs.h"
void node_cache (struct node *node);
void node_unlink (struct node *node, struct pcifs *fs);
#endif