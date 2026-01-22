#include "priv.h"
#include <stdlib.h>
#include <string.h>
error_t
_pager_pagemap_resize (struct pager *p, vm_address_t off)
{
off /= __vm_page_size;
if (p->pagemapsize < off)
{
void *newaddr = reallocarray (p->pagemap, off,
sizeof (*p->pagemap));
if (!newaddr)
return errno;
memset ((short *) newaddr + p->pagemapsize, 0,
(off - p->pagemapsize) * sizeof (*p->pagemap));
p->pagemap = newaddr;
p->pagemapsize = off;
}
return 0;
}