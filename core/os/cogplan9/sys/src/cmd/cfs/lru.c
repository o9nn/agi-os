#include <u.h>
#include "lru.h"
void
lruinit(Lru *h)
{
h->lprev = h->lnext = h;
}
void
lruadd(Lru *h, Lru *m)
{
h->lprev->lnext = m;
m->lprev = h->lprev;
h->lprev = m;
m->lnext = h;
}
void
lruref(Lru *h, Lru *m)
{
if(h->lprev == m)
return;
m->lprev->lnext = m->lnext;
m->lnext->lprev = m->lprev;
h->lprev->lnext = m;
m->lprev = h->lprev;
h->lprev = m;
m->lnext = h;
}
void
lruderef(Lru *h, Lru *m)
{
if(h->lnext == m)
return;
m->lprev->lnext = m->lnext;
m->lnext->lprev = m->lprev;
h->lnext->lprev = m;
m->lnext = h->lnext;
h->lnext = m;
m->lprev = h;
}