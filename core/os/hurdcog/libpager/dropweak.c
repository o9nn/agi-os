#include "priv.h"
void
_pager_real_dropweak (void *arg)
{
struct pager *p = arg;
pager_dropweak (p->upi);
}