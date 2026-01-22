#include "priv.h"
struct user_pager_info *
pager_get_upi (struct pager *p)
{
return p->upi;
}