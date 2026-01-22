#include "priv.h"
error_t __attribute__ ((weak))
diskfs_validate_owner_change (struct node *np, uid_t uid)
{
return 0;
}