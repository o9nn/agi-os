#include "priv.h"
error_t __attribute__ ((weak))
diskfs_validate_group_change (struct node *np, gid_t group)
{
return 0;
}