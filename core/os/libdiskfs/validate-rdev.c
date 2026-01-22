#include "priv.h"
error_t __attribute__ ((weak))
diskfs_validate_rdev_change (struct node *np, dev_t rdev)
{
return 0;
}