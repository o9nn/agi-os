#include "priv.h"
error_t __attribute__ ((weak))
diskfs_validate_mode_change (struct node *np, mode_t mode)
{
return 0;
}