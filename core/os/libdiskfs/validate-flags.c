#include "priv.h"
error_t __attribute__ ((weak))
diskfs_validate_flags_change (struct node *np, int flags)
{
return 0;
}