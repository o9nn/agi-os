#include "priv.h"
error_t __attribute__ ((weak))
diskfs_validate_author_change (struct node *np, uid_t author)
{
return 0;
}