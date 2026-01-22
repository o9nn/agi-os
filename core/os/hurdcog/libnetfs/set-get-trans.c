#include "netfs.h"
error_t __attribute__ ((weak))
netfs_set_translator (struct iouser *cred, struct node *np,
const char *argz, mach_msg_type_number_t argzlen)
{
return EOPNOTSUPP;
}
error_t __attribute__ ((weak))
netfs_get_translator (struct node *node, char **argz, mach_msg_type_number_t *argz_len)
{
return EOPNOTSUPP;
}