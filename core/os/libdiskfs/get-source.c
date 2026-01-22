#include "priv.h"
error_t __attribute__ ((weak))
diskfs_get_source (char *source, size_t source_len)
{
if (diskfs_disk_name == NULL)
return EOPNOTSUPP;
strncpy (source, diskfs_disk_name, source_len - 1);
source[source_len - 1] = '\0';
return 0;
}