#include <fcntl.h>
#include <hurd.h>
#include "store.h"
error_t
store_open (const char *name, int flags,
const struct store_class *const *classes,
struct store **store)
{
error_t err;
int open_flags = (flags & STORE_HARD_READONLY) ? O_RDONLY : O_RDWR;
file_t node = file_name_lookup (name, open_flags, 0);
if (node == MACH_PORT_NULL && !(flags & STORE_HARD_READONLY)
&& (errno == EACCES || errno == EROFS))
{
flags |= STORE_HARD_READONLY;
node = file_name_lookup (name, O_RDONLY, 0);
}
if (node == MACH_PORT_NULL)
return errno;
err = store_create (node, flags, classes, store);
if (err)
{
if (! (flags & STORE_NO_FILEIO))
err = store_file_create (node, flags, store);
if (err)
mach_port_deallocate (mach_task_self (), node);
}
return err;
}
const struct store_class
store_query_class = { -1, "query", open: store_open };
STORE_STD_CLASS (query);