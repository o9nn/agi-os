#include <fcntl.h>
#include "priv.h"
#include "io_S.h"
kern_return_t
diskfs_S_io_map (struct protid *cred,
memory_object_t *rdobj,
mach_msg_type_name_t *rdtype,
memory_object_t *wrobj,
mach_msg_type_name_t *wrtype)
{
int flags;
struct node *node;
if (!cred)
return EOPNOTSUPP;
*wrobj = *rdobj = MACH_PORT_NULL;
node = cred->po->np;
flags = cred->po->openstat & (O_READ | O_WRITE);
pthread_mutex_lock (&node->lock);
switch (flags)
{
case O_READ | O_WRITE:
*wrobj = *rdobj = diskfs_get_filemap (node, VM_PROT_READ |VM_PROT_WRITE);
if (*wrobj == MACH_PORT_NULL)
goto error;
mach_port_mod_refs (mach_task_self (), *rdobj, MACH_PORT_RIGHT_SEND, 1);
break;
case O_READ:
*rdobj = diskfs_get_filemap (node, VM_PROT_READ);
if (*rdobj == MACH_PORT_NULL)
goto error;
break;
case O_WRITE:
*wrobj = diskfs_get_filemap (node, VM_PROT_WRITE);
if (*wrobj == MACH_PORT_NULL)
goto error;
break;
}
pthread_mutex_unlock (&node->lock);
*rdtype = MACH_MSG_TYPE_MOVE_SEND;
*wrtype = MACH_MSG_TYPE_MOVE_SEND;
return 0;
error:
pthread_mutex_unlock (&node->lock);
return errno;
}