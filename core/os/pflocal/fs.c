#include <fcntl.h>
#include "sock.h"
#include "sserver.h"
#include "fs_S.h"
kern_return_t
S_file_check_access (struct sock_user *cred, int *type)
{
if (!cred)
return EOPNOTSUPP;
*type = 0;
if (cred->sock->read_pipe)
*type |= O_READ;
if (cred->sock->write_pipe)
*type |= O_WRITE;
return 0;
}