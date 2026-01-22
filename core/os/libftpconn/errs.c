#include <errno.h>
#include <ftpconn.h>
#include "priv.h"
const error_t
ftp_conn_poss_file_errs[] =
{
EIO, ENOENT, EPERM, EACCES, ENOTDIR, ENAMETOOLONG, ELOOP, EISDIR, EROFS,
EMFILE, ENFILE, ENXIO, EOPNOTSUPP, ENOSPC, EDQUOT, ETXTBSY, EEXIST,
0
};