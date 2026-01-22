#include "priv.h"
#include <hurd/iohelp.h>
#include <fcntl.h>
void
iohelp_put_shared_data (void *arg)
{
struct protid *cred = arg;
cred->mapped->append_mode = (cred->po->openstat & O_APPEND);
cred->mapped->eof_notify = 0;
cred->mapped->do_sigio = (cred->po->openstat & O_FSYNC);
cred->mapped->use_file_size = 1;
cred->mapped->use_read_size = 0;
cred->mapped->optimal_transfer_size = cred->po->np->dn_stat.st_blksize;
cred->mapped->seekable = 1;
cred->mapped->use_prenotify_size = 1;
cred->mapped->use_postnotify_size = 0;
cred->mapped->use_readnotify_size = 0;
cred->mapped->prenotify_size = cred->po->np->allocsize;
cred->mapped->xx_file_pointer = cred->po->filepointer;
cred->mapped->rd_file_pointer = -1;
cred->mapped->wr_file_pointer = -1;
cred->mapped->file_size = cred->po->np->dn_stat.st_size;
cred->mapped->written = 0;
cred->mapped->accessed = 0;
}