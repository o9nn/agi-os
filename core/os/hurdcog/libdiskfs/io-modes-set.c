#include "priv.h"
#include "io_S.h"
#include <fcntl.h>
kern_return_t
diskfs_S_io_set_all_openmodes (struct protid *cred,
int newbits)
{
if (!cred)
return EOPNOTSUPP;
pthread_mutex_lock (&cred->po->np->lock);
iohelp_get_conch (&cred->po->np->conch);
cred->po->openstat &= ~HONORED_STATE_MODES;
cred->po->openstat |= (newbits & HONORED_STATE_MODES);
pthread_mutex_unlock (&cred->po->np->lock);
return 0;
}