#include <string.h>
#include "priv.h"
#include "fs_S.h"
#include "fhandle.h"
kern_return_t
diskfs_S_file_getfh (struct protid *cred, data_t *fh,
mach_msg_type_number_t *fh_len)
{
struct node *node;
union diskfs_fhandle *f;
if (! cred)
return EOPNOTSUPP;
if (! idvec_contains (cred->user->uids, 0))
return EPERM;
assert_backtrace (sizeof *f == sizeof f->bytes);
node = cred->po->np;
pthread_mutex_lock (&node->lock);
if (*fh_len < sizeof (union diskfs_fhandle))
{
*fh = mmap (0, sizeof (union diskfs_fhandle), PROT_READ|PROT_WRITE,
MAP_ANON, 0, 0);
assert_backtrace (*fh != MAP_FAILED);
}
*fh_len = sizeof *f;
f = (union diskfs_fhandle *) *fh;
memset (f, 0, sizeof *f);
f->data.cache_id = node->cache_id;
f->data.gen = node->dn_stat.st_gen;
pthread_mutex_unlock (&node->lock);
return 0;
}