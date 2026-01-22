#include <string.h>
#include <hurd/store.h>
#include "ext2fs.h"
#include "libdiskfs/fs_S.h"
kern_return_t
diskfs_S_file_get_storage_info (struct protid *cred,
mach_port_t **ports,
mach_msg_type_name_t *ports_type,
mach_msg_type_number_t *num_ports,
int **ints, mach_msg_type_number_t *num_ints,
off_t **offsets,
mach_msg_type_number_t *num_offsets,
data_t *data, mach_msg_type_number_t *data_len)
{
error_t err = 0;
unsigned num_fs_blocks;
struct store *file_store;
struct store_run *runs, *run = 0;
block_t index = 0;
size_t num_runs = 0, runs_alloced = 10;
struct node *node = cred->po->np;
runs = malloc (runs_alloced * sizeof (struct store_run));
if (! runs)
return ENOMEM;
pthread_mutex_lock (&node->lock);
if (node->dn_stat.st_size < node->dn_stat.st_blocks * 512)
num_fs_blocks =
(node->dn_stat.st_size + block_size - 1) >> log2_block_size;
else
num_fs_blocks = node->dn_stat.st_blocks >> log2_stat_blocks_per_fs_block;
while (num_fs_blocks-- > 0)
{
block_t block;
err = ext2_getblk (node, index++, 0, &block);
if (err == EINVAL)
err = EOPNOTSUPP;
if (err)
break;
block <<= log2_dev_blocks_per_fs_block;
if (num_runs == 0
|| block != run->start + run->length)
{
if (num_runs == runs_alloced)
{
struct store_run *new;
runs_alloced *= 2;
new = realloc (runs, runs_alloced * sizeof (struct store_run));
if (! new)
{
err = ENOMEM;
break;
}
runs = new;
}
run = runs + num_runs++;
run->start = block;
run->length = 0;
}
run->length += 1 << log2_dev_blocks_per_fs_block;
}
pthread_mutex_unlock (&node->lock);
if (! err)
err = store_clone (store, &file_store);
if (! err)
{
err = store_remap (file_store, runs, num_runs, &file_store);
if (!err
&& !idvec_contains (cred->user->uids, 0)
&& !store_is_securely_returnable (file_store, cred->po->openstat))
{
err = store_set_flags (file_store, STORE_INACTIVE);
if (err == EINVAL)
err = EACCES;
}
if (! err)
{
*ports_type = MACH_MSG_TYPE_COPY_SEND;
err = store_return (file_store, ports, num_ports, ints, num_ints,
offsets, num_offsets, data, data_len);
}
store_free (file_store);
}
free (runs);
return err;
}