#include "default_pager_U.h"
#include "priv.h"
#include "io_S.h"
kern_return_t
diskfs_S_io_map_cntl (struct protid *cred,
memory_object_t *ctlobj,
mach_msg_type_name_t *ctlobj_type)
{
if (!cred)
return EOPNOTSUPP;
assert_backtrace (__vm_page_size >= sizeof (struct shared_io));
pthread_mutex_lock (&cred->po->np->lock);
if (!cred->mapped)
{
default_pager_object_create (diskfs_default_pager, &cred->shared_object,
__vm_page_size);
vm_map (mach_task_self (), (vm_address_t *)&cred->mapped, vm_page_size,
0, 1, cred->shared_object, 0, 0,
VM_PROT_READ|VM_PROT_WRITE, VM_PROT_READ|VM_PROT_WRITE, 0);
cred->mapped->shared_page_magic = SHARED_PAGE_MAGIC;
cred->mapped->conch_status = USER_HAS_NOT_CONCH;
pthread_spin_init (&cred->mapped->lock, PTHREAD_PROCESS_PRIVATE);
*ctlobj = cred->shared_object;
*ctlobj_type = MACH_MSG_TYPE_COPY_SEND;
pthread_mutex_unlock (&cred->po->np->lock);
return 0;
}
else
{
pthread_mutex_unlock (&cred->po->np->lock);
return EBUSY;
}
}