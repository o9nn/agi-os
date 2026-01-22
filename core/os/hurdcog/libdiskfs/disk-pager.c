#include "priv.h"
#include "diskfs-pager.h"
#include <hurd/sigpreempt.h>
#include <error.h>
__thread struct disk_image_user *diskfs_exception_diu;
struct pager *diskfs_disk_pager;
struct pager_requests *diskfs_disk_pager_requests;
static void fault_handler (int sig, long int sigcode, struct sigcontext *scp);
static struct hurd_signal_preemptor preemptor =
{
preemptor: NULL,
handler: (sighandler_t) &fault_handler,
};
void
diskfs_start_disk_pager (struct user_pager_info *upi,
struct port_bucket *pager_bucket,
int may_cache, int notify_on_evict,
size_t size, void **image)
{
error_t err;
mach_port_t disk_pager_port;
err = pager_start_workers (pager_bucket, &diskfs_disk_pager_requests);
if (err)
error (2, err, "creating pager worker threads failed");
diskfs_disk_pager = pager_create (upi, pager_bucket,
may_cache, MEMORY_OBJECT_COPY_NONE,
notify_on_evict);
if (diskfs_disk_pager == NULL)
error (2, errno, "creating diskfs_disk_pager failed");
disk_pager_port = pager_get_port (diskfs_disk_pager);
mach_port_insert_right (mach_task_self (), disk_pager_port, disk_pager_port,
MACH_MSG_TYPE_MAKE_SEND);
*image = 0;
err = vm_map (mach_task_self (), (vm_address_t *)image, size,
0, 1, disk_pager_port, 0, 0,
VM_PROT_READ | (diskfs_readonly ? 0 : VM_PROT_WRITE),
VM_PROT_READ | VM_PROT_WRITE,
VM_INHERIT_NONE);
if (err)
error (2, err, "cannot vm_map whole disk");
preemptor.first = (vm_address_t) *image;
preemptor.last = ((vm_address_t) *image + size);
sigemptyset (&preemptor.signals);
sigaddset (&preemptor.signals, SIGSEGV);
sigaddset (&preemptor.signals, SIGBUS);
hurd_preempt_signals (&preemptor);
mach_port_deallocate (mach_task_self (), disk_pager_port);
}
static void
fault_handler (int sig, long int sigcode, struct sigcontext *scp)
{
jmp_buf *env;
error_t err;
#ifndef NDEBUG
if (diskfs_exception_diu == NULL)
{
error (0, 0,
"BUG: unexpected fault on disk image (%d, %#lx) in [%#lx,%#lx)"
" eip %#zx err %#x",
sig, sigcode,
preemptor.first, preemptor.last,
scp->sc_pc, scp->sc_error);
assert_backtrace (scp->sc_error == EKERN_MEMORY_ERROR);
err = pager_get_error (diskfs_disk_pager, sigcode);
assert_backtrace (err);
assert_perror_backtrace (err);
}
#endif
env = &diskfs_exception_diu->env;
diskfs_exception_diu = diskfs_exception_diu->next;
assert_backtrace (scp->sc_error == EKERN_MEMORY_ERROR);
err = pager_get_error (diskfs_disk_pager, sigcode);
assert_backtrace (err);
longjmp (*env, err);
}