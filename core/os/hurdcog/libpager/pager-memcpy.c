#include "priv.h"
#include "pager.h"
#include <sys/mman.h>
#include <hurd/sigpreempt.h>
#include <assert-backtrace.h>
#include <string.h>
#define VMCOPY_BETTER_THAN_MEMCPY (8*vm_page_size)
error_t
pager_memcpy (struct pager *pager, memory_object_t memobj,
vm_offset_t offset, void *other, size_t *size,
vm_prot_t prot)
{
error_t err;
size_t n = *size;
#define VMCOPY_WINDOW_DEFAULT_SIZE (32 * vm_page_size)
#define MEMCPY_WINDOW_DEFAULT_SIZE (32 * vm_page_size)
vm_address_t window;
vm_size_t window_size;
error_t do_vm_copy (void)
{
assert_backtrace ((offset & (vm_page_size - 1)) == 0);
assert_backtrace (((vm_address_t) other & (vm_page_size - 1)) == 0);
assert_backtrace (n >= vm_page_size);
do
{
window_size =
VMCOPY_WINDOW_DEFAULT_SIZE > n
? (n - (n & (vm_page_size - 1)))
: VMCOPY_WINDOW_DEFAULT_SIZE;
assert_backtrace (window_size >= VMCOPY_BETTER_THAN_MEMCPY);
assert_backtrace ((window_size & (vm_page_size - 1)) == 0);
window = 0;
err = vm_map (mach_task_self (), &window, window_size, 0, 1,
memobj, offset, 0, prot, prot, VM_INHERIT_NONE);
if (err)
return err;
if (prot == VM_PROT_READ)
err = vm_copy (mach_task_self (), window, window_size,
(vm_address_t) other);
else
err = vm_copy (mach_task_self (), (vm_address_t) other,
window_size, window);
vm_deallocate (mach_task_self (), window, window_size);
if (err)
return err;
other += window_size;
offset += window_size;
n -= window_size;
}
while (n >= VMCOPY_BETTER_THAN_MEMCPY);
return 0;
}
error_t do_copy (struct hurd_signal_preemptor *preemptor)
{
error_t do_memcpy (size_t to_copy)
{
window_size = MEMCPY_WINDOW_DEFAULT_SIZE;
do
{
size_t pageoff = offset & (vm_page_size - 1);
size_t copy_count = window_size - pageoff;
if (window_size >= round_page (pageoff + to_copy))
{
copy_count = to_copy;
window_size = round_page (pageoff + to_copy);
}
window = 0;
err = vm_map (mach_task_self (), &window, window_size, 0, 1,
memobj, offset - pageoff, 0,
prot, prot, VM_INHERIT_NONE);
if (err)
return err;
preemptor->first = window;
preemptor->last = window + window_size;
__sync_synchronize();
if (prot == VM_PROT_READ)
memcpy (other, (const void *) window + pageoff, copy_count);
else
memcpy ((void *) window + pageoff, other, copy_count);
vm_deallocate (mach_task_self (), window, window_size);
assert_backtrace (n >= copy_count);
assert_backtrace (to_copy >= copy_count);
offset += copy_count;
other += copy_count;
to_copy -= copy_count;
n -= copy_count;
}
while (to_copy > 0);
return 0;
}
if ((((vm_address_t) other & (vm_page_size - 1))
== (offset & (vm_page_size - 1)))
&& (n >= (VMCOPY_BETTER_THAN_MEMCPY + vm_page_size
- ((vm_address_t) other & (vm_page_size - 1)))))
{
err = do_memcpy (vm_page_size
- ((vm_address_t) other & (vm_page_size - 1)));
if (err)
return err;
assert_backtrace (n >= VMCOPY_BETTER_THAN_MEMCPY);
err = do_vm_copy ();
if (err || n == 0)
return err;
assert_backtrace (n < VMCOPY_BETTER_THAN_MEMCPY);
}
return do_memcpy (n);
}
jmp_buf buf;
void fault (int signo, long int sigcode, struct sigcontext *scp)
{
assert_backtrace (scp->sc_error == EKERN_MEMORY_ERROR);
err = pager_get_error (pager, sigcode - window + offset);
n -= sigcode - window;
vm_deallocate (mach_task_self (), window, window_size);
siglongjmp (buf, 1);
}
if (n == 0)
return 0;
if (((vm_address_t) other & (vm_page_size - 1)) == 0
&& (offset & (vm_page_size - 1)) == 0
&& n >= VMCOPY_BETTER_THAN_MEMCPY)
{
err = do_vm_copy ();
if (err || n == 0)
{
*size -= n;
return err;
}
assert_backtrace (n < VMCOPY_BETTER_THAN_MEMCPY);
}
window = 0;
window_size = 0;
if (sigsetjmp (buf, 1) == 0)
{
sigset_t mask;
sigemptyset (&mask);
sigaddset (&mask, SIGSEGV);
sigaddset (&mask, SIGBUS);
hurd_catch_signal (mask, window, window + window_size,
&do_copy, (sighandler_t) &fault);
}
if (! err)
assert_backtrace (n == 0);
*size -= n;
return err;
}