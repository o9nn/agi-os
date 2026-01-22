#include <syscalls.h>
#include <testlib.h>
#include <mach/machine/vm_param.h>
#include <mach/std_types.h>
#include <mach/mach_types.h>
#include <mach/vm_wire.h>
#include <mach/vm_param.h>
#include <device.user.h>
#include <gnumach.user.h>
#include <mach.user.h>
#include <mach_port.user.h>
static void test_memobj()
{
struct mapped_time_value *mtime;
int64_t secs, usecs;
mach_port_t device, memobj;
int err;
err = device_open (device_priv(), 0, "time", &device);
ASSERT_RET(err, "device_open");
err = device_map (device, VM_PROT_READ, 0, sizeof(*mtime), &memobj, 0);
ASSERT_RET(err, "device_map");
err = mach_port_deallocate (mach_task_self (), device);
ASSERT_RET(err, "mach_port_deallocate");
mtime = 0;
err = vm_map(mach_task_self (), (vm_address_t *)&mtime, sizeof *mtime, 0, 1,
memobj, 0, 0, VM_PROT_READ, VM_PROT_READ, VM_INHERIT_NONE);
ASSERT_RET(err, "vm_map");
do
{
secs = mtime->seconds;
__sync_synchronize ();
usecs = mtime->microseconds;
__sync_synchronize ();
}
while (secs != mtime->check_seconds);
printf("mapped time is %lld.%lld\n",secs, usecs);
err = mach_port_deallocate (mach_task_self (), memobj);
ASSERT_RET(err, "mach_port_deallocate");
err = vm_deallocate(mach_task_self(), (vm_address_t)mtime, sizeof(*mtime));
ASSERT_RET(err, "vm_deallocate");
}
static void test_wire()
{
int err = vm_wire_all(host_priv(), mach_task_self(), VM_WIRE_ALL);
ASSERT_RET(err, "vm_wire_all-ALL");
err = vm_wire_all(host_priv(), mach_task_self(), VM_WIRE_NONE);
ASSERT_RET(err, "vm_wire_all-NONE");
}
int main(int argc, char *argv[], int envc, char *envp[])
{
printf("VM_MIN_ADDRESS=0x%p\n", VM_MIN_ADDRESS);
printf("VM_MAX_ADDRESS=0x%p\n", VM_MAX_ADDRESS);
test_wire();
test_memobj();
return 0;
}