#include <mach/message.h>
#include <mach/mach_types.h>
#include <mach/vm_param.h>
#include <syscalls.h>
#include <testlib.h>
#include <mach.user.h>
#include <mach_port.user.h>
void test_mach_port(void)
{
kern_return_t err;
mach_port_name_t *namesp;
mach_msg_type_number_t namesCnt=0;
mach_port_type_t *typesp;
mach_msg_type_number_t typesCnt=0;
err = mach_port_names(mach_task_self(), &namesp, &namesCnt, &typesp, &typesCnt);
ASSERT_RET(err, "mach_port_names");
printf("mach_port_names: type/name length: %d %d\n", namesCnt, typesCnt);
ASSERT((namesCnt != 0) && (namesCnt == typesCnt),
"mach_port_names: wrong type/name length");
for (int i=0; i<namesCnt; i++)
printf("port name %d type %x\n", namesp[i], typesp[i]);
mach_port_type_t pt;
err = mach_port_type(mach_task_self(), host_priv(), &pt);
ASSERT_RET(err, "mach_port_type host_priv");
ASSERT(pt == MACH_PORT_TYPE_SEND, "wrong type for host_priv");
err = mach_port_type(mach_task_self(), device_priv(), &pt);
ASSERT_RET(err, "mach_port_type device_priv");
ASSERT(pt == MACH_PORT_TYPE_SEND, "wrong type for device_priv");
err = mach_port_rename(mach_task_self(), device_priv(), 111);
ASSERT_RET(err, "mach_port_rename");
err = mach_port_rename(mach_task_self(), 111, 112);
ASSERT_RET(err, "mach_port_rename restore orig");
const mach_port_t nrx = 1000, nset = 1001, ndead = 1002;
err = mach_port_allocate_name(mach_task_self(), MACH_PORT_RIGHT_RECEIVE, nrx);
ASSERT_RET(err, "mach_port_allocate_name rx");
err = mach_port_allocate_name(mach_task_self(), MACH_PORT_RIGHT_PORT_SET, nset);
ASSERT_RET(err, "mach_port_allocate_name pset");
err = mach_port_allocate_name(mach_task_self(), MACH_PORT_RIGHT_DEAD_NAME, ndead);
ASSERT_RET(err, "mach_port_allocate_name dead");
mach_port_t newname = nrx, oldname = nrx;
err = mach_port_allocate(mach_task_self(), MACH_PORT_RIGHT_RECEIVE, &newname);
ASSERT_RET(err, "mach_port_allocate");
ASSERT(newname != nrx, "allocated name didn't change");
oldname = newname;
newname = nrx;
err = mach_port_allocate(mach_task_self(), MACH_PORT_RIGHT_PORT_SET, &newname);
ASSERT_RET(err, "mach_port_allocate");
ASSERT(newname != nrx, "allocated name didn't change");
ASSERT(newname != oldname, "allocated name is duplicated");
oldname = newname;
newname = nrx;
err = mach_port_allocate(mach_task_self(), MACH_PORT_RIGHT_DEAD_NAME, &newname);
ASSERT_RET(err, "mach_port_allocate");
ASSERT(newname != nrx, "allocated name didn't change");
ASSERT(newname != oldname, "allocated name is duplicated");
err = mach_port_destroy(mach_task_self(), newname);
ASSERT_RET(err, "mach_port_destroy");
mach_port_urefs_t urefs;
err = mach_port_get_refs(mach_task_self(), nrx, MACH_PORT_RIGHT_RECEIVE, &urefs);
ASSERT_RET(err, "mach_port_get_refs");
ASSERT(urefs == 1, "rx port urefs");
err = mach_port_get_refs(mach_task_self(), nset, MACH_PORT_RIGHT_PORT_SET, &urefs);
ASSERT_RET(err, "mach_port_get_refs");
ASSERT(urefs == 1, "pset port urefs");
err = mach_port_get_refs(mach_task_self(), ndead, MACH_PORT_RIGHT_DEAD_NAME, &urefs);
ASSERT_RET(err, "mach_port_get_refs");
ASSERT(urefs == 1, "dead port urefs");
err = mach_port_destroy(mach_task_self(), nrx);
ASSERT_RET(err, "mach_port_destroy rx");
err = mach_port_destroy(mach_task_self(), nset);
ASSERT_RET(err, "mach_port_destroy pset");
err = mach_port_deallocate(mach_task_self(), ndead);
ASSERT_RET(err, "mach_port_deallocate dead");
}
int main(int argc, char *argv[], int envc, char *envp[])
{
test_mach_port();
return 0;
}