#include <kern/sched_prim.h>
#include "ipc_target.h"
void
ipc_target_init(struct ipc_target *ipt, mach_port_name_t name)
{
ipt->ipt_name = name;
ipc_mqueue_init(&ipt->ipt_messages);
#ifdef MIGRATING_THREADS
ipt->ipt_type = IPT_TYPE_MESSAGE_RPC;
ipt->ipt_acts = 0;
ipc_target_machine_init(ipt);
#endif
}
void
ipc_target_terminate(struct ipc_target *ipt)
{
}
#ifdef MIGRATING_THREADS
struct Act *
ipc_target_block(struct ipc_target *ipt)
{
struct Act *act;
ipt_lock(ipt);
while ((act = ipt->ipt_acts) == 0) {
ipt->ipt_waiting = 1;
ipt_unlock(ipt);
thread_wait((int)&ipt->ipt_acts, FALSE);
ipt_lock(ipt);
}
ipt->ipt_acts = act->ipt_next;
ipt_unlock(ipt);
return act;
}
void
ipc_target_wakeup(struct ipc_target *ipt)
{
ipt_lock(ipt);
if (ipt->ipt_waiting) {
thread_wakeup((int)&ipt->ipt_acts);
ipt->ipt_waiting = 0;
}
ipt_unlock(ipt);
}
#endif