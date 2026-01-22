#include "ports.h"
#include "interrupt_S.h"
kern_return_t
ports_S_interrupt_operation (struct port_info *pi,
mach_port_seqno_t seqno)
{
mach_port_seqno_t old;
if (!pi)
return EOPNOTSUPP;
retry:
old = __atomic_load_n (&pi->cancel_threshold, __ATOMIC_SEQ_CST);
if (old < seqno
&& ! __atomic_compare_exchange_n (&pi->cancel_threshold, &old, seqno,
0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST))
goto retry;
ports_interrupt_rpcs (pi);
return 0;
}