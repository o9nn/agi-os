#include "ports.h"
void
ports_manage_port_operations_one_thread (struct port_bucket *bucket,
ports_demuxer_type demuxer,
int timeout)
{
struct ports_thread thread;
error_t err;
int
internal_demuxer (mach_msg_header_t *inp,
mach_msg_header_t *outheadp)
{
struct port_info *pi;
struct rpc_info link;
int status;
error_t err;
mig_reply_header_t *outp = (mig_reply_header_t *) outheadp;
static const mach_msg_type_t RetCodeType = {
.msgt_name = MACH_MSG_TYPE_INTEGER_32,
.msgt_size = 32,
.msgt_number = 1,
.msgt_inline = TRUE,
.msgt_longform = FALSE,
.msgt_deallocate = FALSE,
.msgt_unused = 0
};
outp->Head.msgh_bits
= MACH_MSGH_BITS(MACH_MSGH_BITS_REMOTE(inp->msgh_bits), 0);
outp->Head.msgh_size = sizeof *outp;
outp->Head.msgh_remote_port = inp->msgh_remote_port;
outp->Head.msgh_local_port = MACH_PORT_NULL;
outp->Head.msgh_seqno = 0;
outp->Head.msgh_id = inp->msgh_id + 100;
outp->RetCodeType = RetCodeType;
outp->RetCode = MIG_BAD_ID;
if (MACH_MSGH_BITS_LOCAL (inp->msgh_bits) ==
MACH_MSG_TYPE_PROTECTED_PAYLOAD)
pi = ports_lookup_payload (bucket, inp->msgh_protected_payload, NULL);
else
{
pi = ports_lookup_port (bucket, inp->msgh_local_port, 0);
if (pi)
{
inp->msgh_bits =
MACH_MSGH_BITS_OTHER (inp->msgh_bits)
| MACH_MSGH_BITS (MACH_MSGH_BITS_REMOTE (inp->msgh_bits),
MACH_MSG_TYPE_PROTECTED_PAYLOAD);
inp->msgh_protected_payload = (unsigned long) pi;
}
}
if (pi)
{
err = ports_begin_rpc (pi, inp->msgh_id, &link);
if (err)
{
mach_port_deallocate (mach_task_self (), inp->msgh_remote_port);
outp->RetCode = err;
status = 1;
}
else
{
status = demuxer (inp, outheadp);
ports_end_rpc (pi, &link);
}
ports_port_deref (pi);
}
else
{
outp->RetCode = EOPNOTSUPP;
status = 1;
}
_ports_thread_quiescent (&bucket->threadpool, &thread);
return status;
}
timeout = 0;
_ports_thread_online (&bucket->threadpool, &thread);
do
err = mach_msg_server_timeout (internal_demuxer, 0, bucket->portset,
timeout ? MACH_RCV_TIMEOUT : 0, timeout);
while (err != MACH_RCV_TIMED_OUT);
_ports_thread_offline (&bucket->threadpool, &thread);
}