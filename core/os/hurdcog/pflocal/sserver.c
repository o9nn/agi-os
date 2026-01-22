#include <pthread.h>
#include <stdio.h>
#include <hurd/ports.h>
struct port_bucket *sock_port_bucket;
static int sock_server_active = 0;
static pthread_spinlock_t sock_server_active_lock = PTHREAD_SPINLOCK_INITIALIZER;
#include "io_S.h"
#include "fs_S.h"
#include "socket_S.h"
#include "../libports/interrupt_S.h"
#include "../libports/notify_S.h"
static int
sock_demuxer (mach_msg_header_t *inp, mach_msg_header_t *outp)
{
mig_routine_t routine;
if ((routine = io_server_routine (inp)) ||
(routine = fs_server_routine (inp)) ||
(routine = socket_server_routine (inp)) ||
(routine = ports_interrupt_server_routine (inp)) ||
(routine = ports_notify_server_routine (inp)))
{
(*routine) (inp, outp);
return TRUE;
}
else
return FALSE;
}
static void *
handle_sock_requests (void *unused)
{
pthread_setname_np (pthread_self (), "sock_requests");
while (ports_count_bucket (sock_port_bucket) > 0)
{
ports_enable_bucket (sock_port_bucket);
ports_manage_port_operations_multithread (sock_port_bucket, sock_demuxer,
30*1000, 2*60*1000, 0);
}
pthread_spin_lock (&sock_server_active_lock);
sock_server_active = 0;
pthread_spin_unlock (&sock_server_active_lock);
ports_enable_bucket (sock_port_bucket);
return NULL;
}
void
ensure_sock_server (void)
{
pthread_t thread;
error_t err;
pthread_spin_lock (&sock_server_active_lock);
if (sock_server_active)
pthread_spin_unlock (&sock_server_active_lock);
else
{
sock_server_active = 1;
pthread_spin_unlock (&sock_server_active_lock);
err = pthread_create (&thread, NULL, handle_sock_requests, NULL);
if (!err)
pthread_detach (thread);
else
{
errno = err;
perror ("pthread_create");
}
}
}