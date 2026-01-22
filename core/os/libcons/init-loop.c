#include <hurd/ports.h>
#include <pthread.h>
#include "cons.h"
void
cons_server_loop (void)
{
pthread_setname_np (pthread_self (), "cons_demuxer");
ports_manage_port_operations_one_thread (cons_port_bucket,
cons_demuxer, 0);
}