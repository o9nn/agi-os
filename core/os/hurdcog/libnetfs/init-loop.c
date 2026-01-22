#include "netfs.h"
#include <pthread.h>
static int thread_timeout = 1000 * 60 * 2;
static int server_timeout = 1000 * 60 * 10;
void
netfs_server_loop (void)
{
error_t err;
pthread_setname_np (pthread_self (), "netfs");
do
{
ports_manage_port_operations_multithread (netfs_port_bucket,
netfs_demuxer,
thread_timeout,
server_timeout,
0);
err = netfs_shutdown (0);
}
while (err);
exit (0);
}