#include "priv.h"
#include <stdlib.h>
#include <pthread.h>
#include <hurd/ports.h>
static int thread_timeout = 1000 * 60 * 2;
static int server_timeout = 1000 * 60 * 10;
static void *
master_thread_function (void *demuxer)
{
error_t err;
pthread_setname_np (pthread_self (), "diskfs");
do
{
ports_manage_port_operations_multithread (diskfs_port_bucket,
(ports_demuxer_type) demuxer,
thread_timeout,
server_timeout,
0);
err = diskfs_shutdown (0);
}
while (err);
exit (0);
return NULL;
}
void
diskfs_spawn_first_thread (ports_demuxer_type demuxer)
{
pthread_t thread;
error_t err;
err = pthread_create (&thread, NULL, master_thread_function, demuxer);
if (!err)
pthread_detach (thread);
else
{
errno = err;
perror ("pthread_create");
}
}