#include "pfinet.h"
#include <pthread.h>
#include <asm/system.h>
#include <linux/sched.h>
#include <linux/interrupt.h>
pthread_mutex_t global_lock = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t net_bh_lock = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t net_bh_wakeup = PTHREAD_COND_INITIALIZER;
int net_bh_raised = 0;
struct task_struct current_contents;
int
sock_wake_async (struct socket *sock, int how)
{
return 0;
}
void *
net_bh_worker (void *arg)
{
pthread_setname_np (pthread_self (), "net_bh");
pthread_mutex_lock (&net_bh_lock);
while (1)
{
while (!net_bh_raised)
pthread_cond_wait (&net_bh_wakeup, &net_bh_lock);
net_bh_raised = 0;
pthread_mutex_lock (&global_lock);
net_bh ();
pthread_mutex_unlock (&global_lock);
}
return 0;
}