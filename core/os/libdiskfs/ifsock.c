#include "priv.h"
#include "ifsock_S.h"
#include <hurd/paths.h>
#include <sys/socket.h>
#include <stdio.h>
#include <hurd/socket.h>
static pthread_spinlock_t pflocalserverlock = PTHREAD_SPINLOCK_INITIALIZER;
static mach_port_t pflocalserver = MACH_PORT_NULL;
kern_return_t
diskfs_S_ifsock_getsockaddr (struct protid *cred,
mach_port_t *address)
{
error_t err;
struct node *np;
unsigned restart_tries = 0;
if (!cred)
return EOPNOTSUPP;
np = cred->po->np;
retry:
pthread_mutex_lock (&np->lock);
if ((np->dn_stat.st_mode & S_IFMT) != S_IFSOCK)
{
pthread_mutex_unlock (&np->lock);
return EOPNOTSUPP;
}
err = fshelp_access (&np->dn_stat, S_IWRITE, cred->user);
if (err)
{
pthread_mutex_unlock (&np->lock);
return err;
}
if (np->sockaddr == MACH_PORT_NULL)
{
mach_port_t server;
mach_port_t sockaddr;
pthread_mutex_unlock (&np->lock);
pthread_spin_lock (&pflocalserverlock);
if (pflocalserver == MACH_PORT_NULL)
{
char buf[100];
pthread_spin_unlock (&pflocalserverlock);
sprintf (buf, "%s/%d", _SERVERS_SOCKET, PF_LOCAL);
server = file_name_lookup (buf, 0, 0);
if (server == MACH_PORT_NULL)
return EIEIO;
pthread_spin_lock (&pflocalserverlock);
if (pflocalserver != MACH_PORT_NULL)
mach_port_deallocate (mach_task_self (), server);
else
pflocalserver = server;
pthread_spin_unlock (&pflocalserverlock);
goto retry;
}
server = pflocalserver;
pthread_spin_unlock (&pflocalserverlock);
err = socket_fabricate_address (server, AF_LOCAL, &sockaddr);
if ((err == MACH_SEND_INVALID_DEST || err == MIG_SERVER_DIED)
&& restart_tries++ == 0)
{
pthread_spin_lock (&pflocalserverlock);
if (pflocalserver == server)
pflocalserver = MACH_PORT_NULL;
pthread_spin_unlock (&pflocalserverlock);
goto retry;
}
if (err)
return EIEIO;
pthread_mutex_lock (&np->lock);
if (np->sockaddr != MACH_PORT_NULL)
mach_port_deallocate (mach_task_self (), sockaddr);
else
{
ports_request_dead_name_notification (cred, sockaddr, NULL);
np->sockaddr = sockaddr;
diskfs_nref_light (np);
}
}
*address = np->sockaddr;
pthread_mutex_unlock (&np->lock);
return 0;
}