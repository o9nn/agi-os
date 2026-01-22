#include <string.h>
#include <fcntl.h>
#include "nfsd.h"
#include "../nfs/mount.h"
#define malloc spoogie_woogie
#include <rpc/types.h>
#include <rpc/xdr.h>
#undef TRUE
#undef FALSE
#include <rpc/pmap_prot.h>
#include <rpc/auth.h>
#include <rpc/rpc_msg.h>
#undef malloc
void *
server_loop (void *arg)
{
int fd = (int) arg;
char buf[MAXIOSIZE];
int xid;
int *p, *r;
char *rbuf;
struct cached_reply *cr;
int program;
struct sockaddr_in sender;
int version;
int procedure;
struct proctable *table = 0;
struct procedure *proc;
struct idspec *cred;
struct cache_handle *c, fakec;
error_t err;
socklen_t addrlen;
int cc;
pthread_setname_np (pthread_self (), "server_loop");
memset (&fakec, 0, sizeof (struct cache_handle));
for (;;)
{
p = (int *) buf;
proc = 0;
addrlen = sizeof (struct sockaddr_in);
cc = recvfrom (fd, buf, MAXIOSIZE, 0, &sender, &addrlen);
if (cc == -1)
continue;
xid = *(p++);
if (ntohl (*p) != CALL)
continue;
p++;
cr = check_cached_replies (xid, &sender);
if (cr->data)
goto repost_reply;
r = (int *) (rbuf = malloc (MAXIOSIZE));
if (ntohl (*p) != RPC_MSG_VERSION)
{
*(r++) = xid;
*(r++) = htonl (REPLY);
*(r++) = htonl (MSG_DENIED);
*(r++) = htonl (RPC_MISMATCH);
*(r++) = htonl (RPC_MSG_VERSION);
*(r++) = htonl (RPC_MSG_VERSION);
goto send_reply;
}
p++;
program = ntohl (*p);
p++;
switch (program)
{
case MOUNTPROG:
version = MOUNTVERS;
table = &mounttable;
break;
case NFS_PROGRAM:
version = NFS_VERSION;
table = &nfs2table;
break;
case PMAPPROG:
version = PMAPVERS;
table = &pmaptable;
break;
default:
*(r++) = xid;
*(r++) = htonl (REPLY);
*(r++) = htonl (MSG_ACCEPTED);
*(r++) = htonl (AUTH_NULL);
*(r++) = htonl (0);
*(r++) = htonl (PROG_UNAVAIL);
goto send_reply;
}
if (ntohl (*p) != version)
{
*(r++) = xid;
*(r++) = htonl (REPLY);
*(r++) = htonl (MSG_ACCEPTED);
*(r++) = htonl (AUTH_NULL);
*(r++) = htonl (0);
*(r++) = htonl (PROG_MISMATCH);
*(r++) = htonl (version);
*(r++) = htonl (version);
goto send_reply;
}
p++;
procedure = htonl (*p);
p++;
if (procedure < table->min
|| procedure > table->max
|| table->procs[procedure - table->min].func == 0)
{
*(r++) = xid;
*(r++) = htonl (REPLY);
*(r++) = htonl (MSG_ACCEPTED);
*(r++) = htonl (AUTH_NULL);
*(r++) = htonl (0);
*(r++) = htonl (PROC_UNAVAIL);
*(r++) = htonl (table->min);
*(r++) = htonl (table->max);
goto send_reply;
}
proc = &table->procs[procedure - table->min];
p = process_cred (p, &cred);
if (proc->need_handle)
p = lookup_cache_handle (p, &c, cred);
else
{
fakec.ids = cred;
c = &fakec;
}
if (proc->alloc_reply)
{
size_t amt;
amt = (*proc->alloc_reply) (p, version) + 256;
if (amt > MAXIOSIZE)
{
free (rbuf);
r = (int *) (rbuf = malloc (amt));
}
}
*(r++) = xid;
*(r++) = htonl (REPLY);
*(r++) = htonl (MSG_ACCEPTED);
*(r++) = htonl (AUTH_NULL);
*(r++) = htonl (0);
*(r++) = htonl (SUCCESS);
if (!proc->process_error)
(void) (*proc->func) (c, p, &r, version);
else
{
if (c)
{
int *errloc = r;
*(r++) = htonl (0);
err = (*proc->func) (c, p, &r, version);
if (err)
{
r = errloc;
*(r++) = htonl (nfs_error_trans (err, version));
}
}
else
*(r++) = htonl (nfs_error_trans (ESTALE, version));
}
cred_rele (cred);
if (c && c != &fakec)
cache_handle_rele (c);
send_reply:
cr->data = rbuf;
cr->len = (char *)r - rbuf;
repost_reply:
sendto (fd, cr->data, cr->len, 0,
(struct sockaddr *) &sender, addrlen);
release_cached_reply (cr);
}
}