#include "nfs.h"
#undef TRUE
#undef FALSE
#define malloc spoiufasdf
#include <rpc/types.h>
#include <rpc/xdr.h>
#include <rpc/auth.h>
#include <rpc/rpc_msg.h>
#include <rpc/auth_unix.h>
#undef malloc
#include <netinet/in.h>
#include <assert-backtrace.h>
#include <errno.h>
#include <error.h>
#include <unistd.h>
#include <stdio.h>
#include <pthread.h>
struct rpc_list
{
struct rpc_list *next, **prevp;
void *reply;
};
static struct rpc_list *outstanding_rpcs;
static pthread_cond_t rpc_wakeup = PTHREAD_COND_INITIALIZER;
static pthread_mutex_t outstanding_lock = PTHREAD_MUTEX_INITIALIZER;
static inline int
generate_xid (void)
{
static int nextxid;
if (nextxid == 0)
nextxid = mapped_time->seconds;
return nextxid++;
}
int *
initialize_rpc (int program, int version, int rpc_proc,
size_t len, void **bufp,
uid_t uid, gid_t gid, gid_t second_gid)
{
void *buf;
int *p, *lenaddr;
struct rpc_list *hdr;
buf = malloc (len + 1024);
if (! buf)
{
errno = ENOMEM;
return NULL;
}
hdr = buf;
hdr->reply = 0;
p = buf + sizeof (struct rpc_list);
*(p++) = htonl (generate_xid ());
*(p++) = htonl (CALL);
*(p++) = htonl (RPC_MSG_VERSION);
*(p++) = htonl (program);
*(p++) = htonl (version);
*(p++) = htonl (rpc_proc);
assert_backtrace ((uid == -1) == (gid == -1));
if (uid == -1)
{
*(p++) = htonl (AUTH_NONE);
*(p++) = 0;
}
else
{
*(p++) = htonl (AUTH_UNIX);
lenaddr = p++;
*(p++) = htonl (mapped_time->seconds);
p = xdr_encode_string (p, hostname);
*(p++) = htonl (uid);
*(p++) = htonl (gid);
if (second_gid == -1)
*(p++) = 0;
else
{
*(p++) = htonl (1);
*(p++) = htonl (second_gid);
}
*lenaddr = htonl ((p - (lenaddr + 1)) * sizeof (int));
}
*(p++) = htonl (AUTH_NONE);
*(p++) = 0;
*bufp = buf;
return p;
}
static inline void
unlink_rpc (struct rpc_list *hdr)
{
*hdr->prevp = hdr->next;
if (hdr->next)
hdr->next->prevp = hdr->prevp;
}
static inline void
link_rpc (struct rpc_list **list, struct rpc_list *hdr)
{
hdr->next = *list;
if (hdr->next)
hdr->next->prevp = &hdr->next;
hdr->prevp = list;
*list = hdr;
}
error_t
conduct_rpc (void **rpcbuf, int **pp)
{
struct rpc_list *hdr = *rpcbuf;
error_t err;
size_t cc, nc;
int timeout = initial_transmit_timeout;
time_t lasttrans;
int ntransmit = 0;
int *p;
int xid;
int n;
int cancel;
pthread_mutex_lock (&outstanding_lock);
link_rpc (&outstanding_rpcs, hdr);
xid = * (int *) (*rpcbuf + sizeof (struct rpc_list));
do
{
if (mounted_soft && ntransmit == soft_retries)
{
unlink_rpc (hdr);
pthread_mutex_unlock (&outstanding_lock);
return ETIMEDOUT;
}
lasttrans = mapped_time->seconds;
ntransmit++;
nc = (void *) *pp - *rpcbuf - sizeof (struct rpc_list);
cc = write (main_udp_socket, *rpcbuf + sizeof (struct rpc_list), nc);
if (cc == -1)
{
unlink_rpc (hdr);
pthread_mutex_unlock (&outstanding_lock);
return errno;
}
else
assert_backtrace (cc == nc);
cancel = 0;
while (!hdr->reply
&& (mapped_time->seconds - lasttrans < timeout)
&& !cancel)
cancel = pthread_hurd_cond_wait_np (&rpc_wakeup, &outstanding_lock);
if (cancel)
{
unlink_rpc (hdr);
pthread_mutex_unlock (&outstanding_lock);
return EINTR;
}
if (!hdr->reply)
{
timeout *= 2;
if (timeout > max_transmit_timeout)
timeout = max_transmit_timeout;
}
}
while (!hdr->reply);
pthread_mutex_unlock (&outstanding_lock);
*rpcbuf = hdr->reply;
free (hdr);
p = (int *) *rpcbuf;
assert_backtrace (*p == xid);
p++;
switch (ntohl (*p))
{
default:
err = EBADRPC;
break;
case REPLY:
p++;
switch (ntohl (*p))
{
default:
err = EBADRPC;
break;
case MSG_DENIED:
p++;
switch (ntohl (*p))
{
default:
err = EBADRPC;
break;
case RPC_MISMATCH:
err = ERPCMISMATCH;
break;
case AUTH_ERROR:
p++;
switch (ntohl (*p))
{
case AUTH_BADCRED:
case AUTH_REJECTEDCRED:
err = EAUTH;
break;
case AUTH_TOOWEAK:
err = ENEEDAUTH;
break;
case AUTH_BADVERF:
case AUTH_REJECTEDVERF:
default:
err = EBADRPC;
break;
}
break;
}
break;
case MSG_ACCEPTED:
p++;
p++;
n = ntohl (*p);
p++;
p += INTSIZE (n);
switch (ntohl (*p))
{
default:
case GARBAGE_ARGS:
err = EBADRPC;
break;
case PROG_UNAVAIL:
err = EPROGUNAVAIL;
break;
case PROG_MISMATCH:
err = EPROGMISMATCH;
break;
case PROC_UNAVAIL:
err = EPROCUNAVAIL;
break;
case SUCCESS:
p++;
*pp = p;
err = 0;
break;
}
break;
}
break;
}
return err;
}
void *
timeout_service_thread (void *arg)
{
(void) arg;
pthread_setname_np (pthread_self (), "timeout");
while (1)
{
sleep (1);
pthread_mutex_lock (&outstanding_lock);
pthread_cond_broadcast (&rpc_wakeup);
pthread_mutex_unlock (&outstanding_lock);
}
return NULL;
}
void *
rpc_receive_thread (void *arg)
{
void *buf;
(void) arg;
pthread_setname_np (pthread_self (), "rpc_receive");
buf = malloc (1024 + read_size);
assert_backtrace (buf);
while (1)
{
int cc = read (main_udp_socket, buf, 1024 + read_size);
if (cc == -1)
{
error (0, errno, "nfs read");
continue;
}
else
{
struct rpc_list *r;
int xid = *(int *)buf;
pthread_mutex_lock (&outstanding_lock);
for (r = outstanding_rpcs; r; r = r->next)
{
if (* (int *) &r[1] == xid)
{
unlink_rpc (r);
r->reply = buf;
pthread_cond_broadcast (&rpc_wakeup);
break;
}
}
#if 0
if (! r)
fprintf (stderr, "NFS dropping reply xid %d\n", xid);
#endif
pthread_mutex_unlock (&outstanding_lock);
if (r)
{
buf = malloc (1024 + read_size);
assert_backtrace (buf);
}
}
}
return NULL;
}