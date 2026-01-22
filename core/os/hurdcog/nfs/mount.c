#include <rpc/types.h>
#include <rpc/xdr.h>
#include <rpc/pmap_prot.h>
#undef TRUE
#undef FALSE
#include <errno.h>
#include <error.h>
#include <sys/socket.h>
#include <netdb.h>
#include <string.h>
#include <netinet/in.h>
#include <stdio.h>
#include "nfs.h"
#include "mount.h"
char *pmap_service_name = "sunrpc";
short pmap_service_number = PMAPPORT;
int mount_program = MOUNTPROG;
int mount_version = MOUNTVERS;
short mount_port = 0;
int mount_port_override = 0;
int nfs_program = NFS_PROGRAM;
int nfs_version = NFS_VERSION;
short nfs_port = NFS_PORT;
int nfs_port_override = 0;
const char *mounted_hostname;
uint16_t mounted_nfs_port;
int protocol_version = 2;
static int *
pmap_initialize_rpc (int procnum, void **buf)
{
return initialize_rpc (PMAPPROG, PMAPVERS, procnum, 0, buf, 0, 0, -1);
}
static int *
mount_initialize_rpc (int procnum, void **buf)
{
return initialize_rpc (MOUNTPROG, MOUNTVERS, procnum, 0, buf, 0, 0, -1);
}
struct node *
mount_root (char *name, char *host)
{
struct sockaddr_in addr;
struct hostent *h;
int *p;
void *rpcbuf;
int port;
error_t err;
struct fhandle mount_fhandle;
struct node *np;
short pmapport;
if (pmap_service_name)
{
struct servent *s;
s = getservbyname (pmap_service_name, "udp");
if (s)
pmapport = s->s_port;
else
pmapport = htons (pmap_service_number);
}
else
pmapport = htons (pmap_service_number);
h = gethostbyname (host);
if (!h)
{
herror (host);
return 0;
}
addr.sin_family = h->h_addrtype;
memcpy (&addr.sin_addr, h->h_addr_list[0], h->h_length);
addr.sin_port = pmapport;
if (mount_port_override)
addr.sin_port = htons (mount_port);
else
{
if (connect (main_udp_socket, (struct sockaddr *)&addr,
sizeof (struct sockaddr_in)) == -1)
{
error (0, errno, "server mount program");
return 0;
}
p = pmap_initialize_rpc (PMAPPROC_GETPORT, &rpcbuf);
if (! p)
{
error (0, errno, "creating rpc packet");
return 0;
}
*(p++) = htonl (MOUNTPROG);
*(p++) = htonl (MOUNTVERS);
*(p++) = htonl (IPPROTO_UDP);
*(p++) = htonl (0);
err = conduct_rpc (&rpcbuf, &p);
if (!err)
{
port = ntohl (*p);
p++;
addr.sin_port = htons (port);
}
else if (mount_port)
addr.sin_port = htons (mount_port);
else
{
error (0, err, "portmap of mount");
goto error_with_rpcbuf;
}
free (rpcbuf);
}
if (connect (main_udp_socket, (struct sockaddr *) &addr,
sizeof (struct sockaddr_in)) == -1)
{
error (0, errno, "connect");
goto error_with_rpcbuf;
}
p = mount_initialize_rpc (MOUNTPROC_MNT, &rpcbuf);
if (! p)
{
error (0, errno, "rpc");
goto error_with_rpcbuf;
}
p = xdr_encode_string (p, name);
err = conduct_rpc (&rpcbuf, &p);
if (err)
{
error (0, err, "%s", name);
goto error_with_rpcbuf;
}
err = nfs_error_trans (htonl (*p));
p++;
if (err)
{
error (0, err, "%s", name);
goto error_with_rpcbuf;
}
mount_fhandle.size = NFS2_FHSIZE;
memcpy(&mount_fhandle.data, p, mount_fhandle.size);
free (rpcbuf);
if (nfs_port_override)
port = nfs_port;
else
{
addr.sin_port = pmapport;
if (connect (main_udp_socket, (struct sockaddr *) &addr,
sizeof (struct sockaddr_in)) == -1)
{
error (0, errno, "connect");
return 0;
}
p = pmap_initialize_rpc (PMAPPROC_GETPORT, &rpcbuf);
if (! p)
{
error (0, errno, "rpc");
goto error_with_rpcbuf;
}
*(p++) = htonl (nfs_program);
*(p++) = htonl (nfs_version);
*(p++) = htonl (IPPROTO_UDP);
*(p++) = htonl (0);
err = conduct_rpc (&rpcbuf, &p);
if (!err)
{
port = ntohl (*p);
p++;
}
else if (nfs_port)
port = nfs_port;
else
{
error (0, err, "portmap of nfs server");
goto error_with_rpcbuf;
}
free (rpcbuf);
}
addr.sin_port = htons (port);
if (connect (main_udp_socket, (struct sockaddr *) &addr,
sizeof (struct sockaddr_in)) == -1)
{
error (0, errno, "connect");
return 0;
}
mounted_hostname = host;
mounted_nfs_port = port;
p = initialize_rpc(nfs_program,
nfs_version,
NFSPROC_LOOKUP (protocol_version),
0, &rpcbuf,
0, 0, -1);
if (! p)
{
error (0, errno, "rpc");
goto error_with_rpcbuf;
}
p = xdr_encode_fhandle(p, &mount_fhandle);
p = xdr_encode_string (p, ".");
err = conduct_rpc (&rpcbuf, &p);
if (!err) {
err = nfs_error_trans (ntohl (*p));
p++;
}
else
{
error (0, errno, "rpc");
goto error_with_rpcbuf;
}
if (!err)
{
xdr_decode_fhandle (p, &np);
pthread_mutex_unlock (&np->lock);
free(rpcbuf);
return np;
}
error_with_rpcbuf:
free (rpcbuf);
return 0;
}