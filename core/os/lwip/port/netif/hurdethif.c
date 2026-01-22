#include <netif/hurdethif.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <pthread.h>
#include <error.h>
#include <device/device.h>
#include <device/net_status.h>
#include <net/if.h>
#include <net/if_arp.h>
#include <lwip/opt.h>
#include <lwip/def.h>
#include <lwip/mem.h>
#include <lwip/pbuf.h>
#include <lwip/stats.h>
#include <lwip/snmp.h>
#include <lwip/ethip6.h>
#include <lwip/etharp.h>
#include <lwip/sockets.h>
#include <lwip/inet.h>
#define GET_HWADDR_BYTE(x,n) (((char*)x)[n])
static short ether_filter[] = {
#ifdef NETF_IN
NETF_IN,
#endif
NETF_PUSHLIT | NETF_NOP,
1
};
static int ether_filter_len = sizeof (ether_filter) / sizeof (short);
static struct bpf_insn bpf_ether_filter[] = {
{NETF_IN | NETF_BPF, 0, 0, 0},
{BPF_LD | BPF_H | BPF_ABS, 0, 0, 12},
{BPF_JMP | BPF_JEQ | BPF_K, 2, 0, 0x0806},
{BPF_JMP | BPF_JEQ | BPF_K, 1, 0, 0x0800},
{BPF_JMP | BPF_JEQ | BPF_K, 0, 1, 0x86DD},
{BPF_RET | BPF_K, 0, 0, TCP_MSS + 0x28 + PBUF_LINK_HLEN},
{BPF_RET | BPF_K, 0, 0, 0},
};
static int bpf_ether_filter_len = sizeof (bpf_ether_filter) / (sizeof (short));
struct port_bucket *etherport_bucket;
struct port_class *etherread_class;
static pthread_t input_thread;
static error_t
hurdethif_device_get_flags (struct netif *netif, uint16_t * flags)
{
error_t err = 0;
mach_msg_type_number_t count;
struct net_status status;
hurdethif *ethif;
memset (&status, 0, sizeof (struct net_status));
ethif = netif_get_state (netif);
count = NET_STATUS_COUNT;
err = device_get_status (ethif->ether_port,
NET_STATUS, (dev_status_t) & status, &count);
if (err == D_INVALID_OPERATION)
{
error (0, 0, "%s: hardware doesn't support getting flags.\n",
ethif->devname);
err = 0;
}
else if (err)
error (0, err, "%s: Cannot get hardware flags", ethif->devname);
else
*flags = status.flags;
return err;
}
static error_t
hurdethif_device_set_flags (struct netif *netif, uint16_t flags)
{
error_t err = 0;
hurdethif *ethif;
int sflags;
sflags = flags;
ethif = netif_get_state (netif);
if (ethif->ether_port == MACH_PORT_NULL)
return 0;
err = device_set_status (ethif->ether_port, NET_FLAGS, &sflags, 1);
if (err == D_INVALID_OPERATION)
{
error (0, 0, "%s: hardware doesn't support setting flags.\n",
ethif->devname);
err = 0;
}
else if (err)
error (0, err, "%s: Cannot set hardware flags", ethif->devname);
else
ethif->flags = flags;
return err;
}
static error_t
hurdethif_device_open (struct netif *netif)
{
error_t err = ERR_OK;
device_t master_device;
hurdethif *ethif = netif_get_state (netif);
if (ethif->ether_port != MACH_PORT_NULL)
{
error (0, 0, "Already opened: %s", ethif->devname);
return -1;
}
err = ports_create_port (etherread_class, etherport_bucket,
sizeof (struct port_info), &ethif->readpt);
if (err)
{
error (0, err, "ports_create_port on %s", ethif->devname);
}
else
{
ethif->readptname = ports_get_right (ethif->readpt);
mach_port_insert_right (mach_task_self (), ethif->readptname,
ethif->readptname, MACH_MSG_TYPE_MAKE_SEND);
mach_port_set_qlimit (mach_task_self (), ethif->readptname,
MACH_PORT_QLIMIT_MAX);
master_device = file_name_lookup (ethif->devname, O_RDWR, 0);
if (master_device != MACH_PORT_NULL)
{
err = device_open (master_device, D_WRITE | D_READ,
"eth", &ethif->ether_port);
mach_port_deallocate (mach_task_self (), master_device);
if (err)
error (0, err, "device_open on %s", ethif->devname);
else
{
err = device_set_filter (ethif->ether_port, ethif->readptname,
MACH_MSG_TYPE_MAKE_SEND, 0,
(filter_array_t) bpf_ether_filter,
bpf_ether_filter_len);
if (err)
error (0, err, "device_set_filter on %s", ethif->devname);
}
}
else
{
int file_errno = errno;
err = get_privileged_ports (0, &master_device);
if (err)
{
error (0, file_errno, "file_name_lookup %s", ethif->devname);
error (0, err, "and cannot get device master port");
}
else
{
err = device_open (master_device, D_WRITE | D_READ,
ethif->devname, &ethif->ether_port);
mach_port_deallocate (mach_task_self (), master_device);
if (err)
{
error (0, file_errno, "file_name_lookup %s",
ethif->devname);
error (0, err, "device_open(%s)", ethif->devname);
}
else
{
err =
device_set_filter (ethif->ether_port, ethif->readptname,
MACH_MSG_TYPE_MAKE_SEND, 0,
(filter_array_t) ether_filter,
ether_filter_len);
if (err)
error (0, err, "device_set_filter on %s", ethif->devname);
}
}
}
}
return err;
}
static error_t
hurdethif_device_close (struct netif *netif)
{
hurdethif *ethif = netif_get_state (netif);
if (ethif->ether_port == MACH_PORT_NULL)
{
error (0, 0, "Already closed: %s", ethif->devname);
return -1;
}
mach_port_deallocate (mach_task_self (), ethif->readptname);
ethif->readptname = MACH_PORT_NULL;
ports_destroy_right (ethif->readpt);
ethif->readpt = NULL;
device_close (ethif->ether_port);
mach_port_deallocate (mach_task_self (), ethif->ether_port);
ethif->ether_port = MACH_PORT_NULL;
return ERR_OK;
}
static err_t
hurdethif_output (struct netif *netif, struct pbuf *p)
{
error_t err;
hurdethif *ethif = netif_get_state (netif);
int count;
uint8_t tried;
if (p->tot_len != p->len)
return ERR_OK;
tried = 0;
do
{
tried++;
err = device_write (ethif->ether_port, D_NOWAIT, 0,
p->payload, p->len, &count);
if (err)
{
if (tried == 2)
break;
if (err == EMACH_SEND_INVALID_DEST || err == EMIG_SERVER_DIED)
{
hurdethif_device_close (netif);
hurdethif_device_open (netif);
}
}
else if (count != p->len)
err = -1;
}
while (err);
return ERR_OK;
}
void
hurdethif_input (struct netif *netif, struct net_rcv_msg *msg)
{
struct pbuf *p, *q;
uint16_t len;
uint16_t off;
uint16_t next_read;
len = PBUF_LINK_HLEN
+ msg->packet_type.msgt_number - sizeof (struct packet_header);
p = pbuf_alloc (PBUF_RAW, len, PBUF_POOL);
if (p)
{
q = p;
off = 0;
do
{
if (off < PBUF_LINK_HLEN)
{
next_read = (off + q->len) > PBUF_LINK_HLEN ?
(PBUF_LINK_HLEN - off) : q->len;
memcpy (q->payload, msg->header + off, next_read);
if ((off + q->len) > PBUF_LINK_HLEN)
memcpy (q->payload + PBUF_LINK_HLEN,
msg->packet + sizeof (struct packet_header),
q->len - next_read);
}
else
memcpy (q->payload, msg->packet +
sizeof (struct packet_header) + off - PBUF_LINK_HLEN,
q->len);
off += q->len;
if (q->tot_len == q->len)
break;
else
q = q->next;
}
while (1);
if (netif->input (p, netif) != ERR_OK)
{
LWIP_DEBUGF (NETIF_DEBUG, ("hurdethif_input: IP input error\n"));
pbuf_free (p);
p = NULL;
}
}
}
int
hurdethif_demuxer (mach_msg_header_t * inp, mach_msg_header_t * outp)
{
struct net_rcv_msg *msg = (struct net_rcv_msg *) inp;
struct netif *netif;
mach_port_t local_port;
if (inp->msgh_id != NET_RCV_MSG_ID)
return 0;
if (MACH_MSGH_BITS_LOCAL (inp->msgh_bits) ==
MACH_MSG_TYPE_PROTECTED_PAYLOAD)
{
struct port_info *pi = ports_lookup_payload (NULL,
inp->msgh_protected_payload,
NULL);
if (pi)
{
local_port = pi->port_right;
ports_port_deref (pi);
}
else
local_port = MACH_PORT_NULL;
}
else
local_port = inp->msgh_local_port;
NETIF_FOREACH(netif)
if (local_port == netif_get_state (netif)->readptname)
break;
if (!netif)
{
if (inp->msgh_remote_port != MACH_PORT_NULL)
mach_port_deallocate (mach_task_self (), inp->msgh_remote_port);
return 1;
}
hurdethif_input (netif, msg);
return 1;
}
static error_t
hurdethif_device_update_mtu (struct netif *netif, uint32_t mtu)
{
error_t err = 0;
netif->mtu = mtu;
bpf_ether_filter[5].k = mtu + PBUF_LINK_HLEN;
return err;
}
static error_t
hurdethif_device_terminate (struct netif *netif)
{
free (netif_get_state (netif)->devname);
free (netif_get_state (netif));
return 0;
}
err_t
hurdethif_device_init (struct netif *netif)
{
error_t err;
mach_msg_type_number_t count = 2;
int net_address[2];
device_t ether_port;
hurdethif *ethif;
ethif = calloc (1, sizeof (hurdethif));
if (!ethif)
{
LWIP_DEBUGF (NETIF_DEBUG, ("hurdethif_init: out of memory\n"));
return ERR_MEM;
}
memcpy (ethif, netif_get_state (netif), sizeof (struct ifcommon));
netif->state = ethif;
ethif->type = ARPHRD_ETHER;
netif->output = etharp_output;
netif->output_ip6 = ethip6_output;
netif->linkoutput = hurdethif_output;
ethif->open = hurdethif_device_open;
ethif->close = hurdethif_device_close;
ethif->terminate = hurdethif_device_terminate;
ethif->update_mtu = hurdethif_device_update_mtu;
ethif->change_flags = hurdethif_device_set_flags;
err = hurdethif_device_open (netif);
if (err)
return ERR_IF;
ether_port = netif_get_state (netif)->ether_port;
err = device_get_status (ether_port, NET_ADDRESS, net_address, &count);
if (err)
error (0, err, "%s: Cannot get hardware Ethernet address",
netif_get_state (netif)->devname);
else if (count * sizeof (int) >= ETHARP_HWADDR_LEN)
{
net_address[0] = ntohl (net_address[0]);
net_address[1] = ntohl (net_address[1]);
netif->hwaddr_len = ETHARP_HWADDR_LEN;
netif->hwaddr[0] = GET_HWADDR_BYTE (net_address, 0);
netif->hwaddr[1] = GET_HWADDR_BYTE (net_address, 1);
netif->hwaddr[2] = GET_HWADDR_BYTE (net_address, 2);
netif->hwaddr[3] = GET_HWADDR_BYTE (net_address, 3);
netif->hwaddr[4] = GET_HWADDR_BYTE (net_address, 4);
netif->hwaddr[5] = GET_HWADDR_BYTE (net_address, 5);
}
else
error (0, 0, "%s: Invalid Ethernet address",
netif_get_state (netif)->devname);
netif->mtu = TCP_MSS + 20 + 20;
hurdethif_device_get_flags (netif, &netif_get_state (netif)->flags);
netif_get_state (netif)->flags |=
IFF_UP | IFF_RUNNING | IFF_BROADCAST | IFF_ALLMULTI;
hurdethif_device_set_flags (netif, netif_get_state (netif)->flags);
netif->flags = NETIF_FLAG_BROADCAST | NETIF_FLAG_ETHARP | NETIF_FLAG_LINK_UP
| NETIF_FLAG_IGMP | NETIF_FLAG_MLD6;
return ERR_OK;
}
static void *
hurdethif_input_thread (void *arg)
{
pthread_setname_np (pthread_self (), "input");
ports_manage_port_operations_one_thread (etherport_bucket,
hurdethif_demuxer, 0);
return 0;
}
error_t
hurdethif_module_init (void)
{
error_t err;
etherport_bucket = ports_create_bucket ();
etherread_class = ports_create_class (0, 0);
err = pthread_create (&input_thread, 0, hurdethif_input_thread, 0);
if (!err)
pthread_detach (input_thread);
else
{
errno = err;
perror ("pthread_create");
}
return err;
}