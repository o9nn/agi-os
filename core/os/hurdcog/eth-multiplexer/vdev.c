#include <string.h>
#include <stdio.h>
#include <net/if_ether.h>
#include <netinet/ip.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdlib.h>
#include <error.h>
#include <hurd/ihash.h>
#include <pthread.h>
#include "vdev.h"
#include "ethernet.h"
#include "queue.h"
#include "bpf_impl.h"
#include "util.h"
static struct vether_device *dev_head;
static int dev_num;
static pthread_mutex_t dev_list_lock = PTHREAD_MUTEX_INITIALIZER;
#define MSG_ALIGNMENT __alignof__(uintptr_t)
static const mach_msg_type_t header_type =
{
.msgt_name = MACH_MSG_TYPE_BYTE,
.msgt_size = 8,
.msgt_number = NET_HDW_HDR_MAX,
.msgt_inline = TRUE,
.msgt_longform = FALSE,
.msgt_deallocate = FALSE,
.msgt_unused = 0
};
static const mach_msg_type_t packet_type =
{
.msgt_name = MACH_MSG_TYPE_BYTE,
.msgt_size = 8,
.msgt_number = 0,
.msgt_inline = TRUE,
.msgt_longform = FALSE,
.msgt_deallocate = FALSE,
.msgt_unused = 0
};
int
get_dev_num (void)
{
return dev_num;
}
struct vether_device *
lookup_dev_by_name (const char *name)
{
struct vether_device *vdev;
pthread_mutex_lock (&dev_list_lock);
for (vdev = dev_head; vdev; vdev = vdev->next)
{
if (strncmp (vdev->name, name, IFNAMSIZ) == 0)
break;
}
pthread_mutex_unlock (&dev_list_lock);
return vdev;
}
int
foreach_dev_do (int (func) (struct vether_device *))
{
struct vether_device *vdev;
int rval = 0;
pthread_mutex_lock (&dev_list_lock);
for (vdev = dev_head; vdev; vdev = vdev->next)
{
pthread_mutex_unlock (&dev_list_lock);
rval = func (vdev);
pthread_mutex_lock (&dev_list_lock);
if (rval)
break;
}
pthread_mutex_unlock (&dev_list_lock);
return rval;
}
int
remove_dead_port_from_dev (mach_port_t dead_port)
{
struct vether_device *vdev;
pthread_mutex_lock (&dev_list_lock);
for (vdev = dev_head; vdev; vdev = vdev->next)
{
remove_dead_filter (&vdev->port_list,
&vdev->port_list.if_rcv_port_list, dead_port);
remove_dead_filter (&vdev->port_list,
&vdev->port_list.if_snd_port_list, dead_port);
}
pthread_mutex_unlock (&dev_list_lock);
return 0;
}
struct vether_device *
add_vdev (char *name, size_t size)
{
error_t err;
uint32_t hash;
struct vether_device *vdev;
if (size < sizeof (*vdev))
size = sizeof (*vdev);
err = ports_create_port (vdev_portclass, port_bucket, size, &vdev);
if (err)
return NULL;
vdev->dev_port = ports_get_right (vdev);
ports_port_deref (vdev);
strncpy (vdev->name, name, IFNAMSIZ-1);
vdev->name[IFNAMSIZ-1] = '\0';
vdev->if_header_size = ETH_HLEN;
vdev->if_mtu = ETH_MTU;
vdev->if_header_format = HDR_ETHERNET;
vdev->if_address_size = ETH_ALEN;
vdev->if_flags = (
IFF_UP
| IFF_RUNNING
| IFF_BROADCAST | IFF_MULTICAST);
vdev->if_address[0] = 0x52;
vdev->if_address[1] = 0x54;
hash = hurd_ihash_hash32 (ether_address, ETH_ALEN, 0);
hash = hurd_ihash_hash32 (name, strlen (name), hash);
memcpy (&vdev->if_address[2], &hash, 4);
queue_init (&vdev->port_list.if_rcv_port_list);
queue_init (&vdev->port_list.if_snd_port_list);
pthread_mutex_lock (&dev_list_lock);
vdev->next = dev_head;
dev_head = vdev;
vdev->pprev = &dev_head;
if (vdev->next)
vdev->next->pprev = &vdev->next;
dev_num++;
pthread_mutex_unlock (&dev_list_lock);
debug ("initialize the virtual device\n");
return vdev;
}
void
destroy_vdev (void *port)
{
struct vether_device *vdev = (struct vether_device *)port;
debug ("device %s is going to be destroyed\n", vdev->name);
pthread_mutex_lock (&dev_list_lock);
*vdev->pprev = vdev->next;
if (vdev->next)
vdev->next->pprev = vdev->pprev;
dev_num--;
pthread_mutex_unlock (&dev_list_lock);
destroy_filters (&vdev->port_list);
}
static int deliver_msg (struct net_rcv_msg *msg, struct vether_device *vdev);
int
broadcast_pack (char *data, int datalen, struct vether_device *from_vdev)
{
struct net_rcv_msg msg;
int pack_size;
struct ethhdr *header;
struct packet_header *packet;
pack_size = datalen - sizeof (struct ethhdr);
msg.msg_hdr.msgh_size = sizeof (struct net_rcv_msg) - NET_RCV_MAX + pack_size;
msg.msg_hdr.msgh_size = (mach_msg_size_t) ((msg.msg_hdr.msgh_size +
MSG_ALIGNMENT - 1) & ~(MSG_ALIGNMENT - 1));
header = (struct ethhdr *) msg.header;
packet = (struct packet_header *) msg.packet;
msg.header_type = header_type;
memcpy (header, data, sizeof (struct ethhdr));
msg.packet_type = packet_type;
memcpy (packet + 1, data + sizeof (struct ethhdr), pack_size);
packet->type = header->h_proto;
packet->length = pack_size + sizeof (struct packet_header);
msg.packet_type.msgt_number = packet->length;
int internal_deliver_pack (struct vether_device *vdev)
{
if (from_vdev == vdev)
return 0;
if ((vdev->if_flags & IFF_UP) == 0)
return 0;
return deliver_msg (&msg, vdev);
}
return foreach_dev_do (internal_deliver_pack);
}
int
broadcast_msg (struct net_rcv_msg *msg)
{
int rval = 0;
mach_msg_header_t header;
int internal_deliver_msg (struct vether_device *vdev)
{
if ((vdev->if_flags & IFF_UP) == 0)
return 0;
return deliver_msg (msg, vdev);
}
header = msg->msg_hdr;
rval = foreach_dev_do (internal_deliver_msg);
msg->msg_hdr = header;
return rval;
}
static int
deliver_msg(struct net_rcv_msg *msg, struct vether_device *vdev)
{
mach_msg_return_t err;
queue_head_t *if_port_list;
net_rcv_port_t infp, nextfp;
msg->msg_hdr.msgh_bits = MACH_MSGH_BITS (MACH_MSG_TYPE_COPY_SEND, 0);
msg->msg_hdr.msgh_local_port = MACH_PORT_NULL;
msg->msg_hdr.msgh_seqno = 0;
msg->msg_hdr.msgh_id = NET_RCV_MSG_ID;
if_port_list = &vdev->port_list.if_rcv_port_list;
FILTER_ITERATE (if_port_list, infp, nextfp, &infp->input)
{
mach_port_t dest;
net_hash_entry_t entp, *hash_headp;
int ret_count;
entp = (net_hash_entry_t) 0;
ret_count = bpf_do_filter (infp,
msg->packet + sizeof (struct packet_header),
msg->net_rcv_msg_packet_count, msg->header,
sizeof (struct ethhdr), &hash_headp, &entp);
if (entp == (net_hash_entry_t) 0)
dest = infp->rcv_port;
else
dest = entp->rcv_port;
if (ret_count)
{
debug ("before delivering the packet\n");
msg->msg_hdr.msgh_remote_port = dest;
err = mach_msg ((mach_msg_header_t *)msg,
MACH_SEND_MSG|MACH_SEND_TIMEOUT,
msg->msg_hdr.msgh_size, 0, MACH_PORT_NULL,
0, MACH_PORT_NULL);
if (err != MACH_MSG_SUCCESS)
{
mach_port_deallocate(mach_task_self (),
((mach_msg_header_t *)msg)->msgh_remote_port);
error (0, err, "mach_msg");
}
debug ("after delivering the packet\n");
}
}
FILTER_ITERATE_END
return 0;
}