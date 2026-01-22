#include "pfinet.h"
#include <hurd.h>
#include <pthread.h>
#include <fcntl.h>
#include <device/device.h>
#include <device/net_status.h>
#include <netinet/in.h>
#include <string.h>
#include <error.h>
#include <errno.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/if_arp.h>
#include <linux/ppp_defs.h>
#include <linux/if_ppp.h>
#include "libtrivfs/trivfs_fs_S.h"
#include "libtrivfs/trivfs_io_S.h"
struct port_class *tunnel_cntlclass;
struct port_class *tunnel_class;
struct tunnel_device
{
struct tunnel_device *next;
struct trivfs_control *cntl;
char *devname;
file_t underlying;
struct iouser *user;
struct sk_buff_head xq;
pthread_cond_t wait;
pthread_cond_t select_alert;
pthread_mutex_t lock;
int read_blocked;
struct device dev;
struct net_device_stats stats;
};
struct tunnel_device *tunnel_dev;
struct net_device_stats *
tunnel_get_stats (struct device *dev)
{
struct tunnel_device *tdev = (struct tunnel_device *) dev->priv;
assert_backtrace (tdev);
return &tdev->stats;
}
int
tunnel_stop (struct device *dev)
{
struct tunnel_device *tdev = (struct tunnel_device *) dev->priv;
struct sk_buff *skb;
assert_backtrace (tdev);
while ((skb = skb_dequeue (&tdev->xq)) != 0)
dev_kfree_skb(skb);
return 0;
}
void
tunnel_set_multi (struct device *dev)
{
}
void
tunnel_initialize (void)
{
}
int
tunnel_open (struct device *dev)
{
struct tunnel_device *tdev = (struct tunnel_device *) dev->priv;
assert_backtrace (tdev);
skb_queue_head_init(&tdev->xq);
return 0;
}
int
tunnel_xmit (struct sk_buff *skb, struct device *dev)
{
struct tunnel_device *tdev = (struct tunnel_device *) dev->priv;
assert_backtrace (tdev);
pthread_mutex_lock (&tdev->lock);
if (skb_queue_len(&tdev->xq) > 128)
{
struct sk_buff *skb;
skb = skb_dequeue(&tdev->xq);
dev_kfree_skb(skb);
}
skb_queue_tail(&tdev->xq, skb);
if (tdev->read_blocked)
{
tdev->read_blocked = 0;
pthread_cond_broadcast (&tdev->wait);
pthread_cond_broadcast (&tdev->select_alert);
}
pthread_mutex_unlock (&tdev->lock);
return 0;
}
void
setup_tunnel_device (char *name, struct device **device)
{
error_t err;
struct tunnel_device *tdev;
struct device *dev;
char *base_name;
if (!tunnel_dev)
{
trivfs_add_control_port_class (&tunnel_cntlclass);
trivfs_add_protid_port_class (&tunnel_class);
}
tdev = calloc (1, sizeof (struct tunnel_device));
if (!tdev)
error (2, ENOMEM, "%s", name);
tdev->next = tunnel_dev;
tunnel_dev = tdev;
*device = dev = &tdev->dev;
base_name = strrchr (name, '/');
if (base_name)
base_name++;
else
base_name = name;
dev->name = strdup (base_name);
dev->priv = tdev;
dev->get_stats = tunnel_get_stats;
dev->open = tunnel_open;
dev->stop = tunnel_stop;
dev->hard_start_xmit = tunnel_xmit;
dev->set_multicast_list = tunnel_set_multi;
dev->hard_header = 0;
dev->hard_header_len = 0;
dev->mtu = PPP_MTU;
dev->addr_len = 0;
dev->tx_queue_len = 3;
dev->type = ARPHRD_PPP;
dev->flags = IFF_POINTOPOINT | IFF_NOARP | IFF_MULTICAST;
dev_init_buffers (dev);
if (base_name != name)
tdev->devname = strdup (name);
else
asprintf (&tdev->devname, "/dev/%s", tdev->dev.name);
tdev->underlying = file_name_lookup (tdev->devname, O_CREAT|O_NOTRANS, 0664);
if (tdev->underlying == MACH_PORT_NULL)
error (2, 1, "%s", tdev->dev.name);
err = trivfs_create_control (tdev->underlying, tunnel_cntlclass,
pfinet_bucket, tunnel_class, pfinet_bucket,
&tdev->cntl);
tdev->cntl->hook = tdev;
if (! err)
{
mach_port_t right = ports_get_send_right (tdev->cntl);
err = file_set_translator (tdev->underlying, 0, FS_TRANS_EXCL
| FS_TRANS_SET, 0, 0, 0, right,
MACH_MSG_TYPE_COPY_SEND);
mach_port_deallocate (mach_task_self (), right);
}
if (err)
error (2, err, "%s", tdev->dev.name);
pthread_mutex_init (&tdev->lock, NULL);
pthread_cond_init (&tdev->wait, NULL);
pthread_cond_init (&tdev->select_alert, NULL);
err = - register_netdevice (dev);
assert_perror_backtrace (err);
}
static error_t
check_open_hook (struct trivfs_control *cntl,
struct iouser *user,
int flags)
{
struct tunnel_device *tdev;
for (tdev = tunnel_dev; tdev; tdev = tdev->next)
if (tdev->cntl == cntl)
break;
if (tdev && flags != O_NORW)
{
if (tdev->user)
return EBUSY;
else
tdev->user = user;
}
return 0;
}
static void
pi_destroy_hook (struct trivfs_protid *cred)
{
struct tunnel_device *tdev;
if (cred->pi.class != tunnel_class)
return;
tdev = (struct tunnel_device *) cred->po->cntl->hook;
if (tdev->user == cred->user)
tdev->user = 0;
}
error_t (*trivfs_check_open_hook)(struct trivfs_control *,
struct iouser *, int)
= check_open_hook;
void (*trivfs_protid_destroy_hook) (struct trivfs_protid *) = pi_destroy_hook;
kern_return_t
trivfs_S_io_read (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
data_t *data, mach_msg_type_number_t *data_len,
off_t offs, vm_size_t amount)
{
struct tunnel_device *tdev;
struct sk_buff *skb;
if (! cred)
return EOPNOTSUPP;
else if (! (cred->po->openmodes & O_READ))
return EBADF;
if (cred->pi.class != tunnel_class)
return EOPNOTSUPP;
tdev = (struct tunnel_device *) cred->po->cntl->hook;
pthread_mutex_lock (&tdev->lock);
while (skb_queue_len(&tdev->xq) == 0)
{
if (cred->po->openmodes & O_NONBLOCK)
{
pthread_mutex_unlock (&tdev->lock);
return EWOULDBLOCK;
}
tdev->read_blocked = 1;
if (pthread_hurd_cond_wait_np (&tdev->wait, &tdev->lock))
{
pthread_mutex_unlock (&tdev->lock);
return EINTR;
}
}
skb = skb_dequeue (&tdev->xq);
assert_backtrace (skb);
if (skb->len < amount)
amount = skb->len;
if (amount > 0)
{
if (*data_len < amount)
{
*data = mmap (0, amount, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
if (*data == MAP_FAILED)
{
dev_kfree_skb (skb);
pthread_mutex_unlock (&tdev->lock);
return ENOMEM;
}
}
memcpy ((char *) *data, skb->data, amount);
}
*data_len = amount;
dev_kfree_skb (skb);
pthread_mutex_unlock (&tdev->lock);
return 0;
}
kern_return_t
trivfs_S_io_write (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replytype,
const_data_t data,
mach_msg_type_number_t datalen,
off_t offset,
vm_size_t *amount)
{
struct tunnel_device *tdev;
struct sk_buff *skb;
if (! cred)
return EOPNOTSUPP;
else if (! (cred->po->openmodes & O_WRITE))
return EBADF;
if (cred->pi.class != tunnel_class)
return EOPNOTSUPP;
tdev = (struct tunnel_device *) cred->po->cntl->hook;
pthread_mutex_lock (&tdev->lock);
pthread_mutex_lock (&net_bh_lock);
skb = alloc_skb (NET_IP_ALIGN + datalen, GFP_ATOMIC);
skb_reserve(skb, NET_IP_ALIGN);
skb->len = datalen;
skb->dev = &tdev->dev;
memcpy (skb->data, data, datalen);
skb->mac.raw = skb->data;
skb->protocol = htons (ETH_P_IP);
netif_rx (skb);
pthread_mutex_unlock (&net_bh_lock);
*amount = datalen;
pthread_mutex_unlock (&tdev->lock);
return 0;
}
kern_return_t
trivfs_S_io_readable (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t replytype,
vm_size_t *amount)
{
struct tunnel_device *tdev;
struct sk_buff *skb;
if (! cred)
return EOPNOTSUPP;
else if (! (cred->po->openmodes & O_READ))
return EBADF;
if (cred->pi.class != tunnel_class)
return EOPNOTSUPP;
tdev = (struct tunnel_device *) cred->po->cntl->hook;
pthread_mutex_lock (&tdev->lock);
skb = skb_dequeue(&tdev->xq);
if (skb)
{
*amount = skb->len;
skb_queue_head(&tdev->xq, skb);
}
else
*amount = 0;
pthread_mutex_unlock (&tdev->lock);
return 0;
}
static error_t
io_select_common (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t reply_type,
struct timespec *tsp, int *type)
{
struct tunnel_device *tdev;
error_t err;
if (!cred)
return EOPNOTSUPP;
if (cred->pi.class != tunnel_class)
return EOPNOTSUPP;
tdev = (struct tunnel_device *) cred->po->cntl->hook;
*type &= SELECT_READ | SELECT_WRITE;
if (*type == 0)
return 0;
pthread_mutex_lock (&tdev->lock);
if (*type & SELECT_WRITE)
{
if (skb_queue_len (&tdev->xq) == 0)
*type &= ~SELECT_READ;
pthread_mutex_unlock (&tdev->lock);
return 0;
}
while (1)
{
if (skb_queue_len (&tdev->xq) != 0)
{
*type = SELECT_READ;
pthread_mutex_unlock (&tdev->lock);
return 0;
}
ports_interrupt_self_on_port_death (cred, reply);
tdev->read_blocked = 1;
err = pthread_hurd_cond_timedwait_np (&tdev->select_alert, &tdev->lock,
tsp);
if (err)
{
*type = 0;
pthread_mutex_unlock (&tdev->lock);
if (err == ETIMEDOUT)
err = 0;
return err;
}
}
}
kern_return_t
trivfs_S_io_select (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t reply_type,
int *type)
{
return io_select_common (cred, reply, reply_type, NULL, type);
}
kern_return_t
trivfs_S_io_select_timeout (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t reply_type,
struct timespec ts,
int *type)
{
return io_select_common (cred, reply, reply_type, &ts, type);
}
kern_return_t
trivfs_S_io_seek (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
off_t offs, int whence, off_t *new_offs)
{
if (!cred)
return EOPNOTSUPP;
if (cred->pi.class != tunnel_class)
return EOPNOTSUPP;
return ESPIPE;
}
kern_return_t
trivfs_S_file_set_size (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
off_t size)
{
if (!cred)
return EOPNOTSUPP;
if (cred->pi.class != tunnel_class)
return EOPNOTSUPP;
return size == 0 ? 0 : EINVAL;
}
kern_return_t
trivfs_S_io_set_all_openmodes(struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t reply_type,
int mode)
{
if (!cred)
return EOPNOTSUPP;
if (cred->pi.class != tunnel_class)
return EOPNOTSUPP;
return 0;
}
kern_return_t
trivfs_S_io_set_some_openmodes (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t reply_type,
int bits)
{
if (!cred)
return EOPNOTSUPP;
if (cred->pi.class != tunnel_class)
return EOPNOTSUPP;
return 0;
}
kern_return_t
trivfs_S_io_clear_some_openmodes (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t reply_type,
int bits)
{
if (!cred)
return EOPNOTSUPP;
if (cred->pi.class != tunnel_class)
return EOPNOTSUPP;
return 0;
}
kern_return_t
trivfs_S_io_get_owner (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t reply_type,
pid_t *owner)
{
if (!cred)
return EOPNOTSUPP;
if (cred->pi.class != tunnel_class)
return EOPNOTSUPP;
*owner = 0;
return 0;
}
kern_return_t
trivfs_S_io_mod_owner (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
pid_t owner)
{
if (!cred)
return EOPNOTSUPP;
if (cred->pi.class != tunnel_class)
return EOPNOTSUPP;
return EINVAL;
}
kern_return_t
trivfs_S_io_map (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replyPoly,
memory_object_t *rdobj,
mach_msg_type_name_t *rdtype,
memory_object_t *wrobj,
mach_msg_type_name_t *wrtype)
{
if (!cred)
return EOPNOTSUPP;
if (cred->pi.class != tunnel_class)
return EOPNOTSUPP;
return EINVAL;
}