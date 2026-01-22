#include <device/virtio.h>
#include <device/ds_routines.h>
#include <device/if_hdr.h>
#include <device/net_status.h>
#include <kern/printf.h>
#include <kern/kalloc.h>
#include <string.h>
#include <sys/types.h>
struct virtio_net_config {
uint8_t mac[6];
uint16_t status;
uint16_t max_virtqueue_pairs;
uint16_t mtu;
uint32_t speed;
uint8_t duplex;
uint8_t rss_max_key_size;
uint16_t rss_max_indirection_table_length;
uint32_t supported_hash_types;
};
struct virtio_net_hdr {
uint8_t flags;
uint8_t gso_type;
uint16_t hdr_len;
uint16_t gso_size;
uint16_t csum_start;
uint16_t csum_offset;
uint16_t num_buffers;
};
#define VIRTIO_NET_F_CSUM             0
#define VIRTIO_NET_F_GUEST_CSUM       1
#define VIRTIO_NET_F_MTU              3
#define VIRTIO_NET_F_MAC              5
#define VIRTIO_NET_F_GSO              6
#define VIRTIO_NET_F_GUEST_TSO4       7
#define VIRTIO_NET_F_GUEST_TSO6       8
#define VIRTIO_NET_F_GUEST_ECN        9
#define VIRTIO_NET_F_GUEST_UFO        10
#define VIRTIO_NET_F_HOST_TSO4        11
#define VIRTIO_NET_F_HOST_TSO6        12
#define VIRTIO_NET_F_HOST_ECN         13
#define VIRTIO_NET_F_HOST_UFO         14
#define VIRTIO_NET_F_MRG_RXBUF        15
#define VIRTIO_NET_F_STATUS           16
#define VIRTIO_NET_F_CTRL_VQ          17
#define VIRTIO_NET_F_CTRL_RX          18
#define VIRTIO_NET_F_CTRL_VLAN        19
#define VIRTIO_NET_F_GUEST_ANNOUNCE   21
#define VIRTIO_NET_F_MQ               22
#define VIRTIO_NET_S_LINK_UP          1
#define VIRTIO_NET_S_ANNOUNCE         2
struct virtio_net_dev {
struct virtio_device *vdev;
struct virtio_net_config config;
struct virtqueue *rx_vq;
struct virtqueue *tx_vq;
struct virtqueue *ctrl_vq;
uint32_t features;
uint8_t mac_addr[6];
uint16_t mtu;
char name[16];
boolean_t link_up;
};
static struct virtio_net_dev *virtio_net_devices[4];
static int virtio_net_device_count = 0;
static void virtio_net_read_config(struct virtio_net_dev *netdev)
{
struct virtio_device *vdev = netdev->vdev;
int i;
for (i = 0; i < 6; i++) {
netdev->config.mac[i] = virtio_config_readb(vdev, i);
netdev->mac_addr[i] = netdev->config.mac[i];
}
printf("VIRTIO-NET: MAC address: %02x:%02x:%02x:%02x:%02x:%02x\n",
netdev->mac_addr[0], netdev->mac_addr[1], netdev->mac_addr[2],
netdev->mac_addr[3], netdev->mac_addr[4], netdev->mac_addr[5]);
if (virtio_has_feature(vdev, VIRTIO_NET_F_STATUS)) {
netdev->config.status = virtio_config_readw(vdev, 6);
netdev->link_up = !!(netdev->config.status & VIRTIO_NET_S_LINK_UP);
printf("VIRTIO-NET: Link status: %s\n", netdev->link_up ? "up" : "down");
} else {
netdev->link_up = TRUE;
}
if (virtio_has_feature(vdev, VIRTIO_NET_F_MTU)) {
netdev->config.mtu = virtio_config_readw(vdev, 10);
netdev->mtu = netdev->config.mtu;
printf("VIRTIO-NET: MTU: %u bytes\n", netdev->mtu);
} else {
netdev->mtu = 1500;
}
}
static io_return_t virtio_net_transmit(struct virtio_net_dev *netdev,
io_req_t ior)
{
struct virtio_net_hdr hdr;
if (!netdev || !ior || !ior->io_data) {
return D_INVALID_OPERATION;
}
if (!netdev->link_up) {
printf("VIRTIO-NET: Cannot transmit - link is down\n");
return D_IO_ERROR;
}
printf("VIRTIO-NET: Transmitting packet, length %u\n", ior->io_count);
memset(&hdr, 0, sizeof(hdr));
ior->io_residual = 0;
ior->io_error = 0;
return D_SUCCESS;
}
static io_return_t virtio_net_receive(struct virtio_net_dev *netdev,
io_req_t ior)
{
if (!netdev || !ior) {
return D_INVALID_OPERATION;
}
if (!netdev->link_up) {
printf("VIRTIO-NET: Cannot receive - link is down\n");
return D_IO_ERROR;
}
printf("VIRTIO-NET: Receive request\n");
ior->io_residual = ior->io_count;
ior->io_error = 0;
return D_WOULD_BLOCK;
}
static io_return_t virtio_net_open(dev_t dev, dev_mode_t mode, io_req_t ior)
{
int minor = minor(dev);
if (minor >= virtio_net_device_count || !virtio_net_devices[minor]) {
return D_NO_SUCH_DEVICE;
}
printf("VIRTIO-NET: Opening device %s\n", virtio_net_devices[minor]->name);
return D_SUCCESS;
}
static void virtio_net_close(dev_t dev)
{
int minor = minor(dev);
if (minor < virtio_net_device_count && virtio_net_devices[minor]) {
printf("VIRTIO-NET: Closing device %s\n", virtio_net_devices[minor]->name);
}
}
static io_return_t virtio_net_read(dev_t dev, io_req_t ior)
{
int minor = minor(dev);
if (minor >= virtio_net_device_count || !virtio_net_devices[minor]) {
return D_NO_SUCH_DEVICE;
}
return virtio_net_receive(virtio_net_devices[minor], ior);
}
static io_return_t virtio_net_write(dev_t dev, io_req_t ior)
{
int minor = minor(dev);
if (minor >= virtio_net_device_count || !virtio_net_devices[minor]) {
return D_NO_SUCH_DEVICE;
}
return virtio_net_transmit(virtio_net_devices[minor], ior);
}
static io_return_t virtio_net_get_status(dev_t dev, dev_flavor_t flavor,
dev_status_t status, natural_t *count)
{
int minor = minor(dev);
struct virtio_net_dev *netdev;
if (minor >= virtio_net_device_count || !virtio_net_devices[minor]) {
return D_NO_SUCH_DEVICE;
}
netdev = virtio_net_devices[minor];
switch (flavor) {
case NET_STATUS:
if (*count < NET_STATUS_COUNT) {
return D_INVALID_OPERATION;
}
struct net_status *net_stat = (struct net_status *)status;
net_stat->min_packet_size = 64;
net_stat->max_packet_size = netdev->mtu + 14;
net_stat->header_format = HDR_ETHERNET;
net_stat->header_size = 14;
net_stat->address_size = 6;
net_stat->flags = netdev->link_up ? IFF_UP | IFF_RUNNING : 0;
net_stat->mapped_size = 0;
*count = NET_STATUS_COUNT;
break;
default:
return D_INVALID_OPERATION;
}
return D_SUCCESS;
}
static int virtio_net_probe(struct virtio_device *vdev)
{
struct virtio_net_dev *netdev;
const char *vq_names[] = { "rx", "tx", "ctrl" };
int nvqs = 2;
printf("VIRTIO-NET: Probing virtio network device\n");
netdev = (struct virtio_net_dev *)kalloc(sizeof(struct virtio_net_dev));
if (!netdev) {
return -1;
}
memset(netdev, 0, sizeof(struct virtio_net_dev));
netdev->vdev = vdev;
netdev->features = vdev->features & ((1U << VIRTIO_NET_F_MAC) |
(1U << VIRTIO_NET_F_STATUS) |
(1U << VIRTIO_NET_F_MTU) |
(1U << VIRTIO_NET_F_CSUM) |
(1U << VIRTIO_NET_F_GUEST_CSUM));
if (vdev->features & (1U << VIRTIO_NET_F_CTRL_VQ)) {
netdev->features |= (1U << VIRTIO_NET_F_CTRL_VQ);
nvqs = 3;
}
vdev->features = netdev->features;
virtio_finalize_features(vdev);
virtio_net_read_config(netdev);
if (virtio_setup_vqs(vdev, nvqs, vq_names) != KERN_SUCCESS) {
printf("VIRTIO-NET: Failed to setup virtqueues\n");
kfree((vm_offset_t)netdev, sizeof(struct virtio_net_dev));
return -1;
}
netdev->rx_vq = virtio_find_vq(vdev, 0);
netdev->tx_vq = virtio_find_vq(vdev, 1);
if (nvqs > 2) {
netdev->ctrl_vq = virtio_find_vq(vdev, 2);
}
if (!netdev->rx_vq || !netdev->tx_vq) {
printf("VIRTIO-NET: Failed to find required virtqueues\n");
kfree((vm_offset_t)netdev, sizeof(struct virtio_net_dev));
return -1;
}
vdev->priv = netdev;
if (virtio_net_device_count < 4) {
virtio_net_devices[virtio_net_device_count] = netdev;
snprintf(netdev->name, sizeof(netdev->name), "eth%d",
virtio_net_device_count);
printf("VIRTIO-NET: Registered network device %s\n", netdev->name);
virtio_net_device_count++;
}
virtio_config_writeb(vdev, VIRTIO_PCI_STATUS,
VIRTIO_STATUS_ACKNOWLEDGE |
VIRTIO_STATUS_DRIVER |
VIRTIO_STATUS_FEATURES_OK |
VIRTIO_STATUS_DRIVER_OK);
printf("VIRTIO-NET: Network device probe successful\n");
return 0;
}
static void virtio_net_remove(struct virtio_device *vdev)
{
struct virtio_net_dev *netdev = (struct virtio_net_dev *)vdev->priv;
int i;
if (!netdev) {
return;
}
printf("VIRTIO-NET: Removing network device %s\n", netdev->name);
for (i = 0; i < virtio_net_device_count; i++) {
if (virtio_net_devices[i] == netdev) {
virtio_net_devices[i] = NULL;
break;
}
}
kfree((vm_offset_t)netdev, sizeof(struct virtio_net_dev));
vdev->priv = NULL;
}
static uint32_t virtio_net_features[] = {
VIRTIO_NET_F_MAC,
VIRTIO_NET_F_STATUS,
VIRTIO_NET_F_MTU,
VIRTIO_NET_F_CSUM,
VIRTIO_NET_F_GUEST_CSUM,
VIRTIO_NET_F_CTRL_VQ,
VIRTIO_NET_F_CTRL_RX,
};
static struct virtio_driver virtio_net_driver = {
.name = "virtio-net",
.device_id = VIRTIO_ID_NET,
.feature_table = virtio_net_features,
.feature_table_size = sizeof(virtio_net_features) / sizeof(virtio_net_features[0]),
.probe = virtio_net_probe,
.remove = virtio_net_remove,
.suspend = NULL,
.resume = NULL
};
kern_return_t virtio_net_init(void)
{
printf("VIRTIO-NET: Initializing virtio network driver\n");
memset(virtio_net_devices, 0, sizeof(virtio_net_devices));
virtio_net_device_count = 0;
if (virtio_register_driver(&virtio_net_driver) != KERN_SUCCESS) {
printf("VIRTIO-NET: Failed to register driver\n");
return KERN_FAILURE;
}
printf("VIRTIO-NET: Network driver initialized successfully\n");
return KERN_SUCCESS;
}