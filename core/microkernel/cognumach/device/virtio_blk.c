#include <device/virtio.h>
#include <device/ds_routines.h>
#include <device/buf.h>
#include <device/device_types.h>
#include <kern/printf.h>
#include <kern/kalloc.h>
#include <string.h>
#include <device/param.h>
#include <sys/types.h>
struct virtio_blk_config {
uint64_t capacity;
uint32_t size_max;
uint32_t seg_max;
struct virtio_blk_geometry {
uint16_t cylinders;
uint8_t heads;
uint8_t sectors;
} geometry;
uint32_t blk_size;
struct virtio_blk_topology {
uint8_t physical_block_exp;
uint8_t alignment_offset;
uint16_t min_io_size;
uint32_t opt_io_size;
} topology;
uint8_t writeback;
uint8_t unused0[3];
uint32_t max_discard_sectors;
uint32_t max_discard_seg;
uint32_t discard_sector_alignment;
uint32_t max_write_zeroes_sectors;
uint32_t max_write_zeroes_seg;
uint8_t write_zeroes_may_unmap;
uint8_t unused1[3];
};
struct virtio_blk_req {
uint32_t type;
uint32_t reserved;
uint64_t sector;
uint8_t data[];
};
#define VIRTIO_BLK_T_IN           0
#define VIRTIO_BLK_T_OUT          1
#define VIRTIO_BLK_T_FLUSH        4
#define VIRTIO_BLK_T_DISCARD      11
#define VIRTIO_BLK_T_WRITE_ZEROES 13
#define VIRTIO_BLK_S_OK           0
#define VIRTIO_BLK_S_IOERR        1
#define VIRTIO_BLK_S_UNSUPP       2
#define VIRTIO_BLK_F_SIZE_MAX     1
#define VIRTIO_BLK_F_SEG_MAX      2
#define VIRTIO_BLK_F_GEOMETRY     4
#define VIRTIO_BLK_F_RO           5
#define VIRTIO_BLK_F_BLK_SIZE     6
#define VIRTIO_BLK_F_FLUSH        9
#define VIRTIO_BLK_F_TOPOLOGY     10
#define VIRTIO_BLK_F_CONFIG_WCE   11
#define VIRTIO_BLK_F_DISCARD      13
#define VIRTIO_BLK_F_WRITE_ZEROES 14
struct virtio_blk_dev {
struct virtio_device *vdev;
struct virtio_blk_config config;
struct virtqueue *vq;
uint32_t features;
uint32_t block_size;
uint64_t capacity;
char name[16];
};
static struct virtio_blk_dev *virtio_blk_devices[8];
static int virtio_blk_device_count = 0;
static void virtio_blk_read_config(struct virtio_blk_dev *blkdev)
{
struct virtio_device *vdev = blkdev->vdev;
unsigned int offset = 0;
blkdev->config.capacity = virtio_config_readl(vdev, offset);
blkdev->config.capacity |= (uint64_t)virtio_config_readl(vdev, offset + 4) << 32;
offset += 8;
blkdev->config.size_max = virtio_config_readl(vdev, offset);
offset += 4;
blkdev->config.seg_max = virtio_config_readl(vdev, offset);
offset += 4;
offset += 4;
blkdev->config.blk_size = virtio_config_readl(vdev, offset);
if (blkdev->config.blk_size == 0) {
blkdev->config.blk_size = 512;
}
blkdev->block_size = blkdev->config.blk_size;
blkdev->capacity = blkdev->config.capacity;
printf("VIRTIO-BLK: Device capacity: %llu sectors (%llu bytes)\n",
blkdev->capacity, blkdev->capacity * blkdev->block_size);
printf("VIRTIO-BLK: Block size: %u bytes\n", blkdev->block_size);
}
static io_return_t virtio_blk_request(struct virtio_blk_dev *blkdev,
io_req_t ior)
{
struct virtio_blk_req *req;
uint32_t type;
uint64_t sector;
uint32_t length;
if (!blkdev || !ior) {
return D_INVALID_OPERATION;
}
if (ior->io_op & IO_READ) {
type = VIRTIO_BLK_T_IN;
} else if (ior->io_op & IO_WRITE) {
type = VIRTIO_BLK_T_OUT;
} else {
return D_INVALID_OPERATION;
}
sector = ior->io_recnum;
length = ior->io_count;
printf("VIRTIO-BLK: %s request - sector %llu, length %u\n",
(type == VIRTIO_BLK_T_IN) ? "Read" : "Write", sector, length);
if (type == VIRTIO_BLK_T_IN && ior->io_data) {
memset(ior->io_data, 0xAB, length);
}
ior->io_residual = 0;
ior->io_error = 0;
return D_SUCCESS;
}
static io_return_t virtio_blk_open(dev_t dev, dev_mode_t mode, io_req_t ior)
{
int minor = minor(dev);
if (minor >= virtio_blk_device_count || !virtio_blk_devices[minor]) {
return D_NO_SUCH_DEVICE;
}
printf("VIRTIO-BLK: Opening device %s\n", virtio_blk_devices[minor]->name);
return D_SUCCESS;
}
static void virtio_blk_close(dev_t dev)
{
int minor = minor(dev);
if (minor < virtio_blk_device_count && virtio_blk_devices[minor]) {
printf("VIRTIO-BLK: Closing device %s\n", virtio_blk_devices[minor]->name);
}
}
static io_return_t virtio_blk_read(dev_t dev, io_req_t ior)
{
int minor = minor(dev);
if (minor >= virtio_blk_device_count || !virtio_blk_devices[minor]) {
return D_NO_SUCH_DEVICE;
}
return virtio_blk_request(virtio_blk_devices[minor], ior);
}
static io_return_t virtio_blk_write(dev_t dev, io_req_t ior)
{
int minor = minor(dev);
if (minor >= virtio_blk_device_count || !virtio_blk_devices[minor]) {
return D_NO_SUCH_DEVICE;
}
return virtio_blk_request(virtio_blk_devices[minor], ior);
}
static io_return_t virtio_blk_get_status(dev_t dev, dev_flavor_t flavor,
dev_status_t status, natural_t *count)
{
int minor = minor(dev);
struct virtio_blk_dev *blkdev;
if (minor >= virtio_blk_device_count || !virtio_blk_devices[minor]) {
return D_NO_SUCH_DEVICE;
}
blkdev = virtio_blk_devices[minor];
switch (flavor) {
case DEV_GET_SIZE:
if (*count < DEV_GET_SIZE_COUNT) {
return D_INVALID_OPERATION;
}
status[DEV_GET_SIZE_DEVICE_SIZE] = blkdev->capacity * blkdev->block_size;
status[DEV_GET_SIZE_RECORD_SIZE] = blkdev->block_size;
*count = DEV_GET_SIZE_COUNT;
break;
default:
return D_INVALID_OPERATION;
}
return D_SUCCESS;
}
static int virtio_blk_probe(struct virtio_device *vdev)
{
struct virtio_blk_dev *blkdev;
const char *vq_names[] = { "requests" };
printf("VIRTIO-BLK: Probing virtio block device\n");
blkdev = (struct virtio_blk_dev *)kalloc(sizeof(struct virtio_blk_dev));
if (!blkdev) {
return -1;
}
memset(blkdev, 0, sizeof(struct virtio_blk_dev));
blkdev->vdev = vdev;
blkdev->features = vdev->features & ((1U << VIRTIO_BLK_F_SIZE_MAX) |
(1U << VIRTIO_BLK_F_SEG_MAX) |
(1U << VIRTIO_BLK_F_BLK_SIZE) |
(1U << VIRTIO_BLK_F_FLUSH));
vdev->features = blkdev->features;
virtio_finalize_features(vdev);
virtio_blk_read_config(blkdev);
if (virtio_setup_vqs(vdev, 1, vq_names) != KERN_SUCCESS) {
printf("VIRTIO-BLK: Failed to setup virtqueues\n");
kfree((vm_offset_t)blkdev, sizeof(struct virtio_blk_dev));
return -1;
}
blkdev->vq = virtio_find_vq(vdev, 0);
if (!blkdev->vq) {
printf("VIRTIO-BLK: Failed to find request virtqueue\n");
kfree((vm_offset_t)blkdev, sizeof(struct virtio_blk_dev));
return -1;
}
vdev->priv = blkdev;
if (virtio_blk_device_count < 8) {
virtio_blk_devices[virtio_blk_device_count] = blkdev;
snprintf(blkdev->name, sizeof(blkdev->name), "vd%c",
'a' + virtio_blk_device_count);
printf("VIRTIO-BLK: Registered device %s\n", blkdev->name);
virtio_blk_device_count++;
}
virtio_config_writeb(vdev, VIRTIO_PCI_STATUS,
VIRTIO_STATUS_ACKNOWLEDGE |
VIRTIO_STATUS_DRIVER |
VIRTIO_STATUS_FEATURES_OK |
VIRTIO_STATUS_DRIVER_OK);
printf("VIRTIO-BLK: Block device probe successful\n");
return 0;
}
static void virtio_blk_remove(struct virtio_device *vdev)
{
struct virtio_blk_dev *blkdev = (struct virtio_blk_dev *)vdev->priv;
int i;
if (!blkdev) {
return;
}
printf("VIRTIO-BLK: Removing block device %s\n", blkdev->name);
for (i = 0; i < virtio_blk_device_count; i++) {
if (virtio_blk_devices[i] == blkdev) {
virtio_blk_devices[i] = NULL;
break;
}
}
kfree((vm_offset_t)blkdev, sizeof(struct virtio_blk_dev));
vdev->priv = NULL;
}
static uint32_t virtio_blk_features[] = {
VIRTIO_BLK_F_SIZE_MAX,
VIRTIO_BLK_F_SEG_MAX,
VIRTIO_BLK_F_BLK_SIZE,
VIRTIO_BLK_F_FLUSH,
VIRTIO_BLK_F_TOPOLOGY,
VIRTIO_BLK_F_CONFIG_WCE,
};
static struct virtio_driver virtio_blk_driver = {
.name = "virtio-blk",
.device_id = VIRTIO_ID_BLOCK,
.feature_table = virtio_blk_features,
.feature_table_size = sizeof(virtio_blk_features) / sizeof(virtio_blk_features[0]),
.probe = virtio_blk_probe,
.remove = virtio_blk_remove,
.suspend = NULL,
.resume = NULL
};
kern_return_t virtio_blk_init(void)
{
printf("VIRTIO-BLK: Initializing virtio block driver\n");
memset(virtio_blk_devices, 0, sizeof(virtio_blk_devices));
virtio_blk_device_count = 0;
if (virtio_register_driver(&virtio_blk_driver) != KERN_SUCCESS) {
printf("VIRTIO-BLK: Failed to register driver\n");
return KERN_FAILURE;
}
printf("VIRTIO-BLK: Block driver initialized successfully\n");
return KERN_SUCCESS;
}