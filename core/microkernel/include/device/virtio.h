#ifndef _DEVICE_VIRTIO_H_
#define _DEVICE_VIRTIO_H_
#include <device/dev_hdr.h>
#include <kern/lock.h>
#include <kern/queue.h>
#include <mach/kern_return.h>
#include <stddef.h>
#define VIRTIO_ID_NET        1
#define VIRTIO_ID_BLOCK      2
#define VIRTIO_ID_CONSOLE    3
#define VIRTIO_ID_SCSI       8
#define VIRTIO_ID_BALLOON    5
#define VIRTIO_F_RING_INDIRECT_DESC  28
#define VIRTIO_F_RING_EVENT_IDX      29
#define VIRTIO_F_VERSION_1           32
#define VIRTIO_PCI_HOST_FEATURES     0
#define VIRTIO_PCI_GUEST_FEATURES    4
#define VIRTIO_PCI_QUEUE_PFN         8
#define VIRTIO_PCI_QUEUE_NUM         12
#define VIRTIO_PCI_QUEUE_SEL         14
#define VIRTIO_PCI_QUEUE_NOTIFY      16
#define VIRTIO_PCI_STATUS            18
#define VIRTIO_PCI_ISR               19
#define VIRTIO_PCI_CONFIG            20
#define VIRTIO_STATUS_RESET          0x00
#define VIRTIO_STATUS_ACKNOWLEDGE    0x01
#define VIRTIO_STATUS_DRIVER         0x02
#define VIRTIO_STATUS_DRIVER_OK      0x04
#define VIRTIO_STATUS_FEATURES_OK    0x08
#define VIRTIO_STATUS_FAILED         0x80
#define VRING_DESC_F_NEXT     1
#define VRING_DESC_F_WRITE    2
#define VRING_DESC_F_INDIRECT 4
struct vring_desc {
uint64_t addr;
uint32_t len;
uint16_t flags;
uint16_t next;
};
struct vring_avail {
uint16_t flags;
uint16_t idx;
uint16_t ring[];
};
struct vring_used_elem {
uint32_t id;
uint32_t len;
};
struct vring_used {
uint16_t flags;
uint16_t idx;
struct vring_used_elem ring[];
};
struct virtqueue {
unsigned int num;
struct vring_desc *desc;
struct vring_avail *avail;
struct vring_used *used;
uint16_t last_used_idx;
void *data;
simple_lock_data_t lock;
};
struct virtio_device;
struct virtio_driver {
queue_chain_t link;
const char *name;
uint32_t device_id;
uint32_t *feature_table;
unsigned int feature_table_size;
int (*probe)(struct virtio_device *dev);
void (*remove)(struct virtio_device *dev);
int (*suspend)(struct virtio_device *dev);
int (*resume)(struct virtio_device *dev);
};
struct virtio_device {
queue_chain_t link;
uint32_t device_id;
uint32_t vendor_id;
uint32_t features;
struct virtio_driver *driver;
void *priv;
vm_offset_t config_base;
int irq;
struct virtqueue **vqs;
unsigned int nvqs;
simple_lock_data_t lock;
int status;
};
struct virtio_subsystem {
queue_head_t devices;
queue_head_t drivers;
simple_lock_data_t lock;
boolean_t initialized;
};
extern void virtio_init(void);
extern kern_return_t virtio_register_driver(struct virtio_driver *driver);
extern void virtio_unregister_driver(struct virtio_driver *driver);
extern kern_return_t virtio_register_device(struct virtio_device *dev);
extern void virtio_unregister_device(struct virtio_device *dev);
extern struct virtio_device *virtio_alloc_device(void);
extern void virtio_free_device(struct virtio_device *dev);
extern kern_return_t virtio_setup_vqs(struct virtio_device *dev,
unsigned int nvqs,
const char **names);
extern void virtio_cleanup_vqs(struct virtio_device *dev);
extern struct virtqueue *virtio_find_vq(struct virtio_device *dev, unsigned int index);
extern kern_return_t virtio_add_buf(struct virtqueue *vq,
struct vring_desc *desc_list,
unsigned int out_num,
unsigned int in_num,
void *data);
extern void *virtio_get_buf(struct virtqueue *vq, uint32_t *len);
extern void virtio_kick(struct virtqueue *vq);
extern void virtio_disable_cb(struct virtqueue *vq);
extern boolean_t virtio_enable_cb(struct virtqueue *vq);
extern uint32_t virtio_config_readl(struct virtio_device *dev, unsigned int offset);
extern uint16_t virtio_config_readw(struct virtio_device *dev, unsigned int offset);
extern uint8_t virtio_config_readb(struct virtio_device *dev, unsigned int offset);
extern void virtio_config_writel(struct virtio_device *dev, unsigned int offset, uint32_t val);
extern void virtio_config_writew(struct virtio_device *dev, unsigned int offset, uint16_t val);
extern void virtio_config_writeb(struct virtio_device *dev, unsigned int offset, uint8_t val);
extern boolean_t virtio_has_feature(struct virtio_device *dev, uint32_t feature);
extern void virtio_finalize_features(struct virtio_device *dev);
extern uint32_t virtio_get_features(struct virtio_device *dev);
struct pci_dev;
extern kern_return_t virtio_pci_init(void);
extern kern_return_t virtio_pci_probe_device(struct pci_dev *pci_dev);
extern void virtio_dump_device_info(struct virtio_device *dev);
extern void virtio_dump_queue_info(struct virtqueue *vq);
extern void virtio_dump_subsystem_info(void);
#define VIRTIO_DEVICE_LOCK(dev) \
simple_lock(&(dev)->lock)
#define VIRTIO_DEVICE_UNLOCK(dev) \
simple_unlock(&(dev)->lock)
#define VIRTIO_QUEUE_LOCK(vq) \
simple_lock(&(vq)->lock)
#define VIRTIO_QUEUE_UNLOCK(vq) \
simple_unlock(&(vq)->lock)
#endif