#ifndef _PCMCIA_GLUE_H
#define _PCMCIA_GLUE_H
#define PCMCIA_DEBUG 4
#define MAX_SOCKS 8
#include <linux/version.h>
#define UTS_VERSION ""
#define KERNEL_VERSION(v,p,s)          (((v)<<16)+(p<<8)+s)
#ifdef CONFIG_CARDBUS
#define CARDBUS 1
#endif
#include <linux/malloc.h>
#include <pcmcia/driver_ops.h>
#include <linux/pci.h>
#include <linux/compatmac.h>
#define iounmap(x)             (((long)x<0x100000)?0:vfree ((void*)x))
extern int check_mem_region(u_long base, u_long num);
extern void request_mem_region(u_long base, u_long num, char *name);
extern void release_mem_region(u_long base, u_long num);
#include <linux/delay.h>
#define mod_timer(a, b) \
do { del_timer(a); (a)->expires = (b); add_timer(a); } while (0)
#define mdelay(x) \
do { int i; for (i=0;i<x;i++) __udelay(1000); } while (0)
#define interruptible_sleep_on_timeout(w,t) \
interruptible_sleep_on(w)
#undef signal_pending
#define signal_pending(c) \
0
#include <asm/byteorder.h>
#ifndef le16_to_cpu
#define le16_to_cpu(x)          (x)
#define le32_to_cpu(x)          (x)
#endif
#ifndef cpu_to_le16
#define cpu_to_le16(val)        (val)
#define cpu_to_le32(val)        (val)
#endif
#define wake_up_interruptible wake_up
#undef pci_read_config_word
#undef pci_read_config_dword
#define bus_number(pci_dev)   ((pci_dev)->bus->number)
#define devfn_number(pci_dev) ((pci_dev)->devfn)
#define pci_read_config_byte(pdev, where, valp) \
pcibios_read_config_byte(bus_number(pdev), devfn_number(pdev), where, valp)
#define pci_read_config_word(pdev, where, valp) \
pcibios_read_config_word(bus_number(pdev), devfn_number(pdev), where, valp)
#define pci_read_config_dword(pdev, where, valp) \
pcibios_read_config_dword(bus_number(pdev), devfn_number(pdev), where, valp)
#define pci_write_config_byte(pdev, where, val) \
pcibios_write_config_byte(bus_number(pdev), devfn_number(pdev), where, val)
#define pci_write_config_word(pdev, where, val) \
pcibios_write_config_word(bus_number(pdev), devfn_number(pdev), where, val)
#define pci_write_config_dword(pdev, where, val) \
pcibios_write_config_dword(bus_number(pdev), devfn_number(pdev), where, val)
#define pci_for_each_dev(p) for (p = pci_devices; p; p = p->next)
extern struct pci_dev *pci_find_slot(u_int bus, u_int devfn);
extern struct pci_dev *pci_find_class(u_int class, struct pci_dev *from);
extern int pci_set_power_state(struct pci_dev *dev, int state);
extern int pci_enable_device(struct pci_dev *dev);
extern u32 pci_irq_mask;
#ifdef PCMCIA_CLIENT
#include <linux/netdevice.h>
#include <linux/kcomp.h>
static inline void
init_dev_name(struct net_device *dev, dev_node_t node)
{
dev->name = kmalloc(8, GFP_KERNEL);
dev->name[0] = 0;
int stub(struct device *dev)
{
(void) dev;
return 0;
}
dev->init = stub;
}
#define copy_dev_name(node, dev) do { } while (0)
#endif
#define netif_mark_up(dev)      do { (dev)->start = 1; } while (0)
#define netif_mark_down(dev)    do { (dev)->start = 0; } while (0)
#define netif_carrier_on(dev)   do { dev->flags |= IFF_RUNNING; } while (0)
#define netif_carrier_off(dev)  do { dev->flags &= ~IFF_RUNNING; } while (0)
#define tx_timeout_check(dev, tx_timeout)			 \
do { if (test_and_set_bit(0, (void *)&(dev)->tbusy) != 0) {	 \
if (jiffies - (dev)->trans_start < TX_TIMEOUT) return 1;	 \
tx_timeout(dev);						 \
} } while (0)
#define skb_tx_check(dev, skb)				\
do { if (skb == NULL) { dev_tint(dev); return 0; }	\
if (skb->len <= 0) return 0; } while (0)
#define tx_timeout_check(dev, tx_timeout)			 \
do { if (test_and_set_bit(0, (void *)&(dev)->tbusy) != 0) {	  \
if (jiffies - (dev)->trans_start < TX_TIMEOUT) return 1;	  \
tx_timeout(dev);						  \
} } while (0)
#define DEV_KFREE_SKB(skb)      dev_kfree_skb(skb, FREE_WRITE)
#define net_device_stats        enet_statistics
#define add_rx_bytes(stats, n)  do { int x; x = (n); } while (0)
#define add_tx_bytes(stats, n)  do { int x; x = (n); } while (0)
#define readw_ns(p)             readw(p)
#define writew_ns(v,p)          writew(v,p)
#define MODULE_PARM(a,b)
#define MODULE_AUTHOR(a)
#define MODULE_DESCRIPTION(a)
#define MODULE_LICENSE(a)
#define module_init(a) \
void pcmcia_mod ## a (void) { a(); return; }
#define module_exit(a)
#define disable_irq_nosync(irq) disable_irq(irq)
#endif