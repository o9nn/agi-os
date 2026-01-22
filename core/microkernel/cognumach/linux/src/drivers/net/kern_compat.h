#ifndef _KERN_COMPAT_H
#define _KERN_COMPAT_H
#if ! defined(LINUX_VERSION_CODE) || (LINUX_VERSION_CODE < 0x10000)
#include <linux/version.h>
#endif
#if LINUX_VERSION_CODE < 0x20300 && defined(MODVERSIONS)
#include <linux/modversions.h>
#endif
#if LINUX_VERSION_CODE < 0x20100 && ! defined(__alpha__)
#define ioremap(a,b)\
(((unsigned long)(a) >= 0x100000) ? vremap(a,b) : (void*)(a))
#define iounmap(v)\
do { if ((unsigned long)(v) >= 0x100000) vfree(v);} while (0)
#endif
#if LINUX_VERSION_CODE < 0x20115
#define MODULE_AUTHOR(name) extern int nonesuch
#define MODULE_DESCRIPTION(string) extern int nonesuch
#define MODULE_PARM(varname, typestring) extern int nonesuch
#define MODULE_PARM_DESC(var,desc) extern int nonesuch
#endif
#if !defined(MODULE_LICENSE)
#define MODULE_LICENSE(license) \
static const char __module_license[] __attribute__((section(".modinfo"))) = \
"license=" license
#endif
#if !defined(MODULE_PARM_DESC)
#define MODULE_PARM_DESC(var,desc) \
const char __module_parm_desc_##var[] \
__attribute__((section(".modinfo"))) = \
"parm_desc_" __MODULE_STRING(var) "=" desc
#endif
#if LINUX_VERSION_CODE < 0x20123
#define hard_smp_processor_id() smp_processor_id()
#define cpu_to_le16(val) (val)
#define cpu_to_le32(val) (val)
#define le16_to_cpu(val) (val)
#define le16_to_cpus(val)
#define le32_to_cpu(val) (val)
#define cpu_to_be16(val) ((((val) & 0xff) << 8) + (((val) >> 8) & 0xff))
#define cpu_to_be32(val) ((cpu_to_be16(val) << 16) + cpu_to_be16((val) >> 16))
typedef long spinlock_t;
#define SPIN_LOCK_UNLOCKED 0
#define spin_lock(lock)
#define spin_unlock(lock)
#define spin_lock_irqsave(lock, flags) do {save_flags(flags); cli();} while(0)
#define spin_unlock_irqrestore(lock, flags) restore_flags(flags)
#endif
#if LINUX_VERSION_CODE <= 0x20139
#define net_device_stats enet_statistics
#else
#define NETSTATS_VER2
#endif
#ifndef SIOCGMIIPHY
#define SIOCGMIIPHY (SIOCDEVPRIVATE)
#define SIOCGMIIREG (SIOCDEVPRIVATE+1)
#define SIOCSMIIREG (SIOCDEVPRIVATE+2)
#endif
#ifndef SIOCGPARAMS
#define SIOCGPARAMS (SIOCDEVPRIVATE+3)
#define SIOCSPARAMS (SIOCDEVPRIVATE+4)
#endif
#if !defined(HAVE_NETIF_MSG)
enum {
NETIF_MSG_DRV = 0x0001,
NETIF_MSG_PROBE = 0x0002,
NETIF_MSG_LINK = 0x0004,
NETIF_MSG_TIMER = 0x0008,
NETIF_MSG_IFDOWN = 0x0010,
NETIF_MSG_IFUP = 0x0020,
NETIF_MSG_RX_ERR = 0x0040,
NETIF_MSG_TX_ERR = 0x0080,
NETIF_MSG_TX_QUEUED = 0x0100,
NETIF_MSG_INTR = 0x0200,
NETIF_MSG_TX_DONE = 0x0400,
NETIF_MSG_RX_STATUS = 0x0800,
NETIF_MSG_PKTDATA = 0x1000,
NETIF_MSG_WOL = 0x4000,
NETIF_MSG_MISC = 0x8000,
NETIF_MSG_RXFILTER = 0x10000,
};
#define NETIF_MSG_MAX 0x10000
#endif
#if !defined(NETIF_MSG_MAX) || NETIF_MSG_MAX < 0x8000
#define NETIF_MSG_MISC 0x8000
#endif
#if !defined(NETIF_MSG_MAX) || NETIF_MSG_MAX < 0x10000
#define NETIF_MSG_RXFILTER 0x10000
#endif
#if LINUX_VERSION_CODE < 0x20155
#include <linux/bios32.h>
#define PCI_SUPPORT_VER1
#if LINUX_VERSION_CODE < 0x20014
struct pci_dev { int not_used; };
#endif
#define pci_find_slot(bus, devfn) (struct pci_dev*)((bus<<8) | devfn | 0xf0000)
#define bus_number(pci_dev) ((((int)(pci_dev))>>8) & 0xff)
#define devfn_number(pci_dev) (((int)(pci_dev)) & 0xff)
#define pci_bus_number(pci_dev) ((((int)(pci_dev))>>8) & 0xff)
#define pci_devfn(pci_dev) (((int)(pci_dev)) & 0xff)
#ifndef CONFIG_PCI
extern inline int pci_present(void) { return 0; }
#else
#define pci_present pcibios_present
#endif
#define pci_read_config_byte(pdev, where, valp)\
pcibios_read_config_byte(bus_number(pdev), devfn_number(pdev), where, valp)
#define pci_read_config_word(pdev, where, valp)\
pcibios_read_config_word(bus_number(pdev), devfn_number(pdev), where, valp)
#define pci_read_config_dword(pdev, where, valp)\
pcibios_read_config_dword(bus_number(pdev), devfn_number(pdev), where, valp)
#define pci_write_config_byte(pdev, where, val)\
pcibios_write_config_byte(bus_number(pdev), devfn_number(pdev), where, val)
#define pci_write_config_word(pdev, where, val)\
pcibios_write_config_word(bus_number(pdev), devfn_number(pdev), where, val)
#define pci_write_config_dword(pdev, where, val)\
pcibios_write_config_dword(bus_number(pdev), devfn_number(pdev), where, val)
#else
#define PCI_SUPPORT_VER2
#define pci_bus_number(pci_dev) ((pci_dev)->bus->number)
#define pci_devfn(pci_dev) ((pci_dev)->devfn)
#endif
#if LINUX_VERSION_CODE < 0x20159
#define dev_free_skb(skb) dev_kfree_skb(skb, FREE_WRITE)
#define dev_free_skb_irq(skb) dev_kfree_skb(skb, FREE_WRITE)
#elif LINUX_VERSION_CODE < 0x20400
#define dev_free_skb(skb) dev_kfree_skb(skb)
#define dev_free_skb_irq(skb) dev_kfree_skb(skb)
#else
#define dev_free_skb(skb) dev_kfree_skb(skb)
#define dev_free_skb_irq(skb) dev_kfree_skb_irq(skb)
#endif
#if LINUX_VERSION_CODE > 0x20153
#include <linux/init.h>
#else
#define __init
#define __initdata
#define __initfunc(__arginit) __arginit
#endif
#if LINUX_VERSION_CODE < 0x2030d
#define net_device device
#endif
#if LINUX_VERSION_CODE < 0x20363
#define DECLARE_MUTEX(name) struct semaphore (name) = MUTEX;
#define down_write(semaphore_p) down(semaphore_p)
#define down_read(semaphore_p) down(semaphore_p)
#define up_write(semaphore_p) up(semaphore_p)
#define up_read(semaphore_p) up(semaphore_p)
#define time_after(a,b) ((long)(b) - (long)(a) < 0)
#define time_before(a,b) ((long)(a) - (long)(b) < 0)
#else
#define get_free_page get_zeroed_page
#endif
#if ! defined(CAP_NET_ADMIN)
#define capable(CAP_XXX) (suser())
#endif
#if ! defined(HAVE_NETIF_QUEUE)
#define netif_wake_queue(dev) do { clear_bit( 0, (void*)&(dev)->tbusy); mark_bh(NET_BH); } while (0)
#define netif_start_tx_queue(dev) do { (dev)->tbusy = 0; dev->start = 1; } while (0)
#define netif_stop_tx_queue(dev) do { (dev)->tbusy = 1; dev->start = 0; } while (0)
#define netif_queue_paused(dev) ((dev)->tbusy != 0)
#define netif_pause_tx_queue(dev) (test_and_set_bit( 0, (void*)&(dev)->tbusy))
#define netif_unpause_tx_queue(dev) do { clear_bit( 0, (void*)&(dev)->tbusy); } while (0)
#define netif_resume_tx_queue(dev) do { clear_bit( 0, (void*)&(dev)->tbusy); mark_bh(NET_BH); } while (0)
#define netif_running(dev) ((dev)->start != 0)
#define netif_device_attach(dev) do {; } while (0)
#define netif_device_detach(dev) do {; } while (0)
#define netif_device_present(dev) (1)
#define netif_set_tx_timeout(dev, func, deltajiffs) do {; } while (0)
#define netif_link_down(dev) (dev)->flags &= ~IFF_RUNNING
#define netif_link_up(dev) (dev)->flags |= IFF_RUNNING
#else
#define netif_start_tx_queue(dev) netif_start_queue(dev)
#define netif_stop_tx_queue(dev) netif_stop_queue(dev)
#define netif_queue_paused(dev) netif_queue_stopped(dev)
#define netif_resume_tx_queue(dev) netif_wake_queue(dev)
#define netif_pause_tx_queue(dev) 0
#define netif_unpause_tx_queue(dev) do {; } while (0)
#ifdef __LINK_STATE_NOCARRIER
#define netif_link_down(dev) netif_carrier_off(dev)
#define netif_link_up(dev) netif_carrier_on(dev)
#else
#define netif_link_down(dev) (dev)->flags &= ~IFF_RUNNING
#define netif_link_up(dev) (dev)->flags |= IFF_RUNNING
#endif
#endif
#ifndef PCI_DMA_BUS_IS_PHYS
#define pci_dma_sync_single(pci_dev, base_addr, extent, tofrom) do {; } while (0)
#define pci_map_single(pci_dev, base_addr, extent, dir) virt_to_bus(base_addr)
#define pci_unmap_single(pci_dev, base_addr, extent, dir) do {; } while (0)
#endif
#endif