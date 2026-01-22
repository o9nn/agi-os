#include <linux/module.h>
#include <linux/version.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/string.h>
#include <linux/timer.h>
#include <linux/errno.h>
#include <linux/ioport.h>
#include <linux/malloc.h>
#include <linux/interrupt.h>
#include <linux/pci.h>
#include <linux/netdevice.h>
#include <linux/bios32.h>
#include <linux/compatmac.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#include <asm/processor.h>
#include <asm/bitops.h>
#include <asm/io.h>
#include <linux/delay.h>
#include <asm/types.h>
#include "sis900.h"
#if LINUX_VERSION_CODE < 0x20159
#define dev_free_skb(skb) dev_kfree_skb (skb, FREE_WRITE);
#else
#define dev_free_skb(skb) dev_kfree_skb(skb);
#endif
static const char *version =
"sis900.c: modified v1.06.11  4/30/2002";
static int max_interrupt_work = 20;
static int multicast_filter_limit = 128;
#define sis900_debug debug
static int sis900_debug = 0;
#define TX_TIMEOUT  (4*HZ)
enum pci_flags_bit {
PCI_USES_IO=1, PCI_USES_MEM=2, PCI_USES_MASTER=4,
PCI_ADDR0=0x10<<0, PCI_ADDR1=0x10<<1, PCI_ADDR2=0x10<<2, PCI_ADDR3=0x10<<3,
};
struct mac_chip_info {
const char *name;
u16	vendor_id, device_id, flags;
int	io_size;
struct device *(*probe) (struct mac_chip_info *mac, long ioaddr, int irq,
int pci_index,  unsigned char pci_device_fn, unsigned char pci_bus, struct device * net_dev);
};
static struct device * sis900_mac_probe (struct mac_chip_info * mac, long ioaddr, int irq,
int pci_index,  unsigned char pci_device_fn,
unsigned char pci_bus, struct device * net_dev);
static struct mac_chip_info  mac_chip_table[] = {
{ "SiS 900 PCI Fast Ethernet", PCI_VENDOR_ID_SI, PCI_DEVICE_ID_SI_900,
PCI_COMMAND_IO|PCI_COMMAND_MASTER, SIS900_TOTAL_SIZE, sis900_mac_probe},
{ "SiS 7016 PCI Fast Ethernet",PCI_VENDOR_ID_SI, PCI_DEVICE_ID_SI_7016,
PCI_COMMAND_IO|PCI_COMMAND_MASTER, SIS900_TOTAL_SIZE, sis900_mac_probe},
{0,},
};
static void sis900_read_mode(struct device *net_dev, int *speed, int *duplex);
static struct mii_chip_info {
const char * name;
u16 phy_id0;
u16 phy_id1;
u8  phy_types;
#define	HOME 	0x0001
#define LAN	0x0002
#define MIX	0x0003
} mii_chip_table[] = {
{ "SiS 900 Internal MII PHY", 		0x001d, 0x8000, LAN },
{ "SiS 7014 Physical Layer Solution", 	0x0016, 0xf830, LAN },
{ "AMD 79C901 10BASE-T PHY",  		0x0000, 0x6B70, LAN },
{ "AMD 79C901 HomePNA PHY",		0x0000, 0x6B90, HOME},
{ "ICS LAN PHY",			0x0015, 0xF440, LAN },
{ "NS  83851 PHY",			0x2000, 0x5C20, MIX },
{ "Realtek RTL8201 PHY",		0x0000, 0x8200, LAN },
{0,},
};
struct mii_phy {
struct mii_phy * next;
int phy_addr;
u16 phy_id0;
u16 phy_id1;
u16 status;
u8  phy_types;
};
typedef struct _BufferDesc {
u32	link;
u32	cmdsts;
u32	bufptr;
} BufferDesc;
struct sis900_private {
struct device *next_module;
struct enet_statistics stats;
unsigned char pci_bus;
unsigned char pci_device_fn;
int pci_index;
struct mac_chip_info * mac;
struct mii_phy * mii;
struct mii_phy * first_mii;
unsigned int cur_phy;
struct timer_list timer;
u8     autong_complete;
unsigned int cur_rx, dirty_rx;
unsigned int cur_tx, dirty_tx;
struct sk_buff *tx_skbuff[NUM_TX_DESC];
struct sk_buff *rx_skbuff[NUM_RX_DESC];
BufferDesc tx_ring[NUM_TX_DESC];
BufferDesc rx_ring[NUM_RX_DESC];
unsigned int tx_full;
int LinkOn;
};
#ifdef MODULE
#if LINUX_VERSION_CODE > 0x20115
MODULE_AUTHOR("Jim Huang <cmhuang@sis.com.tw>, Ollie Lho <ollie@sis.com.tw>");
MODULE_DESCRIPTION("SiS 900 PCI Fast Ethernet driver");
MODULE_PARM(multicast_filter_limit, "i");
MODULE_PARM(max_interrupt_work, "i");
MODULE_PARM(debug, "i");
#endif
#endif
static int sis900_open(struct device *net_dev);
static int sis900_mii_probe (unsigned char pci_bus, unsigned char pci_device_fn, struct device * net_dev);
static void sis900_init_rxfilter (struct device * net_dev);
static u16 read_eeprom(long ioaddr, int location);
static u16 mdio_read(struct device *net_dev, int phy_id, int location);
static void mdio_write(struct device *net_dev, int phy_id, int location, int val);
static void sis900_timer(unsigned long data);
static void sis900_check_mode (struct device *net_dev, struct mii_phy *mii_phy);
static void sis900_tx_timeout(struct device *net_dev);
static void sis900_init_tx_ring(struct device *net_dev);
static void sis900_init_rx_ring(struct device *net_dev);
static int sis900_start_xmit(struct sk_buff *skb, struct device *net_dev);
static int sis900_rx(struct device *net_dev);
static void sis900_finish_xmit (struct device *net_dev);
static void sis900_interrupt(int irq, void *dev_instance, struct pt_regs *regs);
static int sis900_close(struct device *net_dev);
static int mii_ioctl(struct device *net_dev, struct ifreq *rq, int cmd);
static struct enet_statistics *sis900_get_stats(struct device *net_dev);
static u16 sis900_compute_hashtable_index(u8 *addr, u8 revision);
static void set_rx_mode(struct device *net_dev);
static void sis900_reset(struct device *net_dev);
static void sis630_set_eq(struct device *net_dev, u8 revision);
static u16 sis900_default_phy(struct device * net_dev);
static void sis900_set_capability( struct device *net_dev ,struct mii_phy *phy);
static u16 sis900_reset_phy(struct device *net_dev, int phy_addr);
static void sis900_auto_negotiate(struct device *net_dev, int phy_addr);
static void sis900_set_mode (long ioaddr, int speed, int duplex);
static struct device *root_sis900_dev = NULL;
#ifdef HAVE_DEVLIST
struct netdev_entry netcard_drv =
{"sis900", sis900_probe, SIS900_TOTAL_SIZE, NULL};
#endif
int sis900_probe (struct device * net_dev)
{
int found = 0;
int pci_index = 0;
unsigned char pci_bus, pci_device_fn;
long ioaddr;
int irq;
if (!pcibios_present())
return -ENODEV;
for (; pci_index < 0xff; pci_index++)
{
u16 vendor, device, pci_command;
struct mac_chip_info *mac;
if (pcibios_find_class (PCI_CLASS_NETWORK_ETHERNET << 8, pci_index,
&pci_bus, &pci_device_fn) != PCIBIOS_SUCCESSFUL)
break;
pcibios_read_config_word(pci_bus, pci_device_fn, PCI_VENDOR_ID, &vendor);
pcibios_read_config_word(pci_bus, pci_device_fn, PCI_DEVICE_ID, &device);
for (mac = mac_chip_table; mac->vendor_id; mac++)
{
if (vendor == mac->vendor_id && device == mac->device_id) break;
}
if (mac->vendor_id == 0)
continue;
{
u32 pci_ioaddr;
u8 pci_irq_line;
pcibios_read_config_byte(pci_bus, pci_device_fn,
PCI_INTERRUPT_LINE, &pci_irq_line);
pcibios_read_config_dword(pci_bus, pci_device_fn,
PCI_BASE_ADDRESS_0, &pci_ioaddr);
ioaddr = pci_ioaddr & ~3;
irq = pci_irq_line;
if ((mac->flags & PCI_USES_IO) &&
check_region (pci_ioaddr, mac->io_size))
continue;
pcibios_read_config_word(pci_bus, pci_device_fn,
PCI_COMMAND, &pci_command);
{
u8 lat;
pcibios_read_config_byte(pci_bus, pci_device_fn, PCI_LATENCY_TIMER, &lat);
if (lat < 16) {
printk("PCI: Increasing latency timer of device %02x:%02x to 64\n",
pci_bus, pci_device_fn);
pcibios_write_config_byte(pci_bus, pci_device_fn, PCI_LATENCY_TIMER, 64);
}
}
net_dev = mac->probe (mac, ioaddr, irq, pci_index, pci_device_fn, pci_bus, net_dev);
if (net_dev != NULL)
{
found++;
}
net_dev = NULL;
}
}
return found ? 0 : -ENODEV;
}
static int
sis900_get_mac_addr(long ioaddr, struct device *net_dev)
{
u16 signature;
int i;
signature = (u16) read_eeprom(ioaddr, EEPROMSignature);
if (signature == 0xffff || signature == 0x0000) {
printk (KERN_INFO "%s: Error EERPOM read %x\n",
net_dev->name, signature);
return 0;
}
for (i = 0; i < 3; i++)
((u16 *)(net_dev->dev_addr))[i] = read_eeprom(ioaddr, i+EEPROMMACAddr);
return 1;
}
static int sis630e_get_mac_addr(long ioaddr, int pci_index, struct device *net_dev)
{
u8 reg;
int i;
u8 pci_bus, pci_dfn;
int not_found;
not_found = pcibios_find_device(0x1039, 0x0008,
pci_index,
&pci_bus,
&pci_dfn);
if (not_found) {
printk("%s: Can not find ISA bridge\n", net_dev->name);
return 0;
}
pcibios_read_config_byte(pci_bus, pci_dfn, 0x48, &reg);
pcibios_write_config_byte(pci_bus, pci_dfn, 0x48, reg | 0x40);
for (i = 0; i < 6; i++) {
outb(0x09 + i, 0x70);
((u8 *)(net_dev->dev_addr))[i] = inb(0x71);
}
pcibios_write_config_byte(pci_bus, pci_dfn, 0x48, reg & ~0x40);
return 1;
}
static int sis635_get_mac_addr(struct device *net_dev)
{
long ioaddr = net_dev->base_addr;
u32 rfcrSave;
u32 i;
rfcrSave = inl(rfcr + ioaddr);
outl(rfcrSave | RELOAD, ioaddr + cr);
outl(0, ioaddr + cr);
outl(rfcrSave & ~RFEN, rfcr + ioaddr);
for (i = 0 ; i < 3 ; i++) {
outl((i << RFADDR_shift), ioaddr + rfcr);
*( ((u16 *)net_dev->dev_addr) + i) = inw(ioaddr + rfdr);
}
outl(rfcrSave | RFEN, rfcr + ioaddr);
return 1;
}
static int sis962_get_mac_addr(struct device *net_dev)
{
long ioaddr = net_dev->base_addr;
long ee_addr = ioaddr + mear;
u32 waittime = 0;
int i;
outl(EEREQ, ee_addr);
while(waittime < 2000) {
if(inl(ee_addr) & EEGNT) {
for (i = 0; i < 3; i++)
((u16 *)(net_dev->dev_addr))[i] = read_eeprom(ioaddr, i+EEPROMMACAddr);
outl(EEDONE, ee_addr);
return 1;
} else {
udelay(1);
waittime ++;
}
}
outl(EEDONE, ee_addr);
return 0;
}
struct device *
sis900_mac_probe (struct mac_chip_info *mac, long ioaddr, int irq, int pci_index,
unsigned char pci_device_fn, unsigned char pci_bus, struct device * net_dev)
{
struct sis900_private *sis_priv;
static int did_version = 0;
u8 revision;
int i, ret = 0;
if (did_version++ == 0)
printk(KERN_INFO "%s\n", version);
if ((net_dev = init_etherdev(net_dev, 0)) == NULL)
return NULL;
if ((net_dev->priv = kmalloc(sizeof(struct sis900_private), GFP_KERNEL)) == NULL) {
unregister_netdev(net_dev);
return NULL;
}
sis_priv = net_dev->priv;
memset(sis_priv, 0, sizeof(struct sis900_private));
request_region(ioaddr, mac->io_size, net_dev->name);
net_dev->base_addr = ioaddr;
net_dev->irq = irq;
sis_priv->mac = mac;
sis_priv->pci_bus = pci_bus;
sis_priv->pci_device_fn = pci_device_fn;
sis_priv->pci_index = pci_index;
pcibios_read_config_byte(pci_bus, pci_device_fn, PCI_CLASS_REVISION, &revision);
if ( revision == SIS630E_900_REV )
ret = sis630e_get_mac_addr(ioaddr, pci_index, net_dev);
else if ((revision > 0x81) && (revision <= 0x90))
ret = sis635_get_mac_addr(net_dev);
else if (revision == SIS962_900_REV)
ret = sis962_get_mac_addr(net_dev);
else
ret = sis900_get_mac_addr(ioaddr, net_dev);
if (ret == 0) {
unregister_netdev(net_dev);
return NULL;
}
printk(KERN_INFO "%s: %s at %#lx, IRQ %d, ", net_dev->name, mac->name,
ioaddr, irq);
for (i = 0; i < 5; i++)
printk("%2.2x:", (u8)net_dev->dev_addr[i]);
printk("%2.2x.\n", net_dev->dev_addr[i]);
if (revision == SIS630ET_900_REV)
outl(ACCESSMODE | inl(ioaddr + cr), ioaddr + cr);
if (sis900_mii_probe(pci_bus, pci_device_fn, net_dev) == 0) {
unregister_netdev(net_dev);
kfree(sis_priv);
release_region(ioaddr, mac->io_size);
return NULL;
}
sis_priv->next_module = root_sis900_dev;
root_sis900_dev = net_dev;
net_dev->open = &sis900_open;
net_dev->hard_start_xmit = &sis900_start_xmit;
net_dev->stop = &sis900_close;
net_dev->get_stats = &sis900_get_stats;
net_dev->set_multicast_list = &set_rx_mode;
net_dev->do_ioctl = &mii_ioctl;
return net_dev;
}
static int sis900_mii_probe (unsigned char pci_bus, unsigned char pci_device_fn, struct device * net_dev)
{
struct sis900_private * sis_priv = (struct sis900_private *)net_dev->priv;
u16 poll_bit = MII_STAT_LINK, status = 0;
unsigned int timeout = jiffies + 5 * HZ;
int phy_addr;
u8 revision;
sis_priv->mii = NULL;
for (phy_addr = 0; phy_addr < 32; phy_addr++) {
struct mii_phy * mii_phy = NULL;
u16 mii_status;
int i;
for(i=0; i<2; i++)
mii_status = mdio_read(net_dev, phy_addr, MII_STATUS);
if (mii_status == 0xffff || mii_status == 0x0000)
continue;
if ((mii_phy = kmalloc(sizeof(struct mii_phy), GFP_KERNEL)) == NULL) {
printk(KERN_INFO "Cannot allocate mem for struct mii_phy\n");
return 0;
}
mii_phy->phy_id0 = mdio_read(net_dev, phy_addr, MII_PHY_ID0);
mii_phy->phy_id1 = mdio_read(net_dev, phy_addr, MII_PHY_ID1);
mii_phy->phy_addr = phy_addr;
mii_phy->status = mii_status;
mii_phy->next = sis_priv->mii;
sis_priv->mii = mii_phy;
sis_priv->first_mii = mii_phy;
for (i=0; mii_chip_table[i].phy_id1; i++)
if ( ( mii_phy->phy_id0 == mii_chip_table[i].phy_id0 ) &&
( (mii_phy->phy_id1 & 0xFFF0) == mii_chip_table[i].phy_id1 )){
mii_phy->phy_types = mii_chip_table[i].phy_types;
if(mii_chip_table[i].phy_types == MIX)
mii_phy->phy_types =
(mii_status & (MII_STAT_CAN_TX_FDX | MII_STAT_CAN_TX))?LAN:HOME;
printk(KERN_INFO "%s: %s transceiver found at address %d.\n",
net_dev->name, mii_chip_table[i].name, phy_addr);
break;
}
if( !mii_chip_table[i].phy_id1 )
printk(KERN_INFO "%s: Unknown PHY transceiver found at address %d.\n",
net_dev->name, phy_addr);
}
if (sis_priv->mii == NULL) {
printk(KERN_INFO "%s: No MII transceivers found!\n",
net_dev->name);
return 0;
}
sis_priv->mii = NULL;
sis900_default_phy( net_dev );
if( (sis_priv->mii->phy_id0 == 0x001D) &&
( (sis_priv->mii->phy_id1&0xFFF0) == 0x8000) )
status = sis900_reset_phy( net_dev,  sis_priv->cur_phy );
if ((sis_priv->mii->phy_id0 == 0x0015) &&
((sis_priv->mii->phy_id1&0xFFF0) == 0xF440))
mdio_write(net_dev, sis_priv->cur_phy, 0x0018, 0xD200);
if( status & MII_STAT_LINK ){
while (poll_bit)
{
poll_bit ^= (mdio_read(net_dev, sis_priv->cur_phy, MII_STATUS) & poll_bit);
if (jiffies >= timeout)
{
printk(KERN_WARNING "%s: reset phy and link down now\n", net_dev->name);
return -ETIME;
}
}
}
pcibios_read_config_byte(pci_bus, pci_device_fn, PCI_CLASS_REVISION, &revision);
if (revision == SIS630E_900_REV) {
mdio_write(net_dev, sis_priv->cur_phy, MII_ANADV, 0x05e1);
mdio_write(net_dev, sis_priv->cur_phy, MII_CONFIG1, 0x22);
mdio_write(net_dev, sis_priv->cur_phy, MII_CONFIG2, 0xff00);
mdio_write(net_dev, sis_priv->cur_phy, MII_MASK, 0xffc0);
}
if (sis_priv->mii->status & MII_STAT_LINK)
sis_priv->LinkOn = TRUE;
else
sis_priv->LinkOn = FALSE;
return 1;
}
static u16 sis900_default_phy(struct device * net_dev)
{
struct sis900_private * sis_priv = (struct sis900_private *)net_dev->priv;
struct mii_phy *phy = NULL, *phy_home = NULL, *default_phy = NULL;
u16 status;
for( phy=sis_priv->first_mii; phy; phy=phy->next ){
status = mdio_read(net_dev, phy->phy_addr, MII_STATUS);
status = mdio_read(net_dev, phy->phy_addr, MII_STATUS);
if ( (status & MII_STAT_LINK) && !(default_phy) )
default_phy = phy;
else{
status = mdio_read(net_dev, phy->phy_addr, MII_CONTROL);
mdio_write(net_dev, phy->phy_addr, MII_CONTROL,
status | MII_CNTL_AUTO | MII_CNTL_ISOLATE);
if( phy->phy_types == HOME )
phy_home = phy;
}
}
if( (!default_phy) && phy_home )
default_phy = phy_home;
else if(!default_phy)
default_phy = sis_priv->first_mii;
if( sis_priv->mii != default_phy ){
sis_priv->mii = default_phy;
sis_priv->cur_phy = default_phy->phy_addr;
printk(KERN_INFO "%s: Using transceiver found at address %d as default\n", net_dev->name,sis_priv->cur_phy);
}
status = mdio_read(net_dev, sis_priv->cur_phy, MII_CONTROL);
status &= (~MII_CNTL_ISOLATE);
mdio_write(net_dev, sis_priv->cur_phy, MII_CONTROL, status);
status = mdio_read(net_dev, sis_priv->cur_phy, MII_STATUS);
status = mdio_read(net_dev, sis_priv->cur_phy, MII_STATUS);
return status;
}
static void sis900_set_capability( struct device *net_dev , struct mii_phy *phy )
{
u16 cap;
u16 status;
status = mdio_read(net_dev, phy->phy_addr, MII_STATUS);
status = mdio_read(net_dev, phy->phy_addr, MII_STATUS);
cap = MII_NWAY_CSMA_CD |
((phy->status & MII_STAT_CAN_TX_FDX)? MII_NWAY_TX_FDX:0) |
((phy->status & MII_STAT_CAN_TX)    ? MII_NWAY_TX:0) |
((phy->status & MII_STAT_CAN_T_FDX) ? MII_NWAY_T_FDX:0)|
((phy->status & MII_STAT_CAN_T)     ? MII_NWAY_T:0);
mdio_write( net_dev, phy->phy_addr, MII_ANADV, cap );
}
#define eeprom_delay()	inl(ee_addr)
static u16 read_eeprom(long ioaddr, int location)
{
int i;
u16 retval = 0;
long ee_addr = ioaddr + mear;
u32 read_cmd = location | EEread;
outl(0, ee_addr);
eeprom_delay();
outl(EECS, ee_addr);
eeprom_delay();
for (i = 8; i >= 0; i--) {
u32 dataval = (read_cmd & (1 << i)) ? EEDI | EECS : EECS;
outl(dataval, ee_addr);
eeprom_delay();
outl(dataval | EECLK, ee_addr);
eeprom_delay();
}
outb(EECS, ee_addr);
eeprom_delay();
for (i = 16; i > 0; i--) {
outl(EECS, ee_addr);
eeprom_delay();
outl(EECS | EECLK, ee_addr);
eeprom_delay();
retval = (retval << 1) | ((inl(ee_addr) & EEDO) ? 1 : 0);
eeprom_delay();
}
outl(0, ee_addr);
eeprom_delay();
return (retval);
}
#define mdio_delay()	inl(mdio_addr)
static void mdio_idle(long mdio_addr)
{
outl(MDIO | MDDIR, mdio_addr);
mdio_delay();
outl(MDIO | MDDIR | MDC, mdio_addr);
}
static void mdio_reset(long mdio_addr)
{
int i;
for (i = 31; i >= 0; i--) {
outl(MDDIR | MDIO, mdio_addr);
mdio_delay();
outl(MDDIR | MDIO | MDC, mdio_addr);
mdio_delay();
}
return;
}
static u16 mdio_read(struct device *net_dev, int phy_id, int location)
{
long mdio_addr = net_dev->base_addr + mear;
int mii_cmd = MIIread|(phy_id<<MIIpmdShift)|(location<<MIIregShift);
u16 retval = 0;
int i;
mdio_reset(mdio_addr);
mdio_idle(mdio_addr);
for (i = 15; i >= 0; i--) {
int dataval = (mii_cmd & (1 << i)) ? MDDIR | MDIO : MDDIR;
outl(dataval, mdio_addr);
mdio_delay();
outl(dataval | MDC, mdio_addr);
mdio_delay();
}
for (i = 16; i > 0; i--) {
outl(0, mdio_addr);
mdio_delay();
retval = (retval << 1) | ((inl(mdio_addr) & MDIO) ? 1 : 0);
outl(MDC, mdio_addr);
mdio_delay();
}
outl(0x00, mdio_addr);
return retval;
}
static void mdio_write(struct device *net_dev, int phy_id, int location, int value)
{
long mdio_addr = net_dev->base_addr + mear;
int mii_cmd = MIIwrite|(phy_id<<MIIpmdShift)|(location<<MIIregShift);
int i;
mdio_reset(mdio_addr);
mdio_idle(mdio_addr);
for (i = 15; i >= 0; i--) {
int dataval = (mii_cmd & (1 << i)) ? MDDIR | MDIO : MDDIR;
outb(dataval, mdio_addr);
mdio_delay();
outb(dataval | MDC, mdio_addr);
mdio_delay();
}
mdio_delay();
for (i = 15; i >= 0; i--) {
int dataval = (value & (1 << i)) ? MDDIR | MDIO : MDDIR;
outl(dataval, mdio_addr);
mdio_delay();
outl(dataval | MDC, mdio_addr);
mdio_delay();
}
mdio_delay();
for (i = 2; i > 0; i--) {
outb(0, mdio_addr);
mdio_delay();
outb(MDC, mdio_addr);
mdio_delay();
}
outl(0x00, mdio_addr);
return;
}
static u16 sis900_reset_phy(struct device *net_dev, int phy_addr)
{
int i = 0;
u16 status;
while (i++ < 2)
status = mdio_read(net_dev, phy_addr, MII_STATUS);
mdio_write( net_dev, phy_addr, MII_CONTROL, MII_CNTL_RESET );
return status;
}
static int
sis900_open(struct device *net_dev)
{
struct sis900_private *sis_priv = (struct sis900_private *)net_dev->priv;
long ioaddr = net_dev->base_addr;
u8 revision;
sis900_reset(net_dev);
pcibios_read_config_byte(sis_priv->pci_bus, sis_priv->pci_device_fn, PCI_CLASS_REVISION, &revision);
sis630_set_eq(net_dev, revision);
if (request_irq(net_dev->irq, &sis900_interrupt, SA_SHIRQ, net_dev->name, net_dev)) {
return -EAGAIN;
}
MOD_INC_USE_COUNT;
sis900_init_rxfilter(net_dev);
sis900_init_tx_ring(net_dev);
sis900_init_rx_ring(net_dev);
set_rx_mode(net_dev);
net_dev->tbusy = 0;
net_dev->interrupt = 0;
net_dev->start = 1;
sis900_set_mode(ioaddr, HW_SPEED_10_MBPS, FDX_CAPABLE_HALF_SELECTED);
outl((RxSOVR|RxORN|RxERR|RxOK|TxURN|TxERR|TxIDLE), ioaddr + imr);
outl(RxENA | inl(ioaddr + cr), ioaddr + cr);
outl(IE, ioaddr + ier);
sis900_check_mode(net_dev, sis_priv->mii);
init_timer(&sis_priv->timer);
sis_priv->timer.expires = jiffies + HZ;
sis_priv->timer.data = (unsigned long)net_dev;
sis_priv->timer.function = &sis900_timer;
add_timer(&sis_priv->timer);
return 0;
}
static void
sis900_init_rxfilter (struct device * net_dev)
{
long ioaddr = net_dev->base_addr;
u32 rfcrSave;
u32 i;
rfcrSave = inl(rfcr + ioaddr);
outl(rfcrSave & ~RFEN, rfcr + ioaddr);
for (i = 0 ; i < 3 ; i++) {
u32 w;
w = (u32) *((u16 *)(net_dev->dev_addr)+i);
outl((i << RFADDR_shift), ioaddr + rfcr);
outl(w, ioaddr + rfdr);
if (sis900_debug > 2) {
printk(KERN_INFO "%s: Receive Filter Addrss[%d]=%x\n",
net_dev->name, i, inl(ioaddr + rfdr));
}
}
outl(rfcrSave | RFEN, rfcr + ioaddr);
}
static void
sis900_init_tx_ring(struct device *net_dev)
{
struct sis900_private *sis_priv = (struct sis900_private *)net_dev->priv;
long ioaddr = net_dev->base_addr;
int i;
sis_priv->tx_full = 0;
sis_priv->dirty_tx = sis_priv->cur_tx = 0;
for (i = 0; i < NUM_TX_DESC; i++) {
sis_priv->tx_skbuff[i] = NULL;
sis_priv->tx_ring[i].link = (u32) virt_to_bus(&sis_priv->tx_ring[i+1]);
sis_priv->tx_ring[i].cmdsts = 0;
sis_priv->tx_ring[i].bufptr = 0;
}
sis_priv->tx_ring[i-1].link = (u32) virt_to_bus(&sis_priv->tx_ring[0]);
outl(virt_to_bus(&sis_priv->tx_ring[0]), ioaddr + txdp);
if (sis900_debug > 2)
printk(KERN_INFO "%s: TX descriptor register loaded with: %8.8x\n",
net_dev->name, inl(ioaddr + txdp));
}
static void
sis900_init_rx_ring(struct device *net_dev)
{
struct sis900_private *sis_priv = (struct sis900_private *)net_dev->priv;
long ioaddr = net_dev->base_addr;
int i;
sis_priv->cur_rx = 0;
sis_priv->dirty_rx = 0;
for (i = 0; i < NUM_RX_DESC; i++) {
sis_priv->rx_skbuff[i] = NULL;
sis_priv->rx_ring[i].link = (u32) virt_to_bus(&sis_priv->rx_ring[i+1]);
sis_priv->rx_ring[i].cmdsts = 0;
sis_priv->rx_ring[i].bufptr = 0;
}
sis_priv->rx_ring[i-1].link = (u32) virt_to_bus(&sis_priv->rx_ring[0]);
for (i = 0; i < NUM_RX_DESC; i++) {
struct sk_buff *skb;
if ((skb = dev_alloc_skb(RX_BUF_SIZE)) == NULL) {
break;
}
skb->dev = net_dev;
sis_priv->rx_skbuff[i] = skb;
sis_priv->rx_ring[i].cmdsts = RX_BUF_SIZE;
sis_priv->rx_ring[i].bufptr = virt_to_bus(skb->tail);
}
sis_priv->dirty_rx = (unsigned int) (i - NUM_RX_DESC);
outl(virt_to_bus(&sis_priv->rx_ring[0]), ioaddr + rxdp);
if (sis900_debug > 2)
printk(KERN_INFO "%s: RX descriptor register loaded with: %8.8x\n",
net_dev->name, inl(ioaddr + rxdp));
}
static void sis630_set_eq(struct device *net_dev, u8 revision)
{
struct sis900_private *sis_priv = (struct sis900_private *)net_dev->priv;
u16 reg14h, eq_value, max_value=0, min_value=0;
u8 host_bridge_rev;
int i, maxcount=10;
int not_found;
u8 pci_bus, pci_device_fn;
if ( !(revision == SIS630E_900_REV || revision == SIS630EA1_900_REV ||
revision == SIS630A_900_REV || revision ==  SIS630ET_900_REV) )
return;
not_found = pcibios_find_device(SIS630_VENDOR_ID, SIS630_DEVICE_ID,
sis_priv->pci_index,
&pci_bus,
&pci_device_fn);
if (not_found)
pcibios_read_config_byte(pci_bus, pci_device_fn, PCI_CLASS_REVISION, &host_bridge_rev);
if (sis_priv->LinkOn) {
reg14h=mdio_read(net_dev, sis_priv->cur_phy, MII_RESV);
mdio_write(net_dev, sis_priv->cur_phy, MII_RESV, (0x2200 | reg14h) & 0xBFFF);
for (i=0; i < maxcount; i++) {
eq_value=(0x00F8 & mdio_read(net_dev, sis_priv->cur_phy, MII_RESV)) >> 3;
if (i == 0)
max_value=min_value=eq_value;
max_value=(eq_value > max_value) ? eq_value : max_value;
min_value=(eq_value < min_value) ? eq_value : min_value;
}
if (revision == SIS630E_900_REV || revision == SIS630EA1_900_REV ||
revision == SIS630ET_900_REV) {
if (max_value < 5)
eq_value=max_value;
else if (max_value >= 5 && max_value < 15)
eq_value=(max_value == min_value) ? max_value+2 : max_value+1;
else if (max_value >= 15)
eq_value=(max_value == min_value) ? max_value+6 : max_value+5;
}
if (revision == SIS630A_900_REV &&
(host_bridge_rev == SIS630B0 || host_bridge_rev == SIS630B1)) {
if (max_value == 0)
eq_value=3;
else
eq_value=(max_value+min_value+1)/2;
}
reg14h=mdio_read(net_dev, sis_priv->cur_phy, MII_RESV);
reg14h=(reg14h & 0xFF07) | ((eq_value << 3) & 0x00F8);
reg14h=(reg14h | 0x6000) & 0xFDFF;
mdio_write(net_dev, sis_priv->cur_phy, MII_RESV, reg14h);
}
else {
reg14h=mdio_read(net_dev, sis_priv->cur_phy, MII_RESV);
if (revision == SIS630A_900_REV &&
(host_bridge_rev == SIS630B0 || host_bridge_rev == SIS630B1))
mdio_write(net_dev, sis_priv->cur_phy, MII_RESV, (reg14h | 0x2200) & 0xBFFF);
else
mdio_write(net_dev, sis_priv->cur_phy, MII_RESV, (reg14h | 0x2000) & 0xBFFF);
}
return;
}
static void sis900_timer(unsigned long data)
{
struct device *net_dev = (struct device *)data;
struct sis900_private *sis_priv = (struct sis900_private *)net_dev->priv;
struct mii_phy *mii_phy = sis_priv->mii;
static int next_tick = 5*HZ;
u16 status;
u8 revision;
if(!sis_priv->autong_complete){
int speed, duplex = 0;
sis900_read_mode(net_dev, &speed, &duplex);
if(duplex){
sis900_set_mode(net_dev->base_addr, speed, duplex);
pcibios_read_config_byte(sis_priv->pci_bus, sis_priv->pci_device_fn, PCI_CLASS_REVISION, &revision);
sis630_set_eq(net_dev, revision);
}
sis_priv->timer.expires = jiffies + HZ;
add_timer(&sis_priv->timer);
return;
}
status = mdio_read(net_dev, sis_priv->cur_phy, MII_STATUS);
status = mdio_read(net_dev, sis_priv->cur_phy, MII_STATUS);
if ( !sis_priv->LinkOn ) {
LookForLink:
status = sis900_default_phy( net_dev );
mii_phy = sis_priv->mii;
if( status & MII_STAT_LINK ){
sis900_check_mode(net_dev, mii_phy);
sis_priv->LinkOn = TRUE;
}
}
else{
if( !(status & MII_STAT_LINK) ){
sis_priv->LinkOn = FALSE;
printk(KERN_INFO "%s: Media Link Off\n", net_dev->name);
if( (mii_phy->phy_id0 == 0x001D) &&
( (mii_phy->phy_id1 & 0xFFF0) == 0x8000 ))
sis900_reset_phy( net_dev,  sis_priv->cur_phy );
pcibios_read_config_byte(sis_priv->pci_bus, sis_priv->pci_device_fn, PCI_CLASS_REVISION, &revision);
sis630_set_eq(net_dev, revision);
goto LookForLink;
}
}
sis_priv->timer.expires = jiffies + next_tick;
add_timer(&sis_priv->timer);
}
static void sis900_check_mode (struct device *net_dev, struct mii_phy *mii_phy)
{
struct sis900_private *sis_priv = (struct sis900_private *)net_dev->priv;
long ioaddr = net_dev->base_addr;
int speed, duplex;
if( mii_phy->phy_types == LAN  ){
outl( ~EXD & inl( ioaddr + cfg ), ioaddr + cfg);
sis900_set_capability(net_dev , mii_phy);
sis900_auto_negotiate(net_dev, sis_priv->cur_phy);
}else{
outl(EXD | inl( ioaddr + cfg ), ioaddr + cfg);
speed = HW_SPEED_HOME;
duplex = FDX_CAPABLE_HALF_SELECTED;
sis900_set_mode(net_dev->base_addr, speed, duplex);
sis_priv->autong_complete = 1;
}
}
static void sis900_set_mode (long ioaddr, int speed, int duplex)
{
u32 tx_flags = 0, rx_flags = 0;
if( inl(ioaddr + cfg) & EDB_MASTER_EN ){
tx_flags = TxATP | (DMA_BURST_64 << TxMXDMA_shift) | (TX_FILL_THRESH << TxFILLT_shift);
rx_flags = DMA_BURST_64 << RxMXDMA_shift;
}
else{
tx_flags = TxATP | (DMA_BURST_512 << TxMXDMA_shift) | (TX_FILL_THRESH << TxFILLT_shift);
rx_flags = DMA_BURST_512 << RxMXDMA_shift;
}
if (speed == HW_SPEED_HOME || speed == HW_SPEED_10_MBPS ) {
rx_flags |= (RxDRNT_10 << RxDRNT_shift);
tx_flags |= (TxDRNT_10 << TxDRNT_shift);
}
else {
rx_flags |= (RxDRNT_100 << RxDRNT_shift);
tx_flags |= (TxDRNT_100 << TxDRNT_shift);
}
if (duplex == FDX_CAPABLE_FULL_SELECTED) {
tx_flags |= (TxCSI | TxHBI);
rx_flags |= RxATX;
}
outl (tx_flags, ioaddr + txcfg);
outl (rx_flags, ioaddr + rxcfg);
}
static void sis900_auto_negotiate(struct device *net_dev, int phy_addr)
{
struct sis900_private *sis_priv = (struct sis900_private *)net_dev->priv;
int i = 0;
u32 status;
while (i++ < 2)
status = mdio_read(net_dev, phy_addr, MII_STATUS);
if (!(status & MII_STAT_LINK)){
printk(KERN_INFO "%s: Media Link Off\n", net_dev->name);
sis_priv->autong_complete = 1;
sis_priv->LinkOn = FALSE;
return;
}
mdio_write(net_dev, phy_addr, MII_CONTROL,
MII_CNTL_AUTO | MII_CNTL_RST_AUTO);
sis_priv->autong_complete = 0;
}
static void sis900_read_mode(struct device *net_dev, int *speed, int *duplex)
{
struct sis900_private *sis_priv = (struct sis900_private *)net_dev->priv;
struct mii_phy *phy = sis_priv->mii;
int phy_addr = sis_priv->cur_phy;
u32 status;
u16 autoadv, autorec;
int i = 0;
while (i++ < 2)
status = mdio_read(net_dev, phy_addr, MII_STATUS);
if (!(status & MII_STAT_LINK))	return;
autoadv = mdio_read(net_dev, phy_addr, MII_ANADV);
autorec = mdio_read(net_dev, phy_addr, MII_ANLPAR);
status = autoadv & autorec;
*speed = HW_SPEED_10_MBPS;
*duplex = FDX_CAPABLE_HALF_SELECTED;
if (status & (MII_NWAY_TX | MII_NWAY_TX_FDX))
*speed = HW_SPEED_100_MBPS;
if (status & ( MII_NWAY_TX_FDX | MII_NWAY_T_FDX))
*duplex = FDX_CAPABLE_FULL_SELECTED;
sis_priv->autong_complete = 1;
if((phy->phy_id0 == 0x0000) && ((phy->phy_id1 & 0xFFF0) == 0x8200)){
if(mdio_read(net_dev, phy_addr, MII_CONTROL) & MII_CNTL_FDX)
*duplex = FDX_CAPABLE_FULL_SELECTED;
if(mdio_read(net_dev, phy_addr, 0x0019) & 0x01)
*speed = HW_SPEED_100_MBPS;
}
printk(KERN_INFO "%s: Media Link On %s %s-duplex \n",
net_dev->name,
*speed == HW_SPEED_100_MBPS ?
"100mbps" : "10mbps",
*duplex == FDX_CAPABLE_FULL_SELECTED ?
"full" : "half");
}
static void sis900_tx_timeout(struct device *net_dev)
{
struct sis900_private *sis_priv = (struct sis900_private *)net_dev->priv;
long ioaddr = net_dev->base_addr;
int i;
printk(KERN_INFO "%s: Transmit timeout, status %8.8x %8.8x \n",
net_dev->name, inl(ioaddr + cr), inl(ioaddr + isr));
outl(0x0000, ioaddr + imr);
sis_priv->dirty_tx = sis_priv->cur_tx = 0;
for (i = 0; i < NUM_TX_DESC; i++) {
if (sis_priv->tx_skbuff[i] != NULL) {
dev_free_skb(sis_priv->tx_skbuff[i]);
sis_priv->tx_skbuff[i] = 0;
sis_priv->tx_ring[i].cmdsts = 0;
sis_priv->tx_ring[i].bufptr = 0;
sis_priv->stats.tx_dropped++;
}
}
net_dev->trans_start = jiffies;
net_dev->tbusy = sis_priv->tx_full = 0;
outl(TxENA | inl(ioaddr + cr), ioaddr + cr);
outl((RxSOVR|RxORN|RxERR|RxOK|TxURN|TxERR|TxIDLE), ioaddr + imr);
return;
}
static int
sis900_start_xmit(struct sk_buff *skb, struct device *net_dev)
{
struct sis900_private *sis_priv = (struct sis900_private *)net_dev->priv;
long ioaddr = net_dev->base_addr;
unsigned int  entry;
if (test_and_set_bit(0, (void*)&net_dev->tbusy) != 0) {
if (jiffies - net_dev->trans_start > TX_TIMEOUT)
sis900_tx_timeout(net_dev);
return 1;
}
entry = sis_priv->cur_tx % NUM_TX_DESC;
sis_priv->tx_skbuff[entry] = skb;
sis_priv->tx_ring[entry].bufptr = virt_to_bus(skb->data);
sis_priv->tx_ring[entry].cmdsts = (OWN | skb->len);
outl(TxENA | inl(ioaddr + cr), ioaddr + cr);
if (++sis_priv->cur_tx - sis_priv->dirty_tx < NUM_TX_DESC) {
clear_bit(0, (void*)&net_dev->tbusy);
} else {
sis_priv->tx_full = 1;
}
net_dev->trans_start = jiffies;
{
int i;
for (i = 0; i < 100000; i++);
}
if (sis900_debug > 3)
printk(KERN_INFO "%s: Queued Tx packet at %p size %d "
"to slot %d.\n",
net_dev->name, skb->data, (int)skb->len, entry);
return 0;
}
static void sis900_interrupt(int irq, void *dev_instance, struct pt_regs *regs)
{
struct device *net_dev = (struct device *)dev_instance;
int boguscnt = max_interrupt_work;
long ioaddr = net_dev->base_addr;
u32 status;
#if defined(__i386__)
if (test_and_set_bit(0, (void*)&net_dev->interrupt)) {
printk(KERN_INFO "%s: SMP simultaneous entry of "
"an interrupt handler.\n", net_dev->name);
net_dev->interrupt = 0;
return;
}
#else
if (net_dev->interrupt) {
printk(KERN_INFO "%s: Re-entering the interrupt handler.\n",
net_dev->name);
return;
}
net_dev->interrupt = 1;
#endif
do {
status = inl(ioaddr + isr);
if ((status & (HIBERR|TxURN|TxERR|TxIDLE|RxORN|RxERR|RxOK)) == 0)
break;
if (status & (RxORN | RxERR | RxOK))
sis900_rx(net_dev);
if (status & (TxURN | TxERR | TxIDLE))
sis900_finish_xmit(net_dev);
if (status & HIBERR) {
printk(KERN_INFO "%s: Abnormal interrupt,"
"status %#8.8x.\n", net_dev->name, status);
break;
}
if (--boguscnt < 0) {
printk(KERN_INFO "%s: Too much work at interrupt, "
"interrupt status = %#8.8x.\n",
net_dev->name, status);
break;
}
} while (1);
if (sis900_debug > 4)
printk(KERN_INFO "%s: exiting interrupt, "
"interrupt status = 0x%#8.8x.\n",
net_dev->name, inl(ioaddr + isr));
#if defined(__i386__)
clear_bit(0, (void*)&net_dev->interrupt);
#else
net_dev->interrupt = 0;
#endif
return;
}
static int sis900_rx(struct device *net_dev)
{
struct sis900_private *sis_priv = (struct sis900_private *)net_dev->priv;
long ioaddr = net_dev->base_addr;
unsigned int entry = sis_priv->cur_rx % NUM_RX_DESC;
u32 rx_status = sis_priv->rx_ring[entry].cmdsts;
if (sis900_debug > 4)
printk(KERN_INFO "sis900_rx, cur_rx:%4.4d, dirty_rx:%4.4d "
"status:0x%8.8x\n",
sis_priv->cur_rx, sis_priv->dirty_rx, rx_status);
while (rx_status & OWN) {
unsigned int rx_size;
rx_size = (rx_status & DSIZE) - CRC_SIZE;
if (rx_status & (ABORT|OVERRUN|TOOLONG|RUNT|RXISERR|CRCERR|FAERR)) {
if (sis900_debug > 4)
printk(KERN_INFO "%s: Corrupted packet "
"received, buffer status = 0x%8.8x.\n",
net_dev->name, rx_status);
sis_priv->stats.rx_errors++;
if (rx_status & OVERRUN)
sis_priv->stats.rx_over_errors++;
if (rx_status & (TOOLONG|RUNT))
sis_priv->stats.rx_length_errors++;
if (rx_status & (RXISERR | FAERR))
sis_priv->stats.rx_frame_errors++;
if (rx_status & CRCERR)
sis_priv->stats.rx_crc_errors++;
sis_priv->rx_ring[entry].cmdsts = RX_BUF_SIZE;
} else {
struct sk_buff * skb;
if (sis_priv->rx_skbuff[entry] == NULL) {
printk(KERN_INFO "%s: NULL pointer "
"encountered in Rx ring, skipping\n",
net_dev->name);
break;
}
skb = sis_priv->rx_skbuff[entry];
skb_put(skb, rx_size);
skb->protocol = eth_type_trans(skb, net_dev);
netif_rx(skb);
if ((rx_status & BCAST) == MCAST)
sis_priv->stats.multicast++;
net_dev->last_rx = jiffies;
sis_priv->stats.rx_packets++;
if ((skb = dev_alloc_skb(RX_BUF_SIZE)) == NULL) {
printk(KERN_INFO "%s: Memory squeeze,"
"deferring packet.\n",
net_dev->name);
sis_priv->rx_skbuff[entry] = NULL;
sis_priv->rx_ring[entry].cmdsts = 0;
sis_priv->rx_ring[entry].bufptr = 0;
sis_priv->stats.rx_dropped++;
break;
}
skb->dev = net_dev;
sis_priv->rx_skbuff[entry] = skb;
sis_priv->rx_ring[entry].cmdsts = RX_BUF_SIZE;
sis_priv->rx_ring[entry].bufptr = virt_to_bus(skb->tail);
sis_priv->dirty_rx++;
}
sis_priv->cur_rx++;
entry = sis_priv->cur_rx % NUM_RX_DESC;
rx_status = sis_priv->rx_ring[entry].cmdsts;
}
for (;sis_priv->cur_rx - sis_priv->dirty_rx > 0; sis_priv->dirty_rx++) {
struct sk_buff *skb;
entry = sis_priv->dirty_rx % NUM_RX_DESC;
if (sis_priv->rx_skbuff[entry] == NULL) {
if ((skb = dev_alloc_skb(RX_BUF_SIZE)) == NULL) {
printk(KERN_INFO "%s: Memory squeeze,"
"deferring packet.\n",
net_dev->name);
sis_priv->stats.rx_dropped++;
break;
}
skb->dev = net_dev;
sis_priv->rx_skbuff[entry] = skb;
sis_priv->rx_ring[entry].cmdsts = RX_BUF_SIZE;
sis_priv->rx_ring[entry].bufptr = virt_to_bus(skb->tail);
}
}
outl(RxENA | inl(ioaddr + cr), ioaddr + cr );
return 0;
}
static void sis900_finish_xmit (struct device *net_dev)
{
struct sis900_private *sis_priv = (struct sis900_private *)net_dev->priv;
for (; sis_priv->dirty_tx < sis_priv->cur_tx; sis_priv->dirty_tx++) {
unsigned int entry;
u32 tx_status;
entry = sis_priv->dirty_tx % NUM_TX_DESC;
tx_status = sis_priv->tx_ring[entry].cmdsts;
if (tx_status & OWN) {
break;
}
if (tx_status & (ABORT | UNDERRUN | OWCOLL)) {
if (sis900_debug > 4)
printk(KERN_INFO "%s: Transmit "
"error, Tx status %8.8x.\n",
net_dev->name, tx_status);
sis_priv->stats.tx_errors++;
if (tx_status & UNDERRUN)
sis_priv->stats.tx_fifo_errors++;
if (tx_status & ABORT)
sis_priv->stats.tx_aborted_errors++;
if (tx_status & NOCARRIER)
sis_priv->stats.tx_carrier_errors++;
if (tx_status & OWCOLL)
sis_priv->stats.tx_window_errors++;
} else {
sis_priv->stats.collisions += (tx_status & COLCNT) >> 16;
sis_priv->stats.tx_packets++;
}
dev_free_skb(sis_priv->tx_skbuff[entry]);
sis_priv->tx_skbuff[entry] = NULL;
sis_priv->tx_ring[entry].bufptr = 0;
sis_priv->tx_ring[entry].cmdsts = 0;
}
if (sis_priv->tx_full && net_dev->tbusy &&
sis_priv->cur_tx - sis_priv->dirty_tx < NUM_TX_DESC - 4) {
sis_priv->tx_full = 0;
clear_bit(0, (void *)&net_dev->tbusy);
mark_bh(NET_BH);
}
}
static int
sis900_close(struct device *net_dev)
{
long ioaddr = net_dev->base_addr;
struct sis900_private *sis_priv = (struct sis900_private *)net_dev->priv;
int i;
net_dev->start = 0;
net_dev->tbusy = 1;
outl(0x0000, ioaddr + imr);
outl(0x0000, ioaddr + ier);
outl(RxDIS | TxDIS | inl(ioaddr + cr), ioaddr + cr);
del_timer(&sis_priv->timer);
free_irq(net_dev->irq, net_dev);
for (i = 0; i < NUM_RX_DESC; i++) {
if (sis_priv->rx_skbuff[i] != NULL)
dev_free_skb(sis_priv->rx_skbuff[i]);
sis_priv->rx_skbuff[i] = 0;
}
for (i = 0; i < NUM_TX_DESC; i++) {
if (sis_priv->tx_skbuff[i] != NULL)
dev_free_skb(sis_priv->tx_skbuff[i]);
sis_priv->tx_skbuff[i] = 0;
}
MOD_DEC_USE_COUNT;
return 0;
}
static int mii_ioctl(struct device *net_dev, struct ifreq *rq, int cmd)
{
struct sis900_private *sis_priv = (struct sis900_private *)net_dev->priv;
u16 *data = (u16 *)&rq->ifr_data;
switch(cmd) {
case SIOCDEVPRIVATE:
data[0] = sis_priv->mii->phy_addr;
case SIOCDEVPRIVATE+1:
data[3] = mdio_read(net_dev, data[0] & 0x1f, data[1] & 0x1f);
return 0;
case SIOCDEVPRIVATE+2:
if (!suser())
return -EPERM;
mdio_write(net_dev, data[0] & 0x1f, data[1] & 0x1f, data[2]);
return 0;
default:
return -EOPNOTSUPP;
}
}
static struct enet_statistics *
sis900_get_stats(struct device *net_dev)
{
struct sis900_private *sis_priv = (struct sis900_private *)net_dev->priv;
return &sis_priv->stats;
}
static u16 sis900_compute_hashtable_index(u8 *addr, u8 revision)
{
#define POLYNOMIAL 0x04C11DB7L
u32 crc = 0xffffffff, msb;
int  i, j;
u32 byte;
for (i = 0; i < 6; i++) {
byte = *addr++;
for (j = 0; j < 8; j++) {
msb = crc >> 31;
crc <<= 1;
if (msb ^ (byte & 1)) {
crc ^= POLYNOMIAL;
}
byte >>= 1;
}
}
if((revision >= SIS635A_900_REV) || (revision == SIS900B_900_REV))
return ((int)(crc >> 24));
else
return ((int)(crc >> 25));
}
static void set_rx_mode(struct device *net_dev)
{
long ioaddr = net_dev->base_addr;
struct sis900_private * sis_priv = (struct sis900_private *)net_dev->priv;
u16 mc_filter[16] = {0};
int i, table_entries;
u32 rx_mode;
u8 revision;
pcibios_read_config_byte(sis_priv->pci_bus, sis_priv->pci_device_fn, PCI_CLASS_REVISION, &revision);
if((revision >= SIS635A_900_REV) || (revision == SIS900B_900_REV))
table_entries = 16;
else
table_entries = 8;
if (net_dev->flags & IFF_PROMISC) {
rx_mode = RFPromiscuous;
for (i = 0; i < table_entries; i++)
mc_filter[i] = 0xffff;
} else if ((net_dev->mc_count > multicast_filter_limit) ||
(net_dev->flags & IFF_ALLMULTI)) {
rx_mode = RFAAB | RFAAM;
for (i = 0; i < table_entries; i++)
mc_filter[i] = 0xffff;
} else {
struct dev_mc_list *mclist;
rx_mode = RFAAB;
for (i = 0, mclist = net_dev->mc_list; mclist && i < net_dev->mc_count;
i++, mclist = mclist->next)
set_bit(sis900_compute_hashtable_index(mclist->dmi_addr, revision),
mc_filter);
}
for (i = 0; i < table_entries; i++) {
outl((u32)(0x00000004+i) << RFADDR_shift, ioaddr + rfcr);
outl(mc_filter[i], ioaddr + rfdr);
}
outl(RFEN | rx_mode, ioaddr + rfcr);
if (net_dev->flags & IFF_LOOPBACK) {
u32 cr_saved;
cr_saved = inl(ioaddr + cr);
outl(cr_saved | TxDIS | RxDIS, ioaddr + cr);
outl(inl(ioaddr + txcfg) | TxMLB, ioaddr + txcfg);
outl(inl(ioaddr + rxcfg) | RxATX, ioaddr + rxcfg);
outl(cr_saved, ioaddr + cr);
}
return;
}
static void sis900_reset(struct device *net_dev)
{
long ioaddr = net_dev->base_addr;
struct sis900_private *sis_priv = (struct sis900_private *)net_dev->priv;
int i = 0;
u8 revision;
u32 status = TxRCMP | RxRCMP;
outl(0, ioaddr + ier);
outl(0, ioaddr + imr);
outl(0, ioaddr + rfcr);
outl(RxRESET | TxRESET | RESET | inl(ioaddr + cr), ioaddr + cr);
while (status && (i++ < 1000)) {
status ^= (inl(isr + ioaddr) & status);
}
pcibios_read_config_byte(sis_priv->pci_bus, sis_priv->pci_device_fn, PCI_CLASS_REVISION, &revision);
if( (revision >= SIS635A_900_REV) || (revision == SIS900B_900_REV) )
outl(PESEL | RND_CNT, ioaddr + cfg);
else
outl(PESEL, ioaddr + cfg);
}
#ifdef MODULE
int init_module(void)
{
return sis900_probe(NULL);
}
void
cleanup_module(void)
{
while (root_sis900_dev) {
struct sis900_private *sis_priv =
(struct sis900_private *)root_sis900_dev->priv;
struct device *next_dev = sis_priv->next_module;
struct mii_phy *phy = NULL;
while(sis_priv->first_mii){
phy = sis_priv->first_mii;
sis_priv->first_mii = phy->next;
kfree(phy);
}
unregister_netdev(root_sis900_dev);
release_region(root_sis900_dev->base_addr,
sis_priv->mac->io_size);
kfree(sis_priv);
kfree(root_sis900_dev);
root_sis900_dev = next_dev;
}
}
#endif