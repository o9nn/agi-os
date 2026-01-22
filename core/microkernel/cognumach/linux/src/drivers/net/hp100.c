#define HP100_DEFAULT_PRIORITY_TX 0
#undef HP100_DEBUG
#undef HP100_DEBUG_B
#undef HP100_DEBUG_BM
#undef HP100_DEBUG_TRAINING
#undef HP100_DEBUG_TX
#undef HP100_DEBUG_IRQ
#undef HP100_DEBUG_RX
#undef HP100_MULTICAST_FILTER
#include <linux/version.h>
#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/string.h>
#include <linux/errno.h>
#include <linux/ioport.h>
#include <linux/malloc.h>
#include <linux/interrupt.h>
#include <linux/pci.h>
#include <linux/bios32.h>
#include <asm/bitops.h>
#include <asm/io.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#include <linux/types.h>
#include <linux/config.h>
#include <linux/delay.h>
#if LINUX_VERSION_CODE < 0x020100
#define ioremap vremap
#define iounmap vfree
typedef struct enet_statistics hp100_stats_t;
#else
#define LINUX_2_1
typedef struct net_device_stats hp100_stats_t;
#endif
#ifndef __initfunc
#define __initfunc(__initarg) __initarg
#else
#include <linux/init.h>
#endif
#include "hp100.h"
#define HP100_BUS_ISA     0
#define HP100_BUS_EISA    1
#define HP100_BUS_PCI     2
#ifndef PCI_DEVICE_ID_HP_J2585B
#define PCI_DEVICE_ID_HP_J2585B 0x1031
#endif
#ifndef PCI_VENDOR_ID_COMPEX
#define PCI_VENDOR_ID_COMPEX 0x11f6
#endif
#ifndef PCI_DEVICE_ID_COMPEX_ENET100VG4
#define PCI_DEVICE_ID_COMPEX_ENET100VG4 0x0112
#endif
#ifndef PCI_VENDOR_ID_COMPEX2
#define PCI_VENDOR_ID_COMPEX2 0x101a
#endif
#ifndef PCI_DEVICE_ID_COMPEX2_100VG
#define PCI_DEVICE_ID_COMPEX2_100VG 0x0005
#endif
#define HP100_REGION_SIZE  0x20
#define HP100_MAX_PACKET_SIZE  (1536+4)
#define HP100_MIN_PACKET_SIZE  60
#ifndef HP100_DEFAULT_RX_RATIO
#define HP100_DEFAULT_RX_RATIO  75
#endif
#ifndef HP100_DEFAULT_PRIORITY_TX
#define HP100_DEFAULT_PRIORITY_TX 0
#endif
struct hp100_eisa_id {
u_int id;
const char *name;
u_char bus;
};
struct hp100_pci_id {
u_short vendor;
u_short device;
};
struct hp100_private {
struct hp100_eisa_id *id;
u_short chip;
u_short soft_model;
u_int memory_size;
u_int virt_memory_size;
u_short rx_ratio;
u_short priority_tx;
u_short mode;
u_char bus;
u_char pci_bus;
u_char pci_device_fn;
short mem_mapped;
u_int *mem_ptr_virt;
u_int *mem_ptr_phys;
short lan_type;
int hub_status;
u_char mac1_mode;
u_char mac2_mode;
u_char hash_bytes[ 8 ];
hp100_stats_t stats;
hp100_ring_t *rxrhead;
hp100_ring_t *rxrtail;
hp100_ring_t *txrhead;
hp100_ring_t *txrtail;
hp100_ring_t rxring[ MAX_RX_PDL ];
hp100_ring_t txring[ MAX_TX_PDL ];
u_int *page_vaddr;
u_int *page_vaddr_algn;
int rxrcommit;
int txrcommit;
};
static struct hp100_eisa_id hp100_eisa_ids[] = {
{ 0x80F1F022, "HP J2577 rev A", HP100_BUS_EISA },
{ 0x50F1F022, "HP J2573 rev A", HP100_BUS_ISA },
{ 0x2019F022, "HP 27248B",      HP100_BUS_EISA },
{ 0x4019F022, "HP J2577",       HP100_BUS_EISA },
{ 0x5019F022, "HP J2573",       HP100_BUS_ISA },
{ 0x1030103c, "HP J2585A", 	    HP100_BUS_PCI },
{ 0x1041103c, "HP J2585B",      HP100_BUS_PCI },
{ 0x1042103c, "HP J2970",       HP100_BUS_PCI },
{ 0x1040103c, "HP J2973",       HP100_BUS_PCI },
{ 0x0103180e, "ReadyLink ENET100-VG4", HP100_BUS_EISA },
{ 0x0104180e, "FreedomLine 100/VG", HP100_BUS_EISA },
{ 0x021211f6, "FreedomLine 100/VG", HP100_BUS_PCI },
{ 0x011211f6, "ReadyLink ENET100-VG4", HP100_BUS_PCI }
};
#define HP100_EISA_IDS_SIZE	(sizeof(hp100_eisa_ids)/sizeof(struct hp100_eisa_id))
static struct hp100_pci_id hp100_pci_ids[] = {
{ PCI_VENDOR_ID_HP, 		PCI_DEVICE_ID_HP_J2585A },
{ PCI_VENDOR_ID_HP,		PCI_DEVICE_ID_HP_J2585B },
{ PCI_VENDOR_ID_COMPEX,	PCI_DEVICE_ID_COMPEX_ENET100VG4 },
{ PCI_VENDOR_ID_COMPEX2,	PCI_DEVICE_ID_COMPEX2_100VG }
};
#define HP100_PCI_IDS_SIZE	(sizeof(hp100_pci_ids)/sizeof(struct hp100_pci_id))
static int hp100_rx_ratio = HP100_DEFAULT_RX_RATIO;
static int hp100_priority_tx = HP100_DEFAULT_PRIORITY_TX;
static int hp100_mode = 1;
#ifdef LINUX_2_1
MODULE_PARM( hp100_rx_ratio, "1i" );
MODULE_PARM( hp100_priority_tx, "1i" );
MODULE_PARM( hp100_mode, "1i" );
#endif
static int  hp100_probe1( struct device *dev, int ioaddr, u_char bus, u_char pci_bus, u_char pci_device_fn  );
static int  hp100_open( struct device *dev );
static int  hp100_close( struct device *dev );
static int  hp100_start_xmit( struct sk_buff *skb, struct device *dev );
static int  hp100_start_xmit_bm (struct sk_buff *skb, struct device *dev );
static void hp100_rx( struct device *dev );
static hp100_stats_t *hp100_get_stats( struct device *dev );
static void hp100_misc_interrupt( struct device *dev );
static void hp100_update_stats( struct device *dev );
static void hp100_clear_stats( int ioaddr );
static void hp100_set_multicast_list( struct device *dev);
static void hp100_interrupt( int irq, void *dev_id, struct pt_regs *regs );
static void hp100_start_interface( struct device *dev );
static void hp100_stop_interface( struct device *dev );
static void hp100_load_eeprom( struct device *dev, u_short ioaddr );
static int  hp100_sense_lan( struct device *dev );
static int  hp100_login_to_vg_hub( struct device *dev, u_short force_relogin );
static int  hp100_down_vg_link( struct device *dev );
static void hp100_cascade_reset( struct device *dev, u_short enable );
static void hp100_BM_shutdown( struct device *dev );
static void hp100_mmuinit( struct device *dev );
static void hp100_init_pdls( struct device *dev );
static int  hp100_init_rxpdl( struct device *dev, register hp100_ring_t *ringptr, register u_int *pdlptr);
static int  hp100_init_txpdl( struct device *dev, register hp100_ring_t *ringptr, register u_int *pdlptr);
static void hp100_rxfill( struct device *dev );
static void hp100_hwinit( struct device *dev );
static void hp100_clean_txring( struct device *dev );
#ifdef HP100_DEBUG
static void hp100_RegisterDump( struct device *dev );
#endif
static void wait( void )
{
udelay( 1000 );
}
__initfunc(int hp100_probe( struct device *dev ))
{
int base_addr = dev ? dev -> base_addr : 0;
int ioaddr = 0;
#ifdef CONFIG_PCI
int pci_start_index = 0;
#endif
#ifdef HP100_DEBUG_B
hp100_outw( 0x4200, TRACE );
printk( "hp100: %s: probe\n", dev->name );
#endif
if ( base_addr > 0xff )
{
if ( check_region( base_addr, HP100_REGION_SIZE ) ) return -EINVAL;
if ( base_addr < 0x400 )
return hp100_probe1( dev, base_addr, HP100_BUS_ISA, 0, 0 );
if ( EISA_bus && base_addr >= 0x1c38 && ( (base_addr - 0x1c38) & 0x3ff ) == 0 )
return hp100_probe1( dev, base_addr, HP100_BUS_EISA, 0, 0 );
#ifdef CONFIG_PCI
printk( "hp100: %s: You may specify card # in i/o address parameter for PCI bus...", dev->name );
return hp100_probe1( dev, base_addr, HP100_BUS_PCI, 0, 0 );
#else
return -ENODEV;
#endif
}
else
#ifdef CONFIG_PCI
if ( base_addr > 0 && base_addr < 8 + 1 )
pci_start_index = 0x100 | ( base_addr - 1 );
else
#endif
if ( base_addr != 0 ) return -ENXIO;
#ifdef CONFIG_PCI
if ( pcibios_present() )
{
int pci_index;
#ifdef HP100_DEBUG_PCI
printk( "hp100: %s: PCI BIOS is present, checking for devices..\n", dev->name );
#endif
for ( pci_index = pci_start_index & 7; pci_index < 8; pci_index++ )
{
u_char pci_bus, pci_device_fn;
u_short pci_command;
int pci_id_index;
for ( pci_id_index = 0; pci_id_index < HP100_PCI_IDS_SIZE; pci_id_index++ )
if ( pcibios_find_device( hp100_pci_ids[ pci_id_index ].vendor,
hp100_pci_ids[ pci_id_index ].device,
pci_index, &pci_bus,
&pci_device_fn ) == 0 ) goto __pci_found;
break;
__pci_found:
pcibios_read_config_dword( pci_bus, pci_device_fn,
PCI_BASE_ADDRESS_0, &ioaddr );
ioaddr &= ~3;
if ( check_region( ioaddr, HP100_REGION_SIZE ) ) continue;
pcibios_read_config_word( pci_bus, pci_device_fn,
PCI_COMMAND, &pci_command );
if ( !( pci_command & PCI_COMMAND_IO ) )
{
#ifdef HP100_DEBUG
printk( "hp100: %s: PCI I/O Bit has not been set. Setting...\n", dev->name );
#endif
pci_command |= PCI_COMMAND_IO;
pcibios_write_config_word( pci_bus, pci_device_fn,
PCI_COMMAND, pci_command );
}
if ( !( pci_command & PCI_COMMAND_MASTER ) )
{
#ifdef HP100_DEBUG
printk( "hp100: %s: PCI Master Bit has not been set. Setting...\n", dev->name );
#endif
pci_command |= PCI_COMMAND_MASTER;
pcibios_write_config_word( pci_bus, pci_device_fn,
PCI_COMMAND, pci_command );
}
#ifdef HP100_DEBUG
printk( "hp100: %s: PCI adapter found at 0x%x\n", dev->name, ioaddr );
#endif
if ( hp100_probe1( dev, ioaddr, HP100_BUS_PCI, pci_bus, pci_device_fn ) == 0 )
return 0;
}
}
if ( pci_start_index > 0 ) return -ENODEV;
#endif
for ( ioaddr = 0x1c38; EISA_bus && ioaddr < 0x10000; ioaddr += 0x400 )
{
if ( check_region( ioaddr, HP100_REGION_SIZE ) ) continue;
if ( hp100_probe1( dev, ioaddr, HP100_BUS_EISA, 0, 0 ) == 0 ) return 0;
}
for ( ioaddr = 0x100; ioaddr < 0x400; ioaddr += 0x20 )
{
if ( check_region( ioaddr, HP100_REGION_SIZE ) ) continue;
if ( hp100_probe1( dev, ioaddr, HP100_BUS_ISA, 0, 0 ) == 0 ) return 0;
}
return -ENODEV;
}
__initfunc(static int hp100_probe1( struct device *dev, int ioaddr, u_char bus, u_char pci_bus, u_char pci_device_fn ))
{
int i;
u_char uc, uc_1;
u_int eisa_id;
u_int chip;
u_int memory_size = 0, virt_memory_size = 0;
u_short local_mode, lsw;
short mem_mapped;
u_int *mem_ptr_phys, *mem_ptr_virt;
struct hp100_private *lp;
struct hp100_eisa_id *eid;
#ifdef HP100_DEBUG_B
hp100_outw( 0x4201, TRACE );
printk("hp100: %s: probe1\n",dev->name);
#endif
if ( dev == NULL )
{
#ifdef HP100_DEBUG
printk( "hp100_probe1: %s: dev == NULL ?\n", dev->name );
#endif
return EIO;
}
if ( hp100_inw( HW_ID ) != HP100_HW_ID_CASCADE )
{
return -ENODEV;
}
else
{
chip = hp100_inw( PAGING ) & HP100_CHIPID_MASK;
#ifdef HP100_DEBUG
if ( chip == HP100_CHIPID_SHASTA )
printk("hp100: %s: Shasta Chip detected. (This is a pre 802.12 chip)\n", dev->name);
else if ( chip == HP100_CHIPID_RAINIER )
printk("hp100: %s: Rainier Chip detected. (This is a pre 802.12 chip)\n", dev->name);
else if ( chip == HP100_CHIPID_LASSEN )
printk("hp100: %s: Lassen Chip detected.\n", dev->name);
else
printk("hp100: %s: Warning: Unknown CASCADE chip (id=0x%.4x).\n",dev->name,chip);
#endif
}
dev->base_addr = ioaddr;
hp100_page( ID_MAC_ADDR );
for ( i = uc = eisa_id = 0; i < 4; i++ )
{
eisa_id >>= 8;
uc_1 = hp100_inb( BOARD_ID + i );
eisa_id |= uc_1 << 24;
uc += uc_1;
}
uc += hp100_inb( BOARD_ID + 4 );
if ( uc != 0xff )
{
printk("hp100_probe: %s: bad EISA ID checksum at base port 0x%x\n", dev->name, ioaddr );
return -ENODEV;
}
for ( i=0; i < HP100_EISA_IDS_SIZE; i++)
if ( hp100_eisa_ids[ i ].id == eisa_id )
break;
if ( i >= HP100_EISA_IDS_SIZE ) {
for ( i = 0; i < HP100_EISA_IDS_SIZE; i++)
if ( ( hp100_eisa_ids[ i ].id & 0xf0ffffff ) == ( eisa_id & 0xf0ffffff ) )
break;
if ( i >= HP100_EISA_IDS_SIZE ) {
printk( "hp100_probe: %s: card at port 0x%x isn't known (id = 0x%x)\n", dev -> name, ioaddr, eisa_id );
return -ENODEV;
}
}
eid = &hp100_eisa_ids[ i ];
if ( ( eid->id & 0x0f000000 ) < ( eisa_id & 0x0f000000 ) )
{
printk( "hp100_probe: %s: newer version of card %s at port 0x%x - unsupported\n",
dev->name, eid->name, ioaddr );
return -ENODEV;
}
for ( i = uc = 0; i < 7; i++ )
uc += hp100_inb( LAN_ADDR + i );
if ( uc != 0xff )
{
printk("hp100_probe: %s: bad lan address checksum (card %s at port 0x%x)\n",
dev->name, eid->name, ioaddr );
return -EIO;
}
hp100_load_eeprom( dev, ioaddr );
wait();
#if 0
local_mode = 0x2270;
hp100_outw(0xfefe,OPTION_LSW);
hp100_outw(local_mode|HP100_SET_LB|HP100_SET_HB,OPTION_LSW);
#endif
local_mode=hp100_mode;
if ( local_mode < 1 || local_mode > 4 )
local_mode = 1;
#ifdef HP100_DEBUG
printk( "hp100: %s: original LSW = 0x%x\n", dev->name, hp100_inw(OPTION_LSW) );
#endif
if(local_mode==3)
{
hp100_outw(HP100_MEM_EN|HP100_RESET_LB, OPTION_LSW);
hp100_outw(HP100_IO_EN|HP100_SET_LB, OPTION_LSW);
hp100_outw(HP100_BM_WRITE|HP100_BM_READ|HP100_RESET_HB, OPTION_LSW);
printk("hp100: %s: IO mapped mode forced.\n", dev->name);
}
else if(local_mode==2)
{
hp100_outw(HP100_MEM_EN|HP100_SET_LB, OPTION_LSW);
hp100_outw(HP100_IO_EN |HP100_SET_LB, OPTION_LSW);
hp100_outw(HP100_BM_WRITE|HP100_BM_READ|HP100_RESET_HB, OPTION_LSW);
printk("hp100: %s: Shared memory mode requested.\n", dev->name);
}
else if(local_mode==4)
{
if(chip==HP100_CHIPID_LASSEN)
{
hp100_outw(HP100_BM_WRITE|
HP100_BM_READ | HP100_SET_HB, OPTION_LSW);
hp100_outw(HP100_IO_EN   |
HP100_MEM_EN  | HP100_RESET_LB, OPTION_LSW);
printk("hp100: %s: Busmaster mode requested.\n",dev->name);
}
local_mode=1;
}
if(local_mode==1)
{
lsw = hp100_inw(OPTION_LSW);
if ( (lsw & HP100_IO_EN) &&
(~lsw & HP100_MEM_EN) &&
(~lsw & (HP100_BM_WRITE|HP100_BM_READ)) )
{
#ifdef HP100_DEBUG
printk("hp100: %s: IO_EN bit is set on card.\n",dev->name);
#endif
local_mode=3;
}
else if ( chip == HP100_CHIPID_LASSEN &&
( lsw & (HP100_BM_WRITE|HP100_BM_READ) ) ==
(HP100_BM_WRITE|HP100_BM_READ) )
{
printk("hp100: %s: Busmaster mode enabled.\n",dev->name);
hp100_outw(HP100_MEM_EN|HP100_IO_EN|HP100_RESET_LB, OPTION_LSW);
}
else
{
#ifdef HP100_DEBUG
printk("hp100: %s: Card not configured for BM or BM not supported with this card.\n", dev->name );
printk("hp100: %s: Trying shared memory mode.\n", dev->name);
#endif
local_mode=2;
hp100_outw(HP100_MEM_EN|HP100_SET_LB, OPTION_LSW);
}
}
#ifdef HP100_DEBUG
printk( "hp100: %s: new LSW = 0x%x\n", dev->name, hp100_inw(OPTION_LSW) );
#endif
hp100_page( HW_MAP );
mem_mapped = (( hp100_inw( OPTION_LSW ) & ( HP100_MEM_EN ) ) != 0);
mem_ptr_phys = mem_ptr_virt = NULL;
memory_size = (8192<<( (hp100_inb(SRAM)>>5)&0x07));
virt_memory_size = 0;
if ( mem_mapped || (local_mode==1))
{
mem_ptr_phys = (u_int *)( hp100_inw( MEM_MAP_LSW ) |
( hp100_inw( MEM_MAP_MSW ) << 16 ) );
mem_ptr_phys = (u_int *) ((u_int) mem_ptr_phys & ~0x1fff);
if ( bus == HP100_BUS_ISA && ( (u_long)mem_ptr_phys & ~0xfffff ) != 0 )
{
printk("hp100: %s: Can only use programmed i/o mode.\n", dev->name);
mem_ptr_phys = NULL;
mem_mapped = 0;
local_mode=3;
}
if(local_mode!=1)
{
if ( bus == HP100_BUS_PCI && mem_ptr_phys >= (u_int *)0x100000 )
{
for(virt_memory_size = memory_size; virt_memory_size>16383; virt_memory_size>>=1)
{
if((mem_ptr_virt=ioremap((u_long)mem_ptr_phys,virt_memory_size))==NULL)
{
#ifdef HP100_DEBUG
printk( "hp100: %s: ioremap for 0x%x bytes high PCI memory at 0x%lx failed\n", dev->name, virt_memory_size, (u_long)mem_ptr_phys );
#endif
}
else
{
#ifdef HP100_DEBUG
printk( "hp100: %s: remapped 0x%x bytes high PCI memory at 0x%lx to 0x%lx.\n", dev->name, virt_memory_size, (u_long)mem_ptr_phys, (u_long)mem_ptr_virt);
#endif
break;
}
}
if(mem_ptr_virt==NULL)
{
printk("hp100: %s: Failed to ioremap the PCI card memory. Will have to use i/o mapped mode.\n", dev->name);
local_mode=3;
virt_memory_size = 0;
}
}
}
}
if(local_mode==3)
{
mem_mapped = 0;
mem_ptr_phys = mem_ptr_virt = NULL;
printk("hp100: %s: Using (slow) programmed i/o mode.\n", dev->name);
}
if ( (dev->priv=kmalloc(sizeof(struct hp100_private), GFP_KERNEL)) == NULL)
return -ENOMEM;
memset( dev->priv, 0, sizeof(struct hp100_private) );
lp = (struct hp100_private *)dev->priv;
lp->id = eid;
lp->chip = chip;
lp->mode = local_mode;
lp->pci_bus = pci_bus;
lp->bus = bus;
lp->pci_device_fn = pci_device_fn;
lp->priority_tx = hp100_priority_tx;
lp->rx_ratio = hp100_rx_ratio;
lp->mem_ptr_phys = mem_ptr_phys;
lp->mem_ptr_virt = mem_ptr_virt;
hp100_page( ID_MAC_ADDR );
lp->soft_model = hp100_inb( SOFT_MODEL );
lp->mac1_mode = HP100_MAC1MODE3;
lp->mac2_mode = HP100_MAC2MODE3;
memset( &lp->hash_bytes, 0x00, 8 );
dev->base_addr = ioaddr;
lp->memory_size = memory_size;
lp->virt_memory_size = virt_memory_size;
lp->rx_ratio = hp100_rx_ratio;
request_region( dev->base_addr, HP100_REGION_SIZE, eid->name );
dev->open = hp100_open;
dev->stop = hp100_close;
if (lp->mode==1)
dev->hard_start_xmit = hp100_start_xmit_bm;
else
dev->hard_start_xmit = hp100_start_xmit;
dev->get_stats = hp100_get_stats;
dev->set_multicast_list = &hp100_set_multicast_list;
hp100_page( HW_MAP );
dev->irq = hp100_inb( IRQ_CHANNEL ) & HP100_IRQMASK;
if ( dev->irq == 2 )
dev->irq = 9;
if(lp->mode==1)
dev->dma=4;
hp100_page( ID_MAC_ADDR );
for ( i = uc = 0; i < 6; i++ )
dev->dev_addr[ i ] = hp100_inb( LAN_ADDR + i );
hp100_clear_stats( ioaddr );
ether_setup( dev );
if(lp->mode==1)
{
if ( (lp->page_vaddr=kmalloc(MAX_RINGSIZE+0x0f,GFP_KERNEL) ) == NULL)
return -ENOMEM;
lp->page_vaddr_algn=((u_int *) ( ((u_int)(lp->page_vaddr)+0x0f) &~0x0f));
memset(lp->page_vaddr, 0, MAX_RINGSIZE+0x0f);
#ifdef HP100_DEBUG_BM
printk("hp100: %s: Reserved DMA memory from 0x%x to 0x%x\n",
dev->name,
(u_int)lp->page_vaddr_algn,
(u_int)lp->page_vaddr_algn+MAX_RINGSIZE);
#endif
lp->rxrcommit  = lp->txrcommit = 0;
lp->rxrhead    = lp->rxrtail   = &(lp->rxring[0]);
lp->txrhead    = lp->txrtail   = &(lp->txring[0]);
}
hp100_hwinit( dev );
lp->lan_type = hp100_sense_lan( dev );
printk( "hp100: %s: %s at 0x%x, IRQ %d, ",
dev->name, lp->id->name, ioaddr, dev->irq );
switch ( bus ) {
case HP100_BUS_EISA: printk( "EISA" ); break;
case HP100_BUS_PCI:  printk( "PCI" );  break;
default:     printk( "ISA" );  break;
}
printk( " bus, %dk SRAM (rx/tx %d%%).\n",
lp->memory_size >> 10, lp->rx_ratio );
if ( lp->mode==2 )
{
printk( "hp100: %s: Memory area at 0x%lx-0x%lx",
dev->name,(u_long)mem_ptr_phys,
((u_long)mem_ptr_phys+(mem_ptr_phys>(u_int *)0x100000?(u_long)lp->memory_size:16*1024))-1 );
if ( mem_ptr_virt )
printk( " (virtual base 0x%lx)", (u_long)mem_ptr_virt );
printk( ".\n" );
dev->mem_start = (u_long)mem_ptr_phys;
dev->mem_end = (u_long)mem_ptr_phys+(u_long)lp->memory_size;
}
printk( "hp100: %s: ", dev->name );
if ( lp->lan_type != HP100_LAN_ERR )
printk( "Adapter is attached to " );
switch ( lp->lan_type ) {
case HP100_LAN_100:
printk( "100Mb/s Voice Grade AnyLAN network.\n" );
break;
case HP100_LAN_10:
printk( "10Mb/s network.\n" );
break;
default:
printk( "Warning! Link down.\n" );
}
return 0;
}
static void hp100_hwinit( struct device *dev )
{
int ioaddr = dev->base_addr;
struct hp100_private *lp = (struct hp100_private *)dev->priv;
#ifdef HP100_DEBUG_B
hp100_outw( 0x4202, TRACE );
printk("hp100: %s: hwinit\n", dev->name);
#endif
hp100_page( PERFORMANCE );
hp100_outw( 0xfefe, IRQ_MASK );
hp100_outw( 0xffff, IRQ_STATUS );
hp100_outw( HP100_INT_EN | HP100_RESET_LB, OPTION_LSW );
hp100_outw( HP100_TRI_INT | HP100_SET_HB, OPTION_LSW );
if(lp->mode==1)
{
hp100_BM_shutdown( dev );
wait();
}
else
{
hp100_outw( HP100_INT_EN | HP100_RESET_LB, OPTION_LSW );
hp100_cascade_reset( dev, TRUE );
hp100_page( MAC_CTRL );
hp100_andb( ~(HP100_RX_EN|HP100_TX_EN), MAC_CFG_1);
}
hp100_load_eeprom( dev, 0 );
wait();
hp100_cascade_reset( dev, TRUE );
hp100_outw( HP100_DEBUG_EN |
HP100_RX_HDR   |
HP100_EE_EN    |
HP100_BM_WRITE |
HP100_BM_READ  | HP100_RESET_HB |
HP100_FAKE_INT |
HP100_INT_EN   |
HP100_MEM_EN   |
HP100_IO_EN    | HP100_RESET_LB, OPTION_LSW);
hp100_outw( HP100_TRI_INT  |
HP100_MMAP_DIS | HP100_SET_HB, OPTION_LSW );
hp100_outb( HP100_PRIORITY_TX |
HP100_ADV_NXT_PKT |
HP100_TX_CMD      | HP100_RESET_LB, OPTION_MSW );
hp100_mmuinit( dev );
wait();
hp100_cascade_reset( dev, FALSE );
if( lp->lan_type != HP100_LAN_10 )
hp100_login_to_vg_hub( dev, FALSE );
}
static void hp100_mmuinit( struct device *dev )
{
int ioaddr = dev->base_addr;
struct hp100_private *lp = (struct hp100_private *)dev->priv;
int i;
#ifdef HP100_DEBUG_B
hp100_outw( 0x4203, TRACE );
printk("hp100: %s: mmuinit\n",dev->name);
#endif
#ifdef HP100_DEBUG
if( 0!=(hp100_inw(OPTION_LSW)&HP100_HW_RST) )
{
printk("hp100: %s: Not in reset when entering mmuinit. Fix me.\n",dev->name);
return;
}
#endif
hp100_page( PERFORMANCE );
hp100_outw( 0xfefe, IRQ_MASK );
hp100_outw( 0xffff, IRQ_STATUS );
hp100_outw( HP100_DEBUG_EN |
HP100_RX_HDR   |
HP100_EE_EN    | HP100_RESET_HB |
HP100_IO_EN    |
HP100_FAKE_INT |
HP100_INT_EN   | HP100_RESET_LB, OPTION_LSW );
hp100_outw( HP100_TRI_INT | HP100_SET_HB, OPTION_LSW);
if(lp->mode==1)
{
hp100_outw( HP100_BM_WRITE |
HP100_BM_READ  |
HP100_MMAP_DIS | HP100_SET_HB, OPTION_LSW );
}
else if(lp->mode==2)
{
hp100_outw( HP100_BM_WRITE |
HP100_BM_READ  | HP100_RESET_HB, OPTION_LSW );
hp100_outw( HP100_MMAP_DIS | HP100_RESET_HB, OPTION_LSW );
hp100_outw( HP100_MEM_EN | HP100_SET_LB, OPTION_LSW );
hp100_outw( HP100_IO_EN | HP100_SET_LB, OPTION_LSW );
}
else if( lp->mode==3 )
{
hp100_outw( HP100_MMAP_DIS | HP100_SET_HB |
HP100_IO_EN    | HP100_SET_LB, OPTION_LSW );
}
hp100_page( HW_MAP );
hp100_outb( 0, EARLYRXCFG );
hp100_outw( 0, EARLYTXCFG );
if(lp->mode==1)
{
hp100_page( HW_MAP );
hp100_andb( ~HP100_PDL_USE3, MODECTRL1 );
hp100_andb( ~HP100_TX_DUALQ, MODECTRL1 );
hp100_orb( HP100_EN_BUS_FAIL, MODECTRL2);
hp100_outw( HP100_BM_READ | HP100_BM_WRITE | HP100_SET_HB, OPTION_LSW );
hp100_page( HW_MAP );
hp100_orb( HP100_BM_BURST_RD |
HP100_BM_BURST_WR, BM);
if((lp->chip==HP100_CHIPID_RAINIER)||(lp->chip==HP100_CHIPID_SHASTA))
hp100_orb( HP100_BM_PAGE_CK, BM );
hp100_orb( HP100_BM_MASTER, BM );
}
else
{
hp100_page(HW_MAP);
hp100_andb(~HP100_BM_MASTER, BM );
}
hp100_page( MMU_CFG );
if(lp->mode==1)
{
int xmit_stop, recv_stop;
if((lp->chip==HP100_CHIPID_RAINIER)||(lp->chip==HP100_CHIPID_SHASTA))
{
int pdl_stop;
pdl_stop  = lp->memory_size;
xmit_stop = ( pdl_stop-508*(MAX_RX_PDL)-16 )& ~(0x03ff);
recv_stop = ( xmit_stop * (lp->rx_ratio)/100 ) &~(0x03ff);
hp100_outw( (pdl_stop>>4)-1, PDL_MEM_STOP );
#ifdef HP100_DEBUG_BM
printk("hp100: %s: PDL_STOP = 0x%x\n", dev->name, pdl_stop);
#endif
}
else
{
xmit_stop = ( lp->memory_size ) - 1;
recv_stop = ( ( lp->memory_size * lp->rx_ratio ) / 100 ) & ~(0x03ff);
}
hp100_outw( xmit_stop>>4 , TX_MEM_STOP );
hp100_outw( recv_stop>>4 , RX_MEM_STOP );
#ifdef HP100_DEBUG_BM
printk("hp100: %s: TX_STOP  = 0x%x\n",dev->name,xmit_stop>>4);
printk("hp100: %s: RX_STOP  = 0x%x\n",dev->name,recv_stop>>4);
#endif
}
else
{
hp100_outw( (((lp->memory_size*lp->rx_ratio)/100)>>4), RX_MEM_STOP );
hp100_outw( ((lp->memory_size - 1 )>>4), TX_MEM_STOP );
#ifdef HP100_DEBUG
printk("hp100: %s: TX_MEM_STOP: 0x%x\n", dev->name,hp100_inw(TX_MEM_STOP));
printk("hp100: %s: RX_MEM_STOP: 0x%x\n", dev->name,hp100_inw(RX_MEM_STOP));
#endif
}
hp100_page( MAC_ADDRESS );
for ( i = 0; i < 6; i++ )
hp100_outb( dev->dev_addr[ i ], MAC_ADDR + i );
for ( i = 0; i < 8; i++ )
hp100_outb( 0x0, HASH_BYTE0 + i );
hp100_page( MAC_CTRL );
hp100_andb( ~(HP100_RX_EN|
HP100_TX_EN|
HP100_ACC_ERRORED|
HP100_ACC_MC|
HP100_ACC_BC|
HP100_ACC_PHY),   MAC_CFG_1 );
hp100_outb( 0x00, MAC_CFG_2 );
hp100_outb( 0x00, VG_LAN_CFG_2);
if(lp->priority_tx)
hp100_outb( HP100_PRIORITY_TX | HP100_SET_LB, OPTION_MSW );
else
hp100_outb( HP100_PRIORITY_TX | HP100_RESET_LB, OPTION_MSW );
hp100_outb( HP100_ADV_NXT_PKT |
HP100_TX_CMD      | HP100_RESET_LB, OPTION_MSW );
if(lp->mode==1)
hp100_init_pdls( dev );
hp100_page( PERFORMANCE );
hp100_outw( 0xfefe, IRQ_MASK );
hp100_outw( 0xffff, IRQ_STATUS );
}
static int hp100_open( struct device *dev )
{
struct hp100_private *lp = (struct hp100_private *)dev->priv;
#ifdef HP100_DEBUG_B
int ioaddr=dev->base_addr;
#endif
#ifdef HP100_DEBUG_B
hp100_outw( 0x4204, TRACE );
printk("hp100: %s: open\n",dev->name);
#endif
if ( request_irq(dev->irq, hp100_interrupt,
lp->bus==HP100_BUS_PCI||lp->bus==HP100_BUS_EISA?SA_SHIRQ:SA_INTERRUPT,
lp->id->name, dev))
{
printk( "hp100: %s: unable to get IRQ %d\n", dev->name, dev->irq );
return -EAGAIN;
}
MOD_INC_USE_COUNT;
dev->tbusy = 0;
dev->trans_start = jiffies;
dev->interrupt = 0;
dev->start = 1;
lp->lan_type = hp100_sense_lan( dev );
lp->mac1_mode = HP100_MAC1MODE3;
lp->mac2_mode = HP100_MAC2MODE3;
memset( &lp->hash_bytes, 0x00, 8 );
hp100_stop_interface( dev );
hp100_hwinit( dev );
hp100_start_interface( dev );
return 0;
}
static int hp100_close( struct device *dev )
{
int ioaddr = dev->base_addr;
struct hp100_private *lp = (struct hp100_private *)dev->priv;
#ifdef HP100_DEBUG_B
hp100_outw( 0x4205, TRACE );
printk("hp100: %s: close\n", dev->name);
#endif
hp100_page( PERFORMANCE );
hp100_outw( 0xfefe, IRQ_MASK );
hp100_stop_interface( dev );
if ( lp->lan_type == HP100_LAN_100 )
lp->hub_status=hp100_login_to_vg_hub( dev, FALSE );
dev->tbusy = 1;
dev->start = 0;
free_irq( dev->irq, dev );
#ifdef HP100_DEBUG
printk( "hp100: %s: close LSW = 0x%x\n", dev->name, hp100_inw(OPTION_LSW) );
#endif
MOD_DEC_USE_COUNT;
return 0;
}
static void hp100_init_pdls( struct device *dev )
{
struct hp100_private *lp = (struct hp100_private *)dev->priv;
hp100_ring_t         *ringptr;
u_int                *pageptr;
int                  i;
#ifdef HP100_DEBUG_B
int ioaddr = dev->base_addr;
#endif
#ifdef HP100_DEBUG_B
hp100_outw( 0x4206, TRACE );
printk("hp100: %s: init pdls\n", dev->name);
#endif
if(0==lp->page_vaddr_algn)
printk("hp100: %s: Warning: lp->page_vaddr_algn not initialised!\n",dev->name);
else
{
memset(lp->page_vaddr_algn, 0, MAX_RINGSIZE);
pageptr=lp->page_vaddr_algn;
lp->rxrcommit =0;
ringptr = lp->rxrhead = lp-> rxrtail = &(lp->rxring[0]);
for (i=MAX_RX_PDL-1; i>=0; i--)
{
lp->rxring[i].next = ringptr;
ringptr=&(lp->rxring[i]);
pageptr+=hp100_init_rxpdl(dev, ringptr, pageptr);
}
lp->txrcommit = 0;
ringptr = lp->txrhead = lp->txrtail = &(lp->txring[0]);
for (i=MAX_TX_PDL-1; i>=0; i--)
{
lp->txring[i].next = ringptr;
ringptr=&(lp->txring[i]);
pageptr+=hp100_init_txpdl(dev, ringptr, pageptr);
}
}
}
static int hp100_init_rxpdl( struct device *dev, register hp100_ring_t *ringptr, register u32 *pdlptr )
{
if( 0!=( ((unsigned)pdlptr) & 0xf) )
printk("hp100: %s: Init rxpdl: Unaligned pdlptr 0x%x.\n",dev->name,(unsigned)pdlptr);
ringptr->pdl       = pdlptr+1;
ringptr->pdl_paddr = virt_to_bus(pdlptr+1);
ringptr->skb       = (void *) NULL;
*(pdlptr+2) =(u_int) virt_to_bus(pdlptr);
*(pdlptr+3) = 4;
return( ( ((MAX_RX_FRAG*2+2)+3) /4)*4 );
}
static int hp100_init_txpdl( struct device *dev, register hp100_ring_t *ringptr, register u32 *pdlptr )
{
if( 0!=( ((unsigned)pdlptr) & 0xf) )
printk("hp100: %s: Init txpdl: Unaligned pdlptr 0x%x.\n",dev->name,(unsigned) pdlptr);
ringptr->pdl       = pdlptr;
ringptr->pdl_paddr = virt_to_bus(pdlptr);
ringptr->skb = (void *) NULL;
return((((MAX_TX_FRAG*2+2)+3)/4)*4);
}
int hp100_build_rx_pdl( hp100_ring_t *ringptr, struct device *dev )
{
#ifdef HP100_DEBUG_B
int ioaddr = dev->base_addr;
#endif
#ifdef HP100_DEBUG_BM
u_int *p;
#endif
#ifdef HP100_DEBUG_B
hp100_outw( 0x4207, TRACE );
printk("hp100: %s: build rx pdl\n", dev->name);
#endif
ringptr->skb = dev_alloc_skb( ((MAX_ETHER_SIZE+2+3)/4)*4 );
if(NULL!=ringptr->skb)
{
skb_reserve(ringptr->skb, 2);
ringptr->skb->dev=dev;
ringptr->skb->data=(u_char *)skb_put(ringptr->skb, MAX_ETHER_SIZE );
#ifdef HP100_DEBUG_BM
printk("hp100: %s: build_rx_pdl: PDH@0x%x, skb->data (len %d) at 0x%x\n",
dev->name,
(u_int) ringptr->pdl,
((MAX_ETHER_SIZE+2+3)/4)*4,
(unsigned int) ringptr->skb->data);
#endif
ringptr->pdl[0] = 0x00020000;
ringptr->pdl[3] = ((u_int)virt_to_bus(ringptr->skb->data));
ringptr->pdl[4] = MAX_ETHER_SIZE;
#ifdef HP100_DEBUG_BM
for(p=(ringptr->pdl); p<(ringptr->pdl+5); p++)
printk("hp100: %s: Adr 0x%.8x = 0x%.8x\n",dev->name,(u_int) p,(u_int) *p );
#endif
return(1);
}
#ifdef HP100_DEBUG_BM
printk("hp100: %s: build_rx_pdl: PDH@0x%x, No space for skb.\n",
dev->name,
(u_int) ringptr->pdl);
#endif
ringptr->pdl[0]=0x00010000;
return(0);
}
static void hp100_rxfill( struct device *dev )
{
int ioaddr=dev->base_addr;
struct hp100_private  *lp      = (struct hp100_private *)dev->priv;
hp100_ring_t    *ringptr;
#ifdef HP100_DEBUG_B
hp100_outw( 0x4208, TRACE );
printk("hp100: %s: rxfill\n",dev->name);
#endif
hp100_page( PERFORMANCE );
while (lp->rxrcommit < MAX_RX_PDL)
{
ringptr = lp->rxrtail;
if (0 == hp100_build_rx_pdl( ringptr, dev ))
{
return;
}
#ifdef HP100_DEBUG_BM
printk("hp100: %s: rxfill: Hand to card: pdl #%d @0x%x phys:0x%x, buffer: 0x%x\n",
dev->name,
lp->rxrcommit,
(u_int)ringptr->pdl,
(u_int)ringptr->pdl_paddr,
(u_int)ringptr->pdl[3]);
#endif
hp100_outl( (u32)ringptr->pdl_paddr, RX_PDA);
lp->rxrcommit += 1;
lp->rxrtail = ringptr->next;
}
}
static void hp100_BM_shutdown( struct device *dev )
{
int ioaddr = dev->base_addr;
struct hp100_private *lp = (struct hp100_private *)dev->priv;
unsigned long time;
#ifdef HP100_DEBUG_B
hp100_outw( 0x4209, TRACE );
printk("hp100: %s: bm shutdown\n",dev->name);
#endif
hp100_page( PERFORMANCE );
hp100_outw( 0xfefe, IRQ_MASK );
hp100_outw( 0xffff, IRQ_STATUS );
hp100_outw( HP100_INT_EN | HP100_RESET_LB , OPTION_LSW );
hp100_page( MAC_CTRL );
hp100_andb( ~(HP100_RX_EN | HP100_TX_EN), MAC_CFG_1 );
if (0 != (hp100_inw(OPTION_LSW)&HP100_HW_RST) )
{
hp100_page( MAC_CTRL );
for(time=0; time<5000; time++)
{
if( (hp100_inb(MAC_CFG_1)&(HP100_TX_IDLE|HP100_RX_IDLE))==
(HP100_TX_IDLE|HP100_RX_IDLE) ) break;
}
if( lp->chip==HP100_CHIPID_LASSEN )
{
hp100_page(HW_MAP);
hp100_andb( ~HP100_BM_MASTER, BM );
for(time=0; time<32000; time++)
{
if ( 0 == (hp100_inb( BM ) & HP100_BM_MASTER) ) break;
}
}
else
{
hp100_page( PERFORMANCE );
for(time=0; time<10000; time++)
{
if ( (hp100_inb( RX_PDL ) == 0) &&
(hp100_inb( RX_PKT_CNT ) == 0) ) break;
}
if(time>=10000)
printk("hp100: %s: BM shutdown error.\n", dev->name);
for(time=0; time<10000; time++) {
if ( (0 == hp100_inb( TX_PKT_CNT )) &&
(0 != (hp100_inb( TX_MEM_FREE )&HP100_AUTO_COMPARE))) break;
}
hp100_page(HW_MAP);
hp100_andb( ~HP100_BM_MASTER, BM );
}
hp100_cascade_reset( dev, TRUE );
}
hp100_page( PERFORMANCE );
}
static int hp100_start_xmit_bm( struct sk_buff *skb, struct device *dev )
{
unsigned long flags;
int i, ok_flag;
int ioaddr = dev->base_addr;
struct hp100_private *lp = (struct hp100_private *)dev->priv;
hp100_ring_t *ringptr;
#ifdef HP100_DEBUG_B
hp100_outw( 0x4210, TRACE );
printk("hp100: %s: start_xmit_bm\n",dev->name);
#endif
if ( skb==NULL )
{
#ifndef LINUX_2_1
dev_tint( dev );
#endif
return 0;
}
if ( skb->len <= 0 ) return 0;
if( lp->txrtail->next==lp->txrhead )
{
#ifdef HP100_DEBUG
printk("hp100: %s: start_xmit_bm: No TX PDL available.\n", dev->name);
#endif
if ( jiffies - dev->trans_start < HZ ) return -EAGAIN;
if ( lp->lan_type < 0 )
{
hp100_stop_interface( dev );
if ( ( lp->lan_type = hp100_sense_lan( dev ) ) < 0 )
{
printk( "hp100: %s: no connection found - check wire\n", dev->name );
hp100_start_interface( dev );
return -EIO;
}
if ( lp->lan_type == HP100_LAN_100 )
lp->hub_status = hp100_login_to_vg_hub( dev, FALSE );
hp100_start_interface( dev );
}
if ( lp->lan_type == HP100_LAN_100 && lp->hub_status < 0 )
{
printk( "hp100: %s: login to 100Mb/s hub retry\n", dev->name );
hp100_stop_interface( dev );
lp->hub_status = hp100_login_to_vg_hub( dev, FALSE );
hp100_start_interface( dev );
}
else
{
hp100_ints_off();
i = hp100_sense_lan( dev );
hp100_ints_on();
if ( i == HP100_LAN_ERR )
printk( "hp100: %s: link down detected\n", dev->name );
else
if ( lp->lan_type != i )
{
printk( "hp100: %s: cable change 10Mb/s <-> 100Mb/s detected\n", dev->name );
lp->lan_type = i;
hp100_stop_interface( dev );
if ( lp->lan_type == HP100_LAN_100 )
lp->hub_status = hp100_login_to_vg_hub( dev, FALSE );
hp100_start_interface( dev );
}
else
{
printk( "hp100: %s: interface reset\n", dev->name );
hp100_stop_interface( dev );
if ( lp->lan_type == HP100_LAN_100 )
lp->hub_status = hp100_login_to_vg_hub( dev, FALSE );
hp100_start_interface( dev );
}
}
dev->trans_start = jiffies;
return -EAGAIN;
}
save_flags( flags );
cli();
ringptr=lp->txrtail;
lp->txrtail=ringptr->next;
ok_flag = skb->len >= HP100_MIN_PACKET_SIZE;
i = ok_flag ? skb->len : HP100_MIN_PACKET_SIZE;
ringptr->skb=skb;
ringptr->pdl[0]=((1<<16) | i);
ringptr->pdl[1]=(u32)virt_to_bus(skb->data);
if(lp->chip==HP100_CHIPID_SHASTA)
{
ringptr->pdl[2]=i;
}
else
{
ringptr->pdl[2]=skb->len;
}
hp100_outl( ringptr->pdl_paddr, TX_PDA_L );
lp->txrcommit++;
restore_flags( flags );
lp->stats.tx_packets++;
#ifdef LINUX_2_1
lp->stats.tx_bytes += skb->len;
#endif
dev->trans_start = jiffies;
return 0;
}
static void hp100_clean_txring( struct device *dev )
{
struct hp100_private *lp = (struct hp100_private *)dev->priv;
int    ioaddr = dev->base_addr;
int    donecount;
#ifdef HP100_DEBUG_B
hp100_outw( 0x4211, TRACE );
printk("hp100: %s: clean txring\n", dev->name);
#endif
donecount=(lp->txrcommit)-hp100_inb(TX_PDL);
#ifdef HP100_DEBUG
if(donecount>MAX_TX_PDL)
printk("hp100: %s: Warning: More PDLs transmitted than commited to card???\n",dev->name);
#endif
for( ; 0!=donecount; donecount-- )
{
#ifdef HP100_DEBUG_BM
printk("hp100: %s: Free skb: data @0x%.8x txrcommit=0x%x TXPDL=0x%x, done=0x%x\n",
dev->name,
(u_int) lp->txrhead->skb->data,
lp->txrcommit,
hp100_inb(TX_PDL),
donecount);
#endif
#ifdef LINUX_2_1
dev_kfree_skb( lp->txrhead->skb );
#else
dev_kfree_skb( lp->txrhead->skb, FREE_WRITE );
#endif
lp->txrhead->skb=(void *)NULL;
lp->txrhead=lp->txrhead->next;
lp->txrcommit--;
}
}
static int hp100_start_xmit( struct sk_buff *skb, struct device *dev )
{
int i, ok_flag;
int ioaddr = dev->base_addr;
u_short val;
struct hp100_private *lp = (struct hp100_private *)dev->priv;
#ifdef HP100_DEBUG_B
hp100_outw( 0x4212, TRACE );
printk("hp100: %s: start_xmit\n", dev->name);
#endif
if ( skb==NULL )
{
#ifndef LINUX_2_1
dev_tint( dev );
#endif
return 0;
}
if ( skb->len <= 0 ) return 0;
if ( lp->lan_type < 0 )
{
hp100_stop_interface( dev );
if ( ( lp->lan_type = hp100_sense_lan( dev ) ) < 0 )
{
printk( "hp100: %s: no connection found - check wire\n", dev->name );
hp100_start_interface( dev );
return -EIO;
}
if ( lp->lan_type == HP100_LAN_100 )
lp->hub_status = hp100_login_to_vg_hub( dev, FALSE );
hp100_start_interface( dev );
}
i=hp100_inl(TX_MEM_FREE)&0x7fffffff;
if ( !(((i/2)-539)>(skb->len+16) && (hp100_inb(TX_PKT_CNT)<255)) )
{
#ifdef HP100_DEBUG
printk( "hp100: %s: start_xmit: tx free mem = 0x%x\n", dev->name, i );
#endif
if ( jiffies - dev->trans_start < HZ )
{
#ifdef HP100_DEBUG
printk("hp100: %s: trans_start timing problem\n", dev->name);
#endif
return -EAGAIN;
}
if ( lp->lan_type == HP100_LAN_100 && lp->hub_status < 0 )
{
printk( "hp100: %s: login to 100Mb/s hub retry\n", dev->name );
hp100_stop_interface( dev );
lp->hub_status = hp100_login_to_vg_hub( dev, FALSE );
hp100_start_interface( dev );
}
else
{
hp100_ints_off();
i = hp100_sense_lan( dev );
hp100_ints_on();
if ( i == HP100_LAN_ERR )
printk( "hp100: %s: link down detected\n", dev->name );
else
if ( lp->lan_type != i )
{
printk( "hp100: %s: cable change 10Mb/s <-> 100Mb/s detected\n", dev->name );
lp->lan_type = i;
hp100_stop_interface( dev );
if ( lp->lan_type == HP100_LAN_100 )
lp->hub_status = hp100_login_to_vg_hub( dev, FALSE );
hp100_start_interface( dev );
}
else
{
printk( "hp100: %s: interface reset\n", dev->name );
hp100_stop_interface( dev );
if ( lp->lan_type == HP100_LAN_100 )
lp->hub_status = hp100_login_to_vg_hub( dev, FALSE );
hp100_start_interface( dev );
udelay(1000);
}
}
dev->trans_start = jiffies;
return -EAGAIN;
}
for ( i=0; i<6000 && ( hp100_inb( OPTION_MSW ) & HP100_TX_CMD ); i++ )
{
#ifdef HP100_DEBUG_TX
printk( "hp100: %s: start_xmit: busy\n", dev->name );
#endif
}
hp100_ints_off();
val = hp100_inw( IRQ_STATUS );
hp100_outw( HP100_TX_COMPLETE, IRQ_STATUS );
#ifdef HP100_DEBUG_TX
printk("hp100: %s: start_xmit: irq_status=0x%.4x, irqmask=0x%.4x, len=%d\n",dev->name,val,hp100_inw(IRQ_MASK),(int)skb->len );
#endif
ok_flag = skb->len >= HP100_MIN_PACKET_SIZE;
i = ok_flag ? skb->len : HP100_MIN_PACKET_SIZE;
hp100_outw( i, DATA32 );
hp100_outw( i, FRAGMENT_LEN );
if ( lp->mode==2 )
{
if ( lp->mem_ptr_virt )
{
memcpy( lp->mem_ptr_virt, skb->data, ( skb->len + 3 ) & ~3 );
if ( !ok_flag )
memset( lp->mem_ptr_virt, 0, HP100_MIN_PACKET_SIZE - skb->len );
}
else
{
memcpy_toio( lp->mem_ptr_phys, skb->data, (skb->len + 3) & ~3 );
if ( !ok_flag )
memset_io( lp->mem_ptr_phys, 0, HP100_MIN_PACKET_SIZE - skb->len );
}
}
else
{
outsl( ioaddr + HP100_REG_DATA32, skb->data, ( skb->len + 3 ) >> 2 );
if ( !ok_flag )
for ( i = ( skb->len + 3 ) & ~3; i < HP100_MIN_PACKET_SIZE; i += 4 )
hp100_outl( 0, DATA32 );
}
hp100_outb( HP100_TX_CMD | HP100_SET_LB, OPTION_MSW );
lp->stats.tx_packets++;
#ifdef LINUX_2_1
lp->stats.tx_bytes += skb->len;
#endif
dev->trans_start=jiffies;
hp100_ints_on();
#ifdef LINUX_2_1
dev_kfree_skb( skb );
#else
dev_kfree_skb( skb, FREE_WRITE );
#endif
#ifdef HP100_DEBUG_TX
printk( "hp100: %s: start_xmit: end\n", dev->name );
#endif
return 0;
}
static void hp100_rx( struct device *dev )
{
int packets, pkt_len;
int ioaddr = dev->base_addr;
struct hp100_private *lp = (struct hp100_private *)dev->priv;
u_int header;
struct sk_buff *skb;
#ifdef DEBUG_B
hp100_outw( 0x4213, TRACE );
printk("hp100: %s: rx\n", dev->name);
#endif
packets = hp100_inb( RX_PKT_CNT );
#ifdef HP100_DEBUG_RX
if ( packets > 1 )
printk( "hp100: %s: rx: waiting packets = %d\n", dev->name,packets );
#endif
while ( packets-- > 0 )
{
for (pkt_len=0; pkt_len<6000 &&(hp100_inb(OPTION_MSW)&HP100_ADV_NXT_PKT);
pkt_len++ )
{
#ifdef HP100_DEBUG_RX
printk( "hp100: %s: rx: busy, remaining packets = %d\n", dev->name, packets );
#endif
}
if( lp->mode==2 )
{
if ( lp->mem_ptr_virt )
header = *(__u32 *)lp->mem_ptr_virt;
else
header = readl( lp->mem_ptr_phys );
}
else
header = hp100_inl( DATA32 );
pkt_len = ((header & HP100_PKT_LEN_MASK) + 3) & ~3;
#ifdef HP100_DEBUG_RX
printk( "hp100: %s: rx: new packet - length=%d, errors=0x%x, dest=0x%x\n",
dev->name,
header & HP100_PKT_LEN_MASK, (header>>16)&0xfff8,
(header>>16)&7);
#endif
skb = dev_alloc_skb( pkt_len );
if ( skb == NULL )
{
#ifdef HP100_DEBUG
printk( "hp100: %s: rx: couldn't allocate a sk_buff of size %d\n", dev->name, pkt_len );
#endif
lp->stats.rx_dropped++;
}
else
{
u_char *ptr;
skb->dev = dev;
ptr = (u_char *)skb_put( skb, pkt_len );
if ( lp->mode==2 )
{
if ( lp->mem_ptr_virt )
memcpy( ptr, lp->mem_ptr_virt, pkt_len );
else
memcpy_fromio( ptr, lp->mem_ptr_phys, pkt_len );
}
else
insl( ioaddr + HP100_REG_DATA32, ptr, pkt_len >> 2 );
skb->protocol = eth_type_trans( skb, dev );
netif_rx( skb );
lp->stats.rx_packets++;
#ifdef LINUX_2_1
lp->stats.rx_bytes += skb->len;
#endif
#ifdef HP100_DEBUG_RX
printk( "hp100: %s: rx: %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x\n",
dev->name,
ptr[ 0 ], ptr[ 1 ], ptr[ 2 ], ptr[ 3 ], ptr[ 4 ], ptr[ 5 ],
ptr[ 6 ], ptr[ 7 ], ptr[ 8 ], ptr[ 9 ], ptr[ 10 ], ptr[ 11 ] );
#endif
}
hp100_outb( HP100_ADV_NXT_PKT | HP100_SET_LB, OPTION_MSW );
switch ( header & 0x00070000 ) {
case (HP100_MULTI_ADDR_HASH<<16):
case (HP100_MULTI_ADDR_NO_HASH<<16):
lp->stats.multicast++; break;
}
}
#ifdef HP100_DEBUG_RX
printk( "hp100_rx: %s: end\n", dev->name );
#endif
}
static void hp100_rx_bm( struct device *dev )
{
int ioaddr = dev->base_addr;
struct hp100_private *lp = (struct hp100_private *)dev->priv;
hp100_ring_t *ptr;
u_int header;
int pkt_len;
#ifdef HP100_DEBUG_B
hp100_outw( 0x4214, TRACE );
printk("hp100: %s: rx_bm\n", dev->name);
#endif
#ifdef HP100_DEBUG
if(0==lp->rxrcommit)
{
printk("hp100: %s: rx_bm called although no PDLs were committed to adapter?\n", dev->name);
return;
}
else
if( (hp100_inw(RX_PKT_CNT)&0x00ff) >= lp->rxrcommit)
{
printk("hp100: %s: More packets received than commited? RX_PKT_CNT=0x%x, commit=0x%x\n", dev->name, hp100_inw(RX_PKT_CNT)&0x00ff, lp->rxrcommit);
return;
}
#endif
while( (lp->rxrcommit > hp100_inb(RX_PDL)) )
{
ptr=lp->rxrhead;
header = *(ptr->pdl-1);
pkt_len = (header & HP100_PKT_LEN_MASK);
#ifdef HP100_DEBUG_BM
printk( "hp100: %s: rx_bm: header@0x%x=0x%x length=%d, errors=0x%x, dest=0x%x\n",
dev->name,
(u_int) (ptr->pdl-1),(u_int) header,
pkt_len,
(header>>16)&0xfff8,
(header>>16)&7);
printk( "hp100: %s: RX_PDL_COUNT:0x%x TX_PDL_COUNT:0x%x, RX_PKT_CNT=0x%x PDH=0x%x, Data@0x%x len=0x%x\n",
dev->name,
hp100_inb( RX_PDL ),
hp100_inb( TX_PDL ),
hp100_inb( RX_PKT_CNT ),
(u_int) *(ptr->pdl),
(u_int) *(ptr->pdl+3),
(u_int) *(ptr->pdl+4));
#endif
if( (pkt_len>=MIN_ETHER_SIZE) &&
(pkt_len<=MAX_ETHER_SIZE) )
{
if(ptr->skb==NULL)
{
printk("hp100: %s: rx_bm: skb null\n", dev->name);
lp->stats.rx_dropped++;
}
else
{
skb_trim( ptr->skb, pkt_len );
ptr->skb->protocol = eth_type_trans( ptr->skb, dev );
netif_rx( ptr->skb );
lp->stats.rx_packets++;
#ifdef LINUX_2_1
lp->stats.rx_bytes += ptr->skb->len;
#endif
}
switch ( header & 0x00070000 ) {
case (HP100_MULTI_ADDR_HASH<<16):
case (HP100_MULTI_ADDR_NO_HASH<<16):
lp->stats.multicast++; break;
}
}
else
{
#ifdef HP100_DEBUG
printk("hp100: %s: rx_bm: Received bad packet (length=%d)\n",dev->name,pkt_len);
#endif
if(ptr->skb!=NULL)
#ifdef LINUX_2_1
dev_kfree_skb( ptr->skb );
#else
dev_kfree_skb( ptr->skb, FREE_READ );
#endif
lp->stats.rx_errors++;
}
lp->rxrhead=lp->rxrhead->next;
if (0 == hp100_build_rx_pdl( lp->rxrtail, dev ))
{
#ifdef HP100_DEBUG
printk("hp100: %s: rx_bm: No space for new PDL.\n", dev->name);
#endif
return;
}
else
{
hp100_outl((u32)lp->rxrtail->pdl_paddr, RX_PDA);
lp->rxrtail=lp->rxrtail->next;
}
}
}
static hp100_stats_t *hp100_get_stats( struct device *dev )
{
int ioaddr = dev->base_addr;
#ifdef HP100_DEBUG_B
hp100_outw( 0x4215, TRACE );
#endif
hp100_ints_off();
hp100_update_stats( dev );
hp100_ints_on();
return &((struct hp100_private *)dev->priv)->stats;
}
static void hp100_update_stats( struct device *dev )
{
int ioaddr = dev->base_addr;
u_short val;
struct hp100_private *lp = (struct hp100_private *)dev->priv;
#ifdef HP100_DEBUG_B
hp100_outw( 0x4216, TRACE );
printk("hp100: %s: update-stats\n", dev->name);
#endif
hp100_page( MAC_CTRL );
val = hp100_inw( DROPPED ) & 0x0fff;
lp->stats.rx_errors += val;
lp->stats.rx_over_errors += val;
val = hp100_inb( CRC );
lp->stats.rx_errors += val;
lp->stats.rx_crc_errors += val;
val = hp100_inb( ABORT );
lp->stats.tx_errors += val;
lp->stats.tx_aborted_errors += val;
hp100_page( PERFORMANCE );
}
static void hp100_misc_interrupt( struct device *dev )
{
struct hp100_private *lp = (struct hp100_private *)dev->priv;
#ifdef HP100_DEBUG_B
hp100_outw( 0x4216, TRACE );
printk("hp100: %s: misc_interrupt\n", dev->name);
#endif
lp->stats.rx_errors++;
lp->stats.tx_errors++;
}
static void hp100_clear_stats( int ioaddr )
{
unsigned long flags;
#ifdef HP100_DEBUG_B
hp100_outw( 0x4217, TRACE );
printk("hp100: %s: clear_stats\n", dev->name);
#endif
save_flags( flags );
cli();
hp100_page( MAC_CTRL );
hp100_inw( DROPPED );
hp100_inb( CRC );
hp100_inb( ABORT );
hp100_page( PERFORMANCE );
restore_flags( flags );
}
static void hp100_set_multicast_list( struct device *dev )
{
unsigned long flags;
int ioaddr = dev->base_addr;
struct hp100_private *lp = (struct hp100_private *)dev->priv;
#ifdef HP100_DEBUG_B
hp100_outw( 0x4218, TRACE );
printk("hp100: %s: set_mc_list\n", dev->name);
#endif
save_flags( flags );
cli();
hp100_ints_off();
hp100_page( MAC_CTRL );
hp100_andb( ~(HP100_RX_EN | HP100_TX_EN), MAC_CFG_1 );
if ( dev->flags & IFF_PROMISC )
{
lp->mac2_mode = HP100_MAC2MODE6;
lp->mac1_mode = HP100_MAC1MODE6;
memset( &lp->hash_bytes, 0xff, 8 );
}
else if ( dev->mc_count || (dev->flags&IFF_ALLMULTI) )
{
lp->mac2_mode = HP100_MAC2MODE5;
lp->mac1_mode = HP100_MAC1MODE5;
#ifdef HP100_MULTICAST_FILTER
if ( dev -> flags & IFF_ALLMULTI )
{
memset( &lp->hash_bytes, 0xff, 8 );
}
else
{
int i, j, idx;
u_char *addrs;
struct dev_mc_list *dmi;
memset( &lp->hash_bytes, 0x00, 8 );
#ifdef HP100_DEBUG
printk("hp100: %s: computing hash filter - mc_count = %i\n", dev -> name, dev -> mc_count );
#endif
for ( i = 0, dmi = dev -> mc_list; i < dev -> mc_count; i++, dmi = dmi -> next )
{
addrs = dmi -> dmi_addr;
if ( ( *addrs & 0x01 ) == 0x01 )
{
#ifdef HP100_DEBUG
printk("hp100: %s: multicast = %02x:%02x:%02x:%02x:%02x:%02x, ",
dev -> name,
addrs[ 0 ], addrs[ 1 ], addrs[ 2 ],
addrs[ 3 ], addrs[ 4 ], addrs[ 5 ] );
#endif
for ( j = idx = 0; j < 6; j++ )
{
idx ^= *addrs++ & 0x3f;
printk( ":%02x:", idx );
}
#ifdef HP100_DEBUG
printk("idx = %i\n", idx );
#endif
lp->hash_bytes[ idx >> 3 ] |= ( 1 << ( idx & 7 ) );
}
}
}
#else
memset( &lp->hash_bytes, 0xff, 8 );
#endif
}
else
{
lp->mac2_mode = HP100_MAC2MODE3;
lp->mac1_mode = HP100_MAC1MODE3;
memset( &lp->hash_bytes, 0x00, 8 );
}
if ( ( (hp100_inb(MAC_CFG_1) & 0x0f)!=lp->mac1_mode ) ||
( hp100_inb(MAC_CFG_2)!=lp->mac2_mode ) )
{
int i;
hp100_outb( lp->mac2_mode, MAC_CFG_2 );
hp100_andb( HP100_MAC1MODEMASK, MAC_CFG_1 );
hp100_orb( lp->mac1_mode, MAC_CFG_1 );
hp100_page( MAC_ADDRESS );
for ( i = 0; i < 8; i++ )
hp100_outb( lp->hash_bytes[ i ], HASH_BYTE0 + i );
#ifdef HP100_DEBUG
printk("hp100: %s: mac1 = 0x%x, mac2 = 0x%x, multicast hash = %02x:%02x:%02x:%02x:%02x:%02x:%02x:%02x\n",
dev->name, lp->mac1_mode, lp->mac2_mode,
lp->hash_bytes[ 0 ], lp->hash_bytes[ 1 ],
lp->hash_bytes[ 2 ], lp->hash_bytes[ 3 ],
lp->hash_bytes[ 4 ], lp->hash_bytes[ 5 ],
lp->hash_bytes[ 6 ], lp->hash_bytes[ 7 ]
);
#endif
if(lp->lan_type==HP100_LAN_100)
{
#ifdef HP100_DEBUG
printk("hp100: %s: 100VG MAC settings have changed - relogin.\n", dev->name);
#endif
lp->hub_status=hp100_login_to_vg_hub( dev, TRUE );
}
}
else
{
int i;
u_char old_hash_bytes[ 8 ];
hp100_page( MAC_ADDRESS );
for ( i = 0; i < 8; i++ )
old_hash_bytes[ i ] = hp100_inb( HASH_BYTE0 + i );
if ( memcmp( old_hash_bytes, &lp->hash_bytes, 8 ) )
{
for ( i = 0; i < 8; i++ )
hp100_outb( lp->hash_bytes[ i ], HASH_BYTE0 + i );
#ifdef HP100_DEBUG
printk("hp100: %s: multicast hash = %02x:%02x:%02x:%02x:%02x:%02x:%02x:%02x\n",
dev->name,
lp->hash_bytes[ 0 ], lp->hash_bytes[ 1 ],
lp->hash_bytes[ 2 ], lp->hash_bytes[ 3 ],
lp->hash_bytes[ 4 ], lp->hash_bytes[ 5 ],
lp->hash_bytes[ 6 ], lp->hash_bytes[ 7 ]
);
#endif
if(lp->lan_type==HP100_LAN_100)
{
#ifdef HP100_DEBUG
printk("hp100: %s: 100VG MAC settings have changed - relogin.\n", dev->name);
#endif
lp->hub_status=hp100_login_to_vg_hub( dev, TRUE );
}
}
}
hp100_page( MAC_CTRL );
hp100_orb( HP100_RX_EN | HP100_RX_IDLE |
HP100_TX_EN | HP100_TX_IDLE, MAC_CFG_1 );
hp100_page( PERFORMANCE );
hp100_ints_on();
restore_flags( flags );
}
static void hp100_interrupt( int irq, void *dev_id, struct pt_regs *regs )
{
struct device *dev = (struct device *)dev_id;
struct hp100_private *lp = (struct hp100_private *)dev->priv;
int ioaddr;
u_int val;
if ( dev == NULL ) return;
ioaddr = dev->base_addr;
if ( dev->interrupt )
printk( "hp100: %s: re-entering the interrupt handler\n", dev->name );
hp100_ints_off();
dev->interrupt = 1;
#ifdef HP100_DEBUG_B
hp100_outw( 0x4219, TRACE );
#endif
val = hp100_inw( IRQ_STATUS );
#ifdef HP100_DEBUG_IRQ
printk( "hp100: %s: mode=%x,IRQ_STAT=0x%.4x,RXPKTCNT=0x%.2x RXPDL=0x%.2x TXPKTCNT=0x%.2x TXPDL=0x%.2x\n",
dev->name,
lp->mode,
(u_int)val,
hp100_inb( RX_PKT_CNT ),
hp100_inb( RX_PDL ),
hp100_inb( TX_PKT_CNT ),
hp100_inb( TX_PDL )
);
#endif
if(val==0)
{
dev->interrupt=0;
hp100_ints_on();
return;
}
if ( val & HP100_RX_PDL_FILL_COMPL )
{
if(lp->mode==1)
hp100_rx_bm( dev );
else
{
printk("hp100: %s: rx_pdl_fill_compl interrupt although not busmaster?\n", dev->name);
}
}
if ( val & HP100_RX_PACKET  )
{
if(lp->mode!=1)
hp100_rx( dev );
else if ( !(val & HP100_RX_PDL_FILL_COMPL ))
{
hp100_rx_bm( dev );
}
}
hp100_outw( val, IRQ_STATUS );
if ( val & ( HP100_TX_ERROR | HP100_RX_ERROR ) )
{
#ifdef HP100_DEBUG_IRQ
printk("hp100: %s: TX/RX Error IRQ\n", dev->name);
#endif
hp100_update_stats( dev );
if(lp->mode==1)
{
hp100_rxfill( dev );
hp100_clean_txring( dev );
}
}
if ( (lp->mode==1)&&(val &(HP100_RX_PDA_ZERO)) )
hp100_rxfill( dev );
if ( (lp->mode==1) && ( val & ( HP100_TX_COMPLETE )) )
hp100_clean_txring( dev );
if ( val & HP100_MISC_ERROR )
{
#ifdef HP100_DEBUG_IRQ
printk("hp100: %s: Misc. Error Interrupt - Check cabling.\n", dev->name);
#endif
if(lp->mode==1)
{
hp100_clean_txring( dev );
hp100_rxfill( dev );
}
hp100_misc_interrupt( dev );
}
dev->interrupt = 0;
hp100_ints_on();
}
static void hp100_start_interface( struct device *dev )
{
unsigned long flags;
int ioaddr = dev->base_addr;
struct hp100_private *lp = (struct hp100_private *)dev->priv;
#ifdef HP100_DEBUG_B
hp100_outw( 0x4220, TRACE );
printk("hp100: %s: hp100_start_interface\n",dev->name);
#endif
save_flags( flags );
cli();
hp100_page( PERFORMANCE );
hp100_outw( 0xfefe, IRQ_MASK );
hp100_outw( 0xffff, IRQ_STATUS );
hp100_outw( HP100_FAKE_INT|HP100_INT_EN|HP100_RESET_LB, OPTION_LSW);
hp100_outw( HP100_TRI_INT | HP100_RESET_HB, OPTION_LSW );
if(lp->mode==1)
{
hp100_page(HW_MAP);
hp100_orb( HP100_BM_MASTER, BM );
hp100_rxfill( dev );
}
else if(lp->mode==2)
{
hp100_outw( HP100_MMAP_DIS | HP100_RESET_HB, OPTION_LSW );
}
hp100_page(PERFORMANCE);
hp100_outw( 0xfefe, IRQ_MASK );
hp100_outw( 0xffff, IRQ_STATUS );
if(lp->mode==1)
{
hp100_outw( HP100_RX_PDL_FILL_COMPL |
HP100_RX_PDA_ZERO  |
HP100_RX_ERROR     |
HP100_SET_HB  |
HP100_TX_COMPLETE  |
HP100_TX_ERROR     | HP100_SET_LB, IRQ_MASK );
}
else
{
hp100_outw( HP100_RX_PACKET  |
HP100_RX_ERROR   | HP100_SET_HB |
HP100_TX_ERROR   | HP100_SET_LB , IRQ_MASK );
}
hp100_set_multicast_list( dev );
restore_flags( flags );
}
static void hp100_stop_interface( struct device *dev )
{
struct hp100_private *lp = (struct hp100_private *)dev->priv;
int ioaddr = dev->base_addr;
u_int val;
#ifdef HP100_DEBUG_B
printk("hp100: %s: hp100_stop_interface\n",dev->name);
hp100_outw( 0x4221, TRACE );
#endif
if (lp->mode==1)
hp100_BM_shutdown( dev );
else
{
hp100_outw( HP100_INT_EN | HP100_RESET_LB |
HP100_TRI_INT | HP100_MMAP_DIS | HP100_SET_HB, OPTION_LSW );
val = hp100_inw( OPTION_LSW );
hp100_page( MAC_CTRL );
hp100_andb( ~(HP100_RX_EN | HP100_TX_EN), MAC_CFG_1 );
if ( !(val & HP100_HW_RST) ) return;
for ( val = 0; val < 6000; val++ )
if ( ( hp100_inb( MAC_CFG_1 ) & (HP100_TX_IDLE | HP100_RX_IDLE) ) ==
(HP100_TX_IDLE | HP100_RX_IDLE) )
{
hp100_page(PERFORMANCE);
return;
}
printk( "hp100: %s: hp100_stop_interface - timeout\n", dev->name );
hp100_page(PERFORMANCE);
}
}
static void hp100_load_eeprom( struct device *dev, u_short probe_ioaddr )
{
int i;
int ioaddr = probe_ioaddr > 0 ? probe_ioaddr : dev->base_addr;
#ifdef HP100_DEBUG_B
hp100_outw( 0x4222, TRACE );
#endif
hp100_page( EEPROM_CTRL );
hp100_andw( ~HP100_EEPROM_LOAD, EEPROM_CTRL );
hp100_orw( HP100_EEPROM_LOAD, EEPROM_CTRL );
for ( i = 0; i < 10000; i++ )
if ( !( hp100_inb( OPTION_MSW ) & HP100_EE_LOAD ) ) return;
printk( "hp100: %s: hp100_load_eeprom - timeout\n", dev->name );
}
static int hp100_sense_lan( struct device *dev )
{
int ioaddr = dev->base_addr;
u_short val_VG, val_10;
struct hp100_private *lp = (struct hp100_private *)dev->priv;
#ifdef HP100_DEBUG_B
hp100_outw( 0x4223, TRACE );
#endif
hp100_page( MAC_CTRL );
val_10 = hp100_inb( 10_LAN_CFG_1 );
val_VG = hp100_inb( VG_LAN_CFG_1 );
hp100_page( PERFORMANCE );
#ifdef HP100_DEBUG
printk( "hp100: %s: sense_lan: val_VG = 0x%04x, val_10 = 0x%04x\n", dev->name, val_VG, val_10 );
#endif
if ( val_10 & HP100_LINK_BEAT_ST )
return HP100_LAN_10;
if ( val_10 & HP100_AUI_ST )
{
val_10 |= HP100_AUI_SEL | HP100_LOW_TH;
hp100_page( MAC_CTRL );
hp100_outb( val_10, 10_LAN_CFG_1 );
hp100_page( PERFORMANCE );
return HP100_LAN_10;
}
if ( (lp->id->id == 0x02019F022) ||
(lp->id->id == 0x01042103c) ||
(lp->id->id == 0x01040103c) )
return HP100_LAN_ERR;
if ( val_VG & HP100_LINK_CABLE_ST )
return HP100_LAN_100;
return HP100_LAN_ERR;
}
static int hp100_down_vg_link( struct device *dev )
{
struct hp100_private *lp = (struct hp100_private *)dev->priv;
int ioaddr = dev->base_addr;
unsigned long time;
long savelan, newlan;
#ifdef HP100_DEBUG_B
hp100_outw( 0x4224, TRACE );
printk("hp100: %s: down_vg_link\n", dev->name);
#endif
hp100_page( MAC_CTRL );
time=jiffies+(HZ/4);
do{
if ( hp100_inb( VG_LAN_CFG_1 ) & HP100_LINK_CABLE_ST ) break;
} while (time>jiffies);
if ( jiffies >= time )
return 0;
hp100_andb( ~( HP100_LOAD_ADDR| HP100_LINK_CMD), VG_LAN_CFG_1);
hp100_orb( HP100_VG_SEL, VG_LAN_CFG_1);
time=jiffies+(HZ/2);
do{
if ( !(hp100_inb( VG_LAN_CFG_1) & HP100_LINK_UP_ST) ) break;
} while(time>jiffies);
#ifdef HP100_DEBUG
if (jiffies>=time)
printk("hp100: %s: down_vg_link: Link does not go down?\n", dev->name);
#endif
if ( lp->chip==HP100_CHIPID_LASSEN )
{
hp100_andb(~HP100_VG_RESET, VG_LAN_CFG_1);
udelay(1500);
hp100_orb(HP100_VG_RESET, VG_LAN_CFG_1);
udelay(1500);
}
if (lp->chip==HP100_CHIPID_LASSEN)
{
savelan=newlan=hp100_inl(10_LAN_CFG_1);
newlan &= ~(HP100_VG_SEL<<16);
newlan |= (HP100_DOT3_MAC)<<8;
hp100_andb( ~HP100_AUTO_MODE, MAC_CFG_3);
hp100_outl(newlan, 10_LAN_CFG_1);
time=jiffies+(HZ*5);
do{
if( !(hp100_inb(MAC_CFG_4) & HP100_MAC_SEL_ST) ) break;
} while(time>jiffies);
hp100_orb( HP100_AUTO_MODE, MAC_CFG_3);
hp100_outl(savelan, 10_LAN_CFG_1);
}
time=jiffies+(3*HZ);
do {
if ( (hp100_inb( VG_LAN_CFG_1 )&HP100_LINK_CABLE_ST) == 0) break;
} while (time>jiffies);
if(time<=jiffies)
{
#ifdef HP100_DEBUG
printk( "hp100: %s: down_vg_link: timeout\n", dev->name );
#endif
return -EIO;
}
time=jiffies+(2*HZ);
do {} while (time>jiffies);
return 0;
}
static int hp100_login_to_vg_hub( struct device *dev, u_short force_relogin )
{
int ioaddr = dev->base_addr;
struct hp100_private *lp = (struct hp100_private *)dev->priv;
u_short val=0;
unsigned long time;
int startst;
#ifdef HP100_DEBUG_B
hp100_outw( 0x4225, TRACE );
printk("hp100: %s: login_to_vg_hub\n", dev->name);
#endif
hp100_page( MAC_CTRL );
startst=hp100_inb( VG_LAN_CFG_1 );
if((force_relogin==TRUE)||(hp100_inb( MAC_CFG_4 )&HP100_MAC_SEL_ST))
{
#ifdef HP100_DEBUG_TRAINING
printk("hp100: %s: Start training\n", dev->name);
#endif
hp100_orb( HP100_VG_RESET , VG_LAN_CFG_1 );
if((lp->chip==HP100_CHIPID_LASSEN)&&( startst & HP100_LINK_CABLE_ST ) )
hp100_andb( ~HP100_DOT3_MAC, 10_LAN_CFG_2 );
hp100_andb( ~(HP100_LINK_CMD), VG_LAN_CFG_1);
#ifdef HP100_DEBUG_TRAINING
printk("hp100: %s: Bring down the link\n", dev->name);
#endif
time = jiffies + (HZ/10);
do {
if (~(hp100_inb( VG_LAN_CFG_1 )& HP100_LINK_UP_ST) ) break;
} while (time>jiffies);
if ( (dev->flags) & IFF_PROMISC )
{
hp100_orb( HP100_PROM_MODE, VG_LAN_CFG_2);
if(lp->chip==HP100_CHIPID_LASSEN)
hp100_orw( HP100_MACRQ_PROMSC, TRAIN_REQUEST );
}
else
{
hp100_andb( ~HP100_PROM_MODE, VG_LAN_CFG_2);
if(lp->chip==HP100_CHIPID_LASSEN)
{
hp100_andw( ~HP100_MACRQ_PROMSC, TRAIN_REQUEST );
}
}
if(lp->chip==HP100_CHIPID_LASSEN)
hp100_orb( HP100_MACRQ_FRAMEFMT_EITHER, TRAIN_REQUEST);
hp100_orb( HP100_LINK_CMD|HP100_LOAD_ADDR|HP100_VG_RESET, VG_LAN_CFG_1);
hp100_page( MAC_CTRL );
time = jiffies + ( 1*HZ );
do {
if ( hp100_inb( VG_LAN_CFG_1 ) & HP100_LINK_CABLE_ST ) break;
} while ( jiffies < time );
if ( jiffies >= time )
{
#ifdef HP100_DEBUG_TRAINING
printk( "hp100: %s: Link cable status not ok? Training aborted.\n", dev->name );
#endif
}
else
{
#ifdef HP100_DEBUG_TRAINING
printk( "hp100: %s: HUB tones detected. Trying to train.\n", dev->name);
#endif
time = jiffies + ( 2*HZ );
do {
val = hp100_inb( VG_LAN_CFG_1 );
if ( (val & ( HP100_LINK_UP_ST )) )
{
#ifdef HP100_DEBUG_TRAINING
printk( "hp100: %s: Passed training.\n", dev->name);
#endif
break;
}
} while ( time > jiffies );
}
if ( (jiffies<=time) && (val & HP100_LINK_UP_ST) )
{
#ifdef HP100_DEBUG_TRAINING
printk( "hp100: %s: Successfully logged into the HUB.\n", dev->name);
if(lp->chip==HP100_CHIPID_LASSEN)
{
val = hp100_inw(TRAIN_ALLOW);
printk( "hp100: %s: Card supports 100VG MAC Version \"%s\" ",
dev->name,(hp100_inw(TRAIN_REQUEST)&HP100_CARD_MACVER) ? "802.12" : "Pre");
printk( "Driver will use MAC Version \"%s\"\n",
( val & HP100_HUB_MACVER) ? "802.12" : "Pre" );
printk( "hp100: %s: Frame format is %s.\n",dev->name,(val&HP100_MALLOW_FRAMEFMT)?"802.5":"802.3");
}
#endif
}
else
{
printk("hp100: %s: Problem logging into the HUB.\n",dev->name);
if(lp->chip==HP100_CHIPID_LASSEN)
{
val = hp100_inw( TRAIN_ALLOW );
#ifdef HP100_DEBUG_TRAINING
printk("hp100: %s: MAC Configuration requested: 0x%04x, HUB allowed: 0x%04x\n", dev->name, hp100_inw(TRAIN_REQUEST), val);
#endif
if ( val & HP100_MALLOW_ACCDENIED )
printk("hp100: %s: HUB access denied.\n", dev->name);
if ( val & HP100_MALLOW_CONFIGURE )
printk("hp100: %s: MAC Configuration is incompatible with the Network.\n", dev->name);
if ( val & HP100_MALLOW_DUPADDR )
printk("hp100: %s: Duplicate MAC Address on the Network.\n", dev->name);
}
}
if( (lp->chip==HP100_CHIPID_LASSEN)&&(startst & HP100_LINK_CABLE_ST) )
{
hp100_page( MAC_CTRL );
hp100_orb( HP100_DOT3_MAC, 10_LAN_CFG_2 );
}
val=hp100_inb(VG_LAN_CFG_1);
hp100_page(PERFORMANCE);
hp100_outw( HP100_MISC_ERROR, IRQ_STATUS);
if (val&HP100_LINK_UP_ST)
return(0);
else
{
printk("hp100: %s: Training failed.\n", dev->name);
hp100_down_vg_link( dev );
return -EIO;
}
}
return -EIO;
}
static void hp100_cascade_reset( struct device *dev, u_short enable )
{
int ioaddr = dev->base_addr;
struct hp100_private *lp = (struct hp100_private *)dev->priv;
int i;
#ifdef HP100_DEBUG_B
hp100_outw( 0x4226, TRACE );
printk("hp100: %s: cascade_reset\n", dev->name);
#endif
if (enable==TRUE)
{
hp100_outw( HP100_HW_RST | HP100_RESET_LB, OPTION_LSW );
if(lp->chip==HP100_CHIPID_LASSEN)
{
hp100_page( HW_MAP );
hp100_andb( ~HP100_PCI_RESET, PCICTRL2 );
hp100_orb( HP100_PCI_RESET, PCICTRL2 );
for (i=0; i<0xffff; i++);
hp100_andb( ~HP100_PCI_RESET, PCICTRL2 );
hp100_page( PERFORMANCE );
}
}
else
{
hp100_outw(HP100_HW_RST|HP100_SET_LB, OPTION_LSW);
for (i=0; i<0xffff; i++ );
hp100_page(PERFORMANCE);
}
}
#ifdef HP100_DEBUG
void hp100_RegisterDump( struct device *dev )
{
int ioaddr=dev->base_addr;
int Page;
int Register;
printk("hp100: %s: Cascade Register Dump\n", dev->name);
printk("hardware id #1: 0x%.2x\n",hp100_inb(HW_ID));
printk("hardware id #2/paging: 0x%.2x\n",hp100_inb(PAGING));
printk("option #1: 0x%.4x\n",hp100_inw(OPTION_LSW));
printk("option #2: 0x%.4x\n",hp100_inw(OPTION_MSW));
for (Page = 0; Page < 8; Page++)
{
printk("page: 0x%.2x\n",Page);
outw( Page, ioaddr+0x02);
for (Register = 0x8; Register < 0x22; Register += 2)
{
if (((Register != 0x10) && (Register != 0x12)) || (Page > 0))
{
printk("0x%.2x = 0x%.4x\n",Register,inw(ioaddr+Register));
}
}
}
hp100_page(PERFORMANCE);
}
#endif
#ifdef MODULE
int hp100_port[5] = { 0, -1, -1, -1, -1 };
#ifdef LINUX_2_1
MODULE_PARM(hp100_port, "1-5i");
#endif
#ifdef LINUX_2_1
char hp100_name[5][IFNAMSIZ] = { "", "", "", "", "" };
MODULE_PARM(hp100_name, "1-5c" __MODULE_STRING(IFNAMSIZ));
#else
static char devname[5][IFNAMSIZ] = { "", "", "", "", "" };
static char *hp100_name[5] = { devname[0], devname[1],
devname[2], devname[3],
devname[4] };
#endif
static struct device *hp100_devlist[5] = { NULL, NULL, NULL, NULL, NULL };
int init_module( void )
{
int	i, cards;
if (hp100_port == 0 && !EISA_bus && !pcibios_present())
printk("hp100: You should not use auto-probing with insmod!\n");
i = -1; cards = 0;
while((hp100_port[++i] != -1) && (i < 5))
{
hp100_devlist[i] = kmalloc(sizeof(struct device), GFP_KERNEL);
memset(hp100_devlist[i], 0x00, sizeof(struct device));
hp100_devlist[i]->name = hp100_name[i];
hp100_devlist[i]->base_addr = hp100_port[i];
hp100_devlist[i]->init = &hp100_probe;
if(register_netdev(hp100_devlist[i]) != 0)
{
kfree_s(hp100_devlist[i], sizeof(struct device));
hp100_devlist[i] = (struct device *) NULL;
}
else
cards++;
}
return cards > 0 ? 0 : -ENODEV;
}
void cleanup_module( void )
{
int		i;
for(i = 0; i < 5; i++)
if(hp100_devlist[i] != (struct device *) NULL)
{
unregister_netdev( hp100_devlist[i] );
release_region( hp100_devlist[i]->base_addr, HP100_REGION_SIZE );
if( ((struct hp100_private *)hp100_devlist[i]->priv)->mode==1 )
kfree_s( ((struct hp100_private *)hp100_devlist[i]->priv)->page_vaddr, MAX_RINGSIZE+0x0f);
if ( ((struct hp100_private *)hp100_devlist[i]->priv) -> mem_ptr_virt )
iounmap( ((struct hp100_private *)hp100_devlist[i]->priv) -> mem_ptr_virt );
kfree_s( hp100_devlist[i]->priv, sizeof( struct hp100_private ) );
hp100_devlist[i]->priv = NULL;
kfree_s(hp100_devlist[i], sizeof(struct device));
hp100_devlist[i] = (struct device *) NULL;
}
}
#endif