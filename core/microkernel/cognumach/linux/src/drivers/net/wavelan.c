#include "wavelan.p.h"
static inline unsigned long
wv_splhi(void)
{
unsigned long flags;
save_flags(flags);
cli();
return(flags);
}
static inline void
wv_splx(unsigned long flags)
{
restore_flags(flags);
}
static u_char
wv_irq_to_psa(int irq)
{
if(irq < 0 || irq >= NELS(irqvals))
return 0;
return irqvals[irq];
}
static int
wv_psa_to_irq(u_char irqval)
{
int irq;
for(irq = 0; irq < NELS(irqvals); irq++)
if(irqvals[irq] == irqval)
return irq;
return -1;
}
#ifdef STRUCT_CHECK
static char *
wv_struct_check(void)
{
#define SC(t,s,n) if (sizeof(t) != s) return(n);
SC(psa_t, PSA_SIZE, "psa_t");
SC(mmw_t, MMW_SIZE, "mmw_t");
SC(mmr_t, MMR_SIZE, "mmr_t");
SC(ha_t, HA_SIZE, "ha_t");
#undef SC
return((char *) NULL);
}
#endif
static inline u_short
hasr_read(u_long ioaddr)
{
return(inw(HASR(ioaddr)));
}
static inline void
hacr_write(u_long ioaddr,
u_short hacr)
{
outw(hacr, HACR(ioaddr));
}
static inline void
hacr_write_slow(u_long ioaddr,
u_short hacr)
{
hacr_write(ioaddr, hacr);
udelay(1000L);
}
static inline void
set_chan_attn(u_long ioaddr,
u_short hacr)
{
hacr_write(ioaddr, hacr | HACR_CA);
}
static inline void
wv_hacr_reset(u_long ioaddr)
{
hacr_write_slow(ioaddr, HACR_RESET);
hacr_write(ioaddr, HACR_DEFAULT);
}
static inline void
wv_16_off(u_long ioaddr,
u_short hacr)
{
hacr &= ~HACR_16BITS;
hacr_write(ioaddr, hacr);
}
static inline void
wv_16_on(u_long ioaddr,
u_short hacr)
{
hacr |= HACR_16BITS;
hacr_write(ioaddr, hacr);
}
static inline void
wv_ints_off(device * dev)
{
net_local * lp = (net_local *)dev->priv;
u_long ioaddr = dev->base_addr;
u_long x;
x = wv_splhi();
lp->hacr &= ~HACR_INTRON;
hacr_write(ioaddr, lp->hacr);
wv_splx(x);
}
static inline void
wv_ints_on(device * dev)
{
net_local * lp = (net_local *)dev->priv;
u_long ioaddr = dev->base_addr;
u_long x;
x = wv_splhi();
lp->hacr |= HACR_INTRON;
hacr_write(ioaddr, lp->hacr);
wv_splx(x);
}
static void
psa_read(u_long ioaddr,
u_short hacr,
int o,
u_char * b,
int n)
{
wv_16_off(ioaddr, hacr);
while(n-- > 0)
{
outw(o, PIOR2(ioaddr));
o++;
*b++ = inb(PIOP2(ioaddr));
}
wv_16_on(ioaddr, hacr);
}
static void
psa_write(u_long ioaddr,
u_short hacr,
int o,
u_char * b,
int n)
{
int count = 0;
wv_16_off(ioaddr, hacr);
while(n-- > 0)
{
outw(o, PIOR2(ioaddr));
o++;
outb(*b, PIOP2(ioaddr));
b++;
count = 0;
while((count++ < 100) &&
(hasr_read(ioaddr) & HASR_PSA_BUSY))
udelay(1000);
}
wv_16_on(ioaddr, hacr);
}
#ifdef PSA_CRC
static u_short
psa_crc(u_short * psa,
int size)
{
int byte_cnt;
u_short crc_bytes = 0;
int bit_cnt;
for(byte_cnt = 0; byte_cnt <= size; byte_cnt++ )
{
crc_bytes ^= psa[byte_cnt];
for(bit_cnt = 1; bit_cnt < 9; bit_cnt++ )
{
if(crc_bytes & 0x0001)
crc_bytes = (crc_bytes >> 1) ^ 0xA001;
else
crc_bytes >>= 1 ;
}
}
return crc_bytes;
}
#endif
static inline void
mmc_out(u_long ioaddr,
u_short o,
u_char d)
{
while(inw(HASR(ioaddr)) & HASR_MMC_BUSY)
;
outw((u_short) (((u_short) d << 8) | (o << 1) | 1),
MMCR(ioaddr));
}
static inline void
mmc_write(u_long ioaddr,
u_char o,
u_char * b,
int n)
{
o += n;
b += n;
while(n-- > 0 )
mmc_out(ioaddr, --o, *(--b));
}
static inline u_char
mmc_in(u_long ioaddr,
u_short o)
{
while(inw(HASR(ioaddr)) & HASR_MMC_BUSY)
;
outw(o << 1, MMCR(ioaddr));
while(inw(HASR(ioaddr)) & HASR_MMC_BUSY)
;
return (u_char) (inw(MMCR(ioaddr)) >> 8);
}
static inline void
mmc_read(u_long ioaddr,
u_char o,
u_char * b,
int n)
{
o += n;
b += n;
while(n-- > 0)
*(--b) = mmc_in(ioaddr, --o);
}
static inline int
mmc_encr(u_long ioaddr)
{
int temp;
temp = mmc_in(ioaddr, mmroff(0, mmr_des_avail));
if((temp != MMR_DES_AVAIL_DES) && (temp != MMR_DES_AVAIL_AES))
return 0;
else
return temp;
}
static inline void
fee_wait(u_long ioaddr,
int delay,
int number)
{
int count = 0;
while((count++ < number) &&
(mmc_in(ioaddr, mmroff(0, mmr_fee_status)) & MMR_FEE_STATUS_BUSY))
udelay(delay);
}
static void
fee_read(u_long ioaddr,
u_short o,
u_short * b,
int n)
{
b += n;
mmc_out(ioaddr, mmwoff(0, mmw_fee_addr), o + n - 1);
while(n-- > 0)
{
mmc_out(ioaddr, mmwoff(0, mmw_fee_ctrl), MMW_FEE_CTRL_READ);
fee_wait(ioaddr, 10, 100);
*--b = ((mmc_in(ioaddr, mmroff(0, mmr_fee_data_h)) << 8) |
mmc_in(ioaddr, mmroff(0, mmr_fee_data_l)));
}
}
#ifdef WIRELESS_EXT
static void
fee_write(u_long ioaddr,
u_short o,
u_short * b,
int n)
{
b += n;
#ifdef EEPROM_IS_PROTECTED
#ifdef DOESNT_SEEM_TO_WORK
mmc_out(ioaddr, mmwoff(0, mmw_fee_ctrl), MMW_FEE_CTRL_PRREAD);
fee_wait(ioaddr, 10, 100);
printk("Protected 2 : %02X-%02X\n",
mmc_in(ioaddr, mmroff(0, mmr_fee_data_h)),
mmc_in(ioaddr, mmroff(0, mmr_fee_data_l)));
#endif
mmc_out(ioaddr, mmwoff(0, mmw_fee_addr), MMW_FEE_ADDR_EN);
mmc_out(ioaddr, mmwoff(0, mmw_fee_ctrl), MMW_FEE_CTRL_PREN);
fee_wait(ioaddr, 10, 100);
mmc_out(ioaddr, mmwoff(0, mmw_fee_addr), o + n);
mmc_out(ioaddr, mmwoff(0, mmw_fee_ctrl), MMW_FEE_CTRL_PRWRITE);
#ifdef DOESNT_SEEM_TO_WORK
mmc_out(ioaddr, mmwoff(0, mmw_fee_ctrl), MMW_FEE_CTRL_PRCLEAR);
#endif
fee_wait(ioaddr, 10, 100);
#endif
mmc_out(ioaddr, mmwoff(0, mmw_fee_addr), MMW_FEE_ADDR_EN);
mmc_out(ioaddr, mmwoff(0, mmw_fee_ctrl), MMW_FEE_CTRL_WREN);
fee_wait(ioaddr, 10, 100);
mmc_out(ioaddr, mmwoff(0, mmw_fee_addr), o + n - 1);
while(n-- > 0)
{
mmc_out(ioaddr, mmwoff(0, mmw_fee_data_h), (*--b) >> 8);
mmc_out(ioaddr, mmwoff(0, mmw_fee_data_l), *b & 0xFF);
mmc_out(ioaddr, mmwoff(0, mmw_fee_ctrl), MMW_FEE_CTRL_WRITE);
udelay(10000);
fee_wait(ioaddr, 10, 100);
}
mmc_out(ioaddr, mmwoff(0, mmw_fee_addr), MMW_FEE_ADDR_DS);
mmc_out(ioaddr, mmwoff(0, mmw_fee_ctrl), MMW_FEE_CTRL_WDS);
fee_wait(ioaddr, 10, 100);
#ifdef EEPROM_IS_PROTECTED
mmc_out(ioaddr, mmwoff(0, mmw_fee_addr), 0x00);
mmc_out(ioaddr, mmwoff(0, mmw_fee_ctrl), MMW_FEE_CTRL_PRWRITE);
fee_wait(ioaddr, 10, 100);
#endif
}
#endif
static void
obram_read(u_long ioaddr,
u_short o,
u_char * b,
int n)
{
outw(o, PIOR1(ioaddr));
insw(PIOP1(ioaddr), (unsigned short *) b, (n + 1) >> 1);
}
static inline void
obram_write(u_long ioaddr,
u_short o,
u_char * b,
int n)
{
outw(o, PIOR1(ioaddr));
outsw(PIOP1(ioaddr), (unsigned short *) b, (n + 1) >> 1);
}
static void
wv_ack(device * dev)
{
net_local * lp = (net_local *)dev->priv;
u_long ioaddr = dev->base_addr;
u_short scb_cs;
int i;
obram_read(ioaddr, scboff(OFFSET_SCB, scb_status),
(unsigned char *) &scb_cs, sizeof(scb_cs));
scb_cs &= SCB_ST_INT;
if(scb_cs == 0)
return;
obram_write(ioaddr, scboff(OFFSET_SCB, scb_command),
(unsigned char *) &scb_cs, sizeof(scb_cs));
set_chan_attn(ioaddr, lp->hacr);
for(i = 1000; i > 0; i--)
{
obram_read(ioaddr, scboff(OFFSET_SCB, scb_command), (unsigned char *)&scb_cs, sizeof(scb_cs));
if(scb_cs == 0)
break;
udelay(10);
}
udelay(100);
#ifdef DEBUG_CONFIG_ERROR
if(i <= 0)
printk(KERN_INFO "%s: wv_ack(): board not accepting command.\n",
dev->name);
#endif
}
static inline int
wv_synchronous_cmd(device * dev,
const char * str)
{
net_local * lp = (net_local *)dev->priv;
u_long ioaddr = dev->base_addr;
u_short scb_cmd;
ach_t cb;
int i;
scb_cmd = SCB_CMD_CUC & SCB_CMD_CUC_GO;
obram_write(ioaddr, scboff(OFFSET_SCB, scb_command),
(unsigned char *) &scb_cmd, sizeof(scb_cmd));
set_chan_attn(ioaddr, lp->hacr);
for (i = 1000; i > 0; i--)
{
obram_read(ioaddr, OFFSET_CU, (unsigned char *)&cb, sizeof(cb));
if (cb.ac_status & AC_SFLD_C)
break;
udelay(10);
}
udelay(100);
if(i <= 0 || !(cb.ac_status & AC_SFLD_OK))
{
#ifdef DEBUG_CONFIG_ERROR
printk(KERN_INFO "%s: %s failed; status = 0x%x\n",
dev->name, str, cb.ac_status);
#endif
#ifdef DEBUG_I82586_SHOW
wv_scb_show(ioaddr);
#endif
return -1;
}
wv_ack(dev);
return 0;
}
static inline int
wv_config_complete(device * dev,
u_long ioaddr,
net_local * lp)
{
unsigned short mcs_addr;
unsigned short status;
int ret;
#ifdef DEBUG_INTERRUPT_TRACE
printk(KERN_DEBUG "%s: ->wv_config_complete()\n", dev->name);
#endif
mcs_addr = lp->tx_first_in_use + sizeof(ac_tx_t) + sizeof(ac_nop_t)
+ sizeof(tbd_t) + sizeof(ac_cfg_t) + sizeof(ac_ias_t);
obram_read(ioaddr, acoff(mcs_addr, ac_status), (unsigned char *)&status, sizeof(status));
if((status & AC_SFLD_C) == 0)
ret = 0;
else
{
#ifdef DEBUG_CONFIG_ERROR
unsigned short cfg_addr;
unsigned short ias_addr;
if(status & AC_SFLD_OK != 0)
printk(KERN_INFO "wv_config_complete(): set_multicast_address failed; status = 0x%x\n",
dev->name, str, status);
ias_addr = mcs_addr - sizeof(ac_ias_t);
obram_read(ioaddr, acoff(ias_addr, ac_status), (unsigned char *)&status, sizeof(status));
if(status & AC_SFLD_OK != 0)
printk(KERN_INFO "wv_config_complete(): set_MAC_address; status = 0x%x\n",
dev->name, str, status);
cfg_addr = ias_addr - sizeof(ac_cfg_t);
obram_read(ioaddr, acoff(cfg_addr, ac_status), (unsigned char *)&status, sizeof(status));
if(status & AC_SFLD_OK != 0)
printk(KERN_INFO "wv_config_complete(): configure; status = 0x%x\n",
dev->name, str, status);
#endif
ret = 1;
}
#ifdef DEBUG_INTERRUPT_TRACE
printk(KERN_DEBUG "%s: <-wv_config_complete() - %d\n", dev->name, ret);
#endif
return ret;
}
static int
wv_complete(device * dev,
u_long ioaddr,
net_local * lp)
{
int nreaped = 0;
#ifdef DEBUG_INTERRUPT_TRACE
printk(KERN_DEBUG "%s: ->wv_complete()\n", dev->name);
#endif
while(lp->tx_first_in_use != I82586NULL)
{
unsigned short tx_status;
obram_read(ioaddr, acoff(lp->tx_first_in_use, ac_status), (unsigned char *)&tx_status, sizeof(tx_status));
if(tx_status == 0xFFFF)
if(!wv_config_complete(dev, ioaddr, lp))
break;
if((tx_status & AC_SFLD_C) == 0)
break;
nreaped++;
--lp->tx_n_in_use;
if(lp->tx_n_in_use <= 0)
lp->tx_first_in_use = I82586NULL;
else
{
lp->tx_first_in_use += TXBLOCKZ;
if(lp->tx_first_in_use >= OFFSET_CU + NTXBLOCKS * TXBLOCKZ)
lp->tx_first_in_use -= NTXBLOCKS * TXBLOCKZ;
}
if(tx_status == 0xFFFF)
continue;
if(tx_status & AC_SFLD_OK)
{
int ncollisions;
lp->stats.tx_packets++;
ncollisions = tx_status & AC_SFLD_MAXCOL;
lp->stats.collisions += ncollisions;
#ifdef DEBUG_INTERRUPT_INFO
if(ncollisions > 0)
printk(KERN_DEBUG "%s: wv_complete(): tx completed after %d collisions.\n",
dev->name, ncollisions);
#endif
}
else
{
lp->stats.tx_errors++;
#ifndef IGNORE_NORMAL_XMIT_ERRS
if(tx_status & AC_SFLD_S10)
{
lp->stats.tx_carrier_errors++;
#ifdef DEBUG_INTERRUPT_ERROR
printk(KERN_INFO "%s: wv_complete(): tx error: no CS.\n",
dev->name);
#endif
}
#endif
if(tx_status & AC_SFLD_S9)
{
lp->stats.tx_carrier_errors++;
#ifdef DEBUG_INTERRUPT_ERROR
printk(KERN_INFO "%s: wv_complete(): tx error: lost CTS.\n",
dev->name);
#endif
}
if(tx_status & AC_SFLD_S8)
{
lp->stats.tx_fifo_errors++;
#ifdef DEBUG_INTERRUPT_ERROR
printk(KERN_INFO "%s: wv_complete(): tx error: slow DMA.\n",
dev->name);
#endif
}
#ifndef IGNORE_NORMAL_XMIT_ERRS
if(tx_status & AC_SFLD_S6)
{
lp->stats.tx_heartbeat_errors++;
#ifdef DEBUG_INTERRUPT_ERROR
printk(KERN_INFO "%s: wv_complete(): tx error: heart beat.\n",
dev->name);
#endif
}
if(tx_status & AC_SFLD_S5)
{
lp->stats.tx_aborted_errors++;
#ifdef DEBUG_INTERRUPT_ERROR
printk(KERN_INFO "%s: wv_complete(): tx error: too many collisions.\n",
dev->name);
#endif
}
#endif
}
#ifdef DEBUG_INTERRUPT_INFO
printk(KERN_DEBUG "%s: wv_complete(): tx completed, tx_status 0x%04x\n",
dev->name, tx_status);
#endif
}
#ifdef DEBUG_INTERRUPT_INFO
if(nreaped > 1)
printk(KERN_DEBUG "%s: wv_complete(): reaped %d\n", dev->name, nreaped);
#endif
if(lp->tx_n_in_use < NTXBLOCKS - 1)
{
dev->tbusy = 0;
mark_bh(NET_BH);
}
#ifdef DEBUG_INTERRUPT_TRACE
printk(KERN_DEBUG "%s: <-wv_complete()\n", dev->name);
#endif
return nreaped;
}
static inline void
wv_82586_reconfig(device * dev)
{
net_local * lp = (net_local *)dev->priv;
if(!(dev->start) || (set_bit(0, (void *)&dev->tbusy) != 0))
{
lp->reconfig_82586 = 1;
#ifdef DEBUG_CONFIG_INFO
printk(KERN_DEBUG "%s: wv_82586_reconfig(): delayed (busy = %ld, start = %d)\n",
dev->name, dev->tbusy, dev->start);
#endif
}
else
wv_82586_config(dev);
}
#ifdef DEBUG_PSA_SHOW
static void
wv_psa_show(psa_t * p)
{
printk(KERN_DEBUG "##### WaveLAN psa contents: #####\n");
printk(KERN_DEBUG "psa_io_base_addr_1: 0x%02X %02X %02X %02X\n",
p->psa_io_base_addr_1,
p->psa_io_base_addr_2,
p->psa_io_base_addr_3,
p->psa_io_base_addr_4);
printk(KERN_DEBUG "psa_rem_boot_addr_1: 0x%02X %02X %02X\n",
p->psa_rem_boot_addr_1,
p->psa_rem_boot_addr_2,
p->psa_rem_boot_addr_3);
printk(KERN_DEBUG "psa_holi_params: 0x%02x, ", p->psa_holi_params);
printk("psa_int_req_no: %d\n", p->psa_int_req_no);
#ifdef DEBUG_SHOW_UNUSED
printk(KERN_DEBUG "psa_unused0[]: %02X:%02X:%02X:%02X:%02X:%02X:%02X\n",
p->psa_unused0[0],
p->psa_unused0[1],
p->psa_unused0[2],
p->psa_unused0[3],
p->psa_unused0[4],
p->psa_unused0[5],
p->psa_unused0[6]);
#endif
printk(KERN_DEBUG "psa_univ_mac_addr[]: %02x:%02x:%02x:%02x:%02x:%02x\n",
p->psa_univ_mac_addr[0],
p->psa_univ_mac_addr[1],
p->psa_univ_mac_addr[2],
p->psa_univ_mac_addr[3],
p->psa_univ_mac_addr[4],
p->psa_univ_mac_addr[5]);
printk(KERN_DEBUG "psa_local_mac_addr[]: %02x:%02x:%02x:%02x:%02x:%02x\n",
p->psa_local_mac_addr[0],
p->psa_local_mac_addr[1],
p->psa_local_mac_addr[2],
p->psa_local_mac_addr[3],
p->psa_local_mac_addr[4],
p->psa_local_mac_addr[5]);
printk(KERN_DEBUG "psa_univ_local_sel: %d, ", p->psa_univ_local_sel);
printk("psa_comp_number: %d, ", p->psa_comp_number);
printk("psa_thr_pre_set: 0x%02x\n", p->psa_thr_pre_set);
printk(KERN_DEBUG "psa_feature_select/decay_prm: 0x%02x, ",
p->psa_feature_select);
printk("psa_subband/decay_update_prm: %d\n", p->psa_subband);
printk(KERN_DEBUG "psa_quality_thr: 0x%02x, ", p->psa_quality_thr);
printk("psa_mod_delay: 0x%02x\n", p->psa_mod_delay);
printk(KERN_DEBUG "psa_nwid: 0x%02x%02x, ", p->psa_nwid[0], p->psa_nwid[1]);
printk("psa_nwid_select: %d\n", p->psa_nwid_select);
printk(KERN_DEBUG "psa_encryption_select: %d, ", p->psa_encryption_select);
printk("psa_encryption_key[]: %02x:%02x:%02x:%02x:%02x:%02x:%02x:%02x\n",
p->psa_encryption_key[0],
p->psa_encryption_key[1],
p->psa_encryption_key[2],
p->psa_encryption_key[3],
p->psa_encryption_key[4],
p->psa_encryption_key[5],
p->psa_encryption_key[6],
p->psa_encryption_key[7]);
printk(KERN_DEBUG "psa_databus_width: %d\n", p->psa_databus_width);
printk(KERN_DEBUG "psa_call_code/auto_squelch: 0x%02x, ",
p->psa_call_code[0]);
printk("psa_call_code[]: %02X:%02X:%02X:%02X:%02X:%02X:%02X:%02X\n",
p->psa_call_code[0],
p->psa_call_code[1],
p->psa_call_code[2],
p->psa_call_code[3],
p->psa_call_code[4],
p->psa_call_code[5],
p->psa_call_code[6],
p->psa_call_code[7]);
#ifdef DEBUG_SHOW_UNUSED
printk(KERN_DEBUG "psa_reserved[]: %02X:%02X:%02X:%02X\n",
p->psa_reserved[0],
p->psa_reserved[1],
p->psa_reserved[2],
p->psa_reserved[3]);
#endif
printk(KERN_DEBUG "psa_conf_status: %d, ", p->psa_conf_status);
printk("psa_crc: 0x%02x%02x, ", p->psa_crc[0], p->psa_crc[1]);
printk("psa_crc_status: 0x%02x\n", p->psa_crc_status);
}
#endif
#ifdef DEBUG_MMC_SHOW
static void
wv_mmc_show(device * dev)
{
u_long ioaddr = dev->base_addr;
net_local * lp = (net_local *)dev->priv;
mmr_t m;
if(hasr_read(ioaddr) & HASR_NO_CLK)
{
printk(KERN_WARNING "%s: wv_mmc_show: modem not connected\n",
dev->name);
return;
}
mmc_out(ioaddr, mmwoff(0, mmw_freeze), 1);
mmc_read(ioaddr, 0, (u_char *)&m, sizeof(m));
mmc_out(ioaddr, mmwoff(0, mmw_freeze), 0);
#ifdef WIRELESS_EXT
lp->wstats.discard.nwid += (m.mmr_wrong_nwid_h << 8) | m.mmr_wrong_nwid_l;
#endif
printk(KERN_DEBUG "##### WaveLAN modem status registers: #####\n");
#ifdef DEBUG_SHOW_UNUSED
printk(KERN_DEBUG "mmc_unused0[]: %02X:%02X:%02X:%02X:%02X:%02X:%02X:%02X\n",
m.mmr_unused0[0],
m.mmr_unused0[1],
m.mmr_unused0[2],
m.mmr_unused0[3],
m.mmr_unused0[4],
m.mmr_unused0[5],
m.mmr_unused0[6],
m.mmr_unused0[7]);
#endif
printk(KERN_DEBUG "Encryption algorythm: %02X - Status: %02X\n",
m.mmr_des_avail, m.mmr_des_status);
#ifdef DEBUG_SHOW_UNUSED
printk(KERN_DEBUG "mmc_unused1[]: %02X:%02X:%02X:%02X:%02X\n",
m.mmr_unused1[0],
m.mmr_unused1[1],
m.mmr_unused1[2],
m.mmr_unused1[3],
m.mmr_unused1[4]);
#endif
printk(KERN_DEBUG "dce_status: 0x%x [%s%s%s%s]\n",
m.mmr_dce_status,
(m.mmr_dce_status & MMR_DCE_STATUS_RX_BUSY) ? "energy detected,":"",
(m.mmr_dce_status & MMR_DCE_STATUS_LOOPT_IND) ?
"loop test indicated," : "",
(m.mmr_dce_status & MMR_DCE_STATUS_TX_BUSY) ? "transmitter on," : "",
(m.mmr_dce_status & MMR_DCE_STATUS_JBR_EXPIRED) ?
"jabber timer expired," : "");
printk(KERN_DEBUG "Dsp ID: %02X\n",
m.mmr_dsp_id);
#ifdef DEBUG_SHOW_UNUSED
printk(KERN_DEBUG "mmc_unused2[]: %02X:%02X\n",
m.mmr_unused2[0],
m.mmr_unused2[1]);
#endif
printk(KERN_DEBUG "# correct_nwid: %d, # wrong_nwid: %d\n",
(m.mmr_correct_nwid_h << 8) | m.mmr_correct_nwid_l,
(m.mmr_wrong_nwid_h << 8) | m.mmr_wrong_nwid_l);
printk(KERN_DEBUG "thr_pre_set: 0x%x [current signal %s]\n",
m.mmr_thr_pre_set & MMR_THR_PRE_SET,
(m.mmr_thr_pre_set & MMR_THR_PRE_SET_CUR) ? "above" : "below");
printk(KERN_DEBUG "signal_lvl: %d [%s], ",
m.mmr_signal_lvl & MMR_SIGNAL_LVL,
(m.mmr_signal_lvl & MMR_SIGNAL_LVL_VALID) ? "new msg" : "no new msg");
printk("silence_lvl: %d [%s], ", m.mmr_silence_lvl & MMR_SILENCE_LVL,
(m.mmr_silence_lvl & MMR_SILENCE_LVL_VALID) ? "update done" : "no new update");
printk("sgnl_qual: 0x%x [%s]\n",
m.mmr_sgnl_qual & MMR_SGNL_QUAL,
(m.mmr_sgnl_qual & MMR_SGNL_QUAL_ANT) ? "Antenna 1" : "Antenna 0");
#ifdef DEBUG_SHOW_UNUSED
printk(KERN_DEBUG "netw_id_l: %x\n", m.mmr_netw_id_l);
#endif
}
#endif
#ifdef DEBUG_I82586_SHOW
static void
wv_scb_show(u_long ioaddr)
{
scb_t scb;
obram_read(ioaddr, OFFSET_SCB, (unsigned char *)&scb, sizeof(scb));
printk(KERN_DEBUG "##### WaveLAN system control block: #####\n");
printk(KERN_DEBUG "status: ");
printk("stat 0x%x[%s%s%s%s] ",
(scb.scb_status & (SCB_ST_CX | SCB_ST_FR | SCB_ST_CNA | SCB_ST_RNR)) >> 12,
(scb.scb_status & SCB_ST_CX) ? "cmd completion interrupt," : "",
(scb.scb_status & SCB_ST_FR) ? "frame received," : "",
(scb.scb_status & SCB_ST_CNA) ? "cmd unit not active," : "",
(scb.scb_status & SCB_ST_RNR) ? "rcv unit not ready," : "");
printk("cus 0x%x[%s%s%s] ",
(scb.scb_status & SCB_ST_CUS) >> 8,
((scb.scb_status & SCB_ST_CUS) == SCB_ST_CUS_IDLE) ? "idle" : "",
((scb.scb_status & SCB_ST_CUS) == SCB_ST_CUS_SUSP) ? "suspended" : "",
((scb.scb_status & SCB_ST_CUS) == SCB_ST_CUS_ACTV) ? "active" : "");
printk("rus 0x%x[%s%s%s%s]\n",
(scb.scb_status & SCB_ST_RUS) >> 4,
((scb.scb_status & SCB_ST_RUS) == SCB_ST_RUS_IDLE) ? "idle" : "",
((scb.scb_status & SCB_ST_RUS) == SCB_ST_RUS_SUSP) ? "suspended" : "",
((scb.scb_status & SCB_ST_RUS) == SCB_ST_RUS_NRES) ? "no resources" : "",
((scb.scb_status & SCB_ST_RUS) == SCB_ST_RUS_RDY) ? "ready" : "");
printk(KERN_DEBUG "command: ");
printk("ack 0x%x[%s%s%s%s] ",
(scb.scb_command & (SCB_CMD_ACK_CX | SCB_CMD_ACK_FR | SCB_CMD_ACK_CNA | SCB_CMD_ACK_RNR)) >> 12,
(scb.scb_command & SCB_CMD_ACK_CX) ? "ack cmd completion," : "",
(scb.scb_command & SCB_CMD_ACK_FR) ? "ack frame received," : "",
(scb.scb_command & SCB_CMD_ACK_CNA) ? "ack CU not active," : "",
(scb.scb_command & SCB_CMD_ACK_RNR) ? "ack RU not ready," : "");
printk("cuc 0x%x[%s%s%s%s%s] ",
(scb.scb_command & SCB_CMD_CUC) >> 8,
((scb.scb_command & SCB_CMD_CUC) == SCB_CMD_CUC_NOP) ? "nop" : "",
((scb.scb_command & SCB_CMD_CUC) == SCB_CMD_CUC_GO) ? "start cbl_offset" : "",
((scb.scb_command & SCB_CMD_CUC) == SCB_CMD_CUC_RES) ? "resume execution" : "",
((scb.scb_command & SCB_CMD_CUC) == SCB_CMD_CUC_SUS) ? "suspend execution" : "",
((scb.scb_command & SCB_CMD_CUC) == SCB_CMD_CUC_ABT) ? "abort execution" : "");
printk("ruc 0x%x[%s%s%s%s%s]\n",
(scb.scb_command & SCB_CMD_RUC) >> 4,
((scb.scb_command & SCB_CMD_RUC) == SCB_CMD_RUC_NOP) ? "nop" : "",
((scb.scb_command & SCB_CMD_RUC) == SCB_CMD_RUC_GO) ? "start rfa_offset" : "",
((scb.scb_command & SCB_CMD_RUC) == SCB_CMD_RUC_RES) ? "resume reception" : "",
((scb.scb_command & SCB_CMD_RUC) == SCB_CMD_RUC_SUS) ? "suspend reception" : "",
((scb.scb_command & SCB_CMD_RUC) == SCB_CMD_RUC_ABT) ? "abort reception" : "");
printk(KERN_DEBUG "cbl_offset 0x%x ", scb.scb_cbl_offset);
printk("rfa_offset 0x%x\n", scb.scb_rfa_offset);
printk(KERN_DEBUG "crcerrs %d ", scb.scb_crcerrs);
printk("alnerrs %d ", scb.scb_alnerrs);
printk("rscerrs %d ", scb.scb_rscerrs);
printk("ovrnerrs %d\n", scb.scb_ovrnerrs);
}
static void
wv_ru_show(device * dev)
{
printk(KERN_DEBUG "##### WaveLAN i82586 receiver unit status: #####\n");
printk(KERN_DEBUG "ru:");
printk("\n");
}
static void
wv_cu_show_one(device * dev,
net_local * lp,
int i,
u_short p)
{
u_long ioaddr;
ac_tx_t actx;
ioaddr = dev->base_addr;
printk("%d: 0x%x:", i, p);
obram_read(ioaddr, p, (unsigned char *)&actx, sizeof(actx));
printk(" status=0x%x,", actx.tx_h.ac_status);
printk(" command=0x%x,", actx.tx_h.ac_command);
printk("|");
}
static void
wv_cu_show(device * dev)
{
net_local * lp = (net_local *)dev->priv;
unsigned int i;
u_short p;
printk(KERN_DEBUG "##### WaveLAN i82586 command unit status: #####\n");
printk(KERN_DEBUG);
for(i = 0, p = lp->tx_first_in_use; i < NTXBLOCKS; i++)
{
wv_cu_show_one(dev, lp, i, p);
p += TXBLOCKZ;
if(p >= OFFSET_CU + NTXBLOCKS * TXBLOCKZ)
p -= NTXBLOCKS * TXBLOCKZ;
}
printk("\n");
}
#endif
#ifdef DEBUG_DEVICE_SHOW
static void
wv_dev_show(device * dev)
{
printk(KERN_DEBUG "dev:");
printk(" start=%d,", dev->start);
printk(" tbusy=%ld,", dev->tbusy);
printk(" interrupt=%d,", dev->interrupt);
printk(" trans_start=%ld,", dev->trans_start);
printk(" flags=0x%x,", dev->flags);
printk("\n");
}
static void
wv_local_show(device * dev)
{
net_local *lp;
lp = (net_local *)dev->priv;
printk(KERN_DEBUG "local:");
printk(" tx_n_in_use=%d,", lp->tx_n_in_use);
printk(" hacr=0x%x,", lp->hacr);
printk(" rx_head=0x%x,", lp->rx_head);
printk(" rx_last=0x%x,", lp->rx_last);
printk(" tx_first_free=0x%x,", lp->tx_first_free);
printk(" tx_first_in_use=0x%x,", lp->tx_first_in_use);
printk("\n");
}
#endif
#if defined(DEBUG_RX_INFO) || defined(DEBUG_TX_INFO)
static inline void
wv_packet_info(u_char * p,
int length,
char * msg1,
char * msg2)
{
#ifndef DEBUG_PACKET_DUMP
printk(KERN_DEBUG "%s: %s(): dest %02X:%02X:%02X:%02X:%02X:%02X, length %d\n",
msg1, msg2, p[0], p[1], p[2], p[3], p[4], p[5], length);
printk(KERN_DEBUG "%s: %s(): src %02X:%02X:%02X:%02X:%02X:%02X, type 0x%02X%02X\n",
msg1, msg2, p[6], p[7], p[8], p[9], p[10], p[11], p[12], p[13]);
#else
int i;
int maxi;
printk(KERN_DEBUG "%s: %s(): len=%d, data=\"", msg1, msg2, length);
if((maxi = length) > DEBUG_PACKET_DUMP)
maxi = DEBUG_PACKET_DUMP;
for(i = 0; i < maxi; i++)
if(p[i] >= ' ' && p[i] <= '~')
printk(" %c", p[i]);
else
printk("%02X", p[i]);
if(maxi < length)
printk("..");
printk("\"\n");
printk(KERN_DEBUG "\n");
#endif
}
#endif
static inline void
wv_init_info(device * dev)
{
short ioaddr = dev->base_addr;
net_local * lp = (net_local *)dev->priv;
psa_t psa;
int i;
psa_read(ioaddr, lp->hacr, 0, (unsigned char *) &psa, sizeof(psa));
#ifdef DEBUG_PSA_SHOW
wv_psa_show(&psa);
#endif
#ifdef DEBUG_MMC_SHOW
wv_mmc_show(dev);
#endif
#ifdef DEBUG_I82586_SHOW
wv_cu_show(dev);
#endif
#ifdef DEBUG_BASIC_SHOW
printk(KERN_NOTICE "%s: WaveLAN at %#x,", dev->name, ioaddr);
for(i = 0; i < WAVELAN_ADDR_SIZE; i++)
printk("%s%02X", (i == 0) ? " " : ":", dev->dev_addr[i]);
printk(", IRQ %d", dev->irq);
if(psa.psa_nwid_select)
printk(", nwid 0x%02X-%02X", psa.psa_nwid[0], psa.psa_nwid[1]);
else
printk(", nwid off");
if(!(mmc_in(ioaddr, mmroff(0, mmr_fee_status)) &
(MMR_FEE_STATUS_DWLD | MMR_FEE_STATUS_BUSY)))
{
unsigned short freq;
fee_read(ioaddr, 0x00 ,
&freq, 1);
printk(", 2.00, %ld", (freq >> 6) + 2400L);
if(freq & 0x20)
printk(".5");
}
else
{
printk(", PC");
switch(psa.psa_comp_number)
{
case PSA_COMP_PC_AT_915:
case PSA_COMP_PC_AT_2400:
printk("-AT");
break;
case PSA_COMP_PC_MC_915:
case PSA_COMP_PC_MC_2400:
printk("-MC");
break;
case PSA_COMP_PCMCIA_915:
printk("MCIA");
break;
default:
printk("???");
}
printk(", ");
switch (psa.psa_subband)
{
case PSA_SUBBAND_915:
printk("915");
break;
case PSA_SUBBAND_2425:
printk("2425");
break;
case PSA_SUBBAND_2460:
printk("2460");
break;
case PSA_SUBBAND_2484:
printk("2484");
break;
case PSA_SUBBAND_2430_5:
printk("2430.5");
break;
default:
printk("???");
}
}
printk(" MHz\n");
#endif
#ifdef DEBUG_VERSION_SHOW
printk(KERN_NOTICE "%s", version);
#endif
}
static en_stats *
wavelan_get_stats(device * dev)
{
#ifdef DEBUG_IOCTL_TRACE
printk(KERN_DEBUG "%s: <>wavelan_get_stats()\n", dev->name);
#endif
return(&((net_local *) dev->priv)->stats);
}
static void
wavelan_set_multicast_list(device * dev)
{
net_local * lp = (net_local *) dev->priv;
#ifdef DEBUG_IOCTL_TRACE
printk(KERN_DEBUG "%s: ->wavelan_set_multicast_list()\n", dev->name);
#endif
#ifdef DEBUG_IOCTL_INFO
printk(KERN_DEBUG "%s: wavelan_set_multicast_list(): setting Rx mode %02X to %d addresses.\n",
dev->name, dev->flags, dev->mc_count);
#endif
if((dev->flags & IFF_PROMISC) ||
(dev->flags & IFF_ALLMULTI) ||
(dev->mc_count > I82586_MAX_MULTICAST_ADDRESSES))
{
if(!lp->promiscuous)
{
lp->promiscuous = 1;
lp->mc_count = 0;
wv_82586_reconfig(dev);
dev->flags |= IFF_PROMISC;
}
}
else
if(dev->mc_list != (struct dev_mc_list *) NULL)
{
#ifdef MULTICAST_AVOID
if(lp->promiscuous ||
(dev->mc_count != lp->mc_count))
#endif
{
lp->promiscuous = 0;
lp->mc_count = dev->mc_count;
wv_82586_reconfig(dev);
}
}
else
{
if(lp->promiscuous || lp->mc_count == 0)
{
lp->promiscuous = 0;
lp->mc_count = 0;
wv_82586_reconfig(dev);
}
}
#ifdef DEBUG_IOCTL_TRACE
printk(KERN_DEBUG "%s: <-wavelan_set_multicast_list()\n", dev->name);
#endif
}
static int
wavelan_set_mac_address(device * dev,
void * addr)
{
struct sockaddr * mac = addr;
memcpy(dev->dev_addr, mac->sa_data, WAVELAN_ADDR_SIZE);
wv_82586_reconfig(dev);
return 0;
}
#ifdef WIRELESS_EXT
static inline int
wv_set_frequency(u_long ioaddr,
iw_freq * frequency)
{
const int BAND_NUM = 10;
long freq = 0L;
#ifdef DEBUG_IOCTL_INFO
int i;
#endif
if((frequency->e == 1) &&
(frequency->m >= (int) 2.412e8) && (frequency->m <= (int) 2.487e8))
{
freq = ((frequency->m / 10000) - 24000L) / 5;
}
if((frequency->e == 0) &&
(frequency->m >= 0) && (frequency->m < BAND_NUM))
{
short bands[] = { 0x30, 0x58, 0x64, 0x7A, 0x80, 0xA8, 0xD0, 0xF0, 0xF8, 0x150 };
freq = bands[frequency->m] >> 1;
}
if(freq != 0L)
{
u_short table[10];
fee_read(ioaddr, 0x71 ,
table, 10);
#ifdef DEBUG_IOCTL_INFO
printk(KERN_DEBUG "Frequency table :");
for(i = 0; i < 10; i++)
{
printk(" %04X",
table[i]);
}
printk("\n");
#endif
if(!(table[9 - ((freq - 24) / 16)] &
(1 << ((freq - 24) % 16))))
return -EINVAL;
}
else
return -EINVAL;
if(freq != 0L)
{
unsigned short area[16];
unsigned short dac[2];
unsigned short area_verify[16];
unsigned short dac_verify[2];
unsigned short power_limit[] = { 40, 80, 120, 160, 0 };
int power_band = 0;
unsigned short power_adjust;
power_band = 0;
while((freq > power_limit[power_band]) &&
(power_limit[++power_band] != 0))
;
fee_read(ioaddr, 0x00,
area, 16);
fee_read(ioaddr, 0x60,
dac, 2);
fee_read(ioaddr, 0x6B - (power_band >> 1),
&power_adjust, 1);
if(power_band & 0x1)
power_adjust >>= 8;
else
power_adjust &= 0xFF;
#ifdef DEBUG_IOCTL_INFO
printk(KERN_DEBUG "WaveLAN EEPROM Area 1:");
for(i = 0; i < 16; i++)
{
printk(" %04X",
area[i]);
}
printk("\n");
printk(KERN_DEBUG "WaveLAN EEPROM DAC: %04X %04X\n",
dac[0], dac[1]);
#endif
area[0] = ((freq << 5) & 0xFFE0) | (area[0] & 0x1F);
area[3] = (freq >> 1) + 2400L - 352L;
area[2] = ((freq & 0x1) << 4) | (area[2] & 0xFFEF);
area[13] = (freq >> 1) + 2400L;
area[12] = ((freq & 0x1) << 4) | (area[2] & 0xFFEF);
dac[1] = ((power_adjust >> 1) & 0x7F) | (dac[1] & 0xFF80);
dac[0] = ((power_adjust & 0x1) << 4) | (dac[0] & 0xFFEF);
fee_write(ioaddr, 0x00,
area, 16);
fee_write(ioaddr, 0x60,
dac, 2);
fee_read(ioaddr, 0x00,
area_verify, 16);
fee_read(ioaddr, 0x60,
dac_verify, 2);
if(memcmp(area, area_verify, 16 * 2) ||
memcmp(dac, dac_verify, 2 * 2))
{
#ifdef DEBUG_IOCTL_ERROR
printk(KERN_INFO "WaveLAN: wv_set_frequency: unable to write new frequency to EEPROM(?).\n");
#endif
return -EOPNOTSUPP;
}
mmc_out(ioaddr, mmwoff(0, mmw_fee_addr), 0x0F);
mmc_out(ioaddr, mmwoff(0, mmw_fee_ctrl),
MMW_FEE_CTRL_READ | MMW_FEE_CTRL_DWLD);
fee_wait(ioaddr, 100, 100);
mmc_out(ioaddr, mmwoff(0, mmw_fee_addr), 0x61);
mmc_out(ioaddr, mmwoff(0, mmw_fee_ctrl),
MMW_FEE_CTRL_READ | MMW_FEE_CTRL_DWLD);
fee_wait(ioaddr, 100, 100);
#ifdef DEBUG_IOCTL_INFO
printk(KERN_DEBUG "WaveLAN EEPROM Area 1:");
for(i = 0; i < 16; i++)
{
printk(" %04X",
area_verify[i]);
}
printk("\n");
printk(KERN_DEBUG "WaveLAN EEPROM DAC: %04X %04X\n",
dac_verify[0], dac_verify[1]);
#endif
return 0;
}
else
return -EINVAL;
}
static inline int
wv_frequency_list(u_long ioaddr,
iw_freq * list,
int max)
{
u_short table[10];
long freq = 0L;
int i;
fee_read(ioaddr, 0x71 ,
table, 10);
i = 0;
for(freq = 0; freq < 150; freq++)
if(table[9 - (freq / 16)] & (1 << (freq % 16)))
{
list[i].m = (((freq + 24) * 5) + 24000L) * 10000;
list[i++].e = 1;
if(i >= max)
return(i);
}
return(i);
}
#ifdef WIRELESS_SPY
static inline void
wl_spy_gather(device * dev,
u_char * mac,
u_char * stats)
{
net_local * lp = (net_local *) dev->priv;
int i;
for(i = 0; i < lp->spy_number; i++)
if(!memcmp(mac, lp->spy_address[i], WAVELAN_ADDR_SIZE))
{
lp->spy_stat[i].qual = stats[2] & MMR_SGNL_QUAL;
lp->spy_stat[i].level = stats[0] & MMR_SIGNAL_LVL;
lp->spy_stat[i].noise = stats[1] & MMR_SILENCE_LVL;
lp->spy_stat[i].updated = 0x7;
}
}
#endif
#ifdef HISTOGRAM
static inline void
wl_his_gather(device * dev,
u_char * stats)
{
net_local * lp = (net_local *) dev->priv;
u_char level = stats[0] & MMR_SIGNAL_LVL;
int i;
i = 0;
while((i < (lp->his_number - 1)) && (level >= lp->his_range[i++]))
;
(lp->his_sum[i])++;
}
#endif
static int
wavelan_ioctl(struct device * dev,
struct ifreq * rq,
int cmd)
{
u_long ioaddr = dev->base_addr;
net_local * lp = (net_local *)dev->priv;
struct iwreq * wrq = (struct iwreq *) rq;
psa_t psa;
mm_t m;
unsigned long x;
int ret = 0;
#ifdef DEBUG_IOCTL_TRACE
printk(KERN_DEBUG "%s: ->wavelan_ioctl(cmd=0x%X)\n", dev->name, cmd);
#endif
x = wv_splhi();
switch(cmd)
{
case SIOCGIWNAME:
strcpy(wrq->u.name, "Wavelan");
break;
case SIOCSIWNWID:
if(wrq->u.nwid.on)
{
psa.psa_nwid[0] = (wrq->u.nwid.nwid & 0xFF00) >> 8;
psa.psa_nwid[1] = wrq->u.nwid.nwid & 0xFF;
psa.psa_nwid_select = 0x01;
psa_write(ioaddr, lp->hacr, (char *)psa.psa_nwid - (char *)&psa,
(unsigned char *)psa.psa_nwid, 3);
m.w.mmw_netw_id_l = wrq->u.nwid.nwid & 0xFF;
m.w.mmw_netw_id_h = (wrq->u.nwid.nwid & 0xFF00) >> 8;
mmc_write(ioaddr, (char *)&m.w.mmw_netw_id_l - (char *)&m,
(unsigned char *)&m.w.mmw_netw_id_l, 2);
mmc_out(ioaddr, mmwoff(0, mmw_loopt_sel), 0x00);
}
else
{
psa.psa_nwid_select = 0x00;
psa_write(ioaddr, lp->hacr,
(char *)&psa.psa_nwid_select - (char *)&psa,
(unsigned char *)&psa.psa_nwid_select, 1);
mmc_out(ioaddr, mmwoff(0, mmw_loopt_sel), MMW_LOOPT_SEL_DIS_NWID);
}
break;
case SIOCGIWNWID:
psa_read(ioaddr, lp->hacr, (char *)psa.psa_nwid - (char *)&psa,
(unsigned char *)psa.psa_nwid, 3);
wrq->u.nwid.nwid = (psa.psa_nwid[0] << 8) + psa.psa_nwid[1];
wrq->u.nwid.on = psa.psa_nwid_select;
break;
case SIOCSIWFREQ:
if(!(mmc_in(ioaddr, mmroff(0, mmr_fee_status)) &
(MMR_FEE_STATUS_DWLD | MMR_FEE_STATUS_BUSY)))
ret = wv_set_frequency(ioaddr, &(wrq->u.freq));
else
ret = -EOPNOTSUPP;
break;
case SIOCGIWFREQ:
if(!(mmc_in(ioaddr, mmroff(0, mmr_fee_status)) &
(MMR_FEE_STATUS_DWLD | MMR_FEE_STATUS_BUSY)))
{
unsigned short freq;
fee_read(ioaddr, 0x00 ,
&freq, 1);
wrq->u.freq.m = ((freq >> 5) * 5 + 24000L) * 10000;
wrq->u.freq.e = 1;
}
else
{
int bands[] = { 915e6, 2.425e8, 2.46e8, 2.484e8, 2.4305e8 };
psa_read(ioaddr, lp->hacr, (char *)&psa.psa_subband - (char *)&psa,
(unsigned char *)&psa.psa_subband, 1);
if(psa.psa_subband <= 4)
{
wrq->u.freq.m = bands[psa.psa_subband];
wrq->u.freq.e = (psa.psa_subband != 0);
}
else
ret = -EOPNOTSUPP;
}
break;
case SIOCSIWSENS:
if(!suser())
return -EPERM;
psa.psa_thr_pre_set = wrq->u.sensitivity & 0x3F;
psa_write(ioaddr, lp->hacr, (char *)&psa.psa_thr_pre_set - (char *)&psa,
(unsigned char *) &psa.psa_thr_pre_set, 1);
mmc_out(ioaddr, mmwoff(0, mmw_thr_pre_set), psa.psa_thr_pre_set);
break;
case SIOCGIWSENS:
psa_read(ioaddr, lp->hacr, (char *)&psa.psa_thr_pre_set - (char *)&psa,
(unsigned char *) &psa.psa_thr_pre_set, 1);
wrq->u.sensitivity = psa.psa_thr_pre_set & 0x3F;
break;
case SIOCSIWENCODE:
if(!mmc_encr(ioaddr))
{
ret = -EOPNOTSUPP;
break;
}
if(wrq->u.encoding.method)
{
int i;
long long key = wrq->u.encoding.code;
for(i = 7; i >= 0; i--)
{
psa.psa_encryption_key[i] = key & 0xFF;
key >>= 8;
}
psa.psa_encryption_select = 1;
psa_write(ioaddr, lp->hacr,
(char *) &psa.psa_encryption_select - (char *) &psa,
(unsigned char *) &psa.psa_encryption_select, 8+1);
mmc_out(ioaddr, mmwoff(0, mmw_encr_enable),
MMW_ENCR_ENABLE_EN | MMW_ENCR_ENABLE_MODE);
mmc_write(ioaddr, mmwoff(0, mmw_encr_key),
(unsigned char *) &psa.psa_encryption_key, 8);
}
else
{
psa.psa_encryption_select = 0;
psa_write(ioaddr, lp->hacr,
(char *) &psa.psa_encryption_select - (char *) &psa,
(unsigned char *) &psa.psa_encryption_select, 1);
mmc_out(ioaddr, mmwoff(0, mmw_encr_enable), 0);
}
break;
case SIOCGIWENCODE:
if(!mmc_encr(ioaddr))
{
ret = -EOPNOTSUPP;
break;
}
if(!suser())
{
ret = -EPERM;
break;
}
else
{
int i;
long long key = 0;
psa_read(ioaddr, lp->hacr,
(char *) &psa.psa_encryption_select - (char *) &psa,
(unsigned char *) &psa.psa_encryption_select, 1+8);
for(i = 0; i < 8; i++)
{
key <<= 8;
key += psa.psa_encryption_key[i];
}
wrq->u.encoding.code = key;
if(psa.psa_encryption_select)
wrq->u.encoding.method = mmc_encr(ioaddr);
else
wrq->u.encoding.method = 0;
}
break;
case SIOCGIWRANGE:
if(wrq->u.data.pointer != (caddr_t) 0)
{
struct iw_range range;
ret = verify_area(VERIFY_WRITE, wrq->u.data.pointer,
sizeof(struct iw_range));
if(ret)
break;
wrq->u.data.length = sizeof(struct iw_range);
range.throughput = 1.6 * 1024 * 1024;
range.min_nwid = 0x0000;
range.max_nwid = 0xFFFF;
if(!(mmc_in(ioaddr, mmroff(0, mmr_fee_status)) &
(MMR_FEE_STATUS_DWLD | MMR_FEE_STATUS_BUSY)))
{
range.num_channels = 10;
range.num_frequency = wv_frequency_list(ioaddr, range.freq,
IW_MAX_FREQUENCIES);
}
else
range.num_channels = range.num_frequency = 0;
range.sensitivity = 0x3F;
range.max_qual.qual = MMR_SGNL_QUAL;
range.max_qual.level = MMR_SIGNAL_LVL;
range.max_qual.noise = MMR_SILENCE_LVL;
copy_to_user(wrq->u.data.pointer, &range,
sizeof(struct iw_range));
}
break;
case SIOCGIWPRIV:
if(wrq->u.data.pointer != (caddr_t) 0)
{
struct iw_priv_args priv[] =
{
{ SIOCSIPQTHR, IW_PRIV_TYPE_BYTE | IW_PRIV_SIZE_FIXED | 1, 0, "setqualthr" },
{ SIOCGIPQTHR, 0, IW_PRIV_TYPE_BYTE | IW_PRIV_SIZE_FIXED | 1, "getqualthr" },
{ SIOCSIPHISTO, IW_PRIV_TYPE_BYTE | 16, 0, "sethisto" },
{ SIOCGIPHISTO, 0, IW_PRIV_TYPE_INT | 16, "gethisto" },
};
ret = verify_area(VERIFY_WRITE, wrq->u.data.pointer,
sizeof(priv));
if(ret)
break;
wrq->u.data.length = 4;
copy_to_user(wrq->u.data.pointer, (u_char *) priv,
sizeof(priv));
}
break;
#ifdef WIRELESS_SPY
case SIOCSIWSPY:
if(wrq->u.data.length > IW_MAX_SPY)
{
ret = -E2BIG;
break;
}
lp->spy_number = wrq->u.data.length;
if(lp->spy_number > 0)
{
struct sockaddr address[IW_MAX_SPY];
int i;
ret = verify_area(VERIFY_READ, wrq->u.data.pointer,
sizeof(struct sockaddr) * lp->spy_number);
if(ret)
break;
copy_from_user(address, wrq->u.data.pointer,
sizeof(struct sockaddr) * lp->spy_number);
for(i = 0; i < lp->spy_number; i++)
{
memcpy(lp->spy_address[i], address[i].sa_data,
WAVELAN_ADDR_SIZE);
}
memset(lp->spy_stat, 0x00, sizeof(iw_qual) * IW_MAX_SPY);
#ifdef DEBUG_IOCTL_INFO
printk(KERN_DEBUG "SetSpy - Set of new addresses is :\n");
for(i = 0; i < wrq->u.data.length; i++)
printk(KERN_DEBUG "%02X:%02X:%02X:%02X:%02X:%02X \n",
lp->spy_address[i][0],
lp->spy_address[i][1],
lp->spy_address[i][2],
lp->spy_address[i][3],
lp->spy_address[i][4],
lp->spy_address[i][5]);
#endif
}
break;
case SIOCGIWSPY:
wrq->u.data.length = lp->spy_number;
if((lp->spy_number > 0) && (wrq->u.data.pointer != (caddr_t) 0))
{
struct sockaddr address[IW_MAX_SPY];
int i;
ret = verify_area(VERIFY_WRITE, wrq->u.data.pointer,
(sizeof(iw_qual) + sizeof(struct sockaddr))
* IW_MAX_SPY);
if(ret)
break;
for(i = 0; i < lp->spy_number; i++)
{
memcpy(address[i].sa_data, lp->spy_address[i],
WAVELAN_ADDR_SIZE);
address[i].sa_family = AF_UNIX;
}
copy_to_user(wrq->u.data.pointer, address,
sizeof(struct sockaddr) * lp->spy_number);
copy_to_user(wrq->u.data.pointer +
(sizeof(struct sockaddr) * lp->spy_number),
lp->spy_stat, sizeof(iw_qual) * lp->spy_number);
for(i = 0; i < lp->spy_number; i++)
lp->spy_stat[i].updated = 0x0;
}
break;
#endif
case SIOCSIPQTHR:
if(!suser())
return -EPERM;
psa.psa_quality_thr = *(wrq->u.name) & 0x0F;
psa_write(ioaddr, lp->hacr, (char *)&psa.psa_quality_thr - (char *)&psa,
(unsigned char *)&psa.psa_quality_thr, 1);
mmc_out(ioaddr, mmwoff(0, mmw_quality_thr), psa.psa_quality_thr);
break;
case SIOCGIPQTHR:
psa_read(ioaddr, lp->hacr, (char *)&psa.psa_quality_thr - (char *)&psa,
(unsigned char *)&psa.psa_quality_thr, 1);
*(wrq->u.name) = psa.psa_quality_thr & 0x0F;
break;
#ifdef HISTOGRAM
case SIOCSIPHISTO:
if(!suser())
return -EPERM;
if(wrq->u.data.length > 16)
{
ret = -E2BIG;
break;
}
lp->his_number = wrq->u.data.length;
if(lp->his_number > 0)
{
ret = verify_area(VERIFY_READ, wrq->u.data.pointer,
sizeof(char) * lp->his_number);
if(ret)
break;
copy_from_user(lp->his_range, wrq->u.data.pointer,
sizeof(char) * lp->his_number);
memset(lp->his_sum, 0x00, sizeof(long) * 16);
}
break;
case SIOCGIPHISTO:
wrq->u.data.length = lp->his_number;
if((lp->his_number > 0) && (wrq->u.data.pointer != (caddr_t) 0))
{
ret = verify_area(VERIFY_WRITE, wrq->u.data.pointer,
sizeof(long) * 16);
if(ret)
break;
copy_to_user(wrq->u.data.pointer, lp->his_sum,
sizeof(long) * lp->his_number);
}
break;
#endif
default:
ret = -EOPNOTSUPP;
}
wv_splx(x);
#ifdef DEBUG_IOCTL_TRACE
printk(KERN_DEBUG "%s: <-wavelan_ioctl()\n", dev->name);
#endif
return ret;
}
static iw_stats *
wavelan_get_wireless_stats(device * dev)
{
u_long ioaddr = dev->base_addr;
net_local * lp = (net_local *) dev->priv;
mmr_t m;
iw_stats * wstats;
unsigned long x;
#ifdef DEBUG_IOCTL_TRACE
printk(KERN_DEBUG "%s: ->wavelan_get_wireless_stats()\n", dev->name);
#endif
x = wv_splhi();
if(lp == (net_local *) NULL)
return (iw_stats *) NULL;
wstats = &lp->wstats;
mmc_out(ioaddr, mmwoff(0, mmw_freeze), 1);
mmc_read(ioaddr, mmroff(0, mmr_dce_status), &m.mmr_dce_status, 1);
mmc_read(ioaddr, mmroff(0, mmr_wrong_nwid_l), &m.mmr_wrong_nwid_l, 2);
mmc_read(ioaddr, mmroff(0, mmr_thr_pre_set), &m.mmr_thr_pre_set, 4);
mmc_out(ioaddr, mmwoff(0, mmw_freeze), 0);
wstats->status = m.mmr_dce_status;
wstats->qual.qual = m.mmr_sgnl_qual & MMR_SGNL_QUAL;
wstats->qual.level = m.mmr_signal_lvl & MMR_SIGNAL_LVL;
wstats->qual.noise = m.mmr_silence_lvl & MMR_SILENCE_LVL;
wstats->qual.updated = (((m.mmr_signal_lvl & MMR_SIGNAL_LVL_VALID) >> 7) |
((m.mmr_signal_lvl & MMR_SIGNAL_LVL_VALID) >> 6) |
((m.mmr_silence_lvl & MMR_SILENCE_LVL_VALID) >> 5));
wstats->discard.nwid += (m.mmr_wrong_nwid_h << 8) | m.mmr_wrong_nwid_l;
wstats->discard.code = 0L;
wstats->discard.misc = 0L;
wv_splx(x);
#ifdef DEBUG_IOCTL_TRACE
printk(KERN_DEBUG "%s: <-wavelan_get_wireless_stats()\n", dev->name);
#endif
return &lp->wstats;
}
#endif
static inline void
wv_packet_read(device * dev,
u_short buf_off,
int sksize)
{
net_local * lp = (net_local *) dev->priv;
u_long ioaddr = dev->base_addr;
struct sk_buff * skb;
#ifdef DEBUG_RX_TRACE
printk(KERN_DEBUG "%s: ->wv_packet_read(0x%X, %d)\n",
dev->name, fd_p, sksize);
#endif
if((skb = dev_alloc_skb(sksize)) == (struct sk_buff *) NULL)
{
#ifdef DEBUG_RX_ERROR
printk(KERN_INFO "%s: wv_packet_read(): could not alloc_skb(%d, GFP_ATOMIC).\n",
dev->name, sksize);
#endif
lp->stats.rx_dropped++;
return;
}
skb->dev = dev;
obram_read(ioaddr, buf_off, skb_put(skb, sksize), sksize);
skb->protocol=eth_type_trans(skb, dev);
#ifdef DEBUG_RX_INFO
wv_packet_info(skb->mac.raw, sksize, dev->name, "wv_packet_read");
#endif
#if defined(WIRELESS_SPY) || defined(HISTOGRAM)
if(
#ifdef WIRELESS_SPY
(lp->spy_number > 0) ||
#endif
#ifdef HISTOGRAM
(lp->his_number > 0) ||
#endif
0)
{
u_char stats[3];
mmc_out(ioaddr, mmwoff(0, mmw_freeze), 1);
mmc_read(ioaddr, mmroff(0, mmr_signal_lvl), stats, 3);
mmc_out(ioaddr, mmwoff(0, mmw_freeze), 0);
#ifdef DEBUG_RX_INFO
printk(KERN_DEBUG "%s: wv_packet_read(): Signal level %d/63, Silence level %d/63, signal quality %d/16\n",
dev->name, stats[0] & 0x3F, stats[1] & 0x3F, stats[2] & 0x0F);
#endif
#ifdef WIRELESS_SPY
wl_spy_gather(dev, skb->mac.raw + WAVELAN_ADDR_SIZE, stats);
#endif
#ifdef HISTOGRAM
wl_his_gather(dev, stats);
#endif
}
#endif
netif_rx(skb);
lp->stats.rx_packets++;
#ifdef DEBUG_RX_TRACE
printk(KERN_DEBUG "%s: <-wv_packet_read()\n", dev->name);
#endif
}
static inline void
wv_receive(device * dev)
{
u_long ioaddr = dev->base_addr;
net_local * lp = (net_local *)dev->priv;
int nreaped = 0;
#ifdef DEBUG_RX_TRACE
printk(KERN_DEBUG "%s: ->wv_receive()\n", dev->name);
#endif
for(;;)
{
fd_t fd;
rbd_t rbd;
ushort pkt_len;
obram_read(ioaddr, lp->rx_head, (unsigned char *) &fd, sizeof(fd));
if((fd.fd_status & FD_STATUS_C) != FD_STATUS_C)
break;
nreaped++;
if((fd.fd_status & (FD_STATUS_B | FD_STATUS_OK)) !=
(FD_STATUS_B | FD_STATUS_OK))
{
#ifndef IGNORE_NORMAL_XMIT_ERRS
#ifdef DEBUG_RX_ERROR
if((fd.fd_status & FD_STATUS_B) != FD_STATUS_B)
printk(KERN_INFO "%s: wv_receive(): frame not consumed by RU.\n",
dev->name);
#endif
#endif
#ifdef DEBUG_RX_ERROR
if((fd.fd_status & FD_STATUS_OK) != FD_STATUS_OK)
printk(KERN_INFO "%s: wv_receive(): frame not received successfully.\n",
dev->name);
#endif
}
if((fd.fd_status & (FD_STATUS_S6 | FD_STATUS_S7 | FD_STATUS_S8 |
FD_STATUS_S9 | FD_STATUS_S10 | FD_STATUS_S11))
!= 0)
{
lp->stats.rx_errors++;
#ifdef DEBUG_RX_ERROR
if((fd.fd_status & FD_STATUS_S6) != 0)
printk(KERN_INFO "%s: wv_receive(): no EOF flag.\n", dev->name);
#endif
if((fd.fd_status & FD_STATUS_S7) != 0)
{
lp->stats.rx_length_errors++;
#ifdef DEBUG_RX_ERROR
printk(KERN_INFO "%s: wv_receive(): frame too short.\n",
dev->name);
#endif
}
if((fd.fd_status & FD_STATUS_S8) != 0)
{
lp->stats.rx_over_errors++;
#ifdef DEBUG_RX_ERROR
printk(KERN_INFO "%s: wv_receive(): rx DMA overrun.\n",
dev->name);
#endif
}
if((fd.fd_status & FD_STATUS_S9) != 0)
{
lp->stats.rx_fifo_errors++;
#ifdef DEBUG_RX_ERROR
printk(KERN_INFO "%s: wv_receive(): ran out of resources.\n",
dev->name);
#endif
}
if((fd.fd_status & FD_STATUS_S10) != 0)
{
lp->stats.rx_frame_errors++;
#ifdef DEBUG_RX_ERROR
printk(KERN_INFO "%s: wv_receive(): alignment error.\n",
dev->name);
#endif
}
if((fd.fd_status & FD_STATUS_S11) != 0)
{
lp->stats.rx_crc_errors++;
#ifdef DEBUG_RX_ERROR
printk(KERN_INFO "%s: wv_receive(): CRC error.\n", dev->name);
#endif
}
}
if(fd.fd_rbd_offset == I82586NULL)
#ifdef DEBUG_RX_ERROR
printk(KERN_INFO "%s: wv_receive(): frame has no data.\n", dev->name);
#endif
else
{
obram_read(ioaddr, fd.fd_rbd_offset,
(unsigned char *) &rbd, sizeof(rbd));
#ifdef DEBUG_RX_ERROR
if((rbd.rbd_status & RBD_STATUS_EOF) != RBD_STATUS_EOF)
printk(KERN_INFO "%s: wv_receive(): missing EOF flag.\n",
dev->name);
if((rbd.rbd_status & RBD_STATUS_F) != RBD_STATUS_F)
printk(KERN_INFO "%s: wv_receive(): missing F flag.\n",
dev->name);
#endif
pkt_len = rbd.rbd_status & RBD_STATUS_ACNT;
wv_packet_read(dev, rbd.rbd_bufl, pkt_len);
}
fd.fd_status = 0;
obram_write(ioaddr, fdoff(lp->rx_head, fd_status),
(unsigned char *) &fd.fd_status, sizeof(fd.fd_status));
fd.fd_command = FD_COMMAND_EL;
obram_write(ioaddr, fdoff(lp->rx_head, fd_command),
(unsigned char *) &fd.fd_command, sizeof(fd.fd_command));
fd.fd_command = 0;
obram_write(ioaddr, fdoff(lp->rx_last, fd_command),
(unsigned char *) &fd.fd_command, sizeof(fd.fd_command));
lp->rx_last = lp->rx_head;
lp->rx_head = fd.fd_link_offset;
}
#ifdef DEBUG_RX_INFO
if(nreaped > 1)
printk(KERN_DEBUG "%s: wv_receive(): reaped %d\n", dev->name, nreaped);
#endif
#ifdef DEBUG_RX_TRACE
printk(KERN_DEBUG "%s: <-wv_receive()\n", dev->name);
#endif
}
static inline void
wv_packet_write(device * dev,
void * buf,
short length)
{
net_local * lp = (net_local *) dev->priv;
u_long ioaddr = dev->base_addr;
unsigned short txblock;
unsigned short txpred;
unsigned short tx_addr;
unsigned short nop_addr;
unsigned short tbd_addr;
unsigned short buf_addr;
ac_tx_t tx;
ac_nop_t nop;
tbd_t tbd;
int clen = length;
unsigned long x;
#ifdef DEBUG_TX_TRACE
printk(KERN_DEBUG "%s: ->wv_packet_write(%d)\n", dev->name, length);
#endif
if(clen < ETH_ZLEN)
clen = ETH_ZLEN;
x = wv_splhi();
txblock = lp->tx_first_free;
txpred = txblock - TXBLOCKZ;
if(txpred < OFFSET_CU)
txpred += NTXBLOCKS * TXBLOCKZ;
lp->tx_first_free += TXBLOCKZ;
if(lp->tx_first_free >= OFFSET_CU + NTXBLOCKS * TXBLOCKZ)
lp->tx_first_free -= NTXBLOCKS * TXBLOCKZ;
lp->tx_n_in_use++;
tx_addr = txblock;
nop_addr = tx_addr + sizeof(tx);
tbd_addr = nop_addr + sizeof(nop);
buf_addr = tbd_addr + sizeof(tbd);
tx.tx_h.ac_status = 0;
obram_write(ioaddr, toff(ac_tx_t, tx_addr, tx_h.ac_status),
(unsigned char *) &tx.tx_h.ac_status,
sizeof(tx.tx_h.ac_status));
nop.nop_h.ac_status = 0;
obram_write(ioaddr, toff(ac_nop_t, nop_addr, nop_h.ac_status),
(unsigned char *) &nop.nop_h.ac_status,
sizeof(nop.nop_h.ac_status));
nop.nop_h.ac_link = nop_addr;
obram_write(ioaddr, toff(ac_nop_t, nop_addr, nop_h.ac_link),
(unsigned char *) &nop.nop_h.ac_link,
sizeof(nop.nop_h.ac_link));
tbd.tbd_status = TBD_STATUS_EOF | (TBD_STATUS_ACNT & clen);
tbd.tbd_next_bd_offset = I82586NULL;
tbd.tbd_bufl = buf_addr;
tbd.tbd_bufh = 0;
obram_write(ioaddr, tbd_addr, (unsigned char *)&tbd, sizeof(tbd));
obram_write(ioaddr, buf_addr, buf, clen);
nop_addr = txpred + sizeof(tx);
nop.nop_h.ac_status = 0;
obram_write(ioaddr, toff(ac_nop_t, nop_addr, nop_h.ac_status),
(unsigned char *)&nop.nop_h.ac_status,
sizeof(nop.nop_h.ac_status));
nop.nop_h.ac_link = txblock;
obram_write(ioaddr, toff(ac_nop_t, nop_addr, nop_h.ac_link),
(unsigned char *) &nop.nop_h.ac_link,
sizeof(nop.nop_h.ac_link));
if(lp->watchdog.prev == (timer_list *) NULL)
{
lp->watchdog.expires = jiffies + WATCHDOG_JIFFIES;
add_timer(&lp->watchdog);
}
if(lp->tx_first_in_use == I82586NULL)
lp->tx_first_in_use = txblock;
if(lp->tx_n_in_use < NTXBLOCKS - 1)
dev->tbusy = 0;
wv_splx(x);
#ifdef DEBUG_TX_INFO
wv_packet_info((u_char *) buf, length, dev->name, "wv_packet_write");
#endif
#ifdef DEBUG_TX_TRACE
printk(KERN_DEBUG "%s: <-wv_packet_write()\n", dev->name);
#endif
}
static int
wavelan_packet_xmit(struct sk_buff * skb,
device * dev)
{
net_local * lp = (net_local *)dev->priv;
#ifdef DEBUG_TX_TRACE
printk(KERN_DEBUG "%s: ->wavelan_packet_xmit(0x%X)\n", dev->name,
(unsigned) skb);
#endif
if(dev->tbusy)
return 1;
if(skb == (struct sk_buff *)0)
{
#ifdef DEBUG_TX_ERROR
printk(KERN_INFO "%s: wavelan_packet_xmit(): skb == NULL\n", dev->name);
#endif
dev_tint(dev);
return 0;
}
if(set_bit(0, (void *)&dev->tbusy) != 0)
#ifdef DEBUG_TX_ERROR
printk(KERN_INFO "%s: Transmitter access conflict.\n", dev->name);
#endif
else
{
if(lp->reconfig_82586)
{
wv_82586_config(dev);
if(dev->tbusy)
return 1;
}
#ifdef DEBUG_TX_ERROR
if(skb->next)
printk(KERN_INFO "skb has next\n");
#endif
wv_packet_write(dev, skb->data, skb->len);
}
dev_kfree_skb(skb, FREE_WRITE);
#ifdef DEBUG_TX_TRACE
printk(KERN_DEBUG "%s: <-wavelan_packet_xmit()\n", dev->name);
#endif
return 0;
}
static inline int
wv_mmc_init(device * dev)
{
u_long ioaddr = dev->base_addr;
net_local * lp = (net_local *)dev->priv;
psa_t psa;
mmw_t m;
int configured;
#ifdef DEBUG_CONFIG_TRACE
printk(KERN_DEBUG "%s: ->wv_mmc_init()\n", dev->name);
#endif
psa_read(ioaddr, lp->hacr, 0, (unsigned char *) &psa, sizeof(psa));
#ifdef USE_PSA_CONFIG
configured = psa.psa_conf_status & 1;
#else
configured = 0;
#endif
if(!configured)
{
psa.psa_nwid[0] = 0;
psa.psa_nwid[1] = 0;
psa.psa_nwid_select = 0;
psa.psa_encryption_select = 0;
if (psa.psa_comp_number & 1)
psa.psa_thr_pre_set = 0x01;
else
psa.psa_thr_pre_set = 0x04;
psa.psa_quality_thr = 0x03;
psa.psa_conf_status |= 1;
#ifdef USE_PSA_CONFIG
psa_write(ioaddr, lp->hacr, (char *)psa.psa_nwid - (char *)&psa,
(unsigned char *)psa.psa_nwid, 4);
psa_write(ioaddr, lp->hacr, (char *)&psa.psa_thr_pre_set - (char *)&psa,
(unsigned char *)&psa.psa_thr_pre_set, 1);
psa_write(ioaddr, lp->hacr, (char *)&psa.psa_quality_thr - (char *)&psa,
(unsigned char *)&psa.psa_quality_thr, 1);
psa_write(ioaddr, lp->hacr, (char *)&psa.psa_conf_status - (char *)&psa,
(unsigned char *)&psa.psa_conf_status, 1);
#endif
}
memset(&m, 0x00, sizeof(m));
m.mmw_netw_id_l = psa.psa_nwid[1];
m.mmw_netw_id_h = psa.psa_nwid[0];
if(psa.psa_nwid_select & 1)
m.mmw_loopt_sel = 0x00;
else
m.mmw_loopt_sel = MMW_LOOPT_SEL_DIS_NWID;
memcpy(&m.mmw_encr_key, &psa.psa_encryption_key,
sizeof(m.mmw_encr_key));
if(psa.psa_encryption_select)
m.mmw_encr_enable = MMW_ENCR_ENABLE_EN | MMW_ENCR_ENABLE_MODE;
else
m.mmw_encr_enable = 0;
m.mmw_thr_pre_set = psa.psa_thr_pre_set & 0x3F;
m.mmw_quality_thr = psa.psa_quality_thr & 0x0F;
m.mmw_jabber_enable = 0x01;
m.mmw_anten_sel = MMW_ANTEN_SEL_ALG_EN;
m.mmw_ifs = 0x20;
m.mmw_mod_delay = 0x04;
m.mmw_jam_time = 0x38;
m.mmw_encr_enable = 0;
m.mmw_des_io_invert = 0;
m.mmw_freeze = 0;
m.mmw_decay_prm = 0;
m.mmw_decay_updat_prm = 0;
mmc_write(ioaddr, 0, (u_char *)&m, sizeof(m));
if(!(mmc_in(ioaddr, mmroff(0, mmr_fee_status)) &
(MMR_FEE_STATUS_DWLD | MMR_FEE_STATUS_BUSY)))
{
m.mmw_fee_addr = 0x0F;
m.mmw_fee_ctrl = MMW_FEE_CTRL_READ | MMW_FEE_CTRL_DWLD;
mmc_write(ioaddr, (char *)&m.mmw_fee_ctrl - (char *)&m,
(unsigned char *)&m.mmw_fee_ctrl, 2);
fee_wait(ioaddr, 100, 100);
#ifdef DEBUG_CONFIG_INFO
mmc_read(ioaddr, (char *)&m.mmw_fee_data_l - (char *)&m,
(unsigned char *)&m.mmw_fee_data_l, 2);
printk(KERN_DEBUG "%s: WaveLAN 2.00 recognised (frequency select) : Current frequency = %ld\n",
dev->name,
((m.mmw_fee_data_h << 4) |
(m.mmw_fee_data_l >> 4)) * 5 / 2 + 24000L);
#endif
m.mmw_fee_addr = 0x61;
m.mmw_fee_ctrl = MMW_FEE_CTRL_READ | MMW_FEE_CTRL_DWLD;
mmc_write(ioaddr, (char *)&m.mmw_fee_ctrl - (char *)&m,
(unsigned char *)&m.mmw_fee_ctrl, 2);
}
#ifdef DEBUG_CONFIG_TRACE
printk(KERN_DEBUG "%s: <-wv_mmc_init()\n", dev->name);
#endif
return 0;
}
static inline int
wv_ru_start(device * dev)
{
net_local * lp = (net_local *) dev->priv;
u_long ioaddr = dev->base_addr;
u_short scb_cs;
fd_t fd;
rbd_t rbd;
u_short rx;
u_short rx_next;
int i;
#ifdef DEBUG_CONFIG_TRACE
printk(KERN_DEBUG "%s: ->wv_ru_start()\n", dev->name);
#endif
obram_read(ioaddr, scboff(OFFSET_SCB, scb_status), (unsigned char *)&scb_cs, sizeof(scb_cs));
if((scb_cs & SCB_ST_RUS) == SCB_ST_RUS_RDY)
return 0;
lp->rx_head = OFFSET_RU;
for(i = 0, rx = lp->rx_head; i < NRXBLOCKS; i++, rx = rx_next)
{
rx_next = (i == NRXBLOCKS - 1) ? lp->rx_head : rx + RXBLOCKZ;
fd.fd_status = 0;
fd.fd_command = (i == NRXBLOCKS - 1) ? FD_COMMAND_EL : 0;
fd.fd_link_offset = rx_next;
fd.fd_rbd_offset = rx + sizeof(fd);
obram_write(ioaddr, rx, (unsigned char *)&fd, sizeof(fd));
rbd.rbd_status = 0;
rbd.rbd_next_rbd_offset = I82586NULL;
rbd.rbd_bufl = rx + sizeof(fd) + sizeof(rbd);
rbd.rbd_bufh = 0;
rbd.rbd_el_size = RBD_EL | (RBD_SIZE & MAXDATAZ);
obram_write(ioaddr, rx + sizeof(fd),
(unsigned char *) &rbd, sizeof(rbd));
lp->rx_last = rx;
}
obram_write(ioaddr, scboff(OFFSET_SCB, scb_rfa_offset),
(unsigned char *) &lp->rx_head, sizeof(lp->rx_head));
scb_cs = SCB_CMD_RUC_GO;
obram_write(ioaddr, scboff(OFFSET_SCB, scb_command),
(unsigned char *) &scb_cs, sizeof(scb_cs));
set_chan_attn(ioaddr, lp->hacr);
for(i = 1000; i > 0; i--)
{
obram_read(ioaddr, scboff(OFFSET_SCB, scb_command),
(unsigned char *) &scb_cs, sizeof(scb_cs));
if (scb_cs == 0)
break;
udelay(10);
}
if(i <= 0)
{
#ifdef DEBUG_CONFIG_ERRORS
printk(KERN_INFO "%s: wavelan_ru_start(): board not accepting command.\n",
dev->name);
#endif
return -1;
}
#ifdef DEBUG_CONFIG_TRACE
printk(KERN_DEBUG "%s: <-wv_ru_start()\n", dev->name);
#endif
return 0;
}
static inline int
wv_cu_start(device * dev)
{
net_local * lp = (net_local *) dev->priv;
u_long ioaddr = dev->base_addr;
int i;
u_short txblock;
u_short first_nop;
u_short scb_cs;
#ifdef DEBUG_CONFIG_TRACE
printk(KERN_DEBUG "%s: ->wv_cu_start()\n", dev->name);
#endif
lp->tx_first_free = OFFSET_CU;
lp->tx_first_in_use = I82586NULL;
for(i = 0, txblock = OFFSET_CU;
i < NTXBLOCKS;
i++, txblock += TXBLOCKZ)
{
ac_tx_t tx;
ac_nop_t nop;
tbd_t tbd;
unsigned short tx_addr;
unsigned short nop_addr;
unsigned short tbd_addr;
unsigned short buf_addr;
tx_addr = txblock;
nop_addr = tx_addr + sizeof(tx);
tbd_addr = nop_addr + sizeof(nop);
buf_addr = tbd_addr + sizeof(tbd);
tx.tx_h.ac_status = 0;
tx.tx_h.ac_command = acmd_transmit | AC_CFLD_I;
tx.tx_h.ac_link = nop_addr;
tx.tx_tbd_offset = tbd_addr;
obram_write(ioaddr, tx_addr, (unsigned char *) &tx, sizeof(tx));
nop.nop_h.ac_status = 0;
nop.nop_h.ac_command = acmd_nop;
nop.nop_h.ac_link = nop_addr;
obram_write(ioaddr, nop_addr, (unsigned char *) &nop, sizeof(nop));
tbd.tbd_status = TBD_STATUS_EOF;
tbd.tbd_next_bd_offset = I82586NULL;
tbd.tbd_bufl = buf_addr;
tbd.tbd_bufh = 0;
obram_write(ioaddr, tbd_addr, (unsigned char *) &tbd, sizeof(tbd));
}
first_nop = OFFSET_CU + (NTXBLOCKS - 1) * TXBLOCKZ + sizeof(ac_tx_t);
obram_write(ioaddr, scboff(OFFSET_SCB, scb_cbl_offset),
(unsigned char *) &first_nop, sizeof(first_nop));
scb_cs = SCB_CMD_CUC_GO;
obram_write(ioaddr, scboff(OFFSET_SCB, scb_command),
(unsigned char *) &scb_cs, sizeof(scb_cs));
set_chan_attn(ioaddr, lp->hacr);
for(i = 1000; i > 0; i--)
{
obram_read(ioaddr, scboff(OFFSET_SCB, scb_command),
(unsigned char *) &scb_cs, sizeof(scb_cs));
if (scb_cs == 0)
break;
udelay(10);
}
if(i <= 0)
{
#ifdef DEBUG_CONFIG_ERRORS
printk(KERN_INFO "%s: wavelan_cu_start(): board not accepting command.\n",
dev->name);
#endif
return -1;
}
lp->tx_n_in_use = 0;
dev->tbusy = 0;
#ifdef DEBUG_CONFIG_TRACE
printk(KERN_DEBUG "%s: <-wv_cu_start()\n", dev->name);
#endif
return 0;
}
static inline int
wv_82586_start(device * dev)
{
net_local * lp = (net_local *) dev->priv;
u_long ioaddr = dev->base_addr;
scp_t scp;
iscp_t iscp;
scb_t scb;
ach_t cb;
u_char zeroes[512];
int i;
#ifdef DEBUG_CONFIG_TRACE
printk(KERN_DEBUG "%s: ->wv_82586_start()\n", dev->name);
#endif
memset(&zeroes[0], 0x00, sizeof(zeroes));
for(i = 0; i < I82586_MEMZ; i += sizeof(zeroes))
obram_write(ioaddr, i, &zeroes[0], sizeof(zeroes));
memset(&scp, 0x00, sizeof(scp));
scp.scp_sysbus = SCP_SY_16BBUS;
scp.scp_iscpl = OFFSET_ISCP;
obram_write(ioaddr, OFFSET_SCP, (unsigned char *)&scp, sizeof(scp));
memset(&iscp, 0x00, sizeof(iscp));
iscp.iscp_busy = 1;
iscp.iscp_offset = OFFSET_SCB;
obram_write(ioaddr, OFFSET_ISCP, (unsigned char *)&iscp, sizeof(iscp));
memset(&scb, 0x00, sizeof(scb));
scb.scb_command = SCB_CMD_RESET;
scb.scb_cbl_offset = OFFSET_CU;
scb.scb_rfa_offset = OFFSET_RU;
obram_write(ioaddr, OFFSET_SCB, (unsigned char *)&scb, sizeof(scb));
set_chan_attn(ioaddr, lp->hacr);
for(i = 1000; i > 0; i--)
{
obram_read(ioaddr, OFFSET_ISCP, (unsigned char *) &iscp, sizeof(iscp));
if(iscp.iscp_busy == (unsigned short) 0)
break;
udelay(10);
}
if(i <= 0)
{
#ifdef DEBUG_CONFIG_ERRORS
printk(KERN_INFO "%s: wv_82586_start(): iscp_busy timeout.\n",
dev->name);
#endif
return -1;
}
for(i = 15; i > 0; i--)
{
obram_read(ioaddr, OFFSET_SCB, (unsigned char *) &scb, sizeof(scb));
if (scb.scb_status == (SCB_ST_CX | SCB_ST_CNA))
break;
udelay(10);
}
if (i <= 0)
{
#ifdef DEBUG_CONFIG_ERRORS
printk(KERN_INFO "%s: wv_82586_start(): status: expected 0x%02x, got 0x%02x.\n",
dev->name, SCB_ST_CX | SCB_ST_CNA, scb.scb_status);
#endif
return -1;
}
wv_ack(dev);
memset(&cb, 0x00, sizeof(cb));
cb.ac_command = AC_CFLD_EL | (AC_CFLD_CMD & acmd_diagnose);
cb.ac_link = OFFSET_CU;
obram_write(ioaddr, OFFSET_CU, (unsigned char *)&cb, sizeof(cb));
if(wv_synchronous_cmd(dev, "diag()") == -1)
return -1;
obram_read(ioaddr, OFFSET_CU, (unsigned char *)&cb, sizeof(cb));
if(cb.ac_status & AC_SFLD_FAIL)
{
#ifdef DEBUG_CONFIG_ERRORS
printk(KERN_INFO "%s: wv_82586_start(): i82586 Self Test failed.\n",
dev->name);
#endif
return -1;
}
#ifdef DEBUG_I82586_SHOW
wv_scb_show(ioaddr);
#endif
#ifdef DEBUG_CONFIG_TRACE
printk(KERN_DEBUG "%s: <-wv_82586_start()\n", dev->name);
#endif
return 0;
}
static void
wv_82586_config(device * dev)
{
net_local * lp = (net_local *) dev->priv;
u_long ioaddr = dev->base_addr;
unsigned short txblock;
unsigned short txpred;
unsigned short tx_addr;
unsigned short nop_addr;
unsigned short tbd_addr;
unsigned short cfg_addr;
unsigned short ias_addr;
unsigned short mcs_addr;
ac_tx_t tx;
ac_nop_t nop;
ac_cfg_t cfg;
ac_ias_t ias;
ac_mcs_t mcs;
struct dev_mc_list * dmi;
unsigned long x;
#ifdef DEBUG_CONFIG_TRACE
printk(KERN_DEBUG "%s: ->wv_82586_config()\n", dev->name);
#endif
x = wv_splhi();
txblock = lp->tx_first_free;
txpred = txblock - TXBLOCKZ;
if(txpred < OFFSET_CU)
txpred += NTXBLOCKS * TXBLOCKZ;
lp->tx_first_free += TXBLOCKZ;
if(lp->tx_first_free >= OFFSET_CU + NTXBLOCKS * TXBLOCKZ)
lp->tx_first_free -= NTXBLOCKS * TXBLOCKZ;
lp->tx_n_in_use++;
tx_addr = txblock;
nop_addr = tx_addr + sizeof(tx);
tbd_addr = nop_addr + sizeof(nop);
cfg_addr = tbd_addr + sizeof(tbd_t);
ias_addr = cfg_addr + sizeof(cfg);
mcs_addr = ias_addr + sizeof(ias);
tx.tx_h.ac_status = 0xFFFF;
obram_write(ioaddr, toff(ac_tx_t, tx_addr, tx_h.ac_status),
(unsigned char *) &tx.tx_h.ac_status,
sizeof(tx.tx_h.ac_status));
nop.nop_h.ac_status = 0;
obram_write(ioaddr, toff(ac_nop_t, nop_addr, nop_h.ac_status),
(unsigned char *) &nop.nop_h.ac_status,
sizeof(nop.nop_h.ac_status));
nop.nop_h.ac_link = nop_addr;
obram_write(ioaddr, toff(ac_nop_t, nop_addr, nop_h.ac_link),
(unsigned char *) &nop.nop_h.ac_link,
sizeof(nop.nop_h.ac_link));
memset(&cfg, 0x00, sizeof(cfg));
#if 0
cfg.fifolim_bytecnt = 0x080c;
cfg.addrlen_mode = 0x2600;
cfg.linprio_interframe = 0x7820;
cfg.slot_time = 0xf00c;
cfg.hardware = 0x0008;
cfg.min_frame_len = 0x0040;
#endif
cfg.cfg_byte_cnt = AC_CFG_BYTE_CNT(sizeof(ac_cfg_t) - sizeof(ach_t));
cfg.cfg_fifolim = AC_CFG_FIFOLIM(8);
cfg.cfg_byte8 = AC_CFG_SAV_BF(0) |
AC_CFG_SRDY(0);
cfg.cfg_byte9 = AC_CFG_ELPBCK(0) |
AC_CFG_ILPBCK(0) |
AC_CFG_PRELEN(AC_CFG_PLEN_2) |
AC_CFG_ALOC(1) |
AC_CFG_ADDRLEN(WAVELAN_ADDR_SIZE);
cfg.cfg_byte10 = AC_CFG_BOFMET(0) |
AC_CFG_ACR(0) |
AC_CFG_LINPRIO(0);
cfg.cfg_ifs = 32;
cfg.cfg_slotl = 0;
cfg.cfg_byte13 = AC_CFG_RETRYNUM(15) |
AC_CFG_SLTTMHI(2);
cfg.cfg_byte14 = AC_CFG_FLGPAD(0) |
AC_CFG_BTSTF(0) |
AC_CFG_CRC16(0) |
AC_CFG_NCRC(0) |
AC_CFG_TNCRS(1) |
AC_CFG_MANCH(0) |
AC_CFG_BCDIS(0) |
AC_CFG_PRM(lp->promiscuous);
cfg.cfg_byte15 = AC_CFG_ICDS(0) |
AC_CFG_CDTF(0) |
AC_CFG_ICSS(0) |
AC_CFG_CSTF(0);
cfg.cfg_min_frm_len = AC_CFG_MNFRM(8);
cfg.cfg_h.ac_command = (AC_CFLD_CMD & acmd_configure);
cfg.cfg_h.ac_link = ias_addr;
obram_write(ioaddr, cfg_addr, (unsigned char *)&cfg, sizeof(cfg));
memset(&ias, 0x00, sizeof(ias));
ias.ias_h.ac_command = (AC_CFLD_CMD & acmd_ia_setup);
ias.ias_h.ac_link = mcs_addr;
memcpy(&ias.ias_addr[0], (unsigned char *)&dev->dev_addr[0], sizeof(ias.ias_addr));
obram_write(ioaddr, ias_addr, (unsigned char *)&ias, sizeof(ias));
memset(&mcs, 0x00, sizeof(mcs));
mcs.mcs_h.ac_command = AC_CFLD_I | (AC_CFLD_CMD & acmd_mc_setup);
mcs.mcs_h.ac_link = nop_addr;
mcs.mcs_cnt = WAVELAN_ADDR_SIZE * lp->mc_count;
obram_write(ioaddr, mcs_addr, (unsigned char *)&mcs, sizeof(mcs));
if(lp->mc_count)
{
for(dmi=dev->mc_list; dmi; dmi=dmi->next)
outsw(PIOP1(ioaddr), (u_short *) dmi->dmi_addr,
WAVELAN_ADDR_SIZE >> 1);
#ifdef DEBUG_CONFIG_INFO
printk(KERN_DEBUG "%s: wv_82586_config(): set %d multicast addresses:\n",
dev->name, lp->mc_count);
for(dmi=dev->mc_list; dmi; dmi=dmi->next)
printk(KERN_DEBUG " %02x:%02x:%02x:%02x:%02x:%02x\n",
dmi->dmi_addr[0], dmi->dmi_addr[1], dmi->dmi_addr[2],
dmi->dmi_addr[3], dmi->dmi_addr[4], dmi->dmi_addr[5] );
#endif
}
nop_addr = txpred + sizeof(tx);
nop.nop_h.ac_status = 0;
obram_write(ioaddr, toff(ac_nop_t, nop_addr, nop_h.ac_status),
(unsigned char *)&nop.nop_h.ac_status,
sizeof(nop.nop_h.ac_status));
nop.nop_h.ac_link = cfg_addr;
obram_write(ioaddr, toff(ac_nop_t, nop_addr, nop_h.ac_link),
(unsigned char *) &nop.nop_h.ac_link,
sizeof(nop.nop_h.ac_link));
if(lp->watchdog.prev == (timer_list *) NULL)
{
lp->watchdog.expires = jiffies + WATCHDOG_JIFFIES;
add_timer(&lp->watchdog);
}
lp->reconfig_82586 = 0;
if(lp->tx_first_in_use == I82586NULL)
lp->tx_first_in_use = txblock;
if(lp->tx_n_in_use < NTXBLOCKS - 1)
dev->tbusy = 0;
wv_splx(x);
#ifdef DEBUG_CONFIG_TRACE
printk(KERN_DEBUG "%s: <-wv_82586_config()\n", dev->name);
#endif
}
static inline void
wv_82586_stop(device * dev)
{
net_local * lp = (net_local *) dev->priv;
u_long ioaddr = dev->base_addr;
u_short scb_cmd;
#ifdef DEBUG_CONFIG_TRACE
printk(KERN_DEBUG "%s: ->wv_82586_stop()\n", dev->name);
#endif
scb_cmd = (SCB_CMD_CUC & SCB_CMD_CUC_SUS) | (SCB_CMD_RUC & SCB_CMD_RUC_SUS);
obram_write(ioaddr, scboff(OFFSET_SCB, scb_command),
(unsigned char *)&scb_cmd, sizeof(scb_cmd));
set_chan_attn(ioaddr, lp->hacr);
wv_ints_off(dev);
#ifdef DEBUG_CONFIG_TRACE
printk(KERN_DEBUG "%s: <-wv_82586_stop()\n", dev->name);
#endif
}
static int
wv_hw_reset(device * dev)
{
net_local * lp = (net_local *)dev->priv;
u_long ioaddr = dev->base_addr;
#ifdef DEBUG_CONFIG_TRACE
printk(KERN_DEBUG "%s: ->wv_hw_reset(dev=0x%x)\n", dev->name,
(unsigned int)dev);
#endif
if(lp->watchdog.prev != (timer_list *) NULL)
del_timer(&lp->watchdog);
lp->nresets++;
wv_hacr_reset(ioaddr);
lp->hacr = HACR_DEFAULT;
if((wv_mmc_init(dev) < 0) ||
(wv_82586_start(dev) < 0))
return -1;
wv_ints_on(dev);
if((wv_ru_start(dev) < 0) ||
(wv_cu_start(dev) < 0))
return -1;
wv_82586_config(dev);
#ifdef DEBUG_CONFIG_TRACE
printk(KERN_DEBUG "%s: <-wv_hw_reset()\n", dev->name);
#endif
return 0;
}
static int
wv_check_ioaddr(u_long ioaddr,
u_char * mac)
{
int i;
if(check_region(ioaddr, sizeof(ha_t)))
return EADDRINUSE;
wv_hacr_reset(ioaddr);
psa_read(ioaddr, HACR_DEFAULT, psaoff(0, psa_univ_mac_addr),
mac, 6);
for(i = 0; i < (sizeof(MAC_ADDRESSES) / sizeof(char) / 3); i++)
if((mac[0] == MAC_ADDRESSES[i][0]) &&
(mac[1] == MAC_ADDRESSES[i][1]) &&
(mac[2] == MAC_ADDRESSES[i][2]))
return 0;
#ifdef DEBUG_CONFIG_INFO
printk(KERN_WARNING "WaveLAN (0x%3X): your MAC address might be: %02X:%02X:%02X.\n",
ioaddr, mac[0], mac[1], mac[2]);
#endif
return ENODEV;
}
static void
wavelan_interrupt(int irq,
void * dev_id,
struct pt_regs * regs)
{
device * dev;
u_long ioaddr;
net_local * lp;
u_short hasr;
u_short status;
u_short ack_cmd;
if((dev = (device *) (irq2dev_map[irq])) == (device *) NULL)
{
#ifdef DEBUG_INTERRUPT_ERROR
printk(KERN_WARNING "wavelan_interrupt(): irq %d for unknown device.\n",
irq);
#endif
return;
}
#ifdef DEBUG_INTERRUPT_TRACE
printk(KERN_DEBUG "%s: ->wavelan_interrupt()\n", dev->name);
#endif
lp = (net_local *) dev->priv;
ioaddr = dev->base_addr;
#ifdef DEBUG_INTERRUPT_ERROR
if(dev->interrupt)
printk(KERN_INFO "%s: wavelan_interrupt(): Re-entering the interrupt handler.\n",
dev->name);
#endif
dev->interrupt = 1;
if((hasr = hasr_read(ioaddr)) & HASR_MMC_INTR)
{
u_char dce_status;
mmc_read(ioaddr, mmroff(0, mmr_dce_status), &dce_status, sizeof(dce_status));
#ifdef DEBUG_INTERRUPT_ERROR
printk(KERN_INFO "%s: wavelan_interrupt(): unexpected mmc interrupt: status 0x%04x.\n",
dev->name, dce_status);
#endif
}
if((hasr & HASR_82586_INTR) == 0)
{
dev->interrupt = 0;
#ifdef DEBUG_INTERRUPT_ERROR
printk(KERN_INFO "%s: wavelan_interrupt(): interrupt not coming from i82586\n",
dev->name);
#endif
return;
}
obram_read(ioaddr, scboff(OFFSET_SCB, scb_status),
(unsigned char *) &status, sizeof(status));
ack_cmd = status & SCB_ST_INT;
obram_write(ioaddr, scboff(OFFSET_SCB, scb_command),
(unsigned char *) &ack_cmd, sizeof(ack_cmd));
set_chan_attn(ioaddr, lp->hacr);
#ifdef DEBUG_INTERRUPT_INFO
printk(KERN_DEBUG "%s: wavelan_interrupt(): status 0x%04x.\n",
dev->name, status);
#endif
if((status & SCB_ST_CX) == SCB_ST_CX)
{
#ifdef DEBUG_INTERRUPT_INFO
printk(KERN_DEBUG "%s: wavelan_interrupt(): command completed.\n",
dev->name);
#endif
wv_complete(dev, ioaddr, lp);
if(lp->watchdog.prev != (timer_list *) NULL)
del_timer(&lp->watchdog);
if(lp->tx_n_in_use > 0)
{
lp->watchdog.expires = jiffies + WATCHDOG_JIFFIES;
add_timer(&lp->watchdog);
}
}
if((status & SCB_ST_FR) == SCB_ST_FR)
{
#ifdef DEBUG_INTERRUPT_INFO
printk(KERN_DEBUG "%s: wavelan_interrupt(): received packet.\n",
dev->name);
#endif
wv_receive(dev);
}
if(((status & SCB_ST_CNA) == SCB_ST_CNA) ||
(((status & SCB_ST_CUS) != SCB_ST_CUS_ACTV) && dev->start))
{
#ifdef DEBUG_INTERRUPT_ERROR
printk(KERN_INFO "%s: wavelan_interrupt(): CU inactive -- restarting\n",
dev->name);
#endif
wv_hw_reset(dev);
}
if(((status & SCB_ST_RNR) == SCB_ST_RNR) ||
(((status & SCB_ST_RUS) != SCB_ST_RUS_RDY) && dev->start))
{
#ifdef DEBUG_INTERRUPT_ERROR
printk(KERN_INFO "%s: wavelan_interrupt(): RU not ready -- restarting\n",
dev->name);
#endif
wv_hw_reset(dev);
}
dev->interrupt = 0;
#ifdef DEBUG_INTERRUPT_TRACE
printk(KERN_DEBUG "%s: <-wavelan_interrupt()\n", dev->name);
#endif
}
static void
wavelan_watchdog(u_long a)
{
device * dev;
net_local * lp;
u_long ioaddr;
unsigned long x;
unsigned int nreaped;
dev = (device *) a;
ioaddr = dev->base_addr;
lp = (net_local *) dev->priv;
#ifdef DEBUG_INTERRUPT_TRACE
printk(KERN_DEBUG "%s: ->wavelan_watchdog()\n", dev->name);
#endif
#ifdef DEBUG_INTERRUPT_ERROR
printk(KERN_INFO "%s: wavelan_watchdog: watchdog timer expired\n",
dev->name);
#endif
x = wv_splhi();
dev = (device *) a;
ioaddr = dev->base_addr;
lp = (net_local *) dev->priv;
if(lp->tx_n_in_use <= 0)
{
wv_splx(x);
return;
}
nreaped = wv_complete(dev, ioaddr, lp);
#ifdef DEBUG_INTERRUPT_INFO
printk(KERN_DEBUG "%s: wavelan_watchdog(): %d reaped, %d remain.\n",
dev->name, nreaped, lp->tx_n_in_use);
#endif
#ifdef DEBUG_PSA_SHOW
{
psa_t psa;
psa_read(dev, 0, (unsigned char *) &psa, sizeof(psa));
wv_psa_show(&psa);
}
#endif
#ifdef DEBUG_MMC_SHOW
wv_mmc_show(dev);
#endif
#ifdef DEBUG_I82586_SHOW
wv_cu_show(dev);
#endif
if(nreaped == 0)
{
#ifdef DEBUG_INTERRUPT_ERROR
printk(KERN_INFO "%s: wavelan_watchdog(): cleanup failed, trying reset\n",
dev->name);
#endif
wv_hw_reset(dev);
}
else
if(lp->tx_n_in_use > 0)
{
lp->watchdog.expires = jiffies + WATCHDOG_JIFFIES;
add_timer(&lp->watchdog);
}
wv_splx(x);
#ifdef DEBUG_INTERRUPT_TRACE
printk(KERN_DEBUG "%s: <-wavelan_watchdog()\n", dev->name);
#endif
}
static int
wavelan_open(device * dev)
{
u_long x;
#ifdef DEBUG_CALLBACK_TRACE
printk(KERN_DEBUG "%s: ->wavelan_open(dev=0x%x)\n", dev->name,
(unsigned int) dev);
#endif
if(dev->irq == 0)
{
#ifdef DEBUG_CONFIG_ERRORS
printk(KERN_WARNING "%s: wavelan_open(): no IRQ\n", dev->name);
#endif
return -ENXIO;
}
if((irq2dev_map[dev->irq] != (device *) NULL) ||
((irq2dev_map[dev->irq] = dev) == (device *) NULL) ||
(request_irq(dev->irq, &wavelan_interrupt, 0, "WaveLAN", NULL) != 0))
{
irq2dev_map[dev->irq] = (device *) NULL;
#ifdef DEBUG_CONFIG_ERRORS
printk(KERN_WARNING "%s: wavelan_open(): invalid IRQ\n", dev->name);
#endif
return -EAGAIN;
}
x = wv_splhi();
if(wv_hw_reset(dev) != -1)
{
dev->interrupt = 0;
dev->start = 1;
}
else
{
free_irq(dev->irq, NULL);
irq2dev_map[dev->irq] = (device *) NULL;
#ifdef DEBUG_CONFIG_ERRORS
printk(KERN_INFO "%s: wavelan_open(): impossible to start the card\n",
dev->name);
#endif
return -EAGAIN;
}
wv_splx(x);
MOD_INC_USE_COUNT;
#ifdef DEBUG_CALLBACK_TRACE
printk(KERN_DEBUG "%s: <-wavelan_open()\n", dev->name);
#endif
return 0;
}
static int
wavelan_close(device * dev)
{
net_local * lp = (net_local *)dev->priv;
#ifdef DEBUG_CALLBACK_TRACE
printk(KERN_DEBUG "%s: ->wavelan_close(dev=0x%x)\n", dev->name,
(unsigned int) dev);
#endif
if(dev->start == 0)
return 0;
dev->tbusy = 1;
dev->start = 0;
if(lp->watchdog.prev != (timer_list *) NULL)
del_timer(&lp->watchdog);
wv_82586_stop(dev);
free_irq(dev->irq, NULL);
irq2dev_map[dev->irq] = (device *) NULL;
MOD_DEC_USE_COUNT;
#ifdef DEBUG_CALLBACK_TRACE
printk(KERN_DEBUG "%s: <-wavelan_close()\n", dev->name);
#endif
return 0;
}
static int
wavelan_config(device * dev)
{
u_long ioaddr = dev->base_addr;
u_char irq_mask;
int irq;
net_local * lp;
#ifdef DEBUG_CALLBACK_TRACE
printk(KERN_DEBUG "%s: ->wavelan_config(dev=0x%x, ioaddr=0x%x)\n", dev->name,
(unsigned int)dev, ioaddr);
#endif
if(dev->irq != 0)
{
irq_mask = wv_irq_to_psa(dev->irq);
if(irq_mask == 0)
{
#ifdef DEBUG_CONFIG_ERROR
printk(KERN_WARNING "%s: wavelan_config(): invalid irq %d -- ignored.\n",
dev->name, dev->irq);
#endif
dev->irq = 0;
}
else
{
#ifdef DEBUG_CONFIG_INFO
printk(KERN_DEBUG "%s: wavelan_config(): changing irq to %d\n",
dev->name, dev->irq);
#endif
psa_write(ioaddr, HACR_DEFAULT,
psaoff(0, psa_int_req_no), &irq_mask, 1);
wv_hacr_reset(ioaddr);
}
}
psa_read(ioaddr, HACR_DEFAULT, psaoff(0, psa_int_req_no), &irq_mask, 1);
if((irq = wv_psa_to_irq(irq_mask)) == -1)
{
#ifdef DEBUG_CONFIG_ERROR
printk(KERN_INFO "%s: wavelan_config(): could not wavelan_map_irq(%d).\n",
dev->name, irq_mask);
#endif
return EAGAIN;
}
dev->irq = irq;
request_region(ioaddr, sizeof(ha_t), "wavelan");
dev->mem_start = 0x0000;
dev->mem_end = 0x0000;
dev->if_port = 0;
dev->priv = kmalloc(sizeof(net_local), GFP_KERNEL);
if(dev->priv == NULL)
return -ENOMEM;
memset(dev->priv, 0x00, sizeof(net_local));
lp = (net_local *)dev->priv;
lp->dev = dev;
lp->next = wavelan_list;
wavelan_list = lp;
lp->hacr = HACR_DEFAULT;
lp->watchdog.function = wavelan_watchdog;
lp->watchdog.data = (unsigned long) dev;
lp->promiscuous = 0;
lp->mc_count = 0;
ether_setup(dev);
dev->open = wavelan_open;
dev->stop = wavelan_close;
dev->hard_start_xmit = wavelan_packet_xmit;
dev->get_stats = wavelan_get_stats;
dev->set_multicast_list = &wavelan_set_multicast_list;
dev->set_mac_address = &wavelan_set_mac_address;
#ifdef WIRELESS_EXT
dev->do_ioctl = wavelan_ioctl;
dev->get_wireless_stats = wavelan_get_wireless_stats;
#endif
dev->mtu = WAVELAN_MTU;
wv_init_info(dev);
#ifdef DEBUG_CALLBACK_TRACE
printk(KERN_DEBUG "%s: <-wavelan_config()\n", dev->name);
#endif
return 0;
}
int
wavelan_probe(device * dev)
{
short base_addr;
mac_addr mac;
int i;
int r;
#ifdef DEBUG_CALLBACK_TRACE
printk(KERN_DEBUG "%s: ->wavelan_probe(dev=0x%x (base_addr=0x%x))\n",
dev->name, (unsigned int)dev, (unsigned int)dev->base_addr);
#endif
#ifdef STRUCT_CHECK
if (wv_struct_check() != (char *) NULL)
{
printk(KERN_WARNING "%s: wavelan_probe(): structure/compiler botch: \"%s\"\n",
dev->name, wv_struct_check());
return ENODEV;
}
#endif
base_addr = dev->base_addr;
if(base_addr < 0)
{
#ifdef DEBUG_CONFIG_ERRORS
printk(KERN_WARNING "%s: wavelan_probe(): invalid base address\n",
dev->name);
#endif
return ENXIO;
}
if(base_addr > 0x100)
{
if((r = wv_check_ioaddr(base_addr, mac)) == 0)
{
memcpy(dev->dev_addr, mac, 6);
r = wavelan_config(dev);
}
#ifdef DEBUG_CONFIG_INFO
if(r != 0)
printk(KERN_DEBUG "%s: wavelan_probe(): no device at specified base address (0x%X) or address already in use\n",
dev->name, base_addr);
#endif
#ifdef DEBUG_CALLBACK_TRACE
printk(KERN_DEBUG "%s: <-wavelan_probe()\n", dev->name);
#endif
return r;
}
for(i = 0; i < NELS(iobase); i++)
{
if(wv_check_ioaddr(iobase[i], mac) == 0)
{
dev->base_addr = iobase[i];
memcpy(dev->dev_addr, mac, 6);
if(wavelan_config(dev) == 0)
{
#ifdef DEBUG_CALLBACK_TRACE
printk(KERN_DEBUG "%s: <-wavelan_probe()\n", dev->name);
#endif
return 0;
}
}
}
dev->base_addr = base_addr;
#ifdef DEBUG_CONFIG_INFO
printk(KERN_DEBUG "%s: wavelan_probe(): no device found\n",
dev->name);
#endif
return ENODEV;
}
#ifdef MODULE
int
init_module(void)
{
mac_addr mac;
int ret = 0;
int i;
#ifdef DEBUG_MODULE_TRACE
printk(KERN_DEBUG "-> init_module()\n");
#endif
if(io[0] == 0)
{
#ifdef DEBUG_CONFIG_ERRORS
printk(KERN_WARNING "WaveLAN init_module(): doing device probing (bad !)\n");
printk(KERN_WARNING "Specify base addresses while loading module to correct the problem\n");
#endif
for(i = 0; i < NELS(iobase); i++)
io[i] = iobase[i];
}
i = -1;
while((io[++i] != 0) && (i < NELS(io)))
{
if(wv_check_ioaddr(io[i], mac) == 0)
{
device * dev;
dev = kmalloc(sizeof(struct device), GFP_KERNEL);
memset(dev, 0x00, sizeof(struct device));
dev->name = name[i];
dev->base_addr = io[i];
dev->irq = irq[i];
dev->init = &wavelan_config;
memcpy(dev->dev_addr, mac, 6);
if(register_netdev(dev) != 0)
{
kfree_s(dev, sizeof(struct device));
ret = -EIO;
}
}
}
#ifdef DEBUG_CONFIG_ERRORS
if(wavelan_list == (net_local *) NULL)
printk(KERN_WARNING "WaveLAN init_module(): no device found\n");
#endif
#ifdef DEBUG_MODULE_TRACE
printk(KERN_DEBUG "<- init_module()\n");
#endif
return ret;
}
void
cleanup_module(void)
{
#ifdef DEBUG_MODULE_TRACE
printk(KERN_DEBUG "-> cleanup_module()\n");
#endif
while(wavelan_list != (net_local *) NULL)
{
device * dev = wavelan_list->dev;
#ifdef DEBUG_CONFIG_INFO
printk(KERN_DEBUG "%s: cleanup_module(): removing device at 0x%x\n",
dev->name, (unsigned int) dev);
#endif
release_region(dev->base_addr, sizeof(ha_t));
unregister_netdev(dev);
wavelan_list = wavelan_list->next;
kfree_s(dev->priv, sizeof(struct net_local));
kfree_s(dev, sizeof(struct device));
}
#ifdef DEBUG_MODULE_TRACE
printk(KERN_DEBUG "<- cleanup_module()\n");
#endif
}
#endif