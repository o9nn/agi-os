#undef REALLY_SLOW_IO
#define CMD640_PREFETCH_MASKS 1
#include <linux/config.h>
#include <linux/types.h>
#include <linux/kernel.h>
#include <linux/delay.h>
#include <linux/timer.h>
#include <linux/mm.h>
#include <linux/ioport.h>
#include <linux/blkdev.h>
#include <linux/hdreg.h>
#include <asm/io.h>
#include "ide.h"
#include "ide_modes.h"
int cmd640_vlb = 0;
#define VID		0x00
#define DID		0x02
#define PCMD		0x04
#define   PCMD_ENA	0x01
#define PSTTS		0x06
#define REVID		0x08
#define PROGIF		0x09
#define SUBCL		0x0a
#define BASCL		0x0b
#define BaseA0		0x10
#define BaseA1		0x14
#define BaseA2		0x18
#define BaseA3		0x1c
#define INTLINE		0x3c
#define INPINE		0x3d
#define	CFR		0x50
#define   CFR_DEVREV		0x03
#define   CFR_IDE01INTR		0x04
#define	  CFR_DEVID		0x18
#define	  CFR_AT_VESA_078h	0x20
#define	  CFR_DSA1		0x40
#define	  CFR_DSA0		0x80
#define CNTRL		0x51
#define	  CNTRL_DIS_RA0		0x40
#define   CNTRL_DIS_RA1		0x80
#define	  CNTRL_ENA_2ND		0x08
#define	CMDTIM		0x52
#define	ARTTIM0		0x53
#define	DRWTIM0		0x54
#define ARTTIM1 	0x55
#define DRWTIM1		0x56
#define ARTTIM23	0x57
#define   ARTTIM23_DIS_RA2	0x04
#define   ARTTIM23_DIS_RA3	0x08
#define DRWTIM23	0x58
#define BRST		0x59
static byte prefetch_regs[4]  = {CNTRL, CNTRL, ARTTIM23, ARTTIM23};
static byte prefetch_masks[4] = {CNTRL_DIS_RA0, CNTRL_DIS_RA1, ARTTIM23_DIS_RA2, ARTTIM23_DIS_RA3};
#ifdef CONFIG_BLK_DEV_CMD640_ENHANCED
static byte arttim_regs[4] = {ARTTIM0, ARTTIM1, ARTTIM23, ARTTIM23};
static byte drwtim_regs[4] = {DRWTIM0, DRWTIM1, DRWTIM23, DRWTIM23};
static byte setup_counts[4]    = {4, 4, 4, 4};
static byte active_counts[4]   = {16, 16, 16, 16};
static byte recovery_counts[4] = {16, 16, 16, 16};
#endif
static ide_hwif_t  *cmd_hwif0, *cmd_hwif1;
static ide_drive_t *cmd_drives[4];
static unsigned int cmd640_key;
static void (*put_cmd640_reg)(unsigned short reg, byte val);
static byte (*get_cmd640_reg)(unsigned short reg);
static unsigned int cmd640_chip_version;
static void put_cmd640_reg_pci1 (unsigned short reg, byte val)
{
unsigned long flags;
save_flags(flags);
cli();
outl_p((reg & 0xfc) | cmd640_key, 0xcf8);
outb_p(val, (reg & 3) | 0xcfc);
restore_flags(flags);
}
static byte get_cmd640_reg_pci1 (unsigned short reg)
{
byte b;
unsigned long flags;
save_flags(flags);
cli();
outl_p((reg & 0xfc) | cmd640_key, 0xcf8);
b = inb_p((reg & 3) | 0xcfc);
restore_flags(flags);
return b;
}
static void put_cmd640_reg_pci2 (unsigned short reg, byte val)
{
unsigned long flags;
save_flags(flags);
cli();
outb_p(0x10, 0xcf8);
outb_p(val, cmd640_key + reg);
outb_p(0, 0xcf8);
restore_flags(flags);
}
static byte get_cmd640_reg_pci2 (unsigned short reg)
{
byte b;
unsigned long flags;
save_flags(flags);
cli();
outb_p(0x10, 0xcf8);
b = inb_p(cmd640_key + reg);
outb_p(0, 0xcf8);
restore_flags(flags);
return b;
}
static void put_cmd640_reg_vlb (unsigned short reg, byte val)
{
unsigned long flags;
save_flags(flags);
cli();
outb_p(reg, cmd640_key);
outb_p(val, cmd640_key + 4);
restore_flags(flags);
}
static byte get_cmd640_reg_vlb (unsigned short reg)
{
byte b;
unsigned long flags;
save_flags(flags);
cli();
outb_p(reg, cmd640_key);
b = inb_p(cmd640_key + 4);
restore_flags(flags);
return b;
}
static int match_pci_cmd640_device (void)
{
const byte ven_dev[4] = {0x95, 0x10, 0x40, 0x06};
unsigned int i;
for (i = 0; i < 4; i++) {
if (get_cmd640_reg(i) != ven_dev[i])
return 0;
}
#ifdef STUPIDLY_TRUST_BROKEN_PCMD_ENA_BIT
if ((get_cmd640_reg(PCMD) & PCMD_ENA) == 0) {
printk("ide: cmd640 on PCI disabled by BIOS\n");
return 0;
}
#endif
return 1;
}
static int probe_for_cmd640_pci1 (void)
{
get_cmd640_reg = get_cmd640_reg_pci1;
put_cmd640_reg = put_cmd640_reg_pci1;
for (cmd640_key = 0x80000000; cmd640_key <= 0x8000f800; cmd640_key += 0x800) {
if (match_pci_cmd640_device())
return 1;
}
return 0;
}
static int probe_for_cmd640_pci2 (void)
{
get_cmd640_reg = get_cmd640_reg_pci2;
put_cmd640_reg = put_cmd640_reg_pci2;
for (cmd640_key = 0xc000; cmd640_key <= 0xcf00; cmd640_key += 0x100) {
if (match_pci_cmd640_device())
return 1;
}
return 0;
}
static int probe_for_cmd640_vlb (void)
{
byte b;
get_cmd640_reg = get_cmd640_reg_vlb;
put_cmd640_reg = put_cmd640_reg_vlb;
cmd640_key = 0x178;
b = get_cmd640_reg(CFR);
if (b == 0xff || b == 0x00 || (b & CFR_AT_VESA_078h)) {
cmd640_key = 0x78;
b = get_cmd640_reg(CFR);
if (b == 0xff || b == 0x00 || !(b & CFR_AT_VESA_078h))
return 0;
}
return 1;
}
static int secondary_port_responding (void)
{
unsigned long flags;
save_flags(flags);
cli();
outb_p(0x0a, 0x170 + IDE_SELECT_OFFSET);
udelay(100);
if ((inb_p(0x170 + IDE_SELECT_OFFSET) & 0x1f) != 0x0a) {
outb_p(0x1a, 0x170 + IDE_SELECT_OFFSET);
udelay(100);
if ((inb_p(0x170 + IDE_SELECT_OFFSET) & 0x1f) != 0x1a) {
restore_flags(flags);
return 0;
}
}
restore_flags(flags);
return 1;
}
#ifdef CMD640_DUMP_REGS
void cmd640_dump_regs (void)
{
unsigned int reg = cmd640_vlb ? 0x50 : 0x00;
printk("ide: cmd640 internal register dump:");
for (; reg <= 0x59; reg++) {
if (!(reg & 0x0f))
printk("\n%04x:", reg);
printk(" %02x", get_cmd640_reg(reg));
}
printk("\n");
}
#endif
static void check_prefetch (unsigned int index)
{
ide_drive_t *drive = cmd_drives[index];
byte b = get_cmd640_reg(prefetch_regs[index]);
if (b & prefetch_masks[index]) {
drive->no_unmask = 0;
drive->no_io_32bit = 1;
drive->io_32bit = 0;
} else {
#if CMD640_PREFETCH_MASKS
drive->no_unmask = 1;
drive->unmask = 0;
#endif
drive->no_io_32bit = 0;
}
}
static void setup_device_ptrs (void)
{
unsigned int i;
cmd_hwif0 = &ide_hwifs[0];
cmd_hwif1 = &ide_hwifs[1];
for (i = 0; i < MAX_HWIFS; i++) {
ide_hwif_t *hwif = &ide_hwifs[i];
if (hwif->chipset == ide_unknown || hwif->chipset == ide_generic) {
if (hwif->io_base == 0x1f0)
cmd_hwif0 = hwif;
else if (hwif->io_base == 0x170)
cmd_hwif1 = hwif;
}
}
cmd_drives[0] = &cmd_hwif0->drives[0];
cmd_drives[1] = &cmd_hwif0->drives[1];
cmd_drives[2] = &cmd_hwif1->drives[0];
cmd_drives[3] = &cmd_hwif1->drives[1];
}
#ifdef CONFIG_BLK_DEV_CMD640_ENHANCED
static void set_prefetch_mode (unsigned int index, int mode)
{
ide_drive_t *drive = cmd_drives[index];
int reg = prefetch_regs[index];
byte b;
unsigned long flags;
save_flags(flags);
cli();
b = get_cmd640_reg(reg);
if (mode) {
#if CMD640_PREFETCH_MASKS
drive->no_unmask = 1;
drive->unmask = 0;
#endif
drive->no_io_32bit = 0;
b &= ~prefetch_masks[index];
} else {
drive->no_unmask = 0;
drive->no_io_32bit = 1;
drive->io_32bit = 0;
b |= prefetch_masks[index];
}
put_cmd640_reg(reg, b);
restore_flags(flags);
}
static void display_clocks (unsigned int index)
{
byte active_count, recovery_count;
active_count = active_counts[index];
if (active_count == 1)
++active_count;
recovery_count = recovery_counts[index];
if (active_count > 3 && recovery_count == 1)
++recovery_count;
if (cmd640_chip_version > 1)
recovery_count += 1;
printk(", clocks=%d/%d/%d\n", setup_counts[index], active_count, recovery_count);
}
inline static byte pack_nibbles (byte upper, byte lower)
{
return ((upper & 0x0f) << 4) | (lower & 0x0f);
}
static void retrieve_drive_counts (unsigned int index)
{
byte b;
b = get_cmd640_reg(arttim_regs[index]) & ~0x3f;
switch (b) {
case 0x00: b = 4; break;
case 0x80: b = 3; break;
case 0x40: b = 2; break;
default:   b = 5; break;
}
setup_counts[index] = b;
b = get_cmd640_reg(drwtim_regs[index]);
active_counts[index]   = (b >> 4)   ? (b >> 4)   : 0x10;
recovery_counts[index] = (b & 0x0f) ? (b & 0x0f) : 0x10;
}
static void program_drive_counts (unsigned int index)
{
unsigned long flags;
byte setup_count    = setup_counts[index];
byte active_count   = active_counts[index];
byte recovery_count = recovery_counts[index];
if (index > 1) {
unsigned int mate;
if (cmd_drives[mate = index ^ 1]->present) {
if (setup_count < setup_counts[mate])
setup_count = setup_counts[mate];
if (active_count < active_counts[mate])
active_count = active_counts[mate];
if (recovery_count < recovery_counts[mate])
recovery_count = recovery_counts[mate];
}
}
switch (setup_count) {
case 4:	 setup_count = 0x00; break;
case 3:	 setup_count = 0x80; break;
case 1:
case 2:	 setup_count = 0x40; break;
default: setup_count = 0xc0;
}
save_flags (flags);
cli();
setup_count |= get_cmd640_reg(arttim_regs[index]) & 0x3f;
put_cmd640_reg(arttim_regs[index], setup_count);
put_cmd640_reg(drwtim_regs[index], pack_nibbles(active_count, recovery_count));
restore_flags(flags);
}
static void cmd640_set_mode (unsigned int index, byte pio_mode, unsigned int cycle_time)
{
int setup_time, active_time, recovery_time, clock_time;
byte setup_count, active_count, recovery_count, recovery_count2, cycle_count;
int bus_speed = ide_system_bus_speed();
if (pio_mode > 5)
pio_mode = 5;
setup_time  = ide_pio_timings[pio_mode].setup_time;
active_time = ide_pio_timings[pio_mode].active_time;
recovery_time = cycle_time - (setup_time + active_time);
clock_time = 1000 / bus_speed;
cycle_count = (cycle_time + clock_time - 1) / clock_time;
setup_count = (setup_time + clock_time - 1) / clock_time;
active_count = (active_time + clock_time - 1) / clock_time;
if (active_count < 2)
active_count = 2;
recovery_count = (recovery_time + clock_time - 1) / clock_time;
recovery_count2 = cycle_count - (setup_count + active_count);
if (recovery_count2 > recovery_count)
recovery_count = recovery_count2;
if (recovery_count < 2)
recovery_count = 2;
if (recovery_count > 17) {
active_count += recovery_count - 17;
recovery_count = 17;
}
if (active_count > 16)
active_count = 16;
if (cmd640_chip_version > 1)
recovery_count -= 1;
if (recovery_count > 16)
recovery_count = 16;
setup_counts[index]    = setup_count;
active_counts[index]   = active_count;
recovery_counts[index] = recovery_count;
program_drive_counts (index);
}
static void cmd640_tune_drive (ide_drive_t *drive, byte mode_wanted)
{
byte b;
ide_pio_data_t  d;
unsigned int index = 0;
while (drive != cmd_drives[index]) {
if (++index > 3) {
printk("%s: bad news in cmd640_tune_drive\n", drive->name);
return;
}
}
switch (mode_wanted) {
case 6:
case 7:
mode_wanted &= 1;
b = get_cmd640_reg(CNTRL) & ~0x27;
if (mode_wanted)
b |= 0x27;
put_cmd640_reg(CNTRL, b);
printk("%s: %sabled cmd640 fast host timing (devsel)\n", drive->name, mode_wanted ? "en" : "dis");
return;
case 8:
case 9:
mode_wanted &= 1;
set_prefetch_mode(index, mode_wanted);
printk("%s: %sabled cmd640 prefetch\n", drive->name, mode_wanted ? "en" : "dis");
return;
}
(void) ide_get_best_pio_mode (drive, mode_wanted, 5, &d);
cmd640_set_mode (index, d.pio_mode, d.cycle_time);
printk ("%s: selected cmd640 PIO mode%d (%dns) %s/IORDY%s",
drive->name,
d.pio_mode,
d.cycle_time,
d.use_iordy ? "w" : "wo",
d.overridden ? " (overriding vendor mode)" : "");
display_clocks(index);
}
#endif
int ide_probe_for_cmd640x (void)
{
#ifdef CONFIG_BLK_DEV_CMD640_ENHANCED
int second_port_toggled = 0;
#endif
int second_port_cmd640 = 0;
const char *bus_type, *port2;
unsigned int index;
byte b, cfr;
if (cmd640_vlb && probe_for_cmd640_vlb()) {
bus_type = "VLB";
} else {
cmd640_vlb = 0;
if (probe_for_cmd640_pci1())
bus_type = "PCI (type1)";
else if (probe_for_cmd640_pci2())
bus_type = "PCI (type2)";
else
return 0;
}
put_cmd640_reg(0x5b, 0xbd);
if (get_cmd640_reg(0x5b) != 0xbd) {
printk("ide: cmd640 init failed: wrong value in reg 0x5b\n");
return 0;
}
put_cmd640_reg(0x5b, 0);
#ifdef CMD640_DUMP_REGS
CMD640_DUMP_REGS;
#endif
cfr = get_cmd640_reg(CFR);
cmd640_chip_version = cfr & CFR_DEVREV;
if (cmd640_chip_version == 0) {
printk ("ide: bad cmd640 revision: %d\n", cmd640_chip_version);
return 0;
}
setup_device_ptrs ();
printk("%s: buggy cmd640%c interface on %s, config=0x%02x\n",
cmd_hwif0->name, 'a' + cmd640_chip_version - 1, bus_type, cfr);
cmd_hwif0->chipset = ide_cmd640;
#ifdef CONFIG_BLK_DEV_CMD640_ENHANCED
cmd_hwif0->tuneproc = &cmd640_tune_drive;
#endif
put_cmd640_reg(CMDTIM, 0);
put_cmd640_reg(BRST, 0x40);
if (cmd_hwif1->noprobe) {
port2 = "not probed";
} else {
b = get_cmd640_reg(CNTRL);
if (secondary_port_responding()) {
if ((b & CNTRL_ENA_2ND)) {
second_port_cmd640 = 1;
port2 = "okay";
} else if (cmd640_vlb) {
second_port_cmd640 = 1;
port2 = "alive";
} else
port2 = "not cmd640";
} else {
put_cmd640_reg(CNTRL, b ^ CNTRL_ENA_2ND);
if (secondary_port_responding()) {
second_port_cmd640 = 1;
#ifdef CONFIG_BLK_DEV_CMD640_ENHANCED
second_port_toggled = 1;
#endif
port2 = "enabled";
} else {
put_cmd640_reg(CNTRL, b);
port2 = "not responding";
}
}
}
if (second_port_cmd640) {
cmd_hwif0->serialized = 1;
cmd_hwif1->serialized = 1;
cmd_hwif1->chipset = ide_cmd640;
#ifdef CONFIG_BLK_DEV_CMD640_ENHANCED
cmd_hwif1->tuneproc = &cmd640_tune_drive;
#endif
}
printk("%s: %sserialized, secondary interface %s\n", cmd_hwif1->name,
cmd_hwif0->serialized ? "" : "not ", port2);
for (index = 0; index < (2 + (second_port_cmd640 << 1)); index++) {
ide_drive_t *drive = cmd_drives[index];
#ifdef CONFIG_BLK_DEV_CMD640_ENHANCED
if (drive->autotune || ((index > 1) && second_port_toggled)) {
setup_counts    [index] = 4;
active_counts   [index] = 16;
recovery_counts [index] = 16;
program_drive_counts (index);
set_prefetch_mode (index, 0);
printk("cmd640: drive%d timings/prefetch cleared\n", index);
} else {
retrieve_drive_counts (index);
check_prefetch (index);
printk("cmd640: drive%d timings/prefetch(%s) preserved",
index, drive->no_io_32bit ? "off" : "on");
display_clocks(index);
}
#else
check_prefetch (index);
printk("cmd640: drive%d timings/prefetch(%s) preserved\n",
index, drive->no_io_32bit ? "off" : "on");
#endif
}
#ifdef CMD640_DUMP_REGS
CMD640_DUMP_REGS;
#endif
return 1;
}