#define _IDE_C
#undef REALLY_SLOW_IO
#include <linux/config.h>
#include <linux/types.h>
#include <linux/string.h>
#include <linux/kernel.h>
#include <linux/delay.h>
#include <linux/timer.h>
#include <linux/mm.h>
#include <linux/ioport.h>
#include <linux/interrupt.h>
#include <linux/major.h>
#include <linux/blkdev.h>
#include <linux/errno.h>
#include <linux/hdreg.h>
#include <linux/genhd.h>
#include <linux/malloc.h>
#include <ahci.h>
#include <asm/byteorder.h>
#include <asm/irq.h>
#include <asm/segment.h>
#include <asm/io.h>
#ifdef CONFIG_PCI
#include <linux/bios32.h>
#include <linux/pci.h>
#endif
#include "ide.h"
#include "ide_modes.h"
#ifdef CONFIG_BLK_DEV_PROMISE
#include "promise.h"
#define IS_PROMISE_DRIVE (HWIF(drive)->chipset == ide_promise)
#else
#define IS_PROMISE_DRIVE (0)
#endif
static const byte ide_hwif_to_major[MAX_HWIFS] = {IDE0_MAJOR, IDE1_MAJOR, IDE2_MAJOR, IDE3_MAJOR};
static unsigned short default_io_base[MAX_HWIFS] = {0x1f0, 0x170, 0x1e8, 0x168};
static const byte default_irqs[MAX_HWIFS] = {14, 15, 11, 10};
static int idebus_parameter;
static int system_bus_speed;
ide_hwif_t ide_hwifs[MAX_HWIFS];
#if (DISK_RECOVERY_TIME > 0)
static unsigned long read_timer(void)
{
unsigned long t, flags;
int i;
save_flags(flags);
cli();
t = jiffies * 11932;
outb_p(0, 0x43);
i = inb_p(0x40);
i |= inb(0x40) << 8;
restore_flags(flags);
return (t - i);
}
static void set_recovery_timer (ide_hwif_t *hwif)
{
hwif->last_time = read_timer();
}
#define SET_RECOVERY_TIMER(drive) set_recovery_timer (drive)
#else
#define SET_RECOVERY_TIMER(drive)
#endif
void ide_disable_base(unsigned base)
{
unsigned i;
for (i = 0; i < MAX_HWIFS; i++)
if (default_io_base[i] == base)
default_io_base[i] = 0;
}
static void init_hwif_data (unsigned int index)
{
byte *p;
unsigned int unit;
ide_hwif_t *hwif = &ide_hwifs[index];
p = ((byte *) hwif) + sizeof(ide_hwif_t);
do {
*--p = 0;
} while (p > (byte *) hwif);
hwif->index = index;
hwif->io_base = default_io_base[index];
hwif->irq = default_irqs[index];
hwif->ctl_port = hwif->io_base ? hwif->io_base+0x206 : 0x000;
#ifdef CONFIG_BLK_DEV_HD
if (hwif->io_base == HD_DATA)
hwif->noprobe = 1;
#endif
hwif->major = ide_hwif_to_major[index];
hwif->name[0] = 'i';
hwif->name[1] = 'd';
hwif->name[2] = 'e';
hwif->name[3] = '0' + index;
#ifdef CONFIG_BLK_DEV_IDETAPE
hwif->tape_drive = NULL;
#endif
for (unit = 0; unit < MAX_DRIVES; ++unit) {
ide_drive_t *drive = &hwif->drives[unit];
drive->select.all = (unit<<4)|0xa0;
drive->hwif = hwif;
drive->ctl = 0x08;
drive->ready_stat = READY_STAT;
drive->bad_wstat = BAD_W_STAT;
drive->special.b.recalibrate = 1;
drive->special.b.set_geometry = 1;
drive->name[0] = 'h';
drive->name[1] = 'd';
#ifdef MACH
drive->name[2] = '0' + (index * MAX_DRIVES) + unit;
#else
drive->name[2] = 'a' + (index * MAX_DRIVES) + unit;
#endif
}
}
#define MAGIC_COOKIE 0x12345678
static void init_ide_data (void)
{
unsigned int index;
static unsigned long magic_cookie = MAGIC_COOKIE;
if (magic_cookie != MAGIC_COOKIE)
return;
magic_cookie = 0;
for (index = 0; index < MAX_HWIFS; ++index)
init_hwif_data(index);
idebus_parameter = 0;
system_bus_speed = 0;
}
int ide_system_bus_speed (void)
{
if (!system_bus_speed) {
if (idebus_parameter)
system_bus_speed = idebus_parameter;
#ifdef CONFIG_PCI
else if (pcibios_present())
system_bus_speed = 40;
#endif
else
system_bus_speed = 50;
printk("ide: Assuming %dMhz system bus speed for PIO modes; override with idebus=xx\n", system_bus_speed);
}
return system_bus_speed;
}
#if SUPPORT_VLB_SYNC
static inline void do_vlb_sync (unsigned short port) {
(void) inb (port);
(void) inb (port);
(void) inb (port);
}
#endif
void ide_input_data (ide_drive_t *drive, void *buffer, unsigned int wcount)
{
unsigned short io_base = HWIF(drive)->io_base;
unsigned short data_reg = io_base+IDE_DATA_OFFSET;
byte io_32bit = drive->io_32bit;
if (io_32bit) {
#if SUPPORT_VLB_SYNC
if (io_32bit & 2) {
cli();
do_vlb_sync(io_base+IDE_NSECTOR_OFFSET);
insl(data_reg, buffer, wcount);
if (drive->unmask)
sti();
} else
#endif
insl(data_reg, buffer, wcount);
} else {
#if SUPPORT_SLOW_DATA_PORTS
if (drive->slow) {
unsigned short *ptr = (unsigned short *) buffer;
while (wcount--) {
*ptr++ = inw_p(data_reg);
*ptr++ = inw_p(data_reg);
}
} else
#endif
insw(data_reg, buffer, wcount<<1);
}
}
void ide_output_data (ide_drive_t *drive, void *buffer, unsigned int wcount)
{
unsigned short io_base = HWIF(drive)->io_base;
unsigned short data_reg = io_base+IDE_DATA_OFFSET;
byte io_32bit = drive->io_32bit;
if (io_32bit) {
#if SUPPORT_VLB_SYNC
if (io_32bit & 2) {
cli();
do_vlb_sync(io_base+IDE_NSECTOR_OFFSET);
outsl(data_reg, buffer, wcount);
if (drive->unmask)
sti();
} else
#endif
outsl(data_reg, buffer, wcount);
} else {
#if SUPPORT_SLOW_DATA_PORTS
if (drive->slow) {
unsigned short *ptr = (unsigned short *) buffer;
while (wcount--) {
outw_p(*ptr++, data_reg);
outw_p(*ptr++, data_reg);
}
} else
#endif
outsw(data_reg, buffer, wcount<<1);
}
}
void atapi_input_bytes (ide_drive_t *drive, void *buffer, unsigned int bytecount)
{
++bytecount;
ide_input_data (drive, buffer, bytecount / 4);
if ((bytecount & 0x03) >= 2)
insw (IDE_DATA_REG, ((byte *)buffer) + (bytecount & ~0x03), 1);
}
void atapi_output_bytes (ide_drive_t *drive, void *buffer, unsigned int bytecount)
{
++bytecount;
ide_output_data (drive, buffer, bytecount / 4);
if ((bytecount & 0x03) >= 2)
outsw (IDE_DATA_REG, ((byte *)buffer) + (bytecount & ~0x03), 1);
}
void ide_set_handler (ide_drive_t *drive, ide_handler_t *handler, unsigned int timeout)
{
ide_hwgroup_t *hwgroup = HWGROUP(drive);
#ifdef DEBUG
if (hwgroup->handler != NULL) {
printk("%s: ide_set_handler: handler not null; old=%p, new=%p\n",
drive->name, hwgroup->handler, handler);
}
#endif
hwgroup->handler = handler;
hwgroup->timer.expires = jiffies + timeout;
add_timer(&(hwgroup->timer));
}
static int lba_capacity_is_ok (struct hd_driveid *id)
{
unsigned long lba_sects = id->lba_capacity;
unsigned long chs_sects = id->cyls * id->heads * id->sectors;
unsigned long _10_percent = chs_sects / 10;
if (id->cyls == 16383 && id->sectors == 63 &&
(id->heads == 15 || id->heads == 16) &&
id->lba_capacity >= 16383*63*id->heads)
return 1;
if ((lba_sects - chs_sects) < _10_percent)
return 1;
lba_sects = (lba_sects << 16) | (lba_sects >> 16);
if ((lba_sects - chs_sects) < _10_percent) {
id->lba_capacity = lba_sects;
return 1;
}
return 0;
}
static unsigned long current_capacity (ide_drive_t *drive)
{
struct hd_driveid *id = drive->id;
unsigned long capacity;
if (!drive->present)
return 0;
#ifdef CONFIG_BLK_DEV_IDEFLOPPY
if (drive->media == ide_floppy)
return idefloppy_capacity(drive);
#endif
if (drive->media != ide_disk)
return 0x7fffffff;
drive->select.b.lba = 0;
capacity = drive->cyl * drive->head * drive->sect;
if (id != NULL && (id->capability & 2) && lba_capacity_is_ok(id)) {
if (id->lba_capacity >= capacity) {
capacity = id->lba_capacity;
drive->select.b.lba = 1;
}
}
return (capacity - drive->sect0);
}
static void ide_geninit (struct gendisk *gd)
{
unsigned int unit;
ide_hwif_t *hwif = gd->real_devices;
for (unit = 0; unit < gd->nr_real; ++unit) {
ide_drive_t *drive = &hwif->drives[unit];
#ifdef CONFIG_BLK_DEV_IDECD
if (drive->present && drive->media == ide_cdrom)
ide_cdrom_setup(drive);
#endif
#ifdef CONFIG_BLK_DEV_IDETAPE
if (drive->present && drive->media == ide_tape)
idetape_setup(drive);
#endif
#ifdef CONFIG_BLK_DEV_IDEFLOPPY
if (drive->present && drive->media == ide_floppy)
idefloppy_setup(drive);
#endif
drive->part[0].nr_sects = current_capacity(drive);
if (!drive->present || (drive->media != ide_disk && drive->media != ide_floppy) ||
!drive->part[0].nr_sects) {
drive->part[0].start_sect = -1;
}
}
}
static void init_gendisk (ide_hwif_t *hwif)
{
struct gendisk *gd, **gdp;
unsigned int unit, units, minors;
int *bs;
for (units = MAX_DRIVES; units > 0; --units) {
if (hwif->drives[units-1].present)
break;
}
minors = units * (1<<PARTN_BITS);
gd = kmalloc (sizeof(struct gendisk), GFP_KERNEL);
gd->sizes = kmalloc (minors * sizeof(int), GFP_KERNEL);
gd->part = kmalloc (minors * sizeof(struct hd_struct), GFP_KERNEL);
bs = kmalloc (minors*sizeof(int), GFP_KERNEL);
memset(gd->part, 0, minors * sizeof(struct hd_struct));
blksize_size[hwif->major] = bs;
for (unit = 0; unit < minors; ++unit)
*bs++ = BLOCK_SIZE;
for (unit = 0; unit < units; ++unit)
hwif->drives[unit].part = &gd->part[unit << PARTN_BITS];
gd->major = hwif->major;
gd->major_name = IDE_MAJOR_NAME;
gd->minor_shift = PARTN_BITS;
gd->max_p = 1<<PARTN_BITS;
gd->max_nr = units;
gd->nr_real = units;
gd->init = ide_geninit;
gd->real_devices= hwif;
gd->next = NULL;
for (gdp = &gendisk_head; *gdp; gdp = &((*gdp)->next)) ;
hwif->gd = *gdp = gd;
}
static void do_reset1 (ide_drive_t *, int);
#ifdef CONFIG_BLK_DEV_IDEATAPI
static void atapi_reset_pollfunc (ide_drive_t *drive)
{
ide_hwgroup_t *hwgroup = HWGROUP(drive);
byte stat;
OUT_BYTE (drive->select.all, IDE_SELECT_REG);
udelay (10);
if (OK_STAT(stat=GET_STAT(), 0, BUSY_STAT)) {
printk("%s: ATAPI reset complete\n", drive->name);
} else {
if (jiffies < hwgroup->poll_timeout) {
ide_set_handler (drive, &atapi_reset_pollfunc, HZ/20);
return;
}
hwgroup->poll_timeout = 0;
printk("%s: ATAPI reset timed-out, status=0x%02x\n", drive->name, stat);
do_reset1 (drive, 1);
return;
}
hwgroup->poll_timeout = 0;
}
#endif
static void reset_pollfunc (ide_drive_t *drive)
{
ide_hwgroup_t *hwgroup = HWGROUP(drive);
ide_hwif_t *hwif = HWIF(drive);
byte tmp;
if (!OK_STAT(tmp=GET_STAT(), 0, BUSY_STAT)) {
if (jiffies < hwgroup->poll_timeout) {
ide_set_handler (drive, &reset_pollfunc, HZ/20);
return;
}
printk("%s: reset timed-out, status=0x%02x\n", hwif->name, tmp);
} else {
printk("%s: reset: ", hwif->name);
if ((tmp = GET_ERR()) == 1)
printk("success\n");
else {
#if FANCY_STATUS_DUMPS
printk("master: ");
switch (tmp & 0x7f) {
case 1: printk("passed");
break;
case 2: printk("formatter device error");
break;
case 3: printk("sector buffer error");
break;
case 4: printk("ECC circuitry error");
break;
case 5: printk("controlling MPU error");
break;
default:printk("error (0x%02x?)", tmp);
}
if (tmp & 0x80)
printk("; slave: failed");
printk("\n");
#else
printk("failed\n");
#endif
}
}
hwgroup->poll_timeout = 0;
}
static void do_reset1 (ide_drive_t *drive, int do_not_try_atapi)
{
unsigned int unit;
unsigned long flags;
ide_hwif_t *hwif = HWIF(drive);
ide_hwgroup_t *hwgroup = HWGROUP(drive);
save_flags(flags);
cli();
#ifdef CONFIG_BLK_DEV_IDEATAPI
if (drive->media != ide_disk) {
if (!do_not_try_atapi) {
if (!drive->keep_settings) {
drive->unmask = 0;
drive->io_32bit = 0;
}
OUT_BYTE (drive->select.all, IDE_SELECT_REG);
udelay (20);
OUT_BYTE (WIN_SRST, IDE_COMMAND_REG);
hwgroup->poll_timeout = jiffies + WAIT_WORSTCASE;
ide_set_handler (drive, &atapi_reset_pollfunc, HZ/20);
restore_flags (flags);
return;
}
}
#endif
for (unit = 0; unit < MAX_DRIVES; ++unit) {
ide_drive_t *rdrive = &hwif->drives[unit];
#ifdef CONFIG_BLK_DEV_IDETAPE
if (rdrive->media == ide_tape)
rdrive->tape.reset_issued = 1;
#endif
rdrive->special.all = 0;
rdrive->special.b.set_geometry = 1;
rdrive->special.b.recalibrate = 1;
if (OK_TO_RESET_CONTROLLER)
rdrive->mult_count = 0;
if (!rdrive->keep_settings) {
rdrive->mult_req = 0;
rdrive->unmask = 0;
rdrive->io_32bit = 0;
if (rdrive->using_dma) {
rdrive->using_dma = 0;
printk("%s: disabled DMA\n", rdrive->name);
}
}
if (rdrive->mult_req != rdrive->mult_count)
rdrive->special.b.set_multmode = 1;
}
#if OK_TO_RESET_CONTROLLER
OUT_BYTE(drive->ctl|6,IDE_CONTROL_REG);
udelay(10);
OUT_BYTE(drive->ctl|2,IDE_CONTROL_REG);
udelay(10);
hwgroup->poll_timeout = jiffies + WAIT_WORSTCASE;
ide_set_handler (drive, &reset_pollfunc, HZ/20);
#endif
restore_flags (flags);
}
void ide_do_reset (ide_drive_t *drive)
{
do_reset1 (drive, 0);
#ifdef CONFIG_BLK_DEV_IDETAPE
if (drive->media == ide_tape)
drive->tape.reset_issued=1;
#endif
}
void ide_end_drive_cmd (ide_drive_t *drive, byte stat, byte err)
{
unsigned long flags;
struct request *rq = HWGROUP(drive)->rq;
if (rq->cmd == IDE_DRIVE_CMD) {
byte *args = (byte *) rq->buffer;
rq->errors = !OK_STAT(stat,READY_STAT,BAD_STAT);
if (args) {
args[0] = stat;
args[1] = err;
args[2] = IN_BYTE(IDE_NSECTOR_REG);
}
}
save_flags(flags);
cli();
blk_dev[MAJOR(rq->rq_dev)].current_request = rq->next;
HWGROUP(drive)->rq = NULL;
rq->rq_status = RQ_INACTIVE;
if (rq->sem != NULL)
up(rq->sem);
restore_flags(flags);
}
byte ide_dump_status (ide_drive_t *drive, const char *msg, byte stat)
{
unsigned long flags;
byte err = 0;
save_flags (flags);
sti();
printk("%s: %s: status=0x%02x", drive->name, msg, stat);
#if FANCY_STATUS_DUMPS
if (drive->media == ide_disk) {
printk(" { ");
if (stat & BUSY_STAT)
printk("Busy ");
else {
if (stat & READY_STAT) printk("DriveReady ");
if (stat & WRERR_STAT) printk("DeviceFault ");
if (stat & SEEK_STAT) printk("SeekComplete ");
if (stat & DRQ_STAT) printk("DataRequest ");
if (stat & ECC_STAT) printk("CorrectedError ");
if (stat & INDEX_STAT) printk("Index ");
if (stat & ERR_STAT) printk("Error ");
}
printk("}");
}
#endif
printk("\n");
if ((stat & (BUSY_STAT|ERR_STAT)) == ERR_STAT) {
err = GET_ERR();
printk("%s: %s: error=0x%02x", drive->name, msg, err);
#if FANCY_STATUS_DUMPS
if (drive->media == ide_disk) {
printk(" { ");
if (err & ICRC_ERR) printk((err & ABRT_ERR) ? "BadCRC " : "BadSector ");
if (err & ECC_ERR) printk("UncorrectableError ");
if (err & ID_ERR) printk("SectorIdNotFound ");
if (err & ABRT_ERR) printk("DriveStatusError ");
if (err & TRK0_ERR) printk("TrackZeroNotFound ");
if (err & MARK_ERR) printk("AddrMarkNotFound ");
printk("}");
if (err & (BBD_ERR|ECC_ERR|ID_ERR|MARK_ERR)) {
byte cur = IN_BYTE(IDE_SELECT_REG);
if (cur & 0x40) {
printk(", LBAsect=%ld", (unsigned long)
((cur&0xf)<<24)
|(IN_BYTE(IDE_HCYL_REG)<<16)
|(IN_BYTE(IDE_LCYL_REG)<<8)
| IN_BYTE(IDE_SECTOR_REG));
} else {
printk(", CHS=%d/%d/%d",
(IN_BYTE(IDE_HCYL_REG)<<8) +
IN_BYTE(IDE_LCYL_REG),
cur & 0xf,
IN_BYTE(IDE_SECTOR_REG));
}
if (HWGROUP(drive)->rq)
printk(", sector=%ld", HWGROUP(drive)->rq->sector);
}
}
#endif
printk("\n");
}
restore_flags (flags);
return err;
}
static void try_to_flush_leftover_data (ide_drive_t *drive)
{
int i = (drive->mult_count ? drive->mult_count : 1) * SECTOR_WORDS;
while (i > 0) {
unsigned long buffer[16];
unsigned int wcount = (i > 16) ? 16 : i;
i -= wcount;
ide_input_data (drive, buffer, wcount);
}
}
void ide_error (ide_drive_t *drive, const char *msg, byte stat)
{
struct request *rq;
byte err;
err = ide_dump_status(drive, msg, stat);
if ((rq = HWGROUP(drive)->rq) == NULL || drive == NULL)
return;
if (rq->cmd == IDE_DRIVE_CMD) {
rq->errors = 1;
ide_end_drive_cmd(drive, stat, err);
return;
}
if (stat & BUSY_STAT) {
rq->errors |= ERROR_RESET;
} else {
if (drive->media == ide_disk && (stat & ERR_STAT)) {
if (err == ABRT_ERR) {
if (drive->select.b.lba && IN_BYTE(IDE_COMMAND_REG) == WIN_SPECIFY)
return;
} else if ((err & (ABRT_ERR | ICRC_ERR)) == (ABRT_ERR | ICRC_ERR))
;
else if (err & (BBD_ERR | ECC_ERR))
rq->errors = ERROR_MAX;
else if (err & TRK0_ERR)
rq->errors |= ERROR_RECAL;
else if (err & MC_ERR)
drive->special.b.mc = 1;
}
if ((stat & DRQ_STAT) && rq->cmd != WRITE)
try_to_flush_leftover_data(drive);
}
if (GET_STAT() & (BUSY_STAT|DRQ_STAT))
rq->errors |= ERROR_RESET;
if (rq->errors >= ERROR_MAX) {
#ifdef CONFIG_BLK_DEV_IDETAPE
if (drive->media == ide_tape) {
rq->errors = 0;
idetape_end_request(0, HWGROUP(drive));
} else
#endif
#ifdef CONFIG_BLK_DEV_IDEFLOPPY
if (drive->media == ide_floppy) {
rq->errors = 0;
idefloppy_end_request(0, HWGROUP(drive));
} else
#endif
#ifdef CONFIG_BLK_DEV_IDESCSI
if (drive->media == ide_scsi) {
rq->errors = 0;
idescsi_end_request(0, HWGROUP(drive));
} else
#endif
ide_end_request(0, HWGROUP(drive));
}
else {
if ((rq->errors & ERROR_RESET) == ERROR_RESET) {
++rq->errors;
ide_do_reset(drive);
return;
} else if ((rq->errors & ERROR_RECAL) == ERROR_RECAL)
drive->special.b.recalibrate = 1;
++rq->errors;
}
}
static void read_intr (ide_drive_t *drive)
{
byte stat;
int i;
unsigned int msect, nsect;
struct request *rq;
if (!OK_STAT(stat=GET_STAT(),DATA_READY,BAD_R_STAT)) {
ide_error(drive, "read_intr", stat);
return;
}
msect = drive->mult_count;
read_next:
rq = HWGROUP(drive)->rq;
if (msect) {
if ((nsect = rq->current_nr_sectors) > msect)
nsect = msect;
msect -= nsect;
} else
nsect = 1;
i = rq->nr_sectors - nsect;
if (i > 0 && !msect)
ide_set_handler (drive, &read_intr, WAIT_CMD);
ide_input_data(drive, rq->buffer, nsect * SECTOR_WORDS);
#ifdef DEBUG
printk("%s:  read: sectors(%ld-%ld), buffer=0x%08lx, remaining=%ld\n",
drive->name, rq->sector, rq->sector+nsect-1,
(unsigned long) rq->buffer+(nsect<<9), rq->nr_sectors-nsect);
#endif
rq->sector += nsect;
rq->buffer += nsect<<9;
rq->errors = 0;
rq->nr_sectors = i;
if ((rq->current_nr_sectors -= nsect) <= 0)
ide_end_request(1, HWGROUP(drive));
if (i > 0 && msect)
goto read_next;
}
static void write_intr (ide_drive_t *drive)
{
byte stat;
int i;
ide_hwgroup_t *hwgroup = HWGROUP(drive);
struct request *rq = hwgroup->rq;
if (OK_STAT(stat=GET_STAT(),DRIVE_READY,drive->bad_wstat)) {
#ifdef DEBUG
printk("%s: write: sector %ld, buffer=0x%08lx, remaining=%ld\n",
drive->name, rq->sector, (unsigned long) rq->buffer,
rq->nr_sectors-1);
#endif
if ((rq->nr_sectors == 1) ^ ((stat & DRQ_STAT) != 0)) {
rq->sector++;
rq->buffer += 512;
rq->errors = 0;
i = --rq->nr_sectors;
--rq->current_nr_sectors;
if (rq->current_nr_sectors <= 0)
ide_end_request(1, hwgroup);
if (i > 0) {
ide_set_handler (drive, &write_intr, WAIT_CMD);
ide_output_data (drive, rq->buffer, SECTOR_WORDS);
}
return;
}
}
ide_error(drive, "write_intr", stat);
}
void ide_multwrite (ide_drive_t *drive, unsigned int mcount)
{
struct request *rq = &HWGROUP(drive)->wrq;
do {
unsigned int nsect = rq->current_nr_sectors;
if (nsect > mcount)
nsect = mcount;
mcount -= nsect;
ide_output_data(drive, rq->buffer, nsect<<7);
#ifdef DEBUG
printk("%s: multwrite: sector %ld, buffer=0x%08lx, count=%d, remaining=%ld\n",
drive->name, rq->sector, (unsigned long) rq->buffer,
nsect, rq->nr_sectors - nsect);
#endif
if ((rq->nr_sectors -= nsect) <= 0)
break;
if ((rq->current_nr_sectors -= nsect) == 0) {
if ((rq->bh = rq->bh->b_reqnext) != NULL) {
rq->current_nr_sectors = rq->bh->b_size>>9;
rq->buffer = rq->bh->b_data;
} else {
panic("%s: buffer list corrupted\n", drive->name);
break;
}
} else {
rq->buffer += nsect << 9;
}
} while (mcount);
}
static void multwrite_intr (ide_drive_t *drive)
{
byte stat;
int i;
ide_hwgroup_t *hwgroup = HWGROUP(drive);
struct request *rq = &hwgroup->wrq;
if (OK_STAT(stat=GET_STAT(),DRIVE_READY,drive->bad_wstat)) {
if (stat & DRQ_STAT) {
if (rq->nr_sectors) {
ide_set_handler (drive, &multwrite_intr, WAIT_CMD);
ide_multwrite(drive, drive->mult_count);
return;
}
} else {
if (!rq->nr_sectors) {
rq = hwgroup->rq;
for (i = rq->nr_sectors; i > 0;){
i -= rq->current_nr_sectors;
ide_end_request(1, hwgroup);
}
return;
}
}
}
ide_error(drive, "multwrite_intr", stat);
}
static void ide_cmd(ide_drive_t *drive, byte cmd, byte nsect, ide_handler_t *handler)
{
ide_set_handler (drive, handler, WAIT_CMD);
OUT_BYTE(drive->ctl,IDE_CONTROL_REG);
OUT_BYTE(nsect,IDE_NSECTOR_REG);
OUT_BYTE(cmd,IDE_COMMAND_REG);
}
static void set_multmode_intr (ide_drive_t *drive)
{
byte stat = GET_STAT();
sti();
if (OK_STAT(stat,READY_STAT,BAD_STAT)) {
drive->mult_count = drive->mult_req;
} else {
drive->mult_req = drive->mult_count = 0;
drive->special.b.recalibrate = 1;
(void) ide_dump_status(drive, "set_multmode", stat);
}
}
static void set_geometry_intr (ide_drive_t *drive)
{
byte stat = GET_STAT();
sti();
if (!OK_STAT(stat,READY_STAT,BAD_STAT))
ide_error(drive, "set_geometry_intr", stat);
}
static void recal_intr (ide_drive_t *drive)
{
byte stat = GET_STAT();
sti();
if (!OK_STAT(stat,READY_STAT,BAD_STAT))
ide_error(drive, "recal_intr", stat);
}
static void mc_intr (ide_drive_t *drive)
{
byte stat = GET_STAT();
sti();
if (!OK_STAT(stat,READY_STAT,BAD_STAT))
ide_error(drive, "mc_intr", stat);
drive->special.b.mc = 0;
}
static void drive_cmd_intr (ide_drive_t *drive)
{
struct request *rq = HWGROUP(drive)->rq;
byte *args = (byte *) rq->buffer;
byte stat = GET_STAT();
sti();
if ((stat & DRQ_STAT) && args && args[3]) {
byte io_32bit = drive->io_32bit;
drive->io_32bit = 0;
ide_input_data(drive, &args[4], args[3] * SECTOR_WORDS);
drive->io_32bit = io_32bit;
stat = GET_STAT();
}
if (OK_STAT(stat,READY_STAT,BAD_STAT))
ide_end_drive_cmd (drive, stat, GET_ERR());
else
ide_error(drive, "drive_cmd", stat);
}
static inline void do_special (ide_drive_t *drive)
{
special_t *s = &drive->special;
#ifdef DEBUG
printk("%s: do_special: 0x%02x\n", drive->name, s->all);
#endif
if (s->b.set_geometry) {
s->b.set_geometry = 0;
if (drive->media == ide_disk && !drive->no_geom) {
OUT_BYTE(drive->sect,IDE_SECTOR_REG);
OUT_BYTE(drive->cyl,IDE_LCYL_REG);
OUT_BYTE(drive->cyl>>8,IDE_HCYL_REG);
OUT_BYTE(((drive->head-1)|drive->select.all)&0xBF,IDE_SELECT_REG);
if (!IS_PROMISE_DRIVE)
ide_cmd(drive, WIN_SPECIFY, drive->sect, &set_geometry_intr);
}
} else if (s->b.recalibrate) {
s->b.recalibrate = 0;
if (drive->media == ide_disk && !IS_PROMISE_DRIVE)
ide_cmd(drive, WIN_RESTORE, drive->sect, &recal_intr);
} else if (s->b.set_tune) {
ide_tuneproc_t *tuneproc = HWIF(drive)->tuneproc;
s->b.set_tune = 0;
if (tuneproc != NULL)
tuneproc(drive, drive->tune_req);
} else if (s->b.set_multmode) {
s->b.set_multmode = 0;
if (drive->media == ide_disk) {
if (drive->id && drive->mult_req > drive->id->max_multsect)
drive->mult_req = drive->id->max_multsect;
if (!IS_PROMISE_DRIVE)
ide_cmd(drive, WIN_SETMULT, drive->mult_req, &set_multmode_intr);
} else
drive->mult_req = 0;
} else if (s->b.mc) {
s->b.mc = 0;
if (drive->media == ide_disk && !IS_PROMISE_DRIVE)
ide_cmd(drive, WIN_ACKMC, drive->sect, &mc_intr);
} else if (s->all) {
int special = s->all;
s->all = 0;
printk("%s: bad special flag: 0x%02x\n", drive->name, special);
}
}
int ide_wait_stat (ide_drive_t *drive, byte good, byte bad, unsigned long timeout)
{
byte stat;
unsigned long flags;
udelay(1);
if ((stat = GET_STAT()) & BUSY_STAT) {
save_flags(flags);
sti();
timeout += jiffies;
while ((stat = GET_STAT()) & BUSY_STAT) {
if (jiffies > timeout) {
restore_flags(flags);
ide_error(drive, "status timeout", stat);
return 1;
}
}
restore_flags(flags);
}
udelay(1);
if (OK_STAT((stat = GET_STAT()), good, bad))
return 0;
ide_error(drive, "status error", stat);
return 1;
}
static inline void do_rw_disk (ide_drive_t *drive, struct request *rq, unsigned long block)
{
ide_hwif_t *hwif = HWIF(drive);
unsigned short io_base = hwif->io_base;
#ifdef CONFIG_BLK_DEV_PROMISE
int use_promise_io = 0;
#endif
OUT_BYTE(drive->ctl,IDE_CONTROL_REG);
OUT_BYTE(rq->nr_sectors,io_base+IDE_NSECTOR_OFFSET);
#ifdef CONFIG_BLK_DEV_PROMISE
if (IS_PROMISE_DRIVE) {
if (hwif->is_promise2 || rq->cmd == READ) {
use_promise_io = 1;
}
}
if (drive->select.b.lba || use_promise_io) {
#else
if (drive->select.b.lba) {
#endif
if (block >= 1UL << 28) {
printk("block %lu beyond LBA28\n", block);
ide_end_request(0, hwif->hwgroup);
return;
}
#ifdef DEBUG
printk("%s: %sing: LBAsect=%ld, sectors=%ld, buffer=0x%08lx\n",
drive->name, (rq->cmd==READ)?"read":"writ",
block, rq->nr_sectors, (unsigned long) rq->buffer);
#endif
OUT_BYTE(block,io_base+IDE_SECTOR_OFFSET);
OUT_BYTE(block>>=8,io_base+IDE_LCYL_OFFSET);
OUT_BYTE(block>>=8,io_base+IDE_HCYL_OFFSET);
OUT_BYTE(((block>>8)&0x0f)|drive->select.all,io_base+IDE_SELECT_OFFSET);
} else {
unsigned int sect,head,cyl,track;
track = block / drive->sect;
sect = block % drive->sect + 1;
OUT_BYTE(sect,io_base+IDE_SECTOR_OFFSET);
head = track % drive->head;
cyl = track / drive->head;
if (cyl >= 1 << 16) {
printk("block %lu cylinder %u beyond CHS\n", block, cyl);
ide_end_request(0, hwif->hwgroup);
return;
}
OUT_BYTE(cyl,io_base+IDE_LCYL_OFFSET);
OUT_BYTE(cyl>>8,io_base+IDE_HCYL_OFFSET);
OUT_BYTE(head|drive->select.all,io_base+IDE_SELECT_OFFSET);
#ifdef DEBUG
printk("%s: %sing: CHS=%d/%d/%d, sectors=%ld, buffer=0x%08lx\n",
drive->name, (rq->cmd==READ)?"read":"writ", cyl,
head, sect, rq->nr_sectors, (unsigned long) rq->buffer);
#endif
}
#ifdef CONFIG_BLK_DEV_PROMISE
if (use_promise_io) {
do_promise_io (drive, rq);
return;
}
#endif
if (rq->cmd == READ) {
#ifdef CONFIG_BLK_DEV_TRITON
if (drive->using_dma && !(HWIF(drive)->dmaproc(ide_dma_read, drive)))
return;
#endif
ide_set_handler(drive, &read_intr, WAIT_CMD);
OUT_BYTE(drive->mult_count ? WIN_MULTREAD : WIN_READ, io_base+IDE_COMMAND_OFFSET);
return;
}
if (rq->cmd == WRITE) {
#ifdef CONFIG_BLK_DEV_TRITON
if (drive->using_dma && !(HWIF(drive)->dmaproc(ide_dma_write, drive)))
return;
#endif
if (drive->mult_count)
ide_set_handler (drive, &multwrite_intr, WAIT_CMD);
else
ide_set_handler (drive, &write_intr, WAIT_CMD);
OUT_BYTE(drive->mult_count ? WIN_MULTWRITE : WIN_WRITE, io_base+IDE_COMMAND_OFFSET);
if (ide_wait_stat(drive, DATA_READY, drive->bad_wstat, WAIT_DRQ)) {
printk("%s: no DRQ after issuing %s\n", drive->name,
drive->mult_count ? "MULTWRITE" : "WRITE");
return;
}
if (!drive->unmask)
cli();
if (drive->mult_count) {
HWGROUP(drive)->wrq = *rq;
ide_multwrite(drive, drive->mult_count);
} else {
ide_output_data(drive, rq->buffer, SECTOR_WORDS);
}
return;
}
printk("%s: bad command: %d\n", drive->name, rq->cmd);
ide_end_request(0, HWGROUP(drive));
}
static void execute_drive_cmd (ide_drive_t *drive, struct request *rq)
{
byte *args = (byte *)rq->buffer;
if (args) {
#ifdef DEBUG
printk("%s: DRIVE_CMD cmd=0x%02x sc=0x%02x fr=0x%02x xx=0x%02x\n",
drive->name, args[0], args[1], args[2], args[3]);
#endif
OUT_BYTE(args[2],IDE_FEATURE_REG);
ide_cmd(drive, args[0], args[1], &drive_cmd_intr);
return;
} else {
#ifdef DEBUG
printk("%s: DRIVE_CMD (null)\n", drive->name);
#endif
ide_end_drive_cmd(drive, GET_STAT(), GET_ERR());
return;
}
}
static inline void do_request (ide_hwif_t *hwif, struct request *rq)
{
unsigned int minor, unit;
unsigned long block, blockend;
ide_drive_t *drive;
sti();
#ifdef DEBUG
printk("%s: do_request: current=0x%08lx\n", hwif->name, (unsigned long) rq);
#endif
minor = MINOR(rq->rq_dev);
unit = minor >> PARTN_BITS;
if (MAJOR(rq->rq_dev) != hwif->major || unit >= MAX_DRIVES) {
printk("%s: bad device number: %s\n",
hwif->name, kdevname(rq->rq_dev));
goto kill_rq;
}
drive = &hwif->drives[unit];
#ifdef DEBUG
if (rq->bh && !buffer_locked(rq->bh)) {
printk("%s: block not locked\n", drive->name);
goto kill_rq;
}
#endif
block = rq->sector;
blockend = block + rq->nr_sectors;
if ((blockend < block) || (blockend > drive->part[minor&PARTN_MASK].nr_sects)) {
#ifdef MACH
printk ("%s%c: bad access: block=%ld, count=%ld, blockend=%ld, nr_sects%ld\n",
drive->name, (minor&PARTN_MASK)?'0'+(minor&PARTN_MASK):' ',
block, rq->nr_sectors, blockend, drive->part[minor&PARTN_MASK].nr_sects);
#else
printk("%s%c: bad access: block=%ld, count=%ld\n", drive->name,
(minor&PARTN_MASK)?'0'+(minor&PARTN_MASK):' ', block, rq->nr_sectors);
#endif
goto kill_rq;
}
block += drive->part[minor&PARTN_MASK].start_sect + drive->sect0;
#if FAKE_FDISK_FOR_EZDRIVE
if (block == 0 && drive->remap_0_to_1)
block = 1;
#endif
((ide_hwgroup_t *)hwif->hwgroup)->drive = drive;
#if (DISK_RECOVERY_TIME > 0)
while ((read_timer() - hwif->last_time) < DISK_RECOVERY_TIME);
#endif
#ifdef CONFIG_BLK_DEV_IDETAPE
POLL_HWIF_TAPE_DRIVE;
#endif
SELECT_DRIVE(hwif,drive);
if (ide_wait_stat(drive, drive->ready_stat, BUSY_STAT|DRQ_STAT, WAIT_READY)) {
printk("%s: drive not ready for command\n", drive->name);
return;
}
if (!drive->special.all) {
if (rq->cmd == IDE_DRIVE_CMD) {
execute_drive_cmd(drive, rq);
return;
}
#ifdef CONFIG_BLK_DEV_IDEATAPI
switch (drive->media) {
case ide_disk:
do_rw_disk (drive, rq, block);
return;
#ifdef CONFIG_BLK_DEV_IDECD
case ide_cdrom:
ide_do_rw_cdrom (drive, block);
return;
#endif
#ifdef CONFIG_BLK_DEV_IDETAPE
case ide_tape:
idetape_do_request (drive, rq, block);
return;
#endif
#ifdef CONFIG_BLK_DEV_IDEFLOPPY
case ide_floppy:
idefloppy_do_request (drive, rq, block);
return;
#endif
#ifdef CONFIG_BLK_DEV_IDESCSI
case ide_scsi:
idescsi_do_request (drive, rq, block);
return;
#endif
default:
printk("%s: media type %d not supported\n",
drive->name, drive->media);
goto kill_rq;
}
#else
do_rw_disk (drive, rq, block);
return;
#endif
}
do_special(drive);
return;
kill_rq:
ide_end_request(0, hwif->hwgroup);
}
void ide_do_request (ide_hwgroup_t *hwgroup)
{
cli();
if (hwgroup->handler != NULL) {
printk("%s: EEeekk!! handler not NULL in ide_do_request()\n", hwgroup->hwif->name);
return;
}
do {
ide_hwif_t *hwif = hwgroup->hwif;
struct request *rq;
if ((rq = hwgroup->rq) == NULL) {
if (hwif->sharing_irq && hwgroup->drive)
OUT_BYTE(hwgroup->drive->ctl|2,hwif->ctl_port);
hwif = hwgroup->next_hwif;
do {
rq = blk_dev[hwif->major].current_request;
if (rq != NULL && rq->rq_status != RQ_INACTIVE)
goto got_rq;
} while ((hwif = hwif->next) != hwgroup->next_hwif);
hwgroup->active = 0;
return;
}
got_rq:
do_request(hwgroup->hwif = hwgroup->next_hwif = hwif, hwgroup->rq = rq);
cli();
} while (hwgroup->handler == NULL);
}
static void do_hwgroup_request (ide_hwgroup_t *hwgroup)
{
if (hwgroup->handler == NULL) {
ide_hwif_t *hgif = hwgroup->hwif;
ide_hwif_t *hwif = hgif;
hwgroup->active = 1;
do {
disable_irq(hwif->irq);
} while ((hwif = hwif->next) != hgif);
ide_do_request (hwgroup);
do {
enable_irq(hwif->irq);
} while ((hwif = hwif->next) != hgif);
}
}
static void do_ide0_request (void)
{
do_hwgroup_request (ide_hwifs[0].hwgroup);
}
#if MAX_HWIFS > 1
static void do_ide1_request (void)
{
do_hwgroup_request (ide_hwifs[1].hwgroup);
}
#endif
#if MAX_HWIFS > 2
static void do_ide2_request (void)
{
do_hwgroup_request (ide_hwifs[2].hwgroup);
}
#endif
#if MAX_HWIFS > 3
static void do_ide3_request (void)
{
do_hwgroup_request (ide_hwifs[3].hwgroup);
}
#endif
static void timer_expiry (unsigned long data)
{
ide_hwgroup_t *hwgroup = (ide_hwgroup_t *) data;
ide_drive_t *drive = hwgroup->drive;
unsigned long flags;
save_flags(flags);
cli();
if (hwgroup->poll_timeout != 0) {
ide_handler_t *handler = hwgroup->handler;
hwgroup->handler = NULL;
handler(drive);
} else if (hwgroup->handler == NULL) {
sti();
printk("%s: marginal timeout\n", drive->name);
} else {
hwgroup->handler = NULL;
if (hwgroup->hwif->dmaproc)
(void) hwgroup->hwif->dmaproc (ide_dma_abort, drive);
ide_error(drive, "irq timeout", GET_STAT());
}
if (hwgroup->handler == NULL)
do_hwgroup_request (hwgroup);
restore_flags(flags);
}
static void unexpected_intr (int irq, ide_hwgroup_t *hwgroup)
{
byte stat;
unsigned int unit;
ide_hwif_t *hwif = hwgroup->hwif;
do {
if (hwif->irq == irq) {
for (unit = 0; unit < MAX_DRIVES; ++unit) {
ide_drive_t *drive = &hwif->drives[unit];
if (!drive->present)
continue;
SELECT_DRIVE(hwif,drive);
udelay(100);
if (!OK_STAT(stat=GET_STAT(), drive->ready_stat, BAD_STAT)) {
static unsigned long last_msgtime = 0;
if ((last_msgtime + (HZ/2)) < jiffies) {
last_msgtime = jiffies;
(void) ide_dump_status(drive, "unexpected_intr", stat);
}
}
if ((stat & DRQ_STAT))
try_to_flush_leftover_data(drive);
}
}
} while ((hwif = hwif->next) != hwgroup->hwif);
SELECT_DRIVE(hwif,hwgroup->drive);
udelay(100);
}
void ide_intr (int irq, void *dev_id, struct pt_regs *regs)
{
ide_hwgroup_t *hwgroup = dev_id;
ide_handler_t *handler;
if (irq == hwgroup->hwif->irq && (handler = hwgroup->handler) != NULL) {
ide_drive_t *drive = hwgroup->drive;
hwgroup->handler = NULL;
del_timer(&(hwgroup->timer));
if (drive->unmask)
sti();
handler(drive);
cli();
if (hwgroup->handler == NULL) {
SET_RECOVERY_TIMER(HWIF(drive));
ide_do_request(hwgroup);
}
} else {
unexpected_intr(irq, hwgroup);
}
cli();
}
static ide_drive_t *get_info_ptr (kdev_t i_rdev)
{
int major = MAJOR(i_rdev);
unsigned int h;
for (h = 0; h < MAX_HWIFS; ++h) {
ide_hwif_t *hwif = &ide_hwifs[h];
if (hwif->present && major == hwif->major) {
unsigned unit = DEVICE_NR(i_rdev);
if (unit < MAX_DRIVES) {
ide_drive_t *drive = &hwif->drives[unit];
if (drive->present)
return drive;
} else if (major == IDE0_MAJOR && unit < 4) {
printk("ide: probable bad entry for /dev/hd%c\n", 'a'+unit);
printk("ide: to fix it, run:  /usr/src/linux/scripts/MAKEDEV.ide\n");
}
break;
}
}
return NULL;
}
void ide_init_drive_cmd (struct request *rq)
{
rq->buffer = NULL;
rq->cmd = IDE_DRIVE_CMD;
rq->sector = 0;
rq->nr_sectors = 0;
rq->current_nr_sectors = 0;
rq->sem = NULL;
rq->bh = NULL;
rq->bhtail = NULL;
rq->next = NULL;
#if 0
rq->errors = 0;
rq->rq_status = RQ_ACTIVE;
rq->rq_dev = ????;
#endif
rq->quiet = 0;
}
int ide_do_drive_cmd (ide_drive_t *drive, struct request *rq, ide_action_t action)
{
unsigned long flags;
unsigned int major = HWIF(drive)->major;
struct request *cur_rq;
struct blk_dev_struct *bdev = &blk_dev[major];
struct semaphore sem = MUTEX_LOCKED;
if (IS_PROMISE_DRIVE && rq->buffer != NULL)
return -ENOSYS;
rq->errors = 0;
rq->rq_status = RQ_ACTIVE;
rq->rq_dev = MKDEV(major,(drive->select.b.unit)<<PARTN_BITS);
if (action == ide_wait)
rq->sem = &sem;
unplug_device(bdev);
save_flags(flags);
cli();
if (action == ide_next)
HWGROUP(drive)->next_hwif = HWIF(drive);
cur_rq = bdev->current_request;
if (cur_rq == NULL || action == ide_preempt) {
rq->next = cur_rq;
bdev->current_request = rq;
if (action == ide_preempt)
HWGROUP(drive)->rq = NULL;
} else {
if (action == ide_wait || action == ide_end) {
while (cur_rq->next != NULL)
cur_rq = cur_rq->next;
}
rq->next = cur_rq->next;
cur_rq->next = rq;
}
if (!HWGROUP(drive)->active) {
do_hwgroup_request(HWGROUP(drive));
cli();
}
if (action == ide_wait && rq->rq_status != RQ_INACTIVE)
down(&sem);
restore_flags(flags);
return rq->errors ? -EIO : 0;
}
static int ide_open(struct inode * inode, struct file * filp)
{
ide_drive_t *drive;
unsigned long flags;
if ((drive = get_info_ptr(inode->i_rdev)) == NULL)
return -ENXIO;
save_flags(flags);
cli();
while (drive->busy)
sleep_on(&drive->wqueue);
drive->usage++;
restore_flags(flags);
#ifdef CONFIG_BLK_DEV_IDECD
if (drive->media == ide_cdrom)
return ide_cdrom_open (inode, filp, drive);
#endif
#ifdef CONFIG_BLK_DEV_IDETAPE
if (drive->media == ide_tape)
return idetape_blkdev_open (inode, filp, drive);
#endif
#ifdef CONFIG_BLK_DEV_IDEFLOPPY
if (drive->media == ide_floppy)
return idefloppy_open (inode, filp, drive);
#endif
#ifdef CONFIG_BLK_DEV_IDESCSI
if (drive->media == ide_scsi)
return idescsi_open (inode, filp, drive);
#endif
if (drive->removable && drive->usage == 1) {
byte door_lock[] = {WIN_DOORLOCK,0,0,0};
struct request rq;
check_disk_change(inode->i_rdev);
ide_init_drive_cmd (&rq);
rq.buffer = (char *)door_lock;
(void) ide_do_drive_cmd(drive, &rq, ide_wait);
}
return 0;
}
static void ide_release(struct inode * inode, struct file * file)
{
ide_drive_t *drive;
if ((drive = get_info_ptr(inode->i_rdev)) != NULL) {
fsync_dev(inode->i_rdev);
drive->usage--;
#ifdef CONFIG_BLK_DEV_IDECD
if (drive->media == ide_cdrom) {
ide_cdrom_release (inode, file, drive);
return;
}
#endif
#ifdef CONFIG_BLK_DEV_IDETAPE
if (drive->media == ide_tape) {
idetape_blkdev_release (inode, file, drive);
return;
}
#endif
#ifdef CONFIG_BLK_DEV_IDEFLOPPY
if (drive->media == ide_floppy) {
idefloppy_release (inode, file, drive);
return;
}
#endif
#ifdef CONFIG_BLK_DEV_IDESCSI
if (drive->media == ide_scsi) {
idescsi_ide_release (inode, file, drive);
return;
}
#endif
if (drive->removable && !drive->usage) {
byte door_unlock[] = {WIN_DOORUNLOCK,0,0,0};
struct request rq;
invalidate_buffers(inode->i_rdev);
ide_init_drive_cmd (&rq);
rq.buffer = (char *)door_unlock;
(void) ide_do_drive_cmd(drive, &rq, ide_wait);
}
}
}
static int revalidate_disk(kdev_t i_rdev)
{
ide_drive_t *drive;
unsigned int p, major, minor;
long flags;
if ((drive = get_info_ptr(i_rdev)) == NULL)
return -ENODEV;
major = MAJOR(i_rdev);
minor = drive->select.b.unit << PARTN_BITS;
save_flags(flags);
cli();
if (drive->busy || (drive->usage > 1)) {
restore_flags(flags);
return -EBUSY;
};
drive->busy = 1;
restore_flags(flags);
for (p = 0; p < (1<<PARTN_BITS); ++p) {
if (drive->part[p].nr_sects > 0) {
kdev_t devp = MKDEV(major, minor+p);
fsync_dev (devp);
invalidate_inodes (devp);
invalidate_buffers (devp);
set_blocksize(devp, 1024);
}
drive->part[p].start_sect = 0;
drive->part[p].nr_sects = 0;
};
drive->part[0].nr_sects = current_capacity(drive);
if ((drive->media != ide_disk && drive->media != ide_floppy) || !drive->part[0].nr_sects)
drive->part[0].start_sect = -1;
resetup_one_dev(HWIF(drive)->gd, drive->select.b.unit);
drive->busy = 0;
wake_up(&drive->wqueue);
return 0;
}
static int write_fs_long (unsigned long useraddr, long value)
{
int err;
if (NULL == (long *)useraddr)
return -EINVAL;
if ((err = verify_area(VERIFY_WRITE, (long *)useraddr, sizeof(long))))
return err;
put_user((unsigned)value, (long *) useraddr);
return 0;
}
static int ide_ioctl (struct inode *inode, struct file *file,
unsigned int cmd, unsigned long arg)
{
int err;
ide_drive_t *drive;
unsigned long flags;
struct request rq;
if (!inode || !(inode->i_rdev))
return -EINVAL;
if ((drive = get_info_ptr(inode->i_rdev)) == NULL)
return -ENODEV;
ide_init_drive_cmd (&rq);
switch (cmd) {
case HDIO_GETGEO:
{
struct hd_geometry *loc = (struct hd_geometry *) arg;
if (!loc || (drive->media != ide_disk && drive->media != ide_floppy)) return -EINVAL;
#ifdef MACH
loc->heads = drive->bios_head;
loc->sectors = drive->bios_sect;
loc->cylinders = drive->bios_cyl;
loc->start
= (drive->part[MINOR(inode->i_rdev)&PARTN_MASK]
.start_sect);
#else
err = verify_area(VERIFY_WRITE, loc, sizeof(*loc));
if (err) return err;
put_user(drive->bios_head, (byte *) &loc->heads);
put_user(drive->bios_sect, (byte *) &loc->sectors);
put_user(drive->bios_cyl, (unsigned short *) &loc->cylinders);
put_user((unsigned)drive->part[MINOR(inode->i_rdev)&PARTN_MASK].start_sect,
(unsigned long *) &loc->start);
#endif
return 0;
}
case BLKFLSBUF:
if (!suser()) return -EACCES;
fsync_dev(inode->i_rdev);
invalidate_buffers(inode->i_rdev);
return 0;
case BLKRASET:
if (!suser()) return -EACCES;
if(arg > 0xff) return -EINVAL;
read_ahead[MAJOR(inode->i_rdev)] = arg;
return 0;
case BLKRAGET:
return write_fs_long(arg, read_ahead[MAJOR(inode->i_rdev)]);
case BLKGETSIZE:
return write_fs_long(arg, drive->part[MINOR(inode->i_rdev)&PARTN_MASK].nr_sects);
case BLKRRPART:
if (!suser()) return -EACCES;
return revalidate_disk(inode->i_rdev);
case HDIO_GET_KEEPSETTINGS:
return write_fs_long(arg, drive->keep_settings);
case HDIO_GET_UNMASKINTR:
return write_fs_long(arg, drive->unmask);
case HDIO_GET_DMA:
return write_fs_long(arg, drive->using_dma);
case HDIO_GET_32BIT:
return write_fs_long(arg, drive->io_32bit);
case HDIO_GET_MULTCOUNT:
return write_fs_long(arg, drive->mult_count);
case HDIO_GET_IDENTITY:
if (!arg || (MINOR(inode->i_rdev) & PARTN_MASK))
return -EINVAL;
if (drive->id == NULL)
return -ENOMSG;
err = verify_area(VERIFY_WRITE, (char *)arg, sizeof(*drive->id));
if (!err)
memcpy_tofs((char *)arg, (char *)drive->id, sizeof(*drive->id));
return err;
case HDIO_GET_NOWERR:
return write_fs_long(arg, drive->bad_wstat == BAD_R_STAT);
case HDIO_SET_DMA:
if (!suser()) return -EACCES;
#ifdef CONFIG_BLK_DEV_IDECD
if (drive->media == ide_cdrom)
return -EPERM;
#endif
if (!drive->id || !(drive->id->capability & 1) || !HWIF(drive)->dmaproc)
return -EPERM;
case HDIO_SET_KEEPSETTINGS:
case HDIO_SET_UNMASKINTR:
case HDIO_SET_NOWERR:
if (arg > 1)
return -EINVAL;
case HDIO_SET_32BIT:
if (!suser()) return -EACCES;
if ((MINOR(inode->i_rdev) & PARTN_MASK))
return -EINVAL;
save_flags(flags);
cli();
switch (cmd) {
case HDIO_SET_DMA:
if (!(HWIF(drive)->dmaproc)) {
restore_flags(flags);
return -EPERM;
}
drive->using_dma = arg;
break;
case HDIO_SET_KEEPSETTINGS:
drive->keep_settings = arg;
break;
case HDIO_SET_UNMASKINTR:
if (arg && drive->no_unmask) {
restore_flags(flags);
return -EPERM;
}
drive->unmask = arg;
break;
case HDIO_SET_NOWERR:
drive->bad_wstat = arg ? BAD_R_STAT : BAD_W_STAT;
break;
case HDIO_SET_32BIT:
if (arg > (1 + (SUPPORT_VLB_SYNC<<1))) {
restore_flags(flags);
return -EINVAL;
}
if (arg && drive->no_io_32bit) {
restore_flags(flags);
return -EPERM;
}
drive->io_32bit = arg;
#ifdef CONFIG_BLK_DEV_DTC2278
if (HWIF(drive)->chipset == ide_dtc2278)
HWIF(drive)->drives[!drive->select.b.unit].io_32bit = arg;
#endif
break;
}
restore_flags(flags);
return 0;
case HDIO_SET_MULTCOUNT:
if (!suser()) return -EACCES;
if (MINOR(inode->i_rdev) & PARTN_MASK)
return -EINVAL;
if (drive->id && arg > drive->id->max_multsect)
return -EINVAL;
save_flags(flags);
cli();
if (drive->special.b.set_multmode) {
restore_flags(flags);
return -EBUSY;
}
drive->mult_req = arg;
drive->special.b.set_multmode = 1;
restore_flags(flags);
(void) ide_do_drive_cmd (drive, &rq, ide_wait);
return (drive->mult_count == arg) ? 0 : -EIO;
case HDIO_DRIVE_CMD:
{
byte args[4], *argbuf = args;
int argsize = 4;
if (!suser() || securelevel > 0) return -EACCES;
if (NULL == (void *) arg) {
err = ide_do_drive_cmd(drive, &rq, ide_wait);
} else if (!(err = verify_area(VERIFY_READ,(void *)arg, 4))) {
memcpy_fromfs(args, (void *)arg, 4);
if (args[3]) {
argsize = 4 + (SECTOR_WORDS * 4 * args[3]);
argbuf = kmalloc(argsize, GFP_KERNEL);
if (argbuf == NULL)
return -ENOMEM;
argbuf[0] = args[0];
argbuf[1] = args[1];
argbuf[2] = args[2];
argbuf[3] = args[3];
}
if (!(err = verify_area(VERIFY_WRITE,(void *)arg, argsize))) {
rq.buffer = (char *)argbuf;
err = ide_do_drive_cmd(drive, &rq, ide_wait);
memcpy_tofs((void *)arg, argbuf, argsize);
}
if (argsize > 4)
kfree(argbuf);
}
return err;
}
case HDIO_SET_PIO_MODE:
if (!suser()) return -EACCES;
if (MINOR(inode->i_rdev) & PARTN_MASK)
return -EINVAL;
if (!HWIF(drive)->tuneproc)
return -ENOSYS;
save_flags(flags);
cli();
if (drive->special.b.set_tune) {
restore_flags(flags);
return -EBUSY;
}
drive->tune_req = (byte) arg;
drive->special.b.set_tune = 1;
restore_flags(flags);
(void) ide_do_drive_cmd (drive, &rq, ide_wait);
return 0;
RO_IOCTLS(inode->i_rdev, arg);
default:
#ifdef CONFIG_BLK_DEV_IDECD
if (drive->media == ide_cdrom)
return ide_cdrom_ioctl(drive, inode, file, cmd, arg);
#endif
#ifdef CONFIG_BLK_DEV_IDETAPE
if (drive->media == ide_tape)
return idetape_blkdev_ioctl(drive, inode, file, cmd, arg);
#endif
#ifdef CONFIG_BLK_DEV_IDEFLOPPY
if (drive->media == ide_floppy)
return idefloppy_ioctl(drive, inode, file, cmd, arg);
#endif
#ifdef CONFIG_BLK_DEV_IDESCSI
if (drive->media == ide_scsi)
return idescsi_ioctl(drive, inode, file, cmd, arg);
#endif
return -EPERM;
}
}
static int ide_check_media_change (kdev_t i_rdev)
{
ide_drive_t *drive;
if ((drive = get_info_ptr(i_rdev)) == NULL)
return -ENODEV;
#ifdef CONFIG_BLK_DEV_IDECD
if (drive->media == ide_cdrom)
return ide_cdrom_check_media_change (drive);
#endif
#ifdef CONFIG_BLK_DEV_IDEFLOPPY
if (drive->media == ide_floppy)
return idefloppy_media_change (drive);
#endif
if (drive->removable)
return 1;
return 0;
}
void ide_fixstring (byte *s, const int bytecount, const int byteswap)
{
byte *p = s, *end = &s[bytecount & ~1];
if (byteswap) {
for (p = end ; p != s;) {
unsigned short *pp = (unsigned short *) (p -= 2);
*pp = ntohs(*pp);
}
}
while (s != end && *s == ' ')
++s;
while (s != end && *s) {
if (*s++ != ' ' || (s != end && *s && *s != ' '))
*p++ = *(s-1);
}
while (p != end)
*p++ = '\0';
}
static inline void do_identify (ide_drive_t *drive, byte cmd)
{
int bswap;
struct hd_driveid *id;
unsigned long capacity, check;
id = drive->id = kmalloc (SECTOR_WORDS*4, GFP_KERNEL);
ide_input_data(drive, id, SECTOR_WORDS);
sti();
#if defined (CONFIG_SCSI_EATA_DMA) || defined (CONFIG_SCSI_EATA_PIO) || defined (CONFIG_SCSI_EATA)
if ((id->model[0] == 'P' && id->model[1] == 'M')
|| (id->model[0] == 'S' && id->model[1] == 'K')) {
printk("%s: EATA SCSI HBA %.10s\n", drive->name, id->model);
drive->present = 0;
return;
}
#endif
bswap = 1;
if (cmd == WIN_PIDENTIFY) {
if ((id->model[0] == 'N' && id->model[1] == 'E')
|| (id->model[0] == 'F' && id->model[1] == 'X')
|| (id->model[0] == 'P' && id->model[1] == 'i'))
bswap = 0;
}
ide_fixstring (id->model, sizeof(id->model), bswap);
ide_fixstring (id->fw_rev, sizeof(id->fw_rev), bswap);
ide_fixstring (id->serial_no, sizeof(id->serial_no), bswap);
if (strstr((char *)id->model, "E X A B Y T E N E S T"))
return;
#ifdef CONFIG_BLK_DEV_IDEATAPI
if (cmd == WIN_PIDENTIFY) {
byte type = (id->config >> 8) & 0x1f;
printk("%s: %s, ATAPI ", drive->name, id->model);
#ifdef CONFIG_BLK_DEV_PROMISE
if (HWIF(drive)->is_promise2) {
printk(" -- not supported on 2nd Promise port\n");
drive->present = 0;
return;
}
#endif
if (!drive->ide_scsi) switch (type) {
case 0:
if (!strstr((char *)id->model, "oppy") &&
!strstr((char *)id->model, "poyp") &&
!strstr((char *)id->model, "ZIP"))
printk("cdrom or floppy?, assuming ");
if (drive->media != ide_cdrom &&
!strstr((char *)id->model, "CD-ROM")) {
#ifdef CONFIG_BLK_DEV_IDEFLOPPY
printk("FLOPPY drive\n");
drive->media = ide_floppy;
if (idefloppy_identify_device(drive, id))
drive->present = 1;
return;
#else
printk("FLOPPY ");
break;
#endif
}
case 5:
#ifdef CONFIG_BLK_DEV_IDECD
printk ("CDROM drive\n");
drive->media = ide_cdrom;
drive->present = 1;
drive->removable = 1;
return;
#else
printk ("CDROM ");
break;
#endif
case 1:
#ifdef CONFIG_BLK_DEV_IDETAPE
printk ("TAPE drive");
if (idetape_identify_device (drive,id)) {
drive->media = ide_tape;
drive->present = 1;
drive->removable = 1;
if (drive->autotune != 2 && HWIF(drive)->dmaproc != NULL && !drive->nodma) {
if (!HWIF(drive)->dmaproc(ide_dma_check, drive))
printk(", DMA");
}
printk("\n");
}
else {
drive->present = 0;
printk ("\nide-tape: the tape is not supported by this version of the driver\n");
}
return;
#else
printk ("TAPE ");
break;
#endif
default:
drive->present = 0;
printk("Type %d - Unknown device\n", type);
return;
}
#ifdef CONFIG_BLK_DEV_IDESCSI
printk("drive - enabling SCSI emulation\n");
drive->media = ide_scsi;
drive->present = 1;
idescsi_setup(drive);
#else
drive->present = 0;
printk("- not supported by this kernel\n");
#endif
return;
}
#endif
if (id->config & (1<<7)) {
if (id->model[0] != 'W' || id->model[1] != 'D')
drive->removable = 1;
}
if (id->model[0] == 'S' && id->model[1] == 'u') {
drive->removable = 0;
if (drive->select.all & (1<<4)) {
drive->present = 0;
return;
}
}
drive->media = ide_disk;
if (!drive->present) {
drive->present = 1;
drive->cyl = drive->bios_cyl = id->cyls;
drive->head = drive->bios_head = id->heads;
drive->sect = drive->bios_sect = id->sectors;
}
if ((id->field_valid & 1) && id->cur_cyls && id->cur_heads
&& (id->cur_heads <= 16) && id->cur_sectors) {
drive->cyl = id->cur_cyls;
drive->head = id->cur_heads;
drive->sect = id->cur_sectors;
capacity = drive->cyl * drive->head * drive->sect;
check = (id->cur_capacity0 << 16) | id->cur_capacity1;
if (check == capacity) {
id->cur_capacity0 = (capacity >> 0) & 0xffff;
id->cur_capacity1 = (capacity >> 16) & 0xffff;
}
}
if ((!drive->head || drive->head > 16) &&
id->heads && id->heads <= 16) {
drive->cyl = id->cyls;
drive->head = id->heads;
drive->sect = id->sectors;
}
capacity = current_capacity (drive);
if (capacity > drive->bios_cyl * drive->bios_head * drive->bios_sect
&& !drive->forced_geom && drive->bios_sect && drive->bios_head) {
int cyl = (capacity / drive->bios_sect) / drive->bios_head;
if (cyl <= 65535)
drive->bios_cyl = cyl;
else {
drive->bios_sect = 63;
drive->bios_head = 255;
drive->bios_cyl = capacity / (63*255);
}
}
if (!strncmp((char *)id->model, "BMI ", 4) &&
strstr((char *)id->model, " ENHANCED IDE ") &&
drive->select.b.lba)
drive->no_geom = 1;
printk ("%s: %.40s, %ldMB w/%dkB Cache, CHS=%d/%d/%d",
drive->name, id->model, current_capacity(drive)/2048L, id->buf_size/2,
drive->bios_cyl, drive->bios_head, drive->bios_sect);
drive->mult_count = 0;
if (id->max_multsect) {
drive->mult_req = INITIAL_MULT_COUNT;
if (drive->mult_req > id->max_multsect)
drive->mult_req = id->max_multsect;
if (drive->mult_req || ((id->multsect_valid & 1) && id->multsect))
drive->special.b.set_multmode = 1;
}
if (drive->autotune != 2 && HWIF(drive)->dmaproc != NULL && !drive->nodma) {
if (!(HWIF(drive)->dmaproc(ide_dma_check, drive))) {
if ((id->field_valid & 4) && (id->dma_ultra & (id->dma_ultra >> 8) & 7))
printk(", UDMA");
else
printk(", DMA");
}
}
printk("\n");
}
static void delay_50ms (void)
{
unsigned long timer = jiffies + ((HZ + 19)/20) + 1;
while (timer > jiffies);
}
static int try_to_identify (ide_drive_t *drive, byte cmd)
{
int hd_status, rc;
unsigned long timeout;
unsigned long irqs_on = 0;
int irq_off;
if (!HWIF(drive)->irq) {
printk("%s: Not probing legacy IRQs)\n", drive->name);
return 2;
probe_irq_off(probe_irq_on());
irqs_on = probe_irq_on();
OUT_BYTE(drive->ctl,IDE_CONTROL_REG);
}
delay_50ms();
if ((IN_BYTE(IDE_ALTSTATUS_REG) ^ IN_BYTE(IDE_STATUS_REG)) & ~INDEX_STAT) {
printk("%s: probing with STATUS instead of ALTSTATUS\n", drive->name);
hd_status = IDE_STATUS_REG;
} else
hd_status = IDE_ALTSTATUS_REG;
#if CONFIG_BLK_DEV_PROMISE
if (IS_PROMISE_DRIVE) {
if (promise_cmd(drive,PROMISE_IDENTIFY)) {
if (irqs_on)
(void) probe_irq_off(irqs_on);
return 1;
}
} else
#endif
OUT_BYTE(cmd,IDE_COMMAND_REG);
timeout = ((cmd == WIN_IDENTIFY) ? WAIT_WORSTCASE : WAIT_PIDENTIFY) / 2;
timeout += jiffies;
do {
if (jiffies > timeout) {
if (irqs_on)
(void) probe_irq_off(irqs_on);
return 1;
}
delay_50ms();
} while (IN_BYTE(hd_status) & BUSY_STAT);
delay_50ms();
if (OK_STAT(GET_STAT(),DRQ_STAT,BAD_R_STAT)) {
unsigned long flags;
save_flags(flags);
cli();
do_identify(drive, cmd);
rc = 0;
(void) GET_STAT();
restore_flags(flags);
} else
rc = 2;
if (!HWIF(drive)->irq) {
irq_off = probe_irq_off(irqs_on);
if (irq_off > 0) {
HWIF(drive)->irq = irq_off;
irqs_on = probe_irq_on();
OUT_BYTE(drive->ctl|2,IDE_CONTROL_REG);
udelay(5);
(void) probe_irq_off(irqs_on);
(void) probe_irq_off(probe_irq_on());
(void) GET_STAT();
} else {
printk("%s: IRQ probe failed (%d)\n", drive->name, irq_off);
#ifdef CONFIG_BLK_DEV_CMD640
#ifdef CMD640_DUMP_REGS
if (HWIF(drive)->chipset == ide_cmd640) {
printk("%s: Hmmm.. probably a driver problem.\n", drive->name);
CMD640_DUMP_REGS;
}
#endif
#endif
}
}
return rc;
}
static int do_probe (ide_drive_t *drive, byte cmd)
{
int rc;
ide_hwif_t *hwif = HWIF(drive);
unsigned long timeout;
#ifdef CONFIG_BLK_DEV_IDEATAPI
if (drive->present) {
if ((drive->media != ide_disk) && (cmd == WIN_IDENTIFY))
return 4;
}
#endif
#ifdef DEBUG
printk("probing for %s: present=%d, media=%d, probetype=%s\n",
drive->name, drive->present, drive->media,
(cmd == WIN_IDENTIFY) ? "ATA" : "ATAPI");
#endif
SELECT_DRIVE(hwif,drive);
delay_50ms();
if (IN_BYTE(IDE_SELECT_REG) != drive->select.all && !drive->present) {
OUT_BYTE(0xa0,IDE_SELECT_REG);
delay_50ms();
return 3;
}
if (OK_STAT(GET_STAT(),READY_STAT,BUSY_STAT)
|| drive->present || cmd == WIN_PIDENTIFY)
{
if ((rc = try_to_identify(drive,cmd)))
rc = try_to_identify(drive,cmd);
if (rc == 1 && cmd == WIN_PIDENTIFY && drive->autotune != 2) {
printk("%s: no response (status = 0x%02x), resetting drive\n", drive->name, GET_STAT());
delay_50ms();
OUT_BYTE (drive->select.all, IDE_SELECT_REG);
delay_50ms();
OUT_BYTE(WIN_SRST, IDE_COMMAND_REG);
timeout = jiffies;
while ((GET_STAT() & BUSY_STAT) && jiffies < timeout + WAIT_WORSTCASE)
delay_50ms();
rc = try_to_identify(drive, cmd);
}
if (rc == 1)
printk("%s: no response (status = 0x%02x)\n", drive->name, GET_STAT());
(void) GET_STAT();
} else {
rc = 3;
}
if (drive->select.b.unit != 0) {
OUT_BYTE(0xa0,IDE_SELECT_REG);
delay_50ms();
(void) GET_STAT();
}
return rc;
}
static void enable_nest (ide_drive_t *drive)
{
unsigned long timeout;
printk("%s: enabling %s -- ", HWIF(drive)->name, drive->id->model);
SELECT_DRIVE(HWIF(drive), drive);
delay_50ms();
OUT_BYTE(EXABYTE_ENABLE_NEST, IDE_COMMAND_REG);
timeout = jiffies + WAIT_WORSTCASE;
do {
if (jiffies > timeout) {
printk("failed (timeout)\n");
return;
}
delay_50ms();
} while (GET_STAT() & BUSY_STAT);
delay_50ms();
if (!OK_STAT(GET_STAT(), 0, BAD_STAT))
printk("failed (status = 0x%02x)\n", GET_STAT());
else
printk("success\n");
if (do_probe(drive, WIN_IDENTIFY) >= 2) {
#ifdef CONFIG_BLK_DEV_IDEATAPI
(void) do_probe(drive, WIN_PIDENTIFY);
#endif
}
}
static inline byte probe_for_drive (ide_drive_t *drive)
{
if (drive->noprobe)
return drive->present;
if (do_probe(drive, WIN_IDENTIFY) >= 2) {
#ifdef CONFIG_BLK_DEV_IDEATAPI
(void) do_probe(drive, WIN_PIDENTIFY);
#endif
}
if (drive->id && strstr((char *)drive->id->model, "E X A B Y T E N E S T"))
enable_nest(drive);
if (!drive->present)
return 0;
if (drive->id == NULL) {
if (drive->media == ide_disk) {
printk ("%s: non-IDE drive, CHS=%d/%d/%d\n",
drive->name, drive->cyl, drive->head, drive->sect);
}
#ifdef CONFIG_BLK_DEV_IDECD
else if (drive->media == ide_cdrom) {
printk("%s: ATAPI cdrom (?)\n", drive->name);
}
#endif
else {
drive->present = 0;
}
}
return 1;
}
static void probe_cmos_for_drives (ide_hwif_t *hwif)
{
#ifdef __i386__
extern struct drive_info_struct drive_info;
byte cmos_disks, *BIOS = (byte *) &drive_info;
int unit;
#ifdef CONFIG_BLK_DEV_PROMISE
if (hwif->is_promise2)
return;
#endif
outb_p(0x12,0x70);
cmos_disks = inb_p(0x71);
for (unit = 0; unit < MAX_DRIVES; ++unit) {
ide_drive_t *drive = &hwif->drives[unit];
if ((cmos_disks & (0xf0 >> (unit*4))) && !drive->present && !drive->nobios) {
unsigned short cyl = *(unsigned short *)BIOS;
unsigned char head = *(BIOS+2);
unsigned char sect = *(BIOS+14);
unsigned char ctl = *(BIOS+8);
if (cyl > 0 && head > 0 && sect > 0 && sect < 64 && head < 255) {
drive->cyl = drive->bios_cyl = cyl;
drive->head = drive->bios_head = head;
drive->sect = drive->bios_sect = sect;
drive->ctl = ctl;
drive->present = 1;
printk("hd%d: got CHS=%d/%d/%d CTL=%x from BIOS\n",
unit, cyl, head, sect, ctl);
} else {
printk("hd%d: CHS=%d/%d/%d CTL=%x from BIOS ignored\n",
unit, cyl, head, sect, ctl);
}
}
BIOS += 16;
}
#endif
}
static void probe_hwif (ide_hwif_t *hwif)
{
unsigned int unit;
if (hwif->noprobe)
return;
if (hwif->io_base == HD_DATA)
probe_cmos_for_drives (hwif);
#if CONFIG_BLK_DEV_PROMISE
if (!hwif->is_promise2 &&
(check_region(hwif->io_base,8) || check_region(hwif->ctl_port,1))) {
#else
if (check_region(hwif->io_base,8) || check_region(hwif->ctl_port,1)) {
#endif
int msgout = 0;
for (unit = 0; unit < MAX_DRIVES; ++unit) {
ide_drive_t *drive = &hwif->drives[unit];
if (drive->present) {
drive->present = 0;
printk("%s: ERROR, PORTS ALREADY IN USE\n", drive->name);
msgout = 1;
}
}
if (!msgout)
printk("%s: ports already in use, skipping probe\n", hwif->name);
} else {
unsigned long flags;
save_flags(flags);
sti();
for (unit = 0; unit < MAX_DRIVES; ++unit) {
ide_drive_t *drive = &hwif->drives[unit];
(void) probe_for_drive (drive);
if (drive->present && drive->media == ide_disk) {
if ((!drive->head || drive->head > 16) && !drive->select.b.lba) {
printk("%s: INVALID GEOMETRY: %d PHYSICAL HEADS?\n",
drive->name, drive->head);
drive->present = 0;
}
}
if (drive->present && !hwif->present) {
hwif->present = 1;
request_region(hwif->io_base, 8, hwif->name);
request_region(hwif->ctl_port, 1, hwif->name);
}
}
restore_flags(flags);
for (unit = 0; unit < MAX_DRIVES; ++unit) {
ide_drive_t *drive = &hwif->drives[unit];
if (drive->present && drive->media != ide_tape) {
ide_tuneproc_t *tuneproc = HWIF(drive)->tuneproc;
if (tuneproc != NULL && drive->autotune == 1)
tuneproc(drive, 255);
}
}
}
}
static int stridx (const char *s, char c)
{
char *i = strchr(s, c);
return (i && c) ? i - s : -1;
}
static int match_parm (char *s, const char *keywords[], int vals[], int max_vals)
{
static const char *decimal = "0123456789";
static const char *hex = "0123456789abcdef";
int i, n;
if (*s++ == '=') {
if (keywords != NULL) {
for (i = 0; *keywords != NULL; ++i) {
if (!strcmp(s, *keywords++))
return -(i+1);
}
}
for (n = 0; (i = stridx(decimal, *s)) >= 0;) {
vals[n] = i;
while ((i = stridx(decimal, *++s)) >= 0)
vals[n] = (vals[n] * 10) + i;
if (*s == 'x' && !vals[n]) {
while ((i = stridx(hex, *++s)) >= 0)
vals[n] = (vals[n] * 0x10) + i;
}
if (++n == max_vals)
break;
if (*s == ',')
++s;
}
if (!*s)
return n;
}
return 0;
}
void ide_setup (char *s)
{
int i, vals[3];
ide_hwif_t *hwif;
ide_drive_t *drive;
unsigned int hw, unit;
#ifdef MACH
const char max_drive = '0' + ((MAX_HWIFS * MAX_DRIVES) - 1);
#else
const char max_drive = 'a' + ((MAX_HWIFS * MAX_DRIVES) - 1);
#endif
const char max_hwif = '0' + (MAX_HWIFS - 1);
printk("ide_setup: %s", s);
init_ide_data ();
#ifdef MACH
if (s[0] == 'h' && s[1] == 'd' && s[2] >= '0' && s[2] <= max_drive) {
#else
if (s[0] == 'h' && s[1] == 'd' && s[2] >= 'a' && s[2] <= max_drive) {
#endif
const char *hd_words[] = {"none", "noprobe", "nowerr", "cdrom",
"serialize", "autotune", "noautotune",
"slow", "ide-scsi", "nodma", NULL};
#ifdef MACH
unit = s[2] - '0';
#else
unit = s[2] - 'a';
#endif
hw = unit / MAX_DRIVES;
unit = unit % MAX_DRIVES;
hwif = &ide_hwifs[hw];
drive = &hwif->drives[unit];
switch (match_parm(&s[3], hd_words, vals, 3)) {
case -1:
drive->nobios = 1;
case -2:
drive->noprobe = 1;
goto done;
case -3:
drive->bad_wstat = BAD_R_STAT;
hwif->noprobe = 0;
goto done;
case -4:
drive->present = 1;
drive->media = ide_cdrom;
hwif->noprobe = 0;
goto done;
case -5:
printk(" -- USE \"ide%d=serialize\" INSTEAD", hw);
goto do_serialize;
case -6:
drive->autotune = 1;
goto done;
case -7:
drive->autotune = 2;
goto done;
case -8:
drive->slow = 1;
goto done;
case -9:
drive->ide_scsi = 1;
goto done;
case -10:
drive->nodma = 1;
goto done;
case 3:
drive->media = ide_disk;
drive->cyl = drive->bios_cyl = vals[0];
drive->head = drive->bios_head = vals[1];
drive->sect = drive->bios_sect = vals[2];
drive->present = 1;
drive->forced_geom = 1;
hwif->noprobe = 0;
goto done;
default:
goto bad_option;
}
}
if (s[0] != 'i' || s[1] != 'd' || s[2] != 'e')
goto bad_option;
if (s[3] == 'b' && s[4] == 'u' && s[5] == 's') {
if (match_parm(&s[6], NULL, vals, 1) != 1)
goto bad_option;
if (vals[0] >= 20 && vals[0] <= 66)
idebus_parameter = vals[0];
else
printk(" -- BAD BUS SPEED! Expected value from 20 to 66");
goto done;
}
if (s[3] >= '0' && s[3] <= max_hwif) {
const char *ide_words[] = {"noprobe", "serialize", "autotune", "noautotune",
"qd6580", "ht6560b", "cmd640_vlb", "dtc2278", "umc8672", "ali14xx", "dc4030", NULL};
hw = s[3] - '0';
hwif = &ide_hwifs[hw];
i = match_parm(&s[4], ide_words, vals, 3);
if (i > 0 || i <= -5) {
if (hwif->chipset != ide_unknown)
goto bad_option;
if (i <= -5) {
if (ide_hwifs[1].chipset != ide_unknown)
goto bad_option;
if (hw != 0)
goto bad_hwif;
}
}
switch (i) {
#ifdef CONFIG_BLK_DEV_PROMISE
case -11:
{
setup_dc4030(hwif);
goto done;
}
#endif
#ifdef CONFIG_BLK_DEV_ALI14XX
case -10:
{
extern void init_ali14xx (void);
init_ali14xx();
goto done;
}
#endif
#ifdef CONFIG_BLK_DEV_UMC8672
case -9:
{
extern void init_umc8672 (void);
init_umc8672();
goto done;
}
#endif
#ifdef CONFIG_BLK_DEV_DTC2278
case -8:
{
extern void init_dtc2278 (void);
init_dtc2278();
goto done;
}
#endif
#ifdef CONFIG_BLK_DEV_CMD640
case -7:
{
extern int cmd640_vlb;
cmd640_vlb = 1;
goto done;
}
#endif
#ifdef CONFIG_BLK_DEV_HT6560B
case -6:
{
extern void init_ht6560b (void);
init_ht6560b();
goto done;
}
#endif
#if CONFIG_BLK_DEV_QD6580
case -5:
{
extern void init_qd6580 (void);
init_qd6580();
goto done;
}
#endif
case -4:
hwif->drives[0].autotune = 2;
hwif->drives[1].autotune = 2;
goto done;
case -3:
hwif->drives[0].autotune = 1;
hwif->drives[1].autotune = 1;
goto done;
case -2:
do_serialize:
ide_hwifs[hw].serialized = 1;
ide_hwifs[hw^1].serialized = 1;
goto done;
case -1:
hwif->noprobe = 1;
goto done;
case 1:
vals[1] = vals[0] + 0x206;
case 2:
vals[2] = 0;
case 3:
hwif->io_base = vals[0];
hwif->ctl_port = vals[1];
hwif->irq = vals[2];
hwif->noprobe = 0;
hwif->chipset = ide_generic;
goto done;
case 0: goto bad_option;
default:
printk(" -- SUPPORT NOT CONFIGURED IN THIS KERNEL\n");
return;
}
}
bad_option:
printk(" -- BAD OPTION\n");
return;
bad_hwif:
printk("-- NOT SUPPORTED ON ide%d", hw);
done:
printk("\n");
}
int ide_xlate_1024 (kdev_t i_rdev, int xparm, const char *msg)
{
ide_drive_t *drive;
static const byte head_vals[] = {4, 8, 16, 32, 64, 128, 255, 0};
const byte *heads = head_vals;
unsigned long tracks;
drive = get_info_ptr(i_rdev);
if (!drive)
return 0;
if (drive->forced_geom)
return 0;
if (xparm > 1 && xparm <= drive->bios_head && drive->bios_sect == 63)
return 0;
printk("%s ", msg);
if (xparm < 0 && (drive->bios_cyl * drive->bios_head * drive->bios_sect) < (1024 * 16 * 63)) {
return 0;
}
if (drive->id) {
drive->cyl = drive->id->cyls;
drive->head = drive->id->heads;
drive->sect = drive->id->sectors;
}
drive->bios_cyl = drive->cyl;
drive->bios_head = drive->head;
drive->bios_sect = drive->sect;
drive->special.b.set_geometry = 1;
tracks = drive->bios_cyl * drive->bios_head * drive->bios_sect / 63;
drive->bios_sect = 63;
if (xparm > 1) {
drive->bios_head = xparm;
drive->bios_cyl = tracks / drive->bios_head;
} else {
while (drive->bios_cyl >= 1024) {
drive->bios_head = *heads;
drive->bios_cyl = tracks / drive->bios_head;
if (0 == *++heads)
break;
}
#if FAKE_FDISK_FOR_EZDRIVE
if (xparm == -1) {
drive->remap_0_to_1 = 1;
msg = "0->1";
} else
#endif
if (xparm == 1) {
drive->sect0 = 63;
drive->bios_cyl = (tracks - 1) / drive->bios_head;
msg = "+63";
}
printk("[remap %s] ", msg);
}
drive->part[0].nr_sects = current_capacity(drive);
printk("[%d/%d/%d]", drive->bios_cyl, drive->bios_head, drive->bios_sect);
return 1;
}
#if MAX_HWIFS > 1
static void save_match (ide_hwif_t *hwif, ide_hwif_t *new, ide_hwif_t **match)
{
ide_hwif_t *m = *match;
if (m && m->hwgroup && m->hwgroup != new->hwgroup) {
if (!new->hwgroup)
return;
printk("%s: potential irq problem with %s and %s\n", hwif->name, new->name, m->name);
}
if (!m || m->irq != hwif->irq)
*match = new;
}
#endif
static int init_irq (ide_hwif_t *hwif)
{
unsigned long flags;
#if MAX_HWIFS > 1
unsigned int index;
#endif
ide_hwgroup_t *hwgroup;
ide_hwif_t *match = NULL;
save_flags(flags);
cli();
hwif->hwgroup = NULL;
#if MAX_HWIFS > 1
for (index = 0; index < MAX_HWIFS; index++) {
ide_hwif_t *h = &ide_hwifs[index];
if (h->hwgroup) {
if (hwif->irq == h->irq) {
hwif->sharing_irq = h->sharing_irq = 1;
save_match(hwif, h, &match);
}
if (hwif->serialized) {
ide_hwif_t *mate = &ide_hwifs[hwif->index^1];
if (index == mate->index || h->irq == mate->irq)
save_match(hwif, h, &match);
}
if (h->serialized) {
ide_hwif_t *mate = &ide_hwifs[h->index^1];
if (hwif->irq == mate->irq)
save_match(hwif, h, &match);
}
}
}
#endif
if (match) {
hwgroup = match->hwgroup;
} else {
hwgroup = kmalloc(sizeof(ide_hwgroup_t), GFP_KERNEL);
hwgroup->hwif = hwgroup->next_hwif = hwif->next = hwif;
hwgroup->rq = NULL;
hwgroup->handler = NULL;
if (hwif->drives[0].present)
hwgroup->drive = &hwif->drives[0];
else
hwgroup->drive = &hwif->drives[1];
hwgroup->poll_timeout = 0;
hwgroup->active = 0;
init_timer(&hwgroup->timer);
hwgroup->timer.function = &timer_expiry;
hwgroup->timer.data = (unsigned long) hwgroup;
}
if (!match || match->irq != hwif->irq) {
if (request_irq(hwif->irq, ide_intr, SA_INTERRUPT, hwif->name, hwgroup)) {
if (!match)
kfree(hwgroup);
restore_flags(flags);
return 1;
}
}
hwif->hwgroup = hwgroup;
hwif->next = hwgroup->hwif->next;
hwgroup->hwif->next = hwif;
restore_flags(flags);
printk("%s at 0x%03x-0x%03x,0x%03x on irq %d", hwif->name,
hwif->io_base, hwif->io_base+7, hwif->ctl_port, hwif->irq);
if (match)
printk(" (%sed with %s)", hwif->sharing_irq ? "shar" : "serializ", match->name);
printk("\n");
return 0;
}
static struct file_operations ide_fops = {
NULL,
block_read,
block_write,
NULL,
NULL,
ide_ioctl,
NULL,
ide_open,
ide_release,
block_fsync
,NULL,
ide_check_media_change,
revalidate_disk
};
#ifdef CONFIG_PCI
#if defined(CONFIG_BLK_DEV_RZ1000) || defined(CONFIG_BLK_DEV_TRITON)
typedef void (ide_pci_init_proc_t)(byte, byte);
static void ide_probe_pci (unsigned short vendor, unsigned short device, ide_pci_init_proc_t *init, int func_adj)
{
unsigned long flags;
unsigned index;
byte fn, bus;
save_flags(flags);
cli();
for (index = 0; !pcibios_find_device (vendor, device, index, &bus, &fn); ++index) {
init (bus, fn + func_adj);
}
restore_flags(flags);
}
#endif
static void ide_probe_promise_20246(void)
{
byte fn, bus;
unsigned short io[6], count = 0;
unsigned int reg, tmp, i;
ide_hwif_t *hwif;
memset(io, 0, 6 * sizeof(unsigned short));
if (pcibios_find_device(PCI_VENDOR_ID_PROMISE, PCI_DEVICE_ID_PROMISE_20246, 0, &bus, &fn))
return;
printk("ide: Promise Technology IDE Ultra-DMA 33 on PCI bus %d function %d\n", bus, fn);
for (reg = PCI_BASE_ADDRESS_0; reg <= PCI_BASE_ADDRESS_5; reg += 4) {
pcibios_read_config_dword(bus, fn, reg, &tmp);
if (tmp & PCI_BASE_ADDRESS_SPACE_IO)
io[count++] = tmp & PCI_BASE_ADDRESS_IO_MASK;
}
for (i = 2; i < 4; i++) {
hwif = ide_hwifs + i;
if (hwif->chipset == ide_generic) {
printk("ide%d: overridden with command line parameter\n", i);
return;
}
tmp = (i - 2) * 2;
if (!io[tmp] || !io[tmp + 1]) {
printk("ide%d: invalid port address %x, %x -- aborting\n", i, io[tmp], io[tmp + 1]);
return;
}
hwif->io_base = io[tmp];
hwif->ctl_port = io[tmp + 1] + 2;
hwif->noprobe = 0;
}
}
#endif
static void probe_for_hwifs (void)
{
#ifdef CONFIG_PCI
if (pcibios_present()) {
#ifdef CONFIG_BLK_DEV_RZ1000
ide_pci_init_proc_t init_rz1000;
ide_probe_pci (PCI_VENDOR_ID_PCTECH, PCI_DEVICE_ID_PCTECH_RZ1000, &init_rz1000, 0);
ide_probe_pci (PCI_VENDOR_ID_PCTECH, PCI_DEVICE_ID_PCTECH_RZ1001, &init_rz1000, 0);
#endif
#ifdef CONFIG_BLK_DEV_TRITON
ide_probe_pci (PCI_VENDOR_ID_INTEL, PCI_DEVICE_ID_INTEL_82371_0, &ide_init_triton, 1);
ide_probe_pci (PCI_VENDOR_ID_INTEL, PCI_DEVICE_ID_INTEL_82371SB_1, &ide_init_triton, 0);
ide_probe_pci (PCI_VENDOR_ID_INTEL, PCI_DEVICE_ID_INTEL_82371AB, &ide_init_triton, 0);
ide_probe_pci (PCI_VENDOR_ID_SI, PCI_DEVICE_ID_SI_5513, &ide_init_triton, 0);
ide_probe_pci (PCI_VENDOR_ID_VIA, PCI_DEVICE_ID_VIA_82C586_1, &ide_init_triton, 0);
ide_probe_pci (PCI_VENDOR_ID_AL, PCI_DEVICE_ID_AL_M5229, &ide_init_triton, 0);
#endif
ide_probe_promise_20246();
}
#endif
#ifdef CONFIG_BLK_DEV_CMD640
{
extern void ide_probe_for_cmd640x (void);
ide_probe_for_cmd640x();
}
#endif
#ifdef CONFIG_BLK_DEV_PROMISE
init_dc4030();
#endif
extern char *kernel_cmdline;
if (strncmp(kernel_cmdline, "noahci", 6) &&
!strstr(kernel_cmdline, " noahci"))
ahci_probe_pci();
}
static int hwif_init (int h)
{
ide_hwif_t *hwif = &ide_hwifs[h];
void (*rfn)(void);
if (!hwif->present)
return 0;
if (!hwif->irq) {
if (!(hwif->irq = default_irqs[h])) {
printk("%s: DISABLED, NO IRQ\n", hwif->name);
return (hwif->present = 0);
}
}
#ifdef CONFIG_BLK_DEV_HD
if (hwif->irq == HD_IRQ && hwif->io_base != HD_DATA) {
printk("%s: CANNOT SHARE IRQ WITH OLD HARDDISK DRIVER (hd.c)\n", hwif->name);
return (hwif->present = 0);
}
#endif
hwif->present = 0;
switch (hwif->major) {
case IDE0_MAJOR: rfn = &do_ide0_request; break;
#if MAX_HWIFS > 1
case IDE1_MAJOR: rfn = &do_ide1_request; break;
#endif
#if MAX_HWIFS > 2
case IDE2_MAJOR: rfn = &do_ide2_request; break;
#endif
#if MAX_HWIFS > 3
case IDE3_MAJOR: rfn = &do_ide3_request; break;
#endif
default:
printk("%s: request_fn NOT DEFINED\n", hwif->name);
return (hwif->present = 0);
}
if (register_blkdev (hwif->major, hwif->name, &ide_fops)) {
printk("%s: UNABLE TO GET MAJOR NUMBER %d\n", hwif->name, hwif->major);
} else if (init_irq (hwif)) {
printk("%s: UNABLE TO GET IRQ %d\n", hwif->name, hwif->irq);
(void) unregister_blkdev (hwif->major, hwif->name);
} else {
init_gendisk(hwif);
blk_dev[hwif->major].request_fn = rfn;
read_ahead[hwif->major] = 8;
hwif->present = 1;
}
return hwif->present;
}
int ide_init (void)
{
int index;
init_ide_data ();
probe_for_hwifs ();
for (index = 0; index < MAX_HWIFS; ++index)
probe_hwif (&ide_hwifs[index]);
for (index = 0; index < MAX_HWIFS; ++index)
hwif_init (index);
#ifdef CONFIG_BLK_DEV_IDETAPE
idetape_register_chrdev();
#endif
return 0;
}
#ifdef CONFIG_BLK_DEV_IDE_PCMCIA
int ide_register(int io_base, int ctl_port, int irq)
{
int index, i, rc = -1;
ide_hwif_t *hwif;
ide_drive_t *drive;
unsigned long flags;
save_flags(flags);
cli();
for (index = 0; index < MAX_HWIFS; ++index) {
hwif = &ide_hwifs[index];
if (hwif->present) {
if (hwif->io_base == io_base || hwif->ctl_port == ctl_port)
break;
} else {
hwif->io_base = io_base;
hwif->ctl_port = ctl_port;
hwif->irq = irq;
hwif->noprobe = 0;
probe_hwif(hwif);
if (!hwif_init(index))
break;
for (i = 0; i < hwif->gd->nr_real; i++) {
drive = &hwif->drives[i];
revalidate_disk(MKDEV(hwif->major, i<<PARTN_BITS));
#ifdef CONFIG_BLK_DEV_IDECD
if (drive->present && drive->media == ide_cdrom)
ide_cdrom_setup(drive);
#endif
}
rc = index;
break;
}
}
restore_flags(flags);
return rc;
}
void ide_unregister (unsigned int index)
{
struct gendisk *gd, **gdp;
ide_hwif_t *hwif, *g;
ide_hwgroup_t *hwgroup;
int irq_count = 0;
unsigned long flags;
if (index >= MAX_HWIFS)
return;
save_flags(flags);
cli();
hwif = &ide_hwifs[index];
if (!hwif->present || hwif->drives[0].busy || hwif->drives[1].busy) {
restore_flags(flags);
return;
}
hwif->present = 0;
hwgroup = hwif->hwgroup;
g = hwgroup->hwif;
do {
if (g->irq == hwif->irq)
++irq_count;
g = g->next;
} while (g != hwgroup->hwif);
if (irq_count == 1)
free_irq(hwif->irq, hwgroup);
release_region(hwif->io_base, 8);
release_region(hwif->ctl_port, 1);
while (hwgroup->hwif->next != hwif)
hwgroup->hwif = hwgroup->hwif->next;
hwgroup->hwif->next = hwif->next;
if (hwgroup->hwif == hwif)
hwgroup->hwif = hwif->next;
if (hwgroup->next_hwif == hwif)
hwgroup->next_hwif = hwif->next;
if (hwgroup->hwif == hwif)
kfree(hwgroup);
unregister_blkdev(hwif->major, hwif->name);
kfree(blksize_size[hwif->major]);
blk_dev[hwif->major].request_fn = NULL;
blksize_size[hwif->major] = NULL;
for (gdp = &gendisk_head; *gdp; gdp = &((*gdp)->next))
if (*gdp == hwif->gd)
break;
if (*gdp == NULL)
printk("gd not in disk chain!\n");
else {
gd = *gdp; *gdp = gd->next;
kfree(gd->sizes);
kfree(gd->part);
kfree(gd);
}
init_hwif_data (index);
restore_flags(flags);
}
#endif