#ifdef MACH
#include <kern/sched_prim.h>
#endif
#include <linux/types.h>
#include <linux/kernel.h>
#include <linux/delay.h>
#include <linux/timer.h>
#include <linux/malloc.h>
#include <linux/ioport.h>
#include <linux/interrupt.h>
#include <linux/blkdev.h>
#include <linux/errno.h>
#include <linux/hdreg.h>
#include <linux/cdrom.h>
#include <linux/ucdrom.h>
#include <asm/irq.h>
#include <asm/io.h>
#include <asm/byteorder.h>
#include <asm/segment.h>
#include <asm/unaligned.h>
#include "ide.h"
#ifndef VERBOSE_IDE_CD_ERRORS
#define VERBOSE_IDE_CD_ERRORS 0
#endif
#ifndef STANDARD_ATAPI
#define STANDARD_ATAPI 0
#endif
#ifndef NO_DOOR_LOCKING
#define NO_DOOR_LOCKING 0
#endif
#ifndef CDROM_NBLOCKS_BUFFER
#define CDROM_NBLOCKS_BUFFER 8
#endif
#define SECTOR_SIZE 512
#define SECTOR_BITS 9
#define SECTORS_PER_FRAME (CD_FRAMESIZE / SECTOR_SIZE)
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define PACKET_COMMAND        4315
#define REQUEST_SENSE_COMMAND 4316
#define RESET_DRIVE_COMMAND   4317
#define TEST_UNIT_READY         0x00
#define REQUEST_SENSE           0x03
#define START_STOP              0x1b
#define ALLOW_MEDIUM_REMOVAL    0x1e
#define READ_CAPACITY		0x25
#define READ_10                 0x28
#define MODE_SENSE_10           0x5a
#define MODE_SELECT_10          0x55
#define READ_CD                 0xbe
#define LOAD_UNLOAD             0xa6
#define NO_SENSE                0x00
#define RECOVERED_ERROR         0x01
#define NOT_READY               0x02
#define MEDIUM_ERROR            0x03
#define HARDWARE_ERROR          0x04
#define ILLEGAL_REQUEST         0x05
#define UNIT_ATTENTION          0x06
#define DATA_PROTECT            0x07
#define ABORTED_COMMAND         0x0b
#define MISCOMPARE              0x0e
struct ide_cd_config_flags {
__u8 drq_interrupt    : 1;
__u8 no_doorlock      : 1;
#if ! STANDARD_ATAPI
__u8 old_readcd       : 1;
__u8 playmsf_as_bcd   : 1;
__u8 tocaddr_as_bcd   : 1;
__u8 toctracks_as_bcd : 1;
__u8 subchan_as_bcd   : 1;
#endif
__u8 reserved         : 1;
};
#define CDROM_CONFIG_FLAGS(drive) ((struct ide_cd_config_flags *)&((drive)->bios_sect))
struct ide_cd_state_flags {
__u8 media_changed : 1;
__u8 toc_valid     : 1;
__u8 door_locked   : 1;
__u8 eject_on_close: 1;
__u8 sanyo_slot    : 2;
__u8 reserved      : 2;
};
#define CDROM_STATE_FLAGS(drive)  ((struct ide_cd_state_flags *)&((drive)->bios_head))
#define SECTOR_BUFFER_SIZE CD_FRAMESIZE
static inline
void cdrom_in_bytes (ide_drive_t *drive, void *buffer, uint bytecount)
{
++bytecount;
ide_input_data (drive, buffer, bytecount / 4);
if ((bytecount & 0x03) >= 2) {
insw (IDE_DATA_REG, ((byte *)buffer) + (bytecount & ~0x03), 1);
}
}
static inline
void cdrom_out_bytes (ide_drive_t *drive, void *buffer, uint bytecount)
{
++bytecount;
ide_output_data (drive, buffer, bytecount / 4);
if ((bytecount & 0x03) >= 2) {
outsw (IDE_DATA_REG,
((byte *)buffer) + (bytecount & ~0x03), 1);
}
}
#define ARY_LEN(a) ((sizeof(a) / sizeof(a[0])))
#if VERBOSE_IDE_CD_ERRORS
char *sense_key_texts[16] = {
"No sense data",
"Recovered error",
"Not ready",
"Medium error",
"Hardware error",
"Illegal request",
"Unit attention",
"Data protect",
"(reserved)",
"(reserved)",
"(reserved)",
"Aborted command",
"(reserved)",
"(reserved)",
"Miscompare",
"(reserved)",
};
struct {
short asc_ascq;
char *text;
} sense_data_texts[] = {
{ 0x0000, "No additional sense information" },
{ 0x0011, "Audio play operation in progress" },
{ 0x0012, "Audio play operation paused" },
{ 0x0013, "Audio play operation successfully completed" },
{ 0x0014, "Audio play operation stopped due to error" },
{ 0x0015, "No current audio status to return" },
{ 0x0200, "No seek complete" },
{ 0x0400, "Logical unit not ready - cause not reportable" },
{ 0x0401,
"Logical unit not ready - in progress (sic) of becoming ready" },
{ 0x0402, "Logical unit not ready - initializing command required" },
{ 0x0403, "Logical unit not ready - manual intervention required" },
{ 0x0600, "No reference position found" },
{ 0x0900, "Track following error" },
{ 0x0901, "Tracking servo failure" },
{ 0x0902, "Focus servo failure" },
{ 0x0903, "Spindle servo failure" },
{ 0x1100, "Unrecovered read error" },
{ 0x1106, "CIRC unrecovered error" },
{ 0x1500, "Random positioning error" },
{ 0x1501, "Mechanical positioning error" },
{ 0x1502, "Positioning error detected by read of medium" },
{ 0x1700, "Recovered data with no error correction applied" },
{ 0x1701, "Recovered data with retries" },
{ 0x1702, "Recovered data with positive head offset" },
{ 0x1703, "Recovered data with negative head offset" },
{ 0x1704, "Recovered data with retries and/or CIRC applied" },
{ 0x1705, "Recovered data using previous sector ID" },
{ 0x1800, "Recovered data with error correction applied" },
{ 0x1801, "Recovered data with error correction and retries applied" },
{ 0x1802, "Recovered data - the data was auto-reallocated" },
{ 0x1803, "Recovered data with CIRC" },
{ 0x1804, "Recovered data with L-EC" },
{ 0x1805, "Recovered data - recommend reassignment" },
{ 0x1806, "Recovered data - recommend rewrite" },
{ 0x1a00, "Parameter list length error" },
{ 0x2000, "Invalid command operation code" },
{ 0x2100, "Logical block address out of range" },
{ 0x2400, "Invalid field in command packet" },
{ 0x2600, "Invalid field in parameter list" },
{ 0x2601, "Parameter not supported" },
{ 0x2602, "Parameter value invalid" },
{ 0x2603, "Threshold parameters not supported" },
{ 0x2800, "Not ready to ready transition, medium may have changed" },
{ 0x2900, "Power on, reset or bus device reset occurred" },
{ 0x2a00, "Parameters changed" },
{ 0x2a01, "Mode parameters changed" },
{ 0x3000, "Incompatible medium installed" },
{ 0x3001, "Cannot read medium - unknown format" },
{ 0x3002, "Cannot read medium - incompatible format" },
{ 0x3700, "Rounded parameter" },
{ 0x3900, "Saving parameters not supported" },
{ 0x3a00, "Medium not present" },
{ 0x3f00, "ATAPI CD-ROM drive operating conditions have changed" },
{ 0x3f01, "Microcode has been changed" },
{ 0x3f02, "Changed operating definition" },
{ 0x3f03, "Inquiry data has changed" },
{ 0x4000, "Diagnostic failure on component (ASCQ)" },
{ 0x4400, "Internal ATAPI CD-ROM drive failure" },
{ 0x4e00, "Overlapped commands attempted" },
{ 0x5300, "Media load or eject failed" },
{ 0x5302, "Medium removal prevented" },
{ 0x5700, "Unable to recover table of contents" },
{ 0x5a00, "Operator request or state change input (unspecified)" },
{ 0x5a01, "Operator medium removal request" },
{ 0x5b00, "Threshold condition met" },
{ 0x5c00, "Status change" },
{ 0x6300, "End of user area encountered on this track" },
{ 0x6400, "Illegal mode for this track" },
{ 0xbf00, "Loss of streaming" },
};
#endif
static
void cdrom_analyze_sense_data (ide_drive_t *drive,
struct atapi_request_sense *reqbuf,
struct packet_command *failed_command)
{
if (failed_command &&
failed_command->c[0] == SCMD_READ_SUBCHANNEL &&
(reqbuf->sense_key == NOT_READY ||
reqbuf->sense_key == UNIT_ATTENTION))
return;
#if VERBOSE_IDE_CD_ERRORS
{
int i;
char *s;
char buf[80];
printk ("ATAPI device %s:\n", drive->name);
printk ("  Error code: 0x%02x\n", reqbuf->error_code);
if (reqbuf->sense_key >= 0 &&
reqbuf->sense_key < ARY_LEN (sense_key_texts))
s = sense_key_texts[reqbuf->sense_key];
else
s = "(bad sense key)";
printk ("  Sense key: 0x%02x - %s\n", reqbuf->sense_key, s);
if (reqbuf->asc == 0x40) {
sprintf (buf, "Diagnostic failure on component 0x%02x",
reqbuf->ascq);
s = buf;
} else {
int lo, hi;
int key = (reqbuf->asc << 8);
if ( ! (reqbuf->ascq >= 0x80 && reqbuf->ascq <= 0xdd) )
key |= reqbuf->ascq;
lo = 0;
hi = ARY_LEN (sense_data_texts);
s = NULL;
while (hi > lo) {
int mid = (lo + hi) / 2;
if (sense_data_texts[mid].asc_ascq == key) {
s = sense_data_texts[mid].text;
break;
}
else if (sense_data_texts[mid].asc_ascq > key)
hi = mid;
else
lo = mid+1;
}
}
if (s == NULL) {
if (reqbuf->asc > 0x80)
s = "(vendor-specific error)";
else
s = "(reserved error code)";
}
printk ("  Additional sense data: 0x%02x, 0x%02x  - %s\n",
reqbuf->asc, reqbuf->ascq, s);
if (failed_command != NULL) {
printk ("  Failed packet command: ");
for (i=0; i<sizeof (failed_command->c); i++)
printk ("%02x ", failed_command->c[i]);
printk ("\n");
}
if (reqbuf->sense_key == ILLEGAL_REQUEST &&
(reqbuf->sense_key_specific[0] & 0x80) != 0) {
printk ("  Error in %s byte %d",
(reqbuf->sense_key_specific[0] & 0x40) != 0
? "command packet"
: "command data",
(reqbuf->sense_key_specific[1] << 8) +
reqbuf->sense_key_specific[2]);
if ((reqbuf->sense_key_specific[0] & 0x40) != 0) {
printk (" bit %d",
reqbuf->sense_key_specific[0] & 0x07);
}
printk ("\n");
}
}
#else
if (reqbuf->sense_key == UNIT_ATTENTION ||
(reqbuf->sense_key == NOT_READY && (reqbuf->asc == 4 ||
reqbuf->asc == 0x3a)))
return;
printk ("%s: code: 0x%02x  key: 0x%02x  asc: 0x%02x  ascq: 0x%02x\n",
drive->name,
reqbuf->error_code, reqbuf->sense_key,
reqbuf->asc, reqbuf->ascq);
#endif
}
static void restore_request (struct request *rq)
{
if (rq->buffer != rq->bh->b_data) {
int n = (rq->buffer - rq->bh->b_data) / SECTOR_SIZE;
rq->buffer = rq->bh->b_data;
rq->nr_sectors += n;
rq->sector -= n;
}
rq->current_nr_sectors = rq->bh->b_size >> SECTOR_BITS;
}
static void cdrom_queue_request_sense (ide_drive_t *drive,
struct semaphore *sem,
struct atapi_request_sense *reqbuf,
struct packet_command *failed_command)
{
struct request *rq;
struct packet_command *pc;
int len;
if (reqbuf == NULL)
reqbuf = &drive->cdrom_info.sense_data;
pc = &HWIF(drive)->request_sense_pc;
memset (pc, 0, sizeof (*pc));
len = sizeof (*reqbuf) / 4;
len *= 4;
pc->c[0] = REQUEST_SENSE;
pc->c[4] = len;
pc->buffer = (unsigned char *)reqbuf;
pc->buflen = len;
pc->sense_data = (struct atapi_request_sense *)failed_command;
rq = &HWIF(drive)->request_sense_request;
ide_init_drive_cmd (rq);
rq->cmd = REQUEST_SENSE_COMMAND;
rq->buffer = (char *)pc;
rq->sem = sem;
(void) ide_do_drive_cmd (drive, rq, ide_preempt);
}
static void cdrom_end_request (int uptodate, ide_drive_t *drive)
{
struct request *rq = HWGROUP(drive)->rq;
if (rq->cmd == REQUEST_SENSE_COMMAND && uptodate && !rq->quiet) {
struct packet_command *pc = (struct packet_command *)
rq->buffer;
cdrom_analyze_sense_data (drive,
(struct atapi_request_sense *)
(pc->buffer - pc->c[4]),
(struct packet_command *)
pc->sense_data);
}
ide_end_request (uptodate, HWGROUP(drive));
}
static void cdrom_saw_media_change (ide_drive_t *drive)
{
CDROM_STATE_FLAGS (drive)->media_changed = 1;
CDROM_STATE_FLAGS (drive)->toc_valid = 0;
drive->cdrom_info.nsectors_buffered = 0;
}
static int cdrom_decode_status (ide_drive_t *drive, int good_stat,
int *stat_ret)
{
struct request *rq = HWGROUP(drive)->rq;
int stat, err, sense_key, cmd;
stat = GET_STAT();
*stat_ret = stat;
if (OK_STAT (stat, good_stat, BAD_R_STAT))
return 0;
err = IN_BYTE (IDE_ERROR_REG);
sense_key = err >> 4;
if (rq == NULL)
printk ("%s : missing request in cdrom_decode_status\n",
drive->name);
else {
cmd = rq->cmd;
if (cmd == REQUEST_SENSE_COMMAND) {
struct packet_command *pc = (struct packet_command *)
rq->buffer;
pc->stat = 1;
cdrom_end_request (1, drive);
ide_error (drive, "request sense failure", stat);
return 1;
} else if (cmd == PACKET_COMMAND) {
struct packet_command *pc = (struct packet_command *)
rq->buffer;
struct semaphore *sem = NULL;
if (sense_key == NOT_READY) {
cdrom_saw_media_change (drive);
if (pc->c[0] != SCMD_READ_SUBCHANNEL && !rq->quiet)
printk ("%s : tray open or drive not ready\n",
drive->name);
} else if (sense_key == UNIT_ATTENTION) {
cdrom_saw_media_change (drive);
if (!rq->quiet)
printk ("%s: media changed\n", drive->name);
} else {
if (!rq->quiet)
ide_dump_status (drive, "packet command error",
stat);
}
if ((stat & ERR_STAT) != 0) {
sem = rq->sem;
rq->sem = NULL;
}
pc->stat = 1;
cdrom_end_request (1, drive);
if ((stat & ERR_STAT) != 0)
cdrom_queue_request_sense (drive, sem,
pc->sense_data, pc);
} else {
if (sense_key == NOT_READY) {
cdrom_saw_media_change (drive);
if (!rq->quiet)
printk ("%s : tray open\n", drive->name);
cdrom_end_request (0, drive);
} else if (sense_key == UNIT_ATTENTION) {
cdrom_saw_media_change (drive);
if (++rq->errors > ERROR_MAX)
cdrom_end_request (0, drive);
} else if (sense_key == ILLEGAL_REQUEST ||
sense_key == DATA_PROTECT) {
if (!rq->quiet)
ide_dump_status (drive, "command error", stat);
cdrom_end_request (0, drive);
} else if ((err & ~ABRT_ERR) != 0) {
ide_error (drive, "cdrom_decode_status", stat);
return 1;
} else if ((++rq->errors > ERROR_MAX)) {
cdrom_end_request (0, drive);
}
if ((stat & ERR_STAT) != 0)
cdrom_queue_request_sense (drive,
NULL, NULL, NULL);
}
}
return 1;
}
static int cdrom_start_packet_command (ide_drive_t *drive, int xferlen,
ide_handler_t *handler)
{
if (ide_wait_stat (drive, 0, BUSY_STAT, WAIT_READY)) return 1;
OUT_BYTE (0, IDE_FEATURE_REG);
OUT_BYTE (0, IDE_NSECTOR_REG);
OUT_BYTE (0, IDE_SECTOR_REG);
OUT_BYTE (xferlen & 0xff, IDE_LCYL_REG);
OUT_BYTE (xferlen >> 8  , IDE_HCYL_REG);
OUT_BYTE (drive->ctl, IDE_CONTROL_REG);
if (CDROM_CONFIG_FLAGS (drive)->drq_interrupt) {
ide_set_handler (drive, handler, WAIT_CMD);
OUT_BYTE (WIN_PACKETCMD, IDE_COMMAND_REG);
} else {
OUT_BYTE (WIN_PACKETCMD, IDE_COMMAND_REG);
(*handler) (drive);
}
return 0;
}
static int cdrom_transfer_packet_command (ide_drive_t *drive,
unsigned char *cmd_buf, int cmd_len,
ide_handler_t *handler)
{
if (CDROM_CONFIG_FLAGS (drive)->drq_interrupt) {
int stat_dum;
if (cdrom_decode_status (drive, DRQ_STAT, &stat_dum)) return 1;
} else {
if (ide_wait_stat (drive, DRQ_STAT, BUSY_STAT, WAIT_READY))
return 1;
}
ide_set_handler (drive, handler, WAIT_CMD);
cdrom_out_bytes (drive, cmd_buf, cmd_len);
return 0;
}
static void cdrom_buffer_sectors (ide_drive_t *drive, unsigned long sector,
int sectors_to_transfer)
{
struct cdrom_info *info = &drive->cdrom_info;
int sectors_to_buffer = MIN (sectors_to_transfer,
(SECTOR_BUFFER_SIZE >> SECTOR_BITS) -
info->nsectors_buffered);
char *dest;
if (info->sector_buffer == NULL) {
info->sector_buffer = (char *) kmalloc (SECTOR_BUFFER_SIZE,
GFP_ATOMIC);
if (info->sector_buffer == NULL)
sectors_to_buffer = 0;
}
if (info->nsectors_buffered == 0)
info->sector_buffered = sector;
dest = info->sector_buffer + info->nsectors_buffered * SECTOR_SIZE;
while (sectors_to_buffer > 0) {
cdrom_in_bytes (drive, dest, SECTOR_SIZE);
--sectors_to_buffer;
--sectors_to_transfer;
++info->nsectors_buffered;
dest += SECTOR_SIZE;
}
while (sectors_to_transfer > 0) {
char dum[SECTOR_SIZE];
cdrom_in_bytes (drive, dum, sizeof (dum));
--sectors_to_transfer;
}
}
static inline
int cdrom_read_check_ireason (ide_drive_t *drive, int len, int ireason)
{
ireason &= 3;
if (ireason == 2) return 0;
if (ireason == 0) {
printk ("%s: cdrom_read_intr: "
"Drive wants to transfer data the wrong way!\n",
drive->name);
while (len > 0) {
int dum = 0;
cdrom_out_bytes (drive, &dum, sizeof (dum));
len -= sizeof (dum);
}
} else {
printk ("%s: cdrom_read_intr: bad interrupt reason %d\n",
drive->name, ireason);
}
cdrom_end_request (0, drive);
return -1;
}
static void cdrom_read_intr (ide_drive_t *drive)
{
int stat;
int ireason, len, sectors_to_transfer, nskip;
struct request *rq = HWGROUP(drive)->rq;
if (cdrom_decode_status (drive, 0, &stat)) return;
ireason = IN_BYTE (IDE_NSECTOR_REG);
len = IN_BYTE (IDE_LCYL_REG) + 256 * IN_BYTE (IDE_HCYL_REG);
if ((stat & DRQ_STAT) == 0) {
if (rq->current_nr_sectors > 0) {
printk ("%s: cdrom_read_intr: data underrun (%ld blocks)\n",
drive->name, rq->current_nr_sectors);
cdrom_end_request (0, drive);
} else
cdrom_end_request (1, drive);
return;
}
if (cdrom_read_check_ireason (drive, len, ireason)) return;
if ((len % SECTOR_SIZE) != 0) {
printk ("%s: cdrom_read_intr: Bad transfer size %d\n",
drive->name, len);
printk ("  This drive is not supported by this version of the driver\n");
cdrom_end_request (0, drive);
return;
}
sectors_to_transfer = len / SECTOR_SIZE;
nskip = MIN ((int)(rq->current_nr_sectors -
(rq->bh->b_size >> SECTOR_BITS)),
sectors_to_transfer);
while (nskip > 0) {
char dum[SECTOR_SIZE];
cdrom_in_bytes (drive, dum, sizeof (dum));
--rq->current_nr_sectors;
--nskip;
--sectors_to_transfer;
}
while (sectors_to_transfer > 0) {
int this_transfer;
if (rq->current_nr_sectors == 0 &&
rq->nr_sectors > 0)
cdrom_end_request (1, drive);
if (rq->current_nr_sectors == 0) {
cdrom_buffer_sectors (drive,
rq->sector, sectors_to_transfer);
sectors_to_transfer = 0;
} else {
this_transfer = MIN (sectors_to_transfer,
rq->current_nr_sectors);
while (this_transfer > 0) {
cdrom_in_bytes (drive
, rq->buffer, SECTOR_SIZE);
rq->buffer += SECTOR_SIZE;
--rq->nr_sectors;
--rq->current_nr_sectors;
++rq->sector;
--this_transfer;
--sectors_to_transfer;
}
}
}
ide_set_handler (drive, &cdrom_read_intr, WAIT_CMD);
}
static int cdrom_read_from_buffer (ide_drive_t *drive)
{
struct cdrom_info *info = &drive->cdrom_info;
struct request *rq = HWGROUP(drive)->rq;
if (info->sector_buffer == NULL) return 0;
while (rq->nr_sectors > 0 &&
rq->sector >= info->sector_buffered &&
rq->sector < info->sector_buffered + info->nsectors_buffered) {
if (rq->current_nr_sectors == 0)
cdrom_end_request (1, drive);
memcpy (rq->buffer,
info->sector_buffer +
(rq->sector - info->sector_buffered) * SECTOR_SIZE,
SECTOR_SIZE);
rq->buffer += SECTOR_SIZE;
--rq->current_nr_sectors;
--rq->nr_sectors;
++rq->sector;
}
if (rq->nr_sectors == 0) {
cdrom_end_request (1, drive);
return -1;
}
if (rq->current_nr_sectors == 0)
cdrom_end_request (1, drive);
if (rq->current_nr_sectors < (rq->bh->b_size >> SECTOR_BITS) &&
(rq->sector % SECTORS_PER_FRAME) != 0) {
printk ("%s: cdrom_read_from_buffer: buffer botch (%ld)\n",
drive->name, rq->sector);
cdrom_end_request (0, drive);
return -1;
}
return 0;
}
static void cdrom_start_read_continuation (ide_drive_t *drive)
{
struct packet_command pc;
struct request *rq = HWGROUP(drive)->rq;
int nsect, sector, nframes, frame, nskip;
nsect = rq->nr_sectors;
#if !STANDARD_ATAPI
if (nsect > drive->cdrom_info.max_sectors)
nsect = drive->cdrom_info.max_sectors;
#endif
sector = rq->sector;
nskip = (sector % SECTORS_PER_FRAME);
if (nskip > 0) {
if (rq->current_nr_sectors !=
(rq->bh->b_size >> SECTOR_BITS)) {
printk ("%s: cdrom_start_read_continuation: buffer botch (%ld)\n",
drive->name, rq->current_nr_sectors);
cdrom_end_request (0, drive);
return;
}
sector -= nskip;
nsect += nskip;
rq->current_nr_sectors += nskip;
}
nframes = (nsect + SECTORS_PER_FRAME-1) / SECTORS_PER_FRAME;
frame = sector / SECTORS_PER_FRAME;
nframes = MIN (nframes, 65535);
memset (&pc.c, 0, sizeof (pc.c));
pc.c[0] = READ_10;
pc.c[7] = (nframes >> 8);
pc.c[8] = (nframes & 0xff);
put_unaligned(htonl (frame), (unsigned int *) &pc.c[2]);
(void) cdrom_transfer_packet_command (drive, pc.c, sizeof (pc.c),
&cdrom_read_intr);
}
static void cdrom_start_read (ide_drive_t *drive, unsigned int block)
{
struct request *rq = HWGROUP(drive)->rq;
int minor = MINOR (rq->rq_dev);
if ((minor & PARTN_MASK) != 0) {
rq->sector = block;
minor &= ~PARTN_MASK;
rq->rq_dev = MKDEV (MAJOR(rq->rq_dev), minor);
}
restore_request (rq);
if (cdrom_read_from_buffer (drive))
return;
drive->cdrom_info.nsectors_buffered = 0;
cdrom_start_packet_command (drive, 32768,
cdrom_start_read_continuation);
}
static int
cdrom_lockdoor (ide_drive_t *drive, int lockflag,
struct atapi_request_sense *reqbuf);
static void cdrom_pc_intr (ide_drive_t *drive)
{
int ireason, len, stat, thislen;
struct request *rq = HWGROUP(drive)->rq;
struct packet_command *pc = (struct packet_command *)rq->buffer;
if (cdrom_decode_status (drive, 0, &stat)) return;
ireason = IN_BYTE (IDE_NSECTOR_REG);
len = IN_BYTE (IDE_LCYL_REG) + 256 * IN_BYTE (IDE_HCYL_REG);
if ((stat & DRQ_STAT) == 0) {
if (pc->c[0] == REQUEST_SENSE &&
pc->buflen > 0 &&
pc->buflen <= 5) {
while (pc->buflen > 0) {
*pc->buffer++ = 0;
--pc->buflen;
}
}
if (pc->buflen == 0)
cdrom_end_request (1, drive);
else {
printk ("%s: cdrom_pc_intr: data underrun %d\n",
drive->name, pc->buflen);
pc->stat = 1;
cdrom_end_request (1, drive);
}
return;
}
thislen = pc->buflen;
if (thislen < 0) thislen = -thislen;
if (thislen > len) thislen = len;
if ((ireason & 3) == 0) {
if (pc->buflen > 0) {
printk ("%s: cdrom_pc_intr: Drive wants "
"to transfer data the wrong way!\n",
drive->name);
pc->stat = 1;
thislen = 0;
}
cdrom_out_bytes (drive, pc->buffer, thislen);
while (len > thislen) {
int dum = 0;
cdrom_out_bytes (drive, &dum, sizeof (dum));
len -= sizeof (dum);
}
pc->buffer += thislen;
pc->buflen += thislen;
}
else if ((ireason & 3) == 2) {
if (pc->buflen < 0) {
printk ("%s: cdrom_pc_intr: Drive wants to "
"transfer data the wrong way!\n",
drive->name);
pc->stat = 1;
thislen = 0;
}
cdrom_in_bytes (drive, pc->buffer, thislen);
while (len > thislen) {
int dum = 0;
cdrom_in_bytes (drive, &dum, sizeof (dum));
len -= sizeof (dum);
}
pc->buffer += thislen;
pc->buflen -= thislen;
} else {
printk ("%s: cdrom_pc_intr: The drive "
"appears confused (ireason = 0x%2x)\n",
drive->name, ireason);
pc->stat = 1;
}
ide_set_handler (drive, &cdrom_pc_intr, WAIT_CMD);
}
static void cdrom_do_pc_continuation (ide_drive_t *drive)
{
struct request *rq = HWGROUP(drive)->rq;
struct packet_command *pc = (struct packet_command *)rq->buffer;
cdrom_transfer_packet_command (drive, pc->c,
sizeof (pc->c), &cdrom_pc_intr);
}
static void cdrom_do_packet_command (ide_drive_t *drive)
{
int len;
struct request *rq = HWGROUP(drive)->rq;
struct packet_command *pc = (struct packet_command *)rq->buffer;
len = pc->buflen;
if (len < 0) len = -len;
pc->stat = 0;
cdrom_start_packet_command (drive, len, cdrom_do_pc_continuation);
}
#ifdef MACH
static
void cdrom_sleep (int time)
{
int xxx;
assert_wait ((event_t) &xxx, TRUE);
thread_set_timeout (time);
schedule ();
}
#else
static
void cdrom_sleep (int time)
{
current->state = TASK_INTERRUPTIBLE;
current->timeout = jiffies + time;
schedule ();
}
#endif
static
int cdrom_queue_packet_command (ide_drive_t *drive, struct packet_command *pc, int quiet)
{
struct atapi_request_sense my_reqbuf;
int retries = 10;
struct request req;
if (pc->sense_data == NULL)
pc->sense_data = &my_reqbuf;
pc->sense_data->sense_key = 0;
do {
ide_init_drive_cmd (&req);
req.cmd = PACKET_COMMAND;
req.buffer = (char *)pc;
req.quiet = quiet;
(void) ide_do_drive_cmd (drive, &req, ide_wait);
if (pc->stat != 0) {
struct atapi_request_sense *reqbuf = pc->sense_data;
if (reqbuf->sense_key == UNIT_ATTENTION)
;
else if (reqbuf->sense_key == NOT_READY &&
reqbuf->asc == 4) {
cdrom_sleep (HZ);
} else
retries = 0;
--retries;
}
} while (pc->stat != 0 && retries >= 0);
if (pc->stat != 0)
return -EIO;
else {
if (CDROM_STATE_FLAGS (drive)->door_locked == 0 &&
(pc->c[0] != REQUEST_SENSE &&
pc->c[0] != ALLOW_MEDIUM_REMOVAL &&
pc->c[0] != START_STOP)) {
(void) cdrom_lockdoor (drive, 1, NULL);
}
return 0;
}
}
void ide_do_rw_cdrom (ide_drive_t *drive, unsigned long block)
{
struct request *rq = HWGROUP(drive)->rq;
if (rq -> cmd == PACKET_COMMAND || rq -> cmd == REQUEST_SENSE_COMMAND)
cdrom_do_packet_command (drive);
else if (rq -> cmd == RESET_DRIVE_COMMAND) {
cdrom_end_request (1, drive);
ide_do_reset (drive);
return;
} else if (rq -> cmd != READ) {
printk ("ide-cd: bad cmd %d\n", rq -> cmd);
cdrom_end_request (0, drive);
} else
cdrom_start_read (drive, block);
}
#if ! STANDARD_ATAPI
static inline
int bin2bcd (int x)
{
return (x%10) | ((x/10) << 4);
}
static inline
int bcd2bin (int x)
{
return (x >> 4) * 10 + (x & 0x0f);
}
static
void msf_from_bcd (struct atapi_msf *msf)
{
msf->minute = bcd2bin (msf->minute);
msf->second = bcd2bin (msf->second);
msf->frame  = bcd2bin (msf->frame);
}
#endif
static inline
void lba_to_msf (int lba, byte *m, byte *s, byte *f)
{
lba += CD_BLOCK_OFFSET;
lba &= 0xffffff;
*m = lba / (CD_SECS * CD_FRAMES);
lba %= (CD_SECS * CD_FRAMES);
*s = lba / CD_FRAMES;
*f = lba % CD_FRAMES;
}
static inline
int msf_to_lba (byte m, byte s, byte f)
{
return (((m * CD_SECS) + s) * CD_FRAMES + f) - CD_BLOCK_OFFSET;
}
static int
cdrom_check_status (ide_drive_t  *drive,
struct atapi_request_sense *reqbuf)
{
struct packet_command pc;
memset (&pc, 0, sizeof (pc));
pc.sense_data = reqbuf;
pc.c[0] = TEST_UNIT_READY;
pc.c[7] = CDROM_STATE_FLAGS (drive)->sanyo_slot % 3;
return cdrom_queue_packet_command (drive, &pc, 1);
}
static int
cdrom_lockdoor (ide_drive_t *drive, int lockflag,
struct atapi_request_sense *reqbuf)
{
struct atapi_request_sense my_reqbuf;
int stat;
struct packet_command pc;
if (reqbuf == NULL)
reqbuf = &my_reqbuf;
if (CDROM_CONFIG_FLAGS (drive)->no_doorlock)
stat = 0;
else {
memset (&pc, 0, sizeof (pc));
pc.sense_data = reqbuf;
pc.c[0] = ALLOW_MEDIUM_REMOVAL;
pc.c[4] = (lockflag != 0);
stat = cdrom_queue_packet_command (drive, &pc, 0);
}
if (stat == 0)
CDROM_STATE_FLAGS (drive)->door_locked = lockflag;
else {
if (reqbuf->sense_key == ILLEGAL_REQUEST &&
(reqbuf->asc == 0x24 || reqbuf->asc == 0x20)) {
printk ("%s: door locking not supported\n",
drive->name);
CDROM_CONFIG_FLAGS (drive)->no_doorlock = 1;
stat = 0;
CDROM_STATE_FLAGS (drive)->door_locked = lockflag;
}
}
return stat;
}
static int
cdrom_eject (ide_drive_t *drive, int ejectflag,
struct atapi_request_sense *reqbuf)
{
struct packet_command pc;
memset (&pc, 0, sizeof (pc));
pc.sense_data = reqbuf;
pc.c[0] = START_STOP;
pc.c[4] = 2 + (ejectflag != 0);
return cdrom_queue_packet_command (drive, &pc, 0);
}
static int
cdrom_pause (ide_drive_t *drive, int pauseflag,
struct atapi_request_sense *reqbuf)
{
struct packet_command pc;
memset (&pc, 0, sizeof (pc));
pc.sense_data = reqbuf;
pc.c[0] = SCMD_PAUSE_RESUME;
pc.c[8] = !pauseflag;
return cdrom_queue_packet_command (drive, &pc, 0);
}
static int
cdrom_startstop (ide_drive_t *drive, int startflag,
struct atapi_request_sense *reqbuf)
{
struct packet_command pc;
memset (&pc, 0, sizeof (pc));
pc.sense_data = reqbuf;
pc.c[0] = START_STOP;
pc.c[1] = 1;
pc.c[4] = startflag;
return cdrom_queue_packet_command (drive, &pc, 0);
}
static int
cdrom_read_capacity (ide_drive_t *drive, unsigned *capacity,
struct atapi_request_sense *reqbuf)
{
struct {
unsigned lba;
unsigned blocklen;
} capbuf;
int stat;
struct packet_command pc;
memset (&pc, 0, sizeof (pc));
pc.sense_data = reqbuf;
pc.c[0] = READ_CAPACITY;
pc.buffer = (unsigned char *)&capbuf;
pc.buflen = sizeof (capbuf);
stat = cdrom_queue_packet_command (drive, &pc, 1);
if (stat == 0)
*capacity = ntohl (capbuf.lba);
return stat;
}
static int
cdrom_read_tocentry (ide_drive_t *drive, int trackno, int msf_flag,
int format, char *buf, int buflen,
struct atapi_request_sense *reqbuf)
{
struct packet_command pc;
memset (&pc, 0, sizeof (pc));
pc.sense_data = reqbuf;
pc.buffer = (unsigned char *)buf;
pc.buflen = buflen;
pc.c[0] = SCMD_READ_TOC;
pc.c[6] = trackno;
pc.c[7] = (buflen >> 8);
pc.c[8] = (buflen & 0xff);
pc.c[9] = (format << 6);
if (msf_flag) pc.c[1] = 2;
return cdrom_queue_packet_command (drive, &pc, 1);
}
static int
cdrom_read_toc (ide_drive_t *drive,
struct atapi_request_sense *reqbuf)
{
int stat, ntracks, i;
struct atapi_toc *toc = drive->cdrom_info.toc;
struct {
struct atapi_toc_header hdr;
struct atapi_toc_entry  ent;
} ms_tmp;
if (toc == NULL) {
toc = (struct atapi_toc *) kmalloc (sizeof (struct atapi_toc),
GFP_KERNEL);
drive->cdrom_info.toc = toc;
}
if (toc == NULL) {
printk ("%s: No cdrom TOC buffer!\n", drive->name);
return -EIO;
}
if (CDROM_STATE_FLAGS (drive)->toc_valid)
(void) cdrom_check_status (drive, NULL);
if (CDROM_STATE_FLAGS (drive)->toc_valid) return 0;
stat = cdrom_read_tocentry (drive, 0, 1, 0, (char *)&toc->hdr,
sizeof (struct atapi_toc_header) +
sizeof (struct atapi_toc_entry),
reqbuf);
if (stat) return stat;
#if ! STANDARD_ATAPI
if (CDROM_CONFIG_FLAGS (drive)->toctracks_as_bcd) {
toc->hdr.first_track = bcd2bin (toc->hdr.first_track);
toc->hdr.last_track  = bcd2bin (toc->hdr.last_track);
}
#endif
ntracks = toc->hdr.last_track - toc->hdr.first_track + 1;
if (ntracks <= 0) return -EIO;
if (ntracks > MAX_TRACKS) ntracks = MAX_TRACKS;
stat = cdrom_read_tocentry (drive, 0, 1, 0, (char *)&toc->hdr,
sizeof (struct atapi_toc_header) +
(ntracks+1) *
sizeof (struct atapi_toc_entry),
reqbuf);
if (stat) return stat;
toc->hdr.toc_length = ntohs (toc->hdr.toc_length);
#if ! STANDARD_ATAPI
if (CDROM_CONFIG_FLAGS (drive)->toctracks_as_bcd) {
toc->hdr.first_track = bcd2bin (toc->hdr.first_track);
toc->hdr.last_track  = bcd2bin (toc->hdr.last_track);
}
#endif
for (i=0; i<=ntracks; i++) {
#if ! STANDARD_ATAPI
if (CDROM_CONFIG_FLAGS (drive)->tocaddr_as_bcd) {
if (CDROM_CONFIG_FLAGS (drive)->toctracks_as_bcd)
toc->ent[i].track = bcd2bin (toc->ent[i].track);
msf_from_bcd (&toc->ent[i].addr.msf);
}
#endif
toc->ent[i].addr.lba = msf_to_lba (toc->ent[i].addr.msf.minute,
toc->ent[i].addr.msf.second,
toc->ent[i].addr.msf.frame);
}
stat = cdrom_read_tocentry (drive, 0, 1, 1,
(char *)&ms_tmp, sizeof (ms_tmp),
reqbuf);
if (stat) return stat;
#if ! STANDARD_ATAPI
if (CDROM_CONFIG_FLAGS (drive)->tocaddr_as_bcd)
msf_from_bcd (&ms_tmp.ent.addr.msf);
#endif
toc->last_session_lba = msf_to_lba (ms_tmp.ent.addr.msf.minute,
ms_tmp.ent.addr.msf.second,
ms_tmp.ent.addr.msf.frame);
toc->xa_flag = (ms_tmp.hdr.first_track != ms_tmp.hdr.last_track);
stat = cdrom_read_capacity (drive, &toc->capacity, reqbuf);
if (stat) toc->capacity = 0x1fffff;
HWIF(drive)->gd->sizes[drive->select.b.unit << PARTN_BITS]
= toc->capacity * SECTORS_PER_FRAME;
drive->part[0].nr_sects = toc->capacity * SECTORS_PER_FRAME;
CDROM_STATE_FLAGS (drive)->toc_valid = 1;
return 0;
}
static int
cdrom_read_subchannel (ide_drive_t *drive, int format,
char *buf, int buflen,
struct atapi_request_sense *reqbuf)
{
struct packet_command pc;
memset (&pc, 0, sizeof (pc));
pc.sense_data = reqbuf;
pc.buffer = (unsigned char *) buf;
pc.buflen = buflen;
pc.c[0] = SCMD_READ_SUBCHANNEL;
pc.c[1] = 2;
pc.c[2] = 0x40;
pc.c[3] = format,
pc.c[7] = (buflen >> 8);
pc.c[8] = (buflen & 0xff);
return cdrom_queue_packet_command (drive, &pc, 0);
}
static int
cdrom_mode_sense (ide_drive_t *drive, int pageno, int modeflag,
char *buf, int buflen,
struct atapi_request_sense *reqbuf)
{
struct packet_command pc;
memset (&pc, 0, sizeof (pc));
pc.sense_data = reqbuf;
pc.buffer = (unsigned char *)buf;
pc.buflen = buflen;
pc.c[0] = MODE_SENSE_10;
pc.c[2] = pageno | (modeflag << 6);
pc.c[7] = (buflen >> 8);
pc.c[8] = (buflen & 0xff);
return cdrom_queue_packet_command (drive, &pc, 0);
}
static int
cdrom_mode_select (ide_drive_t *drive, int pageno, char *buf, int buflen,
struct atapi_request_sense *reqbuf)
{
struct packet_command pc;
memset (&pc, 0, sizeof (pc));
pc.sense_data = reqbuf;
pc.buffer = (unsigned char *)buf;
pc.buflen = - buflen;
pc.c[0] = MODE_SELECT_10;
pc.c[1] = 0x10;
pc.c[2] = pageno;
pc.c[7] = (buflen >> 8);
pc.c[8] = (buflen & 0xff);
return cdrom_queue_packet_command (drive, &pc, 0);
}
static int
cdrom_play_lba_range_1 (ide_drive_t *drive, int lba_start, int lba_end,
struct atapi_request_sense *reqbuf)
{
struct packet_command pc;
memset (&pc, 0, sizeof (pc));
pc.sense_data = reqbuf;
pc.c[0] = SCMD_PLAYAUDIO_MSF;
lba_to_msf (lba_start, &pc.c[3], &pc.c[4], &pc.c[5]);
lba_to_msf (lba_end-1, &pc.c[6], &pc.c[7], &pc.c[8]);
#if ! STANDARD_ATAPI
if (CDROM_CONFIG_FLAGS (drive)->playmsf_as_bcd) {
pc.c[3] = bin2bcd (pc.c[3]);
pc.c[4] = bin2bcd (pc.c[4]);
pc.c[5] = bin2bcd (pc.c[5]);
pc.c[6] = bin2bcd (pc.c[6]);
pc.c[7] = bin2bcd (pc.c[7]);
pc.c[8] = bin2bcd (pc.c[8]);
}
#endif
return cdrom_queue_packet_command (drive, &pc, 0);
}
static int
cdrom_play_lba_range (ide_drive_t *drive, int lba_start, int lba_end,
struct atapi_request_sense *reqbuf)
{
int i, stat;
struct atapi_request_sense my_reqbuf;
if (reqbuf == NULL)
reqbuf = &my_reqbuf;
for (i=0; i<75; i++) {
stat = cdrom_play_lba_range_1 (drive, lba_start, lba_end,
reqbuf);
if (stat == 0 ||
!(reqbuf->sense_key == ILLEGAL_REQUEST &&
reqbuf->asc == 0x24))
return stat;
--lba_end;
if (lba_end <= lba_start) break;
}
return stat;
}
static
int cdrom_get_toc_entry (ide_drive_t *drive, int track,
struct atapi_toc_entry **ent,
struct atapi_request_sense *reqbuf)
{
int stat, ntracks;
struct atapi_toc *toc;
stat = cdrom_read_toc (drive, reqbuf);
if (stat) return stat;
toc = drive->cdrom_info.toc;
ntracks = toc->hdr.last_track - toc->hdr.first_track + 1;
if (track == CDROM_LEADOUT)
*ent = &toc->ent[ntracks];
else if (track < toc->hdr.first_track ||
track > toc->hdr.last_track)
return -EINVAL;
else
*ent = &toc->ent[track - toc->hdr.first_track];
return 0;
}
static int
cdrom_read_block (ide_drive_t *drive, int format, int lba, int nblocks,
char *buf, int buflen,
struct atapi_request_sense *reqbuf)
{
struct packet_command pc;
struct atapi_request_sense my_reqbuf;
int stat;
if (reqbuf == NULL)
reqbuf = &my_reqbuf;
memset (&pc, 0, sizeof (pc));
pc.sense_data = reqbuf;
pc.buffer = (unsigned char *)buf;
pc.buflen = buflen;
#if ! STANDARD_ATAPI
if (CDROM_CONFIG_FLAGS (drive)->old_readcd)
pc.c[0] = 0xd4;
else
#endif
pc.c[0] = READ_CD;
pc.c[1] = (format << 2);
put_unaligned(htonl(lba), (unsigned int *) &pc.c[2]);
pc.c[8] = (nblocks & 0xff);
pc.c[7] = ((nblocks>>8) & 0xff);
pc.c[6] = ((nblocks>>16) & 0xff);
if (format <= 1)
pc.c[9] = 0xf8;
else
pc.c[9] = 0x10;
stat = cdrom_queue_packet_command (drive, &pc, 0);
#if ! STANDARD_ATAPI
if (stat && reqbuf->sense_key == ILLEGAL_REQUEST &&
reqbuf->asc == 0x20 &&
CDROM_CONFIG_FLAGS (drive)->old_readcd == 0) {
printk ("%s: Drive does not recognize READ_CD;"
"trying opcode 0xd4\n",
drive->name);
CDROM_CONFIG_FLAGS (drive)->old_readcd = 1;
return cdrom_read_block (drive, format, lba, nblocks,
buf, buflen, reqbuf);
}
#endif
return stat;
}
static int
cdrom_load_unload (ide_drive_t *drive, int slot,
struct atapi_request_sense *reqbuf)
{
if (CDROM_STATE_FLAGS (drive)->sanyo_slot > 0) {
if ((slot == 1) || (slot == 2)) {
CDROM_STATE_FLAGS (drive)->sanyo_slot = slot;
} else if (slot >= 0) {
CDROM_STATE_FLAGS (drive)->sanyo_slot = 3;
} else {
return 0;
}
return cdrom_check_status (drive, NULL);
} else {
struct packet_command pc;
memset (&pc, 0, sizeof (pc));
pc.sense_data = reqbuf;
pc.c[0] = LOAD_UNLOAD;
pc.c[4] = 2 + (slot >= 0);
pc.c[8] = slot;
return cdrom_queue_packet_command (drive, &pc, 0);
}
}
int ide_cdrom_ioctl (ide_drive_t *drive, struct inode *inode,
struct file *file, unsigned int cmd, unsigned long arg)
{
switch (cmd) {
case CDROMEJECT: {
int stat;
if (drive->usage > 1)
return -EBUSY;
stat = cdrom_lockdoor (drive, 0, NULL);
if (stat) return stat;
return cdrom_eject (drive, 0, NULL);
}
case CDROMCLOSETRAY: {
int stat;
if (drive->usage > 1)
return -EBUSY;
stat = cdrom_eject (drive, 1, NULL);
if (stat) return stat;
return cdrom_lockdoor (drive, 1, NULL);
}
case CDROMEJECT_SW: {
CDROM_STATE_FLAGS (drive)->eject_on_close = arg;
return 0;
}
case CDROMPAUSE:
return cdrom_pause (drive, 1, NULL);
case CDROMRESUME:
return cdrom_pause (drive, 0, NULL);
case CDROMSTART:
return cdrom_startstop (drive, 1, NULL);
case CDROMSTOP: {
#ifdef IHAVEADOLPHIN
int stat;
stat = cdrom_startstop (drive, 0, NULL);
if (stat) return stat;
return cdrom_eject (drive, 1, NULL);
#endif
return cdrom_startstop (drive, 0, NULL);
}
case CDROMPLAYMSF: {
struct cdrom_msf msf;
int stat, lba_start, lba_end;
stat = verify_area (VERIFY_READ, (void *)arg, sizeof (msf));
if (stat) return stat;
memcpy_fromfs (&msf, (void *) arg, sizeof(msf));
lba_start = msf_to_lba (msf.cdmsf_min0, msf.cdmsf_sec0,
msf.cdmsf_frame0);
lba_end = msf_to_lba (msf.cdmsf_min1, msf.cdmsf_sec1,
msf.cdmsf_frame1) + 1;
if (lba_end <= lba_start) return -EINVAL;
return cdrom_play_lba_range (drive, lba_start, lba_end, NULL);
}
case CDROMPLAYTRKIND: {
int stat, lba_start, lba_end;
struct cdrom_ti ti;
struct atapi_toc_entry *first_toc, *last_toc;
stat = verify_area (VERIFY_READ, (void *)arg, sizeof (ti));
if (stat) return stat;
memcpy_fromfs (&ti, (void *) arg, sizeof(ti));
stat = cdrom_get_toc_entry (drive, ti.cdti_trk0, &first_toc,
NULL);
if (stat) return stat;
stat = cdrom_get_toc_entry (drive, ti.cdti_trk1, &last_toc,
NULL);
if (stat) return stat;
if (ti.cdti_trk1 != CDROM_LEADOUT) ++last_toc;
lba_start = first_toc->addr.lba;
lba_end   = last_toc->addr.lba;
if (lba_end <= lba_start) return -EINVAL;
return cdrom_play_lba_range (drive, lba_start, lba_end, NULL);
}
case CDROMREADTOCHDR: {
int stat;
struct cdrom_tochdr tochdr;
struct atapi_toc *toc;
stat = verify_area (VERIFY_WRITE, (void *) arg,
sizeof (tochdr));
if (stat) return stat;
stat = cdrom_read_toc (drive, NULL);
if (stat) return stat;
toc = drive->cdrom_info.toc;
tochdr.cdth_trk0 = toc->hdr.first_track;
tochdr.cdth_trk1 = toc->hdr.last_track;
memcpy_tofs ((void *) arg, &tochdr, sizeof (tochdr));
return stat;
}
case CDROMREADTOCENTRY: {
int stat;
struct cdrom_tocentry tocentry;
struct atapi_toc_entry *toce;
stat = verify_area (VERIFY_WRITE, (void *) arg,
sizeof (tocentry));
if (stat) return stat;
memcpy_fromfs (&tocentry, (void *) arg, sizeof (tocentry));
stat = cdrom_get_toc_entry (drive, tocentry.cdte_track, &toce,
NULL);
if (stat) return stat;
tocentry.cdte_ctrl = toce->control;
tocentry.cdte_adr  = toce->adr;
if (tocentry.cdte_format == CDROM_MSF) {
lba_to_msf (toce->addr.lba,
&tocentry.cdte_addr.msf.minute,
&tocentry.cdte_addr.msf.second,
&tocentry.cdte_addr.msf.frame);
} else
tocentry.cdte_addr.lba = toce->addr.lba;
memcpy_tofs ((void *) arg, &tocentry, sizeof (tocentry));
return stat;
}
case CDROMSUBCHNL: {
struct atapi_cdrom_subchnl scbuf;
int stat;
struct cdrom_subchnl subchnl;
stat = verify_area (VERIFY_WRITE, (void *) arg,
sizeof (subchnl));
if (stat) return stat;
memcpy_fromfs (&subchnl, (void *) arg, sizeof (subchnl));
stat = cdrom_read_subchannel (drive, 1,
(char *)&scbuf, sizeof (scbuf),
NULL);
if (stat) return stat;
#if ! STANDARD_ATAPI
if (CDROM_CONFIG_FLAGS (drive)->subchan_as_bcd) {
msf_from_bcd (&scbuf.acdsc_absaddr.msf);
msf_from_bcd (&scbuf.acdsc_reladdr.msf);
}
if (CDROM_CONFIG_FLAGS (drive)->tocaddr_as_bcd)
scbuf.acdsc_trk = bcd2bin (scbuf.acdsc_trk);
#endif
if (subchnl.cdsc_format == CDROM_MSF) {
subchnl.cdsc_absaddr.msf.minute =
scbuf.acdsc_absaddr.msf.minute;
subchnl.cdsc_absaddr.msf.second =
scbuf.acdsc_absaddr.msf.second;
subchnl.cdsc_absaddr.msf.frame =
scbuf.acdsc_absaddr.msf.frame;
subchnl.cdsc_reladdr.msf.minute =
scbuf.acdsc_reladdr.msf.minute;
subchnl.cdsc_reladdr.msf.second =
scbuf.acdsc_reladdr.msf.second;
subchnl.cdsc_reladdr.msf.frame =
scbuf.acdsc_reladdr.msf.frame;
} else {
subchnl.cdsc_absaddr.lba =
msf_to_lba (scbuf.acdsc_absaddr.msf.minute,
scbuf.acdsc_absaddr.msf.second,
scbuf.acdsc_absaddr.msf.frame);
subchnl.cdsc_reladdr.lba =
msf_to_lba (scbuf.acdsc_reladdr.msf.minute,
scbuf.acdsc_reladdr.msf.second,
scbuf.acdsc_reladdr.msf.frame);
}
subchnl.cdsc_audiostatus = scbuf.acdsc_audiostatus;
subchnl.cdsc_ctrl = scbuf.acdsc_ctrl;
subchnl.cdsc_trk  = scbuf.acdsc_trk;
subchnl.cdsc_ind  = scbuf.acdsc_ind;
memcpy_tofs ((void *) arg, &subchnl, sizeof (subchnl));
return stat;
}
case CDROMVOLCTRL: {
struct cdrom_volctrl volctrl;
char buffer[24], mask[24];
int stat;
stat = verify_area (VERIFY_READ, (void *) arg,
sizeof (volctrl));
if (stat) return stat;
memcpy_fromfs (&volctrl, (void *) arg, sizeof (volctrl));
stat = cdrom_mode_sense (drive, 0x0e, 0, buffer,
sizeof (buffer), NULL);
if (stat) return stat;
stat = cdrom_mode_sense (drive, 0x0e, 1, mask,
sizeof (buffer), NULL);
if (stat) return stat;
buffer[1] = buffer[2] = 0;
buffer[17] = volctrl.channel0 & mask[17];
buffer[19] = volctrl.channel1 & mask[19];
buffer[21] = volctrl.channel2 & mask[21];
buffer[23] = volctrl.channel3 & mask[23];
return cdrom_mode_select (drive, 0x0e, buffer,
sizeof (buffer), NULL);
}
case CDROMVOLREAD: {
struct cdrom_volctrl volctrl;
char buffer[24];
int stat;
stat = verify_area (VERIFY_WRITE, (void *) arg,
sizeof (volctrl));
if (stat) return stat;
stat = cdrom_mode_sense (drive, 0x0e, 0, buffer,
sizeof (buffer), NULL);
if (stat) return stat;
volctrl.channel0 = buffer[17];
volctrl.channel1 = buffer[19];
volctrl.channel2 = buffer[21];
volctrl.channel3 = buffer[23];
memcpy_tofs ((void *) arg, &volctrl, sizeof (volctrl));
return 0;
}
case CDROMMULTISESSION: {
struct cdrom_multisession ms_info;
struct atapi_toc *toc;
int stat;
stat = verify_area (VERIFY_WRITE, (void *)arg,
sizeof (ms_info));
if (stat) return stat;
memcpy_fromfs (&ms_info, (void *)arg, sizeof (ms_info));
stat = cdrom_read_toc (drive, NULL);
if (stat) return stat;
toc = drive->cdrom_info.toc;
if (ms_info.addr_format == CDROM_MSF)
lba_to_msf (toc->last_session_lba,
&ms_info.addr.msf.minute,
&ms_info.addr.msf.second,
&ms_info.addr.msf.frame);
else if (ms_info.addr_format == CDROM_LBA)
ms_info.addr.lba = toc->last_session_lba;
else
return -EINVAL;
ms_info.xa_flag = toc->xa_flag;
memcpy_tofs ((void *)arg, &ms_info, sizeof (ms_info));
return 0;
}
case CDROMREADAUDIO: {
int stat, lba;
struct atapi_toc *toc;
struct cdrom_read_audio ra;
char *buf;
stat = cdrom_read_toc (drive, NULL);
if (stat) return stat;
toc = drive->cdrom_info.toc;
stat = verify_area (VERIFY_READ, (char *)arg, sizeof (ra));
if (stat) return stat;
memcpy_fromfs (&ra, (void *)arg, sizeof (ra));
if (ra.nframes < 0 || ra.nframes > toc->capacity)
return -EINVAL;
else if (ra.nframes == 0)
return 0;
stat = verify_area (VERIFY_WRITE, (char *)ra.buf,
ra.nframes * CD_FRAMESIZE_RAW);
if (stat) return stat;
if (ra.addr_format == CDROM_MSF)
lba = msf_to_lba (ra.addr.msf.minute,
ra.addr.msf.second,
ra.addr.msf.frame);
else if (ra.addr_format == CDROM_LBA)
lba = ra.addr.lba;
else
return -EINVAL;
if (lba < 0 || lba >= toc->capacity)
return -EINVAL;
buf = (char *) kmalloc (CDROM_NBLOCKS_BUFFER*CD_FRAMESIZE_RAW,
GFP_KERNEL);
if (buf == NULL)
return -ENOMEM;
while (ra.nframes > 0) {
int this_nblocks = ra.nframes;
if (this_nblocks > CDROM_NBLOCKS_BUFFER)
this_nblocks = CDROM_NBLOCKS_BUFFER;
stat = cdrom_read_block
(drive, 1, lba, this_nblocks,
buf, this_nblocks * CD_FRAMESIZE_RAW, NULL);
if (stat) break;
memcpy_tofs (ra.buf, buf,
this_nblocks * CD_FRAMESIZE_RAW);
ra.buf += this_nblocks * CD_FRAMESIZE_RAW;
ra.nframes -= this_nblocks;
lba += this_nblocks;
}
kfree (buf);
return stat;
}
case CDROMREADRAW:
case CDROMREADMODE1:
case CDROMREADMODE2: {
struct cdrom_msf msf;
int blocksize, format, stat, lba;
char *buf;
if (cmd == CDROMREADMODE1) {
blocksize = CD_FRAMESIZE;
format = 2;
} else if (cmd == CDROMREADMODE2) {
blocksize = CD_FRAMESIZE_RAW0;
format = 3;
} else {
blocksize = CD_FRAMESIZE_RAW;
format = 0;
}
stat = verify_area (VERIFY_WRITE, (char *)arg, blocksize);
if (stat) return stat;
memcpy_fromfs (&msf, (void *)arg, sizeof (msf));
lba = msf_to_lba (msf.cdmsf_min0,
msf.cdmsf_sec0,
msf.cdmsf_frame0);
buf = (char *) kmalloc (CD_FRAMESIZE_RAW, GFP_KERNEL);
if (buf == NULL)
return -ENOMEM;
stat = cdrom_read_block (drive, format, lba, 1, buf, blocksize,
NULL);
if (stat == 0)
memcpy_tofs ((char *)arg, buf, blocksize);
kfree (buf);
return stat;
}
case CDROM_GET_UPC: {
int stat;
char mcnbuf[24];
struct cdrom_mcn mcn;
stat = verify_area (VERIFY_WRITE, (void *) arg,
sizeof (mcn));
if (stat) return stat;
stat = cdrom_read_subchannel (drive, 2,
mcnbuf, sizeof (mcnbuf),
NULL);
if (stat) return stat;
memcpy (mcn.medium_catalog_number, mcnbuf+9,
sizeof (mcn.medium_catalog_number)-1);
mcn.medium_catalog_number[sizeof (mcn.medium_catalog_number)-1]
= '\0';
memcpy_tofs ((void *) arg, &mcn, sizeof (mcn));
return stat;
}
case CDROMLOADFROMSLOT:
printk ("%s: Use CDROM_SELECT_DISC "
" instead of CDROMLOADFROMSLOT.\n", drive->name);
case CDROM_SELECT_DISC: {
struct atapi_request_sense my_reqbuf;
int stat;
if (drive->usage > 1)
return -EBUSY;
(void) cdrom_load_unload (drive, -1, NULL);
cdrom_saw_media_change (drive);
if (arg == -1) {
(void) cdrom_lockdoor (drive, 0, NULL);
return 0;
}
(void) cdrom_load_unload (drive, (int)arg, NULL);
stat = cdrom_check_status (drive, &my_reqbuf);
if (stat && my_reqbuf.sense_key == NOT_READY) {
return -ENOENT;
}
return cdrom_read_toc (drive, &my_reqbuf);
}
#if 0
case CDROMRESET: {
struct request req;
ide_init_drive_cmd (&req);
req.cmd = RESET_DRIVE_COMMAND;
return ide_do_drive_cmd (drive, &req, ide_wait);
}
#endif
#ifdef TEST
case 0x1234: {
int stat;
struct packet_command pc;
int len, lena;
memset (&pc, 0, sizeof (pc));
stat = verify_area (VERIFY_READ, (void *) arg, sizeof (pc.c));
if (stat) return stat;
memcpy_fromfs (&pc.c, (void *) arg, sizeof (pc.c));
arg += sizeof (pc.c);
stat = verify_area (VERIFY_READ, (void *) arg, sizeof (len));
if (stat) return stat;
memcpy_fromfs (&len, (void *) arg , sizeof (len));
arg += sizeof (len);
if (len > 0) {
stat = verify_area (VERIFY_WRITE, (void *) arg, len);
if (stat) return stat;
}
lena = len;
if (lena  < 0) lena = 0;
{
char buf[lena];
if (len > 0) {
pc.buflen = len;
pc.buffer = buf;
}
stat = cdrom_queue_packet_command (drive, &pc, 0);
if (len > 0)
memcpy_tofs ((void *)arg, buf, len);
}
return stat;
}
#endif
default:
return -EPERM;
}
}
int ide_cdrom_check_media_change (ide_drive_t *drive)
{
int retval;
(void) cdrom_check_status (drive, NULL);
retval = CDROM_STATE_FLAGS (drive)->media_changed;
CDROM_STATE_FLAGS (drive)->media_changed = 0;
return retval;
}
int ide_cdrom_open (struct inode *ip, struct file *fp, ide_drive_t *drive)
{
if (fp->f_mode & 2) {
--drive->usage;
return -EROFS;
}
if (drive->usage == 1) {
int stat;
struct atapi_request_sense my_reqbuf;
my_reqbuf.sense_key = 0;
stat = cdrom_check_status (drive, &my_reqbuf);
if (stat && my_reqbuf.sense_key == NOT_READY) {
cdrom_eject (drive, 1, &my_reqbuf);
stat = cdrom_check_status (drive, &my_reqbuf);
}
if (stat == 0 || my_reqbuf.sense_key == UNIT_ATTENTION) {
(void) cdrom_lockdoor (drive, 1, &my_reqbuf);
(void) cdrom_read_toc (drive, &my_reqbuf);
} else {
--drive->usage;
return -ENXIO;
}
}
return 0;
}
void ide_cdrom_release (struct inode *inode, struct file *file,
ide_drive_t *drive)
{
if (drive->usage == 0) {
invalidate_buffers (inode->i_rdev);
(void) cdrom_lockdoor (drive, 0, NULL);
if (CDROM_STATE_FLAGS (drive)->eject_on_close)
(void) cdrom_eject (drive, 0, NULL);
}
}
void ide_cdrom_setup (ide_drive_t *drive)
{
blksize_size[HWIF(drive)->major][drive->select.b.unit << PARTN_BITS] =
CD_FRAMESIZE;
drive->special.all = 0;
drive->ready_stat = 0;
CDROM_STATE_FLAGS (drive)->media_changed = 0;
CDROM_STATE_FLAGS (drive)->toc_valid     = 0;
CDROM_STATE_FLAGS (drive)->door_locked   = 0;
CDROM_STATE_FLAGS (drive)->eject_on_close= 0;
#if NO_DOOR_LOCKING
CDROM_CONFIG_FLAGS (drive)->no_doorlock = 1;
#else
CDROM_CONFIG_FLAGS (drive)->no_doorlock = 0;
#endif
CDROM_STATE_FLAGS (drive)->sanyo_slot = 0;
if (drive->id != NULL)
CDROM_CONFIG_FLAGS (drive)->drq_interrupt =
((drive->id->config & 0x0060) == 0x20);
else
CDROM_CONFIG_FLAGS (drive)->drq_interrupt = 0;
#if ! STANDARD_ATAPI
drive->cdrom_info.max_sectors = 252;
CDROM_CONFIG_FLAGS (drive)->old_readcd = 0;
CDROM_CONFIG_FLAGS (drive)->toctracks_as_bcd = 0;
CDROM_CONFIG_FLAGS (drive)->tocaddr_as_bcd = 0;
CDROM_CONFIG_FLAGS (drive)->playmsf_as_bcd = 0;
CDROM_CONFIG_FLAGS (drive)->subchan_as_bcd = 0;
if (drive->id != NULL) {
const char *model = (const char *)drive->id->model;
const char *fw_rev = (const char *)drive->id->fw_rev;
if (strcmp (model, "V003S0DS") == 0 &&
fw_rev[4] == '1' &&
fw_rev[6] <= '2') {
CDROM_CONFIG_FLAGS (drive)->toctracks_as_bcd = 1;
CDROM_CONFIG_FLAGS (drive)->tocaddr_as_bcd = 1;
CDROM_CONFIG_FLAGS (drive)->playmsf_as_bcd = 1;
CDROM_CONFIG_FLAGS (drive)->subchan_as_bcd = 1;
}
else if (strcmp (model, "V006E0DS") == 0 &&
fw_rev[4] == '1' &&
fw_rev[6] <= '2') {
CDROM_CONFIG_FLAGS (drive)->toctracks_as_bcd = 1;
}
else if (strcmp (model, "GCD-R580B") == 0)
drive->cdrom_info.max_sectors = 124;
else if (strcmp (model,
"NEC CD-ROM DRIVE:260") == 0 &&
strcmp (fw_rev, "1.01") == 0) {
CDROM_CONFIG_FLAGS (drive)->tocaddr_as_bcd = 1;
CDROM_CONFIG_FLAGS (drive)->playmsf_as_bcd = 1;
CDROM_CONFIG_FLAGS (drive)->subchan_as_bcd = 1;
}
else if (strcmp (model, "WEARNES CDD-120") == 0 &&
strcmp (fw_rev, "A1.1") == 0) {
CDROM_CONFIG_FLAGS (drive)->playmsf_as_bcd = 1;
CDROM_CONFIG_FLAGS (drive)->subchan_as_bcd = 1;
}
else if ((strcmp(model, "CD-ROM CDR-C3 G") == 0) ||
(strcmp(model, "CD-ROM CDR-C3G") == 0) ||
(strcmp(model, "CD-ROM CDR_C36") == 0)) {
CDROM_STATE_FLAGS (drive)->sanyo_slot = 3;
}
}
#endif
drive->cdrom_info.toc               = NULL;
drive->cdrom_info.sector_buffer     = NULL;
drive->cdrom_info.sector_buffered   = 0;
drive->cdrom_info.nsectors_buffered = 0;
}