#include <linux/module.h>
#include <linux/fs.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/mm.h>
#include <linux/string.h>
#include <linux/errno.h>
#include <linux/cdrom.h>
#include <linux/interrupt.h>
#include <asm/system.h>
#define MAJOR_NR SCSI_CDROM_MAJOR
#include <linux/blk.h>
#include "scsi.h"
#include "hosts.h"
#include "sr.h"
#include <scsi/scsi_ioctl.h>
#include "constants.h"
#define MAX_RETRIES 3
#define SR_TIMEOUT (30 * HZ)
static int sr_init(void);
static void sr_finish(void);
static int sr_attach(Scsi_Device *);
static int sr_detect(Scsi_Device *);
static void sr_detach(Scsi_Device *);
struct Scsi_Device_Template sr_template = {NULL, "cdrom", "sr", NULL, TYPE_ROM,
SCSI_CDROM_MAJOR, 0, 0, 0, 1,
sr_detect, sr_init,
sr_finish, sr_attach, sr_detach};
Scsi_CD * scsi_CDs = NULL;
static int * sr_sizes;
static int * sr_blocksizes;
static int sr_open(struct inode *, struct file *);
void get_sectorsize(int);
void sr_photocd(struct inode *);
extern int sr_ioctl(struct inode *, struct file *, unsigned int, unsigned long);
void requeue_sr_request (Scsi_Cmnd * SCpnt);
static int check_cdrom_media_change(kdev_t);
static void sr_release(struct inode * inode, struct file * file)
{
sync_dev(inode->i_rdev);
if(! --scsi_CDs[MINOR(inode->i_rdev)].device->access_count)
{
sr_ioctl(inode, NULL, SCSI_IOCTL_DOORUNLOCK, 0);
if (scsi_CDs[MINOR(inode->i_rdev)].auto_eject)
sr_ioctl(inode, NULL, CDROMEJECT, 0);
}
if (scsi_CDs[MINOR(inode->i_rdev)].device->host->hostt->usage_count)
(*scsi_CDs[MINOR(inode->i_rdev)].device->host->hostt->usage_count)--;
if(sr_template.usage_count) (*sr_template.usage_count)--;
}
static struct file_operations sr_fops =
{
NULL,
block_read,
block_write,
NULL,
NULL,
sr_ioctl,
NULL,
sr_open,
sr_release,
NULL,
NULL,
check_cdrom_media_change,
NULL
};
int check_cdrom_media_change(kdev_t full_dev){
int retval, target;
struct inode inode;
int flag = 0;
target =  MINOR(full_dev);
if (target >= sr_template.nr_dev) {
printk("CD-ROM request error: invalid device.\n");
return 0;
};
inode.i_rdev = full_dev;
retval = sr_ioctl(&inode, NULL, SCSI_IOCTL_TEST_UNIT_READY, 0);
if(retval){
scsi_CDs[target].device->changed = 1;
return 1;
};
retval = scsi_CDs[target].device->changed;
if(!flag) {
scsi_CDs[target].device->changed = 0;
if (retval) scsi_CDs[target].needs_sector_size = 1;
};
return retval;
}
static void rw_intr (Scsi_Cmnd * SCpnt)
{
int result = SCpnt->result;
int this_count = SCpnt->this_count;
int good_sectors = (result == 0 ? this_count : 0);
int block_sectors = 0;
#ifdef DEBUG
printk("sr.c done: %x %x\n",result, SCpnt->request.bh->b_data);
#endif
if (driver_byte(result) != 0 &&
SCpnt->sense_buffer[0] == 0xF0 &&
(SCpnt->sense_buffer[2] == MEDIUM_ERROR ||
SCpnt->sense_buffer[2] == VOLUME_OVERFLOW ||
SCpnt->sense_buffer[2] == ILLEGAL_REQUEST))
{
long error_sector = (SCpnt->sense_buffer[3] << 24) |
(SCpnt->sense_buffer[4] << 16) |
(SCpnt->sense_buffer[5] << 8) |
SCpnt->sense_buffer[6];
int device_nr = DEVICE_NR(SCpnt->request.rq_dev);
if (SCpnt->request.bh != NULL)
block_sectors = SCpnt->request.bh->b_size >> 9;
if (block_sectors < 4) block_sectors = 4;
if (scsi_CDs[device_nr].sector_size == 2048)
error_sector <<= 2;
error_sector &= ~ (block_sectors - 1);
good_sectors = error_sector - SCpnt->request.sector;
if (good_sectors < 0 || good_sectors >= this_count)
good_sectors = 0;
if ((error_sector >> 1) < sr_sizes[device_nr] &&
scsi_CDs[device_nr].capacity - error_sector < 4*75)
sr_sizes[device_nr] = error_sector >> 1;
}
if (good_sectors > 0)
{
if (SCpnt->use_sg == 0) {
if (SCpnt->buffer != SCpnt->request.buffer)
{
int offset;
offset = (SCpnt->request.sector % 4) << 9;
memcpy((char *)SCpnt->request.buffer,
(char *)SCpnt->buffer + offset,
good_sectors << 9);
if((offset == 0) && good_sectors == 2 &&
SCpnt->request.nr_sectors > good_sectors &&
SCpnt->request.bh &&
SCpnt->request.bh->b_reqnext &&
SCpnt->request.bh->b_reqnext->b_size == 1024) {
memcpy((char *)SCpnt->request.bh->b_reqnext->b_data,
(char *)SCpnt->buffer + 1024,
1024);
good_sectors += 2;
};
scsi_free(SCpnt->buffer, 2048);
}
} else {
struct scatterlist * sgpnt;
int i;
sgpnt = (struct scatterlist *) SCpnt->buffer;
for(i=0; i<SCpnt->use_sg; i++) {
if (sgpnt[i].alt_address) {
if (sgpnt[i].alt_address != sgpnt[i].address) {
memcpy(sgpnt[i].alt_address, sgpnt[i].address, sgpnt[i].length);
};
scsi_free(sgpnt[i].address, sgpnt[i].length);
};
};
scsi_free(SCpnt->buffer, SCpnt->sglist_len);
if(SCpnt->request.sector % 4) good_sectors -= 2;
if(good_sectors > SCpnt->request.nr_sectors)
good_sectors -= 2;
};
#ifdef DEBUG
printk("(%x %x %x) ",SCpnt->request.bh, SCpnt->request.nr_sectors,
good_sectors);
#endif
if (SCpnt->request.nr_sectors > this_count)
{
SCpnt->request.errors = 0;
if (!SCpnt->request.bh)
panic("sr.c: linked page request (%lx %x)",
SCpnt->request.sector, this_count);
}
SCpnt = end_scsi_request(SCpnt, 1, good_sectors);
if (result == 0)
{
requeue_sr_request(SCpnt);
return;
}
}
if (good_sectors == 0) {
if (SCpnt->use_sg) {
struct scatterlist * sgpnt;
int i;
sgpnt = (struct scatterlist *) SCpnt->buffer;
for(i=0; i<SCpnt->use_sg; i++) {
if (sgpnt[i].alt_address) {
scsi_free(sgpnt[i].address, sgpnt[i].length);
}
}
scsi_free(SCpnt->buffer, SCpnt->sglist_len);
} else {
if (SCpnt->buffer != SCpnt->request.buffer)
scsi_free(SCpnt->buffer, SCpnt->bufflen);
}
}
if (driver_byte(result) != 0) {
if ((SCpnt->sense_buffer[0] & 0x7f) == 0x70) {
if ((SCpnt->sense_buffer[2] & 0xf) == UNIT_ATTENTION) {
scsi_CDs[DEVICE_NR(SCpnt->request.rq_dev)].device->changed = 1;
SCpnt = end_scsi_request(SCpnt, 0, this_count);
requeue_sr_request(SCpnt);
return;
}
}
if (SCpnt->sense_buffer[2] == ILLEGAL_REQUEST) {
printk("CD-ROM error: ");
print_sense("sr", SCpnt);
printk("command was: ");
print_command(SCpnt->cmnd);
if (scsi_CDs[DEVICE_NR(SCpnt->request.rq_dev)].ten) {
scsi_CDs[DEVICE_NR(SCpnt->request.rq_dev)].ten = 0;
requeue_sr_request(SCpnt);
result = 0;
return;
} else {
SCpnt = end_scsi_request(SCpnt, 0, this_count);
requeue_sr_request(SCpnt);
return;
}
}
if (SCpnt->sense_buffer[2] == NOT_READY) {
printk("CD-ROM not ready.  Make sure you have a disc in the drive.\n");
SCpnt = end_scsi_request(SCpnt, 0, this_count);
requeue_sr_request(SCpnt);
return;
}
if (SCpnt->sense_buffer[2] == MEDIUM_ERROR) {
printk("scsi%d: MEDIUM ERROR on "
"channel %d, id %d, lun %d, CDB: ",
SCpnt->host->host_no, (int) SCpnt->channel,
(int) SCpnt->target, (int) SCpnt->lun);
print_command(SCpnt->cmnd);
print_sense("sr", SCpnt);
SCpnt = end_scsi_request(SCpnt, 0, block_sectors);
requeue_sr_request(SCpnt);
return;
}
if (SCpnt->sense_buffer[2] == VOLUME_OVERFLOW) {
printk("scsi%d: VOLUME OVERFLOW on "
"channel %d, id %d, lun %d, CDB: ",
SCpnt->host->host_no, (int) SCpnt->channel,
(int) SCpnt->target, (int) SCpnt->lun);
print_command(SCpnt->cmnd);
print_sense("sr", SCpnt);
SCpnt = end_scsi_request(SCpnt, 0, block_sectors);
requeue_sr_request(SCpnt);
return;
}
}
if(result) {
printk("SCSI CD error : host %d id %d lun %d return code = %03x\n",
scsi_CDs[DEVICE_NR(SCpnt->request.rq_dev)].device->host->host_no,
scsi_CDs[DEVICE_NR(SCpnt->request.rq_dev)].device->id,
scsi_CDs[DEVICE_NR(SCpnt->request.rq_dev)].device->lun,
result);
if (status_byte(result) == CHECK_CONDITION)
print_sense("sr", SCpnt);
SCpnt = end_scsi_request(SCpnt, 0, SCpnt->request.current_nr_sectors);
requeue_sr_request(SCpnt);
}
}
void sr_photocd(struct inode *inode)
{
unsigned long   sector,min,sec,frame;
unsigned char   buf[40];
unsigned char   *cmd;
unsigned char   *send;
unsigned char   *rec;
int             rc,is_xa,no_multi;
if (scsi_CDs[MINOR(inode->i_rdev)].xa_flags & 0x02) {
#ifdef DEBUG
printk(KERN_DEBUG "sr_photocd: CDROM and/or driver do not support multisession CD's");
#endif
return;
}
if (!suser()) {
if (1 == scsi_CDs[MINOR(inode->i_rdev)].device->access_count) {
scsi_CDs[MINOR(inode->i_rdev)].mpcd_sector = 0;
scsi_CDs[MINOR(inode->i_rdev)].xa_flags &= ~0x01;
}
return;
}
sector   = 0;
is_xa    = 0;
no_multi = 0;
cmd = rec = &buf[8];
switch(scsi_CDs[MINOR(inode->i_rdev)].device->manufacturer) {
case SCSI_MAN_NEC:
#ifdef DEBUG
printk(KERN_DEBUG "sr_photocd: use NEC code\n");
#endif
memset(buf,0,40);
*((unsigned int*)buf)   = 0x0;
*((unsigned int*)buf+1) = 0x16;
cmd[0] = 0xde;
cmd[1] = 0x03;
cmd[2] = 0xb0;
rc = kernel_scsi_ioctl(scsi_CDs[MINOR(inode->i_rdev)].device,
SCSI_IOCTL_SEND_COMMAND, buf);
if (rc != 0) {
if (rc != 0x28000002)
printk(KERN_WARNING"sr_photocd: ioctl error (NEC): 0x%x\n",rc);
break;
}
if (rec[14] != 0 && rec[14] != 0xb0) {
printk(KERN_INFO"sr_photocd: (NEC) Hmm, seems the CDROM doesn't support multisession CD's\n");
no_multi = 1;
break;
}
min   = (unsigned long) rec[15]/16*10 + (unsigned long) rec[15]%16;
sec   = (unsigned long) rec[16]/16*10 + (unsigned long) rec[16]%16;
frame = (unsigned long) rec[17]/16*10 + (unsigned long) rec[17]%16;
sector = min*CD_SECS*CD_FRAMES + sec*CD_FRAMES + frame;
is_xa  = (rec[14] == 0xb0);
#ifdef DEBUG
if (sector) {
printk(KERN_DEBUG "sr_photocd: multisession CD detected. start: %lu\n",sector);
}
#endif
break;
case SCSI_MAN_TOSHIBA:
#ifdef DEBUG
printk(KERN_DEBUG "sr_photocd: use TOSHIBA code\n");
#endif
memset(buf,0,40);
*((unsigned int*)buf)   = (unsigned int) 0;
*((unsigned int*)buf+1) = (unsigned int) 4;
cmd[0]                  = (unsigned char) 0x00c7;
cmd[1]                  = (unsigned char) 3;
rc = kernel_scsi_ioctl(scsi_CDs[MINOR(inode->i_rdev)].device,
SCSI_IOCTL_SEND_COMMAND, buf);
if (rc != 0) {
if (rc == 0x28000002) {
if (!kernel_scsi_ioctl(scsi_CDs[MINOR(inode->i_rdev)].device,
SCSI_IOCTL_TEST_UNIT_READY, NULL)) {
printk(KERN_INFO "sr_photocd: (TOSHIBA) Hmm, seems the CDROM doesn't support multisession CD's\n");
no_multi = 1;
}
} else
printk(KERN_INFO"sr_photocd: ioctl error (TOSHIBA #1): 0x%x\n",rc);
break;
}
is_xa  = (rec[0] == 0x20);
min    = (unsigned long) rec[1]/16*10 + (unsigned long) rec[1]%16;
sec    = (unsigned long) rec[2]/16*10 + (unsigned long) rec[2]%16;
frame  = (unsigned long) rec[3]/16*10 + (unsigned long) rec[3]%16;
sector = min*CD_SECS*CD_FRAMES + sec*CD_FRAMES + frame;
if (sector) {
sector -= CD_BLOCK_OFFSET;
#ifdef DEBUG
printk(KERN_DEBUG "sr_photocd: multisession CD detected: start: %lu\n",sector);
#endif
}
memset(buf,0,40);
*((unsigned int*)buf)   = (unsigned int) 0;
*((unsigned int*)buf+1) = (unsigned int) 12;
cmd[0]                  = (unsigned char) MODE_SENSE;
cmd[2]                  = (unsigned char) 1;
cmd[4]                  = (unsigned char) 12;
rc = kernel_scsi_ioctl(scsi_CDs[MINOR(inode->i_rdev)].device,
SCSI_IOCTL_SEND_COMMAND, buf);
if (rc != 0) {
printk(KERN_WARNING "sr_photocd: ioctl error (TOSHIBA #2): 0x%x\n",rc);
break;
}
#ifdef DEBUG
printk(KERN_DEBUG "sr_photocd: get_density: 0x%x\n",rec[4]);
#endif
if ((rec[4] != 0x81 && is_xa) || (rec[4] != 0 && !is_xa)) {
#ifdef DEBUG
printk(KERN_DEBUG "sr_photocd: doing set_density\n");
#endif
memset(buf,0,40);
*((unsigned int*)buf)   = (unsigned int) 12;
*((unsigned int*)buf+1) = (unsigned int) 0;
cmd[0]                  = (unsigned char) MODE_SELECT;
cmd[1]                  = (unsigned char) (1 << 4);
cmd[4]                  = (unsigned char) 12;
send = &cmd[6];
send[ 3]                = (unsigned char) 0x08;
send[ 4]                = (is_xa) ?
(unsigned char) 0x81 : (unsigned char) 0;
send[10]                = (unsigned char) 0x08;
rc = kernel_scsi_ioctl(scsi_CDs[MINOR(inode->i_rdev)].device,
SCSI_IOCTL_SEND_COMMAND, buf);
if (rc != 0) {
printk(KERN_WARNING "sr_photocd: ioctl error (TOSHIBA #3): 0x%x\n",rc);
}
scsi_CDs[MINOR(inode->i_rdev)].needs_sector_size = 1;
}
break;
case SCSI_MAN_SONY:
case SCSI_MAN_PIONEER:
case SCSI_MAN_UNKNOWN:
#ifdef DEBUG
printk(KERN_DEBUG "sr_photocd: use SONY/PIONEER code\n");
#endif
get_sectorsize(MINOR(inode->i_rdev));
memset(buf,0,40);
*((unsigned int*)buf)   = 0x0;
*((unsigned int*)buf+1) = 0x0c;
cmd[0] = READ_TOC;
cmd[8] = 0x0c;
cmd[9] = 0x40;
rc = kernel_scsi_ioctl(scsi_CDs[MINOR(inode->i_rdev)].device,
SCSI_IOCTL_SEND_COMMAND, buf);
if (rc != 0) {
if (rc != 0x28000002)
printk(KERN_WARNING "sr_photocd: ioctl error (SONY/PIONEER): 0x%x\n",rc);
break;
}
if ((rec[0] << 8) + rec[1] < 0x0a) {
printk(KERN_INFO "sr_photocd: (SONY/PIONEER) Hmm, seems the CDROM doesn't support multisession CD's\n");
no_multi = 1;
break;
}
sector = rec[11] + (rec[10] << 8) + (rec[9] << 16) + (rec[8] << 24);
is_xa = !!sector;
#ifdef DEBUG
if (sector)
printk (KERN_DEBUG "sr_photocd: multisession CD detected. start: %lu\n",sector);
#endif
break;
case SCSI_MAN_NEC_OLDCDR:
default:
sector = 0;
no_multi = 1;
break; }
scsi_CDs[MINOR(inode->i_rdev)].mpcd_sector = sector;
if (is_xa)
scsi_CDs[MINOR(inode->i_rdev)].xa_flags |= 0x01;
else
scsi_CDs[MINOR(inode->i_rdev)].xa_flags &= ~0x01;
if (no_multi)
scsi_CDs[MINOR(inode->i_rdev)].xa_flags |= 0x02;
return;
}
static int sr_open(struct inode * inode, struct file * filp)
{
if(MINOR(inode->i_rdev) >= sr_template.nr_dev ||
!scsi_CDs[MINOR(inode->i_rdev)].device) return -ENXIO;
if (filp->f_mode & 2)
return -EROFS;
if(sr_template.usage_count) (*sr_template.usage_count)++;
sr_ioctl(inode,filp,CDROMCLOSETRAY,0);
check_disk_change(inode->i_rdev);
if(!scsi_CDs[MINOR(inode->i_rdev)].device->access_count++)
sr_ioctl(inode, NULL, SCSI_IOCTL_DOORLOCK, 0);
if (scsi_CDs[MINOR(inode->i_rdev)].device->host->hostt->usage_count)
(*scsi_CDs[MINOR(inode->i_rdev)].device->host->hostt->usage_count)++;
sr_photocd(inode);
if(scsi_CDs[MINOR(inode->i_rdev)].needs_sector_size)
get_sectorsize(MINOR(inode->i_rdev));
return 0;
}
static void do_sr_request (void)
{
Scsi_Cmnd * SCpnt = NULL;
struct request * req = NULL;
Scsi_Device * SDev;
unsigned long flags;
int flag = 0;
while (1==1){
save_flags(flags);
cli();
if (CURRENT != NULL && CURRENT->rq_status == RQ_INACTIVE) {
restore_flags(flags);
return;
};
INIT_SCSI_REQUEST;
SDev = scsi_CDs[DEVICE_NR(CURRENT->rq_dev)].device;
if( SDev->was_reset )
{
if( SDev->removable && !intr_count )
{
scsi_ioctl(SDev, SCSI_IOCTL_DOORLOCK, 0);
}
SDev->was_reset = 0;
}
if (flag++ == 0)
SCpnt = allocate_device(&CURRENT,
scsi_CDs[DEVICE_NR(CURRENT->rq_dev)].device, 0);
else SCpnt = NULL;
restore_flags(flags);
if (!SCpnt && sr_template.nr_dev > 1){
struct request *req1;
req1 = NULL;
save_flags(flags);
cli();
req = CURRENT;
while(req){
SCpnt = request_queueable(req,
scsi_CDs[DEVICE_NR(req->rq_dev)].device);
if(SCpnt) break;
req1 = req;
req = req->next;
};
if (SCpnt && req->rq_status == RQ_INACTIVE) {
if (req == CURRENT)
CURRENT = CURRENT->next;
else
req1->next = req->next;
};
restore_flags(flags);
};
if (!SCpnt)
return;
wake_up(&wait_for_request);
requeue_sr_request(SCpnt);
};
}
void requeue_sr_request (Scsi_Cmnd * SCpnt)
{
unsigned int dev, block, realcount;
unsigned char cmd[12], *buffer, tries;
int this_count, start, end_rec;
tries = 2;
repeat:
if(!SCpnt || SCpnt->request.rq_status == RQ_INACTIVE) {
do_sr_request();
return;
}
dev =  MINOR(SCpnt->request.rq_dev);
block = SCpnt->request.sector;
buffer = NULL;
this_count = 0;
if (dev >= sr_template.nr_dev) {
SCpnt = end_scsi_request(SCpnt, 0, SCpnt->request.nr_sectors);
tries = 2;
goto repeat;
}
if (!scsi_CDs[dev].use) {
SCpnt = end_scsi_request(SCpnt, 0, SCpnt->request.nr_sectors);
tries = 2;
goto repeat;
}
if (scsi_CDs[dev].device->changed) {
SCpnt = end_scsi_request(SCpnt, 0, SCpnt->request.nr_sectors);
tries = 2;
goto repeat;
}
switch (SCpnt->request.cmd)
{
case WRITE:
SCpnt = end_scsi_request(SCpnt, 0, SCpnt->request.nr_sectors);
goto repeat;
break;
case READ :
cmd[0] = READ_6;
break;
default :
panic ("Unknown sr command %d\n", SCpnt->request.cmd);
}
cmd[1] = (SCpnt->lun << 5) & 0xe0;
SCpnt->use_sg = 0;
if (SCpnt->host->sg_tablesize > 0 &&
(!need_isa_buffer ||
dma_free_sectors >= 10)) {
struct buffer_head * bh;
struct scatterlist * sgpnt;
int count, this_count_max;
bh = SCpnt->request.bh;
this_count = 0;
count = 0;
this_count_max = (scsi_CDs[dev].ten ? 0xffff : 0xff) << 4;
this_count = SCpnt->request.sector % 4;
if(this_count) count++;
while(bh && count < SCpnt->host->sg_tablesize) {
if ((this_count + (bh->b_size >> 9)) > this_count_max) break;
this_count += (bh->b_size >> 9);
count++;
bh = bh->b_reqnext;
};
end_rec = 0;
if(this_count % 4) {
if (count < SCpnt->host->sg_tablesize) {
count++;
end_rec = (4 - (this_count % 4)) << 9;
this_count += 4 - (this_count % 4);
} else {
count--;
this_count -= (this_count % 4);
};
};
SCpnt->use_sg = count;
count  = (SCpnt->use_sg * sizeof(struct scatterlist) + 511) & ~511;
SCpnt->sglist_len = count;
sgpnt = (struct scatterlist * ) scsi_malloc(count);
if (!sgpnt) {
printk("Warning - running *really* short on DMA buffers\n");
SCpnt->use_sg = 0;
} else {
buffer = (unsigned char *) sgpnt;
count = 0;
bh = SCpnt->request.bh;
if(SCpnt->request.sector % 4) {
sgpnt[count].length = (SCpnt->request.sector % 4) << 9;
sgpnt[count].address = (char *) scsi_malloc(sgpnt[count].length);
if(!sgpnt[count].address) panic("SCSI DMA pool exhausted.");
sgpnt[count].alt_address = sgpnt[count].address;
count++;
};
for(bh = SCpnt->request.bh; count < SCpnt->use_sg;
count++, bh = bh->b_reqnext) {
if (bh) {
sgpnt[count].address = bh->b_data;
sgpnt[count].length = bh->b_size;
sgpnt[count].alt_address = NULL;
} else {
sgpnt[count].address = (char *) scsi_malloc(end_rec);
if(!sgpnt[count].address) panic("SCSI DMA pool exhausted.");
sgpnt[count].length = end_rec;
sgpnt[count].alt_address = sgpnt[count].address;
if (count+1 != SCpnt->use_sg) panic("Bad sr request list");
break;
};
if (((long) sgpnt[count].address) + sgpnt[count].length - 1 >
ISA_DMA_THRESHOLD && SCpnt->host->unchecked_isa_dma) {
sgpnt[count].alt_address = sgpnt[count].address;
if(dma_free_sectors < (sgpnt[count].length >> 9) + 5) {
sgpnt[count].address = NULL;
} else {
sgpnt[count].address = (char *) scsi_malloc(sgpnt[count].length);
};
if(sgpnt[count].address == NULL){
printk("Warning: Running low on SCSI DMA buffers");
while(--count >= 0){
if(sgpnt[count].alt_address)
scsi_free(sgpnt[count].address, sgpnt[count].length);
};
SCpnt->use_sg = 0;
scsi_free(buffer, SCpnt->sglist_len);
break;
};
};
};
#ifdef DEBUG
printk("SR: %d %d %d %d %d *** ",SCpnt->use_sg, SCpnt->request.sector,
this_count,
SCpnt->request.current_nr_sectors,
SCpnt->request.nr_sectors);
for(count=0; count<SCpnt->use_sg; count++)
printk("SGlist: %d %x %x %x\n", count,
sgpnt[count].address,
sgpnt[count].alt_address,
sgpnt[count].length);
#endif
};
};
if (SCpnt->use_sg == 0){
if (!SCpnt->request.bh)
this_count = SCpnt->request.nr_sectors;
else
this_count = (SCpnt->request.bh->b_size >> 9);
start = block % 4;
if (start)
{
this_count = ((this_count > 4 - start) ?
(4 - start) : (this_count));
buffer = (unsigned char *) scsi_malloc(2048);
}
else if (this_count < 4)
{
buffer = (unsigned char *) scsi_malloc(2048);
}
else
{
this_count -= this_count % 4;
buffer = (unsigned char *) SCpnt->request.buffer;
if (((long) buffer) + (this_count << 9) > ISA_DMA_THRESHOLD &&
SCpnt->host->unchecked_isa_dma)
buffer = (unsigned char *) scsi_malloc(this_count << 9);
}
};
if (scsi_CDs[dev].sector_size == 2048)
block = block >> 2;
else
block = block & 0xfffffffc;
realcount = (this_count + 3) / 4;
if (scsi_CDs[dev].sector_size == 512) realcount = realcount << 2;
if (((realcount > 0xff) || (block > 0x1fffff)) && scsi_CDs[dev].ten)
{
if (realcount > 0xffff)
{
realcount = 0xffff;
this_count = realcount * (scsi_CDs[dev].sector_size >> 9);
}
cmd[0] += READ_10 - READ_6 ;
cmd[2] = (unsigned char) (block >> 24) & 0xff;
cmd[3] = (unsigned char) (block >> 16) & 0xff;
cmd[4] = (unsigned char) (block >> 8) & 0xff;
cmd[5] = (unsigned char) block & 0xff;
cmd[6] = cmd[9] = 0;
cmd[7] = (unsigned char) (realcount >> 8) & 0xff;
cmd[8] = (unsigned char) realcount & 0xff;
}
else
{
if (realcount > 0xff)
{
realcount = 0xff;
this_count = realcount * (scsi_CDs[dev].sector_size >> 9);
}
cmd[1] |= (unsigned char) ((block >> 16) & 0x1f);
cmd[2] = (unsigned char) ((block >> 8) & 0xff);
cmd[3] = (unsigned char) block & 0xff;
cmd[4] = (unsigned char) realcount;
cmd[5] = 0;
}
#ifdef DEBUG
{
int i;
printk("ReadCD: %d %d %d %d\n",block, realcount, buffer, this_count);
printk("Use sg: %d\n", SCpnt->use_sg);
printk("Dumping command: ");
for(i=0; i<12; i++) printk("%2.2x ", cmd[i]);
printk("\n");
};
#endif
SCpnt->transfersize = (scsi_CDs[dev].sector_size > 1024) ?
1024 : scsi_CDs[dev].sector_size;
SCpnt->this_count = this_count;
scsi_do_cmd (SCpnt, (void *) cmd, buffer,
realcount * scsi_CDs[dev].sector_size,
rw_intr, SR_TIMEOUT, MAX_RETRIES);
}
static int sr_detect(Scsi_Device * SDp){
if(SDp->type != TYPE_ROM && SDp->type != TYPE_WORM) return 0;
#ifdef MACH
printk("Detected scsi CD-ROM cd%d at scsi%d, channel %d, id %d, lun %d\n",
#else
printk("Detected scsi CD-ROM sr%d at scsi%d, channel %d, id %d, lun %d\n",
#endif
sr_template.dev_noticed++,
SDp->host->host_no, SDp->channel, SDp->id, SDp->lun);
return 1;
}
static int sr_attach(Scsi_Device * SDp){
Scsi_CD * cpnt;
int i;
if(SDp->type != TYPE_ROM && SDp->type != TYPE_WORM) return 1;
if (sr_template.nr_dev >= sr_template.dev_max)
{
SDp->attached--;
return 1;
}
for(cpnt = scsi_CDs, i=0; i<sr_template.dev_max; i++, cpnt++)
if(!cpnt->device) break;
if(i >= sr_template.dev_max) panic ("scsi_devices corrupt (sr)");
SDp->scsi_request_fn = do_sr_request;
scsi_CDs[i].device = SDp;
sr_template.nr_dev++;
if(sr_template.nr_dev > sr_template.dev_max)
panic ("scsi_devices corrupt (sr)");
return 0;
}
static void sr_init_done (Scsi_Cmnd * SCpnt)
{
struct request * req;
req = &SCpnt->request;
req->rq_status = RQ_SCSI_DONE;
if (req->sem != NULL) {
up(req->sem);
}
}
void get_sectorsize(int i){
unsigned char cmd[12];
unsigned char *buffer;
int the_result, retries;
Scsi_Cmnd * SCpnt;
buffer = (unsigned char *) scsi_malloc(512);
SCpnt = allocate_device(NULL, scsi_CDs[i].device, 1);
retries = 3;
do {
cmd[0] = READ_CAPACITY;
cmd[1] = (scsi_CDs[i].device->lun << 5) & 0xe0;
memset ((void *) &cmd[2], 0, 8);
SCpnt->request.rq_status = RQ_SCSI_BUSY;
SCpnt->cmd_len = 0;
memset(buffer, 0, 8);
{
struct semaphore sem = MUTEX_LOCKED;
SCpnt->request.sem = &sem;
scsi_do_cmd (SCpnt,
(void *) cmd, (void *) buffer,
512, sr_init_done,  SR_TIMEOUT,
MAX_RETRIES);
down(&sem);
}
the_result = SCpnt->result;
retries--;
} while(the_result && retries);
SCpnt->request.rq_status = RQ_INACTIVE;
wake_up(&SCpnt->device->device_wait);
if (the_result) {
scsi_CDs[i].capacity = 0x1fffff;
scsi_CDs[i].sector_size = 2048;
scsi_CDs[i].needs_sector_size = 1;
} else {
scsi_CDs[i].capacity = 1 + ((buffer[0] << 24) |
(buffer[1] << 16) |
(buffer[2] << 8) |
buffer[3]);
scsi_CDs[i].sector_size = (buffer[4] << 24) |
(buffer[5] << 16) | (buffer[6] << 8) | buffer[7];
switch (scsi_CDs[i].sector_size) {
case 0: case 2340: case 2352:
scsi_CDs[i].sector_size = 2048;
case 2048:
scsi_CDs[i].capacity *= 4;
case 512:
break;
default:
#ifdef MACH
printk ("cd%d : unsupported sector size %d.\n",
i, scsi_CDs[i].sector_size);
#else
printk ("scd%d : unsupported sector size %d.\n",
i, scsi_CDs[i].sector_size);
#endif
scsi_CDs[i].capacity = 0;
scsi_CDs[i].needs_sector_size = 1;
}
scsi_CDs[i].needs_sector_size = 0;
sr_sizes[i] = scsi_CDs[i].capacity >> (BLOCK_SIZE_BITS - 9);
};
scsi_free(buffer, 512);
}
static int sr_registered = 0;
static int sr_init()
{
int i;
if(sr_template.dev_noticed == 0) return 0;
if(!sr_registered) {
if (register_blkdev(MAJOR_NR,"sr",&sr_fops)) {
printk("Unable to get major %d for SCSI-CD\n",MAJOR_NR);
return 1;
}
sr_registered++;
}
if (scsi_CDs) return 0;
sr_template.dev_max = sr_template.dev_noticed + SR_EXTRA_DEVS;
scsi_CDs = (Scsi_CD *) scsi_init_malloc(sr_template.dev_max * sizeof(Scsi_CD), GFP_ATOMIC);
memset(scsi_CDs, 0, sr_template.dev_max * sizeof(Scsi_CD));
sr_sizes = (int *) scsi_init_malloc(sr_template.dev_max * sizeof(int), GFP_ATOMIC);
memset(sr_sizes, 0, sr_template.dev_max * sizeof(int));
sr_blocksizes = (int *) scsi_init_malloc(sr_template.dev_max *
sizeof(int), GFP_ATOMIC);
for(i=0;i<sr_template.dev_max;i++) sr_blocksizes[i] = 2048;
blksize_size[MAJOR_NR] = sr_blocksizes;
return 0;
}
void sr_finish()
{
int i;
blk_dev[MAJOR_NR].request_fn = DEVICE_REQUEST;
blk_size[MAJOR_NR] = sr_sizes;
for (i = 0; i < sr_template.nr_dev; ++i)
{
if (scsi_CDs[i].capacity) continue;
scsi_CDs[i].capacity = 0x1fffff;
scsi_CDs[i].sector_size = 2048;
scsi_CDs[i].needs_sector_size = 1;
#if 0
get_sectorsize(i);
printk("Scd sectorsize = %d bytes.\n", scsi_CDs[i].sector_size);
#endif
scsi_CDs[i].use = 1;
scsi_CDs[i].ten = 1;
scsi_CDs[i].remap = 1;
scsi_CDs[i].auto_eject = 0;
sr_sizes[i] = scsi_CDs[i].capacity >> (BLOCK_SIZE_BITS - 9);
}
if(scsi_CDs[0].device && scsi_CDs[0].device->host->sg_tablesize)
read_ahead[MAJOR_NR] = 32;
else
read_ahead[MAJOR_NR] = 4;
return;
}
static void sr_detach(Scsi_Device * SDp)
{
Scsi_CD * cpnt;
int i;
for(cpnt = scsi_CDs, i=0; i<sr_template.dev_max; i++, cpnt++)
if(cpnt->device == SDp) {
kdev_t devi = MKDEV(MAJOR_NR, i);
invalidate_inodes(devi);
invalidate_buffers(devi);
cpnt->device = NULL;
cpnt->capacity = 0;
SDp->attached--;
sr_template.nr_dev--;
sr_template.dev_noticed--;
sr_sizes[i] = 0;
return;
}
return;
}
#ifdef MODULE
int init_module(void) {
sr_template.usage_count = &mod_use_count_;
return scsi_register_module(MODULE_SCSI_DEV, &sr_template);
}
void cleanup_module( void)
{
scsi_unregister_module(MODULE_SCSI_DEV, &sr_template);
unregister_blkdev(SCSI_CDROM_MAJOR, "sr");
sr_registered--;
if(scsi_CDs != NULL) {
scsi_init_free((char *) scsi_CDs,
(sr_template.dev_noticed + SR_EXTRA_DEVS)
* sizeof(Scsi_CD));
scsi_init_free((char *) sr_sizes, sr_template.dev_max * sizeof(int));
scsi_init_free((char *) sr_blocksizes, sr_template.dev_max * sizeof(int));
}
blksize_size[MAJOR_NR] = NULL;
blk_dev[MAJOR_NR].request_fn = NULL;
blk_size[MAJOR_NR] = NULL;
read_ahead[MAJOR_NR] = 0;
sr_template.dev_max = 0;
}
#endif