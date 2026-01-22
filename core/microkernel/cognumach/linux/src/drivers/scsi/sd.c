#include <linux/module.h>
#ifdef MODULE
#define MODULE_FLAG 1
#else
#define MODULE_FLAG scsi_loadable_module_flag
#endif
#include <linux/fs.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/mm.h>
#include <linux/string.h>
#include <linux/errno.h>
#include <linux/interrupt.h>
#include <asm/system.h>
#define MAJOR_NR SCSI_DISK_MAJOR
#include <linux/blk.h>
#include "scsi.h"
#include "hosts.h"
#include "sd.h"
#include <scsi/scsi_ioctl.h>
#include "constants.h"
#include <linux/genhd.h>
#define MAX_RETRIES 5
#define SD_TIMEOUT (20 * HZ)
#define SD_MOD_TIMEOUT (25 * HZ)
#define CLUSTERABLE_DEVICE(SC) (SC->host->use_clustering && \
SC->device->type != TYPE_MOD)
struct hd_struct * sd;
Scsi_Disk * rscsi_disks = NULL;
static int * sd_sizes;
static int * sd_blocksizes;
static int * sd_hardsizes;
extern int sd_ioctl(struct inode *, struct file *, unsigned int, unsigned long);
static int check_scsidisk_media_change(kdev_t);
static int fop_revalidate_scsidisk(kdev_t);
static int sd_init_onedisk(int);
static void requeue_sd_request (Scsi_Cmnd * SCpnt);
static int sd_init(void);
static void sd_finish(void);
static int sd_attach(Scsi_Device *);
static int sd_detect(Scsi_Device *);
static void sd_detach(Scsi_Device *);
struct Scsi_Device_Template sd_template =
{ NULL, "disk", "sd", NULL, TYPE_DISK,
SCSI_DISK_MAJOR, 0, 0, 0, 1,
sd_detect, sd_init,
sd_finish, sd_attach, sd_detach
};
static int sd_open(struct inode * inode, struct file * filp)
{
int target;
target =  DEVICE_NR(inode->i_rdev);
if(target >= sd_template.dev_max || !rscsi_disks[target].device)
return -ENXIO;
while (rscsi_disks[target].device->busy)
barrier();
if(rscsi_disks[target].device->removable) {
check_disk_change(inode->i_rdev);
if ( !rscsi_disks[target].ready )
return -ENXIO;
if ( (rscsi_disks[target].write_prot) && (filp->f_mode & 2) )
return -EROFS;
}
if(sd_sizes[MINOR(inode->i_rdev)] == 0)
return -ENXIO;
if(rscsi_disks[target].device->removable)
if(!rscsi_disks[target].device->access_count)
sd_ioctl(inode, NULL, SCSI_IOCTL_DOORLOCK, 0);
rscsi_disks[target].device->access_count++;
if (rscsi_disks[target].device->host->hostt->usage_count)
(*rscsi_disks[target].device->host->hostt->usage_count)++;
if(sd_template.usage_count) (*sd_template.usage_count)++;
return 0;
}
static void sd_release(struct inode * inode, struct file * file)
{
int target;
fsync_dev(inode->i_rdev);
target =  DEVICE_NR(inode->i_rdev);
rscsi_disks[target].device->access_count--;
if (rscsi_disks[target].device->host->hostt->usage_count)
(*rscsi_disks[target].device->host->hostt->usage_count)--;
if(sd_template.usage_count) (*sd_template.usage_count)--;
if(rscsi_disks[target].device->removable) {
if(!rscsi_disks[target].device->access_count)
sd_ioctl(inode, NULL, SCSI_IOCTL_DOORUNLOCK, 0);
}
}
static void sd_geninit(struct gendisk *);
static struct file_operations sd_fops = {
NULL,
block_read,
block_write,
NULL,
NULL,
sd_ioctl,
NULL,
sd_open,
sd_release,
block_fsync,
NULL,
check_scsidisk_media_change,
fop_revalidate_scsidisk
};
static struct gendisk sd_gendisk = {
MAJOR_NR,
"sd",
4,
1 << 4,
0,
sd_geninit,
NULL,
NULL,
0,
NULL,
NULL
};
static void sd_geninit (struct gendisk *ignored)
{
int i;
for (i = 0; i < sd_template.dev_max; ++i)
if(rscsi_disks[i].device)
sd[i << 4].nr_sects = rscsi_disks[i].capacity;
#if 0
sd_gendisk.nr_real = sd_template.dev_max;
#endif
}
static void rw_intr (Scsi_Cmnd *SCpnt)
{
int result = SCpnt->result;
int this_count = SCpnt->bufflen >> 9;
int good_sectors = (result == 0 ? this_count : 0);
int block_sectors = 1;
#ifdef DEBUG
printk("sd%c : rw_intr(%d, %d)\n", 'a' + MINOR(SCpnt->request.rq_dev),
SCpnt->host->host_no, result);
#endif
if (driver_byte(result) != 0 &&
SCpnt->sense_buffer[0] == 0xF0 &&
SCpnt->sense_buffer[2] == MEDIUM_ERROR)
{
long error_sector = (SCpnt->sense_buffer[3] << 24) |
(SCpnt->sense_buffer[4] << 16) |
(SCpnt->sense_buffer[5] << 8) |
SCpnt->sense_buffer[6];
int sector_size =
rscsi_disks[DEVICE_NR(SCpnt->request.rq_dev)].sector_size;
if (SCpnt->request.bh != NULL)
block_sectors = SCpnt->request.bh->b_size >> 9;
if (sector_size == 1024)
{
error_sector <<= 1;
if (block_sectors < 2) block_sectors = 2;
}
else if (sector_size == 256)
error_sector >>= 1;
error_sector -= sd[MINOR(SCpnt->request.rq_dev)].start_sect;
error_sector &= ~ (block_sectors - 1);
good_sectors = error_sector - SCpnt->request.sector;
if (good_sectors < 0 || good_sectors >= this_count)
good_sectors = 0;
}
if (SCpnt->sense_buffer[0] == 0xF0 &&
SCpnt->sense_buffer[2] == RECOVERED_ERROR)
{
printk("scsidisk recovered I/O error: dev %s, sector %lu, absolute sector %lu\n",
kdevname(SCpnt->request.rq_dev), SCpnt->request.sector,
SCpnt->request.sector + sd[MINOR(SCpnt->request.rq_dev)].start_sect);
good_sectors = this_count;
result = 0;
}
if (good_sectors > 0) {
#ifdef DEBUG
printk("sd%c : %d sectors remain.\n", 'a' + MINOR(SCpnt->request.rq_dev),
SCpnt->request.nr_sectors);
printk("use_sg is %d\n ",SCpnt->use_sg);
#endif
if (SCpnt->use_sg) {
struct scatterlist * sgpnt;
int i;
sgpnt = (struct scatterlist *) SCpnt->buffer;
for(i=0; i<SCpnt->use_sg; i++) {
#ifdef DEBUG
printk(":%x %x %d\n",sgpnt[i].alt_address, sgpnt[i].address,
sgpnt[i].length);
#endif
if (sgpnt[i].alt_address) {
if (SCpnt->request.cmd == READ)
memcpy(sgpnt[i].alt_address, sgpnt[i].address,
sgpnt[i].length);
scsi_free(sgpnt[i].address, sgpnt[i].length);
}
}
scsi_free(SCpnt->buffer, SCpnt->sglist_len);
} else {
if (SCpnt->buffer != SCpnt->request.buffer) {
#ifdef DEBUG
printk("nosg: %x %x %d\n",SCpnt->request.buffer, SCpnt->buffer,
SCpnt->bufflen);
#endif
if (SCpnt->request.cmd == READ)
memcpy(SCpnt->request.buffer, SCpnt->buffer,
SCpnt->bufflen);
scsi_free(SCpnt->buffer, SCpnt->bufflen);
}
}
if (SCpnt->request.nr_sectors > this_count)
{
SCpnt->request.errors = 0;
if (!SCpnt->request.bh)
{
#ifdef DEBUG
printk("sd%c : handling page request, no buffer\n",
'a' + MINOR(SCpnt->request.rq_dev));
#endif
panic("sd.c: linked page request (%lx %x)",
SCpnt->request.sector, this_count);
}
}
SCpnt = end_scsi_request(SCpnt, 1, good_sectors);
if (result == 0)
{
requeue_sd_request(SCpnt);
return;
}
}
if (good_sectors == 0) {
if (SCpnt->use_sg) {
struct scatterlist * sgpnt;
int i;
sgpnt = (struct scatterlist *) SCpnt->buffer;
for(i=0; i<SCpnt->use_sg; i++) {
#ifdef DEBUG
printk("err: %x %x %d\n",SCpnt->request.buffer, SCpnt->buffer,
SCpnt->bufflen);
#endif
if (sgpnt[i].alt_address) {
scsi_free(sgpnt[i].address, sgpnt[i].length);
}
}
scsi_free(SCpnt->buffer, SCpnt->sglist_len);
} else {
#ifdef DEBUG
printk("nosgerr: %x %x %d\n",SCpnt->request.buffer, SCpnt->buffer,
SCpnt->bufflen);
#endif
if (SCpnt->buffer != SCpnt->request.buffer)
scsi_free(SCpnt->buffer, SCpnt->bufflen);
}
}
if (driver_byte(result) != 0) {
if (suggestion(result) == SUGGEST_REMAP) {
#ifdef REMAP
if rscsi_disks[DEVICE_NR(SCpnt->request.rq_dev)].remap
{
result = 0;
}
else
#endif
}
if ((SCpnt->sense_buffer[0] & 0x7f) == 0x70) {
if ((SCpnt->sense_buffer[2] & 0xf) == UNIT_ATTENTION) {
if(rscsi_disks[DEVICE_NR(SCpnt->request.rq_dev)].device->removable) {
rscsi_disks[DEVICE_NR(SCpnt->request.rq_dev)].device->changed = 1;
SCpnt = end_scsi_request(SCpnt, 0, this_count);
requeue_sd_request(SCpnt);
return;
}
else
{
requeue_sd_request(SCpnt);
return;
}
}
}
if (SCpnt->sense_buffer[2] == ILLEGAL_REQUEST) {
if (rscsi_disks[DEVICE_NR(SCpnt->request.rq_dev)].ten) {
rscsi_disks[DEVICE_NR(SCpnt->request.rq_dev)].ten = 0;
requeue_sd_request(SCpnt);
result = 0;
} else {
}
}
if (SCpnt->sense_buffer[2] == MEDIUM_ERROR) {
printk("scsi%d: MEDIUM ERROR on channel %d, id %d, lun %d, CDB: ",
SCpnt->host->host_no, (int) SCpnt->channel,
(int) SCpnt->target, (int) SCpnt->lun);
print_command(SCpnt->cmnd);
print_sense("sd", SCpnt);
SCpnt = end_scsi_request(SCpnt, 0, block_sectors);
requeue_sd_request(SCpnt);
return;
}
}
if (result) {
printk("SCSI disk error : host %d channel %d id %d lun %d return code = %x\n",
rscsi_disks[DEVICE_NR(SCpnt->request.rq_dev)].device->host->host_no,
rscsi_disks[DEVICE_NR(SCpnt->request.rq_dev)].device->channel,
rscsi_disks[DEVICE_NR(SCpnt->request.rq_dev)].device->id,
rscsi_disks[DEVICE_NR(SCpnt->request.rq_dev)].device->lun, result);
if (driver_byte(result) & DRIVER_SENSE)
print_sense("sd", SCpnt);
SCpnt = end_scsi_request(SCpnt, 0, SCpnt->request.current_nr_sectors);
requeue_sd_request(SCpnt);
return;
}
}
static void do_sd_request (void)
{
Scsi_Cmnd * SCpnt = NULL;
Scsi_Device * SDev;
struct request * req = NULL;
unsigned long flags;
int flag = 0;
save_flags(flags);
while (1==1){
cli();
if (CURRENT != NULL && CURRENT->rq_status == RQ_INACTIVE) {
restore_flags(flags);
return;
}
INIT_SCSI_REQUEST;
SDev = rscsi_disks[DEVICE_NR(CURRENT->rq_dev)].device;
if( SDev->was_reset )
{
if( SDev->removable && !intr_count )
{
scsi_ioctl(SDev, SCSI_IOCTL_DOORLOCK, 0);
SDev->was_reset = 0;
continue;
}
SDev->was_reset = 0;
}
if (flag++ == 0)
SCpnt = allocate_device(&CURRENT,
rscsi_disks[DEVICE_NR(CURRENT->rq_dev)].device, 0);
else SCpnt = NULL;
restore_flags(flags);
if (!SCpnt && sd_template.nr_dev > 1){
struct request *req1;
req1 = NULL;
cli();
req = CURRENT;
while(req){
SCpnt = request_queueable(req,
rscsi_disks[DEVICE_NR(req->rq_dev)].device);
if(SCpnt) break;
req1 = req;
req = req->next;
}
if (SCpnt && req->rq_status == RQ_INACTIVE) {
if (req == CURRENT)
CURRENT = CURRENT->next;
else
req1->next = req->next;
}
restore_flags(flags);
}
if (!SCpnt) return;
requeue_sd_request(SCpnt);
}
}
static void requeue_sd_request (Scsi_Cmnd * SCpnt)
{
int dev, devm, block, this_count;
unsigned char cmd[12];
int bounce_size, contiguous;
int max_sg;
struct buffer_head * bh, *bhp;
char * buff, *bounce_buffer;
repeat:
if(!SCpnt || SCpnt->request.rq_status == RQ_INACTIVE) {
do_sd_request();
return;
}
devm =  MINOR(SCpnt->request.rq_dev);
dev = DEVICE_NR(SCpnt->request.rq_dev);
block = SCpnt->request.sector;
this_count = 0;
#ifdef DEBUG
printk("Doing sd request, dev = %d, block = %d\n", devm, block);
#endif
if (devm >= (sd_template.dev_max << 4) ||
!rscsi_disks[dev].device ||
block + SCpnt->request.nr_sectors > sd[devm].nr_sects)
{
SCpnt = end_scsi_request(SCpnt, 0, SCpnt->request.nr_sectors);
goto repeat;
}
block += sd[devm].start_sect;
if (rscsi_disks[dev].device->changed)
{
SCpnt = end_scsi_request(SCpnt, 0, SCpnt->request.nr_sectors);
goto repeat;
}
#ifdef DEBUG
printk("sd%c : real dev = /dev/sd%c, block = %d\n",
'a' + devm, dev, block);
#endif
if (rscsi_disks[dev].sector_size == 1024)
if((block & 1) || (SCpnt->request.nr_sectors & 1)) {
printk("sd.c:Bad block number requested");
SCpnt = end_scsi_request(SCpnt, 0, SCpnt->request.nr_sectors);
goto repeat;
}
switch (SCpnt->request.cmd)
{
case WRITE :
if (!rscsi_disks[dev].device->writeable)
{
SCpnt = end_scsi_request(SCpnt, 0, SCpnt->request.nr_sectors);
goto repeat;
}
cmd[0] = WRITE_6;
break;
case READ :
cmd[0] = READ_6;
break;
default :
panic ("Unknown sd command %d\n", SCpnt->request.cmd);
}
SCpnt->this_count = 0;
contiguous = (!CLUSTERABLE_DEVICE(SCpnt) ? 0 :1);
bounce_buffer = NULL;
bounce_size = (SCpnt->request.nr_sectors << 9);
if (contiguous && SCpnt->request.bh &&
((long) SCpnt->request.bh->b_data)
+ (SCpnt->request.nr_sectors << 9) - 1 > ISA_DMA_THRESHOLD
&& SCpnt->host->unchecked_isa_dma) {
if(((long) SCpnt->request.bh->b_data) > ISA_DMA_THRESHOLD)
bounce_buffer = (char *) scsi_malloc(bounce_size);
if(!bounce_buffer) contiguous = 0;
}
if(contiguous && SCpnt->request.bh && SCpnt->request.bh->b_reqnext)
for(bh = SCpnt->request.bh, bhp = bh->b_reqnext; bhp; bh = bhp,
bhp = bhp->b_reqnext) {
if(!CONTIGUOUS_BUFFERS(bh,bhp)) {
if(bounce_buffer) scsi_free(bounce_buffer, bounce_size);
contiguous = 0;
break;
}
}
if (!SCpnt->request.bh || contiguous) {
this_count = SCpnt->request.nr_sectors;
buff = SCpnt->request.buffer;
SCpnt->use_sg = 0;
} else if (SCpnt->host->sg_tablesize == 0 ||
(need_isa_buffer && dma_free_sectors <= 10)) {
if (SCpnt->host->sg_tablesize != 0 &&
need_isa_buffer &&
dma_free_sectors <= 10)
printk("Warning: SCSI DMA buffer space running low.  Using non scatter-gather I/O.\n");
this_count = SCpnt->request.current_nr_sectors;
buff = SCpnt->request.buffer;
SCpnt->use_sg = 0;
} else {
struct scatterlist * sgpnt;
int count, this_count_max;
int counted;
bh = SCpnt->request.bh;
this_count = 0;
this_count_max = (rscsi_disks[dev].ten ? 0xffff : 0xff);
count = 0;
bhp = NULL;
while(bh) {
if ((this_count + (bh->b_size >> 9)) > this_count_max) break;
if(!bhp || !CONTIGUOUS_BUFFERS(bhp,bh) ||
!CLUSTERABLE_DEVICE(SCpnt) ||
(SCpnt->host->unchecked_isa_dma &&
((unsigned long) bh->b_data-1) == ISA_DMA_THRESHOLD)) {
if (count < SCpnt->host->sg_tablesize) count++;
else break;
}
this_count += (bh->b_size >> 9);
bhp = bh;
bh = bh->b_reqnext;
}
#if 0
if(SCpnt->host->unchecked_isa_dma &&
((unsigned int) SCpnt->request.bh->b_data-1) == ISA_DMA_THRESHOLD) count--;
#endif
SCpnt->use_sg = count;
count  = (SCpnt->use_sg * sizeof(struct scatterlist) + 511) & ~511;
SCpnt->sglist_len = count;
max_sg = count / sizeof(struct scatterlist);
if(SCpnt->host->sg_tablesize < max_sg)
max_sg = SCpnt->host->sg_tablesize;
sgpnt = (struct scatterlist * ) scsi_malloc(count);
if (!sgpnt) {
printk("Warning - running *really* short on DMA buffers\n");
SCpnt->use_sg = 0;
this_count = SCpnt->request.current_nr_sectors;
buff = SCpnt->request.buffer;
} else {
memset(sgpnt, 0, count);
buff = (char *) sgpnt;
counted = 0;
for(count = 0, bh = SCpnt->request.bh, bhp = bh->b_reqnext;
count < SCpnt->use_sg && bh;
count++, bh = bhp) {
bhp = bh->b_reqnext;
if(!sgpnt[count].address) sgpnt[count].address = bh->b_data;
sgpnt[count].length += bh->b_size;
counted += bh->b_size >> 9;
if (((long) sgpnt[count].address) + sgpnt[count].length - 1 >
ISA_DMA_THRESHOLD && (SCpnt->host->unchecked_isa_dma) &&
!sgpnt[count].alt_address) {
sgpnt[count].alt_address = sgpnt[count].address;
if(dma_free_sectors < (sgpnt[count].length >> 9) + 10) {
sgpnt[count].address = NULL;
} else {
sgpnt[count].address =
(char *) scsi_malloc(sgpnt[count].length);
}
if(sgpnt[count].address == NULL){
#if 0
printk("Warning: Running low on SCSI DMA buffers");
while(--count >= 0){
if(sgpnt[count].alt_address)
scsi_free(sgpnt[count].address,
sgpnt[count].length);
}
this_count = SCpnt->request.current_nr_sectors;
buff = SCpnt->request.buffer;
SCpnt->use_sg = 0;
scsi_free(sgpnt, SCpnt->sglist_len);
#endif
SCpnt->use_sg = count;
this_count = counted -= bh->b_size >> 9;
break;
}
}
if(bhp && CONTIGUOUS_BUFFERS(bh,bhp)
&& CLUSTERABLE_DEVICE(SCpnt)) {
char * tmp;
if (((long) sgpnt[count].address) + sgpnt[count].length +
bhp->b_size - 1 > ISA_DMA_THRESHOLD &&
(SCpnt->host->unchecked_isa_dma) &&
!sgpnt[count].alt_address) continue;
if(!sgpnt[count].alt_address) {count--; continue; }
if(dma_free_sectors > 10)
tmp = (char *) scsi_malloc(sgpnt[count].length
+ bhp->b_size);
else {
tmp = NULL;
max_sg = SCpnt->use_sg;
}
if(tmp){
scsi_free(sgpnt[count].address, sgpnt[count].length);
sgpnt[count].address = tmp;
count--;
continue;
}
if (SCpnt->use_sg < max_sg) SCpnt->use_sg++;
}
}
this_count = counted;
if(count < SCpnt->use_sg || SCpnt->use_sg
> SCpnt->host->sg_tablesize){
bh = SCpnt->request.bh;
printk("Use sg, count %d %x %d\n",
SCpnt->use_sg, count, dma_free_sectors);
printk("maxsg = %x, counted = %d this_count = %d\n",
max_sg, counted, this_count);
while(bh){
printk("[%p %lx] ", bh->b_data, bh->b_size);
bh = bh->b_reqnext;
}
if(SCpnt->use_sg < 16)
for(count=0; count<SCpnt->use_sg; count++)
printk("{%d:%p %p %d}  ", count,
sgpnt[count].address,
sgpnt[count].alt_address,
sgpnt[count].length);
panic("Ooops");
}
if (SCpnt->request.cmd == WRITE)
for(count=0; count<SCpnt->use_sg; count++)
if(sgpnt[count].alt_address)
memcpy(sgpnt[count].address, sgpnt[count].alt_address,
sgpnt[count].length);
}
}
if(SCpnt->use_sg == 0){
if (((long) buff) + (this_count << 9) - 1 > ISA_DMA_THRESHOLD &&
(SCpnt->host->unchecked_isa_dma)) {
if(bounce_buffer)
buff = bounce_buffer;
else
buff = (char *) scsi_malloc(this_count << 9);
if(buff == NULL) {
this_count = SCpnt->request.current_nr_sectors;
buff = (char *) scsi_malloc(this_count << 9);
if(!buff) panic("Ran out of DMA buffers.");
}
if (SCpnt->request.cmd == WRITE)
memcpy(buff, (char *)SCpnt->request.buffer, this_count << 9);
}
}
#ifdef DEBUG
printk("sd%c : %s %d/%d 512 byte blocks.\n",
'a' + devm,
(SCpnt->request.cmd == WRITE) ? "writing" : "reading",
this_count, SCpnt->request.nr_sectors);
#endif
cmd[1] = (SCpnt->lun << 5) & 0xe0;
if (rscsi_disks[dev].sector_size == 1024){
if(block & 1) panic("sd.c:Bad block number requested");
if(this_count & 1) panic("sd.c:Bad block number requested");
block = block >> 1;
this_count = this_count >> 1;
}
if (rscsi_disks[dev].sector_size == 256){
block = block << 1;
this_count = this_count << 1;
}
if (((this_count > 0xff) ||  (block > 0x1fffff)) && rscsi_disks[dev].ten)
{
if (this_count > 0xffff)
this_count = 0xffff;
cmd[0] += READ_10 - READ_6 ;
cmd[2] = (unsigned char) (block >> 24) & 0xff;
cmd[3] = (unsigned char) (block >> 16) & 0xff;
cmd[4] = (unsigned char) (block >> 8) & 0xff;
cmd[5] = (unsigned char) block & 0xff;
cmd[6] = cmd[9] = 0;
cmd[7] = (unsigned char) (this_count >> 8) & 0xff;
cmd[8] = (unsigned char) this_count & 0xff;
}
else
{
if (this_count > 0xff)
this_count = 0xff;
cmd[1] |= (unsigned char) ((block >> 16) & 0x1f);
cmd[2] = (unsigned char) ((block >> 8) & 0xff);
cmd[3] = (unsigned char) block & 0xff;
cmd[4] = (unsigned char) this_count;
cmd[5] = 0;
}
SCpnt->transfersize = rscsi_disks[dev].sector_size;
SCpnt->underflow = this_count << 9;
scsi_do_cmd (SCpnt, (void *) cmd, buff,
this_count * rscsi_disks[dev].sector_size,
rw_intr,
(SCpnt->device->type == TYPE_DISK ?
SD_TIMEOUT : SD_MOD_TIMEOUT),
MAX_RETRIES);
}
static int check_scsidisk_media_change(kdev_t full_dev){
int retval;
int target;
struct inode inode;
int flag = 0;
target =  DEVICE_NR(full_dev);
if (target >= sd_template.dev_max ||
!rscsi_disks[target].device) {
printk("SCSI disk request error: invalid device.\n");
return 0;
}
if(!rscsi_disks[target].device->removable) return 0;
inode.i_rdev = full_dev;
retval = sd_ioctl(&inode, NULL, SCSI_IOCTL_START_UNIT, 0);
if(retval){
rscsi_disks[target].ready = 0;
rscsi_disks[target].device->changed = 1;
return 1;
}
rscsi_disks[target].ready = 1;
retval = rscsi_disks[target].device->changed;
if(!flag) rscsi_disks[target].device->changed = 0;
return retval;
}
static void sd_init_done (Scsi_Cmnd * SCpnt)
{
struct request * req;
req = &SCpnt->request;
req->rq_status = RQ_SCSI_DONE;
if (req->sem != NULL) {
up(req->sem);
}
}
static int sd_init_onedisk(int i)
{
unsigned char cmd[12];
unsigned char *buffer;
unsigned long spintime;
int the_result, retries;
Scsi_Cmnd * SCpnt;
SCpnt = allocate_device(NULL, rscsi_disks[i].device, 1);
buffer = (unsigned char *) scsi_malloc(512);
spintime = 0;
do{
retries = 0;
while(retries < 3)
{
cmd[0] = TEST_UNIT_READY;
cmd[1] = (rscsi_disks[i].device->lun << 5) & 0xe0;
memset ((void *) &cmd[2], 0, 8);
SCpnt->cmd_len = 0;
SCpnt->sense_buffer[0] = 0;
SCpnt->sense_buffer[2] = 0;
{
struct semaphore sem = MUTEX_LOCKED;
SCpnt->request.rq_status = RQ_SCSI_BUSY;
SCpnt->request.sem = &sem;
scsi_do_cmd (SCpnt,
(void *) cmd, (void *) buffer,
512, sd_init_done,  SD_TIMEOUT,
MAX_RETRIES);
down(&sem);
}
the_result = SCpnt->result;
retries++;
if(   the_result == 0
|| SCpnt->sense_buffer[2] != UNIT_ATTENTION)
break;
}
if(the_result && !rscsi_disks[i].device->removable &&
SCpnt->sense_buffer[2] == NOT_READY) {
unsigned long time1;
if(!spintime){
#ifdef MACH
printk( "sd%d: Spinning up disk...", i);
#else
printk( "sd%c: Spinning up disk...", 'a' + i );
#endif
cmd[0] = START_STOP;
cmd[1] = (rscsi_disks[i].device->lun << 5) & 0xe0;
cmd[1] |= 1;
memset ((void *) &cmd[2], 0, 8);
cmd[4] = 1;
SCpnt->cmd_len = 0;
SCpnt->sense_buffer[0] = 0;
SCpnt->sense_buffer[2] = 0;
{
struct semaphore sem = MUTEX_LOCKED;
SCpnt->request.rq_status = RQ_SCSI_BUSY;
SCpnt->request.sem = &sem;
scsi_do_cmd (SCpnt,
(void *) cmd, (void *) buffer,
512, sd_init_done,  SD_TIMEOUT,
MAX_RETRIES);
down(&sem);
}
spintime = jiffies;
}
time1 = jiffies + HZ;
while(jiffies < time1);
printk( "." );
}
} while(the_result && spintime && spintime+100*HZ > jiffies);
if (spintime) {
if (the_result)
printk( "not responding...\n" );
else
printk( "ready\n" );
}
retries = 3;
do {
cmd[0] = READ_CAPACITY;
cmd[1] = (rscsi_disks[i].device->lun << 5) & 0xe0;
memset ((void *) &cmd[2], 0, 8);
memset ((void *) buffer, 0, 8);
SCpnt->cmd_len = 0;
SCpnt->sense_buffer[0] = 0;
SCpnt->sense_buffer[2] = 0;
{
struct semaphore sem = MUTEX_LOCKED;
SCpnt->request.rq_status = RQ_SCSI_BUSY;
SCpnt->request.sem = &sem;
scsi_do_cmd (SCpnt,
(void *) cmd, (void *) buffer,
8, sd_init_done,  SD_TIMEOUT,
MAX_RETRIES);
down(&sem);
}
the_result = SCpnt->result;
retries--;
} while(the_result && retries);
SCpnt->request.rq_status = RQ_INACTIVE;
wake_up(&SCpnt->device->device_wait);
if (the_result)
{
#ifdef MACH
printk ("sd%d : READ CAPACITY failed.\n"
"sd%d : status = %x, message = %02x, host = %d, driver = %02x \n",
i, i,
#else
printk ("sd%c : READ CAPACITY failed.\n"
"sd%c : status = %x, message = %02x, host = %d, driver = %02x \n",
'a' + i, 'a' + i,
#endif
status_byte(the_result),
msg_byte(the_result),
host_byte(the_result),
driver_byte(the_result)
);
if (driver_byte(the_result)  & DRIVER_SENSE)
#ifdef MACH
printk("sd%d : extended sense code = %1x \n",
i, SCpnt->sense_buffer[2] & 0xf);
#else
printk("sd%c : extended sense code = %1x \n",
'a' + i, SCpnt->sense_buffer[2] & 0xf);
#endif
else
#ifdef MACH
printk("sd%d : sense not available. \n", i);
#else
printk("sd%c : sense not available. \n", 'a' + i);
#endif
#ifdef MACH
printk("sd%d : block size assumed to be 512 bytes, disk size 1GB.  \n",
i);
#else
printk("sd%c : block size assumed to be 512 bytes, disk size 1GB.  \n",
'a' + i);
#endif
rscsi_disks[i].capacity = 0x1fffff;
rscsi_disks[i].sector_size = 512;
if(rscsi_disks[i].device->removable &&
SCpnt->sense_buffer[2] == NOT_READY)
rscsi_disks[i].device->changed = 1;
}
else
{
rscsi_disks[i].ready = 1;
rscsi_disks[i].capacity = 1 + ((buffer[0] << 24) |
(buffer[1] << 16) |
(buffer[2] << 8) |
buffer[3]);
rscsi_disks[i].sector_size = (buffer[4] << 24) |
(buffer[5] << 16) | (buffer[6] << 8) | buffer[7];
if (rscsi_disks[i].sector_size == 0) {
rscsi_disks[i].sector_size = 512;
#ifdef MACH
printk("sd%d : sector size 0 reported, assuming 512.\n", i);
#else
printk("sd%c : sector size 0 reported, assuming 512.\n", 'a' + i);
#endif
}
if (rscsi_disks[i].sector_size != 512 &&
rscsi_disks[i].sector_size != 1024 &&
rscsi_disks[i].sector_size != 256)
{
#ifdef MACH
printk ("sd%d : unsupported sector size %d.\n",
i, rscsi_disks[i].sector_size);
#else
printk ("sd%c : unsupported sector size %d.\n",
'a' + i, rscsi_disks[i].sector_size);
#endif
if(rscsi_disks[i].device->removable){
rscsi_disks[i].capacity = 0;
} else {
printk ("scsi : deleting disk entry.\n");
rscsi_disks[i].device = NULL;
sd_template.nr_dev--;
sd_gendisk.nr_real--;
return i;
}
}
{
int m, mb;
int sz_quot, sz_rem;
int hard_sector = rscsi_disks[i].sector_size;
for (m=i<<4; m<((i+1)<<4); m++){
sd_hardsizes[m] = hard_sector;
}
mb = rscsi_disks[i].capacity / 1024 * hard_sector / 1024;
m = (mb + 50) / 100;
sz_quot = m / 10;
sz_rem = m - (10 * sz_quot);
#ifdef MACH
printk ("SCSI device sd%d: hdwr sector= %d bytes."
" Sectors= %d [%d MB] [%d.%1d GB]\n",
i, hard_sector, rscsi_disks[i].capacity,
mb, sz_quot, sz_rem);
#else
printk ("SCSI device sd%c: hdwr sector= %d bytes."
" Sectors= %d [%d MB] [%d.%1d GB]\n",
i+'a', hard_sector, rscsi_disks[i].capacity,
mb, sz_quot, sz_rem);
#endif
}
if(rscsi_disks[i].sector_size == 1024)
rscsi_disks[i].capacity <<= 1;
if(rscsi_disks[i].sector_size == 256)
rscsi_disks[i].capacity >>= 1;
}
rscsi_disks[i].write_prot = 0;
if ( rscsi_disks[i].device->removable && rscsi_disks[i].ready ) {
memset ((void *) &cmd[0], 0, 8);
cmd[0] = MODE_SENSE;
cmd[1] = (rscsi_disks[i].device->lun << 5) & 0xe0;
cmd[2] = 1;
cmd[4] = 12;
SCpnt->cmd_len = 0;
SCpnt->sense_buffer[0] = 0;
SCpnt->sense_buffer[2] = 0;
{
struct semaphore sem = MUTEX_LOCKED;
SCpnt->request.rq_status = RQ_SCSI_BUSY;
SCpnt->request.sem = &sem;
scsi_do_cmd (SCpnt,
(void *) cmd, (void *) buffer,
512, sd_init_done,  SD_TIMEOUT,
MAX_RETRIES);
down(&sem);
}
the_result = SCpnt->result;
SCpnt->request.rq_status = RQ_INACTIVE;
wake_up(&SCpnt->device->device_wait);
if ( the_result ) {
#ifdef MACH
printk ("sd%d: test WP failed, assume Write Protected\n",i);
#else
printk ("sd%c: test WP failed, assume Write Protected\n",i+'a');
#endif
rscsi_disks[i].write_prot = 1;
} else {
rscsi_disks[i].write_prot = ((buffer[2] & 0x80) != 0);
#ifdef MACH
printk ("sd%d: Write Protect is %s\n",i,
rscsi_disks[i].write_prot ? "on" : "off");
#else
printk ("sd%c: Write Protect is %s\n",i+'a',
rscsi_disks[i].write_prot ? "on" : "off");
#endif
}
}
rscsi_disks[i].ten = 1;
rscsi_disks[i].remap = 1;
scsi_free(buffer, 512);
return i;
}
static int sd_registered = 0;
static int sd_init()
{
int i;
if (sd_template.dev_noticed == 0) return 0;
if(!sd_registered) {
if (register_blkdev(MAJOR_NR,"sd",&sd_fops)) {
printk("Unable to get major %d for SCSI disk\n",MAJOR_NR);
return 1;
}
sd_registered++;
}
if(rscsi_disks) return 0;
sd_template.dev_max = sd_template.dev_noticed + SD_EXTRA_DEVS;
rscsi_disks = (Scsi_Disk *)
scsi_init_malloc(sd_template.dev_max * sizeof(Scsi_Disk), GFP_ATOMIC);
memset(rscsi_disks, 0, sd_template.dev_max * sizeof(Scsi_Disk));
sd_sizes = (int *) scsi_init_malloc((sd_template.dev_max << 4) *
sizeof(int), GFP_ATOMIC);
memset(sd_sizes, 0, (sd_template.dev_max << 4) * sizeof(int));
sd_blocksizes = (int *) scsi_init_malloc((sd_template.dev_max << 4) *
sizeof(int), GFP_ATOMIC);
sd_hardsizes = (int *) scsi_init_malloc((sd_template.dev_max << 4) *
sizeof(int), GFP_ATOMIC);
for(i=0;i<(sd_template.dev_max << 4);i++){
sd_blocksizes[i] = 1024;
sd_hardsizes[i] = 512;
}
blksize_size[MAJOR_NR] = sd_blocksizes;
hardsect_size[MAJOR_NR] = sd_hardsizes;
sd = (struct hd_struct *) scsi_init_malloc((sd_template.dev_max << 4) *
sizeof(struct hd_struct),
GFP_ATOMIC);
sd_gendisk.max_nr = sd_template.dev_max;
sd_gendisk.part = sd;
sd_gendisk.sizes = sd_sizes;
sd_gendisk.real_devices = (void *) rscsi_disks;
return 0;
}
static void sd_finish(void)
{
struct gendisk *gendisk;
int i;
blk_dev[MAJOR_NR].request_fn = DEVICE_REQUEST;
for (gendisk = gendisk_head; gendisk != NULL; gendisk = gendisk->next)
if (gendisk == &sd_gendisk)
break;
if (gendisk == NULL)
{
sd_gendisk.next = gendisk_head;
gendisk_head = &sd_gendisk;
}
for (i = 0; i < sd_template.dev_max; ++i)
if (!rscsi_disks[i].capacity &&
rscsi_disks[i].device)
{
if (MODULE_FLAG
&& !rscsi_disks[i].has_part_table) {
sd_sizes[i << 4] = rscsi_disks[i].capacity;
revalidate_scsidisk(MKDEV(MAJOR_NR, i << 4), 0);
}
else
i=sd_init_onedisk(i);
rscsi_disks[i].has_part_table = 1;
}
if(rscsi_disks[0].device && rscsi_disks[0].device->host->sg_tablesize)
read_ahead[MAJOR_NR] = 120;
else
read_ahead[MAJOR_NR] = 4;
return;
}
static int sd_detect(Scsi_Device * SDp){
if(SDp->type != TYPE_DISK && SDp->type != TYPE_MOD) return 0;
#ifdef MACH
printk("Detected scsi %sdisk sd%d at scsi%d, channel %d, id %d, lun %d\n",
SDp->removable ? "removable " : "",
sd_template.dev_noticed++,
SDp->host->host_no, SDp->channel, SDp->id, SDp->lun);
#else
printk("Detected scsi %sdisk sd%c at scsi%d, channel %d, id %d, lun %d\n",
SDp->removable ? "removable " : "",
'a'+ (sd_template.dev_noticed++),
SDp->host->host_no, SDp->channel, SDp->id, SDp->lun);
#endif
return 1;
}
static int sd_attach(Scsi_Device * SDp){
Scsi_Disk * dpnt;
int i;
if(SDp->type != TYPE_DISK && SDp->type != TYPE_MOD) return 0;
if(sd_template.nr_dev >= sd_template.dev_max) {
SDp->attached--;
return 1;
}
for(dpnt = rscsi_disks, i=0; i<sd_template.dev_max; i++, dpnt++)
if(!dpnt->device) break;
if(i >= sd_template.dev_max) panic ("scsi_devices corrupt (sd)");
SDp->scsi_request_fn = do_sd_request;
rscsi_disks[i].device = SDp;
rscsi_disks[i].has_part_table = 0;
sd_template.nr_dev++;
sd_gendisk.nr_real++;
return 0;
}
#define DEVICE_BUSY rscsi_disks[target].device->busy
#define USAGE rscsi_disks[target].device->access_count
#define CAPACITY rscsi_disks[target].capacity
#define MAYBE_REINIT  sd_init_onedisk(target)
#define GENDISK_STRUCT sd_gendisk
int revalidate_scsidisk(kdev_t dev, int maxusage){
int target;
struct gendisk * gdev;
unsigned long flags;
int max_p;
int start;
int i;
target =  DEVICE_NR(dev);
gdev = &GENDISK_STRUCT;
save_flags(flags);
cli();
if (DEVICE_BUSY || USAGE > maxusage) {
restore_flags(flags);
printk("Device busy for revalidation (usage=%d)\n", USAGE);
return -EBUSY;
}
DEVICE_BUSY = 1;
restore_flags(flags);
max_p = gdev->max_p;
start = target << gdev->minor_shift;
for (i=max_p - 1; i >=0 ; i--) {
int minor = start+i;
kdev_t devi = MKDEV(MAJOR_NR, minor);
sync_dev(devi);
invalidate_inodes(devi);
invalidate_buffers(devi);
gdev->part[minor].start_sect = 0;
gdev->part[minor].nr_sects = 0;
blksize_size[MAJOR_NR][minor] = 1024;
}
#ifdef MAYBE_REINIT
MAYBE_REINIT;
#endif
gdev->part[start].nr_sects = CAPACITY;
resetup_one_dev(gdev, target);
DEVICE_BUSY = 0;
return 0;
}
static int fop_revalidate_scsidisk(kdev_t dev){
return revalidate_scsidisk(dev, 0);
}
static void sd_detach(Scsi_Device * SDp)
{
Scsi_Disk * dpnt;
int i;
int max_p;
int start;
for(dpnt = rscsi_disks, i=0; i<sd_template.dev_max; i++, dpnt++)
if(dpnt->device == SDp) {
max_p = sd_gendisk.max_p;
start = i << sd_gendisk.minor_shift;
for (i=max_p - 1; i >=0 ; i--) {
int minor = start+i;
kdev_t devi = MKDEV(MAJOR_NR, minor);
sync_dev(devi);
invalidate_inodes(devi);
invalidate_buffers(devi);
sd_gendisk.part[minor].start_sect = 0;
sd_gendisk.part[minor].nr_sects = 0;
sd_sizes[minor] = 0;
}
dpnt->has_part_table = 0;
dpnt->device = NULL;
dpnt->capacity = 0;
SDp->attached--;
sd_template.dev_noticed--;
sd_template.nr_dev--;
sd_gendisk.nr_real--;
return;
}
return;
}
#ifdef MODULE
int init_module(void) {
sd_template.usage_count = &mod_use_count_;
return scsi_register_module(MODULE_SCSI_DEV, &sd_template);
}
void cleanup_module( void)
{
struct gendisk * prev_sdgd;
struct gendisk * sdgd;
scsi_unregister_module(MODULE_SCSI_DEV, &sd_template);
unregister_blkdev(SCSI_DISK_MAJOR, "sd");
sd_registered--;
if( rscsi_disks != NULL )
{
scsi_init_free((char *) rscsi_disks,
(sd_template.dev_noticed + SD_EXTRA_DEVS)
* sizeof(Scsi_Disk));
scsi_init_free((char *) sd_sizes, sd_template.dev_max * sizeof(int));
scsi_init_free((char *) sd_blocksizes, sd_template.dev_max * sizeof(int));
scsi_init_free((char *) sd_hardsizes, sd_template.dev_max * sizeof(int));
scsi_init_free((char *) sd,
(sd_template.dev_max << 4) * sizeof(struct hd_struct));
sdgd = gendisk_head;
prev_sdgd = NULL;
while(sdgd != &sd_gendisk)
{
prev_sdgd = sdgd;
sdgd = sdgd->next;
}
if(sdgd != &sd_gendisk)
printk("sd_gendisk not in disk chain.\n");
else {
if(prev_sdgd != NULL)
prev_sdgd->next = sdgd->next;
else
gendisk_head = sdgd->next;
}
}
blksize_size[MAJOR_NR] = NULL;
blk_dev[MAJOR_NR].request_fn = NULL;
blk_size[MAJOR_NR] = NULL;
hardsect_size[MAJOR_NR] = NULL;
read_ahead[MAJOR_NR] = 0;
sd_template.dev_max = 0;
}
#endif