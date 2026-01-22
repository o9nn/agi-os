#define _SCSI_SYMS_VER_
#define __NO_VERSION__
#include <linux/module.h>
#include <asm/io.h>
#include <asm/segment.h>
#include <asm/system.h>
#include <asm/page.h>
#include <linux/errno.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/mm.h>
#include <linux/string.h>
#include <linux/blk.h>
#include "scsi.h"
#include "hosts.h"
#include <scsi/scsi_ioctl.h>
#define NORMAL_RETRIES 5
#define NORMAL_TIMEOUT (10 * HZ)
#define FORMAT_UNIT_TIMEOUT (2 * 60 * 60 * HZ)
#define START_STOP_TIMEOUT (60 * HZ)
#define MOVE_MEDIUM_TIMEOUT (5 * 60 * HZ)
#define READ_ELEMENT_STATUS_TIMEOUT (5 * 60 * HZ)
#define MAX_BUF PAGE_SIZE
#define max(a,b) (((a) > (b)) ? (a) : (b))
static int ioctl_probe(struct Scsi_Host * host, void *buffer)
{
int temp, result;
unsigned int len,slen;
const char * string;
if ((temp = host->hostt->present) && buffer) {
result = verify_area(VERIFY_READ, buffer, sizeof(long));
if (result) return result;
len = get_user ((unsigned int *) buffer);
if(host->hostt->info)
string = host->hostt->info(host);
else
string = host->hostt->name;
if(string) {
slen = strlen(string);
if (len > slen)
len = slen + 1;
result = verify_area(VERIFY_WRITE, buffer, len);
if (result) return result;
memcpy_tofs (buffer, string, len);
}
}
return temp;
}
static void scsi_ioctl_done (Scsi_Cmnd * SCpnt)
{
struct request * req;
req = &SCpnt->request;
req->rq_status = RQ_SCSI_DONE;
if (req->sem != NULL) {
up(req->sem);
}
}
static int ioctl_internal_command(Scsi_Device *dev, char * cmd,
int timeout, int retries)
{
int result;
Scsi_Cmnd * SCpnt;
SCpnt = allocate_device(NULL, dev, 1);
{
struct semaphore sem = MUTEX_LOCKED;
SCpnt->request.sem = &sem;
scsi_do_cmd(SCpnt,  cmd, NULL,  0, scsi_ioctl_done,  timeout, retries);
down(&sem);
}
if(driver_byte(SCpnt->result) != 0)
switch(SCpnt->sense_buffer[2] & 0xf) {
case ILLEGAL_REQUEST:
if(cmd[0] == ALLOW_MEDIUM_REMOVAL) dev->lockable = 0;
else printk("SCSI device (ioctl) reports ILLEGAL REQUEST.\n");
break;
case NOT_READY:
if(dev->removable){
printk(KERN_INFO "Device not ready.  Make sure there is a disc in the drive.\n");
break;
};
case UNIT_ATTENTION:
if (dev->removable){
dev->changed = 1;
SCpnt->result = 0;
printk(KERN_INFO "Disc change detected.\n");
break;
};
default:
printk("SCSI error: host %d id %d lun %d return code = %x\n",
dev->host->host_no,
dev->id,
dev->lun,
SCpnt->result);
printk("\tSense class %x, sense error %x, extended sense %x\n",
sense_class(SCpnt->sense_buffer[0]),
sense_error(SCpnt->sense_buffer[0]),
SCpnt->sense_buffer[2] & 0xf);
};
result = SCpnt->result;
SCpnt->request.rq_status = RQ_INACTIVE;
if (!SCpnt->device->was_reset && SCpnt->device->scsi_request_fn)
(*SCpnt->device->scsi_request_fn)();
wake_up(&SCpnt->device->device_wait);
return result;
}
int scsi_ioctl_send_command(Scsi_Device *dev, void *buffer)
{
char * buf;
unsigned char cmd[12];
char * cmd_in;
Scsi_Cmnd * SCpnt;
unsigned char opcode;
int inlen, outlen, cmdlen;
int needed, buf_needed;
int timeout, retries, result;
if (!buffer)
return -EINVAL;
result = verify_area(VERIFY_READ, buffer, 2*sizeof(long) + 1);
if (result) return result;
inlen = get_user((unsigned int *) buffer);
outlen = get_user( ((unsigned int *) buffer) + 1);
if( inlen > MAX_BUF )  return -EINVAL;
if( outlen > MAX_BUF )  return -EINVAL;
cmd_in = (char *) ( ((int *)buffer) + 2);
opcode = get_user(cmd_in);
needed = buf_needed = (inlen > outlen ? inlen : outlen);
if(buf_needed){
buf_needed = (buf_needed + 511) & ~511;
if (buf_needed > MAX_BUF) buf_needed = MAX_BUF;
buf = (char *) scsi_malloc(buf_needed);
if (!buf) return -ENOMEM;
memset(buf, 0, buf_needed);
} else
buf = NULL;
cmdlen = COMMAND_SIZE(opcode);
result = verify_area(VERIFY_READ, cmd_in,
cmdlen + inlen > MAX_BUF ? MAX_BUF : inlen);
if (result) return result;
memcpy_fromfs ((void *) cmd,  cmd_in,  cmdlen);
memcpy_fromfs ((void *) buf,
(void *) (cmd_in + cmdlen),
inlen);
cmd[1] = ( cmd[1] & 0x1f ) | (dev->lun << 5);
switch (opcode)
{
case FORMAT_UNIT:
timeout = FORMAT_UNIT_TIMEOUT;
retries = 1;
break;
case START_STOP:
timeout = START_STOP_TIMEOUT;
retries = NORMAL_RETRIES;
break;
case MOVE_MEDIUM:
timeout = MOVE_MEDIUM_TIMEOUT;
retries = NORMAL_RETRIES;
break;
case READ_ELEMENT_STATUS:
timeout = READ_ELEMENT_STATUS_TIMEOUT;
retries = NORMAL_RETRIES;
break;
default:
timeout = NORMAL_TIMEOUT;
retries = NORMAL_RETRIES;
break;
}
#ifndef DEBUG_NO_CMD
SCpnt = allocate_device(NULL, dev, 1);
{
struct semaphore sem = MUTEX_LOCKED;
SCpnt->request.sem = &sem;
scsi_do_cmd(SCpnt,  cmd,  buf, needed,  scsi_ioctl_done,
timeout, retries);
down(&sem);
}
if(SCpnt->result) {
result = verify_area(VERIFY_WRITE,
cmd_in,
sizeof(SCpnt->sense_buffer));
if (result) return result;
memcpy_tofs((void *) cmd_in,
SCpnt->sense_buffer,
sizeof(SCpnt->sense_buffer));
} else {
result = verify_area(VERIFY_WRITE, cmd_in, outlen);
if (result) return result;
memcpy_tofs ((void *) cmd_in,  buf,  outlen);
}
result = SCpnt->result;
SCpnt->request.rq_status = RQ_INACTIVE;
if (buf) scsi_free(buf, buf_needed);
if(SCpnt->device->scsi_request_fn)
(*SCpnt->device->scsi_request_fn)();
wake_up(&SCpnt->device->device_wait);
return result;
#else
{
int i;
printk("scsi_ioctl : device %d.  command = ", dev->id);
for (i = 0; i < 12; ++i)
printk("%02x ", cmd[i]);
printk("\nbuffer =");
for (i = 0; i < 20; ++i)
printk("%02x ", buf[i]);
printk("\n");
printk("inlen = %d, outlen = %d, cmdlen = %d\n",
inlen, outlen, cmdlen);
printk("buffer = %d, cmd_in = %d\n", buffer, cmd_in);
}
return 0;
#endif
}
int scsi_ioctl (Scsi_Device *dev, int cmd, void *arg)
{
int result;
char scsi_cmd[12];
if (!dev) return -ENXIO;
switch (cmd) {
case SCSI_IOCTL_GET_IDLUN:
result = verify_area(VERIFY_WRITE, (void *) arg, 2*sizeof(long));
if (result) return result;
put_user(dev->id
+ (dev->lun << 8)
+ (dev->channel << 16)
+ ((dev->host->hostt->proc_dir->low_ino & 0xff) << 24),
(unsigned long *) arg);
put_user( dev->host->unique_id, (unsigned long *) arg+1);
return 0;
case SCSI_IOCTL_GET_BUS_NUMBER:
result = verify_area(VERIFY_WRITE, (void *) arg, sizeof(int));
if (result) return result;
put_user( dev->host->host_no, (int *) arg);
return 0;
case SCSI_IOCTL_TAGGED_ENABLE:
if(!suser())  return -EACCES;
if(!dev->tagged_supported) return -EINVAL;
dev->tagged_queue = 1;
dev->current_tag = 1;
return 0;
case SCSI_IOCTL_TAGGED_DISABLE:
if(!suser())  return -EACCES;
if(!dev->tagged_supported) return -EINVAL;
dev->tagged_queue = 0;
dev->current_tag = 0;
return 0;
case SCSI_IOCTL_PROBE_HOST:
return ioctl_probe(dev->host, arg);
case SCSI_IOCTL_SEND_COMMAND:
if(!suser() || securelevel > 0)  return -EACCES;
return scsi_ioctl_send_command((Scsi_Device *) dev, arg);
case SCSI_IOCTL_DOORLOCK:
if (!dev->removable || !dev->lockable) return 0;
scsi_cmd[0] = ALLOW_MEDIUM_REMOVAL;
scsi_cmd[1] = dev->lun << 5;
scsi_cmd[2] = scsi_cmd[3] = scsi_cmd[5] = 0;
scsi_cmd[4] = SCSI_REMOVAL_PREVENT;
return ioctl_internal_command((Scsi_Device *) dev, scsi_cmd,
NORMAL_TIMEOUT, NORMAL_RETRIES);
break;
case SCSI_IOCTL_DOORUNLOCK:
if (!dev->removable || !dev->lockable) return 0;
scsi_cmd[0] = ALLOW_MEDIUM_REMOVAL;
scsi_cmd[1] = dev->lun << 5;
scsi_cmd[2] = scsi_cmd[3] = scsi_cmd[5] = 0;
scsi_cmd[4] = SCSI_REMOVAL_ALLOW;
return ioctl_internal_command((Scsi_Device *) dev, scsi_cmd,
NORMAL_TIMEOUT, NORMAL_RETRIES);
case SCSI_IOCTL_TEST_UNIT_READY:
scsi_cmd[0] = TEST_UNIT_READY;
scsi_cmd[1] = dev->lun << 5;
scsi_cmd[2] = scsi_cmd[3] = scsi_cmd[5] = 0;
scsi_cmd[4] = 0;
return ioctl_internal_command((Scsi_Device *) dev, scsi_cmd,
NORMAL_TIMEOUT, NORMAL_RETRIES);
break;
case SCSI_IOCTL_START_UNIT:
scsi_cmd[0] = START_STOP;
scsi_cmd[1] = dev->lun << 5;
scsi_cmd[2] = scsi_cmd[3] = scsi_cmd[5] = 0;
scsi_cmd[4] = 1;
return ioctl_internal_command((Scsi_Device *) dev, scsi_cmd,
START_STOP_TIMEOUT, NORMAL_RETRIES);
break;
case SCSI_IOCTL_STOP_UNIT:
scsi_cmd[0] = START_STOP;
scsi_cmd[1] = dev->lun << 5;
scsi_cmd[2] = scsi_cmd[3] = scsi_cmd[5] = 0;
scsi_cmd[4] = 0;
return ioctl_internal_command((Scsi_Device *) dev, scsi_cmd,
START_STOP_TIMEOUT, NORMAL_RETRIES);
break;
default :
return -EINVAL;
}
return -EINVAL;
}
int kernel_scsi_ioctl (Scsi_Device *dev, int cmd, void *arg) {
unsigned long oldfs;
int tmp;
oldfs = get_fs();
set_fs(get_ds());
tmp = scsi_ioctl (dev, cmd, arg);
set_fs(oldfs);
return tmp;
}