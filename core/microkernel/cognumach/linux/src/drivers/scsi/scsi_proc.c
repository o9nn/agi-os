#define _SCSI_SYMS_VER_
#define __NO_VERSION__
#include <linux/module.h>
#include <linux/string.h>
#include <linux/mm.h>
#include <linux/malloc.h>
#include <linux/proc_fs.h>
#include <linux/errno.h>
#include <linux/stat.h>
#include <linux/blk.h>
#include "scsi.h"
#include "hosts.h"
#ifndef TRUE
#define TRUE  1
#define FALSE 0
#endif
extern int scsi_proc_info(char *, char **, off_t, int, int, int);
struct scsi_dir {
struct proc_dir_entry entry;
char name[4];
};
int generic_proc_info(char *buffer, char **start, off_t offset,
int length, int inode, int inout)
{
int len, pos, begin;
if(inout == TRUE)
return(-ENOSYS);
begin = 0;
pos = len = sprintf(buffer,
"The driver does not yet support the proc-fs\n");
if(pos < offset) {
len = 0;
begin = pos;
}
*start = buffer + (offset - begin);
len -= (offset - begin);
if(len > length)
len = length;
return(len);
}
extern int dispatch_scsi_info(int ino, char *buffer, char **start,
off_t offset, int length, int func)
{
struct Scsi_Host *hpnt = scsi_hostlist;
if(ino == PROC_SCSI_SCSI) {
return(scsi_proc_info(buffer, start, offset, length, 0, func));
}
while(hpnt) {
if (ino == (hpnt->host_no + PROC_SCSI_FILE)) {
if(hpnt->hostt->proc_info == NULL)
return generic_proc_info(buffer, start, offset, length,
hpnt->host_no, func);
else
return(hpnt->hostt->proc_info(buffer, start, offset,
length, hpnt->host_no, func));
}
hpnt = hpnt->next;
}
return(-EBADF);
}
void build_proc_dir_entries(Scsi_Host_Template *tpnt)
{
struct Scsi_Host *hpnt;
struct scsi_dir *scsi_hba_dir;
proc_scsi_register(0, tpnt->proc_dir);
hpnt = scsi_hostlist;
while (hpnt) {
if (tpnt == hpnt->hostt) {
scsi_hba_dir = scsi_init_malloc(sizeof(struct scsi_dir), GFP_KERNEL);
if(scsi_hba_dir == NULL)
panic("Not enough memory to register SCSI HBA in /proc/scsi !\n");
memset(scsi_hba_dir, 0, sizeof(struct scsi_dir));
scsi_hba_dir->entry.low_ino = PROC_SCSI_FILE + hpnt->host_no;
scsi_hba_dir->entry.namelen = sprintf(scsi_hba_dir->name,"%d",
hpnt->host_no);
scsi_hba_dir->entry.name = scsi_hba_dir->name;
scsi_hba_dir->entry.mode = S_IFREG | S_IRUGO | S_IWUSR;
proc_scsi_register(tpnt->proc_dir, &scsi_hba_dir->entry);
}
hpnt = hpnt->next;
}
}
typedef struct
{
char *buf,
*cmdList,
*bufPos,
**cmdPos,
cmdNum;
} parseHandle;
inline int parseFree (parseHandle *handle)
{
kfree (handle->cmdPos);
kfree (handle);
return(-1);
}
parseHandle *parseInit(char *buf, char *cmdList, int cmdNum)
{
char        *ptr;
parseHandle *handle;
if (!buf || !cmdList)
return(NULL);
if ((handle = (parseHandle*) kmalloc(sizeof(parseHandle), GFP_KERNEL)) == 0)
return(NULL);
if ((handle->cmdPos = (char**) kmalloc(sizeof(int) * cmdNum, GFP_KERNEL)) == 0) {
kfree(handle);
return(NULL);
}
handle->buf     = handle->bufPos = buf;
handle->cmdList = cmdList;
handle->cmdNum  = cmdNum;
handle->cmdPos[cmdNum = 0] = cmdList;
for (ptr = cmdList; *ptr; ptr++) {
if(*ptr == ' ') {
*ptr++ = 0;
handle->cmdPos[++cmdNum] = ptr++;
}
}
return(handle);
}
int parseOpt(parseHandle *handle, char **param)
{
int  cmdIndex = 0,
cmdLen = 0;
char *startPos;
if (!handle)
return(parseFree(handle));
for (; *(handle->bufPos) && *(handle->bufPos) == ' '; handle->bufPos++);
if (!*(handle->bufPos))
return(parseFree(handle));
startPos = handle->bufPos;
for (; handle->cmdPos[cmdIndex][cmdLen] && *(handle->bufPos); handle->bufPos++)
{
for (;;)
{
if (*(handle->bufPos) == handle->cmdPos[cmdIndex][cmdLen])
break;
else
if (memcmp(startPos, (char*)(handle->cmdPos[++cmdIndex]), cmdLen))
return(parseFree(handle));
if (cmdIndex >= handle->cmdNum)
return(parseFree(handle));
}
cmdLen++;
}
for (; *(handle->bufPos) && *(handle->bufPos) == ' '; handle->bufPos++);
*param = handle->bufPos;
for (; *(handle->bufPos) && *(handle->bufPos) != ' '; handle->bufPos++);
*(handle->bufPos++) = 0;
return(cmdIndex);
}
void proc_print_scsidevice(Scsi_Device *scd, char *buffer, int *size, int len)
{
int x, y = *size;
y = sprintf(buffer + len,
"Host: scsi%d Channel: %02d Id: %02d Lun: %02d\n  Vendor: ",
scd->host->host_no, scd->channel, scd->id, scd->lun);
for (x = 0; x < 8; x++) {
if (scd->vendor[x] >= 0x20)
y += sprintf(buffer + len + y, "%c", scd->vendor[x]);
else
y += sprintf(buffer + len + y," ");
}
y += sprintf(buffer + len + y, " Model: ");
for (x = 0; x < 16; x++) {
if (scd->model[x] >= 0x20)
y +=  sprintf(buffer + len + y, "%c", scd->model[x]);
else
y += sprintf(buffer + len + y, " ");
}
y += sprintf(buffer + len + y, " Rev: ");
for (x = 0; x < 4; x++) {
if (scd->rev[x] >= 0x20)
y += sprintf(buffer + len + y, "%c", scd->rev[x]);
else
y += sprintf(buffer + len + y, " ");
}
y += sprintf(buffer + len + y, "\n");
y += sprintf(buffer + len + y, "  Type:   %s ",
scd->type < MAX_SCSI_DEVICE_CODE ?
scsi_device_types[(int)scd->type] : "Unknown          " );
y += sprintf(buffer + len + y, "               ANSI"
" SCSI revision: %02x", (scd->scsi_level < 3)?1:2);
if (scd->scsi_level == 2)
y += sprintf(buffer + len + y, " CCS\n");
else
y += sprintf(buffer + len + y, "\n");
*size = y;
return;
}