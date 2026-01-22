int eata_pio_set_info(char *buffer, int length, struct Scsi_Host *HBA_ptr)
{
DBG(DBG_PROC_WRITE, printk("%s\n", buffer));
return(-ENOSYS);
}
int eata_pio_proc_info(char *buffer, char **start, off_t offset, int length,
int hostno, int inout)
{
Scsi_Device *scd;
struct Scsi_Host *HBA_ptr;
static u8 buff[512];
int i;
int size, len = 0;
off_t begin = 0;
off_t pos = 0;
HBA_ptr = first_HBA;
for (i = 1; i <= registered_HBAs; i++) {
if (HBA_ptr->host_no == hostno)
break;
HBA_ptr = SD(HBA_ptr)->next;
}
if(inout == TRUE)
return(eata_pio_set_info(buffer, length, HBA_ptr));
if (offset == 0)
memset(buff, 0, sizeof(buff));
size = sprintf(buffer+len, "EATA (Extended Attachment) PIO driver version: "
"%d.%d%s\n",VER_MAJOR, VER_MINOR, VER_SUB);
len += size; pos = begin + len;
size = sprintf(buffer + len, "queued commands:     %10ld\n"
"processed interrupts:%10ld\n", queue_counter, int_counter);
len += size; pos = begin + len;
size = sprintf(buffer + len, "\nscsi%-2d: HBA %.10s\n",
HBA_ptr->host_no, SD(HBA_ptr)->name);
len += size;
pos = begin + len;
size = sprintf(buffer + len, "Firmware revision: v%s\n",
SD(HBA_ptr)->revision);
len += size;
pos = begin + len;
size = sprintf(buffer + len, "IO: PIO\n");
len += size;
pos = begin + len;
size = sprintf(buffer + len, "Base IO : %#.4x\n", (u32) HBA_ptr->base);
len += size;
pos = begin + len;
size = sprintf(buffer + len, "Host Bus: %s\n",
(SD(HBA_ptr)->bustype == 'P')?"PCI ":
(SD(HBA_ptr)->bustype == 'E')?"EISA":"ISA ");
len += size;
pos = begin + len;
if (pos < offset) {
len = 0;
begin = pos;
}
if (pos > offset + length)
goto stop_output;
scd = scsi_devices;
size = sprintf(buffer+len,"Attached devices: %s\n", (scd)?"":"none");
len += size;
pos = begin + len;
while (scd) {
if (scd->host == HBA_ptr) {
proc_print_scsidevice(scd, buffer, &size, len);
len += size;
pos = begin + len;
if (pos < offset) {
len = 0;
begin = pos;
}
if (pos > offset + length)
goto stop_output;
}
scd = scd->next;
}
stop_output:
DBG(DBG_PROC, printk("2pos: %ld offset: %ld len: %d\n", pos, offset, len));
*start=buffer+(offset-begin);
len-=(offset-begin);
if(len>length)
len = length;
DBG(DBG_PROC, printk("3pos: %ld offset: %ld len: %d\n", pos, offset, len));
return (len);
}