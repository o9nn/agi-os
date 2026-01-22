#define	BLS	(&aic7xxx_buffer[size])
#define HDRB \
"             < 2K      2K+     4K+     8K+    16K+    32K+    64K+   128K+"
#ifdef PROC_DEBUG
extern int vsprintf(char *, const char *, va_list);
static void
proc_debug(const char *fmt, ...)
{
va_list ap;
char buf[256];
va_start(ap, fmt);
vsprintf(buf, fmt, ap);
printk(buf);
va_end(ap);
}
#else
#  define proc_debug(fmt, args...)
#endif
static int aic7xxx_buffer_size = 0;
static char *aic7xxx_buffer = NULL;
int
aic7xxx_set_info(char *buffer, int length, struct Scsi_Host *HBAptr)
{
proc_debug("aic7xxx_set_info(): %s\n", buffer);
return (-ENOSYS);
}
int
aic7xxx_proc_info ( char *buffer, char **start, off_t offset, int length,
int hostno, int inout)
{
struct Scsi_Host *HBAptr;
struct aic7xxx_host *p;
int    size = 0;
unsigned char i;
struct aic7xxx_xferstats *sp;
unsigned char target;
HBAptr = NULL;
for(p=first_aic7xxx; p->host->host_no != hostno; p=p->next)
;
if (!p)
{
size += sprintf(buffer, "Can't find adapter for host number %d\n", hostno);
if (size > length)
{
return (size);
}
else
{
return (length);
}
}
HBAptr = p->host;
if (inout == TRUE)
{
return (aic7xxx_set_info(buffer, length, HBAptr));
}
p = (struct aic7xxx_host *) HBAptr->hostdata;
size = 4096;
for (target = 0; target < MAX_TARGETS; target++)
{
if (p->dev_flags[target] & DEVICE_PRESENT)
#ifdef AIC7XXX_PROC_STATS
size += 512;
#else
size += 256;
#endif
}
if (aic7xxx_buffer_size != size)
{
if (aic7xxx_buffer != NULL)
{
kfree(aic7xxx_buffer);
aic7xxx_buffer_size = 0;
}
aic7xxx_buffer = kmalloc(size, GFP_KERNEL);
}
if (aic7xxx_buffer == NULL)
{
size = sprintf(buffer, "AIC7xxx - kmalloc error at line %d\n",
__LINE__);
return size;
}
aic7xxx_buffer_size = size;
size = 0;
size += sprintf(BLS, "Adaptec AIC7xxx driver version: ");
size += sprintf(BLS, "%s/", AIC7XXX_C_VERSION);
size += sprintf(BLS, "%s", AIC7XXX_H_VERSION);
size += sprintf(BLS, "\n");
size += sprintf(BLS, "Compile Options:\n");
#ifdef CONFIG_AIC7XXX_TCQ_ON_BY_DEFAULT
size += sprintf(BLS, "  TCQ Enabled By Default : Enabled\n");
#else
size += sprintf(BLS, "  TCQ Enabled By Default : Disabled\n");
#endif
#ifdef AIC7XXX_PROC_STATS
size += sprintf(BLS, "  AIC7XXX_PROC_STATS     : Enabled\n");
#else
size += sprintf(BLS, "  AIC7XXX_PROC_STATS     : Disabled\n");
#endif
size += sprintf(BLS, "  AIC7XXX_RESET_DELAY    : %d\n", AIC7XXX_RESET_DELAY);
size += sprintf(BLS, "\n");
size += sprintf(BLS, "Adapter Configuration:\n");
size += sprintf(BLS, "           SCSI Adapter: %s\n",
board_names[p->board_name_index]);
if (p->flags & AHC_TWIN)
size += sprintf(BLS, "                         Twin Channel\n");
else
{
char *channel = "";
char *ultra = "";
char *wide = "Narrow ";
if (p->flags & AHC_MULTI_CHANNEL)
{
channel = " Channel A";
if (p->flags & (AHC_CHNLB|AHC_CHNLC))
channel = (p->flags & AHC_CHNLB) ? " Channel B" : " Channel C";
}
if (p->features & AHC_WIDE)
wide = "Wide ";
if (p->features & AHC_ULTRA2)
ultra = "Ultra2-LVD/SE ";
else if (p->features & AHC_ULTRA)
ultra = "Ultra ";
size += sprintf(BLS, "                           %s%sController%s\n",
ultra, wide, channel);
}
if( !(p->maddr) )
{
size += sprintf(BLS, "    Programmed I/O Base: %lx\n", p->base);
}
else
{
size += sprintf(BLS, "    PCI MMAPed I/O Base: 0x%lx\n", p->mbase);
}
if( (p->chip & (AHC_VL | AHC_EISA)) )
{
size += sprintf(BLS, "    BIOS Memory Address: 0x%08x\n", p->bios_address);
}
size += sprintf(BLS, " Adapter SEEPROM Config: %s\n",
(p->flags & AHC_SEEPROM_FOUND) ? "SEEPROM found and used." :
((p->flags & AHC_USEDEFAULTS) ? "SEEPROM not found, using defaults." :
"SEEPROM not found, using leftover BIOS values.") );
size += sprintf(BLS, "      Adaptec SCSI BIOS: %s\n",
(p->flags & AHC_BIOS_ENABLED) ? "Enabled" : "Disabled");
size += sprintf(BLS, "                    IRQ: %d\n", HBAptr->irq);
size += sprintf(BLS, "                   SCBs: Active %d, Max Active %d,\n",
p->activescbs, p->max_activescbs);
size += sprintf(BLS, "                         Allocated %d, HW %d, "
"Page %d\n", p->scb_data->numscbs, p->scb_data->maxhscbs,
p->scb_data->maxscbs);
if (p->flags & AHC_EXTERNAL_SRAM)
size += sprintf(BLS, "                         Using External SCB SRAM\n");
size += sprintf(BLS, "             Interrupts: %ld", p->isr_count);
if (p->chip & AHC_EISA)
{
size += sprintf(BLS, " %s\n",
(p->pause & IRQMS) ? "(Level Sensitive)" : "(Edge Triggered)");
}
else
{
size += sprintf(BLS, "\n");
}
size += sprintf(BLS, "      BIOS Control Word: 0x%04x\n",
p->bios_control);
size += sprintf(BLS, "   Adapter Control Word: 0x%04x\n",
p->adapter_control);
size += sprintf(BLS, "   Extended Translation: %sabled\n",
(p->flags & AHC_EXTEND_TRANS_A) ? "En" : "Dis");
size += sprintf(BLS, "Disconnect Enable Flags: 0x%04x\n", p->discenable);
if (p->features & (AHC_ULTRA | AHC_ULTRA2))
{
size += sprintf(BLS, "     Ultra Enable Flags: 0x%04x\n", p->ultraenb);
}
size += sprintf(BLS, " Tag Queue Enable Flags: 0x%04x\n", p->tagenable);
size += sprintf(BLS, "Ordered Queue Tag Flags: 0x%04x\n", p->orderedtag);
size += sprintf(BLS, "Default Tag Queue Depth: %d\n", AIC7XXX_CMDS_PER_DEVICE);
size += sprintf(BLS, "    Tagged Queue By Device array for aic7xxx host "
"instance %d:\n", p->instance);
size += sprintf(BLS, "      {");
for(i=0; i < (MAX_TARGETS - 1); i++)
size += sprintf(BLS, "%d,",aic7xxx_tag_info[p->instance].tag_commands[i]);
size += sprintf(BLS, "%d}\n",aic7xxx_tag_info[p->instance].tag_commands[i]);
size += sprintf(BLS, "    Actual queue depth per device for aic7xxx host "
"instance %d:\n", p->instance);
size += sprintf(BLS, "      {");
for(i=0; i < (MAX_TARGETS - 1); i++)
size += sprintf(BLS, "%d,", p->dev_max_queue_depth[i]);
size += sprintf(BLS, "%d}\n", p->dev_max_queue_depth[i]);
size += sprintf(BLS, "\n");
size += sprintf(BLS, "Statistics:\n\n");
for (target = 0; target < MAX_TARGETS; target++)
{
sp = &p->stats[target];
if ((p->dev_flags[target] & DEVICE_PRESENT) == 0)
{
continue;
}
if (p->features & AHC_TWIN)
{
size += sprintf(BLS, "(scsi%d:%d:%d:%d)\n",
p->host_no, (target >> 3), (target & 0x7), 0);
}
else
{
size += sprintf(BLS, "(scsi%d:%d:%d:%d)\n",
p->host_no, 0, target, 0);
}
size += sprintf(BLS, "  Device using %s/%s",
(p->transinfo[target].cur_width == MSG_EXT_WDTR_BUS_16_BIT) ?
"Wide" : "Narrow",
(p->transinfo[target].cur_offset != 0) ?
"Sync transfers at " : "Async transfers.\n" );
if (p->transinfo[target].cur_offset != 0)
{
struct aic7xxx_syncrate *sync_rate;
int period = p->transinfo[target].cur_period;
int rate = (p->transinfo[target].cur_width ==
MSG_EXT_WDTR_BUS_16_BIT) ? 1 : 0;
sync_rate = aic7xxx_find_syncrate(p, &period, AHC_SYNCRATE_ULTRA2);
if (sync_rate != NULL)
{
size += sprintf(BLS, "%s MByte/sec, offset %d\n",
sync_rate->rate[rate],
p->transinfo[target].cur_offset );
}
else
{
size += sprintf(BLS, "3.3 MByte/sec, offset %d\n",
p->transinfo[target].cur_offset );
}
}
size += sprintf(BLS, "  Transinfo settings: ");
size += sprintf(BLS, "current(%d/%d/%d), ",
p->transinfo[target].cur_period,
p->transinfo[target].cur_offset,
p->transinfo[target].cur_width);
size += sprintf(BLS, "goal(%d/%d/%d), ",
p->transinfo[target].goal_period,
p->transinfo[target].goal_offset,
p->transinfo[target].goal_width);
size += sprintf(BLS, "user(%d/%d/%d)\n",
p->transinfo[target].user_period,
p->transinfo[target].user_offset,
p->transinfo[target].user_width);
#ifdef AIC7XXX_PROC_STATS
size += sprintf(BLS, "  Total transfers %ld (%ld reads and %ld writes)\n",
sp->r_total + sp->w_total, sp->r_total, sp->w_total);
size += sprintf(BLS, "%s\n", HDRB);
size += sprintf(BLS, "   Reads:");
for (i = 0; i < NUMBER(sp->r_bins); i++)
{
size += sprintf(BLS, " %7ld", sp->r_bins[i]);
}
size += sprintf(BLS, "\n");
size += sprintf(BLS, "  Writes:");
for (i = 0; i < NUMBER(sp->w_bins); i++)
{
size += sprintf(BLS, " %7ld", sp->w_bins[i]);
}
size += sprintf(BLS, "\n");
#else
size += sprintf(BLS, "  Total transfers %ld (%ld reads and %ld writes)\n",
sp->r_total + sp->w_total, sp->r_total, sp->w_total);
#endif
size += sprintf(BLS, "\n\n");
}
if (size >= aic7xxx_buffer_size)
{
printk(KERN_WARNING "aic7xxx: Overflow in aic7xxx_proc.c\n");
}
if (offset > size - 1)
{
kfree(aic7xxx_buffer);
aic7xxx_buffer = NULL;
aic7xxx_buffer_size = length = 0;
*start = NULL;
}
else
{
*start = &aic7xxx_buffer[offset];
if (size - offset < length)
{
length = size - offset;
}
}
return (length);
}