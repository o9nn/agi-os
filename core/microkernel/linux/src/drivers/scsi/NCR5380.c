#ifndef NDEBUG
#define NDEBUG (NDEBUG_RESTART_SELECT | NDEBUG_ABORT)
#endif
#if (NDEBUG & NDEBUG_LISTS)
#define LIST(x,y) {printk("LINE:%d   Adding %p to %p\n", __LINE__, (void*)(x), (void*)(y)); if ((x)==(y)) udelay(5); }
#define REMOVE(w,x,y,z) {printk("LINE:%d   Removing: %p->%p  %p->%p \n", __LINE__, (void*)(w), (void*)(x), (void*)(y), (void*)(z)); if ((x)==(y)) udelay(5); }
#else
#define LIST(x,y)
#define REMOVE(w,x,y,z)
#endif
#ifndef notyet
#undef LINKED
#undef USLEEP
#undef REAL_DMA
#endif
#ifdef REAL_DMA_POLL
#undef READ_OVERRUNS
#define READ_OVERRUNS
#endif
static int do_abort (struct Scsi_Host *host);
static void do_reset (struct Scsi_Host *host);
static struct Scsi_Host *first_instance = NULL;
static Scsi_Host_Template *the_template = NULL;
static __inline__ void initialize_SCp(Scsi_Cmnd *cmd) {
if (cmd->use_sg) {
cmd->SCp.buffer = (struct scatterlist *) cmd->buffer;
cmd->SCp.buffers_residual = cmd->use_sg - 1;
cmd->SCp.ptr = (char *) cmd->SCp.buffer->address;
cmd->SCp.this_residual = cmd->SCp.buffer->length;
} else {
cmd->SCp.buffer = NULL;
cmd->SCp.buffers_residual = 0;
cmd->SCp.ptr = (char *) cmd->request_buffer;
cmd->SCp.this_residual = cmd->request_bufflen;
}
}
#include <linux/delay.h>
#ifdef NDEBUG
static struct {
unsigned char mask;
const char * name;}
signals[] = {{ SR_DBP, "PARITY"}, { SR_RST, "RST" }, { SR_BSY, "BSY" },
{ SR_REQ, "REQ" }, { SR_MSG, "MSG" }, { SR_CD,  "CD" }, { SR_IO, "IO" },
{ SR_SEL, "SEL" }, {0, NULL}},
basrs[] = {{BASR_ATN, "ATN"}, {BASR_ACK, "ACK"}, {0, NULL}},
icrs[] = {{ICR_ASSERT_RST, "ASSERT RST"},{ICR_ASSERT_ACK, "ASSERT ACK"},
{ICR_ASSERT_BSY, "ASSERT BSY"}, {ICR_ASSERT_SEL, "ASSERT SEL"},
{ICR_ASSERT_ATN, "ASSERT ATN"}, {ICR_ASSERT_DATA, "ASSERT DATA"},
{0, NULL}},
mrs[] = {{MR_BLOCK_DMA_MODE, "MODE BLOCK DMA"}, {MR_TARGET, "MODE TARGET"},
{MR_ENABLE_PAR_CHECK, "MODE PARITY CHECK"}, {MR_ENABLE_PAR_INTR,
"MODE PARITY INTR"}, {MR_MONITOR_BSY, "MODE MONITOR BSY"},
{MR_DMA_MODE, "MODE DMA"}, {MR_ARBITRATE, "MODE ARBITRATION"},
{0, NULL}};
static void NCR5380_print(struct Scsi_Host *instance) {
NCR5380_local_declare();
unsigned char status, data, basr, mr, icr, i;
NCR5380_setup(instance);
cli();
data = NCR5380_read(CURRENT_SCSI_DATA_REG);
status = NCR5380_read(STATUS_REG);
mr = NCR5380_read(MODE_REG);
icr = NCR5380_read(INITIATOR_COMMAND_REG);
basr = NCR5380_read(BUS_AND_STATUS_REG);
sti();
printk("STATUS_REG: %02x ", status);
for (i = 0; signals[i].mask ; ++i)
if (status & signals[i].mask)
printk(",%s", signals[i].name);
printk("\nBASR: %02x ", basr);
for (i = 0; basrs[i].mask ; ++i)
if (basr & basrs[i].mask)
printk(",%s", basrs[i].name);
printk("\nICR: %02x ", icr);
for (i = 0; icrs[i].mask; ++i)
if (icr & icrs[i].mask)
printk(",%s", icrs[i].name);
printk("\nMODE: %02x ", mr);
for (i = 0; mrs[i].mask; ++i)
if (mr & mrs[i].mask)
printk(",%s", mrs[i].name);
printk("\n");
}
static struct {
unsigned char value;
const char *name;
} phases[] = {
{PHASE_DATAOUT, "DATAOUT"}, {PHASE_DATAIN, "DATAIN"}, {PHASE_CMDOUT, "CMDOUT"},
{PHASE_STATIN, "STATIN"}, {PHASE_MSGOUT, "MSGOUT"}, {PHASE_MSGIN, "MSGIN"},
{PHASE_UNKNOWN, "UNKNOWN"}};
static void NCR5380_print_phase(struct Scsi_Host *instance) {
NCR5380_local_declare();
unsigned char status;
int i;
NCR5380_setup(instance);
status = NCR5380_read(STATUS_REG);
if (!(status & SR_REQ))
printk("scsi%d : REQ not asserted, phase unknown.\n",
instance->host_no);
else {
for (i = 0; (phases[i].value != PHASE_UNKNOWN) &&
(phases[i].value != (status & PHASE_MASK)); ++i);
printk("scsi%d : phase %s\n", instance->host_no, phases[i].name);
}
}
#endif
static volatile int main_running = 0;
static __inline__ void run_main(void) {
cli();
if (!main_running) {
main_running = 1;
NCR5380_main();
sti();
} else
sti();
}
#ifdef USLEEP
#ifndef NCR5380_TIMER
#error "NCR5380_TIMER must be defined so that this type of NCR5380 driver gets a unique timer."
#endif
#ifndef USLEEP_SLEEP
#define USLEEP_SLEEP (20*HZ/1000)
#endif
#ifndef USLEEP_POLL
#define USLEEP_POLL (200*HZ/1000)
#endif
static struct Scsi_Host * expires_first = NULL;
static int should_disconnect (unsigned char cmd) {
switch (cmd) {
case READ_6:
case WRITE_6:
case SEEK_6:
case READ_10:
case WRITE_10:
case SEEK_10:
return DISCONNECT_TIME_TO_DATA;
case FORMAT_UNIT:
case SEARCH_HIGH:
case SEARCH_LOW:
case SEARCH_EQUAL:
return DISCONNECT_LONG;
default:
return DISCONNECT_NONE;
}
}
static int NCR5380_set_timer (struct Scsi_Host *instance) {
struct Scsi_Host *tmp, **prev;
cli();
if (((struct NCR5380_hostdata *) (instance->host_data))->next_timer) {
sti();
return -1;
}
for (prev = &expires_first, tmp = expires_first; tmp;
prev = &(((struct NCR5380_hostdata *) tmp->host_data)->next_timer),
tmp = ((struct NCR5380_hostdata *) tmp->host_data)->next_timer)
if (instance->time_expires < tmp->time_expires)
break;
instance->next_timer = tmp;
*prev = instance;
timer_table[NCR5380_TIMER].expires = expires_first->time_expires;
timer_active |= 1 << NCR5380_TIMER;
sti();
return 0;
}
void NCR5380_timer_fn(void) {
struct Scsi_Host *instance;
cli();
for (; expires_first && expires_first->time_expires >= jiffies; ) {
instance = ((NCR5380_hostdata *) expires_first->host_data)->
expires_next;
((NCR5380_hostdata *) expires_first->host_data)->expires_next =
NULL;
((NCR5380_hostdata *) expires_first->host_data)->time_expires =
0;
expires_first = instance;
}
if (expires_first) {
timer_table[NCR5380_TIMER].expires = ((NCR5380_hostdata *)
expires_first->host_data)->time_expires;
timer_active |= (1 << NCR5380_TIMER);
} else {
timer_table[NCR5380_TIMER].expires = 0;
timer_active &= ~(1 << MCR5380_TIMER);
}
sti();
run_main();
}
#endif
static void NCR5380_all_init (void) {
static int done = 0;
if (!done) {
#if (NDEBUG & NDEBUG_INIT)
printk("scsi : NCR5380_all_init()\n");
#endif
done = 1;
#ifdef USLEEP
timer_table[NCR5380_TIMER].expires = 0;
timer_table[NCR5380_TIMER].fn = NCR5380_timer_fn;
#endif
}
}
#ifdef AUTOPROBE_IRQ
static int probe_irq;
static void probe_intr (int irq, void *dev_id, struct pt_regs * regs) {
probe_irq = irq;
};
static int NCR5380_probe_irq (struct Scsi_Host *instance, int possible) {
NCR5380_local_declare();
struct NCR5380_hostdata *hostdata = (struct NCR5380_hostdata *)
instance->hostdata;
unsigned long timeout;
int trying_irqs, i, mask;
NCR5380_setup(instance);
for (trying_irqs = i = 0, mask = 1; i < 16; ++i, mask <<= 1)
if ((mask & possible) &&  (request_irq(i, &probe_intr, SA_INTERRUPT, "NCR-probe", NULL)
== 0))
trying_irqs |= mask;
timeout = jiffies + 250*HZ/1000;
probe_irq = IRQ_NONE;
NCR5380_write(TARGET_COMMAND_REG, 0);
NCR5380_write(SELECT_ENABLE_REG, hostdata->id_mask);
NCR5380_write(OUTPUT_DATA_REG, hostdata->id_mask);
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE | ICR_ASSERT_DATA |
ICR_ASSERT_SEL);
while (probe_irq == IRQ_NONE && jiffies < timeout)
barrier();
NCR5380_write(SELECT_ENABLE_REG, 0);
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE);
for (i = 0, mask = 1; i < 16; ++i, mask <<= 1)
if (trying_irqs & mask)
free_irq(i, NULL);
return probe_irq;
}
#endif
static void NCR5380_print_options (struct Scsi_Host *instance) {
printk(" generic options"
#ifdef AUTOPROBE_IRQ
" AUTOPROBE_IRQ"
#endif
#ifdef AUTOSENSE
" AUTOSENSE"
#endif
#ifdef DIFFERENTIAL
" DIFFERENTIAL"
#endif
#ifdef REAL_DMA
" REAL DMA"
#endif
#ifdef REAL_DMA_POLL
" REAL DMA POLL"
#endif
#ifdef PARITY
" PARITY"
#endif
#ifdef PSEUDO_DMA
" PSEUDO DMA"
#endif
#ifdef SCSI2
" SCSI-2"
#endif
#ifdef UNSAFE
" UNSAFE "
#endif
);
#ifdef USLEEP
printk(" USLEEP, USLEEP_POLL=%d USLEEP_SLEEP=%d", USLEEP_POLL, USLEEP_SLEEP);
#endif
printk(" generic release=%d", NCR5380_PUBLIC_RELEASE);
if (((struct NCR5380_hostdata *)instance->hostdata)->flags & FLAG_NCR53C400) {
printk(" ncr53c400 release=%d", NCR53C400_PUBLIC_RELEASE);
}
}
static void NCR5380_print_status (struct Scsi_Host *instance) {
static char pr_bfr[512];
char *start;
int len;
printk("NCR5380 : coroutine is%s running.\n",
main_running ? "" : "n't");
#ifdef NDEBUG
NCR5380_print (instance);
NCR5380_print_phase (instance);
#endif
len = NCR5380_proc_info(pr_bfr, &start, 0, sizeof(pr_bfr),
instance->host_no, 0);
pr_bfr[len] = 0;
printk("\n%s\n", pr_bfr);
}
#undef SPRINTF
#define SPRINTF(args...) do { if(pos < buffer + length-80) pos += sprintf(pos, ## args); } while(0)
static
char *lprint_Scsi_Cmnd (Scsi_Cmnd *cmd, char *pos, char *buffer, int length);
static
char *lprint_command (unsigned char *cmd, char *pos, char *buffer, int len);
static
char *lprint_opcode(int opcode, char *pos, char *buffer, int length);
#ifndef NCR5380_proc_info
static
#endif
int NCR5380_proc_info (
char *buffer, char **start,off_t offset,
int length,int hostno,int inout)
{
char *pos = buffer;
struct Scsi_Host *instance;
struct NCR5380_hostdata *hostdata;
Scsi_Cmnd *ptr;
for (instance = first_instance; instance &&
instance->host_no != hostno; instance=instance->next)
;
if (!instance)
return(-ESRCH);
hostdata = (struct NCR5380_hostdata *)instance->hostdata;
if (inout) {
#ifdef DTC_PUBLIC_RELEASE
dtc_wmaxi = dtc_maxi = 0;
#endif
#ifdef PAS16_PUBLIC_RELEASE
pas_wmaxi = pas_maxi = 0;
#endif
return(-ENOSYS);
}
SPRINTF("NCR5380 core release=%d.   ", NCR5380_PUBLIC_RELEASE);
if (((struct NCR5380_hostdata *)instance->hostdata)->flags & FLAG_NCR53C400)
SPRINTF("ncr53c400 release=%d.  ", NCR53C400_PUBLIC_RELEASE);
#ifdef DTC_PUBLIC_RELEASE
SPRINTF("DTC 3180/3280 release %d", DTC_PUBLIC_RELEASE);
#endif
#ifdef T128_PUBLIC_RELEASE
SPRINTF("T128 release %d", T128_PUBLIC_RELEASE);
#endif
#ifdef GENERIC_NCR5380_PUBLIC_RELEASE
SPRINTF("Generic5380 release %d", GENERIC_NCR5380_PUBLIC_RELEASE);
#endif
#ifdef PAS16_PUBLIC_RELEASE
SPRINTF("PAS16 release=%d", PAS16_PUBLIC_RELEASE);
#endif
SPRINTF("\nBase Addr: 0x%05lX    ", (long)instance->base);
SPRINTF("io_port: %04x      ", (int)instance->io_port);
if (instance->irq == IRQ_NONE)
SPRINTF("IRQ: None.\n");
else
SPRINTF("IRQ: %d.\n", instance->irq);
#ifdef DTC_PUBLIC_RELEASE
SPRINTF("Highwater I/O busy_spin_counts -- write: %d  read: %d\n",
dtc_wmaxi, dtc_maxi);
#endif
#ifdef PAS16_PUBLIC_RELEASE
SPRINTF("Highwater I/O busy_spin_counts -- write: %d  read: %d\n",
pas_wmaxi, pas_maxi);
#endif
cli();
SPRINTF("NCR5380 : coroutine is%s running.\n", main_running ? "" : "n't");
if (!hostdata->connected)
SPRINTF("scsi%d: no currently connected command\n", instance->host_no);
else
pos = lprint_Scsi_Cmnd ((Scsi_Cmnd *) hostdata->connected,
pos, buffer, length);
SPRINTF("scsi%d: issue_queue\n", instance->host_no);
for (ptr = (Scsi_Cmnd *) hostdata->issue_queue; ptr;
ptr = (Scsi_Cmnd *) ptr->host_scribble)
pos = lprint_Scsi_Cmnd (ptr, pos, buffer, length);
SPRINTF("scsi%d: disconnected_queue\n", instance->host_no);
for (ptr = (Scsi_Cmnd *) hostdata->disconnected_queue; ptr;
ptr = (Scsi_Cmnd *) ptr->host_scribble)
pos = lprint_Scsi_Cmnd (ptr, pos, buffer, length);
sti();
*start=buffer;
if (pos - buffer < offset)
return 0;
else if (pos - buffer - offset < length)
return pos - buffer - offset;
return length;
}
static
char *lprint_Scsi_Cmnd (Scsi_Cmnd *cmd, char *pos, char *buffer, int length) {
SPRINTF("scsi%d : destination target %d, lun %d\n",
cmd->host->host_no, cmd->target, cmd->lun);
SPRINTF("        command = ");
pos = lprint_command (cmd->cmnd, pos, buffer, length);
return (pos);
}
static
char *lprint_command (unsigned char *command,
char *pos, char *buffer, int length) {
int i, s;
pos = lprint_opcode(command[0], pos, buffer, length);
for ( i = 1, s = COMMAND_SIZE(command[0]); i < s; ++i)
SPRINTF("%02x ", command[i]);
SPRINTF("\n");
return(pos);
}
static
char *lprint_opcode(int opcode, char *pos, char *buffer, int length) {
SPRINTF("%2d (0x%02x)", opcode, opcode);
return(pos);
}
static void NCR5380_init (struct Scsi_Host *instance, int flags) {
NCR5380_local_declare();
int i, pass;
unsigned long timeout;
struct NCR5380_hostdata *hostdata = (struct NCR5380_hostdata *)
instance->hostdata;
#ifdef NCR53C400
if (flags & FLAG_NCR53C400)
instance->NCR5380_instance_name += NCR53C400_address_adjust;
#endif
NCR5380_setup(instance);
NCR5380_all_init();
hostdata->aborted = 0;
hostdata->id_mask = 1 << instance->this_id;
for (i = hostdata->id_mask; i <= 0x80; i <<= 1)
if (i > hostdata->id_mask)
hostdata->id_higher_mask |= i;
for (i = 0; i < 8; ++i)
hostdata->busy[i] = 0;
#ifdef REAL_DMA
hostdata->dmalen = 0;
#endif
hostdata->targets_present = 0;
hostdata->connected = NULL;
hostdata->issue_queue = NULL;
hostdata->disconnected_queue = NULL;
#ifdef NCR5380_STATS
for (i = 0; i < 8; ++i) {
hostdata->time_read[i] = 0;
hostdata->time_write[i] = 0;
hostdata->bytes_read[i] = 0;
hostdata->bytes_write[i] = 0;
}
hostdata->timebase = 0;
hostdata->pendingw = 0;
hostdata->pendingr = 0;
#endif
if (flags & FLAG_NCR53C400)
hostdata->flags = FLAG_HAS_LAST_BYTE_SENT | flags;
else
hostdata->flags = FLAG_CHECK_LAST_BYTE_SENT | flags;
if (!the_template) {
the_template = instance->hostt;
first_instance = instance;
}
#ifdef USLEEP
hostdata->time_expires = 0;
hostdata->next_timer = NULL;
#endif
#ifndef AUTOSENSE
if ((instance->cmd_per_lun > 1) || instance->can_queue > 1))
printk("scsi%d : WARNING : support for multiple outstanding commands enabled\n"
"         without AUTOSENSE option, contingent allegiance conditions may\n"
"         be incorrectly cleared.\n", instance->host_no);
#endif
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE);
NCR5380_write(MODE_REG, MR_BASE);
NCR5380_write(TARGET_COMMAND_REG, 0);
NCR5380_write(SELECT_ENABLE_REG, 0);
#ifdef NCR53C400
if (hostdata->flags & FLAG_NCR53C400) {
NCR5380_write(C400_CONTROL_STATUS_REG, CSR_BASE);
}
#endif
for (pass = 1; (NCR5380_read(STATUS_REG) & SR_BSY) &&
pass <= 6 ; ++pass) {
switch (pass) {
case 1:
case 3:
case 5:
printk("scsi%d: SCSI bus busy, waiting up to five seconds\n",
instance->host_no);
timeout = jiffies + 5*HZ;
while (jiffies < timeout && (NCR5380_read(STATUS_REG) & SR_BSY));
break;
case 2:
printk("scsi%d: bus busy, attempting abort\n",
instance->host_no);
do_abort (instance);
break;
case 4:
printk("scsi%d: bus busy, attempting reset\n",
instance->host_no);
do_reset (instance);
break;
case 6:
printk("scsi%d: bus locked solid or invalid override\n",
instance->host_no);
}
}
}
#ifndef NCR5380_queue_command
static
#endif
int NCR5380_queue_command (Scsi_Cmnd *cmd, void (*done)(Scsi_Cmnd *)) {
struct Scsi_Host *instance = cmd->host;
struct NCR5380_hostdata *hostdata = (struct NCR5380_hostdata *)
instance->hostdata;
Scsi_Cmnd *tmp;
#if (NDEBUG & NDEBUG_NO_WRITE)
switch (cmd->cmnd[0]) {
case WRITE_6:
case WRITE_10:
printk("scsi%d : WRITE attempted with NO_WRITE debugging flag set\n",
instance->host_no);
cmd->result = (DID_ERROR << 16);
done(cmd);
return 0;
}
#endif
#ifdef NCR5380_STATS
# if 0
if (!hostdata->connected && !hostdata->issue_queue &&
!hostdata->disconnected_queue) {
hostdata->timebase = jiffies;
}
# endif
# ifdef NCR5380_STAT_LIMIT
if (cmd->request_bufflen > NCR5380_STAT_LIMIT)
# endif
switch (cmd->cmnd[0])
{
case WRITE:
case WRITE_6:
case WRITE_10:
hostdata->time_write[cmd->target] -= (jiffies - hostdata->timebase);
hostdata->bytes_write[cmd->target] += cmd->request_bufflen;
hostdata->pendingw++;
break;
case READ:
case READ_6:
case READ_10:
hostdata->time_read[cmd->target] -= (jiffies - hostdata->timebase);
hostdata->bytes_read[cmd->target] += cmd->request_bufflen;
hostdata->pendingr++;
break;
}
#endif
cmd->host_scribble = NULL;
cmd->scsi_done = done;
cmd->result = 0;
cli();
if (!(hostdata->issue_queue) || (cmd->cmnd[0] == REQUEST_SENSE)) {
LIST(cmd, hostdata->issue_queue);
cmd->host_scribble = (unsigned char *) hostdata->issue_queue;
hostdata->issue_queue = cmd;
} else {
for (tmp = (Scsi_Cmnd *) hostdata->issue_queue; tmp->host_scribble;
tmp = (Scsi_Cmnd *) tmp->host_scribble);
LIST(cmd, tmp);
tmp->host_scribble = (unsigned char *) cmd;
}
#if (NDEBUG & NDEBUG_QUEUES)
printk("scsi%d : command added to %s of queue\n", instance->host_no,
(cmd->cmnd[0] == REQUEST_SENSE) ? "head" : "tail");
#endif
run_main();
return 0;
}
static void NCR5380_main (void) {
Scsi_Cmnd *tmp, *prev;
struct Scsi_Host *instance;
struct NCR5380_hostdata *hostdata;
int done;
do {
cli();
done = 1;
for (instance = first_instance; instance &&
instance->hostt == the_template; instance=instance->next) {
hostdata = (struct NCR5380_hostdata *) instance->hostdata;
cli();
if (!hostdata->connected) {
#if (NDEBUG & NDEBUG_MAIN)
printk("scsi%d : not connected\n", instance->host_no);
#endif
#if (NDEBUG & NDEBUG_LISTS)
for (tmp= (Scsi_Cmnd *) hostdata->issue_queue, prev=NULL; tmp && (tmp != prev); prev=tmp, tmp=(Scsi_Cmnd*)tmp->host_scribble)
;
if ((tmp == prev) && tmp) printk(" LOOP\n");
#endif
for (tmp = (Scsi_Cmnd *) hostdata->issue_queue,
prev = NULL; tmp; prev = tmp, tmp = (Scsi_Cmnd *)
tmp->host_scribble) {
#if (NDEBUG & NDEBUG_LISTS)
if (prev != tmp)
printk("MAIN tmp=%p   target=%d   busy=%d lun=%d\n", tmp, tmp->target, hostdata->busy[tmp->target], tmp->lun);
#endif
if (!(hostdata->busy[tmp->target] & (1 << tmp->lun))) {
if (prev) {
REMOVE(prev,prev->host_scribble,tmp,tmp->host_scribble);
prev->host_scribble = tmp->host_scribble;
} else {
REMOVE(-1,hostdata->issue_queue,tmp,tmp->host_scribble);
hostdata->issue_queue = (Scsi_Cmnd *) tmp->host_scribble;
}
tmp->host_scribble = NULL;
sti();
#if (NDEBUG & (NDEBUG_MAIN | NDEBUG_QUEUES))
printk("scsi%d : main() : command for target %d lun %d removed from issue_queue\n",
instance->host_no, tmp->target, tmp->lun);
#endif
if (!NCR5380_select(instance, tmp,
(tmp->cmnd[0] == REQUEST_SENSE) ? TAG_NONE :
TAG_NEXT)) {
break;
} else {
cli();
LIST(tmp, hostdata->issue_queue);
tmp->host_scribble = (unsigned char *)
hostdata->issue_queue;
hostdata->issue_queue = tmp;
done = 0;
sti();
#if (NDEBUG & (NDEBUG_MAIN | NDEBUG_QUEUES))
printk("scsi%d : main(): select() failed, returned to issue_queue\n",
instance->host_no);
#endif
}
}
}
}
if (hostdata->connected
#ifdef REAL_DMA
&& !hostdata->dmalen
#endif
#ifdef USLEEP
&& (!hostdata->time_expires || hostdata->time_expires >= jiffies)
#endif
) {
sti();
#if (NDEBUG & NDEBUG_MAIN)
printk("scsi%d : main() : performing information transfer\n",
instance->host_no);
#endif
NCR5380_information_transfer(instance);
#if (NDEBUG & NDEBUG_MAIN)
printk("scsi%d : main() : done set false\n", instance->host_no);
#endif
done = 0;
} else
break;
}
} while (!done);
main_running = 0;
}
#ifndef DONT_USE_INTR
static void NCR5380_intr (int irq, void *dev_id, struct pt_regs * regs) {
NCR5380_local_declare();
struct Scsi_Host *instance;
int done;
unsigned char basr;
#if (NDEBUG & NDEBUG_INTR)
printk("scsi : NCR5380 irq %d triggered\n", irq);
#endif
do {
done = 1;
for (instance = first_instance; instance && (instance->hostt ==
the_template); instance = instance->next)
if (instance->irq == irq) {
NCR5380_setup(instance);
basr = NCR5380_read(BUS_AND_STATUS_REG);
if (basr & BASR_IRQ) {
#if (NDEBUG & NDEBUG_INTR)
NCR5380_print(instance);
#endif
if ((NCR5380_read(STATUS_REG) & (SR_SEL | SR_IO)) ==
(SR_SEL | SR_IO)) {
done = 0;
sti();
#if (NDEBUG & NDEBUG_INTR)
printk("scsi%d : SEL interrupt\n", instance->host_no);
#endif
NCR5380_reselect(instance);
(void) NCR5380_read(RESET_PARITY_INTERRUPT_REG);
} else if (basr & BASR_PARITY_ERROR) {
#if (NDEBUG & NDEBUG_INTR)
printk("scsi%d : PARITY interrupt\n", instance->host_no);
#endif
(void) NCR5380_read(RESET_PARITY_INTERRUPT_REG);
} else if ((NCR5380_read(STATUS_REG) & SR_RST) == SR_RST) {
#if (NDEBUG & NDEBUG_INTR)
printk("scsi%d : RESET interrupt\n", instance->host_no);
#endif
(void)NCR5380_read(RESET_PARITY_INTERRUPT_REG);
} else {
#if defined(REAL_DMA)
if ((NCR5380_read(MODE_REG) & MR_DMA) && ((basr &
BASR_END_DMA_TRANSFER) ||
!(basr & BASR_PHASE_MATCH))) {
int transfered;
if (!hostdata->connected)
panic("scsi%d : received end of DMA interrupt with no connected cmd\n",
instance->hostno);
transfered = (hostdata->dmalen - NCR5380_dma_residual(instance));
hostdata->connected->SCp.this_residual -= transferred;
hostdata->connected->SCp.ptr += transferred;
hostdata->dmalen = 0;
(void) NCR5380_read(RESET_PARITY_INTERRUPT_REG);
#if NCR_TIMEOUT
{
unsigned long timeout = jiffies + NCR_TIMEOUT;
while (NCR5380_read(BUS_AND_STATUS_REG) & BASR_ACK
&& jiffies < timeout)
;
if (jiffies >= timeout)
printk("scsi%d: timeout at NCR5380.c:%d\n",
host->host_no, __LINE__);
}
#else
while (NCR5380_read(BUS_AND_STATUS_REG) & BASR_ACK);
#endif
NCR5380_write(MODE_REG, MR_BASE);
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE);
}
#else
#if (NDEBUG & NDEBUG_INTR)
printk("scsi : unknown interrupt, BASR 0x%X, MR 0x%X, SR 0x%x\n", basr, NCR5380_read(MODE_REG), NCR5380_read(STATUS_REG));
#endif
(void) NCR5380_read(RESET_PARITY_INTERRUPT_REG);
#endif
}
}
if (!done)
run_main();
}
} while (!done);
}
#endif
#ifdef NCR5380_STATS
static void collect_stats(struct NCR5380_hostdata* hostdata, Scsi_Cmnd* cmd)
{
# ifdef NCR5380_STAT_LIMIT
if (cmd->request_bufflen > NCR5380_STAT_LIMIT)
# endif
switch (cmd->cmnd[0])
{
case WRITE:
case WRITE_6:
case WRITE_10:
hostdata->time_write[cmd->target] += (jiffies - hostdata->timebase);
hostdata->pendingw--;
break;
case READ:
case READ_6:
case READ_10:
hostdata->time_read[cmd->target] += (jiffies - hostdata->timebase);
hostdata->pendingr--;
break;
}
}
#endif
static int NCR5380_select (struct Scsi_Host *instance, Scsi_Cmnd *cmd,
int tag) {
NCR5380_local_declare();
struct NCR5380_hostdata *hostdata = (struct NCR5380_hostdata*)
instance->hostdata;
unsigned char tmp[3], phase;
unsigned char *data;
int len;
unsigned long timeout;
NCR5380_setup(instance);
hostdata->restart_select = 0;
#if defined (NDEBUG) && (NDEBUG & NDEBUG_ARBITRATION)
NCR5380_print(instance);
printk("scsi%d : starting arbitration, id = %d\n", instance->host_no,
instance->this_id);
#endif
cli();
NCR5380_write(TARGET_COMMAND_REG, 0);
NCR5380_write(OUTPUT_DATA_REG, hostdata->id_mask);
NCR5380_write(MODE_REG, MR_ARBITRATE);
sti();
#if NCR_TIMEOUT
{
unsigned long timeout = jiffies + 2*NCR_TIMEOUT;
while (!(NCR5380_read(INITIATOR_COMMAND_REG) & ICR_ARBITRATION_PROGRESS)
&& jiffies < timeout)
;
if (jiffies >= timeout)
{
printk("scsi: arbitration timeout at %d\n", __LINE__);
NCR5380_write(MODE_REG, MR_BASE);
NCR5380_write(SELECT_ENABLE_REG, hostdata->id_mask);
return -1;
}
}
#else
while (!(NCR5380_read(INITIATOR_COMMAND_REG) & ICR_ARBITRATION_PROGRESS));
#endif
#if (NDEBUG & NDEBUG_ARBITRATION)
printk("scsi%d : arbitration complete\n", instance->host_no);
__asm__("nop");
#endif
udelay(3);
if ((NCR5380_read(INITIATOR_COMMAND_REG) & ICR_ARBITRATION_LOST) ||
(NCR5380_read(CURRENT_SCSI_DATA_REG) & hostdata->id_higher_mask) ||
(NCR5380_read(INITIATOR_COMMAND_REG) & ICR_ARBITRATION_LOST)) {
NCR5380_write(MODE_REG, MR_BASE);
#if (NDEBUG & NDEBUG_ARBITRATION)
printk("scsi%d : lost arbitration, deasserting MR_ARBITRATE\n",
instance->host_no);
#endif
return -1;
}
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE | ICR_ASSERT_SEL);
if (NCR5380_read(INITIATOR_COMMAND_REG) & ICR_ARBITRATION_LOST) {
NCR5380_write(MODE_REG, MR_BASE);
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE);
#if (NDEBUG & NDEBUG_ARBITRATION)
printk("scsi%d : lost arbitration, deasserting ICR_ASSERT_SEL\n",
instance->host_no);
#endif
return -1;
}
udelay(2);
#if (NDEBUG & NDEBUG_ARBITRATION)
printk("scsi%d : won arbitration\n", instance->host_no);
#endif
NCR5380_write(OUTPUT_DATA_REG, (hostdata->id_mask | (1 << cmd->target)));
NCR5380_write(INITIATOR_COMMAND_REG, (ICR_BASE | ICR_ASSERT_BSY |
ICR_ASSERT_DATA | ICR_ASSERT_ATN | ICR_ASSERT_SEL ));
NCR5380_write(MODE_REG, MR_BASE);
NCR5380_write(SELECT_ENABLE_REG, 0);
udelay(1);
NCR5380_write(INITIATOR_COMMAND_REG, (ICR_BASE | ICR_ASSERT_DATA |
ICR_ASSERT_ATN | ICR_ASSERT_SEL));
udelay(1);
#if (NDEBUG & NDEBUG_SELECTION)
printk("scsi%d : selecting target %d\n", instance->host_no, cmd->target);
#endif
timeout = jiffies + 250*HZ/1000;
while ((jiffies < timeout) && !(NCR5380_read(STATUS_REG) &
(SR_BSY | SR_IO)));
if ((NCR5380_read(STATUS_REG) & (SR_SEL | SR_IO)) ==
(SR_SEL | SR_IO)) {
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE);
NCR5380_reselect(instance);
printk ("scsi%d : reselection after won arbitration?\n",
instance->host_no);
NCR5380_write(SELECT_ENABLE_REG, hostdata->id_mask);
return -1;
}
udelay(1);
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE | ICR_ASSERT_ATN);
if (!(NCR5380_read(STATUS_REG) & SR_BSY)) {
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE);
if (hostdata->targets_present & (1 << cmd->target)) {
printk("scsi%d : weirdness\n", instance->host_no);
if (hostdata->restart_select)
printk("\trestart select\n");
#ifdef NDEBUG
NCR5380_print (instance);
#endif
NCR5380_write(SELECT_ENABLE_REG, hostdata->id_mask);
return -1;
}
cmd->result = DID_BAD_TARGET << 16;
#ifdef NCR5380_STATS
collect_stats(hostdata, cmd);
#endif
cmd->scsi_done(cmd);
NCR5380_write(SELECT_ENABLE_REG, hostdata->id_mask);
#if (NDEBUG & NDEBUG_SELECTION)
printk("scsi%d : target did not respond within 250ms\n",
instance->host_no);
#endif
NCR5380_write(SELECT_ENABLE_REG, hostdata->id_mask);
return 0;
}
hostdata->targets_present |= (1 << cmd->target);
#ifdef NCR_TIMEOUT
{
unsigned long timeout = jiffies + NCR_TIMEOUT;
while (!(NCR5380_read(STATUS_REG) & SR_REQ) && jiffies < timeout);
if (jiffies >= timeout) {
printk("scsi%d: timeout at NCR5380.c:%d\n", __LINE__);
NCR5380_write(SELECT_ENABLE_REG, hostdata->id_mask);
return -1;
}
}
#else
while (!(NCR5380_read(STATUS_REG) & SR_REQ));
#endif
#if (NDEBUG & NDEBUG_SELECTION)
printk("scsi%d : target %d selected, going into MESSAGE OUT phase.\n",
instance->host_no, cmd->target);
#endif
tmp[0] = IDENTIFY(((instance->irq == IRQ_NONE) ? 0 : 1), cmd->lun);
#ifdef SCSI2
if (cmd->device->tagged_queue && (tag != TAG_NONE)) {
tmp[1] = SIMPLE_QUEUE_TAG;
if (tag == TAG_NEXT) {
if (cmd->device->current_tag == 0)
cmd->device->current_tag = 1;
cmd->tag = cmd->device->current_tag;
cmd->device->current_tag++;
} else
cmd->tag = (unsigned char) tag;
tmp[2] = cmd->tag;
hostdata->last_message = SIMPLE_QUEUE_TAG;
len = 3;
} else
#endif
{
len = 1;
cmd->tag=0;
}
data = tmp;
phase = PHASE_MSGOUT;
NCR5380_transfer_pio(instance, &phase, &len, &data);
#if (NDEBUG & NDEBUG_SELECTION)
printk("scsi%d : nexus established.\n", instance->host_no);
#endif
hostdata->connected = cmd;
#ifdef SCSI2
if (!cmd->device->tagged_queue)
#endif
hostdata->busy[cmd->target] |= (1 << cmd->lun);
initialize_SCp(cmd);
return 0;
}
static int NCR5380_transfer_pio (struct Scsi_Host *instance,
unsigned char *phase, int *count, unsigned char **data) {
NCR5380_local_declare();
register unsigned char p = *phase, tmp;
register int c = *count;
register unsigned char *d = *data;
NCR5380_setup(instance);
#if (NDEBUG & NDEBUG_PIO)
if (!(p & SR_IO))
printk("scsi%d : pio write %d bytes\n", instance->host_no, c);
else
printk("scsi%d : pio read %d bytes\n", instance->host_no, c);
#endif
NCR5380_write(TARGET_COMMAND_REG, PHASE_SR_TO_TCR(p));
do {
while (!((tmp = NCR5380_read(STATUS_REG)) & SR_REQ));
#if (NDEBUG & NDEBUG_HANDSHAKE)
printk("scsi%d : REQ detected\n", instance->host_no);
#endif
if ((tmp & PHASE_MASK) != p) {
#if (NDEBUG & NDEBUG_PIO)
printk("scsi%d : phase mismatch\n", instance->host_no);
NCR5380_print_phase(instance);
#endif
break;
}
if (!(p & SR_IO))
NCR5380_write(OUTPUT_DATA_REG, *d);
else
*d = NCR5380_read(CURRENT_SCSI_DATA_REG);
++d;
if (!(p & SR_IO)) {
if (!((p & SR_MSG) && c > 1)) {
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE |
ICR_ASSERT_DATA);
#if (NDEBUG & NDEBUG_PIO)
NCR5380_print(instance);
#endif
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE |
ICR_ASSERT_DATA | ICR_ASSERT_ACK);
} else {
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE |
ICR_ASSERT_DATA | ICR_ASSERT_ATN);
#if (NDEBUG & NDEBUG_PIO)
NCR5380_print(instance);
#endif
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE |
ICR_ASSERT_DATA | ICR_ASSERT_ATN | ICR_ASSERT_ACK);
}
} else {
#if (NDEBUG & NDEBUG_PIO)
NCR5380_print(instance);
#endif
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE | ICR_ASSERT_ACK);
}
while (NCR5380_read(STATUS_REG) & SR_REQ);
#if (NDEBUG & NDEBUG_HANDSHAKE)
printk("scsi%d : req false, handshake complete\n", instance->host_no);
#endif
if (!(p == PHASE_MSGIN && c == 1)) {
if (p == PHASE_MSGOUT && c > 1)
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE | ICR_ASSERT_ATN);
else
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE);
}
} while (--c);
#if (NDEBUG & NDEBUG_PIO)
printk("scsi%d : residual %d\n", instance->host_no, c);
#endif
*count = c;
*data = d;
tmp = NCR5380_read(STATUS_REG);
if (tmp & SR_REQ)
*phase = tmp & PHASE_MASK;
else
*phase = PHASE_UNKNOWN;
if (!c || (*phase == p))
return 0;
else
return -1;
}
static void do_reset (struct Scsi_Host *host) {
NCR5380_local_declare();
NCR5380_setup(host);
cli();
NCR5380_write(TARGET_COMMAND_REG,
PHASE_SR_TO_TCR(NCR5380_read(STATUS_REG) & PHASE_MASK));
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE | ICR_ASSERT_RST);
udelay(25);
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE);
sti();
}
static int do_abort (struct Scsi_Host *host) {
NCR5380_local_declare();
unsigned char tmp, *msgptr, phase;
int len;
NCR5380_setup(host);
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE | ICR_ASSERT_ATN);
while (!((tmp = NCR5380_read(STATUS_REG)) & SR_REQ));
NCR5380_write(TARGET_COMMAND_REG, PHASE_SR_TO_TCR(tmp));
if ((tmp & PHASE_MASK) != PHASE_MSGOUT) {
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE | ICR_ASSERT_ATN |
ICR_ASSERT_ACK);
while (NCR5380_read(STATUS_REG) & SR_REQ);
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE | ICR_ASSERT_ATN);
}
tmp = ABORT;
msgptr = &tmp;
len = 1;
phase = PHASE_MSGOUT;
NCR5380_transfer_pio (host, &phase, &len, &msgptr);
return len ? -1 : 0;
}
#if defined(REAL_DMA) || defined(PSEUDO_DMA) || defined (REAL_DMA_POLL)
static int NCR5380_transfer_dma (struct Scsi_Host *instance,
unsigned char *phase, int *count, unsigned char **data) {
NCR5380_local_declare();
register int c = *count;
register unsigned char p = *phase;
register unsigned char *d = *data;
unsigned char tmp;
int foo;
#if defined(REAL_DMA_POLL)
int cnt, toPIO;
unsigned char saved_data = 0, overrun = 0, residue;
#endif
struct NCR5380_hostdata *hostdata = (struct NCR5380_hostdata *)
instance->hostdata;
NCR5380_setup(instance);
if ((tmp = (NCR5380_read(STATUS_REG) & PHASE_MASK)) != p) {
*phase = tmp;
return -1;
}
#if defined(REAL_DMA) || defined(REAL_DMA_POLL)
#ifdef READ_OVERRUNS
if (p & SR_IO) {
c -= 2;
}
#endif
#if (NDEBUG & NDEBUG_DMA)
printk("scsi%d : initializing DMA channel %d for %s, %d bytes %s %0x\n",
instance->host_no, instance->dma_channel, (p & SR_IO) ? "reading" :
"writing", c, (p & SR_IO) ? "to" : "from", (unsigned) d);
#endif
hostdata->dma_len = (p & SR_IO) ?
NCR5380_dma_read_setup(instance, d, c) :
NCR5380_dma_write_setup(instance, d, c);
#endif
NCR5380_write(TARGET_COMMAND_REG, PHASE_SR_TO_TCR(p));
#ifdef REAL_DMA
NCR5380_write(MODE_REG, MR_BASE | MR_DMA_MODE | MR_ENABLE_EOP_INTR | MR_MONITOR_BSY);
#elif defined(REAL_DMA_POLL)
NCR5380_write(MODE_REG, MR_BASE | MR_DMA_MODE);
#else
#if defined(PSEUDO_DMA) && !defined(UNSAFE)
cli();
#endif
if (hostdata->flags & FLAG_NCR53C400)
NCR5380_write(MODE_REG, MR_BASE | MR_DMA_MODE | MR_ENABLE_PAR_CHECK
| MR_ENABLE_PAR_INTR | MR_ENABLE_EOP_INTR | MR_DMA_MODE
| MR_MONITOR_BSY);
else
NCR5380_write(MODE_REG, MR_BASE | MR_DMA_MODE);
#endif
#if (NDEBUG & NDEBUG_DMA) & 0
printk("scsi%d : mode reg = 0x%X\n", instance->host_no, NCR5380_read(MODE_REG));
#endif
if (p & SR_IO) {
#ifndef FOO
udelay(1);
#endif
NCR5380_write(START_DMA_INITIATOR_RECEIVE_REG, 0);
} else {
#ifndef FOO
udelay(1);
#endif
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE | ICR_ASSERT_DATA);
#ifndef FOO
udelay(1);
#endif
NCR5380_write(START_DMA_SEND_REG, 0);
#ifndef FOO
udelay(1);
#endif
}
#if defined(REAL_DMA_POLL)
do {
tmp = NCR5380_read(BUS_AND_STATUS_REG);
} while ((tmp & BASR_PHASE_MATCH) && !(tmp & (BASR_BUSY_ERROR |
BASR_END_DMA_TRANSFER)));
if (p & SR_IO) {
#ifdef READ_OVERRUNS
udelay(10);
if (((NCR5380_read(BUS_AND_STATUS_REG) & (BASR_PHASE_MATCH|BASR_ACK)) ==
(BASR_PHASE_MATCH | BASR_ACK))) {
saved_data = NCR5380_read(INPUT_DATA_REGISTER);
overrun = 1;
}
#endif
} else {
int limit = 100;
while (((tmp = NCR5380_read(BUS_AND_STATUS_REG)) & BASR_ACK) ||
(NCR5380_read(STATUS_REG) & SR_REQ)) {
if (!(tmp & BASR_PHASE_MATCH)) break;
if (--limit < 0) break;
}
}
#if (NDEBUG & NDEBUG_DMA)
printk("scsi%d : polled DMA transfer complete, basr 0x%X, sr 0x%X\n",
instance->host_no, tmp, NCR5380_read(STATUS_REG));
#endif
NCR5380_write(MODE_REG, MR_BASE);
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE);
residue = NCR5380_dma_residual(instance);
c -= residue;
*count -= c;
*data += c;
*phase = NCR5380_read(STATUS_REG) & PHASE_MASK;
#ifdef READ_OVERRUNS
if (*phase == p && (p & SR_IO) && residue == 0) {
if (overrun) {
#if (NDEBUG & NDEBUG_DMA)
printk("Got an input overrun, using saved byte\n");
#endif
**data = saved_data;
*data += 1;
*count -= 1;
cnt = toPIO = 1;
} else {
printk("No overrun??\n");
cnt = toPIO = 2;
}
#if (NDEBUG & NDEBUG_DMA)
printk("Doing %d-byte PIO to 0x%X\n", cnt, *data);
#endif
NCR5380_transfer_pio(instance, phase, &cnt, data);
*count -= toPIO - cnt;
}
#endif
#if (NDEBUG & NDEBUG_DMA)
printk("Return with data ptr = 0x%X, count %d, last 0x%X, next 0x%X\n",
*data, *count, *(*data+*count-1), *(*data+*count));
#endif
return 0;
#elif defined(REAL_DMA)
return 0;
#else
if (p & SR_IO) {
#ifdef DMA_WORKS_RIGHT
foo = NCR5380_pread(instance, d, c);
#else
int diff = 1;
if (hostdata->flags & FLAG_NCR53C400) {
diff=0;
}
if (!(foo = NCR5380_pread(instance, d, c - diff))) {
if (!(hostdata->flags & FLAG_NCR53C400)) {
while (!(NCR5380_read(BUS_AND_STATUS_REG) & BASR_DRQ));
while (NCR5380_read(STATUS_REG) & SR_REQ);
d[c - 1] = NCR5380_read(INPUT_DATA_REG);
}
}
#endif
} else {
#ifdef DMA_WORKS_RIGHT
foo = NCR5380_pwrite(instance, d, c);
#else
int timeout;
#if (NDEBUG & NDEBUG_C400_PWRITE)
printk("About to pwrite %d bytes\n", c);
#endif
if (!(foo = NCR5380_pwrite(instance, d, c))) {
if (!(hostdata->flags & FLAG_HAS_LAST_BYTE_SENT)) {
timeout = 20000;
#if 1
#if 1
while (!(NCR5380_read(BUS_AND_STATUS_REG) &
BASR_DRQ) && (NCR5380_read(BUS_AND_STATUS_REG) &
BASR_PHASE_MATCH));
#else
if (NCR5380_read(STATUS_REG) & SR_REQ) {
for (; timeout &&
!(NCR5380_read(BUS_AND_STATUS_REG) & BASR_ACK);
--timeout);
for (; timeout && (NCR5380_read(STATUS_REG) & SR_REQ);
--timeout);
}
#endif
#if (NDEBUG & NDEBUG_LAST_BYTE_SENT)
if (!timeout)
printk("scsi%d : timed out on last byte\n",
instance->host_no);
#endif
if (hostdata->flags & FLAG_CHECK_LAST_BYTE_SENT) {
hostdata->flags &= ~FLAG_CHECK_LAST_BYTE_SENT;
if (NCR5380_read(TARGET_COMMAND_REG) & TCR_LAST_BYTE_SENT) {
hostdata->flags |= FLAG_HAS_LAST_BYTE_SENT;
#if (NDEBUG & NDEBUG_LAST_BYTE_SENT)
printk("scsi%d : last bit sent works\n",
instance->host_no);
#endif
}
}
} else  {
#if (NDEBUG & NDEBUG_C400_PWRITE)
printk("Waiting for LASTBYTE\n");
#endif
while (!(NCR5380_read(TARGET_COMMAND_REG) & TCR_LAST_BYTE_SENT));
#if (NDEBUG & NDEBUG_C400_PWRITE)
printk("Got LASTBYTE\n");
#endif
}
#else
udelay (5);
#endif
}
#endif
}
NCR5380_write(MODE_REG, MR_BASE);
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE);
if ((!(p & SR_IO)) && (hostdata->flags & FLAG_NCR53C400)) {
#if (NDEBUG & NDEBUG_C400_PWRITE)
printk("53C400w: Checking for IRQ\n");
#endif
if (NCR5380_read(BUS_AND_STATUS_REG) & BASR_IRQ) {
#if (NDEBUG & NDEBUG_C400_PWRITE)
printk("53C400w:    got it, reading reset interrupt reg\n");
#endif
NCR5380_read(RESET_PARITY_INTERRUPT_REG);
} else {
printk("53C400w:    IRQ NOT THERE!\n");
}
}
*data = d + c;
*count = 0;
*phase = NCR5380_read(STATUS_REG) & PHASE_MASK;
#if 0
NCR5380_print_phase(instance);
#endif
#if defined(PSEUDO_DMA) && !defined(UNSAFE)
sti();
#endif
return foo;
#endif
}
#endif
static void NCR5380_information_transfer (struct Scsi_Host *instance) {
NCR5380_local_declare();
struct NCR5380_hostdata *hostdata = (struct NCR5380_hostdata *)
instance->hostdata;
unsigned char msgout = NOP;
int sink = 0;
int len;
#if defined(PSEUDO_DMA) || defined(REAL_DMA_POLL)
int transfersize;
#endif
unsigned char *data;
unsigned char phase, tmp, extended_msg[10], old_phase=0xff;
Scsi_Cmnd *cmd = (Scsi_Cmnd *) hostdata->connected;
NCR5380_setup(instance);
while (1) {
tmp = NCR5380_read(STATUS_REG);
if (tmp & SR_REQ) {
phase = (tmp & PHASE_MASK);
if (phase != old_phase) {
old_phase = phase;
#if (NDEBUG & NDEBUG_INFORMATION)
NCR5380_print_phase(instance);
#endif
}
if (sink && (phase != PHASE_MSGOUT)) {
NCR5380_write(TARGET_COMMAND_REG, PHASE_SR_TO_TCR(tmp));
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE | ICR_ASSERT_ATN |
ICR_ASSERT_ACK);
while (NCR5380_read(STATUS_REG) & SR_REQ);
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE |
ICR_ASSERT_ATN);
sink = 0;
continue;
}
switch (phase) {
case PHASE_DATAIN:
case PHASE_DATAOUT:
#if (NDEBUG & NDEBUG_NO_DATAOUT)
printk("scsi%d : NDEBUG_NO_DATAOUT set, attempted DATAOUT aborted\n",
instance->host_no);
sink = 1;
do_abort(instance);
cmd->result = DID_ERROR  << 16;
cmd->done(cmd);
return;
#endif
if (!cmd->SCp.this_residual && cmd->SCp.buffers_residual) {
++cmd->SCp.buffer;
--cmd->SCp.buffers_residual;
cmd->SCp.this_residual = cmd->SCp.buffer->length;
cmd->SCp.ptr = cmd->SCp.buffer->address;
#if (NDEBUG & NDEBUG_INFORMATION)
printk("scsi%d : %d bytes and %d buffers left\n",
instance->host_no, cmd->SCp.this_residual,
cmd->SCp.buffers_residual);
#endif
}
#if defined(PSEUDO_DMA) || defined(REAL_DMA_POLL)
#ifdef NCR5380_dma_xfer_len
if (!cmd->device->borken &&
!(hostdata->flags & FLAG_NO_PSEUDO_DMA) &&
(transfersize = NCR5380_dma_xfer_len(instance, cmd)) != 0) {
#else
transfersize = cmd->transfersize;
#ifdef LIMIT_TRANSFERSIZE
if( transfersize > 512 )
transfersize = 512;
#endif
if (!cmd->device->borken && transfersize &&
!(hostdata->flags & FLAG_NO_PSEUDO_DMA) &&
cmd->SCp.this_residual && !(cmd->SCp.this_residual %
transfersize)) {
if (transfersize > 32*1024)
transfersize = 32*1024;
#endif
len = transfersize;
if (NCR5380_transfer_dma(instance, &phase,
&len, (unsigned char **) &cmd->SCp.ptr)) {
printk("scsi%d : switching target %d lun %d to slow handshake\n",
instance->host_no, cmd->target, cmd->lun);
cmd->device->borken = 1;
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE |
ICR_ASSERT_ATN);
sink = 1;
do_abort(instance);
cmd->result = DID_ERROR  << 16;
cmd->done(cmd);
} else
cmd->SCp.this_residual -= transfersize - len;
} else
#endif
NCR5380_transfer_pio(instance, &phase,
(int *) &cmd->SCp.this_residual, (unsigned char **)
&cmd->SCp.ptr);
break;
case PHASE_MSGIN:
len = 1;
data = &tmp;
NCR5380_transfer_pio(instance, &phase, &len, &data);
cmd->SCp.Message = tmp;
switch (tmp) {
#ifdef LINKED
case LINKED_CMD_COMPLETE:
case LINKED_FLG_CMD_COMPLETE:
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE);
#if (NDEBUG & NDEBUG_LINKED)
printk("scsi%d : target %d lun %d linked command complete.\n",
instance->host_no, cmd->target, cmd->lun);
#endif
if (!cmd->next_link) {
printk("scsi%d : target %d lun %d linked command complete, no next_link\n"
instance->host_no, cmd->target, cmd->lun);
sink = 1;
do_abort (instance);
return;
}
initialize_SCp(cmd->next_link);
cmd->next_link->tag = cmd->tag;
cmd->result = cmd->SCp.Status | (cmd->SCp.Message << 8);
#if (NDEBUG & NDEBUG_LINKED)
printk("scsi%d : target %d lun %d linked request done, calling scsi_done().\n",
instance->host_no, cmd->target, cmd->lun);
#endif
#ifdef NCR5380_STATS
collect_stats(hostdata, cmd);
#endif
cmd->scsi_done(cmd);
cmd = hostdata->connected;
break;
#endif
case ABORT:
case COMMAND_COMPLETE:
sink = 1;
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE);
hostdata->connected = NULL;
#if (NDEBUG & NDEBUG_QUEUES)
printk("scsi%d : command for target %d, lun %d completed\n",
instance->host_no, cmd->target, cmd->lun);
#endif
hostdata->busy[cmd->target] &= ~(1 << cmd->lun);
if (cmd->cmnd[0] != REQUEST_SENSE)
cmd->result = cmd->SCp.Status | (cmd->SCp.Message << 8);
else if (cmd->SCp.Status != GOOD)
cmd->result = (cmd->result & 0x00ffff) | (DID_ERROR << 16);
#ifdef AUTOSENSE
if ((cmd->cmnd[0] != REQUEST_SENSE) &&
(cmd->SCp.Status == CHECK_CONDITION)) {
#if (NDEBUG & NDEBUG_AUTOSENSE)
printk("scsi%d : performing request sense\n",
instance->host_no);
#endif
cmd->cmnd[0] = REQUEST_SENSE;
cmd->cmnd[1] &= 0xe0;
cmd->cmnd[2] = 0;
cmd->cmnd[3] = 0;
cmd->cmnd[4] = sizeof(cmd->sense_buffer);
cmd->cmnd[5] = 0;
cmd->SCp.buffer = NULL;
cmd->SCp.buffers_residual = 0;
cmd->SCp.ptr = (char *) cmd->sense_buffer;
cmd->SCp.this_residual = sizeof(cmd->sense_buffer);
cli();
LIST(cmd,hostdata->issue_queue);
cmd->host_scribble = (unsigned char *)
hostdata->issue_queue;
hostdata->issue_queue = (Scsi_Cmnd *) cmd;
sti();
#if (NDEBUG & NDEBUG_QUEUES)
printk("scsi%d : REQUEST SENSE added to head of issue queue\n",instance->host_no);
#endif
} else {
#endif
#ifdef NCR5380_STATS
collect_stats(hostdata, cmd);
#endif
cmd->scsi_done(cmd);
}
NCR5380_write(SELECT_ENABLE_REG, hostdata->id_mask);
NCR5380_write(TARGET_COMMAND_REG, 0);
while ((NCR5380_read(STATUS_REG) & SR_BSY) && !hostdata->connected)
barrier();
return;
case MESSAGE_REJECT:
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE);
switch (hostdata->last_message) {
case HEAD_OF_QUEUE_TAG:
case ORDERED_QUEUE_TAG:
case SIMPLE_QUEUE_TAG:
cmd->device->tagged_queue = 0;
hostdata->busy[cmd->target] |= (1 << cmd->lun);
break;
default:
break;
}
case DISCONNECT:
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE);
cmd->device->disconnect = 1;
cli();
LIST(cmd,hostdata->disconnected_queue);
cmd->host_scribble = (unsigned char *)
hostdata->disconnected_queue;
hostdata->connected = NULL;
hostdata->disconnected_queue = cmd;
sti();
#if (NDEBUG & NDEBUG_QUEUES)
printk("scsi%d : command for target %d lun %d was moved from connected to"
"  the disconnected_queue\n", instance->host_no,
cmd->target, cmd->lun);
#endif
NCR5380_write(TARGET_COMMAND_REG, 0);
NCR5380_write(SELECT_ENABLE_REG, hostdata->id_mask);
while ((NCR5380_read(STATUS_REG) & SR_BSY) && !hostdata->connected)
barrier();
#if 0
NCR5380_print_status(instance);
#endif
return;
case SAVE_POINTERS:
case RESTORE_POINTERS:
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE);
break;
case EXTENDED_MESSAGE:
extended_msg[0] = EXTENDED_MESSAGE;
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE);
#if (NDEBUG & NDEBUG_EXTENDED)
printk("scsi%d : receiving extended message\n",
instance->host_no);
#endif
len = 2;
data = extended_msg + 1;
phase = PHASE_MSGIN;
NCR5380_transfer_pio(instance, &phase, &len, &data);
#if (NDEBUG & NDEBUG_EXTENDED)
printk("scsi%d : length=%d, code=0x%02x\n",
instance->host_no, (int) extended_msg[1],
(int) extended_msg[2]);
#endif
if (!len && extended_msg[1] <=
(sizeof (extended_msg) - 1)) {
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE);
len = extended_msg[1] - 1;
data = extended_msg + 3;
phase = PHASE_MSGIN;
NCR5380_transfer_pio(instance, &phase, &len, &data);
#if (NDEBUG & NDEBUG_EXTENDED)
printk("scsi%d : message received, residual %d\n",
instance->host_no, len);
#endif
switch (extended_msg[2]) {
case EXTENDED_SDTR:
case EXTENDED_WDTR:
case EXTENDED_MODIFY_DATA_POINTER:
case EXTENDED_EXTENDED_IDENTIFY:
tmp = 0;
}
} else if (len) {
printk("scsi%d: error receiving extended message\n",
instance->host_no);
tmp = 0;
} else {
printk("scsi%d: extended message code %02x length %d is too long\n",
instance->host_no, extended_msg[2], extended_msg[1]);
tmp = 0;
}
default:
if (!tmp) {
printk("scsi%d: rejecting message ", instance->host_no);
print_msg (extended_msg);
printk("\n");
} else if (tmp != EXTENDED_MESSAGE)
printk("scsi%d: rejecting unknown message %02x from target %d, lun %d\n",
instance->host_no, tmp, cmd->target, cmd->lun);
else
printk("scsi%d: rejecting unknown extended message code %02x, length %d from target %d, lun %d\n",
instance->host_no, extended_msg[1], extended_msg[0], cmd->target, cmd->lun);
msgout = MESSAGE_REJECT;
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE |
ICR_ASSERT_ATN);
break;
}
break;
case PHASE_MSGOUT:
len = 1;
data = &msgout;
hostdata->last_message = msgout;
NCR5380_transfer_pio(instance, &phase, &len, &data);
if (msgout == ABORT) {
hostdata->busy[cmd->target] &= ~(1 << cmd->lun);
hostdata->connected = NULL;
cmd->result = DID_ERROR << 16;
#ifdef NCR5380_STATS
collect_stats(hostdata, cmd);
#endif
cmd->scsi_done(cmd);
NCR5380_write(SELECT_ENABLE_REG, hostdata->id_mask);
return;
}
msgout = NOP;
break;
case PHASE_CMDOUT:
len = cmd->cmd_len;
data = cmd->cmnd;
NCR5380_transfer_pio(instance, &phase, &len,
&data);
#ifdef USLEEP
if (!disconnect && should_disconnect(cmd->cmnd[0])) {
hostdata->time_expires = jiffies + USLEEP_SLEEP;
#if (NDEBUG & NDEBUG_USLEEP)
printk("scsi%d : issued command, sleeping until %ul\n", instance->host_no,
hostdata->time_expires);
#endif
NCR5380_set_timer (instance);
return;
}
#endif
break;
case PHASE_STATIN:
len = 1;
data = &tmp;
NCR5380_transfer_pio(instance, &phase, &len, &data);
cmd->SCp.Status = tmp;
break;
default:
printk("scsi%d : unknown phase\n", instance->host_no);
#ifdef NDEBUG
NCR5380_print(instance);
#endif
}
}
#ifdef USLEEP
else {
if (!disconnect && hostdata->time_expires && jiffies >
hostdata->time_expires) {
hostdata->time_expires = jiffies + USLEEP_SLEEP;
#if (NDEBUG & NDEBUG_USLEEP)
printk("scsi%d : poll timed out, sleeping until %ul\n", instance->host_no,
hostdata->time_expires);
#endif
NCR5380_set_timer (instance);
return;
}
}
#endif
}
}
static void NCR5380_reselect (struct Scsi_Host *instance) {
NCR5380_local_declare();
struct NCR5380_hostdata *hostdata = (struct NCR5380_hostdata *)
instance->hostdata;
unsigned char target_mask;
unsigned char lun, phase;
int len;
#ifdef SCSI2
unsigned char tag;
#endif
unsigned char msg[3];
unsigned char *data;
Scsi_Cmnd *tmp = NULL, *prev;
int abort = 0;
NCR5380_setup(instance);
NCR5380_write(MODE_REG, MR_BASE);
hostdata->restart_select = 1;
target_mask = NCR5380_read(CURRENT_SCSI_DATA_REG) & ~(hostdata->id_mask);
#if (NDEBUG & NDEBUG_RESELECTION)
printk("scsi%d : reselect\n", instance->host_no);
#endif
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE | ICR_ASSERT_BSY);
while (NCR5380_read(STATUS_REG) & SR_SEL);
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE);
while (!(NCR5380_read(STATUS_REG) & SR_REQ));
len = 1;
data = msg;
phase = PHASE_MSGIN;
NCR5380_transfer_pio(instance, &phase, &len, &data);
if (!(msg[0] & 0x80)) {
printk("scsi%d : expecting IDENTIFY message, got ",
instance->host_no);
print_msg(msg);
abort = 1;
} else {
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE);
lun = (msg[0] & 0x07);
#ifdef SCSI2
#error "SCSI-II tagged queueing is not supported yet"
#endif
for (tmp = (Scsi_Cmnd *) hostdata->disconnected_queue, prev = NULL;
tmp; prev = tmp, tmp = (Scsi_Cmnd *) tmp->host_scribble)
if ((target_mask == (1 << tmp->target)) && (lun == tmp->lun)
#ifdef SCSI2
&& (tag == tmp->tag)
#endif
) {
if (prev) {
REMOVE(prev,prev->host_scribble,tmp,tmp->host_scribble);
prev->host_scribble = tmp->host_scribble;
} else {
REMOVE(-1,hostdata->disconnected_queue,tmp,tmp->host_scribble);
hostdata->disconnected_queue = (Scsi_Cmnd *) tmp->host_scribble;
}
tmp->host_scribble = NULL;
break;
}
if (!tmp) {
#ifdef SCSI2
printk("scsi%d : warning : target bitmask %02x lun %d tag %d not in disconnect_queue.\n",
instance->host_no, target_mask, lun, tag);
#else
printk("scsi%d : warning : target bitmask %02x lun %d not in disconnect_queue.\n",
instance->host_no, target_mask, lun);
#endif
abort = 1;
}
}
if (abort) {
do_abort (instance);
} else {
hostdata->connected = tmp;
#if (NDEBUG & NDEBUG_RESELECTION)
printk("scsi%d : nexus established, target = %d, lun = %d, tag = %d\n",
instance->host_no, tmp->target, tmp->lun, tmp->tag);
#endif
}
}
#ifdef REAL_DMA
static void NCR5380_dma_complete (NCR5380_instance *instance) {
NCR5380_local_declare();
struct NCR5380_hostdata *hostdata = (struct NCR5380_hostdata *
instance->hostdata);
int transferred;
NCR5380_setup(instance);
while (NCR5380_read(BUS_AND_STATUS_REG) & BASR_ACK);
NCR5380_write(MODE_REG, MR_BASE);
NCR5380_write(INITIATOR_COMMAND_REG, ICR_BASE);
if (!(hostdata->connected->SCp.phase & SR_CD)) {
transferred = instance->dmalen - NCR5380_dma_residual();
hostdata->connected->SCp.this_residual -= transferred;
hostdata->connected->SCp.ptr += transferred;
}
}
#endif
#ifndef NCR5380_abort
static
#endif
int NCR5380_abort (Scsi_Cmnd *cmd) {
NCR5380_local_declare();
struct Scsi_Host *instance = cmd->host;
struct NCR5380_hostdata *hostdata = (struct NCR5380_hostdata *)
instance->hostdata;
Scsi_Cmnd *tmp, **prev;
printk("scsi%d : aborting command\n", instance->host_no);
print_Scsi_Cmnd (cmd);
NCR5380_print_status (instance);
printk("scsi%d : aborting command\n", instance->host_no);
print_Scsi_Cmnd (cmd);
NCR5380_print_status (instance);
cli();
NCR5380_setup(instance);
#if (NDEBUG & NDEBUG_ABORT)
printk("scsi%d : abort called\n", instance->host_no);
printk("        basr 0x%X, sr 0x%X\n",
NCR5380_read(BUS_AND_STATUS_REG), NCR5380_read(STATUS_REG));
#endif
#if 0
if (hostdata->connected == cmd) {
#if (NDEBUG & NDEBUG_ABORT)
printk("scsi%d : aborting connected command\n", instance->host_no);
#endif
hostdata->aborted = 1;
NCR5380_write(INITIATOR_COMMAND_REG, ICR_ASSERT_ATN);
return 0;
}
#endif
#if (NDEBUG & NDEBUG_ABORT)
printk("scsi%d : abort going into loop.\n", instance->host_no);
#endif
for (prev = (Scsi_Cmnd **) &(hostdata->issue_queue),
tmp = (Scsi_Cmnd *) hostdata->issue_queue;
tmp; prev = (Scsi_Cmnd **) &(tmp->host_scribble), tmp =
(Scsi_Cmnd *) tmp->host_scribble)
if (cmd == tmp) {
REMOVE(5,*prev,tmp,tmp->host_scribble);
(*prev) = (Scsi_Cmnd *) tmp->host_scribble;
tmp->host_scribble = NULL;
tmp->result = DID_ABORT << 16;
sti();
#if (NDEBUG & NDEBUG_ABORT)
printk("scsi%d : abort removed command from issue queue.\n",
instance->host_no);
#endif
tmp->done(tmp);
return SCSI_ABORT_SUCCESS;
}
#if (NDEBUG  & NDEBUG_ABORT)
else if (prev == tmp) printk("scsi%d : LOOP\n", instance->host_no);
#endif
if (hostdata->connected) {
sti();
#if (NDEBUG & NDEBUG_ABORT)
printk("scsi%d : abort failed, command connected.\n", instance->host_no);
#endif
return SCSI_ABORT_NOT_RUNNING;
}
for (tmp = (Scsi_Cmnd *) hostdata->disconnected_queue; tmp;
tmp = (Scsi_Cmnd *) tmp->host_scribble)
if (cmd == tmp) {
sti();
#if (NDEBUG & NDEBUG_ABORT)
printk("scsi%d : aborting disconnected command.\n", instance->host_no);
#endif
if (NCR5380_select (instance, cmd, (int) cmd->tag))
return SCSI_ABORT_BUSY;
#if (NDEBUG & NDEBUG_ABORT)
printk("scsi%d : nexus reestablished.\n", instance->host_no);
#endif
do_abort (instance);
cli();
for (prev = (Scsi_Cmnd **) &(hostdata->disconnected_queue),
tmp = (Scsi_Cmnd *) hostdata->disconnected_queue;
tmp; prev = (Scsi_Cmnd **) &(tmp->host_scribble), tmp =
(Scsi_Cmnd *) tmp->host_scribble)
if (cmd == tmp) {
REMOVE(5,*prev,tmp,tmp->host_scribble);
*prev = (Scsi_Cmnd *) tmp->host_scribble;
tmp->host_scribble = NULL;
tmp->result = DID_ABORT << 16;
sti();
tmp->done(tmp);
return SCSI_ABORT_SUCCESS;
}
}
sti();
printk("scsi%d : warning : SCSI command probably completed successfully\n"
"         before abortion\n", instance->host_no);
return SCSI_ABORT_NOT_RUNNING;
}
#ifndef NCR5380_reset
static
#endif
int NCR5380_reset (Scsi_Cmnd *cmd, unsigned int dummy) {
NCR5380_local_declare();
NCR5380_setup(cmd->host);
NCR5380_print_status (cmd->host);
do_reset (cmd->host);
return SCSI_RESET_WAKEUP;
}