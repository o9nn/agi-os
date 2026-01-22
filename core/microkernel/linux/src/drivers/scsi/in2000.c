#include <linux/module.h>
#include <asm/system.h>
#include <linux/sched.h>
#include <linux/string.h>
#include <linux/delay.h>
#include <linux/proc_fs.h>
#include <asm/io.h>
#include <linux/ioport.h>
#include <linux/blkdev.h>
#include <linux/blk.h>
#include <linux/stat.h>
#include "scsi.h"
#include "sd.h"
#include "hosts.h"
#define IN2000_VERSION "1.33"
#define IN2000_DATE "26/August/1998"
#include "in2000.h"
static char *setup_args[] =
{"","","","","","","","",""};
static char *setup_strings = 0;
#ifdef MODULE_PARM
MODULE_PARM(setup_strings, "s");
#endif
static struct Scsi_Host *instance_list = 0;
static inline uchar read_3393(struct IN2000_hostdata *hostdata, uchar reg_num)
{
write1_io(reg_num,IO_WD_ADDR);
return read1_io(IO_WD_DATA);
}
#define READ_AUX_STAT() read1_io(IO_WD_ASR)
static inline void write_3393(struct IN2000_hostdata *hostdata, uchar reg_num, uchar value)
{
write1_io(reg_num,IO_WD_ADDR);
write1_io(value,IO_WD_DATA);
}
static inline void write_3393_cmd(struct IN2000_hostdata *hostdata, uchar cmd)
{
write1_io(WD_COMMAND,IO_WD_ADDR);
write1_io(cmd,IO_WD_DATA);
}
static uchar read_1_byte(struct IN2000_hostdata *hostdata)
{
uchar asr, x = 0;
write_3393(hostdata,WD_CONTROL, CTRL_IDI | CTRL_EDI | CTRL_POLLED);
write_3393_cmd(hostdata,WD_CMD_TRANS_INFO|0x80);
do {
asr = READ_AUX_STAT();
if (asr & ASR_DBR)
x = read_3393(hostdata,WD_DATA);
} while (!(asr & ASR_INT));
return x;
}
static void write_3393_count(struct IN2000_hostdata *hostdata, unsigned long value)
{
write1_io(WD_TRANSFER_COUNT_MSB,IO_WD_ADDR);
write1_io((value >> 16),IO_WD_DATA);
write1_io((value >> 8),IO_WD_DATA);
write1_io(value,IO_WD_DATA);
}
static unsigned long read_3393_count(struct IN2000_hostdata *hostdata)
{
unsigned long value;
write1_io(WD_TRANSFER_COUNT_MSB,IO_WD_ADDR);
value = read1_io(IO_WD_DATA) << 16;
value |= read1_io(IO_WD_DATA) << 8;
value |= read1_io(IO_WD_DATA);
return value;
}
static int is_dir_out(Scsi_Cmnd *cmd)
{
switch (cmd->cmnd[0]) {
case WRITE_6: case WRITE_10: case WRITE_12:
case WRITE_LONG: case WRITE_SAME: case WRITE_BUFFER:
case WRITE_VERIFY: case WRITE_VERIFY_12:
case COMPARE: case COPY: case COPY_VERIFY:
case SEARCH_EQUAL: case SEARCH_HIGH: case SEARCH_LOW:
case SEARCH_EQUAL_12: case SEARCH_HIGH_12: case SEARCH_LOW_12:
case FORMAT_UNIT: case REASSIGN_BLOCKS: case RESERVE:
case MODE_SELECT: case MODE_SELECT_10: case LOG_SELECT:
case SEND_DIAGNOSTIC: case CHANGE_DEFINITION: case UPDATE_BLOCK:
case SET_WINDOW: case MEDIUM_SCAN: case SEND_VOLUME_TAG:
case 0xea:
return 1;
default:
return 0;
}
}
static struct sx_period sx_table[] = {
{ 1, 0x20},
{252, 0x20},
{376, 0x30},
{500, 0x40},
{624, 0x50},
{752, 0x60},
{876, 0x70},
{1000,0x00},
{0, 0} };
static int round_period(unsigned int period)
{
int x;
for (x=1; sx_table[x].period_ns; x++) {
if ((period <= sx_table[x-0].period_ns) &&
(period > sx_table[x-1].period_ns)) {
return x;
}
}
return 7;
}
static uchar calc_sync_xfer(unsigned int period, unsigned int offset)
{
uchar result;
period *= 4;
result = sx_table[round_period(period)].reg_value;
result |= (offset < OPTIMUM_SX_OFF)?offset:OPTIMUM_SX_OFF;
return result;
}
static void in2000_execute(struct Scsi_Host *instance);
int in2000_queuecommand (Scsi_Cmnd *cmd, void (*done)(Scsi_Cmnd *))
{
struct IN2000_hostdata *hostdata;
Scsi_Cmnd *tmp;
unsigned long flags;
hostdata = (struct IN2000_hostdata *)cmd->host->hostdata;
DB(DB_QUEUE_COMMAND,printk("Q-%d-%02x-%ld(",cmd->target,cmd->cmnd[0],cmd->pid))
cmd->host_scribble = NULL;
cmd->scsi_done = done;
cmd->result = 0;
if (cmd->use_sg) {
cmd->SCp.buffer = (struct scatterlist *)cmd->buffer;
cmd->SCp.buffers_residual = cmd->use_sg - 1;
cmd->SCp.ptr = (char *)cmd->SCp.buffer->address;
cmd->SCp.this_residual = cmd->SCp.buffer->length;
}
else {
cmd->SCp.buffer = NULL;
cmd->SCp.buffers_residual = 0;
cmd->SCp.ptr = (char *)cmd->request_buffer;
cmd->SCp.this_residual = cmd->request_bufflen;
}
cmd->SCp.have_data_in = 0;
cmd->SCp.Status = ILLEGAL_STATUS_BYTE;
save_flags(flags);
cli();
if (!(hostdata->input_Q) || (cmd->cmnd[0] == REQUEST_SENSE)) {
cmd->host_scribble = (uchar *)hostdata->input_Q;
hostdata->input_Q = cmd;
}
else {
for (tmp=(Scsi_Cmnd *)hostdata->input_Q; tmp->host_scribble;
tmp=(Scsi_Cmnd *)tmp->host_scribble)
;
tmp->host_scribble = (uchar *)cmd;
}
in2000_execute(cmd->host);
DB(DB_QUEUE_COMMAND,printk(")Q-%ld ",cmd->pid))
restore_flags(flags);
return 0;
}
static void in2000_execute (struct Scsi_Host *instance)
{
struct IN2000_hostdata *hostdata;
Scsi_Cmnd *cmd, *prev;
int i;
unsigned short *sp;
unsigned short f;
unsigned short flushbuf[16];
hostdata = (struct IN2000_hostdata *)instance->hostdata;
DB(DB_EXECUTE,printk("EX("))
if (hostdata->selecting || hostdata->connected) {
DB(DB_EXECUTE,printk(")EX-0 "))
return;
}
cmd = (Scsi_Cmnd *)hostdata->input_Q;
prev = 0;
while (cmd) {
if (!(hostdata->busy[cmd->target] & (1 << cmd->lun)))
break;
prev = cmd;
cmd = (Scsi_Cmnd *)cmd->host_scribble;
}
if (!cmd) {
DB(DB_EXECUTE,printk(")EX-1 "))
return;
}
if (prev)
prev->host_scribble = cmd->host_scribble;
else
hostdata->input_Q = (Scsi_Cmnd *)cmd->host_scribble;
#ifdef PROC_STATISTICS
hostdata->cmd_cnt[cmd->target]++;
#endif
if (is_dir_out(cmd))
write_3393(hostdata,WD_DESTINATION_ID, cmd->target);
else
write_3393(hostdata,WD_DESTINATION_ID, cmd->target | DSTID_DPD);
cmd->SCp.phase = 0;
if (hostdata->disconnect == DIS_NEVER)
goto no;
if (hostdata->disconnect == DIS_ALWAYS)
goto yes;
if (cmd->device->type == 1)
goto yes;
if (hostdata->disconnected_Q)
goto yes;
if (!(hostdata->input_Q))
goto no;
for (prev=(Scsi_Cmnd *)hostdata->input_Q; prev;
prev=(Scsi_Cmnd *)prev->host_scribble) {
if ((prev->target != cmd->target) || (prev->lun != cmd->lun)) {
for (prev=(Scsi_Cmnd *)hostdata->input_Q; prev;
prev=(Scsi_Cmnd *)prev->host_scribble)
prev->SCp.phase = 1;
goto yes;
}
}
goto no;
yes:
cmd->SCp.phase = 1;
#ifdef PROC_STATISTICS
hostdata->disc_allowed_cnt[cmd->target]++;
#endif
no:
write_3393(hostdata,WD_SOURCE_ID,((cmd->SCp.phase)?SRCID_ER:0));
write_3393(hostdata,WD_TARGET_LUN, cmd->lun);
write_3393(hostdata,WD_SYNCHRONOUS_TRANSFER,hostdata->sync_xfer[cmd->target]);
hostdata->busy[cmd->target] |= (1 << cmd->lun);
if ((hostdata->level2 <= L2_NONE) ||
(hostdata->sync_stat[cmd->target] == SS_UNSET)) {
hostdata->selecting = cmd;
if (hostdata->sync_stat[cmd->target] == SS_UNSET) {
if (hostdata->sync_off & (1 << cmd->target))
hostdata->sync_stat[cmd->target] = SS_SET;
else
hostdata->sync_stat[cmd->target] = SS_FIRST;
}
hostdata->state = S_SELECTING;
write_3393_count(hostdata,0);
write_3393_cmd(hostdata,WD_CMD_SEL_ATN);
}
else {
hostdata->connected = cmd;
write_3393(hostdata,WD_COMMAND_PHASE, 0);
write1_io(WD_CDB_1, IO_WD_ADDR);
for (i=0; i<cmd->cmd_len; i++)
write1_io(cmd->cmnd[i], IO_WD_DATA);
write_3393(hostdata, WD_OWN_ID, cmd->cmd_len);
if (!(cmd->SCp.phase)) {
write_3393_count(hostdata, cmd->SCp.this_residual);
write_3393(hostdata,WD_CONTROL, CTRL_IDI | CTRL_EDI | CTRL_BUS);
write1_io(0, IO_FIFO_WRITE);
if (is_dir_out(cmd)) {
hostdata->fifo = FI_FIFO_WRITING;
if ((i = cmd->SCp.this_residual) > (IN2000_FIFO_SIZE - 16) )
i = IN2000_FIFO_SIZE - 16;
cmd->SCp.have_data_in = i;
i >>= 1;
sp = (unsigned short *)cmd->SCp.ptr;
f = hostdata->io_base + IO_FIFO;
#ifdef FAST_WRITE_IO
FAST_WRITE2_IO();
#else
while (i--)
write2_io(*sp++,IO_FIFO);
#endif
if (cmd->SCp.have_data_in <= ((IN2000_FIFO_SIZE - 16) - 32)) {
sp = flushbuf;
i = 16;
#ifdef FAST_WRITE_IO
FAST_WRITE2_IO();
#else
while (i--)
write2_io(0,IO_FIFO);
#endif
}
}
else {
write1_io(0, IO_FIFO_READ);
hostdata->fifo = FI_FIFO_READING;
cmd->SCp.have_data_in = 0;
}
}
else {
write_3393_count(hostdata,0);
}
hostdata->state = S_RUNNING_LEVEL2;
write_3393_cmd(hostdata,WD_CMD_SEL_ATN_XFER);
}
DB(DB_EXECUTE,printk("%s%ld)EX-2 ",(cmd->SCp.phase)?"d:":"",cmd->pid))
}
static void transfer_pio(uchar *buf, int cnt,
int data_in_dir, struct IN2000_hostdata *hostdata)
{
uchar asr;
DB(DB_TRANSFER,printk("(%p,%d,%s)",buf,cnt,data_in_dir?"in":"out"))
write_3393(hostdata,WD_CONTROL, CTRL_IDI | CTRL_EDI | CTRL_POLLED);
write_3393_count(hostdata,cnt);
write_3393_cmd(hostdata,WD_CMD_TRANS_INFO);
if (data_in_dir) {
do {
asr = READ_AUX_STAT();
if (asr & ASR_DBR)
*buf++ = read_3393(hostdata,WD_DATA);
} while (!(asr & ASR_INT));
}
else {
do {
asr = READ_AUX_STAT();
if (asr & ASR_DBR)
write_3393(hostdata,WD_DATA, *buf++);
} while (!(asr & ASR_INT));
}
}
static void transfer_bytes(Scsi_Cmnd *cmd, int data_in_dir)
{
struct IN2000_hostdata *hostdata;
unsigned short *sp;
unsigned short f;
int i;
hostdata = (struct IN2000_hostdata *)cmd->host->hostdata;
if (!cmd->SCp.this_residual && cmd->SCp.buffers_residual) {
++cmd->SCp.buffer;
--cmd->SCp.buffers_residual;
cmd->SCp.this_residual = cmd->SCp.buffer->length;
cmd->SCp.ptr = cmd->SCp.buffer->address;
}
write_3393(hostdata,WD_SYNCHRONOUS_TRANSFER,hostdata->sync_xfer[cmd->target]);
write_3393_count(hostdata,cmd->SCp.this_residual);
write_3393(hostdata,WD_CONTROL, CTRL_IDI | CTRL_EDI | CTRL_BUS);
write1_io(0,IO_FIFO_WRITE);
if (data_in_dir) {
write1_io(0,IO_FIFO_READ);
if ((hostdata->level2 >= L2_DATA) ||
(hostdata->level2 == L2_BASIC && cmd->SCp.phase == 0)) {
write_3393(hostdata,WD_COMMAND_PHASE,0x45);
write_3393_cmd(hostdata,WD_CMD_SEL_ATN_XFER);
hostdata->state = S_RUNNING_LEVEL2;
}
else
write_3393_cmd(hostdata,WD_CMD_TRANS_INFO);
hostdata->fifo = FI_FIFO_READING;
cmd->SCp.have_data_in = 0;
return;
}
if ((hostdata->level2 >= L2_DATA) ||
(hostdata->level2 == L2_BASIC && cmd->SCp.phase == 0)) {
write_3393(hostdata,WD_COMMAND_PHASE,0x45);
write_3393_cmd(hostdata,WD_CMD_SEL_ATN_XFER);
hostdata->state = S_RUNNING_LEVEL2;
}
else
write_3393_cmd(hostdata,WD_CMD_TRANS_INFO);
hostdata->fifo = FI_FIFO_WRITING;
sp = (unsigned short *)cmd->SCp.ptr;
if ((i = cmd->SCp.this_residual) > IN2000_FIFO_SIZE)
i = IN2000_FIFO_SIZE;
cmd->SCp.have_data_in = i;
i >>= 1;
f = hostdata->io_base + IO_FIFO;
#ifdef FAST_WRITE_IO
FAST_WRITE2_IO();
#else
while (i--)
write2_io(*sp++,IO_FIFO);
#endif
}
static void in2000_intr (int irqnum, void * dev_id, struct pt_regs *ptregs)
{
struct Scsi_Host *instance;
struct IN2000_hostdata *hostdata;
Scsi_Cmnd *patch, *cmd;
uchar asr, sr, phs, id, lun, *ucp, msg;
int i,j;
unsigned long length;
unsigned short *sp;
unsigned short f;
unsigned long flags;
for (instance = instance_list; instance; instance = instance->next) {
if (instance->irq == irqnum)
break;
}
if (!instance) {
printk("*** Hmm... interrupts are screwed up! ***\n");
return;
}
hostdata = (struct IN2000_hostdata *)instance->hostdata;
CLISPIN_LOCK(flags);
#ifdef PROC_STATISTICS
hostdata->int_cnt++;
#endif
write1_io(0, IO_LED_ON);
asr = READ_AUX_STAT();
if (!(asr & ASR_INT)) {
cmd = (Scsi_Cmnd *)hostdata->connected;
CHECK_NULL(cmd,"fifo_int")
if (hostdata->fifo == FI_FIFO_READING) {
DB(DB_FIFO,printk("{R:%02x} ",read1_io(IO_FIFO_COUNT)))
sp = (unsigned short *)(cmd->SCp.ptr + cmd->SCp.have_data_in);
i = read1_io(IO_FIFO_COUNT) & 0xfe;
i <<= 2;
f = hostdata->io_base + IO_FIFO;
#ifdef FAST_READ_IO
FAST_READ2_IO();
#else
while (i--)
*sp++ = read2_io(IO_FIFO);
#endif
i = sp - (unsigned short *)(cmd->SCp.ptr + cmd->SCp.have_data_in);
i <<= 1;
cmd->SCp.have_data_in += i;
}
else if (hostdata->fifo == FI_FIFO_WRITING) {
DB(DB_FIFO,printk("{W:%02x} ",read1_io(IO_FIFO_COUNT)))
if (cmd->SCp.this_residual == cmd->SCp.have_data_in) {
i = 16;
while (i--)
write2_io(0,IO_FIFO);
}
else {
sp = (unsigned short *)(cmd->SCp.ptr + cmd->SCp.have_data_in);
i = cmd->SCp.this_residual - cmd->SCp.have_data_in;
j = read1_io(IO_FIFO_COUNT) & 0xfe;
j <<= 2;
if ((j << 1) > i)
j = (i >> 1);
while (j--)
write2_io(*sp++,IO_FIFO);
i = sp - (unsigned short *)(cmd->SCp.ptr + cmd->SCp.have_data_in);
i <<= 1;
cmd->SCp.have_data_in += i;
}
}
else {
printk("*** Spurious FIFO interrupt ***");
}
write1_io(0, IO_LED_OFF);
CLISPIN_UNLOCK(flags);
return;
}
cmd = (Scsi_Cmnd *)hostdata->connected;
sr = read_3393(hostdata,WD_SCSI_STATUS);
phs = read_3393(hostdata,WD_COMMAND_PHASE);
if (!cmd && (sr != CSR_RESEL_AM && sr != CSR_TIMEOUT && sr != CSR_SELECT)) {
printk("\nNR:wd-intr-1\n");
write1_io(0, IO_LED_OFF);
CLISPIN_UNLOCK(flags);
return;
}
DB(DB_INTR,printk("{%02x:%02x-",asr,sr))
if (hostdata->fifo == FI_FIFO_READING) {
sp = (unsigned short *)(cmd->SCp.ptr + cmd->SCp.have_data_in);
i = (cmd->SCp.this_residual - read_3393_count(hostdata)) - cmd->SCp.have_data_in;
i >>= 1;
f = hostdata->io_base + IO_FIFO;
#ifdef FAST_READ_IO
FAST_READ2_IO();
#else
while (i--)
*sp++ = read2_io(IO_FIFO);
#endif
hostdata->fifo = FI_FIFO_UNUSED;
length = cmd->SCp.this_residual;
cmd->SCp.this_residual = read_3393_count(hostdata);
cmd->SCp.ptr += (length - cmd->SCp.this_residual);
DB(DB_TRANSFER,printk("(%p,%d)",cmd->SCp.ptr,cmd->SCp.this_residual))
}
else if (hostdata->fifo == FI_FIFO_WRITING) {
hostdata->fifo = FI_FIFO_UNUSED;
length = cmd->SCp.this_residual;
cmd->SCp.this_residual = read_3393_count(hostdata);
cmd->SCp.ptr += (length - cmd->SCp.this_residual);
DB(DB_TRANSFER,printk("(%p,%d)",cmd->SCp.ptr,cmd->SCp.this_residual))
}
switch (sr) {
case CSR_TIMEOUT:
DB(DB_INTR,printk("TIMEOUT"))
if (hostdata->state == S_RUNNING_LEVEL2)
hostdata->connected = NULL;
else {
cmd = (Scsi_Cmnd *)hostdata->selecting;
CHECK_NULL(cmd,"csr_timeout")
hostdata->selecting = NULL;
}
cmd->result = DID_NO_CONNECT << 16;
hostdata->busy[cmd->target] &= ~(1 << cmd->lun);
hostdata->state = S_UNCONNECTED;
cmd->scsi_done(cmd);
in2000_execute(instance);
break;
case CSR_SELECT:
DB(DB_INTR,printk("SELECT"))
hostdata->connected = cmd = (Scsi_Cmnd *)hostdata->selecting;
CHECK_NULL(cmd,"csr_select")
hostdata->selecting = NULL;
hostdata->outgoing_msg[0] = (0x80 | 0x00 | cmd->lun);
if (cmd->SCp.phase)
hostdata->outgoing_msg[0] |= 0x40;
if (hostdata->sync_stat[cmd->target] == SS_FIRST) {
#ifdef SYNC_DEBUG
printk(" sending SDTR ");
#endif
hostdata->sync_stat[cmd->target] = SS_WAITING;
hostdata->outgoing_msg[1] = EXTENDED_MESSAGE;
hostdata->outgoing_msg[2] = 3;
hostdata->outgoing_msg[3] = EXTENDED_SDTR;
hostdata->outgoing_msg[4] = OPTIMUM_SX_PER/4;
hostdata->outgoing_msg[5] = OPTIMUM_SX_OFF;
hostdata->outgoing_len = 6;
}
else
hostdata->outgoing_len = 1;
hostdata->state = S_CONNECTED;
break;
case CSR_XFER_DONE|PHS_DATA_IN:
case CSR_UNEXP |PHS_DATA_IN:
case CSR_SRV_REQ |PHS_DATA_IN:
DB(DB_INTR,printk("IN-%d.%d",cmd->SCp.this_residual,cmd->SCp.buffers_residual))
transfer_bytes(cmd, DATA_IN_DIR);
if (hostdata->state != S_RUNNING_LEVEL2)
hostdata->state = S_CONNECTED;
break;
case CSR_XFER_DONE|PHS_DATA_OUT:
case CSR_UNEXP |PHS_DATA_OUT:
case CSR_SRV_REQ |PHS_DATA_OUT:
DB(DB_INTR,printk("OUT-%d.%d",cmd->SCp.this_residual,cmd->SCp.buffers_residual))
transfer_bytes(cmd, DATA_OUT_DIR);
if (hostdata->state != S_RUNNING_LEVEL2)
hostdata->state = S_CONNECTED;
break;
case CSR_XFER_DONE|PHS_COMMAND:
case CSR_UNEXP |PHS_COMMAND:
case CSR_SRV_REQ |PHS_COMMAND:
DB(DB_INTR,printk("CMND-%02x,%ld",cmd->cmnd[0],cmd->pid))
transfer_pio(cmd->cmnd, cmd->cmd_len, DATA_OUT_DIR, hostdata);
hostdata->state = S_CONNECTED;
break;
case CSR_XFER_DONE|PHS_STATUS:
case CSR_UNEXP |PHS_STATUS:
case CSR_SRV_REQ |PHS_STATUS:
DB(DB_INTR,printk("STATUS="))
cmd->SCp.Status = read_1_byte(hostdata);
DB(DB_INTR,printk("%02x",cmd->SCp.Status))
if (hostdata->level2 >= L2_BASIC) {
sr = read_3393(hostdata,WD_SCSI_STATUS);
hostdata->state = S_RUNNING_LEVEL2;
write_3393(hostdata,WD_COMMAND_PHASE, 0x50);
write_3393_cmd(hostdata,WD_CMD_SEL_ATN_XFER);
}
else {
hostdata->state = S_CONNECTED;
}
break;
case CSR_XFER_DONE|PHS_MESS_IN:
case CSR_UNEXP |PHS_MESS_IN:
case CSR_SRV_REQ |PHS_MESS_IN:
DB(DB_INTR,printk("MSG_IN="))
msg = read_1_byte(hostdata);
sr = read_3393(hostdata,WD_SCSI_STATUS);
hostdata->incoming_msg[hostdata->incoming_ptr] = msg;
if (hostdata->incoming_msg[0] == EXTENDED_MESSAGE)
msg = EXTENDED_MESSAGE;
else
hostdata->incoming_ptr = 0;
cmd->SCp.Message = msg;
switch (msg) {
case COMMAND_COMPLETE:
DB(DB_INTR,printk("CCMP-%ld",cmd->pid))
write_3393_cmd(hostdata,WD_CMD_NEGATE_ACK);
hostdata->state = S_PRE_CMP_DISC;
break;
case SAVE_POINTERS:
DB(DB_INTR,printk("SDP"))
write_3393_cmd(hostdata,WD_CMD_NEGATE_ACK);
hostdata->state = S_CONNECTED;
break;
case RESTORE_POINTERS:
DB(DB_INTR,printk("RDP"))
if (hostdata->level2 >= L2_BASIC) {
write_3393(hostdata,WD_COMMAND_PHASE, 0x45);
write_3393_cmd(hostdata,WD_CMD_SEL_ATN_XFER);
hostdata->state = S_RUNNING_LEVEL2;
}
else {
write_3393_cmd(hostdata,WD_CMD_NEGATE_ACK);
hostdata->state = S_CONNECTED;
}
break;
case DISCONNECT:
DB(DB_INTR,printk("DIS"))
cmd->device->disconnect = 1;
write_3393_cmd(hostdata,WD_CMD_NEGATE_ACK);
hostdata->state = S_PRE_TMP_DISC;
break;
case MESSAGE_REJECT:
DB(DB_INTR,printk("REJ"))
#ifdef SYNC_DEBUG
printk("-REJ-");
#endif
if (hostdata->sync_stat[cmd->target] == SS_WAITING)
hostdata->sync_stat[cmd->target] = SS_SET;
write_3393_cmd(hostdata,WD_CMD_NEGATE_ACK);
hostdata->state = S_CONNECTED;
break;
case EXTENDED_MESSAGE:
DB(DB_INTR,printk("EXT"))
ucp = hostdata->incoming_msg;
#ifdef SYNC_DEBUG
printk("%02x",ucp[hostdata->incoming_ptr]);
#endif
if ((hostdata->incoming_ptr >= 2) &&
(hostdata->incoming_ptr == (ucp[1] + 1))) {
switch (ucp[2]) {
case EXTENDED_SDTR:
id = calc_sync_xfer(ucp[3],ucp[4]);
if (hostdata->sync_stat[cmd->target] != SS_WAITING) {
write_3393_cmd(hostdata,WD_CMD_ASSERT_ATN);
hostdata->outgoing_msg[0] = EXTENDED_MESSAGE;
hostdata->outgoing_msg[1] = 3;
hostdata->outgoing_msg[2] = EXTENDED_SDTR;
hostdata->outgoing_msg[3] = hostdata->default_sx_per/4;
hostdata->outgoing_msg[4] = 0;
hostdata->outgoing_len = 5;
hostdata->sync_xfer[cmd->target] =
calc_sync_xfer(hostdata->default_sx_per/4,0);
}
else {
hostdata->sync_xfer[cmd->target] = id;
}
#ifdef SYNC_DEBUG
printk("sync_xfer=%02x",hostdata->sync_xfer[cmd->target]);
#endif
hostdata->sync_stat[cmd->target] = SS_SET;
write_3393_cmd(hostdata,WD_CMD_NEGATE_ACK);
hostdata->state = S_CONNECTED;
break;
case EXTENDED_WDTR:
write_3393_cmd(hostdata,WD_CMD_ASSERT_ATN);
printk("sending WDTR ");
hostdata->outgoing_msg[0] = EXTENDED_MESSAGE;
hostdata->outgoing_msg[1] = 2;
hostdata->outgoing_msg[2] = EXTENDED_WDTR;
hostdata->outgoing_msg[3] = 0;
hostdata->outgoing_len = 4;
write_3393_cmd(hostdata,WD_CMD_NEGATE_ACK);
hostdata->state = S_CONNECTED;
break;
default:
write_3393_cmd(hostdata,WD_CMD_ASSERT_ATN);
printk("Rejecting Unknown Extended Message(%02x). ",ucp[2]);
hostdata->outgoing_msg[0] = MESSAGE_REJECT;
hostdata->outgoing_len = 1;
write_3393_cmd(hostdata,WD_CMD_NEGATE_ACK);
hostdata->state = S_CONNECTED;
break;
}
hostdata->incoming_ptr = 0;
}
else {
hostdata->incoming_ptr++;
write_3393_cmd(hostdata,WD_CMD_NEGATE_ACK);
hostdata->state = S_CONNECTED;
}
break;
default:
printk("Rejecting Unknown Message(%02x) ",msg);
write_3393_cmd(hostdata,WD_CMD_ASSERT_ATN);
hostdata->outgoing_msg[0] = MESSAGE_REJECT;
hostdata->outgoing_len = 1;
write_3393_cmd(hostdata,WD_CMD_NEGATE_ACK);
hostdata->state = S_CONNECTED;
}
break;
case CSR_SEL_XFER_DONE:
write_3393(hostdata,WD_SOURCE_ID, SRCID_ER);
if (phs == 0x60) {
DB(DB_INTR,printk("SX-DONE-%ld",cmd->pid))
cmd->SCp.Message = COMMAND_COMPLETE;
lun = read_3393(hostdata,WD_TARGET_LUN);
DB(DB_INTR,printk(":%d.%d",cmd->SCp.Status,lun))
hostdata->connected = NULL;
hostdata->busy[cmd->target] &= ~(1 << cmd->lun);
hostdata->state = S_UNCONNECTED;
if (cmd->SCp.Status == ILLEGAL_STATUS_BYTE)
cmd->SCp.Status = lun;
if (cmd->cmnd[0] == REQUEST_SENSE && cmd->SCp.Status != GOOD)
cmd->result = (cmd->result & 0x00ffff) | (DID_ERROR << 16);
else
cmd->result = cmd->SCp.Status | (cmd->SCp.Message << 8);
cmd->scsi_done(cmd);
in2000_execute(instance);
}
else {
printk("%02x:%02x:%02x-%ld: Unknown SEL_XFER_DONE phase!!---",asr,sr,phs,cmd->pid);
}
break;
case CSR_SDP:
DB(DB_INTR,printk("SDP"))
hostdata->state = S_RUNNING_LEVEL2;
write_3393(hostdata,WD_COMMAND_PHASE, 0x41);
write_3393_cmd(hostdata,WD_CMD_SEL_ATN_XFER);
break;
case CSR_XFER_DONE|PHS_MESS_OUT:
case CSR_UNEXP |PHS_MESS_OUT:
case CSR_SRV_REQ |PHS_MESS_OUT:
DB(DB_INTR,printk("MSG_OUT="))
if (hostdata->outgoing_len == 0) {
hostdata->outgoing_len = 1;
hostdata->outgoing_msg[0] = NOP;
}
transfer_pio(hostdata->outgoing_msg, hostdata->outgoing_len,
DATA_OUT_DIR, hostdata);
DB(DB_INTR,printk("%02x",hostdata->outgoing_msg[0]))
hostdata->outgoing_len = 0;
hostdata->state = S_CONNECTED;
break;
case CSR_UNEXP_DISC:
write_3393(hostdata,WD_SOURCE_ID, SRCID_ER);
if (cmd == NULL) {
printk(" - Already disconnected! ");
hostdata->state = S_UNCONNECTED;
CLISPIN_UNLOCK(flags);
return;
}
DB(DB_INTR,printk("UNEXP_DISC-%ld",cmd->pid))
hostdata->connected = NULL;
hostdata->busy[cmd->target] &= ~(1 << cmd->lun);
hostdata->state = S_UNCONNECTED;
if (cmd->cmnd[0] == REQUEST_SENSE && cmd->SCp.Status != GOOD)
cmd->result = (cmd->result & 0x00ffff) | (DID_ERROR << 16);
else
cmd->result = cmd->SCp.Status | (cmd->SCp.Message << 8);
cmd->scsi_done(cmd);
in2000_execute(instance);
break;
case CSR_DISC:
write_3393(hostdata,WD_SOURCE_ID, SRCID_ER);
DB(DB_INTR,printk("DISC-%ld",cmd->pid))
if (cmd == NULL) {
printk(" - Already disconnected! ");
hostdata->state = S_UNCONNECTED;
}
switch (hostdata->state) {
case S_PRE_CMP_DISC:
hostdata->connected = NULL;
hostdata->busy[cmd->target] &= ~(1 << cmd->lun);
hostdata->state = S_UNCONNECTED;
DB(DB_INTR,printk(":%d",cmd->SCp.Status))
if (cmd->cmnd[0] == REQUEST_SENSE && cmd->SCp.Status != GOOD)
cmd->result = (cmd->result & 0x00ffff) | (DID_ERROR << 16);
else
cmd->result = cmd->SCp.Status | (cmd->SCp.Message << 8);
cmd->scsi_done(cmd);
break;
case S_PRE_TMP_DISC:
case S_RUNNING_LEVEL2:
cmd->host_scribble = (uchar *)hostdata->disconnected_Q;
hostdata->disconnected_Q = cmd;
hostdata->connected = NULL;
hostdata->state = S_UNCONNECTED;
#ifdef PROC_STATISTICS
hostdata->disc_done_cnt[cmd->target]++;
#endif
break;
default:
printk("*** Unexpected DISCONNECT interrupt! ***");
hostdata->state = S_UNCONNECTED;
}
in2000_execute(instance);
break;
case CSR_RESEL_AM:
DB(DB_INTR,printk("RESEL"))
if (hostdata->level2 <= L2_NONE) {
if (hostdata->selecting) {
cmd = (Scsi_Cmnd *)hostdata->selecting;
hostdata->selecting = NULL;
hostdata->busy[cmd->target] &= ~(1 << cmd->lun);
cmd->host_scribble = (uchar *)hostdata->input_Q;
hostdata->input_Q = cmd;
}
}
else {
if (cmd) {
if (phs == 0x00) {
hostdata->busy[cmd->target] &= ~(1 << cmd->lun);
cmd->host_scribble = (uchar *)hostdata->input_Q;
hostdata->input_Q = cmd;
}
else {
printk("---%02x:%02x:%02x-TROUBLE: Intrusive ReSelect!---",asr,sr,phs);
while (1)
printk("\r");
}
}
}
id = read_3393(hostdata,WD_SOURCE_ID);
id &= SRCID_MASK;
lun = read_3393(hostdata,WD_DATA);
if (hostdata->level2 < L2_RESELECT)
write_3393_cmd(hostdata,WD_CMD_NEGATE_ACK);
lun &= 7;
cmd = (Scsi_Cmnd *)hostdata->disconnected_Q;
patch = NULL;
while (cmd) {
if (id == cmd->target && lun == cmd->lun)
break;
patch = cmd;
cmd = (Scsi_Cmnd *)cmd->host_scribble;
}
if (!cmd) {
printk("---TROUBLE: target %d.%d not in disconnect queue---",id,lun);
break;
}
if (patch)
patch->host_scribble = cmd->host_scribble;
else
hostdata->disconnected_Q = (Scsi_Cmnd *)cmd->host_scribble;
hostdata->connected = cmd;
if (is_dir_out(cmd))
write_3393(hostdata,WD_DESTINATION_ID,cmd->target);
else
write_3393(hostdata,WD_DESTINATION_ID,cmd->target | DSTID_DPD);
if (hostdata->level2 >= L2_RESELECT) {
write_3393_count(hostdata,0);
write_3393(hostdata,WD_COMMAND_PHASE, 0x45);
write_3393_cmd(hostdata,WD_CMD_SEL_ATN_XFER);
hostdata->state = S_RUNNING_LEVEL2;
}
else
hostdata->state = S_CONNECTED;
DB(DB_INTR,printk("-%ld",cmd->pid))
break;
default:
printk("--UNKNOWN INTERRUPT:%02x:%02x:%02x--",asr,sr,phs);
}
write1_io(0, IO_LED_OFF);
DB(DB_INTR,printk("} "))
CLISPIN_UNLOCK(flags);
}
#define RESET_CARD 0
#define RESET_CARD_AND_BUS 1
#define B_FLAG 0x80
static int reset_hardware(struct Scsi_Host *instance, int type)
{
struct IN2000_hostdata *hostdata;
int qt,x;
unsigned long flags;
hostdata = (struct IN2000_hostdata *)instance->hostdata;
write1_io(0, IO_LED_ON);
if (type == RESET_CARD_AND_BUS) {
write1_io(0,IO_CARD_RESET);
x = read1_io(IO_HARDWARE);
}
x = read_3393(hostdata,WD_SCSI_STATUS);
write_3393(hostdata,WD_OWN_ID, instance->this_id |
OWNID_EAF | OWNID_RAF | OWNID_FS_8);
write_3393(hostdata,WD_CONTROL, CTRL_IDI | CTRL_EDI | CTRL_POLLED);
write_3393(hostdata,WD_SYNCHRONOUS_TRANSFER,
calc_sync_xfer(hostdata->default_sx_per/4,DEFAULT_SX_OFF));
save_flags(flags);
cli();
write1_io(0,IO_FIFO_WRITE);
write1_io(0,IO_FIFO_READ);
write_3393(hostdata,WD_COMMAND, WD_CMD_RESET);
while (!(READ_AUX_STAT() & ASR_INT))
;
x = read_3393(hostdata,WD_SCSI_STATUS);
restore_flags(flags);
write_3393(hostdata,WD_QUEUE_TAG,0xa5);
qt = read_3393(hostdata,WD_QUEUE_TAG);
if (qt == 0xa5) {
x |= B_FLAG;
write_3393(hostdata,WD_QUEUE_TAG,0);
}
write_3393(hostdata,WD_TIMEOUT_PERIOD, TIMEOUT_PERIOD_VALUE);
write_3393(hostdata,WD_CONTROL, CTRL_IDI | CTRL_EDI | CTRL_POLLED);
write1_io(0, IO_LED_OFF);
return x;
}
int in2000_reset(Scsi_Cmnd *cmd, unsigned int reset_flags)
{
unsigned long flags;
struct Scsi_Host *instance;
struct IN2000_hostdata *hostdata;
int x;
instance = cmd->host;
hostdata = (struct IN2000_hostdata *)instance->hostdata;
printk("scsi%d: Reset. ", instance->host_no);
save_flags(flags);
cli();
reset_hardware(instance, RESET_CARD_AND_BUS);
for (x = 0; x < 8; x++) {
hostdata->busy[x] = 0;
hostdata->sync_xfer[x] = calc_sync_xfer(DEFAULT_SX_PER/4,DEFAULT_SX_OFF);
hostdata->sync_stat[x] = SS_UNSET;
}
hostdata->input_Q = NULL;
hostdata->selecting = NULL;
hostdata->connected = NULL;
hostdata->disconnected_Q = NULL;
hostdata->state = S_UNCONNECTED;
hostdata->fifo = FI_FIFO_UNUSED;
hostdata->incoming_ptr = 0;
hostdata->outgoing_len = 0;
cmd->result = DID_RESET << 16;
restore_flags(flags);
return 0;
}
int in2000_abort (Scsi_Cmnd *cmd)
{
struct Scsi_Host *instance;
struct IN2000_hostdata *hostdata;
Scsi_Cmnd *tmp, *prev;
unsigned long flags;
uchar sr, asr;
unsigned long timeout;
save_flags (flags);
cli();
instance = cmd->host;
hostdata = (struct IN2000_hostdata *)instance->hostdata;
printk ("scsi%d: Abort-", instance->host_no);
printk("(asr=%02x,count=%ld,resid=%d,buf_resid=%d,have_data=%d,FC=%02x)- ",
READ_AUX_STAT(),read_3393_count(hostdata),cmd->SCp.this_residual,cmd->SCp.buffers_residual,
cmd->SCp.have_data_in,read1_io(IO_FIFO_COUNT));
tmp = (Scsi_Cmnd *)hostdata->input_Q;
prev = 0;
while (tmp) {
if (tmp == cmd) {
if (prev)
prev->host_scribble = cmd->host_scribble;
cmd->host_scribble = NULL;
cmd->result = DID_ABORT << 16;
printk("scsi%d: Abort - removing command %ld from input_Q. ",
instance->host_no, cmd->pid);
cmd->scsi_done(cmd);
restore_flags(flags);
return SCSI_ABORT_SUCCESS;
}
prev = tmp;
tmp = (Scsi_Cmnd *)tmp->host_scribble;
}
if (hostdata->connected == cmd) {
printk("scsi%d: Aborting connected command %ld - ",
instance->host_no, cmd->pid);
printk("sending wd33c93 ABORT command - ");
write_3393(hostdata, WD_CONTROL, CTRL_IDI | CTRL_EDI | CTRL_POLLED);
write_3393_cmd(hostdata, WD_CMD_ABORT);
printk("flushing fifo - ");
timeout = 1000000;
do {
asr = READ_AUX_STAT();
if (asr & ASR_DBR)
read_3393(hostdata, WD_DATA);
} while (!(asr & ASR_INT) && timeout-- > 0);
sr = read_3393(hostdata, WD_SCSI_STATUS);
printk("asr=%02x, sr=%02x, %ld bytes un-transferred (timeout=%ld) - ",
asr, sr, read_3393_count(hostdata), timeout);
printk("sending wd33c93 DISCONNECT command - ");
write_3393_cmd(hostdata, WD_CMD_DISCONNECT);
timeout = 1000000;
asr = READ_AUX_STAT();
while ((asr & ASR_CIP) && timeout-- > 0)
asr = READ_AUX_STAT();
sr = read_3393(hostdata, WD_SCSI_STATUS);
printk("asr=%02x, sr=%02x.",asr,sr);
hostdata->busy[cmd->target] &= ~(1 << cmd->lun);
hostdata->connected = NULL;
hostdata->state = S_UNCONNECTED;
cmd->result = DID_ABORT << 16;
cmd->scsi_done(cmd);
in2000_execute (instance);
restore_flags(flags);
return SCSI_ABORT_SUCCESS;
}
for (tmp=(Scsi_Cmnd *)hostdata->disconnected_Q; tmp;
tmp=(Scsi_Cmnd *)tmp->host_scribble)
if (cmd == tmp) {
restore_flags(flags);
printk("Sending ABORT_SNOOZE. ");
return SCSI_ABORT_SNOOZE;
}
in2000_execute (instance);
restore_flags(flags);
printk("scsi%d: warning : SCSI command probably completed successfully"
"         before abortion. ", instance->host_no);
return SCSI_ABORT_NOT_RUNNING;
}
#define MAX_IN2000_HOSTS 3
#define MAX_SETUP_ARGS (sizeof(setup_args) / sizeof(char *))
#define SETUP_BUFFER_SIZE 200
static char setup_buffer[SETUP_BUFFER_SIZE];
static char setup_used[MAX_SETUP_ARGS];
static int done_setup = 0;
in2000__INITFUNC( void in2000_setup (char *str, int *ints) )
{
int i;
char *p1,*p2;
strncpy(setup_buffer,str,SETUP_BUFFER_SIZE);
setup_buffer[SETUP_BUFFER_SIZE - 1] = '\0';
p1 = setup_buffer;
i = 0;
while (*p1 && (i < MAX_SETUP_ARGS)) {
p2 = strchr(p1, ',');
if (p2) {
*p2 = '\0';
if (p1 != p2)
setup_args[i] = p1;
p1 = p2 + 1;
i++;
}
else {
setup_args[i] = p1;
break;
}
}
for (i=0; i<MAX_SETUP_ARGS; i++)
setup_used[i] = 0;
done_setup = 1;
}
in2000__INITFUNC( static int check_setup_args(char *key, int *flags, int *val, char *buf) )
{
int x;
char *cp;
for (x=0; x<MAX_SETUP_ARGS; x++) {
if (setup_used[x])
continue;
if (!strncmp(setup_args[x], key, strlen(key)))
break;
}
if (x == MAX_SETUP_ARGS)
return 0;
setup_used[x] = 1;
cp = setup_args[x] + strlen(key);
*val = -1;
if (*cp != ':')
return ++x;
cp++;
if ((*cp >= '0') && (*cp <= '9')) {
*val = simple_strtoul(cp,NULL,0);
}
return ++x;
}
static const unsigned int *bios_tab[] in2000__INITDATA = {
(unsigned int *)0xc8000,
(unsigned int *)0xd0000,
(unsigned int *)0xd8000,
0
};
static const unsigned short base_tab[] in2000__INITDATA = {
0x220,
0x200,
0x110,
0x100,
};
static const int int_tab[] in2000__INITDATA = {
15,
14,
11,
10
};
in2000__INITFUNC( int in2000_detect(Scsi_Host_Template * tpnt) )
{
struct Scsi_Host *instance;
struct IN2000_hostdata *hostdata;
int detect_count;
int bios;
int x;
unsigned short base;
uchar switches;
uchar hrev;
int flags;
int val;
char buf[32];
if (!done_setup && setup_strings)
in2000_setup(setup_strings,0);
detect_count = 0;
for (bios = 0; bios_tab[bios]; bios++) {
if (check_setup_args("ioport",&flags,&val,buf)) {
base = val;
switches = ~inb(base + IO_SWITCHES) & 0xff;
printk("Forcing IN2000 detection at IOport 0x%x ",base);
bios = 2;
}
else if (readl(bios_tab[bios]+0x04) == 0x41564f4e ||
readl(bios_tab[bios]+0x0c) == 0x61776c41) {
printk("Found IN2000 BIOS at 0x%x ",(unsigned int)bios_tab[bios]);
switches = ~((readb(bios_tab[bios]+0x08) & 0xff));
x = switches & (SW_ADDR0 | SW_ADDR1);
base = base_tab[x];
x = ~inb(base + IO_SWITCHES) & 0xff;
if (x != switches) {
printk("Bad IO signature: %02x vs %02x.\n",x,switches);
continue;
}
}
else
continue;
if (!(switches & SW_BIT7)) {
printk("There is no IN-2000 SCSI card at IOport 0x%03x!\n",base);
continue;
}
hrev = inb(base + IO_HARDWARE);
if (switches & SW_DISINT) {
printk("The IN-2000 SCSI card at IOport 0x%03x ",base);
printk("is not configured for interrupt operation!\n");
printk("This driver requires an interrupt: cancelling detection.\n");
continue;
}
tpnt->proc_dir = &proc_scsi_in2000;
detect_count++;
instance = scsi_register(tpnt, sizeof(struct IN2000_hostdata));
if (!instance_list)
instance_list = instance;
hostdata = (struct IN2000_hostdata *)instance->hostdata;
instance->io_port = hostdata->io_base = base;
hostdata->dip_switch = switches;
hostdata->hrev = hrev;
write1_io(0,IO_FIFO_WRITE);
write1_io(0,IO_FIFO_READ);
write1_io(0,IO_INTR_MASK);
x = int_tab[(switches & (SW_INT0 | SW_INT1)) >> SW_INT_SHIFT];
if (request_irq(x, in2000_intr, SA_INTERRUPT, "in2000", NULL)) {
printk("in2000_detect: Unable to allocate IRQ.\n");
detect_count--;
continue;
}
instance->irq = x;
instance->n_io_port = 13;
request_region(base, 13, "in2000");
for (x = 0; x < 8; x++) {
hostdata->busy[x] = 0;
hostdata->sync_xfer[x] = calc_sync_xfer(DEFAULT_SX_PER/4,DEFAULT_SX_OFF);
hostdata->sync_stat[x] = SS_UNSET;
#ifdef PROC_STATISTICS
hostdata->cmd_cnt[x] = 0;
hostdata->disc_allowed_cnt[x] = 0;
hostdata->disc_done_cnt[x] = 0;
#endif
}
hostdata->input_Q = NULL;
hostdata->selecting = NULL;
hostdata->connected = NULL;
hostdata->disconnected_Q = NULL;
hostdata->state = S_UNCONNECTED;
hostdata->fifo = FI_FIFO_UNUSED;
hostdata->level2 = L2_BASIC;
hostdata->disconnect = DIS_ADAPTIVE;
hostdata->args = DEBUG_DEFAULTS;
hostdata->incoming_ptr = 0;
hostdata->outgoing_len = 0;
hostdata->default_sx_per = DEFAULT_SX_PER;
if (readl(bios_tab[bios]+0x04) == 0x41564f4e && (switches & SW_SYNC_DOS5))
hostdata->sync_off = 0x00;
else
hostdata->sync_off = 0xff;
#ifdef PROC_INTERFACE
hostdata->proc = PR_VERSION|PR_INFO|PR_STATISTICS|
PR_CONNECTED|PR_INPUTQ|PR_DISCQ|
PR_STOP;
#ifdef PROC_STATISTICS
hostdata->int_cnt = 0;
#endif
#endif
if (check_setup_args("nosync",&flags,&val,buf))
hostdata->sync_off = val;
if (check_setup_args("period",&flags,&val,buf))
hostdata->default_sx_per = sx_table[round_period((unsigned int)val)].period_ns;
if (check_setup_args("disconnect",&flags,&val,buf)) {
if ((val >= DIS_NEVER) && (val <= DIS_ALWAYS))
hostdata->disconnect = val;
else
hostdata->disconnect = DIS_ADAPTIVE;
}
if (check_setup_args("noreset",&flags,&val,buf))
hostdata->args ^= A_NO_SCSI_RESET;
if (check_setup_args("level2",&flags,&val,buf))
hostdata->level2 = val;
if (check_setup_args("debug",&flags,&val,buf))
hostdata->args = (val & DB_MASK);
#ifdef PROC_INTERFACE
if (check_setup_args("proc",&flags,&val,buf))
hostdata->proc = val;
#endif
x = reset_hardware(instance,(hostdata->args & A_NO_SCSI_RESET)?RESET_CARD:RESET_CARD_AND_BUS);
hostdata->microcode = read_3393(hostdata,WD_CDB_1);
if (x & 0x01) {
if (x & B_FLAG)
hostdata->chip = C_WD33C93B;
else
hostdata->chip = C_WD33C93A;
}
else
hostdata->chip = C_WD33C93;
printk("dip_switch=%02x irq=%d ioport=%02x floppy=%s sync/DOS5=%s ",
(switches & 0x7f),
instance->irq,hostdata->io_base,
(switches & SW_FLOPPY)?"Yes":"No",
(switches & SW_SYNC_DOS5)?"Yes":"No");
printk("hardware_ver=%02x chip=%s microcode=%02x\n",
hrev,
(hostdata->chip==C_WD33C93)?"WD33c93":
(hostdata->chip==C_WD33C93A)?"WD33c93A":
(hostdata->chip==C_WD33C93B)?"WD33c93B":"unknown",
hostdata->microcode);
#ifdef DEBUGGING_ON
printk("setup_args = ");
for (x=0; x<MAX_SETUP_ARGS; x++)
printk("%s,",setup_args[x]);
printk("\n");
#endif
if (hostdata->sync_off == 0xff)
printk("Sync-transfer DISABLED on all devices: ENABLE from command-line\n");
printk("IN2000 driver version %s - %s\n",IN2000_VERSION,IN2000_DATE);
}
return detect_count;
}
int in2000_biosparam(Disk *disk, kdev_t dev, int *iinfo)
{
int size;
size = disk->capacity;
iinfo[0] = 64;
iinfo[1] = 32;
iinfo[2] = size >> 11;
if (iinfo[2] > 1024) {
iinfo[0] = 64;
iinfo[1] = 63;
iinfo[2] = disk->capacity / (iinfo[0] * iinfo[1]);
}
if (iinfo[2] > 1024) {
iinfo[0] = 128;
iinfo[1] = 63;
iinfo[2] = disk->capacity / (iinfo[0] * iinfo[1]);
}
if (iinfo[2] > 1024) {
iinfo[0] = 255;
iinfo[1] = 63;
iinfo[2] = disk->capacity / (iinfo[0] * iinfo[1]);
}
return 0;
}
struct proc_dir_entry proc_scsi_in2000 = {
PROC_SCSI_IN2000, 6, "in2000",
S_IFDIR | S_IRUGO | S_IXUGO, 2
};
int in2000_proc_info(char *buf, char **start, off_t off, int len, int hn, int in)
{
#ifdef PROC_INTERFACE
char *bp;
char tbuf[128];
unsigned long flags;
struct Scsi_Host *instance;
struct IN2000_hostdata *hd;
Scsi_Cmnd *cmd;
int x,i;
static int stop = 0;
for (instance=instance_list; instance; instance=instance->next) {
if (instance->host_no == hn)
break;
}
if (!instance) {
printk("*** Hmm... Can't find host #%d!\n",hn);
return (-ESRCH);
}
hd = (struct IN2000_hostdata *)instance->hostdata;
if (in) {
buf[len] = '\0';
bp = buf;
if (!strncmp(bp,"debug:",6)) {
bp += 6;
hd->args = simple_strtoul(bp,NULL,0) & DB_MASK;
}
else if (!strncmp(bp,"disconnect:",11)) {
bp += 11;
x = simple_strtoul(bp,NULL,0);
if (x < DIS_NEVER || x > DIS_ALWAYS)
x = DIS_ADAPTIVE;
hd->disconnect = x;
}
else if (!strncmp(bp,"period:",7)) {
bp += 7;
x = simple_strtoul(bp,NULL,0);
hd->default_sx_per = sx_table[round_period((unsigned int)x)].period_ns;
}
else if (!strncmp(bp,"resync:",7)) {
bp += 7;
x = simple_strtoul(bp,NULL,0);
for (i=0; i<7; i++)
if (x & (1<<i))
hd->sync_stat[i] = SS_UNSET;
}
else if (!strncmp(bp,"proc:",5)) {
bp += 5;
hd->proc = simple_strtoul(bp,NULL,0);
}
else if (!strncmp(bp,"level2:",7)) {
bp += 7;
hd->level2 = simple_strtoul(bp,NULL,0);
}
return len;
}
save_flags(flags);
cli();
bp = buf;
*bp = '\0';
if (hd->proc & PR_VERSION) {
sprintf(tbuf,"\nVersion %s - %s.",
IN2000_VERSION,IN2000_DATE);
strcat(bp,tbuf);
}
if (hd->proc & PR_INFO) {
sprintf(tbuf,"\ndip_switch=%02x: irq=%d io=%02x floppy=%s sync/DOS5=%s",
(hd->dip_switch & 0x7f), instance->irq, hd->io_base,
(hd->dip_switch & 0x40)?"Yes":"No",
(hd->dip_switch & 0x20)?"Yes":"No");
strcat(bp,tbuf);
strcat(bp,"\nsync_xfer[] =       ");
for (x=0; x<7; x++) {
sprintf(tbuf,"\t%02x",hd->sync_xfer[x]);
strcat(bp,tbuf);
}
strcat(bp,"\nsync_stat[] =       ");
for (x=0; x<7; x++) {
sprintf(tbuf,"\t%02x",hd->sync_stat[x]);
strcat(bp,tbuf);
}
}
#ifdef PROC_STATISTICS
if (hd->proc & PR_STATISTICS) {
strcat(bp,"\ncommands issued:    ");
for (x=0; x<7; x++) {
sprintf(tbuf,"\t%ld",hd->cmd_cnt[x]);
strcat(bp,tbuf);
}
strcat(bp,"\ndisconnects allowed:");
for (x=0; x<7; x++) {
sprintf(tbuf,"\t%ld",hd->disc_allowed_cnt[x]);
strcat(bp,tbuf);
}
strcat(bp,"\ndisconnects done:   ");
for (x=0; x<7; x++) {
sprintf(tbuf,"\t%ld",hd->disc_done_cnt[x]);
strcat(bp,tbuf);
}
sprintf(tbuf,"\ninterrupts:      \t%ld",hd->int_cnt);
strcat(bp,tbuf);
}
#endif
if (hd->proc & PR_CONNECTED) {
strcat(bp,"\nconnected:     ");
if (hd->connected) {
cmd = (Scsi_Cmnd *)hd->connected;
sprintf(tbuf," %ld-%d:%d(%02x)",
cmd->pid, cmd->target, cmd->lun, cmd->cmnd[0]);
strcat(bp,tbuf);
}
}
if (hd->proc & PR_INPUTQ) {
strcat(bp,"\ninput_Q:       ");
cmd = (Scsi_Cmnd *)hd->input_Q;
while (cmd) {
sprintf(tbuf," %ld-%d:%d(%02x)",
cmd->pid, cmd->target, cmd->lun, cmd->cmnd[0]);
strcat(bp,tbuf);
cmd = (Scsi_Cmnd *)cmd->host_scribble;
}
}
if (hd->proc & PR_DISCQ) {
strcat(bp,"\ndisconnected_Q:");
cmd = (Scsi_Cmnd *)hd->disconnected_Q;
while (cmd) {
sprintf(tbuf," %ld-%d:%d(%02x)",
cmd->pid, cmd->target, cmd->lun, cmd->cmnd[0]);
strcat(bp,tbuf);
cmd = (Scsi_Cmnd *)cmd->host_scribble;
}
}
if (hd->proc & PR_TEST) {
;
}
strcat(bp,"\n");
restore_flags(flags);
*start = buf;
if (stop) {
stop = 0;
return 0;
}
if (off > 0x40000)
stop = 1;;
if (hd->proc & PR_STOP)
stop = 1;
return strlen(bp);
#else
return 0;
#endif
}
#ifdef MODULE
Scsi_Host_Template driver_template = IN2000;
#include "scsi_module.c"
#endif