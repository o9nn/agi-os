#include <linux/config.h>
#define PPA_CODE 1
#ifndef HAVE_PC87332
#define HAVE_PC87332 0
#endif
#define PPA_PROBE_SPP 0x0001
#define PPA_PROBE_PS2 0x0002
#define PPA_PROBE_ECR 0x0010
#define PPA_PROBE_EPP17 0x0100
#define PPA_PROBE_EPP19 0x0200
int port_probe(unsigned short);
#include <linux/blk.h>
#include "sd.h"
#include "hosts.h"
typedef struct {
int base;
int mode;
int host;
Scsi_Cmnd *cur_cmd;
struct tq_struct ppa_tq;
unsigned long jstart;
unsigned failed:1;
} ppa_struct;
#define PPA_EMPTY \
{-1, \
PPA_AUTODETECT, \
-1, \
NULL, \
{0, 0, ppa_interrupt, NULL}, \
0, \
0 \
}
#include "ppa.h"
#undef CONFIG_PARPORT
#define NO_HOSTS 4
static ppa_struct ppa_hosts[NO_HOSTS] =
{PPA_EMPTY, PPA_EMPTY, PPA_EMPTY, PPA_EMPTY};
#define PPA_BASE(x) ppa_hosts[(x)].base
int base[NO_HOSTS] =
{0x03bc, 0x0378, 0x0278, 0x0000};
#define parbus_base base
#define parbus_no NO_HOSTS
static inline int ppa_pb_claim(int host_no)
{
if (ppa_hosts[host_no].cur_cmd)
ppa_hosts[host_no].cur_cmd->SCp.phase++;
return 0;
}
#ifndef MODULE
void ppa_setup(char *str, int *ints)
{
static int x = 0;
if (x == 0) {
int i;
for (i = 0; i < NO_HOSTS; i++)
parbus_base[i] = 0x0000;
}
switch (ints[0]) {
case 3:
ppa_sg = ints[3];
case 2:
ppa_hosts[x].mode = ints[2];
parbus_base[x] = ints[1];
break;
default:
printk("PPA: I only use between 2 to 3 parameters.\n");
break;
}
x++;
}
#else
Scsi_Host_Template driver_template = PPA;
#include "scsi_module.c"
#endif
#if HAVE_PC87332 > 0
#warning PC87332 Kludge code included
static inline int pc87332_port(int host_no)
{
int base = ppa_hosts[host_no].base;
unsigned short index_addr[4] =
{
0x0398, 0x026e, 0x015c, 0x002e
};
unsigned short port_ref[4] =
{
0x378, 0x3bc, 0x278, 0xffff
};
unsigned char a;
int loop;
for (loop = 0; loop < 4; loop++) {
inb(index_addr[loop]);
inb(index_addr[loop]);
inb(index_addr[loop]);
inb(index_addr[loop]);
outb(0xff, index_addr[loop]);
a = inb(index_addr[loop]);
switch (a) {
case (0x0f):
break;
case (0x1f):
break;
case (0x7f):
break;
default:
continue;
}
outb(0x01, index_addr[loop]);
a = inb(index_addr[loop] + 1);
if (port_ref[a & 0x03] != base)
continue;
printk("NatSemi PC87332 (or variant) at 0x%04x\n", base);
if (base != 0x3bc) {
outb(0x04, index_addr[loop]);
a = inb(index_addr[loop] + 1);
printk("Old reg1 = %02x\n", a);
a = (a & 0xf0) | 0x03;
outb(a, index_addr[loop] + 1);
outb(a, index_addr[loop] + 1);
outb(0x02, index_addr[loop]);
a = inb(index_addr[loop] + 1);
printk("Old reg2 = %02x\n", a);
a = (a & 0x7f) | 0x80;
outb(a, index_addr[loop] + 1);
outb(a, index_addr[loop] + 1);
ppa_hosts[host_no].mode = PPA_EPP_32;
} else {
outb(0x04, index_addr[loop]);
a = inb(index_addr[loop] + 1);
a = (a & 0xfb) | 0x06;
outb(a, index_addr[loop] + 1);
outb(a, index_addr[loop] + 1);
ppa_hosts[host_no].mode = PPA_PS2;
}
outb(0x04, index_addr[loop]);
a = inb(index_addr[loop] + 1);
return ppa_hosts[host_no].mode;
}
return 0;
}
#else
#define pc87332_port(x)
#endif
static inline int generic_port(int host_no)
{
unsigned int save_ctr, save_ecr, r;
int ppb = PPA_BASE(host_no);
save_ctr = r_ctr(ppb);
save_ecr = r_ecr(ppb);
r = port_probe(ppb);
w_ecr(ppb, save_ecr);
w_ctr(ppb, save_ctr);
if (r & PPA_PROBE_SPP)
ppa_hosts[host_no].mode = PPA_NIBBLE;
if (r & PPA_PROBE_PS2) {
ppa_hosts[host_no].mode = PPA_PS2;
if (r & PPA_PROBE_ECR)
w_ecr(ppb, 0x20);
}
if ((r & PPA_PROBE_EPP17) || (r & PPA_PROBE_EPP19)) {
if (r & PPA_PROBE_ECR)
w_ecr(ppb, 0x80);
}
return ppa_hosts[host_no].mode;
}
int ppa_detect(Scsi_Host_Template * host)
{
struct Scsi_Host *hreg;
int ports;
int i, nhosts;
printk("ppa: Version %s\n", PPA_VERSION);
nhosts = 0;
for (i = 0; i < parbus_no; i++) {
if (parbus_base[i] == 0x0000)
continue;
ppa_hosts[i].base = parbus_base[i];
if (check_region(parbus_base[i],
(parbus_base[i] == 0x03bc) ? 3 : 8))
continue;
pc87332_port(i);
if (!generic_port(i))
continue;
if (ppa_init(i))
continue;
switch (ppa_hosts[i].mode) {
case PPA_NIBBLE:
case PPA_PS2:
ports = 3;
break;
case PPA_EPP_8:
case PPA_EPP_16:
case PPA_EPP_32:
ports = 8;
break;
default:
continue;
}
request_region(ppa_hosts[i].base, ports, "ppa");
host->can_queue = PPA_CAN_QUEUE;
host->sg_tablesize = ppa_sg;
hreg = scsi_register(host, 0);
hreg->io_port = ppa_hosts[i].base;
hreg->n_io_port = ports;
hreg->dma_channel = -1;
hreg->unique_id = i;
ppa_hosts[i].host = hreg->host_no;
nhosts++;
}
if (nhosts == 0)
return 0;
else
return 1;
}
static inline int ppa_strncmp(const char *a, const char *b, int len)
{
int loop;
for (loop = 0; loop < len; loop++)
if (a[loop] != b[loop])
return 1;
return 0;
}
static inline int ppa_proc_write(int hostno, char *buffer, int length)
{
unsigned long x;
if ((length > 5) && (ppa_strncmp(buffer, "mode=", 5) == 0)) {
x = simple_strtoul(buffer + 5, NULL, 0);
ppa_hosts[hostno].mode = x;
return length;
}
printk("ppa /proc: invalid variable\n");
return (-EINVAL);
}
int ppa_proc_info(char *buffer, char **start, off_t offset,
int length, int hostno, int inout)
{
int i;
int len = 0;
for (i = 0; i < 4; i++)
if (ppa_hosts[i].host == hostno)
break;
if (inout)
return ppa_proc_write(i, buffer, length);
len += sprintf(buffer + len, "Version : %s\n", PPA_VERSION);
len += sprintf(buffer + len, "Port    : 0x%04x\n", ppa_hosts[i].base);
len += sprintf(buffer + len, "Mode    : %s\n", PPA_MODE_STRING[ppa_hosts[i].mode]);
if (offset > len)
return 0;
*start = buffer + offset;
len -= offset;
if (len > length)
len = length;
return len;
}
static int device_check(int host_no);
#if PPA_DEBUG > 0
#define ppa_fail(x,y) printk("ppa: ppa_fail(%i) from %s at line %d\n",\
y, __FUNCTION__, __LINE__); ppa_fail_func(x,y);
static inline void ppa_fail_func(int host_no, int error_code)
#else
static inline void ppa_fail(int host_no, int error_code)
#endif
{
if (ppa_hosts[host_no].cur_cmd) {
ppa_hosts[host_no].cur_cmd->result = error_code << 16;
ppa_hosts[host_no].failed = 1;
}
}
static unsigned char ppa_wait(int host_no)
{
int k;
unsigned short ppb = PPA_BASE(host_no);
unsigned char r;
k = PPA_SPIN_TMO;
do {
r = r_str(ppb);
k--;
udelay(1);
}
while (!(r & 0x80) && (k));
if (k)
return (r & 0xf0);
ppa_fail(host_no, DID_TIME_OUT);
printk("ppa timeout in ppa_wait\n");
return 0;
}
static inline void epp_reset(unsigned short ppb)
{
int i;
i = r_str(ppb);
w_str(ppb, i);
w_str(ppb, i & 0xfe);
}
static inline void ecp_sync(unsigned short ppb)
{
int i;
if ((r_ecr(ppb) & 0xe0) != 0x80)
return;
for (i = 0; i < 100; i++) {
if (r_ecr(ppb) & 0x01)
return;
udelay(5);
}
printk("ppa: ECP sync failed as data still present in FIFO.\n");
}
#ifdef __i386__
#define BYTE_OUT(reg) \
"	movb " #reg ",%%al\n" \
"	outb %%al,(%%dx)\n" \
"	addl $2,%%edx\n" \
"	movb $0x0e,%%al\n" \
"	outb %%al,(%%dx)\n" \
"	movb $0x0c,%%al\n" \
"	outb %%al,(%%dx)\n" \
"	subl $2,%%edx\n"
static inline int ppa_byte_out(unsigned short base, char *buffer, unsigned int len)
{
int i;
for (i = len; i; i--) {
w_dtr(base, *buffer++);
w_ctr(base, 0xe);
w_ctr(base, 0xc);
}
return 1;
}
#define BYTE_IN(reg) \
"	inb (%%dx),%%al\n" \
"	movb %%al," #reg "\n" \
"	addl $2,%%edx\n" \
"	movb $0x27,%%al\n" \
"	outb %%al,(%%dx)\n" \
"	movb $0x25,%%al\n" \
"	outb %%al,(%%dx)\n" \
"	subl $2,%%edx\n"
static inline int ppa_byte_in(unsigned short base, char *buffer, int len)
{
int i;
for (i = len; i; i--) {
*buffer++ = r_dtr(base);
w_ctr(base, 0x27);
w_ctr(base, 0x25);
}
return 1;
}
#define NIBBLE_IN(reg) \
"	incl %%edx\n" \
"	movb $0x04,%%al\n" \
"	outb %%al,(%%dx)\n" \
"	decl %%edx\n" \
"	inb (%%dx),%%al\n" \
"	andb $0xf0,%%al\n" \
"	movb %%al," #reg "\n" \
"	incl %%edx\n" \
"	movb $0x06,%%al\n" \
"	outb %%al,(%%dx)\n" \
"	decl %%edx\n" \
"	inb (%%dx),%%al\n" \
"	shrb $4,%%al\n" \
"	orb %%al," #reg "\n"
static inline int ppa_nibble_in(unsigned short base, char *buffer, int len)
{
for (; len; len--) {
unsigned char h;
w_ctr(base, 0x4);
h = r_str(base) & 0xf0;
w_ctr(base, 0x6);
*buffer++ = h | ((r_str(base) & 0xf0) >> 4);
}
return 1;
}
#else
static inline int ppa_byte_out(unsigned short base, const char *buffer, int len)
{
unsigned short ctr_p = base + 2;
int i;
for (i = len; i; i--) {
outb(*buffer++, base);
outb(0xe, ctr_p);
outb(0xc, ctr_p);
}
return 1;
}
static inline int ppa_byte_in(unsigned short base, char *buffer, int len)
{
unsigned short ctr_p = base + 2;
int i;
for (i = len; i; i--) {
*buffer++ = inb(base);
outb(0x27, ctr_p);
outb(0x25, ctr_p);
}
return 1;
}
static inline int ppa_nibble_in(unsigned short str_p, char *buffer, int len)
{
unsigned short ctr_p = str_p + 1;
unsigned char h, l;
int i;
for (i = len; i; i--) {
outb(0x4, ctr_p);
h = inb(str_p);
outb(0x6, ctr_p);
l = inb(str_p);
*buffer++ = (h & 0xf0) | ((l & 0xf0) >> 4);
}
return 1;
}
#endif
static inline int ppa_epp_out(unsigned short epp_p, unsigned short str_p, const char *buffer, int len)
{
int i;
for (i = len; i; i--) {
outb(*buffer++, epp_p);
#ifdef CONFIG_SCSI_PPA_HAVE_PEDANTIC
if (inb(str_p) & 0x01)
return 0;
#endif
}
return 1;
}
static int ppa_out(int host_no, char *buffer, int len)
{
int r;
unsigned short ppb = PPA_BASE(host_no);
r = ppa_wait(host_no);
if ((r & 0x50) != 0x40) {
ppa_fail(host_no, DID_ERROR);
return 0;
}
switch (ppa_hosts[host_no].mode) {
case PPA_NIBBLE:
case PPA_PS2:
r = ppa_byte_out(ppb, buffer, len);
break;
case PPA_EPP_32:
case PPA_EPP_16:
case PPA_EPP_8:
epp_reset(ppb);
w_ctr(ppb, 0x4);
#ifdef CONFIG_SCSI_PPA_HAVE_PEDANTIC
r = ppa_epp_out(ppb + 4, ppb + 1, buffer, len);
#else
if (!(((long) buffer | len) & 0x03))
outsl(ppb + 4, buffer, len >> 2);
else
outsb(ppb + 4, buffer, len);
w_ctr(ppb, 0xc);
r = !(r_str(ppb) & 0x01);
#endif
w_ctr(ppb, 0xc);
ecp_sync(ppb);
break;
default:
printk("PPA: bug in ppa_out()\n");
r = 0;
}
return r;
}
static inline int ppa_epp_in(int epp_p, int str_p, char *buffer, int len)
{
int i;
for (i = len; i; i--) {
*buffer++ = inb(epp_p);
#ifdef CONFIG_SCSI_PPA_HAVE_PEDANTIC
if (inb(str_p) & 0x01)
return 0;
#endif
}
return 1;
}
static int ppa_in(int host_no, char *buffer, int len)
{
int r;
unsigned short ppb = PPA_BASE(host_no);
r = ppa_wait(host_no);
if ((r & 0x50) != 0x50) {
ppa_fail(host_no, DID_ERROR);
return 0;
}
switch (ppa_hosts[host_no].mode) {
case PPA_NIBBLE:
r = ppa_nibble_in(ppb + 1, buffer, len);
w_ctr(ppb, 0xc);
break;
case PPA_PS2:
w_ctr(ppb, 0x25);
r = ppa_byte_in(ppb, buffer, len);
w_ctr(ppb, 0x4);
w_ctr(ppb, 0xc);
break;
case PPA_EPP_32:
case PPA_EPP_16:
case PPA_EPP_8:
epp_reset(ppb);
w_ctr(ppb, 0x24);
#ifdef CONFIG_SCSI_PPA_HAVE_PEDANTIC
r = ppa_epp_in(ppb + 4, ppb + 1, buffer, len);
#else
if (!(((long) buffer | len) & 0x03))
insl(ppb + 4, buffer, len >> 2);
else
insb(ppb + 4, buffer, len);
w_ctr(ppb, 0x2c);
r = !(r_str(ppb) & 0x01);
#endif
w_ctr(ppb, 0x2c);
ecp_sync(ppb);
break;
default:
printk("PPA: bug in ppa_ins()\n");
r = 0;
break;
}
return r;
}
static inline void ppa_d_pulse(unsigned short ppb, unsigned char b)
{
w_dtr(ppb, b);
w_ctr(ppb, 0xc);
w_ctr(ppb, 0xe);
w_ctr(ppb, 0xc);
w_ctr(ppb, 0x4);
w_ctr(ppb, 0xc);
}
static void ppa_disconnect(int host_no)
{
unsigned short ppb = PPA_BASE(host_no);
ppa_d_pulse(ppb, 0);
ppa_d_pulse(ppb, 0x3c);
ppa_d_pulse(ppb, 0x20);
ppa_d_pulse(ppb, 0xf);
}
static inline void ppa_c_pulse(unsigned short ppb, unsigned char b)
{
w_dtr(ppb, b);
w_ctr(ppb, 0x4);
w_ctr(ppb, 0x6);
w_ctr(ppb, 0x4);
w_ctr(ppb, 0xc);
}
static inline void ppa_connect(int host_no, int flag)
{
unsigned short ppb = PPA_BASE(host_no);
ppa_c_pulse(ppb, 0);
ppa_c_pulse(ppb, 0x3c);
ppa_c_pulse(ppb, 0x20);
if ((flag == CONNECT_EPP_MAYBE) &&
IN_EPP_MODE(ppa_hosts[host_no].mode))
ppa_c_pulse(ppb, 0xcf);
else
ppa_c_pulse(ppb, 0x8f);
}
static int ppa_select(int host_no, int target)
{
int k;
unsigned short ppb = PPA_BASE(host_no);
k = PPA_SELECT_TMO;
do {
k--;
} while ((r_str(ppb) & 0x40) && (k));
if (!k)
return 0;
w_dtr(ppb, (1 << target));
w_ctr(ppb, 0xe);
w_ctr(ppb, 0xc);
w_dtr(ppb, 0x80);
w_ctr(ppb, 0x8);
k = PPA_SELECT_TMO;
do {
k--;
}
while (!(r_str(ppb) & 0x40) && (k));
if (!k)
return 0;
return 1;
}
static int ppa_init(int host_no)
{
int retv;
unsigned short ppb = PPA_BASE(host_no);
ppa_disconnect(host_no);
ppa_connect(host_no, CONNECT_NORMAL);
retv = 2;
w_ctr(ppb, 0xe);
if ((r_str(ppb) & 0x08) == 0x08)
retv--;
w_ctr(ppb, 0xc);
if ((r_str(ppb) & 0x08) == 0x00)
retv--;
if (!retv) {
w_dtr(ppb, 0x40);
w_ctr(ppb, 0x08);
udelay(30);
w_ctr(ppb, 0x0c);
udelay(1000);
}
ppa_disconnect(host_no);
udelay(1000);
if (!retv)
retv = device_check(host_no);
return retv;
}
static inline int ppa_send_command(Scsi_Cmnd * cmd)
{
int host_no = cmd->host->unique_id;
int k;
w_ctr(PPA_BASE(host_no), 0x0c);
for (k = 0; k < cmd->cmd_len; k++)
if (!ppa_out(host_no, &cmd->cmnd[k], 1))
return 0;
return 1;
}
static int ppa_completion(Scsi_Cmnd * cmd)
{
int host_no = cmd->host->unique_id;
unsigned short ppb = PPA_BASE(host_no);
unsigned long start_jiffies = jiffies;
unsigned char r, v;
int fast, bulk, status;
v = cmd->cmnd[0];
bulk = ((v == READ_6) ||
(v == READ_10) ||
(v == WRITE_6) ||
(v == WRITE_10));
r = (r_str(ppb) & 0xf0);
while (r != (unsigned char) 0xf0) {
if (jiffies > start_jiffies + 1)
return 0;
if (((r & 0xc0) != 0xc0) || (cmd->SCp.this_residual <= 0)) {
ppa_fail(host_no, DID_ERROR);
return -1;
}
fast = (bulk && (cmd->SCp.this_residual >= PPA_BURST_SIZE))
? PPA_BURST_SIZE : 1;
if (r == (unsigned char) 0xc0)
status = ppa_out(host_no, cmd->SCp.ptr, fast);
else
status = ppa_in(host_no, cmd->SCp.ptr, fast);
cmd->SCp.ptr += fast;
cmd->SCp.this_residual -= fast;
if (!status) {
ppa_fail(host_no, DID_BUS_BUSY);
return -1;
}
if (cmd->SCp.buffer && !cmd->SCp.this_residual) {
if (cmd->SCp.buffers_residual--) {
cmd->SCp.buffer++;
cmd->SCp.this_residual = cmd->SCp.buffer->length;
cmd->SCp.ptr = cmd->SCp.buffer->address;
}
}
r = (r_str(ppb) & 0xf0);
if (!(r & 0x80))
return 0;
}
return 1;
}
static void ppa_interrupt(void *data)
{
ppa_struct *tmp = (ppa_struct *) data;
Scsi_Cmnd *cmd = tmp->cur_cmd;
if (!cmd) {
printk("PPA: bug in ppa_interrupt\n");
return;
}
if (ppa_engine(tmp, cmd)) {
tmp->ppa_tq.data = (void *) tmp;
tmp->ppa_tq.sync = 0;
queue_task(&tmp->ppa_tq, &tq_timer);
return;
}
#if PPA_DEBUG > 0
switch ((cmd->result >> 16) & 0xff) {
case DID_OK:
break;
case DID_NO_CONNECT:
printk("ppa: no device at SCSI ID %i\n", cmd->target);
break;
case DID_BUS_BUSY:
printk("ppa: BUS BUSY - EPP timeout detected\n");
break;
case DID_TIME_OUT:
printk("ppa: unknown timeout\n");
break;
case DID_ABORT:
printk("ppa: told to abort\n");
break;
case DID_PARITY:
printk("ppa: parity error (???)\n");
break;
case DID_ERROR:
printk("ppa: internal driver error\n");
break;
case DID_RESET:
printk("ppa: told to reset device\n");
break;
case DID_BAD_INTR:
printk("ppa: bad interrupt (???)\n");
break;
default:
printk("ppa: bad return code (%02x)\n", (cmd->result >> 16) & 0xff);
}
#endif
if (cmd->SCp.phase > 1)
ppa_disconnect(cmd->host->unique_id);
tmp->cur_cmd = 0;
cmd->scsi_done(cmd);
return;
}
static int ppa_engine(ppa_struct * tmp, Scsi_Cmnd * cmd)
{
int host_no = cmd->host->unique_id;
unsigned short ppb = PPA_BASE(host_no);
unsigned char l = 0, h = 0;
int retv;
if (tmp->failed)
return 0;
switch (cmd->SCp.phase) {
case 0:
if ((jiffies - tmp->jstart) > HZ) {
ppa_fail(host_no, DID_BUS_BUSY);
return 0;
}
return 1;
case 1:
{
int retv = 2;
ppa_connect(host_no, CONNECT_EPP_MAYBE);
w_ctr(ppb, 0xe);
if ((r_str(ppb) & 0x08) == 0x08)
retv--;
w_ctr(ppb, 0xc);
if ((r_str(ppb) & 0x08) == 0x00)
retv--;
if (retv)
if ((jiffies - tmp->jstart) > (1 * HZ)) {
printk("ppa: Parallel port cable is unplugged!!\n");
ppa_fail(host_no, DID_BUS_BUSY);
return 0;
} else {
ppa_disconnect(host_no);
return 1;
}
cmd->SCp.phase++;
}
case 2:
if (!ppa_select(host_no, cmd->target)) {
ppa_fail(host_no, DID_NO_CONNECT);
return 0;
}
cmd->SCp.phase++;
case 3:
w_ctr(ppb, 0x0c);
if (!(r_str(ppb) & 0x80))
return 1;
if (!ppa_send_command(cmd))
return 0;
cmd->SCp.phase++;
case 4:
if (cmd->use_sg) {
cmd->SCp.buffer = (struct scatterlist *) cmd->request_buffer;
cmd->SCp.this_residual = cmd->SCp.buffer->length;
cmd->SCp.ptr = cmd->SCp.buffer->address;
} else {
cmd->SCp.buffer = NULL;
cmd->SCp.this_residual = cmd->request_bufflen;
cmd->SCp.ptr = cmd->request_buffer;
}
cmd->SCp.buffers_residual = cmd->use_sg;
cmd->SCp.phase++;
case 5:
w_ctr(ppb, 0x0c);
if (!(r_str(ppb) & 0x80))
return 1;
retv = ppa_completion(cmd);
if (retv == -1)
return 0;
if (retv == 0)
return 1;
cmd->SCp.phase++;
case 6:
cmd->result = DID_OK << 16;
if (ppa_wait(host_no) != (unsigned char) 0xf0) {
ppa_fail(host_no, DID_ERROR);
return 0;
}
if (ppa_in(host_no, &l, 1)) {
if (ppa_wait(host_no) == (unsigned char) 0xf0)
ppa_in(host_no, &h, 1);
cmd->result = (DID_OK << 16) + (h << 8) + (l & STATUS_MASK);
}
return 0;
break;
default:
printk("ppa: Invalid scsi phase\n");
}
return 0;
}
int ppa_queuecommand(Scsi_Cmnd * cmd, void (*done) (Scsi_Cmnd *))
{
int host_no = cmd->host->unique_id;
if (ppa_hosts[host_no].cur_cmd) {
printk("PPA: bug in ppa_queuecommand\n");
return 0;
}
ppa_hosts[host_no].failed = 0;
ppa_hosts[host_no].jstart = jiffies;
ppa_hosts[host_no].cur_cmd = cmd;
cmd->scsi_done = done;
cmd->result = DID_ERROR << 16;
cmd->SCp.phase = 0;
ppa_pb_claim(host_no);
ppa_hosts[host_no].ppa_tq.data = ppa_hosts + host_no;
ppa_hosts[host_no].ppa_tq.sync = 0;
queue_task(&ppa_hosts[host_no].ppa_tq, &tq_immediate);
mark_bh(IMMEDIATE_BH);
return 0;
}
int ppa_biosparam(Disk * disk, kdev_t dev, int ip[])
{
ip[0] = 0x40;
ip[1] = 0x20;
ip[2] = (disk->capacity + 1) / (ip[0] * ip[1]);
if (ip[2] > 1024) {
ip[0] = 0xff;
ip[1] = 0x3f;
ip[2] = (disk->capacity + 1) / (ip[0] * ip[1]);
if (ip[2] > 1023)
ip[2] = 1023;
}
return 0;
}
int ppa_abort(Scsi_Cmnd * cmd)
{
switch (cmd->SCp.phase) {
case 0:
case 1:
cmd->result = DID_ABORT;
cmd->done(cmd);
return SCSI_ABORT_SUCCESS;
break;
default:
return SCSI_ABORT_BUSY;
break;
}
}
int ppa_reset(Scsi_Cmnd * cmd, unsigned int x)
{
int host_no = cmd->host->unique_id;
int ppb = PPA_BASE(host_no);
if (cmd)
if (cmd->SCp.phase)
ppa_disconnect(cmd->host->unique_id);
ppa_connect(host_no, CONNECT_NORMAL);
w_dtr(ppb, 0x40);
w_ctr(ppb, 0x8);
udelay(30);
w_ctr(ppb, 0xc);
udelay(1000);
ppa_disconnect(host_no);
udelay(1000);
if (!cmd) {
printk("ppa bus reset called for invalid command.\n");
return SCSI_RESET_NOT_RUNNING;
}
ppa_connect(host_no, CONNECT_NORMAL);
ppa_fail(host_no, DID_RESET);
return SCSI_RESET_PENDING;
}
static int device_check(int host_no)
{
static char cmd[6] =
{0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
int loop, old_mode, status, k, ppb = PPA_BASE(host_no);
unsigned char l;
old_mode = ppa_hosts[host_no].mode;
for (loop = 0; loop < 8; loop++) {
if ((ppb & 0x0007) == 0x0000)
ppa_hosts[host_no].mode = PPA_EPP_32;
second_pass:
ppa_connect(host_no, CONNECT_EPP_MAYBE);
if (!ppa_select(host_no, loop)) {
ppa_disconnect(host_no);
continue;
}
printk("ppa: Found device at ID %i, Attempting to use %s\n", loop,
PPA_MODE_STRING[ppa_hosts[host_no].mode]);
status = 1;
w_ctr(ppb, 0x0c);
for (l = 0; (l < 6) && (status); l++)
status = ppa_out(host_no, cmd, 1);
if (!status) {
ppa_disconnect(host_no);
ppa_connect(host_no, CONNECT_EPP_MAYBE);
w_dtr(ppb, 0x40);
w_ctr(ppb, 0x08);
udelay(30);
w_ctr(ppb, 0x0c);
udelay(1000);
ppa_disconnect(host_no);
udelay(1000);
if (ppa_hosts[host_no].mode == PPA_EPP_32) {
ppa_hosts[host_no].mode = old_mode;
goto second_pass;
}
printk("ppa: Unable to establish communication, aborting driver load.\n");
return 1;
}
w_ctr(ppb, 0x0c);
k = 1000000;
do {
l = r_str(ppb);
k--;
udelay(1);
} while (!(l & 0x80) && (k));
l &= 0xf0;
if (l != 0xf0) {
ppa_disconnect(host_no);
ppa_connect(host_no, CONNECT_EPP_MAYBE);
w_dtr(ppb, 0x40);
w_ctr(ppb, 0x08);
udelay(30);
w_ctr(ppb, 0x0c);
udelay(1000);
ppa_disconnect(host_no);
udelay(1000);
if (ppa_hosts[host_no].mode == PPA_EPP_32) {
ppa_hosts[host_no].mode = old_mode;
goto second_pass;
}
printk("ppa: Unable to establish communication, aborting driver load.\n");
return 1;
}
ppa_disconnect(host_no);
printk("ppa: Communication established with ID %i using %s\n", loop,
PPA_MODE_STRING[ppa_hosts[host_no].mode]);
return 0;
}
printk("ppa: No devices found, aborting driver load.\n");
return 1;
}
#define PPA_ID "ppa: "
int port_probe(unsigned short port)
{
int retv = 0;
unsigned char a, b, c;
unsigned int i, j;
printk(PPA_ID "Probing port %04x\n", port);
outb(0x0c, port + 0x402);
outb(0x0c, port + 0x002);
outb(0x55, port);
a = inb(port);
if (a != 0x55)
return retv;
printk(PPA_ID "    SPP port present\n");
retv += PPA_PROBE_SPP;
for (i = 1024; i > 0; i--) {
a = inb(port + 0x402);
if ((a & 0x03) == 0x03)
goto no_ecp;
if (a & 0x01)
break;
inb(port + 0x400);
}
if (i <= 0)
goto no_ecp;
b = a ^ 3;
outb(b, port + 0x402);
c = inb(port + 0x402);
if (a == c) {
outb(0xc0, port + 0x402);
j = 0;
while (!(inb(port + 0x402) & 0x01) && (j < 1024)) {
inb(port + 0x400);
j++;
}
if (j >= 1024)
goto no_ecp;
i = 0;
j = 0;
while (!(inb(port + 0x402) & 0x02) && (j < 1024)) {
outb(0x00, port + 0x400);
i++;
j++;
}
if (j >= 1024)
goto no_ecp;
j = 0;
while (!(inb(port + 0x402) & 0x01) && (j < 1024)) {
inb(port + 0x400);
j++;
}
if (j >= 1024)
goto no_ecp;
printk(PPA_ID "    ECP with a %i byte FIFO present\n", i);
retv += PPA_PROBE_ECR;
}
no_ecp:
if (retv & PPA_PROBE_ECR)
outb(0x20, port + 0x402);
outb(0x55, port);
outb(0x0c, port + 2);
a = inb(port);
outb(0x55, port);
outb(0x2c, port + 2);
b = inb(port);
if (a != b) {
printk(PPA_ID "    PS/2 bidirectional port present\n");
retv += PPA_PROBE_PS2;
}
if (port & 0x007) {
printk(PPA_ID "    EPP not supported at this address\n");
return retv;
}
if (retv & PPA_PROBE_ECR) {
for (i = 0x00; i < 0x80; i += 0x20) {
outb(i, port + 0x402);
a = inb(port + 1);
outb(a, port + 1);
outb(a & 0xfe, port + 1);
a = inb(port + 1);
if (!(a & 0x01)) {
printk(PPA_ID "    Failed Intel bug check. (Phony EPP in ECP)\n");
return retv;
}
}
printk(PPA_ID "    Passed Intel bug check.\n");
outb(0x80, port + 0x402);
}
a = inb(port + 1);
outb(a, port + 1);
outb(a & 0xfe, port + 1);
a = inb(port + 1);
if (a & 0x01) {
outb(0x0c, port + 0x402);
outb(0x0c, port + 0x002);
return retv;
}
outb(0x04, port + 2);
inb(port + 4);
a = inb(port + 1);
outb(a, port + 1);
outb(a & 0xfe, port + 1);
if (a & 0x01) {
printk(PPA_ID "    EPP 1.9 with hardware direction protocol\n");
retv += PPA_PROBE_EPP19;
} else {
outb(0x24, port + 2);
inb(port + 4);
a = inb(port + 1);
outb(a, port + 1);
outb(a & 0xfe, port + 1);
if (a & 0x01) {
printk(PPA_ID "    EPP 1.9 with software direction protocol\n");
retv += PPA_PROBE_EPP19;
} else {
printk(PPA_ID "    EPP 1.7\n");
retv += PPA_PROBE_EPP17;
}
}
outb(0x0c, port + 0x402);
outb(0x0c, port + 0x002);
return retv;
}