#ifdef PCMCIA
#define MODULE
#endif
#ifdef MODULE
#include <linux/module.h>
#endif
#ifdef PCMCIA
#undef MODULE
#endif
#include <linux/sched.h>
#include <asm/io.h>
#include <linux/blk.h>
#include "scsi.h"
#include "hosts.h"
#include "fdomain.h"
#include <asm/system.h>
#include <linux/errno.h>
#include <linux/string.h>
#include <linux/ioport.h>
#include <linux/proc_fs.h>
#include <linux/bios32.h>
#include <linux/pci.h>
#include <linux/stat.h>
#include <linux/config.h>
struct proc_dir_entry proc_scsi_fdomain = {
PROC_SCSI_FDOMAIN, 7, "fdomain",
S_IFDIR | S_IRUGO | S_IXUGO, 2
};
#define VERSION "$Revision: 1.1 $"
#define DEBUG 1
#define ENABLE_PARITY 1
#define FIFO_COUNT 2
#define DO_DETECT 0
#if DEBUG
#define EVERY_ACCESS 0
#define ERRORS_ONLY 1
#define DEBUG_DETECT 0
#define DEBUG_MESSAGES 1
#define DEBUG_ABORT 1
#define DEBUG_RESET 1
#define DEBUG_RACE 1
#else
#define EVERY_ACCESS 0
#define ERRORS_ONLY 0
#define DEBUG_DETECT 0
#define DEBUG_MESSAGES 0
#define DEBUG_ABORT 0
#define DEBUG_RESET 0
#define DEBUG_RACE 0
#endif
#if EVERY_ACCESS
#undef ERRORS_ONLY
#define ERRORS_ONLY 0
#endif
#if ENABLE_PARITY
#define PARITY_MASK 0x08
#else
#define PARITY_MASK 0x00
#endif
enum chip_type {
unknown = 0x00,
tmc1800 = 0x01,
tmc18c50 = 0x02,
tmc18c30 = 0x03,
};
enum {
in_arbitration = 0x02,
in_selection = 0x04,
in_other = 0x08,
disconnect = 0x10,
aborted = 0x20,
sent_ident = 0x40,
};
enum in_port_type {
Read_SCSI_Data = 0,
SCSI_Status = 1,
TMC_Status = 2,
FIFO_Status = 3,
Interrupt_Cond = 4,
LSB_ID_Code = 5,
MSB_ID_Code = 6,
Read_Loopback = 7,
SCSI_Data_NoACK = 8,
Interrupt_Status = 9,
Configuration1 = 10,
Configuration2 = 11,
Read_FIFO = 12,
FIFO_Data_Count = 14
};
enum out_port_type {
Write_SCSI_Data = 0,
SCSI_Cntl = 1,
Interrupt_Cntl = 2,
SCSI_Mode_Cntl = 3,
TMC_Cntl = 4,
Memory_Cntl = 5,
Write_Loopback = 7,
IO_Control = 11,
Write_FIFO = 12
};
static int port_base = 0;
static void *bios_base = NULL;
static int bios_major = 0;
static int bios_minor = 0;
static int PCI_bus = 0;
static int Quantum = 0;
static int interrupt_level = 0;
static volatile int in_command = 0;
static Scsi_Cmnd *current_SC = NULL;
static enum chip_type chip = unknown;
static int adapter_mask = 0;
static int this_id = 0;
static int setup_called = 0;
#if DEBUG_RACE
static volatile int in_interrupt_flag = 0;
#endif
static int SCSI_Mode_Cntl_port;
static int FIFO_Data_Count_port;
static int Interrupt_Cntl_port;
static int Interrupt_Status_port;
static int Read_FIFO_port;
static int Read_SCSI_Data_port;
static int SCSI_Cntl_port;
static int SCSI_Data_NoACK_port;
static int SCSI_Status_port;
static int TMC_Cntl_port;
static int TMC_Status_port;
static int Write_FIFO_port;
static int Write_SCSI_Data_port;
static int FIFO_Size = 0x2000;
extern void fdomain_16x0_intr( int irq, void *dev_id, struct pt_regs * regs );
static void *addresses[] = {
(void *)0xc8000,
(void *)0xca000,
(void *)0xce000,
(void *)0xde000,
(void *)0xcc000,
(void *)0xd0000,
(void *)0xe0000,
};
#define ADDRESS_COUNT (sizeof( addresses ) / sizeof( unsigned ))
static unsigned short ports[] = { 0x140, 0x150, 0x160, 0x170 };
#define PORT_COUNT (sizeof( ports ) / sizeof( unsigned short ))
static unsigned short ints[] = { 3, 5, 10, 11, 12, 14, 15, 0 };
struct signature {
const char *signature;
int sig_offset;
int sig_length;
int major_bios_version;
int minor_bios_version;
int flag;
} signatures[] = {
{ "FUTURE DOMAIN CORP. (C) 1986-1990 1800-V2.07/28/89", 5, 50, 2, 0, 0 },
{ "FUTURE DOMAIN CORP. (C) 1986-1990 1800-V1.07/28/89", 5, 50, 2, 0, 0 },
{ "FUTURE DOMAIN CORP. (C) 1986-1990 1800-V2.07/28/89", 72, 50, 2, 0, 2 },
{ "FUTURE DOMAIN CORP. (C) 1986-1990 1800-V2.0", 73, 43, 2, 0, 3 },
{ "FUTURE DOMAIN CORP. (C) 1991 1800-V2.0.", 72, 39, 2, 0, 4 },
{ "FUTURE DOMAIN CORP. (C) 1992 V3.00.004/02/92", 5, 44, 3, 0, 0 },
{ "FUTURE DOMAIN TMC-18XX (C) 1993 V3.203/12/93", 5, 44, 3, 2, 0 },
{ "IBM F1 P2 BIOS v1.0104/29/93", 5, 28, 3, -1, 0 },
{ "Future Domain Corp. V1.0008/18/93", 5, 33, 3, 4, 0 },
{ "Future Domain Corp. V1.0008/18/93", 26, 33, 3, 4, 1 },
{ "Adaptec AHA-2920 PCI-SCSI Card", 42, 31, 3, -1, 1 },
{ "IBM F1 P264/32", 5, 14, 3, -1, 1 },
{ "Future Domain Corp. V2.0108/18/93", 5, 33, 3, 5, 0 },
{ "FUTURE DOMAIN CORP.  V3.5008/18/93", 5, 34, 3, 5, 0 },
{ "FUTURE DOMAIN 18c30/18c50/1800 (C) 1994 V3.5", 5, 44, 3, 5, 0 },
{ "FUTURE DOMAIN CORP.  V3.6008/18/93", 5, 34, 3, 6, 0 },
{ "FUTURE DOMAIN CORP.  V3.6108/18/93", 5, 34, 3, 6, 0 },
{ "FUTURE DOMAIN TMC-18XX", 5, 22, -1, -1, 0 },
};
#define SIGNATURE_COUNT (sizeof( signatures ) / sizeof( struct signature ))
static void print_banner( struct Scsi_Host *shpnt )
{
if (!shpnt) return;
if (bios_major < 0 && bios_minor < 0) {
printk( "scsi%d <fdomain>: No BIOS; using scsi id %d\n",
shpnt->host_no, shpnt->this_id );
} else {
printk( "scsi%d <fdomain>: BIOS version ", shpnt->host_no );
if (bios_major >= 0) printk( "%d.", bios_major );
else printk( "?." );
if (bios_minor >= 0) printk( "%d", bios_minor );
else printk( "?." );
printk( " at 0x%x using scsi id %d\n",
(unsigned)bios_base, shpnt->this_id );
}
printk( "scsi%d <fdomain>: %s chip at 0x%x irq ",
shpnt->host_no,
chip == tmc1800 ? "TMC-1800"
: (chip == tmc18c50 ? "TMC-18C50"
: (chip == tmc18c30 ?
(PCI_bus ? "TMC-36C70 (PCI bus)" : "TMC-18C30")
: "Unknown")),
port_base );
if (interrupt_level) printk( "%d", interrupt_level );
else printk( "<none>" );
printk( "\n" );
}
void fdomain_setup( char *str, int *ints )
{
if (setup_called++ || ints[0] < 2 || ints[0] > 3) {
printk( "fdomain: usage: fdomain=<PORT_BASE>,<IRQ>[,<ADAPTER_ID>]\n" );
printk( "fdomain: bad LILO parameters?\n" );
}
port_base = ints[0] >= 1 ? ints[1] : 0;
interrupt_level = ints[0] >= 2 ? ints[2] : 0;
this_id = ints[0] >= 3 ? ints[3] : 0;
bios_major = bios_minor = -1;
}
static void do_pause( unsigned amount )
{
unsigned long the_time = jiffies + amount;
while (jiffies < the_time);
}
inline static void fdomain_make_bus_idle( void )
{
outb( 0, SCSI_Cntl_port );
outb( 0, SCSI_Mode_Cntl_port );
if (chip == tmc18c50 || chip == tmc18c30)
outb( 0x21 | PARITY_MASK, TMC_Cntl_port );
else
outb( 0x01 | PARITY_MASK, TMC_Cntl_port );
}
static int fdomain_is_valid_port( int port )
{
#if DEBUG_DETECT
printk( " (%x%x),",
inb( port + MSB_ID_Code ), inb( port + LSB_ID_Code ) );
#endif
if (inb( port + LSB_ID_Code ) != 0xe9) {
if (inb( port + LSB_ID_Code ) != 0x27) return 0;
if (inb( port + MSB_ID_Code ) != 0x61) return 0;
chip = tmc1800;
} else {
if (inb( port + MSB_ID_Code ) != 0x60) return 0;
chip = tmc18c50;
#if 0
outb( 0x80, port + IO_Control );
if ((inb( port + Configuration2 ) & 0x80) == 0x80) {
outb( 0x00, port + IO_Control );
if ((inb( port + Configuration2 ) & 0x80) == 0x00) {
chip = tmc18c30;
FIFO_Size = 0x800;
}
}
#else
if (inb( port + Configuration2 ) & 0x02) {
chip = tmc18c30;
FIFO_Size = 0x800;
}
#endif
}
return 1;
}
static int fdomain_test_loopback( void )
{
int i;
int result;
for (i = 0; i < 255; i++) {
outb( i, port_base + Write_Loopback );
result = inb( port_base + Read_Loopback );
if (i != result)
return 1;
}
return 0;
}
static int fdomain_get_irq( int base )
{
int options = inb( base + Configuration1 );
#if DEBUG_DETECT
printk( " Options = %x\n", options );
#endif
if (chip != tmc18c30
&& !PCI_bus
&& addresses[ (options & 0xc0) >> 6 ] != bios_base) return 0;
return ints[ (options & 0x0e) >> 1 ];
}
static int fdomain_isa_detect( int *irq, int *iobase )
{
int i;
int base;
int flag = 0;
if (bios_major == 2) {
switch (Quantum) {
case 2:
case 3:
base = *((char *)bios_base + 0x1fa2)
+ (*((char *)bios_base + 0x1fa3) << 8);
break;
case 4:
base = *((char *)bios_base + 0x1fa3)
+ (*((char *)bios_base + 0x1fa4) << 8);
break;
default:
base = *((char *)bios_base + 0x1fcc)
+ (*((char *)bios_base + 0x1fcd) << 8);
break;
}
#if DEBUG_DETECT
printk( " %x,", base );
#endif
for (flag = 0, i = 0; !flag && i < PORT_COUNT; i++) {
if (base == ports[i])
++flag;
}
if (flag && fdomain_is_valid_port( base )) {
*irq = fdomain_get_irq( base );
*iobase = base;
return 1;
}
#if DEBUG_DETECT
printk( " RAM FAILED, " );
#endif
}
for (i = 0; i < PORT_COUNT; i++) {
base = ports[i];
if (check_region( base, 0x10 )) {
#if DEBUG_DETECT
printk( " (%x inuse),", base );
#endif
continue;
}
#if DEBUG_DETECT
printk( " %x,", base );
#endif
if ((flag = fdomain_is_valid_port( base ))) break;
}
if (!flag) return 0;
*irq = fdomain_get_irq( base );
*iobase = base;
return 1;
}
static int fdomain_pci_nobios_detect( int *irq, int *iobase )
{
int i;
int flag = 0;
for (i = 0xfff8; i > 0xe000; i -= 8) {
if (check_region( i, 0x10 )) {
#if DEBUG_DETECT
printk( " (%x inuse)," , i );
#endif
continue;
}
if ((flag = fdomain_is_valid_port( i ))) break;
}
if (!flag) return 0;
*irq = fdomain_get_irq( i );
*iobase = i;
return 1;
}
#ifdef CONFIG_PCI
static int fdomain_pci_bios_detect( int *irq, int *iobase )
{
int error;
unsigned char pci_bus, pci_dev_fn;
unsigned char pci_irq;
unsigned int pci_base;
unsigned short pci_vendor, pci_device;
if (!pcibios_present()) return fdomain_pci_nobios_detect( irq, iobase );
#if DEBUG_DETECT
printk( "\nINFO: cat /proc/pci to see list of PCI devices from bios32\n" );
printk( "\nTMC-3260 detect:"
" Using PCI Vendor ID: 0x%x, PCI Device ID: 0x%x\n",
PCI_VENDOR_ID_FD,
PCI_DEVICE_ID_FD_36C70 );
#endif
pci_bus = 0;
for (pci_dev_fn = 0x0; pci_dev_fn < 0xff; pci_dev_fn++) {
pcibios_read_config_word( pci_bus,
pci_dev_fn,
PCI_VENDOR_ID,
&pci_vendor );
if (pci_vendor == PCI_VENDOR_ID_FD) {
pcibios_read_config_word( pci_bus,
pci_dev_fn,
PCI_DEVICE_ID,
&pci_device );
if (pci_device == PCI_DEVICE_ID_FD_36C70) {
break;
} else {
return 0;
}
}
}
#if DEBUG_DETECT
printk( "Future Domain 36C70 : at PCI bus %u, device %u, function %u\n",
pci_bus,
(pci_dev_fn & 0xf8) >> 3,
pci_dev_fn & 7 );
#endif
if ((error = pcibios_read_config_dword( pci_bus,
pci_dev_fn,
PCI_BASE_ADDRESS_0,
&pci_base ))
|| (error = pcibios_read_config_byte( pci_bus,
pci_dev_fn,
PCI_INTERRUPT_LINE,
&pci_irq ))) {
printk ( "PCI ERROR: Future Domain 36C70 not initializing"
" due to error reading configuration space\n" );
return 0;
} else {
#if DEBUG_DETECT
printk( "TMC-3260 PCI: IRQ = %u, I/O base = 0x%lx\n",
pci_irq, pci_base );
#endif
*irq = pci_irq;
*iobase = (pci_base & 0xfff8);
#if DEBUG_DETECT
printk( "TMC-3260 fix: Masking I/O base address with 0xff00.\n" );
printk( "TMC-3260: IRQ = %d, I/O base = 0x%x\n", *irq, *iobase );
#endif
if (!fdomain_is_valid_port( *iobase )) return 0;
return 1;
}
return 0;
}
#endif
int fdomain_16x0_detect( Scsi_Host_Template *tpnt )
{
int i, j;
int retcode;
struct Scsi_Host *shpnt;
#if DO_DETECT
const int buflen = 255;
Scsi_Cmnd SCinit;
unsigned char do_inquiry[] = { INQUIRY, 0, 0, 0, buflen, 0 };
unsigned char do_request_sense[] = { REQUEST_SENSE, 0, 0, 0, buflen, 0 };
unsigned char do_read_capacity[] = { READ_CAPACITY,
0, 0, 0, 0, 0, 0, 0, 0, 0 };
unsigned char buf[buflen];
#endif
#if DEBUG_DETECT
printk( "fdomain_16x0_detect()," );
#endif
tpnt->proc_dir = &proc_scsi_fdomain;
if (setup_called) {
#if DEBUG_DETECT
printk( "no BIOS, using port_base = 0x%x, irq = %d\n",
port_base, interrupt_level );
#endif
if (!fdomain_is_valid_port( port_base )) {
printk( "fdomain: cannot locate chip at port base 0x%x\n",
port_base );
printk( "fdomain: bad LILO parameters?\n" );
return 0;
}
} else {
int flag = 0;
for (i = 0; !bios_base && i < ADDRESS_COUNT; i++) {
#if DEBUG_DETECT
printk( " %x(%x),", (unsigned)addresses[i], (unsigned)bios_base );
#endif
for (j = 0; !bios_base && j < SIGNATURE_COUNT; j++) {
if (!memcmp( ((char *)addresses[i] + signatures[j].sig_offset),
signatures[j].signature, signatures[j].sig_length )) {
bios_major = signatures[j].major_bios_version;
bios_minor = signatures[j].minor_bios_version;
PCI_bus = (signatures[j].flag == 1);
Quantum = (signatures[j].flag > 1) ? signatures[j].flag : 0;
bios_base = addresses[i];
}
}
}
if (!bios_base) {
#if DEBUG_DETECT
printk( " FAILED: NO BIOS\n" );
#endif
return 0;
}
if (!PCI_bus) {
flag = fdomain_isa_detect( &interrupt_level, &port_base );
} else {
#ifdef CONFIG_PCI
flag = fdomain_pci_bios_detect( &interrupt_level, &port_base );
#else
flag = fdomain_pci_nobios_detect( &interrupt_level, &port_base );
#endif
}
if (!flag) {
#if DEBUG_DETECT
printk( " FAILED: NO PORT\n" );
#endif
#ifdef CONFIG_PCI
printk( "\nTMC-3260 36C70 PCI scsi chip detection failed.\n" );
printk( "Send mail to mckinley@msupa.pa.msu.edu.\n" );
#endif
return 0;
}
}
SCSI_Mode_Cntl_port = port_base + SCSI_Mode_Cntl;
FIFO_Data_Count_port = port_base + FIFO_Data_Count;
Interrupt_Cntl_port = port_base + Interrupt_Cntl;
Interrupt_Status_port = port_base + Interrupt_Status;
Read_FIFO_port = port_base + Read_FIFO;
Read_SCSI_Data_port = port_base + Read_SCSI_Data;
SCSI_Cntl_port = port_base + SCSI_Cntl;
SCSI_Data_NoACK_port = port_base + SCSI_Data_NoACK;
SCSI_Status_port = port_base + SCSI_Status;
TMC_Cntl_port = port_base + TMC_Cntl;
TMC_Status_port = port_base + TMC_Status;
Write_FIFO_port = port_base + Write_FIFO;
Write_SCSI_Data_port = port_base + Write_SCSI_Data;
fdomain_16x0_reset( NULL, 0 );
if (fdomain_test_loopback()) {
#if DEBUG_DETECT
printk( "fdomain: LOOPBACK TEST FAILED, FAILING DETECT!\n" );
#endif
if (setup_called) {
printk( "fdomain: loopback test failed at port base 0x%x\n",
port_base );
printk( "fdomain: bad LILO parameters?\n" );
}
return 0;
}
if (this_id) {
tpnt->this_id = (this_id & 0x07);
adapter_mask = (1 << tpnt->this_id);
} else {
if (PCI_bus || (bios_major == 3 && bios_minor >= 2) || bios_major < 0) {
tpnt->this_id = 7;
adapter_mask = 0x80;
} else {
tpnt->this_id = 6;
adapter_mask = 0x40;
}
}
shpnt = scsi_register( tpnt, 0 );
shpnt->irq = interrupt_level;
shpnt->io_port = port_base;
shpnt->n_io_port = 0x10;
print_banner( shpnt );
if (!interrupt_level) {
panic( "fdomain: *NO* interrupt level selected!\n" );
} else {
retcode = request_irq( interrupt_level,
fdomain_16x0_intr, SA_INTERRUPT, "fdomain", NULL);
if (retcode < 0) {
if (retcode == -EINVAL) {
printk( "fdomain: IRQ %d is bad!\n", interrupt_level );
printk( "         This shouldn't happen!\n" );
printk( "         Send mail to faith@cs.unc.edu\n" );
} else if (retcode == -EBUSY) {
printk( "fdomain: IRQ %d is already in use!\n", interrupt_level );
printk( "         Please use another IRQ!\n" );
} else {
printk( "fdomain: Error getting IRQ %d\n", interrupt_level );
printk( "         This shouldn't happen!\n" );
printk( "         Send mail to faith@cs.unc.edu\n" );
}
panic( "fdomain: Driver requires interruptions\n" );
}
}
request_region( port_base, 0x10, "fdomain" );
#if DO_DETECT
SCinit.request_buffer = SCinit.buffer = buf;
SCinit.request_bufflen = SCinit.bufflen = sizeof(buf)-1;
SCinit.use_sg = 0;
SCinit.lun = 0;
printk( "fdomain: detection routine scanning for devices:\n" );
for (i = 0; i < 8; i++) {
SCinit.target = i;
if (i == tpnt->this_id)
continue;
memcpy(SCinit.cmnd, do_request_sense, sizeof(do_request_sense));
retcode = fdomain_16x0_command(&SCinit);
if (!retcode) {
memcpy(SCinit.cmnd, do_inquiry, sizeof(do_inquiry));
retcode = fdomain_16x0_command(&SCinit);
if (!retcode) {
printk( "     SCSI ID %d: ", i );
for (j = 8; j < (buf[4] < 32 ? buf[4] : 32); j++)
printk( "%c", buf[j] >= 20 ? buf[j] : ' ' );
memcpy(SCinit.cmnd, do_read_capacity, sizeof(do_read_capacity));
retcode = fdomain_16x0_command(&SCinit);
if (!retcode) {
unsigned long blocks, size, capacity;
blocks = (buf[0] << 24) | (buf[1] << 16)
| (buf[2] << 8) | buf[3];
size = (buf[4] << 24) | (buf[5] << 16) | (buf[6] << 8) | buf[7];
capacity = +( +(blocks / 1024L) * +(size * 10L)) / 1024L;
printk( "%lu MB (%lu byte blocks)",
((capacity + 5L) / 10L), size );
} else {
memcpy(SCinit.cmnd, do_request_sense, sizeof(do_request_sense));
retcode = fdomain_16x0_command(&SCinit);
}
printk ("\n" );
} else {
memcpy(SCinit.cmnd, do_request_sense, sizeof(do_request_sense));
retcode = fdomain_16x0_command(&SCinit);
}
}
}
#endif
return 1;
}
const char *fdomain_16x0_info( struct Scsi_Host *ignore )
{
static char buffer[80];
char *pt;
strcpy( buffer, "Future Domain TMC-16x0 SCSI driver, version" );
if (strchr( VERSION, ':')) {
strcat( buffer, strchr( VERSION, ':' ) + 1 );
pt = strrchr( buffer, '$') - 1;
if (!pt)
pt = buffer + strlen( buffer ) - 1;
if (*pt != ' ')
++pt;
*pt = '\0';
} else {
strcat( buffer, " " VERSION );
}
return buffer;
}
int fdomain_16x0_proc_info( char *buffer, char **start, off_t offset,
int length, int hostno, int inout )
{
const char *info = fdomain_16x0_info( NULL );
int len;
int pos;
int begin;
if (inout) return(-ENOSYS);
begin = 0;
strcpy( buffer, info );
strcat( buffer, "\n" );
pos = len = strlen( buffer );
if(pos < offset) {
len = 0;
begin = pos;
}
*start = buffer + (offset - begin);
len -= (offset - begin);
if(len > length) len = length;
return(len);
}
#if 0
static int fdomain_arbitrate( void )
{
int status = 0;
unsigned long timeout;
#if EVERY_ACCESS
printk( "fdomain_arbitrate()\n" );
#endif
outb( 0x00, SCSI_Cntl_port );
outb( adapter_mask, port_base + SCSI_Data_NoACK );
outb( 0x04 | PARITY_MASK, TMC_Cntl_port );
timeout = jiffies + 50;
while (jiffies < timeout) {
status = inb( TMC_Status_port );
if (status & 0x02)
return 0;
}
fdomain_make_bus_idle();
#if EVERY_ACCESS
printk( "Arbitration failed, status = %x\n", status );
#endif
#if ERRORS_ONLY
printk( "fdomain: Arbitration failed, status = %x\n", status );
#endif
return 1;
}
#endif
static int fdomain_select( int target )
{
int status;
unsigned long timeout;
static int flag = 0;
outb( 0x82, SCSI_Cntl_port );
outb( adapter_mask | (1 << target), SCSI_Data_NoACK_port );
outb( PARITY_MASK, TMC_Cntl_port );
timeout = jiffies + 35;
while (jiffies < timeout) {
status = inb( SCSI_Status_port );
if (status & 1) {
outb( 0x80, SCSI_Cntl_port );
return 0;
}
}
fdomain_make_bus_idle();
#if EVERY_ACCESS
if (!target) printk( "Selection failed\n" );
#endif
#if ERRORS_ONLY
if (!target) {
if (!flag)
++flag;
else
printk( "fdomain: Selection failed\n" );
}
#endif
return 1;
}
void my_done( int error )
{
if (in_command) {
in_command = 0;
outb( 0x00, Interrupt_Cntl_port );
fdomain_make_bus_idle();
current_SC->result = error;
if (current_SC->scsi_done)
current_SC->scsi_done( current_SC );
else panic( "fdomain: current_SC->scsi_done() == NULL" );
} else {
panic( "fdomain: my_done() called outside of command\n" );
}
#if DEBUG_RACE
in_interrupt_flag = 0;
#endif
}
void fdomain_16x0_intr( int irq, void *dev_id, struct pt_regs * regs )
{
int status;
int done = 0;
unsigned data_count;
sti();
outb( 0x00, Interrupt_Cntl_port );
if (!in_command || !current_SC) {
#if EVERY_ACCESS
printk( "Spurious interrupt, in_command = %d, current_SC = %x\n",
in_command, current_SC );
#endif
return;
}
if (current_SC->SCp.phase & aborted) {
#if DEBUG_ABORT
printk( "Interrupt after abort, ignoring\n" );
#endif
}
#if DEBUG_RACE
++in_interrupt_flag;
#endif
if (current_SC->SCp.phase & in_arbitration) {
status = inb( TMC_Status_port );
if (!(status & 0x02)) {
#if EVERY_ACCESS
printk( " AFAIL " );
#endif
my_done( DID_BUS_BUSY << 16 );
return;
}
current_SC->SCp.phase = in_selection;
outb( 0x40 | FIFO_COUNT, Interrupt_Cntl_port );
outb( 0x82, SCSI_Cntl_port );
outb( adapter_mask | (1 << current_SC->target), SCSI_Data_NoACK_port );
outb( 0x10 | PARITY_MASK, TMC_Cntl_port );
#if DEBUG_RACE
in_interrupt_flag = 0;
#endif
return;
} else if (current_SC->SCp.phase & in_selection) {
status = inb( SCSI_Status_port );
if (!(status & 0x01)) {
if (fdomain_select( current_SC->target )) {
#if EVERY_ACCESS
printk( " SFAIL " );
#endif
my_done( DID_NO_CONNECT << 16 );
return;
} else {
#if EVERY_ACCESS
printk( " AltSel " );
#endif
outb( 0x10 | PARITY_MASK, TMC_Cntl_port );
}
}
current_SC->SCp.phase = in_other;
outb( 0x90 | FIFO_COUNT, Interrupt_Cntl_port );
outb( 0x80, SCSI_Cntl_port );
#if DEBUG_RACE
in_interrupt_flag = 0;
#endif
return;
}
status = inb( SCSI_Status_port );
if (status & 0x10) {
switch (status & 0x0e) {
case 0x08:
outb( current_SC->cmnd[current_SC->SCp.sent_command++],
Write_SCSI_Data_port );
#if EVERY_ACCESS
printk( "CMD = %x,",
current_SC->cmnd[ current_SC->SCp.sent_command - 1] );
#endif
break;
case 0x00:
if (chip != tmc1800 && !current_SC->SCp.have_data_in) {
current_SC->SCp.have_data_in = -1;
outb( 0xd0 | PARITY_MASK, TMC_Cntl_port );
}
break;
case 0x04:
if (chip != tmc1800 && !current_SC->SCp.have_data_in) {
current_SC->SCp.have_data_in = 1;
outb( 0x90 | PARITY_MASK, TMC_Cntl_port );
}
break;
case 0x0c:
current_SC->SCp.Status = inb( Read_SCSI_Data_port );
#if EVERY_ACCESS
printk( "Status = %x, ", current_SC->SCp.Status );
#endif
#if ERRORS_ONLY
if (current_SC->SCp.Status
&& current_SC->SCp.Status != 2
&& current_SC->SCp.Status != 8) {
printk( "fdomain: target = %d, command = %x, status = %x\n",
current_SC->target,
current_SC->cmnd[0],
current_SC->SCp.Status );
}
#endif
break;
case 0x0a:
outb( MESSAGE_REJECT, Write_SCSI_Data_port );
break;
case 0x0e:
current_SC->SCp.Message = inb( Read_SCSI_Data_port );
#if EVERY_ACCESS
printk( "Message = %x, ", current_SC->SCp.Message );
#endif
if (!current_SC->SCp.Message) ++done;
#if DEBUG_MESSAGES || EVERY_ACCESS
if (current_SC->SCp.Message) {
printk( "fdomain: message = %x\n", current_SC->SCp.Message );
}
#endif
break;
}
}
if (chip == tmc1800
&& !current_SC->SCp.have_data_in
&& (current_SC->SCp.sent_command
>= current_SC->cmd_len)) {
switch (current_SC->cmnd[0]) {
case CHANGE_DEFINITION: case COMPARE: case COPY:
case COPY_VERIFY: case LOG_SELECT: case MODE_SELECT:
case MODE_SELECT_10: case SEND_DIAGNOSTIC: case WRITE_BUFFER:
case FORMAT_UNIT: case REASSIGN_BLOCKS: case RESERVE:
case SEARCH_EQUAL: case SEARCH_HIGH: case SEARCH_LOW:
case WRITE_6: case WRITE_10: case WRITE_VERIFY:
case 0x3f: case 0x41:
case 0xb1: case 0xb0: case 0xb2:
case 0xaa: case 0xae:
case 0x24:
case 0x38: case 0x3d:
case 0xb6:
case 0xea:
current_SC->SCp.have_data_in = -1;
outb( 0xd0 | PARITY_MASK, TMC_Cntl_port );
break;
case 0x00:
default:
current_SC->SCp.have_data_in = 1;
outb( 0x90 | PARITY_MASK, TMC_Cntl_port );
break;
}
}
if (current_SC->SCp.have_data_in == -1) {
while ( (data_count = FIFO_Size - inw( FIFO_Data_Count_port )) > 512 ) {
#if EVERY_ACCESS
printk( "DC=%d, ", data_count ) ;
#endif
if (data_count > current_SC->SCp.this_residual)
data_count = current_SC->SCp.this_residual;
if (data_count > 0) {
#if EVERY_ACCESS
printk( "%d OUT, ", data_count );
#endif
if (data_count == 1) {
outb( *current_SC->SCp.ptr++, Write_FIFO_port );
--current_SC->SCp.this_residual;
} else {
data_count >>= 1;
outsw( Write_FIFO_port, current_SC->SCp.ptr, data_count );
current_SC->SCp.ptr += 2 * data_count;
current_SC->SCp.this_residual -= 2 * data_count;
}
}
if (!current_SC->SCp.this_residual) {
if (current_SC->SCp.buffers_residual) {
--current_SC->SCp.buffers_residual;
++current_SC->SCp.buffer;
current_SC->SCp.ptr = current_SC->SCp.buffer->address;
current_SC->SCp.this_residual = current_SC->SCp.buffer->length;
} else
break;
}
}
}
if (current_SC->SCp.have_data_in == 1) {
while ((data_count = inw( FIFO_Data_Count_port )) > 0) {
#if EVERY_ACCESS
printk( "DC=%d, ", data_count );
#endif
if (data_count > current_SC->SCp.this_residual)
data_count = current_SC->SCp.this_residual;
if (data_count) {
#if EVERY_ACCESS
printk( "%d IN, ", data_count );
#endif
if (data_count == 1) {
*current_SC->SCp.ptr++ = inb( Read_FIFO_port );
--current_SC->SCp.this_residual;
} else {
data_count >>= 1;
insw( Read_FIFO_port, current_SC->SCp.ptr, data_count );
current_SC->SCp.ptr += 2 * data_count;
current_SC->SCp.this_residual -= 2 * data_count;
}
}
if (!current_SC->SCp.this_residual
&& current_SC->SCp.buffers_residual) {
--current_SC->SCp.buffers_residual;
++current_SC->SCp.buffer;
current_SC->SCp.ptr = current_SC->SCp.buffer->address;
current_SC->SCp.this_residual = current_SC->SCp.buffer->length;
}
}
}
if (done) {
#if EVERY_ACCESS
printk( " ** IN DONE %d ** ", current_SC->SCp.have_data_in );
#endif
#if ERRORS_ONLY
if (current_SC->cmnd[0] == REQUEST_SENSE && !current_SC->SCp.Status) {
if ((unsigned char)(*((char *)current_SC->request_buffer+2)) & 0x0f) {
unsigned char key;
unsigned char code;
unsigned char qualifier;
key = (unsigned char)(*((char *)current_SC->request_buffer + 2))
& 0x0f;
code = (unsigned char)(*((char *)current_SC->request_buffer + 12));
qualifier = (unsigned char)(*((char *)current_SC->request_buffer
+ 13));
if (key != UNIT_ATTENTION
&& !(key == NOT_READY
&& code == 0x04
&& (!qualifier || qualifier == 0x02 || qualifier == 0x01))
&& !(key == ILLEGAL_REQUEST && (code == 0x25
|| code == 0x24
|| !code)))
printk( "fdomain: REQUEST SENSE "
"Key = %x, Code = %x, Qualifier = %x\n",
key, code, qualifier );
}
}
#endif
#if EVERY_ACCESS
printk( "BEFORE MY_DONE. . ." );
#endif
my_done( (current_SC->SCp.Status & 0xff)
| ((current_SC->SCp.Message & 0xff) << 8) | (DID_OK << 16) );
#if EVERY_ACCESS
printk( "RETURNING.\n" );
#endif
} else {
if (current_SC->SCp.phase & disconnect) {
outb( 0xd0 | FIFO_COUNT, Interrupt_Cntl_port );
outb( 0x00, SCSI_Cntl_port );
} else {
outb( 0x90 | FIFO_COUNT, Interrupt_Cntl_port );
}
}
#if DEBUG_RACE
in_interrupt_flag = 0;
#endif
return;
}
int fdomain_16x0_queue( Scsi_Cmnd * SCpnt, void (*done)(Scsi_Cmnd *))
{
if (in_command) {
panic( "fdomain: fdomain_16x0_queue() NOT REENTRANT!\n" );
}
#if EVERY_ACCESS
printk( "queue: target = %d cmnd = 0x%02x pieces = %d size = %u\n",
SCpnt->target,
*(unsigned char *)SCpnt->cmnd,
SCpnt->use_sg,
SCpnt->request_bufflen );
#endif
fdomain_make_bus_idle();
current_SC = SCpnt;
current_SC->scsi_done = done;
if (current_SC->use_sg) {
current_SC->SCp.buffer =
(struct scatterlist *)current_SC->request_buffer;
current_SC->SCp.ptr = current_SC->SCp.buffer->address;
current_SC->SCp.this_residual = current_SC->SCp.buffer->length;
current_SC->SCp.buffers_residual = current_SC->use_sg - 1;
} else {
current_SC->SCp.ptr = (char *)current_SC->request_buffer;
current_SC->SCp.this_residual = current_SC->request_bufflen;
current_SC->SCp.buffer = NULL;
current_SC->SCp.buffers_residual = 0;
}
current_SC->SCp.Status = 0;
current_SC->SCp.Message = 0;
current_SC->SCp.have_data_in = 0;
current_SC->SCp.sent_command = 0;
current_SC->SCp.phase = in_arbitration;
outb( 0x00, Interrupt_Cntl_port );
outb( 0x00, SCSI_Cntl_port );
outb( adapter_mask, SCSI_Data_NoACK_port );
++in_command;
outb( 0x20, Interrupt_Cntl_port );
outb( 0x14 | PARITY_MASK, TMC_Cntl_port );
return 0;
}
static volatile int internal_done_flag = 0;
static volatile int internal_done_errcode = 0;
static void internal_done( Scsi_Cmnd *SCpnt )
{
internal_done_errcode = SCpnt->result;
++internal_done_flag;
}
int fdomain_16x0_command( Scsi_Cmnd *SCpnt )
{
fdomain_16x0_queue( SCpnt, internal_done );
while (!internal_done_flag)
;
internal_done_flag = 0;
return internal_done_errcode;
}
void print_info( Scsi_Cmnd *SCpnt )
{
unsigned int imr;
unsigned int irr;
unsigned int isr;
if (!SCpnt || !SCpnt->host) {
printk( "fdomain: cannot provide detailed information\n" );
}
printk( "%s\n", fdomain_16x0_info( SCpnt->host ) );
print_banner( SCpnt->host );
switch (SCpnt->SCp.phase) {
case in_arbitration: printk( "arbitration " ); break;
case in_selection: printk( "selection " ); break;
case in_other: printk( "other " ); break;
default: printk( "unknown " ); break;
}
printk( "(%d), target = %d cmnd = 0x%02x pieces = %d size = %u\n",
SCpnt->SCp.phase,
SCpnt->target,
*(unsigned char *)SCpnt->cmnd,
SCpnt->use_sg,
SCpnt->request_bufflen );
printk( "sent_command = %d, have_data_in = %d, timeout = %d\n",
SCpnt->SCp.sent_command,
SCpnt->SCp.have_data_in,
SCpnt->timeout );
#if DEBUG_RACE
printk( "in_interrupt_flag = %d\n", in_interrupt_flag );
#endif
imr = (inb( 0x0a1 ) << 8) + inb( 0x21 );
outb( 0x0a, 0xa0 );
irr = inb( 0xa0 ) << 8;
outb( 0x0a, 0x20 );
irr += inb( 0x20 );
outb( 0x0b, 0xa0 );
isr = inb( 0xa0 ) << 8;
outb( 0x0b, 0x20 );
isr += inb( 0x20 );
printk( "IMR = 0x%04x", imr );
if (imr & (1 << interrupt_level))
printk( " (masked)" );
printk( ", IRR = 0x%04x, ISR = 0x%04x\n", irr, isr );
printk( "SCSI Status      = 0x%02x\n", inb( SCSI_Status_port ) );
printk( "TMC Status       = 0x%02x", inb( TMC_Status_port ) );
if (inb( TMC_Status_port & 1))
printk( " (interrupt)" );
printk( "\n" );
printk( "Interrupt Status = 0x%02x", inb( Interrupt_Status_port ) );
if (inb( Interrupt_Status_port ) & 0x08)
printk( " (enabled)" );
printk( "\n" );
if (chip == tmc18c50 || chip == tmc18c30) {
printk( "FIFO Status      = 0x%02x\n", inb( port_base + FIFO_Status ) );
printk( "Int. Condition   = 0x%02x\n",
inb( port_base + Interrupt_Cond ) );
}
printk( "Configuration 1  = 0x%02x\n", inb( port_base + Configuration1 ) );
if (chip == tmc18c50 || chip == tmc18c30)
printk( "Configuration 2  = 0x%02x\n",
inb( port_base + Configuration2 ) );
}
int fdomain_16x0_abort( Scsi_Cmnd *SCpnt)
{
unsigned long flags;
#if EVERY_ACCESS || ERRORS_ONLY || DEBUG_ABORT
printk( "fdomain: abort " );
#endif
save_flags( flags );
cli();
if (!in_command) {
#if EVERY_ACCESS || ERRORS_ONLY
printk( " (not in command)\n" );
#endif
restore_flags( flags );
return SCSI_ABORT_NOT_RUNNING;
} else printk( "\n" );
#if DEBUG_ABORT
print_info( SCpnt );
#endif
fdomain_make_bus_idle();
current_SC->SCp.phase |= aborted;
current_SC->result = DID_ABORT << 16;
restore_flags( flags );
my_done( DID_ABORT << 16 );
return SCSI_ABORT_SUCCESS;
}
int fdomain_16x0_reset( Scsi_Cmnd *SCpnt, unsigned int flags )
{
#if DEBUG_RESET
static int called_once = 0;
#endif
#if ERRORS_ONLY
if (SCpnt) printk( "fdomain: SCSI Bus Reset\n" );
#endif
#if DEBUG_RESET
if (called_once) print_info( current_SC );
called_once = 1;
#endif
outb( 1, SCSI_Cntl_port );
do_pause( 2 );
outb( 0, SCSI_Cntl_port );
do_pause( 115 );
outb( 0, SCSI_Mode_Cntl_port );
outb( PARITY_MASK, TMC_Cntl_port );
return SCSI_RESET_WAKEUP;
}
#include "sd.h"
#include <scsi/scsi_ioctl.h>
int fdomain_16x0_biosparam( Scsi_Disk *disk, kdev_t dev, int *info_array )
{
int drive;
unsigned char buf[512 + sizeof( int ) * 2];
int size = disk->capacity;
int *sizes = (int *)buf;
unsigned char *data = (unsigned char *)(sizes + 2);
unsigned char do_read[] = { READ_6, 0, 0, 0, 1, 0 };
int retcode;
struct drive_info {
unsigned short cylinders;
unsigned char heads;
unsigned char sectors;
} *i;
drive = MINOR(dev) / 16;
if (bios_major == 2) {
switch (Quantum) {
case 2:
i = (struct drive_info *)( (char *)bios_base + 0x1f33 + drive * 25 );
break;
case 3:
i = (struct drive_info *)( (char *)bios_base + 0x1f36 + drive * 15 );
break;
case 4:
i = (struct drive_info *)( (char *)bios_base + 0x1f34 + drive * 15 );
break;
default:
i = (struct drive_info *)( (char *)bios_base + 0x1f31 + drive * 25 );
break;
}
info_array[0] = i->heads;
info_array[1] = i->sectors;
info_array[2] = i->cylinders;
} else if (bios_major == 3
&& bios_minor >= 0
&& bios_minor < 4) {
i = (struct drive_info *)( (char *)bios_base + 0x1f71 + drive * 10 );
info_array[0] = i->heads + 1;
info_array[1] = i->sectors;
info_array[2] = i->cylinders;
} else {
sizes[0] = 0;
sizes[1] = 512;
memcpy( data, do_read, sizeof( do_read ) );
retcode = kernel_scsi_ioctl( disk->device,
SCSI_IOCTL_SEND_COMMAND,
(void *)buf );
if (!retcode
&& data[511] == 0xaa && data[510] == 0x55
&& data[0x1c2]) {
info_array[0] = data[0x1c3] + 1;
info_array[1] = data[0x1c4] & 0x3f;
} else {
if ((unsigned int)size >= 0x7e0000U) {
info_array[0] = 0xff;
info_array[1] = 0x3f;
} else if ((unsigned int)size >= 0x200000U) {
info_array[0] = 0x80;
info_array[1] = 0x3f;
} else {
info_array[0] = 0x40;
info_array[1] = 0x20;
}
}
info_array[2] = (unsigned int)size / (info_array[0] * info_array[1] );
}
return 0;
}
#ifdef MODULE
Scsi_Host_Template driver_template = FDOMAIN_16X0;
#include "scsi_module.c"
#endif