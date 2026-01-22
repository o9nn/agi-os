#include <linux/module.h>
#include <linux/config.h>
#include <linux/delay.h>
#include <linux/signal.h>
#include <linux/sched.h>
#include <linux/errno.h>
#include <linux/bios32.h>
#include <linux/pci.h>
#include <linux/string.h>
#include <linux/blk.h>
#include <asm/io.h>
#include <asm/system.h>
#include "scsi.h"
#include "hosts.h"
#include "AM53C974.h"
#include "constants.h"
#include "sd.h"
#ifdef AM53C974_DEBUG
#define DEB(x) x
#ifdef AM53C974_DEBUG_KEYWAIT
#define KEYWAIT() AM53C974_keywait()
#else
#define KEYWAIT()
#endif
#ifdef AM53C974_DEBUG_INIT
#define DEB_INIT(x) x
#else
#define DEB_INIT(x)
#endif
#ifdef AM53C974_DEBUG_MSG
#define DEB_MSG(x) x
#else
#define DEB_MSG(x)
#endif
#ifdef AM53C974_DEB_RESEL
#define DEB_RESEL(x) x
#else
#define DEB_RESEL(x)
#endif
#ifdef AM53C974_DEBUG_QUEUE
#define DEB_QUEUE(x) x
#define LIST(x,y) {printk("LINE:%d   Adding %p to %p\n", __LINE__, (void*)(x), (void*)(y)); if ((x)==(y)) udelay(5); }
#define REMOVE(w,x,y,z) {printk("LINE:%d   Removing: %p->%p  %p->%p \n", __LINE__, (void*)(w), (void*)(x), (void*)(y), (void*)(z)); if ((x)==(y)) udelay(5); }
#else
#define DEB_QUEUE(x)
#define LIST(x,y)
#define REMOVE(w,x,y,z)
#endif
#ifdef AM53C974_DEBUG_INFO
#define DEB_INFO(x) x
#else
#define DEB_INFO(x)
#endif
#ifdef AM53C974_DEBUG_LINKED
#define DEB_LINKED(x) x
#else
#define DEB_LINKED(x)
#endif
#ifdef AM53C974_DEBUG_INTR
#define DEB_INTR(x) x
#else
#define DEB_INTR(x)
#endif
#else
#define DEB_INIT(x)
#define DEB(x)
#define DEB_QUEUE(x)
#define LIST(x,y)
#define REMOVE(w,x,y,z)
#define DEB_INFO(x)
#define DEB_LINKED(x)
#define DEB_INTR(x)
#define DEB_MSG(x)
#define DEB_RESEL(x)
#define KEYWAIT()
#endif
#ifdef AM53C974_DEBUG_ABORT
#define DEB_ABORT(x) x
#else
#define DEB_ABORT(x)
#endif
#ifdef VERBOSE_AM53C974_DEBUG
#define VDEB(x) x
#else
#define VDEB(x)
#endif
#define INSIDE(x,l,h) ( ((x) >= (l)) && ((x) <= (h)) )
#ifdef AM53C974_DEBUG
static void AM53C974_print_pci(struct Scsi_Host *instance);
static void AM53C974_print_phase(struct Scsi_Host *instance);
static void AM53C974_print_queues(struct Scsi_Host *instance);
#endif
static void AM53C974_print(struct Scsi_Host *instance);
static void AM53C974_keywait(void);
static int AM53C974_bios_detect(Scsi_Host_Template *tpnt);
static int AM53C974_nobios_detect(Scsi_Host_Template *tpnt);
static int AM53C974_init(Scsi_Host_Template *tpnt, pci_config_t pci_config);
static void AM53C974_config_after_reset(struct Scsi_Host *instance);
static __inline__ void initialize_SCp(Scsi_Cmnd *cmd);
static __inline__ void run_main(void);
static void AM53C974_main (void);
static void AM53C974_intr(int irq, void *dev_id, struct pt_regs *regs);
static void AM53C974_intr_disconnect(struct Scsi_Host *instance);
static int AM53C974_sync_neg(struct Scsi_Host *instance, int target, unsigned char *msg);
static __inline__ void AM53C974_set_async(struct Scsi_Host *instance, int target);
static __inline__ void AM53C974_set_sync(struct Scsi_Host *instance, int target);
static void AM53C974_information_transfer(struct Scsi_Host *instance,
unsigned char statreg, unsigned char isreg,
unsigned char instreg, unsigned char cfifo,
unsigned char dmastatus);
static int AM53C974_message(struct Scsi_Host *instance, Scsi_Cmnd *cmd, unsigned char msg);
static void AM53C974_select(struct Scsi_Host *instance, Scsi_Cmnd *cmd, int tag);
static void AM53C974_intr_reselect(struct Scsi_Host *instance, unsigned char statreg);
static __inline__ void AM53C974_transfer_dma(struct Scsi_Host *instance, short dir,
unsigned long length, char *data);
static void AM53C974_dma_blast(struct Scsi_Host *instance, unsigned char dmastatus,
unsigned char statreg);
static void AM53C974_intr_bus_reset(struct Scsi_Host *instance);
static struct Scsi_Host *first_instance = NULL;
static Scsi_Host_Template *the_template = NULL;
static struct Scsi_Host *first_host = NULL;
static volatile int main_running = 0;
static int commandline_current = 0;
override_t overrides[7] = { {-1, 0, 0, 0}, };
struct proc_dir_entry proc_scsi_am53c974 = {
PROC_SCSI_AM53C974, 8, "am53c974",
S_IFDIR | S_IRUGO | S_IXUGO, 2
};
#ifdef AM53C974_DEBUG
static int deb_stop = 1;
static void AM53C974_print_pci(struct Scsi_Host *instance)
{
int i;
unsigned short vendor_id, device_id, command, status, scratch[8];
unsigned long class_revision, base;
unsigned char irq, cache_line_size, latency_timer, header_type;
AM53C974_PCIREG_OPEN();
for (i = 0; i < 8; i++) *(scratch + i) = AM53C974_PCIREG_READ_WORD(instance, PCI_SCRATCH_REG_0 + 2*i);
vendor_id = AM53C974_PCIREG_READ_WORD(instance, PCI_VENDOR_ID);
device_id = AM53C974_PCIREG_READ_WORD(instance, PCI_DEVICE_ID);
command = AM53C974_PCIREG_READ_WORD(instance, PCI_COMMAND);
status = AM53C974_PCIREG_READ_WORD(instance, PCI_STATUS);
class_revision = AM53C974_PCIREG_READ_DWORD(instance, PCI_CLASS_REVISION);
cache_line_size = AM53C974_PCIREG_READ_BYTE(instance, PCI_CACHE_LINE_SIZE);
latency_timer = AM53C974_PCIREG_READ_BYTE(instance, PCI_LATENCY_TIMER);
header_type = AM53C974_PCIREG_READ_BYTE(instance, PCI_HEADER_TYPE);
base = AM53C974_PCIREG_READ_DWORD(instance, PCI_BASE_ADDRESS_0);
irq = AM53C974_PCIREG_READ_BYTE(instance, PCI_INTERRUPT_LINE);
AM53C974_PCIREG_CLOSE();
printk("------------- start of PCI register dump -------------\n");
printk("PCI_VENDOR_ID:       0x%x\n", vendor_id);
printk("PCI_DEVICE_ID:       0x%x\n", device_id);
printk("PCI_COMMAND:         0x%x\n", command);
printk("PCI_STATUS:          0x%x\n", status);
printk("PCI_CLASS_REVISION:  0x%lx\n", class_revision);
printk("PCI_CACHE_LINE_SIZE: 0x%x\n", cache_line_size);
printk("PCI_LATENCY_TIMER:   0x%x\n", latency_timer);
printk("PCI_HEADER_TYPE:     0x%x\n", header_type);
printk("PCI_BASE_ADDRESS_0:  0x%lx\n", base);
printk("PCI_INTERRUPT_LINE:  %d\n", irq);
for (i = 0; i < 8; i++) printk("PCI_SCRATCH_%d:       0x%x\n", i, scratch[i]);
printk("------------- end of PCI register dump -------------\n\n");
}
static struct {
unsigned char value;
char *name;
} phases[] = {
{PHASE_DATAOUT, "DATAOUT"}, {PHASE_DATAIN, "DATAIN"}, {PHASE_CMDOUT, "CMDOUT"},
{PHASE_STATIN, "STATIN"}, {PHASE_MSGOUT, "MSGOUT"}, {PHASE_MSGIN, "MSGIN"},
{PHASE_RES_0, "RESERVED 0"}, {PHASE_RES_1, "RESERVED 1"}};
static void AM53C974_print_phase(struct Scsi_Host *instance)
{
AM53C974_local_declare();
unsigned char statreg, latched;
int i;
AM53C974_setio(instance);
latched = (AM53C974_read_8(CNTLREG2)) & CNTLREG2_ENF;
statreg = AM53C974_read_8(STATREG);
for (i = 0; (phases[i].value != PHASE_RES_1) &&
(phases[i].value != (statreg & STATREG_PHASE)); ++i);
if (latched)
printk("scsi%d : phase %s, latched at end of last command\n", instance->host_no, phases[i].name);
else
printk("scsi%d : phase %s, real time\n", instance->host_no, phases[i].name);
}
static void AM53C974_print_queues(struct Scsi_Host *instance)
{
struct AM53C974_hostdata *hostdata = (struct AM53C974_hostdata *)instance->hostdata;
Scsi_Cmnd *ptr;
printk("AM53C974: coroutine is%s running.\n", main_running ? "" : "n't");
cli();
if (!hostdata->connected) {
printk ("scsi%d: no currently connected command\n", instance->host_no); }
else {
print_Scsi_Cmnd ((Scsi_Cmnd *)hostdata->connected); }
if (!hostdata->sel_cmd) {
printk ("scsi%d: no currently arbitrating command\n", instance->host_no); }
else {
print_Scsi_Cmnd ((Scsi_Cmnd *)hostdata->sel_cmd); }
printk ("scsi%d: issue_queue ", instance->host_no);
if (!hostdata->issue_queue)
printk("empty\n");
else {
printk(":\n");
for (ptr = (Scsi_Cmnd *)hostdata->issue_queue; ptr; ptr = (Scsi_Cmnd *)ptr->host_scribble)
print_Scsi_Cmnd (ptr); }
printk ("scsi%d: disconnected_queue ", instance->host_no);
if (!hostdata->disconnected_queue)
printk("empty\n");
else {
printk(":\n");
for (ptr = (Scsi_Cmnd *) hostdata->disconnected_queue; ptr; ptr = (Scsi_Cmnd *)ptr->host_scribble)
print_Scsi_Cmnd (ptr); }
sti();
}
#endif
static void AM53C974_print(struct Scsi_Host *instance)
{
AM53C974_local_declare();
unsigned long ctcreg, dmastc, dmaspa, dmawbc, dmawac;
unsigned char cmdreg, statreg, isreg, cfireg, cntlreg[4], dmacmd, dmastatus;
AM53C974_setio(instance);
cli();
ctcreg = AM53C974_read_8(CTCHREG) << 16;
ctcreg |= AM53C974_read_8(CTCMREG) << 8;
ctcreg |= AM53C974_read_8(CTCLREG);
cmdreg = AM53C974_read_8(CMDREG);
statreg = AM53C974_read_8(STATREG);
isreg = AM53C974_read_8(ISREG);
cfireg = AM53C974_read_8(CFIREG);
cntlreg[0] = AM53C974_read_8(CNTLREG1);
cntlreg[1] = AM53C974_read_8(CNTLREG2);
cntlreg[2] = AM53C974_read_8(CNTLREG3);
cntlreg[3] = AM53C974_read_8(CNTLREG4);
dmacmd = AM53C974_read_8(DMACMD);
dmastc = AM53C974_read_32(DMASTC);
dmaspa = AM53C974_read_32(DMASPA);
dmawbc = AM53C974_read_32(DMAWBC);
dmawac = AM53C974_read_32(DMAWAC);
dmastatus = AM53C974_read_8(DMASTATUS);
sti();
printk("AM53C974 register dump:\n");
printk("IO base: 0x%04lx; CTCREG: 0x%04lx; CMDREG: 0x%02x; STATREG: 0x%02x; ISREG: 0x%02x\n",
io_port, ctcreg, cmdreg, statreg, isreg);
printk("CFIREG: 0x%02x; CNTLREG1-4: 0x%02x; 0x%02x; 0x%02x; 0x%02x\n",
cfireg, cntlreg[0], cntlreg[1], cntlreg[2], cntlreg[3]);
printk("DMACMD: 0x%02x; DMASTC: 0x%04lx; DMASPA: 0x%04lx\n", dmacmd, dmastc, dmaspa);
printk("DMAWBC: 0x%04lx; DMAWAC: 0x%04lx; DMASTATUS: 0x%02x\n", dmawbc, dmawac, dmastatus);
printk("---------------------------------------------------------\n");
}
static void AM53C974_keywait(void)
{
#ifdef AM53C974_DEBUG
int key;
if (!deb_stop) return;
#endif
cli();
while ((inb_p(0x64) & 0x01) != 0x01) ;
#ifdef AM53C974_DEBUG
key = inb(0x60);
if (key == 0x93) deb_stop = 0;
#endif
sti();
}
void AM53C974_setup(char *str, int *ints)
{
if (ints[0] < 4)
printk("AM53C974_setup: wrong number of parameters;\n correct syntax is: AM53C974=host-scsi-id, target-scsi-id, max-rate, max-offset\n");
else {
if (commandline_current < (sizeof(overrides) / sizeof(override_t))) {
if ((ints[1] < 0) || (ints[1] > 7) ||
(ints[2] < 0) || (ints[2] > 7) ||
(ints[1] == ints[2]) ||
(ints[3] < (DEF_CLK / MAX_PERIOD)) || (ints[3] > (DEF_CLK / MIN_PERIOD)) ||
(ints[4] < 0) || (ints[4] > MAX_OFFSET))
printk("AM53C974_setup: illegal parameter\n");
else {
overrides[commandline_current].host_scsi_id = ints[1];
overrides[commandline_current].target_scsi_id = ints[2];
overrides[commandline_current].max_rate = ints[3];
overrides[commandline_current].max_offset = ints[4];
commandline_current++; }
}
else
printk("AM53C974_setup: too many overrides\n");
}
}
#if defined (CONFIG_PCI)
int AM53C974_bios_detect(Scsi_Host_Template *tpnt)
{
int count = 0;
int pci_index;
pci_config_t pci_config;
for (pci_index = 0; pci_index <= 16; ++pci_index) {
unsigned char pci_bus, pci_device_fn;
if (pcibios_find_device(PCI_VENDOR_ID_AMD, PCI_DEVICE_ID_AMD_SCSI, pci_index, &pci_bus, &pci_device_fn) != 0)
break;
pcibios_read_config_word(pci_bus, pci_device_fn, PCI_VENDOR_ID, &pci_config._vendor);
pcibios_read_config_word(pci_bus, pci_device_fn, PCI_DEVICE_ID, &pci_config._device);
pcibios_read_config_word(pci_bus, pci_device_fn, PCI_COMMAND, &pci_config._command);
pcibios_read_config_word(pci_bus, pci_device_fn, PCI_STATUS, &pci_config._status);
pcibios_read_config_dword(pci_bus, pci_device_fn, PCI_CLASS_REVISION, &pci_config._class_revision);
pcibios_read_config_byte(pci_bus, pci_device_fn, PCI_CACHE_LINE_SIZE, &pci_config._cache_line_size);
pcibios_read_config_byte(pci_bus, pci_device_fn, PCI_LATENCY_TIMER, &pci_config._latency_timer);
pcibios_read_config_byte(pci_bus, pci_device_fn, PCI_HEADER_TYPE, &pci_config._header_type);
pcibios_read_config_byte(pci_bus, pci_device_fn, PCI_BIST, &pci_config._bist);
pcibios_read_config_dword(pci_bus, pci_device_fn, PCI_BASE_ADDRESS_0, &pci_config._base0);
pcibios_read_config_dword(pci_bus, pci_device_fn, PCI_BASE_ADDRESS_1, &pci_config._base1);
pcibios_read_config_dword(pci_bus, pci_device_fn, PCI_BASE_ADDRESS_2, &pci_config._base2);
pcibios_read_config_dword(pci_bus, pci_device_fn, PCI_BASE_ADDRESS_3, &pci_config._base3);
pcibios_read_config_dword(pci_bus, pci_device_fn, PCI_BASE_ADDRESS_4, &pci_config._base4);
pcibios_read_config_dword(pci_bus, pci_device_fn, PCI_BASE_ADDRESS_5, &pci_config._base5);
pcibios_read_config_dword(pci_bus, pci_device_fn, PCI_ROM_ADDRESS, &pci_config._baserom);
pcibios_read_config_byte(pci_bus, pci_device_fn, PCI_INTERRUPT_LINE, &pci_config._int_line);
pcibios_read_config_byte(pci_bus, pci_device_fn, PCI_INTERRUPT_PIN, &pci_config._int_pin);
pcibios_read_config_byte(pci_bus, pci_device_fn, PCI_MIN_GNT, &pci_config._min_gnt);
pcibios_read_config_byte(pci_bus, pci_device_fn, PCI_MAX_LAT, &pci_config._max_lat);
pci_config._pcibus = 0xFFFFFFFF;
pci_config._cardnum = 0xFFFFFFFF;
if (!(pci_config._command & PCI_COMMAND_IO)) continue;
if (!(pci_config._command & PCI_COMMAND_MASTER)) {
pci_config._command |= PCI_COMMAND_MASTER;
printk("PCI Master Bit has not been set. Setting...\n");
pcibios_write_config_word(pci_bus, pci_device_fn, PCI_COMMAND, pci_config._command); }
if (AM53C974_init(tpnt, pci_config)) count++ ;
}
return (count);
}
#endif
int AM53C974_nobios_detect(Scsi_Host_Template *tpnt)
{
int count = 0;
pci_config_t pci_config;
for (pci_config._pcibus = 0; pci_config._pcibus < 0x10; pci_config._pcibus++) {
for (pci_config._cardnum = 0; pci_config._cardnum < 0x20; pci_config._cardnum++) {
unsigned long config_cmd;
config_cmd = 0x80000000 | (pci_config._pcibus<<16) | (pci_config._cardnum<<11);
outl(config_cmd, 0xCF8);
pci_config._device_vendor = inl(0xCFC);
if ((pci_config._vendor == PCI_VENDOR_ID_AMD) && (pci_config._device == PCI_DEVICE_ID_AMD_SCSI)) {
outl(config_cmd | PCI_COMMAND, 0xCF8); pci_config._status_command = inl(0xCFC);
outl(config_cmd | PCI_CLASS_REVISION, 0xCF8); pci_config._class_revision = inl(0xCFC);
outl(config_cmd | PCI_CACHE_LINE_SIZE, 0xCF8); pci_config._bist_header_latency_cache = inl(0xCFC);
outl(config_cmd | PCI_BASE_ADDRESS_0, 0xCF8); pci_config._base0 = inl(0xCFC);
outl(config_cmd | PCI_BASE_ADDRESS_1, 0xCF8); pci_config._base1 = inl(0xCFC);
outl(config_cmd | PCI_BASE_ADDRESS_2, 0xCF8); pci_config._base2 = inl(0xCFC);
outl(config_cmd | PCI_BASE_ADDRESS_3, 0xCF8); pci_config._base3 = inl(0xCFC);
outl(config_cmd | PCI_BASE_ADDRESS_4, 0xCF8); pci_config._base4 = inl(0xCFC);
outl(config_cmd | PCI_BASE_ADDRESS_5, 0xCF8); pci_config._base5 = inl(0xCFC);
outl(config_cmd | PCI_ROM_ADDRESS, 0xCF8); pci_config._baserom = inl(0xCFC);
outl(config_cmd | PCI_INTERRUPT_LINE, 0xCF8); pci_config._max_min_ipin_iline = inl(0xCFC);
if (!(pci_config._command & PCI_COMMAND_IO)) continue;
if (!(pci_config._command & PCI_COMMAND_MASTER)) {
pci_config._command |= PCI_COMMAND_MASTER;
printk("Config 1; PCI Master Bit has not been set. Setting...\n");
outl(config_cmd | PCI_COMMAND, 0xCF8); outw(pci_config._command, 0xCFC); }
if (AM53C974_init(tpnt, pci_config)) count++ ;
}
}
}
outb(0, 0xCF8);
if (!count) {
AM53C974_PCIREG_OPEN();
pci_config._pcibus = 0xFFFFFFFF;
pci_config._cardnum = 0xFFFFFFFF;
for (pci_config._ioaddr = 0xC000; pci_config._ioaddr < 0xD000; pci_config._ioaddr += 0x0100) {
pci_config._device_vendor = inl(pci_config._ioaddr);
if ((pci_config._vendor == PCI_VENDOR_ID_AMD) && (pci_config._device == PCI_DEVICE_ID_AMD_SCSI)) {
pci_config._status_command = inl(pci_config._ioaddr + PCI_COMMAND);
pci_config._class_revision = inl(pci_config._ioaddr + PCI_CLASS_REVISION);
pci_config._bist_header_latency_cache = inl(pci_config._ioaddr + PCI_CACHE_LINE_SIZE);
pci_config._base0 = inl(pci_config._ioaddr + PCI_BASE_ADDRESS_0);
pci_config._base1 = inl(pci_config._ioaddr + PCI_BASE_ADDRESS_1);
pci_config._base2 = inl(pci_config._ioaddr + PCI_BASE_ADDRESS_2);
pci_config._base3 = inl(pci_config._ioaddr + PCI_BASE_ADDRESS_3);
pci_config._base4 = inl(pci_config._ioaddr + PCI_BASE_ADDRESS_4);
pci_config._base5 = inl(pci_config._ioaddr + PCI_BASE_ADDRESS_5);
pci_config._baserom = inl(pci_config._ioaddr + PCI_ROM_ADDRESS);
pci_config._max_min_ipin_iline = inl(pci_config._ioaddr + PCI_INTERRUPT_LINE);
if (!(pci_config._command & PCI_COMMAND_IO)) continue;
if (!(pci_config._command & PCI_COMMAND_MASTER)) {
pci_config._command |= PCI_COMMAND_MASTER;
printk("Config 2; PCI Master Bit has not been set. Setting...\n");
outw(pci_config._command, pci_config._ioaddr + PCI_COMMAND); }
if (AM53C974_init(tpnt, pci_config)) count++ ;
}
}
AM53C974_PCIREG_CLOSE();
}
return(count);
}
int AM53C974_detect(Scsi_Host_Template *tpnt)
{
int count;
tpnt->proc_dir = &proc_scsi_am53c974;
#if defined (CONFIG_PCI)
if (pcibios_present())
count = AM53C974_bios_detect(tpnt);
else
#endif
count = AM53C974_nobios_detect(tpnt);
return (count);
}
static int AM53C974_init(Scsi_Host_Template *tpnt, pci_config_t pci_config)
{
AM53C974_local_declare();
int i, j;
struct Scsi_Host *instance, *search;
struct AM53C974_hostdata *hostdata;
#ifdef AM53C974_OPTION_DEBUG_PROBE_ONLY
printk ("AM53C974: probe only enabled, aborting initialization\n");
return 0;
#endif
instance = scsi_register(tpnt, sizeof(struct AM53C974_hostdata));
hostdata = (struct AM53C974_hostdata *)instance->hostdata;
instance->base = NULL;
instance->io_port = pci_config._base0 & (pci_config._base0 & 0x1 ?
0xFFFFFFFC : 0xFFFFFFF0);
instance->irq = pci_config._int_line;
instance->dma_channel = -1;
AM53C974_setio(instance);
#ifdef AM53C974_SCSI_ID
instance->this_id = AM53C974_SCSI_ID;
AM53C974_write_8(CNTLREG1, instance->this_id & CNTLREG1_SID);
#else
instance->this_id = AM53C974_read_8(CNTLREG1) & CNTLREG1_SID;
if (instance->this_id != 7)
printk("scsi%d: WARNING: unusual hostadapter SCSI id %d; please verify!\n",
instance->host_no, instance->this_id);
#endif
for (i = 0; i < sizeof(hostdata->msgout); i++) {
hostdata->msgout[i] = NOP;
hostdata->last_message[i] = NOP; }
for (i = 0; i < 8; i++) {
hostdata->busy[i] = 0;
hostdata->sync_per[i] = DEF_STP;
hostdata->sync_off[i] = 0;
hostdata->sync_neg[i] = 0;
hostdata->sync_en[i] = DEFAULT_SYNC_NEGOTIATION_ENABLED;
hostdata->max_rate[i] = DEFAULT_RATE;
hostdata->max_offset[i] = DEFAULT_SYNC_OFFSET; }
for (i = 0; i < commandline_current; i++) {
if (overrides[i].host_scsi_id == instance->this_id) {
j = overrides[i].target_scsi_id;
hostdata->sync_en[j] = 1;
hostdata->max_rate[j] = overrides[i].max_rate;
hostdata->max_offset[j] = overrides[i].max_offset;
}
}
hostdata->sel_cmd = NULL;
hostdata->connected = NULL;
hostdata->issue_queue = NULL;
hostdata->disconnected_queue = NULL;
hostdata->in_reset = 0;
hostdata->aborted = 0;
hostdata->selecting = 0;
hostdata->disconnecting = 0;
hostdata->dma_busy = 0;
for (search = first_host;
search && ( ((the_template != NULL) && (search->hostt != the_template)) ||
(search->irq != instance->irq) || (search == instance) );
search = search->next);
if (!search) {
if (request_irq(instance->irq, AM53C974_intr, SA_INTERRUPT, "AM53C974", NULL)) {
printk("scsi%d: IRQ%d not free, detaching\n", instance->host_no, instance->irq);
scsi_unregister(instance);
return 0; }
}
else {
printk("scsi%d: using interrupt handler previously installed for scsi%d\n",
instance->host_no, search->host_no); }
if (!the_template) {
the_template = instance->hostt;
first_instance = instance; }
AM53C974_write_8(CMDREG, CMDREG_RDEV);
udelay(5);
AM53C974_write_8(CMDREG, CMDREG_NOP);
AM53C974_write_8(CNTLREG1, CNTLREG1_DISR | instance->this_id);
AM53C974_write_8(CMDREG, CMDREG_RBUS);
udelay(10);
AM53C974_config_after_reset(instance);
udelay(500000);
return(1);
}
static void AM53C974_config_after_reset(struct Scsi_Host *instance)
{
AM53C974_local_declare();
AM53C974_setio(instance);
AM53C974_write_8(CMDREG, CMDREG_CFIFO);
AM53C974_write_8(STIMREG, DEF_SCSI_TIMEOUT);
AM53C974_write_8(STPREG, DEF_STP & STPREG_STP);
AM53C974_write_8(SOFREG, (DEF_SOF_RAD<<6) | (DEF_SOF_RAA<<4));
AM53C974_write_8(CLKFREG, DEF_CLKF & CLKFREG_MASK);
AM53C974_write_8(CNTLREG1, (DEF_ETM<<7) | CNTLREG1_DISR | (DEF_PERE<<4) | instance->this_id);
AM53C974_write_8(CNTLREG2, (DEF_ENF<<6));
AM53C974_write_8(CNTLREG3, (DEF_ADIDCHK<<7) | (DEF_FASTSCSI<<4) | (DEF_FASTCLK<<3));
AM53C974_write_8(CNTLREG4, (DEF_GLITCH<<6) | (DEF_PWD<<5) | (DEF_RAE<<3) | (DEF_RADE<<2) | CNTLREG4_RES);
}
const char *AM53C974_info(struct Scsi_Host *instance)
{
static char info[100];
sprintf(info, "AM53/79C974 PCscsi driver rev. %d.%d; host I/O address: 0x%x; irq: %d\n",
AM53C974_DRIVER_REVISION_MAJOR, AM53C974_DRIVER_REVISION_MINOR,
instance->io_port, instance->irq);
return (info);
}
int AM53C974_command(Scsi_Cmnd *SCpnt)
{
DEB(printk("AM53C974_command called\n"));
return 0;
}
static __inline__ void initialize_SCp(Scsi_Cmnd *cmd)
{
if (cmd->use_sg) {
cmd->SCp.buffer = (struct scatterlist *)cmd->buffer;
cmd->SCp.buffers_residual = cmd->use_sg - 1;
cmd->SCp.ptr = (char *)cmd->SCp.buffer->address;
cmd->SCp.this_residual = cmd->SCp.buffer->length; }
else {
cmd->SCp.buffer = NULL;
cmd->SCp.buffers_residual = 0;
cmd->SCp.ptr = (char *)cmd->request_buffer;
cmd->SCp.this_residual = cmd->request_bufflen; }
}
static __inline__ void run_main(void)
{
cli();
if (!main_running) {
main_running = 1;
AM53C974_main();
sti(); }
else
sti();
}
int AM53C974_queue_command(Scsi_Cmnd *cmd, void (*done)(Scsi_Cmnd *))
{
struct Scsi_Host *instance = cmd->host;
struct AM53C974_hostdata *hostdata = (struct AM53C974_hostdata *)instance->hostdata;
Scsi_Cmnd *tmp;
cli();
DEB_QUEUE(printk(SEPARATOR_LINE));
DEB_QUEUE(printk("scsi%d: AM53C974_queue_command called\n", instance->host_no));
DEB_QUEUE(printk("cmd=%02x target=%02x lun=%02x bufflen=%d use_sg = %02x\n",
cmd->cmnd[0], cmd->target, cmd->lun, cmd->request_bufflen, cmd->use_sg));
cmd->host_scribble = NULL;
cmd->scsi_done = done;
cmd->result = 0;
cmd->device->disconnect = 0;
if (!(hostdata->issue_queue) || (cmd->cmnd[0] == REQUEST_SENSE)) {
LIST(cmd, hostdata->issue_queue);
cmd->host_scribble = (unsigned char *)hostdata->issue_queue;
hostdata->issue_queue = cmd; }
else {
for (tmp = (Scsi_Cmnd *)hostdata->issue_queue; tmp->host_scribble;
tmp = (Scsi_Cmnd *)tmp->host_scribble);
LIST(cmd, tmp);
tmp->host_scribble = (unsigned char *)cmd; }
DEB_QUEUE(printk("scsi%d : command added to %s of queue\n", instance->host_no,
(cmd->cmnd[0] == REQUEST_SENSE) ? "head" : "tail"));
run_main();
return 0;
}
static void AM53C974_main(void)
{
AM53C974_local_declare();
Scsi_Cmnd *tmp, *prev;
struct Scsi_Host *instance;
struct AM53C974_hostdata *hostdata;
int done;
do {
cli();
done = 1;
for (instance = first_instance; instance && instance->hostt == the_template;
instance = instance->next) {
hostdata = (struct AM53C974_hostdata *)instance->hostdata;
AM53C974_setio(instance);
if (!hostdata->connected && !hostdata->sel_cmd) {
for (tmp = (Scsi_Cmnd *)hostdata->issue_queue, prev = NULL; tmp;
prev = tmp, tmp = (Scsi_Cmnd *)tmp->host_scribble) {
if (!(hostdata->busy[tmp->target] & (1 << tmp->lun))) {
if (prev) {
REMOVE(prev, (Scsi_Cmnd *)(prev->host_scribble), tmp,
(Scsi_Cmnd *)(tmp->host_scribble));
prev->host_scribble = tmp->host_scribble; }
else {
REMOVE(-1, hostdata->issue_queue, tmp, tmp->host_scribble);
hostdata->issue_queue = (Scsi_Cmnd *)tmp->host_scribble; }
tmp->host_scribble = NULL;
hostdata->selecting = 1;
hostdata->sel_cmd = tmp;
AM53C974_write_8(CMDREG, CMDREG_DSR);
break;
}
}
}
else {
DEB(printk("main: connected; cmd = 0x%lx, sel_cmd = 0x%lx\n",
(long)hostdata->connected, (long)hostdata->sel_cmd));
}
}
} while (!done);
main_running = 0;
}
static void AM53C974_intr(int irq, void *dev_id, struct pt_regs *regs)
{
AM53C974_local_declare();
struct Scsi_Host *instance;
struct AM53C974_hostdata *hostdata;
unsigned char cmdreg, dmastatus, statreg, isreg, instreg, cfifo;
for (instance = first_instance; instance; instance = instance->next)
if ((instance->irq == irq) && (instance->hostt == the_template)) goto FOUND;
sti();
return;
FOUND:
hostdata = (struct AM53C974_hostdata *)instance->hostdata;
AM53C974_setio(instance);
dmastatus = AM53C974_read_8(DMASTATUS);
DEB_INTR(printk(SEPARATOR_LINE));
DEB_INTR(printk("AM53C974 interrupt; dmastatus=0x%02x\n", dmastatus));
KEYWAIT();
if (hostdata->connected && (dmastatus & (DMASTATUS_ERROR | DMASTATUS_PWDN |
DMASTATUS_ABORT))) {
printk("scsi%d: DMA error or powerdown; dmastatus: 0x%02x\n",
instance->host_no, dmastatus);
#ifdef AM53C974_DEBUG
deb_stop = 1;
#endif
panic("scsi%d: cannot recover\n", instance->host_no); }
if (hostdata->connected && (dmastatus & DMASTATUS_DONE)) {
unsigned long residual;
cli();
if (!(AM53C974_read_8(DMACMD) & DMACMD_DIR)) {
do {
dmastatus = AM53C974_read_8(DMASTATUS);
residual = AM53C974_read_8(CTCLREG) | (AM53C974_read_8(CTCMREG) << 8) |
(AM53C974_read_8(CTCHREG) << 16);
residual += AM53C974_read_8(CFIREG) & CFIREG_CF;
} while (!(dmastatus & DMASTATUS_SCSIINT) && residual);
residual = AM53C974_read_8(CTCLREG) | (AM53C974_read_8(CTCMREG) << 8) |
(AM53C974_read_8(CTCHREG) << 16);
residual += AM53C974_read_8(CFIREG) & CFIREG_CF;
}
else
residual = 0;
hostdata->connected->SCp.ptr += hostdata->connected->SCp.this_residual - residual;
hostdata->connected->SCp.this_residual = residual;
AM53C974_write_8(DMACMD, DMACMD_IDLE);
if (hostdata->dma_busy) {
hostdata->dma_busy = 0;
cmdreg = AM53C974_read_8(CMDREG);
statreg = AM53C974_read_8(STATREG);
isreg = AM53C974_read_8(ISREG);
instreg = AM53C974_read_8(INSTREG);
cfifo = AM53C974_cfifo();
AM53C974_information_transfer(instance, statreg, isreg, instreg, cfifo,
dmastatus); }
sti();
}
if (!(dmastatus & DMASTATUS_SCSIINT)) {
sti();
return; }
cmdreg = AM53C974_read_8(CMDREG);
statreg = AM53C974_read_8(STATREG);
isreg = AM53C974_read_8(ISREG);
instreg = AM53C974_read_8(INSTREG);
cfifo = AM53C974_cfifo();
DEB_INTR(printk("scsi%d: statreg: 0x%02x; isreg: 0x%02x; instreg: 0x%02x; cfifo: 0x%02x\n",
instance->host_no, statreg, isreg, instreg, cfifo));
if (statreg & STATREG_PE) {
#ifdef AM53C974_DEBUG
deb_stop = 1;
#endif
printk("scsi%d : PARITY error\n", instance->host_no);
if (hostdata->connected) hostdata->sync_off[hostdata->connected->target] = 0;
hostdata->aborted = 1; }
if (statreg & STATREG_IOE) {
#ifdef AM53C974_DEBUG
deb_stop = 1;
#endif
printk("scsi%d : ILLEGAL OPERATION error\n", instance->host_no);
printk("cmdreg:  0x%02x; dmacmd:  0x%02x; statreg: 0x%02x; \n"
"isreg:   0x%02x; instreg: 0x%02x; cfifo:   0x%02x\n",
cmdreg, AM53C974_read_8(DMACMD), statreg, isreg, instreg, cfifo); }
if (hostdata->in_reset && (instreg & INSTREG_SRST)) {
#ifdef AM53C974_DEBUG
deb_stop = 1;
#endif
DEB(printk("Bus reset interrupt received\n"));
AM53C974_intr_bus_reset(instance);
cli();
if (hostdata->connected) {
hostdata->connected->result = DID_RESET << 16;
hostdata->connected->scsi_done((Scsi_Cmnd *)hostdata->connected);
hostdata->connected = NULL; }
else {
if (hostdata->sel_cmd) {
hostdata->sel_cmd->result = DID_RESET << 16;
hostdata->sel_cmd->scsi_done((Scsi_Cmnd *)hostdata->sel_cmd);
hostdata->sel_cmd = NULL; }
}
sti();
if (hostdata->in_reset == 1) goto EXIT;
else return;
}
if (instreg & INSTREG_ICMD) {
#ifdef AM53C974_DEBUG
deb_stop = 1;
#endif
printk("scsi%d: Invalid command interrupt\n", instance->host_no);
printk("cmdreg:  0x%02x; dmacmd:  0x%02x; statreg: 0x%02x; dmastatus: 0x%02x; \n"
"isreg:   0x%02x; instreg: 0x%02x; cfifo:   0x%02x\n",
cmdreg, AM53C974_read_8(DMACMD), statreg, dmastatus, isreg, instreg, cfifo);
panic("scsi%d: cannot recover\n", instance->host_no); }
if (instreg & INSTREG_DIS) {
DEB_INTR(printk("Disconnect interrupt received; "));
cli();
AM53C974_intr_disconnect(instance);
sti();
goto EXIT; }
if (instreg & INSTREG_RESEL) {
DEB_INTR(printk("Reselection interrupt received\n"));
cli();
AM53C974_intr_reselect(instance, statreg);
sti();
goto EXIT; }
if (instreg & INSTREG_SO) {
DEB_INTR(printk("Successful operation interrupt received\n"));
if (hostdata->selecting) {
DEB_INTR(printk("DSR completed, starting select\n"));
cli();
AM53C974_select(instance, (Scsi_Cmnd *)hostdata->sel_cmd,
(hostdata->sel_cmd->cmnd[0] == REQUEST_SENSE) ?
TAG_NONE : TAG_NEXT);
hostdata->selecting = 0;
AM53C974_set_sync(instance, hostdata->sel_cmd->target);
sti();
return; }
if (hostdata->sel_cmd != NULL) {
if ( ((isreg & ISREG_IS) != ISREG_OK_NO_STOP) &&
((isreg & ISREG_IS) != ISREG_OK_STOP) ) {
DEB_INTR(printk("unsuccessful selection\n"));
cli();
hostdata->dma_busy = 0;
LIST(hostdata->sel_cmd, hostdata->issue_queue);
hostdata->sel_cmd->host_scribble = (unsigned char *)hostdata->issue_queue;
hostdata->issue_queue = hostdata->sel_cmd;
hostdata->sel_cmd = NULL;
hostdata->selecting = 0;
sti();
goto EXIT; }
else {
DEB(printk("successful selection; cmd=0x%02lx\n", (long)hostdata->sel_cmd));
cli();
hostdata->dma_busy = 0;
hostdata->disconnecting = 0;
hostdata->connected = hostdata->sel_cmd;
hostdata->sel_cmd = NULL;
hostdata->selecting = 0;
#ifdef SCSI2
if (!hostdata->connected->device->tagged_queue)
#endif
hostdata->busy[hostdata->connected->target] |= (1 << hostdata->connected->lun);
if ((hostdata->connected->cmnd[0] == REQUEST_SENSE) && hostdata->connected->use_sg) {
DEB(printk("scsi%d: REQUEST_SENSE command with nonzero use_sg\n", instance->host_no));
KEYWAIT();
hostdata->connected->use_sg = 0; }
initialize_SCp((Scsi_Cmnd *)hostdata->connected);
hostdata->connected->SCp.phase = PHASE_CMDOUT;
AM53C974_information_transfer(instance, statreg, isreg, instreg, cfifo, dmastatus);
sti();
return; }
}
else {
cli();
AM53C974_information_transfer(instance, statreg, isreg, instreg, cfifo, dmastatus);
sti();
return; }
}
if (instreg & INSTREG_SR) {
DEB_INTR(printk("Service request interrupt received, "));
if (hostdata->connected) {
DEB_INTR(printk("calling information_transfer\n"));
cli();
AM53C974_information_transfer(instance, statreg, isreg, instreg, cfifo, dmastatus);
sti(); }
else {
printk("scsi%d: weird: service request when no command connected\n", instance->host_no);
AM53C974_write_8(CMDREG, CMDREG_CFIFO); }
return;
}
EXIT:
DEB_INTR(printk("intr: starting main\n"));
run_main();
DEB_INTR(printk("end of intr\n"));
}
static void AM53C974_intr_disconnect(struct Scsi_Host *instance)
{
AM53C974_local_declare();
struct AM53C974_hostdata *hostdata = (struct AM53C974_hostdata *)instance->hostdata;
Scsi_Cmnd *cmd;
AM53C974_setio(instance);
if (hostdata->sel_cmd != NULL) {
cmd = (Scsi_Cmnd *)hostdata->sel_cmd;
DEB_INTR(printk("bad target\n"));
cmd->result = DID_BAD_TARGET << 16;
goto EXIT_FINISHED; }
if (!hostdata->connected) {
AM53C974_write_8(CMDREG, CMDREG_CFIFO);
return; }
if (hostdata->disconnecting) {
cmd = (Scsi_Cmnd *)hostdata->connected;
AM53C974_set_async(instance, cmd->target);
DEB_INTR(printk("scsi%d : disc. from cmnd %d for ta %d, lun %d\n",
instance->host_no, cmd->cmnd[0], cmd->target, cmd->lun));
if (cmd->device->disconnect) {
DEB_INTR(printk("ok, re-enabling selection\n"));
LIST(cmd,hostdata->disconnected_queue);
cmd->host_scribble = (unsigned char *)hostdata->disconnected_queue;
hostdata->disconnected_queue = cmd;
DEB_QUEUE(printk("scsi%d : command for target %d lun %d this %d was moved from connected to"
"  the disconnected_queue\n", instance->host_no, cmd->target,
cmd->lun, hostdata->disconnected_queue->SCp.this_residual));
DEB_QUEUE(AM53C974_print_queues(instance));
goto EXIT_UNFINISHED; }
else {
#ifdef AM53C974_DEBUG
if (cmd->cmnd[0] == REQUEST_SENSE) {
int i;
printk("Request sense data dump:\n");
for (i = 0; i < cmd->request_bufflen; i++) {
printk("%02x ", *((char *)(cmd->request_buffer) + i));
if (i && !(i % 16)) printk("\n"); }
printk("\n"); }
#endif
goto EXIT_FINISHED; }
}
cmd = (Scsi_Cmnd *)hostdata->connected;
if (cmd) {
#ifdef AM53C974_DEBUG
deb_stop = 1;
#endif
AM53C974_set_async(instance, cmd->target);
printk("scsi%d: Unexpected disconnect; phase: %d; target: %d; this_residual: %d; buffers_residual: %d; message: %d\n",
instance->host_no, cmd->SCp.phase, cmd->target, cmd->SCp.this_residual, cmd->SCp.buffers_residual,
cmd->SCp.Message);
printk("cmdreg: 0x%02x; statreg: 0x%02x; isreg: 0x%02x; cfifo: 0x%02x\n",
AM53C974_read_8(CMDREG), AM53C974_read_8(STATREG), AM53C974_read_8(ISREG),
AM53C974_read_8(CFIREG) & CFIREG_CF);
if ((hostdata->last_message[0] == EXTENDED_MESSAGE) &&
(hostdata->last_message[2] == EXTENDED_SDTR)) {
hostdata->sync_off[cmd->target] = 0; }
if (hostdata->aborted || hostdata->msgout[0] == ABORT)
cmd->result = DID_ABORT << 16;
else
cmd->result = DID_ERROR << 16;
goto EXIT_FINISHED; }
EXIT_FINISHED:
hostdata->aborted = 0;
hostdata->msgout[0] = NOP;
hostdata->sel_cmd = NULL;
hostdata->connected = NULL;
hostdata->selecting = 0;
hostdata->disconnecting = 0;
hostdata->dma_busy = 0;
hostdata->busy[cmd->target] &= ~(1 << cmd->lun);
AM53C974_write_8(CMDREG, CMDREG_CFIFO);
DEB(printk("disconnect; issue_queue: 0x%lx, disconnected_queue: 0x%lx\n",
(long)hostdata->issue_queue, (long)hostdata->disconnected_queue));
cmd->scsi_done(cmd);
if (!hostdata->selecting) {
AM53C974_set_async(instance, cmd->target);
AM53C974_write_8(CMDREG, CMDREG_ESR); }
return;
EXIT_UNFINISHED:
hostdata->msgout[0] = NOP;
hostdata->sel_cmd = NULL;
hostdata->connected = NULL;
hostdata->aborted = 0;
hostdata->selecting = 0;
hostdata->disconnecting = 0;
hostdata->dma_busy = 0;
DEB(printk("disconnect; issue_queue: 0x%lx, disconnected_queue: 0x%lx\n",
(long)hostdata->issue_queue, (long)hostdata->disconnected_queue));
if (!hostdata->selecting) {
AM53C974_set_async(instance, cmd->target);
AM53C974_write_8(CMDREG, CMDREG_ESR); }
return;
}
static int AM53C974_sync_neg(struct Scsi_Host *instance, int target, unsigned char *msg)
{
AM53C974_local_declare();
struct AM53C974_hostdata *hostdata = (struct AM53C974_hostdata *)instance->hostdata;
int period, offset, i, rate, rate_rem;
AM53C974_setio(instance);
period = (DEF_CLK * msg[3] * 8 + 1000) / 2000;
if (period < MIN_PERIOD) {
period = MIN_PERIOD;
hostdata->msgout[3] = period / 4; }
else
if (period > MAX_PERIOD) {
period = MAX_PERIOD;
hostdata->msgout[3] = period / 4; }
else
hostdata->msgout[3] = msg[3];
offset = msg[4];
if (offset > MAX_OFFSET) offset = MAX_OFFSET;
hostdata->msgout[4] = offset;
hostdata->sync_per[target] = period;
hostdata->sync_off[target] = offset;
for (i = 0; i < 3; i++) hostdata->msgout[i] = msg[i];
if ((hostdata->msgout[3] != msg[3]) || (msg[4] != offset)) return(1);
rate = DEF_CLK / period;
rate_rem = 10 * (DEF_CLK - period * rate) / period;
if (offset)
printk("\ntarget %d: rate=%d.%d Mhz, synchronous, sync offset=%d bytes\n",
target, rate, rate_rem, offset);
else
printk("\ntarget %d: rate=%d.%d Mhz, asynchronous\n", target, rate, rate_rem);
return(0);
}
static __inline__ void AM53C974_set_async(struct Scsi_Host *instance, int target)
{
AM53C974_local_declare();
struct AM53C974_hostdata *hostdata = (struct AM53C974_hostdata *)instance->hostdata;
AM53C974_setio(instance);
AM53C974_write_8(STPREG, hostdata->sync_per[target]);
AM53C974_write_8(SOFREG, (DEF_SOF_RAD<<6) | (DEF_SOF_RAA<<4));
}
static __inline__ void AM53C974_set_sync(struct Scsi_Host *instance, int target)
{
AM53C974_local_declare();
struct AM53C974_hostdata *hostdata = (struct AM53C974_hostdata *)instance->hostdata;
AM53C974_setio(instance);
AM53C974_write_8(STPREG, hostdata->sync_per[target]);
AM53C974_write_8(SOFREG, (SOFREG_SO & hostdata->sync_off[target]) |
(DEF_SOF_RAD<<6) | (DEF_SOF_RAA<<4));
}
static void AM53C974_information_transfer(struct Scsi_Host *instance,
unsigned char statreg, unsigned char isreg,
unsigned char instreg, unsigned char cfifo,
unsigned char dmastatus)
{
AM53C974_local_declare();
struct AM53C974_hostdata *hostdata = (struct AM53C974_hostdata *)instance->hostdata;
Scsi_Cmnd *cmd = (Scsi_Cmnd *)hostdata->connected;
int ret, i, len, residual=-1;
AM53C974_setio(instance);
DEB_INFO(printk(SEPARATOR_LINE));
switch (statreg & STATREG_PHASE) {
case PHASE_DATAOUT:
DEB_INFO(printk("Dataout phase; cmd=0x%lx, sel_cmd=0x%lx, this_residual=%d, buffers_residual=%d\n",
(long)hostdata->connected, (long)hostdata->sel_cmd, cmd->SCp.this_residual, cmd->SCp.buffers_residual));
cmd->SCp.phase = PHASE_DATAOUT;
goto PHASE_DATA_IO;
case PHASE_DATAIN:
DEB_INFO(printk("Datain phase; cmd=0x%lx, sel_cmd=0x%lx, this_residual=%d, buffers_residual=%d\n",
(long)hostdata->connected, (long)hostdata->sel_cmd, cmd->SCp.this_residual, cmd->SCp.buffers_residual));
cmd->SCp.phase = PHASE_DATAIN;
PHASE_DATA_IO:
if (hostdata->aborted) {
AM53C974_write_8(DMACMD, DMACMD_IDLE);
AM53C974_write_8(CMDREG, CMDREG_CFIFO);
AM53C974_write_8(CMDREG, CMDREG_SATN);
return; }
if ((!cmd->SCp.this_residual) && cmd->SCp.buffers_residual) {
cmd->SCp.buffer++;
cmd->SCp.buffers_residual--;
cmd->SCp.ptr = (unsigned char *)cmd->SCp.buffer->address;
cmd->SCp.this_residual = cmd->SCp.buffer->length; }
if (cmd->SCp.this_residual) {
if (!(AM53C974_read_8(DMACMD) & DMACMD_START)) {
hostdata->dma_busy = 0;
AM53C974_transfer_dma(instance, statreg & STATREG_IO,
(unsigned long)cmd->SCp.this_residual,
cmd->SCp.ptr); }
else
hostdata->dma_busy = 1;
}
return;
case PHASE_MSGIN:
DEB_INFO(printk("Message-In phase; cmd=0x%lx, sel_cmd=0x%lx\n",
(long)hostdata->connected, (long)hostdata->sel_cmd));
AM53C974_set_async(instance, cmd->target);
if (cmd->SCp.phase == PHASE_DATAIN)
AM53C974_dma_blast(instance, dmastatus, statreg);
if ((cmd->SCp.phase == PHASE_DATAOUT) && (AM53C974_read_8(DMACMD) & DMACMD_START)) {
AM53C974_write_8(DMACMD, DMACMD_IDLE);
residual = cfifo + (AM53C974_read_8(CTCLREG) | (AM53C974_read_8(CTCMREG) << 8) |
(AM53C974_read_8(CTCHREG) << 16));
cmd->SCp.ptr += cmd->SCp.this_residual - residual;
cmd->SCp.this_residual = residual;
if (cfifo) { AM53C974_write_8(CMDREG, CMDREG_CFIFO); cfifo = 0; }
}
if (cmd->SCp.phase == PHASE_STATIN) {
while ((AM53C974_read_8(CFIREG) & CFIREG_CF) < 2) ;
cmd->SCp.Status = AM53C974_read_8(FFREG);
cmd->SCp.Message = AM53C974_read_8(FFREG);
DEB_INFO(printk("Message-In phase; status=0x%02x, message=0x%02x\n",
cmd->SCp.Status, cmd->SCp.Message));
ret = AM53C974_message(instance, cmd, cmd->SCp.Message); }
else {
if (!cfifo) {
AM53C974_write_8(CMDREG, CMDREG_IT);
AM53C974_poll_int();
cmd->SCp.Message = AM53C974_read_8(FFREG);
}
ret = AM53C974_message(instance, cmd, cmd->SCp.Message);
}
cmd->SCp.phase = PHASE_MSGIN;
AM53C974_set_sync(instance, cmd->target);
break;
case PHASE_MSGOUT:
DEB_INFO(printk("Message-Out phase; cfifo=%d; msgout[0]=0x%02x\n",
AM53C974_read_8(CFIREG) & CFIREG_CF, hostdata->msgout[0]));
AM53C974_write_8(DMACMD, DMACMD_IDLE);
AM53C974_set_async(instance, cmd->target);
for (i = 0; i < sizeof(hostdata->last_message); i++)
hostdata->last_message[i] = hostdata->msgout[i];
if ((hostdata->msgout[0] == 0) || INSIDE(hostdata->msgout[0], 0x02, 0x1F) ||
INSIDE(hostdata->msgout[0], 0x80, 0xFF))
len = 1;
else {
if (hostdata->msgout[0] == EXTENDED_MESSAGE) {
#ifdef AM53C974_DEBUG_INFO
printk("Extended message dump:\n");
for (i = 0; i < hostdata->msgout[1] + 2; i++) {
printk("%02x ", hostdata->msgout[i]);
if (i && !(i % 16)) printk("\n"); }
printk("\n");
#endif
len = hostdata->msgout[1] + 2; }
else
len = 2;
}
for (i = 0; i < len; i++) AM53C974_write_8(FFREG, hostdata->msgout[i]);
AM53C974_write_8(CMDREG, CMDREG_IT);
cmd->SCp.phase = PHASE_MSGOUT;
hostdata->msgout[0] = NOP;
AM53C974_set_sync(instance, cmd->target);
break;
case PHASE_CMDOUT:
DEB_INFO(printk("Command-Out phase\n"));
AM53C974_set_async(instance, cmd->target);
for (i = 0; i < cmd->cmd_len; i++) AM53C974_write_8(FFREG, cmd->cmnd[i]);
AM53C974_write_8(CMDREG, CMDREG_IT);
cmd->SCp.phase = PHASE_CMDOUT;
AM53C974_set_sync(instance, cmd->target);
break;
case PHASE_STATIN:
DEB_INFO(printk("Status phase\n"));
if (cmd->SCp.phase == PHASE_DATAIN)
AM53C974_dma_blast(instance, dmastatus, statreg);
AM53C974_set_async(instance, cmd->target);
if (cmd->SCp.phase == PHASE_DATAOUT) {
unsigned long residual;
if (AM53C974_read_8(DMACMD) & DMACMD_START) {
AM53C974_write_8(DMACMD, DMACMD_IDLE);
residual = cfifo + (AM53C974_read_8(CTCLREG) | (AM53C974_read_8(CTCMREG) << 8) |
(AM53C974_read_8(CTCHREG) << 16));
cmd->SCp.ptr += cmd->SCp.this_residual - residual;
cmd->SCp.this_residual = residual; }
if (cfifo) { AM53C974_write_8(CMDREG, CMDREG_CFIFO); cfifo = 0; }
}
cmd->SCp.phase = PHASE_STATIN;
AM53C974_write_8(CMDREG, CMDREG_ICCS);
break;
case PHASE_RES_0:
case PHASE_RES_1:
#ifdef AM53C974_DEBUG
deb_stop = 1;
#endif
DEB_INFO(printk("Reserved phase\n"));
break;
}
KEYWAIT();
}
static int AM53C974_message(struct Scsi_Host *instance, Scsi_Cmnd *cmd,
unsigned char msg)
{
AM53C974_local_declare();
static unsigned char extended_msg[10];
unsigned char statreg;
int len, ret = 0;
unsigned char *p;
#ifdef AM53C974_DEBUG_MSG
int j;
#endif
struct AM53C974_hostdata *hostdata = (struct AM53C974_hostdata *)instance->hostdata;
AM53C974_setio(instance);
DEB_MSG(printk(SEPARATOR_LINE));
switch (msg) {
#ifdef LINKED
case LINKED_CMD_COMPLETE:
case LINKED_FLG_CMD_COMPLETE:
DEB_LINKED(printk("scsi%d : target %d lun %d linked command complete.\n",
instance->host_no, cmd->target, cmd->lun));
if (!cmd->next_link) {
printk("scsi%d : target %d lun %d linked command complete, no next_link\n"
instance->host_no, cmd->target, cmd->lun);
hostdata->aborted = 1;
AM53C974_write_8(CMDREG, CMDREG_SATN);
AM53C974_write_8(CMDREG, CMDREG_MA);
break; }
if (hostdata->aborted) {
DEB_ABORT(printk("ATN set for cmnd %d upon reception of LINKED_CMD_COMPLETE or"
"LINKED_FLG_CMD_COMPLETE message\n", cmd->cmnd[0]));
AM53C974_write_8(CMDREG, CMDREG_SATN); }
AM53C974_write_8(CMDREG, CMDREG_MA);
initialize_SCp(cmd->next_link);
cmd->next_link->tag = cmd->tag;
cmd->result = cmd->SCp.Status | (cmd->SCp.Message << 8);
DEB_LINKED(printk("scsi%d : target %d lun %d linked request done, calling scsi_done().\n",
instance->host_no, cmd->target, cmd->lun));
cmd->scsi_done(cmd);
cmd = hostdata->connected;
break;
#endif
case ABORT:
case COMMAND_COMPLETE:
DEB_MSG(printk("scsi%d: command complete message received; cmd %d for target %d, lun %d\n",
instance->host_no, cmd->cmnd[0], cmd->target, cmd->lun));
hostdata->disconnecting = 1;
cmd->device->disconnect = 0;
if (cmd->cmnd[0] != REQUEST_SENSE)
cmd->result = cmd->SCp.Status | (cmd->SCp.Message << 8);
else if (cmd->SCp.Status != GOOD)
cmd->result = (cmd->result & 0x00ffff) | (DID_ERROR << 16);
if (hostdata->aborted) {
AM53C974_write_8(CMDREG, CMDREG_SATN);
AM53C974_write_8(CMDREG, CMDREG_MA);
DEB_ABORT(printk("ATN set for cmnd %d upon reception of ABORT or"
"COMMAND_COMPLETE message\n", cmd->cmnd[0]));
break; }
if ((cmd->cmnd[0] != REQUEST_SENSE) && (cmd->SCp.Status == CHECK_CONDITION)) {
DEB_MSG(printk("scsi%d : performing request sense\n", instance->host_no));
cmd->cmnd[0] = REQUEST_SENSE;
cmd->cmnd[1] &= 0xe0;
cmd->cmnd[2] = 0;
cmd->cmnd[3] = 0;
cmd->cmnd[4] = sizeof(cmd->sense_buffer);
cmd->cmnd[5] = 0;
cmd->SCp.buffer = NULL;
cmd->SCp.buffers_residual = 0;
cmd->SCp.ptr = (char *)cmd->sense_buffer;
cmd->SCp.this_residual = sizeof(cmd->sense_buffer);
LIST(cmd,hostdata->issue_queue);
cmd->host_scribble = (unsigned char *)hostdata->issue_queue;
hostdata->issue_queue = (Scsi_Cmnd *)cmd;
DEB_MSG(printk("scsi%d : REQUEST SENSE added to head of issue queue\n",instance->host_no));
}
AM53C974_write_8(CMDREG, CMDREG_MA);
break;
case MESSAGE_REJECT:
DEB_MSG(printk("scsi%d: reject message received; cmd %d for target %d, lun %d\n",
instance->host_no, cmd->cmnd[0], cmd->target, cmd->lun));
switch (hostdata->last_message[0]) {
case EXTENDED_MESSAGE:
if (hostdata->last_message[2] == EXTENDED_SDTR) {
printk("\ntarget %d: rate=%d Mhz, asynchronous (sync. negotiation rejected)\n",
cmd->target, DEF_CLK / DEF_STP);
hostdata->sync_off[cmd->target] = 0;
hostdata->sync_per[cmd->target] = DEF_STP; }
break;
case HEAD_OF_QUEUE_TAG:
case ORDERED_QUEUE_TAG:
case SIMPLE_QUEUE_TAG:
cmd->device->tagged_queue = 0;
hostdata->busy[cmd->target] |= (1 << cmd->lun);
break;
default:
break;
}
if (hostdata->aborted) AM53C974_write_8(CMDREG, CMDREG_SATN);
AM53C974_write_8(CMDREG, CMDREG_MA);
break;
case DISCONNECT:
DEB_MSG(printk("scsi%d: disconnect message received; cmd %d for target %d, lun %d\n",
instance->host_no, cmd->cmnd[0], cmd->target, cmd->lun));
cmd->device->disconnect = 1;
hostdata->disconnecting = 1;
AM53C974_write_8(CMDREG, CMDREG_MA);
break;
case SAVE_POINTERS:
case RESTORE_POINTERS:
DEB_MSG(printk("scsi%d: save/restore pointers message received; cmd %d for target %d, lun %d\n",
instance->host_no, cmd->cmnd[0], cmd->target, cmd->lun));
if (hostdata->aborted) {
DEB_ABORT(printk("ATN set for cmnd %d upon reception of SAVE/REST. POINTERS message\n",
cmd->cmnd[0]));
AM53C974_write_8(CMDREG, CMDREG_SATN); }
AM53C974_write_8(CMDREG, CMDREG_MA);
break;
case EXTENDED_MESSAGE:
DEB_MSG(printk("scsi%d: extended message received; cmd %d for target %d, lun %d\n",
instance->host_no, cmd->cmnd[0], cmd->target, cmd->lun));
extended_msg[0] = EXTENDED_MESSAGE;
AM53C974_read_8(INSTREG) ;
AM53C974_write_8(CMDREG, CMDREG_MA);
AM53C974_poll_int();
AM53C974_write_8(CMDREG, CMDREG_IT);
AM53C974_poll_int();
AM53C974_write_8(CMDREG, CMDREG_MA);
AM53C974_poll_int();
extended_msg[1] = len = AM53C974_read_8(FFREG);
p = extended_msg+2;
while (len) {
AM53C974_write_8(CMDREG, CMDREG_IT);
AM53C974_poll_int();
if (len > 1) {
AM53C974_write_8(CMDREG, CMDREG_MA);
AM53C974_poll_int(); }
*p = AM53C974_read_8(FFREG);
p++; len--; }
#ifdef AM53C974_DEBUG_MSG
printk("scsi%d: received extended message: ", instance->host_no);
for (j = 0; j < extended_msg[1] + 2; j++) {
printk("0x%02x ", extended_msg[j]);
if (j && !(j % 16)) printk("\n"); }
printk("\n");
#endif
if (extended_msg[2] == EXTENDED_SDTR)
ret = AM53C974_sync_neg(instance, cmd->target, extended_msg);
if (ret || hostdata->aborted) AM53C974_write_8(CMDREG, CMDREG_SATN);
AM53C974_write_8(CMDREG, CMDREG_MA);
break;
default:
printk("scsi%d: unknown message 0x%02x received\n",instance->host_no, msg);
#ifdef AM53C974_DEBUG
deb_stop = 1;
#endif
hostdata->msgout[0] = MESSAGE_REJECT;
AM53C974_write_8(CMDREG, CMDREG_SATN);
AM53C974_write_8(CMDREG, CMDREG_MA);
return(0);
break;
}
KEYWAIT();
return(1);
}
static void AM53C974_select(struct Scsi_Host *instance, Scsi_Cmnd *cmd, int tag)
{
AM53C974_local_declare();
struct AM53C974_hostdata *hostdata = (struct AM53C974_hostdata *)instance->hostdata;
unsigned char cfifo, tmp[3];
unsigned int i, len, cmd_size = COMMAND_SIZE(cmd->cmnd[0]);
AM53C974_setio(instance);
cfifo = AM53C974_cfifo();
if (cfifo) {
printk("scsi%d: select error; %d residual bytes in FIFO\n", instance->host_no, cfifo);
AM53C974_write_8(CMDREG, CMDREG_CFIFO);
}
tmp[0] = IDENTIFY(1, cmd->lun);
#ifdef SCSI2
if (cmd->device->tagged_queue && (tag != TAG_NONE)) {
tmp[1] = SIMPLE_QUEUE_TAG;
if (tag == TAG_NEXT) {
if (cmd->device->current_tag == 0) cmd->device->current_tag = 1;
cmd->tag = cmd->device->current_tag;
cmd->device->current_tag++; }
else
cmd->tag = (unsigned char)tag;
tmp[2] = cmd->tag;
hostdata->last_message[0] = SIMPLE_QUEUE_TAG;
len = 3;
AM53C974_write_8(FFREG, tmp[0]);
AM53C974_write_8(FFREG, tmp[1]);
AM53C974_write_8(FFREG, tmp[2]);
}
else
#endif
{
len = 1;
AM53C974_write_8(FFREG, tmp[0]);
cmd->tag = 0; }
if (((cmd->cmnd[0] == INQUIRY) || (cmd->cmnd[0] == REQUEST_SENSE)) &&
!(hostdata->sync_neg[cmd->target]) && hostdata->sync_en[cmd->target]) {
hostdata->sync_neg[cmd->target] = 1;
hostdata->msgout[0] = EXTENDED_MESSAGE;
hostdata->msgout[1] = 3;
hostdata->msgout[2] = EXTENDED_SDTR;
hostdata->msgout[3] = 250 / (int)hostdata->max_rate[cmd->target];
hostdata->msgout[4] = hostdata->max_offset[cmd->target];
len += 5; }
AM53C974_write_8(SDIDREG, SDIREG_MASK & cmd->target);
AM53C974_write_8(STIMREG, DEF_SCSI_TIMEOUT);
switch (len) {
case 1:
for (i = 0; i < cmd_size; i++) AM53C974_write_8(FFREG, cmd->cmnd[i]);
AM53C974_write_8(CMDREG, CMDREG_SAS);
hostdata->msgout[0] = NOP;
break;
case 3:
for (i = 0; i < cmd_size; i++) AM53C974_write_8(FFREG, cmd->cmnd[i]);
AM53C974_write_8(CMDREG, CMDREG_SA3S);
hostdata->msgout[0] = NOP;
break;
default:
AM53C974_write_8(CMDREG, CMDREG_SASS);
break;
}
}
static void AM53C974_intr_reselect(struct Scsi_Host *instance, unsigned char statreg)
{
AM53C974_local_declare();
struct AM53C974_hostdata *hostdata = (struct AM53C974_hostdata *)instance->hostdata;
unsigned char cfifo, msg[3], lun, t, target = 0;
#ifdef SCSI2
unsigned char tag;
#endif
Scsi_Cmnd *tmp = NULL, *prev;
AM53C974_setio(instance);
cfifo = AM53C974_cfifo();
if (hostdata->selecting) {
DEB_RESEL(printk("AM53C974_intr_reselect: in selection process\n"));
LIST(hostdata->sel_cmd, hostdata->issue_queue);
hostdata->sel_cmd->host_scribble = (unsigned char *)hostdata->issue_queue;
hostdata->issue_queue = hostdata->sel_cmd;
hostdata->sel_cmd = NULL;
hostdata->selecting = 0; }
if (cfifo != 2) {
printk("scsi %d: error: %d bytes in fifo, 2 expected\n", instance->host_no, cfifo);
hostdata->aborted = 1;
goto EXIT_ABORT; }
t = AM53C974_read_8(FFREG);
if (!(t & (1 << instance->this_id))) {
printk("scsi %d: error: invalid host id\n", instance->host_no);
hostdata->aborted = 1;
goto EXIT_ABORT; }
t ^= (1 << instance->this_id);
target = 0; while (t != 1) { t >>= 1; target++; }
DEB_RESEL(printk("scsi %d: reselect; target: %d\n", instance->host_no, target));
if (hostdata->aborted) goto EXIT_ABORT;
if ((statreg & STATREG_PHASE) != PHASE_MSGIN) {
printk("scsi %d: error: upon reselection interrupt not in MSGIN\n", instance->host_no);
hostdata->aborted = 1;
goto EXIT_ABORT; }
msg[0] = AM53C974_read_8(FFREG);
if (!(msg[0] & 0x80)) {
printk("scsi%d: error: expecting IDENTIFY message, got ", instance->host_no);
print_msg(msg);
hostdata->aborted = 1;
goto EXIT_ABORT; }
lun = (msg[0] & 0x07);
#ifdef SCSI2
#error "SCSI-II tagged queueing is not supported yet"
#endif
for (tmp = (Scsi_Cmnd *)hostdata->disconnected_queue, prev = NULL;
tmp; prev = tmp, tmp = (Scsi_Cmnd *)tmp->host_scribble)
if ((target == tmp->target) && (lun == tmp->lun)
#ifdef SCSI2
&& (tag == tmp->tag)
#endif
) {
if (prev) {
REMOVE(prev, (Scsi_Cmnd *)(prev->host_scribble), tmp,
(Scsi_Cmnd *)(tmp->host_scribble));
prev->host_scribble = tmp->host_scribble; }
else {
REMOVE(-1, hostdata->disconnected_queue, tmp, tmp->host_scribble);
hostdata->disconnected_queue = (Scsi_Cmnd *)tmp->host_scribble; }
tmp->host_scribble = NULL;
hostdata->connected = tmp;
break; }
if (!tmp) {
#ifdef SCSI2
printk("scsi%d: warning : target %d lun %d tag %d not in disconnect_queue.\n",
instance->host_no, target, lun, tag);
#else
printk("scsi%d: warning : target %d lun %d not in disconnect_queue.\n",
instance->host_no, target, lun);
#endif
hostdata->aborted = 1;
DEB(AM53C974_keywait());
goto EXIT_ABORT; }
else
goto EXIT_OK;
EXIT_ABORT:
AM53C974_write_8(CMDREG, CMDREG_SATN);
AM53C974_write_8(CMDREG, CMDREG_MA);
return;
EXIT_OK:
DEB_RESEL(printk("scsi%d: nexus established, target = %d, lun = %d, tag = %d\n",
instance->host_no, target, tmp->lun, tmp->tag));
AM53C974_set_sync(instance, target);
AM53C974_write_8(SDIDREG, SDIREG_MASK & target);
AM53C974_write_8(CMDREG, CMDREG_MA);
hostdata->dma_busy = 0;
hostdata->connected->SCp.phase = PHASE_CMDOUT;
}
static __inline__ void AM53C974_transfer_dma(struct Scsi_Host *instance, short dir,
unsigned long length, char *data)
{
AM53C974_local_declare();
AM53C974_setio(instance);
AM53C974_write_8(CMDREG, CMDREG_NOP);
AM53C974_write_8(DMACMD, (dir << 7) | DMACMD_INTE_D);
AM53C974_write_8(STCLREG, (unsigned char)(length & 0xff));
AM53C974_write_8(STCMREG, (unsigned char)((length & 0xff00) >> 8));
AM53C974_write_8(STCHREG, (unsigned char)((length & 0xff0000) >> 16));
AM53C974_write_32(DMASTC, length & 0xffffff);
AM53C974_write_32(DMASPA, virt_to_bus(data));
AM53C974_write_8(CMDREG, CMDREG_IT | CMDREG_DMA);
AM53C974_write_8(DMACMD, (dir << 7) | DMACMD_INTE_D | DMACMD_START);
}
static void AM53C974_dma_blast(struct Scsi_Host *instance, unsigned char dmastatus,
unsigned char statreg)
{
AM53C974_local_declare();
struct AM53C974_hostdata *hostdata = (struct AM53C974_hostdata *)instance->hostdata;
unsigned long ctcreg;
int dir = statreg & STATREG_IO;
int cfifo, pio, i = 0;
AM53C974_setio(instance);
do {
cfifo = AM53C974_cfifo();
i++;
} while (cfifo && (i < 50000));
pio = (i == 50000) ? 1: 0;
if (statreg & STATREG_CTZ) { AM53C974_write_8(DMACMD, DMACMD_IDLE); return; }
if (dmastatus & DMASTATUS_DONE) { AM53C974_write_8(DMACMD, DMACMD_IDLE); return; }
AM53C974_write_8(DMACMD, ((dir << 7) & DMACMD_DIR) | DMACMD_BLAST);
while(!(AM53C974_read_8(DMASTATUS) & DMASTATUS_BCMPLT)) ;
AM53C974_write_8(DMACMD, DMACMD_IDLE);
if (pio) {
unsigned char *wac = (unsigned char *)AM53C974_read_32(DMAWAC);
printk("pio mode, residual=%d\n", AM53C974_read_8(CFIREG) & CFIREG_CF);
while (AM53C974_read_8(CFIREG) & CFIREG_CF) *(wac++) = AM53C974_read_8(FFREG);
}
ctcreg = AM53C974_read_8(CTCLREG) | (AM53C974_read_8(CTCMREG) << 8) |
(AM53C974_read_8(CTCHREG) << 16);
hostdata->connected->SCp.ptr += hostdata->connected->SCp.this_residual - ctcreg;
hostdata->connected->SCp.this_residual = ctcreg;
}
static void AM53C974_intr_bus_reset(struct Scsi_Host *instance)
{
AM53C974_local_declare();
unsigned char cntlreg1;
AM53C974_setio(instance);
AM53C974_write_8(CMDREG, CMDREG_CFIFO);
AM53C974_write_8(CMDREG, CMDREG_NOP);
cntlreg1 = AM53C974_read_8(CNTLREG1);
AM53C974_write_8(CNTLREG1, cntlreg1 | CNTLREG1_DISR);
}
int AM53C974_abort(Scsi_Cmnd *cmd)
{
AM53C974_local_declare();
struct Scsi_Host *instance = cmd->host;
struct AM53C974_hostdata *hostdata = (struct AM53C974_hostdata *)instance->hostdata;
Scsi_Cmnd *tmp, **prev;
#ifdef AM53C974_DEBUG
deb_stop = 1;
#endif
cli();
AM53C974_setio(instance);
DEB_ABORT(printk(SEPARATOR_LINE));
DEB_ABORT(printk("scsi%d : AM53C974_abort called -- trouble starts!!\n", instance->host_no));
DEB_ABORT(AM53C974_print(instance));
DEB_ABORT(AM53C974_keywait());
if ((hostdata->connected == cmd) || (hostdata->sel_cmd == cmd)) {
DEB_ABORT(printk("scsi%d: aborting connected command\n", instance->host_no));
hostdata->aborted = 1;
hostdata->msgout[0] = ABORT;
sti();
return(SCSI_ABORT_PENDING); }
for (prev = (Scsi_Cmnd **)&(hostdata->issue_queue),
tmp = (Scsi_Cmnd *)hostdata->issue_queue; tmp;
prev = (Scsi_Cmnd **)&(tmp->host_scribble),
tmp = (Scsi_Cmnd *)tmp->host_scribble) {
if (cmd == tmp) {
DEB_ABORT(printk("scsi%d : abort removed command from issue queue.\n", instance->host_no));
REMOVE(5, *prev, tmp, tmp->host_scribble);
(*prev) = (Scsi_Cmnd *)tmp->host_scribble;
tmp->host_scribble = NULL;
tmp->result = DID_ABORT << 16;
sti();
tmp->done(tmp);
return(SCSI_ABORT_SUCCESS); }
#ifdef AM53C974_DEBUG_ABORT
else {
if (prev == (Scsi_Cmnd **)tmp)
printk("scsi%d : LOOP\n", instance->host_no);
}
#endif
}
if (hostdata->connected || hostdata->sel_cmd) {
DEB_ABORT(printk("scsi%d : abort failed, other command connected.\n", instance->host_no));
sti();
return(SCSI_ABORT_NOT_RUNNING); }
for (tmp = (Scsi_Cmnd *)hostdata->disconnected_queue; tmp;
tmp = (Scsi_Cmnd *)tmp->host_scribble) {
if (cmd == tmp) {
DEB_ABORT(printk("scsi%d: aborting disconnected command\n", instance->host_no));
hostdata->aborted = 1;
hostdata->msgout[0] = ABORT;
hostdata->selecting = 1;
hostdata->sel_cmd = tmp;
AM53C974_write_8(CMDREG, CMDREG_DSR);
sti();
return(SCSI_ABORT_PENDING); }
}
DEB_ABORT(printk("scsi%d : abort failed, command not found.\n", instance->host_no));
sti();
return(SCSI_ABORT_NOT_RUNNING);
}
int AM53C974_reset(Scsi_Cmnd *cmd, unsigned int flags)
{
AM53C974_local_declare();
int i;
struct Scsi_Host *instance = cmd->host;
struct AM53C974_hostdata *hostdata = (struct AM53C974_hostdata *)instance->hostdata;
AM53C974_setio(instance);
cli();
DEB(printk("AM53C974_reset called; "));
printk("AM53C974_reset called\n");
AM53C974_print(instance);
AM53C974_keywait();
AM53C974_write_8(CMDREG, CMDREG_RDEV);
AM53C974_write_8(CMDREG, CMDREG_NOP);
hostdata->msgout[0] = NOP;
for (i = 0; i < 8; i++) {
hostdata->busy[i] = 0;
hostdata->sync_per[i] = DEF_STP;
hostdata->sync_off[i] = 0;
hostdata->sync_neg[i] = 0; }
hostdata->last_message[0] = NOP;
hostdata->sel_cmd = NULL;
hostdata->connected = NULL;
hostdata->issue_queue = NULL;
hostdata->disconnected_queue = NULL;
hostdata->in_reset = 0;
hostdata->aborted = 0;
hostdata->selecting = 0;
hostdata->disconnecting = 0;
hostdata->dma_busy = 0;
AM53C974_write_8(CNTLREG1, CNTLREG1_DISR | instance->this_id);
AM53C974_write_8(CMDREG, CMDREG_RBUS);
udelay(40);
AM53C974_config_after_reset(instance);
sti();
cmd->result = DID_RESET << 16;
cmd->scsi_done(cmd);
return SCSI_ABORT_SUCCESS;
}
int
AM53C974_release(struct Scsi_Host *shp)
{
free_irq(shp->irq, NULL);
scsi_unregister(shp);
return 0;
}
#ifdef MODULE
Scsi_Host_Template driver_template = AM53C974;
#include "scsi_module.c"
#endif