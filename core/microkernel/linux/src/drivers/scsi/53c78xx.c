#include <linux/config.h>
#ifdef CONFIG_SCSI_NCR53C7xx_sync
#ifdef CONFIG_SCSI_NCR53C7xx_DISCONNECT
#define PERM_OPTIONS (OPTION_IO_MAPPED|OPTION_DEBUG_TEST1|OPTION_DISCONNECT|\
OPTION_SYNCHRONOUS|OPTION_ALWAYS_SYNCHRONOUS)
#else
#define PERM_OPTIONS (OPTION_IO_MAPPED|OPTION_DEBUG_TEST1|\
OPTION_SYNCHRONOUS|OPTION_ALWAYS_SYNCHRONOUS)
#endif
#else
#ifdef CONFIG_SCSI_NCR53C7xx_DISCONNECT
#define PERM_OPTIONS (OPTION_IO_MAPPED|OPTION_DEBUG_TEST1|OPTION_DISCONNECT|\
OPTION_SYNCHRONOUS)
#else
#define PERM_OPTIONS (OPTION_IO_MAPPED|OPTION_DEBUG_TEST1|OPTION_SYNCHRONOUS)
#endif
#endif
#if !defined(LINUX_1_2) && !defined(LINUX_1_3)
#include <linux/version.h>
#if LINUX_VERSION_CODE > 65536 + 3 * 256
#define LINUX_1_3
#else
#define LINUX_1_2
#endif
#endif
#ifdef LINUX_1_2
#define u32 bogus_u32
#define s32 bogus_s32
#include <asm/types.h>
#undef u32
#undef s32
typedef __signed__ int s32;
typedef unsigned int u32;
#endif
#ifdef MODULE
#include <linux/module.h>
#endif
#include <asm/dma.h>
#include <asm/io.h>
#include <asm/system.h>
#include <linux/delay.h>
#include <linux/signal.h>
#include <linux/sched.h>
#include <linux/errno.h>
#include <linux/bios32.h>
#include <linux/pci.h>
#include <linux/proc_fs.h>
#include <linux/string.h>
#include <linux/malloc.h>
#include <linux/mm.h>
#include <linux/ioport.h>
#include <linux/time.h>
#ifdef LINUX_1_2
#include "../block/blk.h"
#else
#include <linux/blk.h>
#endif
#undef current
#include "scsi.h"
#include "hosts.h"
#include "53c7,8xx.h"
#include "constants.h"
#include "sd.h"
#include <linux/stat.h>
#include <linux/stddef.h>
#ifndef LINUX_1_2
struct proc_dir_entry proc_scsi_ncr53c7xx = {
PROC_SCSI_NCR53C7xx, 9, "ncr53c7xx",
S_IFDIR | S_IRUGO | S_IXUGO, 2
};
#endif
static int check_address (unsigned long addr, int size);
static void dump_events (struct Scsi_Host *host, int count);
static Scsi_Cmnd * return_outstanding_commands (struct Scsi_Host *host,
int free, int issue);
static void hard_reset (struct Scsi_Host *host);
static void ncr_scsi_reset (struct Scsi_Host *host);
static void print_lots (struct Scsi_Host *host);
static void set_synchronous (struct Scsi_Host *host, int target, int sxfer,
int scntl3, int now_connected);
static int datapath_residual (struct Scsi_Host *host);
static const char * sbcl_to_phase (int sbcl);
static void print_progress (Scsi_Cmnd *cmd);
static void print_queues (struct Scsi_Host *host);
static void process_issue_queue (unsigned long flags);
static int shutdown (struct Scsi_Host *host);
static void abnormal_finished (struct NCR53c7x0_cmd *cmd, int result);
static int disable (struct Scsi_Host *host);
static int NCR53c8xx_run_tests (struct Scsi_Host *host);
static int NCR53c8xx_script_len;
static int NCR53c8xx_dsa_len;
static void NCR53c7x0_intr(int irq, void *dev_id, struct pt_regs * regs);
static int ncr_halt (struct Scsi_Host *host);
static void intr_phase_mismatch (struct Scsi_Host *host, struct NCR53c7x0_cmd
*cmd);
static void intr_dma (struct Scsi_Host *host, struct NCR53c7x0_cmd *cmd);
static void print_dsa (struct Scsi_Host *host, u32 *dsa,
const char *prefix);
static int print_insn (struct Scsi_Host *host, const u32 *insn,
const char *prefix, int kernel);
static void NCR53c8xx_dsa_fixup (struct NCR53c7x0_cmd *cmd);
static void NCR53c8x0_init_fixup (struct Scsi_Host *host);
static int NCR53c8x0_dstat_sir_intr (struct Scsi_Host *host, struct
NCR53c7x0_cmd *cmd);
static void NCR53c8x0_soft_reset (struct Scsi_Host *host);
static long long perm_options = PERM_OPTIONS;
static int selection_timeout = 14;
static int track_events = 0;
static struct Scsi_Host *first_host = NULL;
static Scsi_Host_Template *the_template = NULL;
static struct {
unsigned short pci_device_id;
int chip;
int max_revision;
int min_revision;
} pci_chip_ids[] = {
{PCI_DEVICE_ID_NCR_53C810, 810, 2, 1},
{PCI_DEVICE_ID_NCR_53C815, 815, 3, 2},
{PCI_DEVICE_ID_NCR_53C820, 820, -1, -1},
{PCI_DEVICE_ID_NCR_53C825, 825, -1, -1}
};
#define NPCI_CHIP_IDS (sizeof (pci_chip_ids) / sizeof(pci_chip_ids[0]))
#define ROUNDUP(adr,type) \
((void *) (((long) (adr) + sizeof(type) - 1) & ~(sizeof(type) - 1)))
static struct override {
int chip;
int board;
unsigned pci:1;
union {
struct {
int base;
int io_port;
int irq;
int dma;
} normal;
struct {
int bus;
int device;
int function;
} pci;
} data;
long long options;
} overrides [4] = {{0,},};
static int commandline_current = 0;
static int no_overrides = 0;
#if 0
#define OVERRIDE_LIMIT (sizeof(overrides) / sizeof(struct override))
#else
#define OVERRIDE_LIMIT commandline_current
#endif
static inline struct NCR53c7x0_cmd *
issue_to_cmd (struct Scsi_Host *host, struct NCR53c7x0_hostdata *hostdata,
u32 *issue)
{
return (issue[0] != hostdata->NOP_insn) ?
(struct NCR53c7x0_cmd *) ((char *) bus_to_virt (issue[1]) -
(hostdata->E_dsa_code_begin - hostdata->E_dsa_code_template) -
offsetof(struct NCR53c7x0_cmd, dsa))
: NULL;
}
static void
internal_setup(int board, int chip, char *str, int *ints) {
unsigned char pci;
pci = (str && !strcmp (str, "pci")) ? 1 : 0;
if (commandline_current < OVERRIDE_LIMIT) {
overrides[commandline_current].pci = pci ? 1 : 0;
if (!pci) {
overrides[commandline_current].data.normal.base = ints[1];
overrides[commandline_current].data.normal.io_port = ints[2];
overrides[commandline_current].data.normal.irq = ints[3];
overrides[commandline_current].data.normal.dma = (ints[0] >= 4) ?
ints[4] : DMA_NONE;
overrides[commandline_current].options = (ints[0] >= 5) ?
ints[5] : 0;
} else {
overrides[commandline_current].data.pci.bus = ints[1];
overrides[commandline_current].data.pci.device = ints[2];
overrides[commandline_current].data.pci.function = ints[3];
overrides[commandline_current].options = (ints[0] >= 4) ?
ints[4] : 0;
}
overrides[commandline_current].board = board;
overrides[commandline_current].chip = chip;
++commandline_current;
++no_overrides;
} else {
printk ("53c7,7x0.c:internal_setup() : too many overrides\n");
}
}
#define setup_wrapper(x) \
void ncr53c##x##_setup (char *str, int *ints) { \
internal_setup (BOARD_GENERIC, x, str, ints); \
}
setup_wrapper(700)
setup_wrapper(70066)
setup_wrapper(710)
setup_wrapper(720)
setup_wrapper(810)
setup_wrapper(815)
setup_wrapper(820)
setup_wrapper(825)
static const unsigned char sdtr_message[] = {
#ifdef CONFIG_SCSI_NCR53C7xx_FAST
EXTENDED_MESSAGE, 3 , EXTENDED_SDTR, 25 , 8
#else
EXTENDED_MESSAGE, 3 , EXTENDED_SDTR, 50 , 8
#endif
};
static const unsigned char async_message[] = {
EXTENDED_MESSAGE, 3 , EXTENDED_SDTR, 0, 0
};
static const unsigned char wdtr_message[] = {
EXTENDED_MESSAGE, 2 , EXTENDED_WDTR, 1
};
static struct Scsi_Host *
find_host (int host) {
struct Scsi_Host *h;
for (h = first_host; h && h->host_no != host; h = h->next);
if (!h) {
printk (KERN_ALERT "scsi%d not found\n", host);
return NULL;
} else if (h->hostt != the_template) {
printk (KERN_ALERT "scsi%d is not a NCR board\n", host);
return NULL;
}
return h;
}
static int
request_synchronous (int host, int target) {
struct Scsi_Host *h;
struct NCR53c7x0_hostdata *hostdata;
unsigned long flags;
if (target < 0) {
printk (KERN_ALERT "target %d is bogus\n", target);
return -1;
}
if (!(h = find_host (host)))
return -1;
else if (h->this_id == target) {
printk (KERN_ALERT "target %d is host ID\n", target);
return -1;
}
#ifndef LINUX_1_2
else if (target > h->max_id) {
printk (KERN_ALERT "target %d exceeds maximum of %d\n", target,
h->max_id);
return -1;
}
#endif
hostdata = (struct NCR53c7x0_hostdata *)h->hostdata;
save_flags(flags);
cli();
if (hostdata->initiate_sdtr & (1 << target)) {
restore_flags(flags);
printk (KERN_ALERT "target %d already doing SDTR\n", target);
return -1;
}
hostdata->initiate_sdtr |= (1 << target);
restore_flags(flags);
return 0;
}
static int
request_disconnect (int host, int on_or_off) {
struct Scsi_Host *h;
struct NCR53c7x0_hostdata *hostdata;
if (!(h = find_host (host)))
return -1;
hostdata = (struct NCR53c7x0_hostdata *) h->hostdata;
if (on_or_off)
hostdata->options |= OPTION_DISCONNECT;
else
hostdata->options &= ~OPTION_DISCONNECT;
return 0;
}
static void
NCR53c7x0_driver_init (struct Scsi_Host *host) {
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
int i, j;
u32 *current;
for (i = 0; i < 16; ++i) {
hostdata->request_sense[i] = 0;
for (j = 0; j < 8; ++j)
hostdata->busy[i][j] = 0;
set_synchronous (host, i, 0, hostdata->saved_scntl3, 0);
}
hostdata->issue_queue = NULL;
hostdata->running_list = hostdata->finished_queue =
hostdata->current = NULL;
for (i = 0, current = (u32 *) hostdata->schedule;
i < host->can_queue; ++i, current += 2) {
current[0] = hostdata->NOP_insn;
current[1] = 0xdeadbeef;
}
current[0] = ((DCMD_TYPE_TCI|DCMD_TCI_OP_JUMP) << 24) | DBC_TCI_TRUE;
current[1] = (u32) virt_to_bus (hostdata->script) +
hostdata->E_wait_reselect;
hostdata->reconnect_dsa_head = 0;
hostdata->addr_reconnect_dsa_head = (u32)
virt_to_bus((void *) &(hostdata->reconnect_dsa_head));
hostdata->expecting_iid = 0;
hostdata->expecting_sto = 0;
if (hostdata->options & OPTION_ALWAYS_SYNCHRONOUS)
hostdata->initiate_sdtr = 0xffff;
else
hostdata->initiate_sdtr = 0;
hostdata->talked_to = 0;
hostdata->idle = 1;
}
static int
ccf_to_clock (int ccf) {
switch (ccf) {
case 1: return 25000000;
case 2: return 37500000;
case 3: return 50000000;
case 0:
case 4: return 66000000;
default: return -1;
}
}
static int
clock_to_ccf (int clock) {
if (clock < 16666666)
return -1;
if (clock < 25000000)
return 1;
else if (clock < 37500000)
return 2;
else if (clock < 50000000)
return 3;
else if (clock < 66000000)
return 4;
else
return -1;
}
static int
NCR53c7x0_init (struct Scsi_Host *host) {
NCR53c7x0_local_declare();
int i, ccf, expected_ccf;
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
struct Scsi_Host *search;
int expected_id = -1;
int expected_clock = -1;
int uninitialized = 0;
int expected_mapping = OPTION_IO_MAPPED;
NCR53c7x0_local_setup(host);
switch (hostdata->chip) {
case 820:
case 825:
#ifdef notyet
host->max_id = 15;
#endif
case 810:
case 815:
hostdata->dstat_sir_intr = NCR53c8x0_dstat_sir_intr;
hostdata->init_save_regs = NULL;
hostdata->dsa_fixup = NCR53c8xx_dsa_fixup;
hostdata->init_fixup = NCR53c8x0_init_fixup;
hostdata->soft_reset = NCR53c8x0_soft_reset;
hostdata->run_tests = NCR53c8xx_run_tests;
expected_clock = hostdata->scsi_clock = 40000000;
expected_id = 7;
break;
default:
printk ("scsi%d : chip type of %d is not supported yet, detaching.\n",
host->host_no, hostdata->chip);
scsi_unregister (host);
return -1;
}
hostdata->NCR53c7xx_zero = 0;
hostdata->NCR53c7xx_msg_reject = MESSAGE_REJECT;
hostdata->NCR53c7xx_msg_abort = ABORT;
hostdata->NCR53c7xx_msg_nop = NOP;
hostdata->NOP_insn = (DCMD_TYPE_TCI|DCMD_TCI_OP_JUMP) << 24;
if (expected_mapping == -1 ||
(hostdata->options & (OPTION_MEMORY_MAPPED)) !=
(expected_mapping & OPTION_MEMORY_MAPPED))
printk ("scsi%d : using %s mapped access\n", host->host_no,
(hostdata->options & OPTION_MEMORY_MAPPED) ? "memory" :
"io");
hostdata->dmode = (hostdata->chip == 700 || hostdata->chip == 70066) ?
DMODE_REG_00 : DMODE_REG_10;
hostdata->istat = ((hostdata->chip / 100) == 8) ?
ISTAT_REG_800 : ISTAT_REG_700;
ncr_halt(host);
#if 0
tmp = hostdata->this_id_mask = NCR53c7x0_read8(SCID_REG);
for (host->this_id = 0; tmp != 1; tmp >>=1, ++host->this_id);
#else
host->this_id = NCR53c7x0_read8(SCID_REG) & 15;
if (host->this_id == 0)
host->this_id = 7;
hostdata->this_id_mask = 1 << host->this_id;
#endif
if (!host->this_id) {
printk("scsi%d : initiator ID was %d, changing to 7\n",
host->host_no, host->this_id);
host->this_id = 7;
hostdata->this_id_mask = 1 << 7;
uninitialized = 1;
};
if (expected_id == -1 || host->this_id != expected_id)
printk("scsi%d : using initiator ID %d\n", host->host_no,
host->this_id);
if ((hostdata->chip / 100) == 8) {
hostdata->saved_ctest4 = NCR53c7x0_read8(CTEST4_REG_800) &
CTEST4_800_SAVE;
} else {
hostdata->saved_ctest7 = NCR53c7x0_read8(CTEST7_REG) & CTEST7_SAVE;
}
hostdata->saved_dcntl = NCR53c7x0_read8(DCNTL_REG);
if ((hostdata->chip / 100) == 8)
hostdata->saved_dcntl &= ~DCNTL_800_IRQM;
hostdata->saved_dmode = NCR53c7x0_read8(hostdata->dmode);
if ((hostdata->chip / 100) == 8) {
if (hostdata->saved_ctest4 & CTEST4_800_BDIS) {
printk ("scsi%d : burst mode disabled\n", host->host_no);
} else {
switch (hostdata->saved_dmode & DMODE_BL_MASK) {
case DMODE_BL_2: i = 2; break;
case DMODE_BL_4: i = 4; break;
case DMODE_BL_8: i = 8; break;
case DMODE_BL_16: i = 16; break;
default: i = 0;
}
printk ("scsi%d : burst length %d\n", host->host_no, i);
}
}
if (hostdata->chip / 100 == 8) {
expected_ccf = clock_to_ccf (expected_clock);
hostdata->saved_scntl3 = NCR53c7x0_read8(SCNTL3_REG_800);
ccf = hostdata->saved_scntl3 & SCNTL3_800_CCF_MASK;
if (expected_ccf != -1 && ccf != expected_ccf && !ccf) {
hostdata->saved_scntl3 = (hostdata->saved_scntl3 &
~SCNTL3_800_CCF_MASK) | expected_ccf;
if (!uninitialized) {
printk ("scsi%d : reset ccf to %d from %d\n",
host->host_no, expected_ccf, ccf);
uninitialized = 1;
}
}
} else
ccf = 0;
if ((!hostdata->scsi_clock) && (hostdata->scsi_clock = ccf_to_clock (ccf))
== -1) {
printk ("scsi%d : clock conversion factor %d unknown.\n"
"         synchronous transfers disabled\n",
host->host_no, ccf);
hostdata->options &= ~OPTION_SYNCHRONOUS;
hostdata->scsi_clock = 0;
}
if (expected_clock == -1 || hostdata->scsi_clock != expected_clock)
printk ("scsi%d : using %dMHz SCSI clock\n", host->host_no,
hostdata->scsi_clock / 1000000);
for (i = 0; i < 16; ++i)
hostdata->cmd_allocated[i] = 0;
if (hostdata->init_save_regs)
hostdata->init_save_regs (host);
if (hostdata->init_fixup)
hostdata->init_fixup (host);
if (!the_template) {
the_template = host->hostt;
first_host = host;
}
hostdata->soft_reset (host);
#if 1
hostdata->debug_count_limit = -1;
#else
hostdata->debug_count_limit = 1;
#endif
hostdata->intrs = -1;
hostdata->resets = -1;
memcpy ((void *) hostdata->synchronous_want, (void *) sdtr_message,
sizeof (hostdata->synchronous_want));
NCR53c7x0_driver_init (host);
for (search = first_host; search && !(search->hostt == the_template &&
search->irq == host->irq && search != host); search=search->next);
if (!search) {
if (request_irq(host->irq, NCR53c7x0_intr, SA_INTERRUPT, "53c7,8xx", NULL)) {
printk("scsi%d : IRQ%d not free, detaching\n"
"         You have either a configuration problem, or a\n"
"         broken BIOS.  You may wish to manually assign\n"
"         an interrupt to the NCR board rather than using\n"
"         an automatic setting.\n",
host->host_no, host->irq);
scsi_unregister (host);
return -1;
}
} else {
printk("scsi%d : using interrupt handler previously installed for scsi%d\n",
host->host_no, search->host_no);
}
if ((hostdata->run_tests && hostdata->run_tests(host) == -1) ||
(hostdata->options & OPTION_DEBUG_TESTS_ONLY)) {
scsi_unregister (host);
return -1;
} else {
if (host->io_port) {
host->n_io_port = 128;
request_region (host->io_port, host->n_io_port, "ncr53c7,8xx");
}
}
if (NCR53c7x0_read8 (SBCL_REG) & SBCL_BSY) {
printk ("scsi%d : bus wedge, doing SCSI reset\n", host->host_no);
hard_reset (host);
}
return 0;
}
static int
normal_init (Scsi_Host_Template *tpnt, int board, int chip,
u32 base, int io_port, int irq, int dma, int pci_valid,
unsigned char pci_bus, unsigned char pci_device_fn, long long options) {
struct Scsi_Host *instance;
struct NCR53c7x0_hostdata *hostdata;
char chip_str[80];
int script_len = 0, dsa_len = 0, size = 0, max_cmd_size = 0,
schedule_size = 0, ok = 0;
void *tmp;
options |= perm_options;
switch (chip) {
case 825:
case 820:
case 815:
case 810:
schedule_size = (tpnt->can_queue + 1) * 8 ;
script_len = NCR53c8xx_script_len;
dsa_len = NCR53c8xx_dsa_len;
options |= OPTION_INTFLY;
sprintf (chip_str, "NCR53c%d", chip);
break;
default:
printk("scsi-ncr53c7,8xx : unsupported SCSI chip %d\n", chip);
return -1;
}
printk("scsi-ncr53c7,8xx : %s at memory 0x%x, io 0x%x, irq %d",
chip_str, (unsigned) base, io_port, irq);
if (dma == DMA_NONE)
printk("\n");
else
printk(", dma %d\n", dma);
if ((chip / 100 == 8) && !pci_valid)
printk ("scsi-ncr53c7,8xx : for better reliability and performance, please use the\n"
"        PCI override instead.\n"
"	 Syntax : ncr53c8{10,15,20,25}=pci,<bus>,<device>,<function>\n"
"                 <bus> and <device> are usually 0.\n");
if (options & OPTION_DEBUG_PROBE_ONLY) {
printk ("scsi-ncr53c7,8xx : probe only enabled, aborting initialization\n");
return -1;
}
max_cmd_size = sizeof(struct NCR53c7x0_cmd) + dsa_len +
2 *
( 2 *
tpnt->sg_tablesize +
3
) *
8 ;
size = sizeof(struct NCR53c7x0_hostdata) + script_len +
(sizeof(void *) - sizeof(u32)) + max_cmd_size + schedule_size;
instance = scsi_register (tpnt, size);
if (!instance)
return -1;
hostdata = (struct NCR53c7x0_hostdata *)
instance->hostdata;
hostdata->size = size;
hostdata->script_count = script_len / sizeof(u32);
hostdata = (struct NCR53c7x0_hostdata *) instance->hostdata;
hostdata->board = board;
hostdata->chip = chip;
if ((hostdata->pci_valid = pci_valid)) {
hostdata->pci_bus = pci_bus;
hostdata->pci_device_fn = pci_device_fn;
}
if (base) {
instance->base = (unsigned char *) (unsigned long) base;
if (!(options & OPTION_IO_MAPPED)) {
options |= OPTION_MEMORY_MAPPED;
ok = 1;
}
} else {
options &= ~OPTION_MEMORY_MAPPED;
}
if (io_port) {
instance->io_port = io_port;
options |= OPTION_IO_MAPPED;
ok = 1;
} else {
options &= ~OPTION_IO_MAPPED;
}
if (!ok) {
printk ("scsi%d : not initializing, no I/O or memory mapping known \n",
instance->host_no);
scsi_unregister (instance);
return -1;
}
instance->irq = irq;
instance->dma_channel = dma;
hostdata->options = options;
hostdata->dsa_len = dsa_len;
hostdata->max_cmd_size = max_cmd_size;
hostdata->num_cmds = 1;
tmp = (hostdata->script + hostdata->script_count);
hostdata->free = ROUNDUP(tmp, void *);
hostdata->free->real = tmp;
hostdata->free->size = max_cmd_size;
hostdata->free->free = NULL;
hostdata->free->next = NULL;
hostdata->extra_allocate = 0;
hostdata->schedule = (chip == 700 || chip == 70066) ?
NULL : (u32 *) ((char *)hostdata->free + max_cmd_size);
if (track_events)
hostdata->events = (struct NCR53c7x0_event *) (track_events ?
vmalloc (sizeof (struct NCR53c7x0_event) * track_events) : NULL);
else
hostdata->events = NULL;
if (hostdata->events) {
memset ((void *) hostdata->events, 0, sizeof(struct NCR53c7x0_event) *
track_events);
hostdata->event_size = track_events;
hostdata->event_index = 0;
} else
hostdata->event_size = 0;
return NCR53c7x0_init(instance);
}
static int
ncr_pci_init (Scsi_Host_Template *tpnt, int board, int chip,
unsigned char bus, unsigned char device_fn, long long options) {
unsigned short vendor_id, device_id, command;
#ifdef LINUX_1_2
unsigned long
#else
unsigned int
#endif
base, io_port;
unsigned char irq, revision;
int error, expected_chip;
int expected_id = -1, max_revision = -1, min_revision = -1;
int i;
printk("scsi-ncr53c7,8xx : at PCI bus %d, device %d,  function %d\n",
bus, (int) (device_fn & 0xf8) >> 3,
(int) device_fn & 7);
if (!pcibios_present()) {
printk("scsi-ncr53c7,8xx : not initializing due to lack of PCI BIOS,\n"
"        try using memory, port, irq override instead.\n");
return -1;
}
if ((error = pcibios_read_config_word (bus, device_fn, PCI_VENDOR_ID,
&vendor_id)) ||
(error = pcibios_read_config_word (bus, device_fn, PCI_DEVICE_ID,
&device_id)) ||
(error = pcibios_read_config_word (bus, device_fn, PCI_COMMAND,
&command)) ||
(error = pcibios_read_config_dword (bus, device_fn,
PCI_BASE_ADDRESS_0, &io_port)) ||
(error = pcibios_read_config_dword (bus, device_fn,
PCI_BASE_ADDRESS_1, &base)) ||
(error = pcibios_read_config_byte (bus, device_fn, PCI_CLASS_REVISION,
&revision)) ||
(error = pcibios_read_config_byte (bus, device_fn, PCI_INTERRUPT_LINE,
&irq))) {
printk ("scsi-ncr53c7,8xx : error %s not initializing due to error reading configuration space\n"
"	 perhaps you specified an incorrect PCI bus, device, or function.\n"
, pcibios_strerror(error));
return -1;
}
if (vendor_id != PCI_VENDOR_ID_NCR) {
printk ("scsi-ncr53c7,8xx : not initializing, 0x%04x is not NCR vendor ID\n",
(int) vendor_id);
return -1;
}
if (command & PCI_COMMAND_IO) {
if ((io_port & 3) != 1) {
printk ("scsi-ncr53c7,8xx : disabling I/O mapping since base address 0 (0x%x)\n"
"        bits 0..1 indicate a non-IO mapping\n",
(unsigned) io_port);
io_port = 0;
} else
io_port &= PCI_BASE_ADDRESS_IO_MASK;
} else {
io_port = 0;
}
if (command & PCI_COMMAND_MEMORY) {
if ((base & PCI_BASE_ADDRESS_SPACE) != PCI_BASE_ADDRESS_SPACE_MEMORY) {
printk("scsi-ncr53c7,8xx : disabling memory mapping since base address 1\n"
"        contains a non-memory mapping\n");
base = 0;
} else
base &= PCI_BASE_ADDRESS_MEM_MASK;
} else {
base = 0;
}
if (!io_port && !base) {
printk ("scsi-ncr53c7,8xx : not initializing, both I/O and memory mappings disabled\n");
return -1;
}
if (!(command & PCI_COMMAND_MASTER)) {
printk ("scsi-ncr53c7,8xx : not initializing, BUS MASTERING was disabled\n");
return -1;
}
for (i = 0; i < NPCI_CHIP_IDS; ++i) {
if (device_id == pci_chip_ids[i].pci_device_id) {
max_revision = pci_chip_ids[i].max_revision;
min_revision = pci_chip_ids[i].min_revision;
expected_chip = pci_chip_ids[i].chip;
}
if (chip == pci_chip_ids[i].chip)
expected_id = pci_chip_ids[i].pci_device_id;
}
if (chip && device_id != expected_id)
printk ("scsi-ncr53c7,8xx : warning : device id of 0x%04x doesn't\n"
"                   match expected 0x%04x\n",
(unsigned int) device_id, (unsigned int) expected_id );
if (max_revision != -1 && revision > max_revision)
printk ("scsi-ncr53c7,8xx : warning : revision of %d is greater than %d.\n",
(int) revision, max_revision);
else if (min_revision != -1 && revision < min_revision)
printk ("scsi-ncr53c7,8xx : warning : revision of %d is less than %d.\n",
(int) revision, min_revision);
if (io_port && check_region (io_port, 128)) {
printk ("scsi-ncr53c7,8xx : IO region 0x%x to 0x%x is in use\n",
(unsigned) io_port, (unsigned) io_port + 127);
return -1;
}
return normal_init (tpnt, board, chip, (int) base, io_port,
(int) irq, DMA_NONE, 1, bus, device_fn, options);
}
int
NCR53c7xx_detect(Scsi_Host_Template *tpnt) {
int i;
int current_override;
int count;
unsigned char pci_bus, pci_device_fn;
static short pci_index=0;
#ifndef LINUX_1_2
tpnt->proc_dir = &proc_scsi_ncr53c7xx;
#endif
for (current_override = count = 0; current_override < OVERRIDE_LIMIT;
++current_override) {
if (overrides[current_override].pci ?
!ncr_pci_init (tpnt, overrides[current_override].board,
overrides[current_override].chip,
(unsigned char) overrides[current_override].data.pci.bus,
(((overrides[current_override].data.pci.device
<< 3) & 0xf8)|(overrides[current_override].data.pci.function &
7)), overrides[current_override].options):
!normal_init (tpnt, overrides[current_override].board,
overrides[current_override].chip,
overrides[current_override].data.normal.base,
overrides[current_override].data.normal.io_port,
overrides[current_override].data.normal.irq,
overrides[current_override].data.normal.dma,
0 , 0 ,
0 ,
overrides[current_override].options)) {
++count;
}
}
if (pcibios_present()) {
for (i = 0; i < NPCI_CHIP_IDS; ++i)
for (pci_index = 0;
!pcibios_find_device (PCI_VENDOR_ID_NCR,
pci_chip_ids[i].pci_device_id, pci_index, &pci_bus,
&pci_device_fn);
++pci_index)
if (!ncr_pci_init (tpnt, BOARD_GENERIC, pci_chip_ids[i].chip,
pci_bus, pci_device_fn, 0))
++count;
}
return count;
}
#include "53c8xx_d.h"
#ifdef A_int_debug_sync
#define DEBUG_SYNC_INTR A_int_debug_sync
#endif
static int NCR53c8xx_script_len = sizeof (SCRIPT);
static int NCR53c8xx_dsa_len = A_dsa_end + Ent_dsa_zero - Ent_dsa_code_template;
static void
NCR53c8x0_init_fixup (struct Scsi_Host *host) {
NCR53c7x0_local_declare();
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
unsigned char tmp;
int i, ncr_to_memory, memory_to_ncr;
u32 base;
NCR53c7x0_local_setup(host);
memcpy ((void *) hostdata->script, (void *) SCRIPT,
sizeof(SCRIPT));
for (i = 0; i < PATCHES; ++i)
hostdata->script[LABELPATCHES[i]] +=
virt_to_bus(hostdata->script);
patch_abs_32 (hostdata->script, 0, NCR53c7xx_msg_abort,
virt_to_bus(&(hostdata->NCR53c7xx_msg_abort)));
patch_abs_32 (hostdata->script, 0, NCR53c7xx_msg_reject,
virt_to_bus(&(hostdata->NCR53c7xx_msg_reject)));
patch_abs_32 (hostdata->script, 0, NCR53c7xx_zero,
virt_to_bus(&(hostdata->NCR53c7xx_zero)));
patch_abs_32 (hostdata->script, 0, NCR53c7xx_sink,
virt_to_bus(&(hostdata->NCR53c7xx_sink)));
patch_abs_32 (hostdata->script, 0, NOP_insn,
virt_to_bus(&(hostdata->NOP_insn)));
patch_abs_32 (hostdata->script, 0, schedule,
virt_to_bus((void *) hostdata->schedule));
for (i = 0; i < EXTERNAL_PATCHES_LEN; ++i)
hostdata->script[EXTERNAL_PATCHES[i].offset] +=
virt_to_bus(EXTERNAL_PATCHES[i].address);
patch_abs_rwri_data (hostdata->script, 0, dsa_save_data_pointer,
Ent_dsa_code_save_data_pointer - Ent_dsa_zero);
patch_abs_rwri_data (hostdata->script, 0, dsa_restore_pointers,
Ent_dsa_code_restore_pointers - Ent_dsa_zero);
patch_abs_rwri_data (hostdata->script, 0, dsa_check_reselect,
Ent_dsa_code_check_reselect - Ent_dsa_zero);
tmp = NCR53c7x0_read8(DMODE_REG_10);
tmp &= (DMODE_800_ERL | DMODE_BL_MASK);
if (!(hostdata->options & OPTION_MEMORY_MAPPED)) {
base = (u32) host->io_port;
memory_to_ncr = tmp|DMODE_800_DIOM;
ncr_to_memory = tmp|DMODE_800_SIOM;
} else {
base = virt_to_bus(host->base);
memory_to_ncr = ncr_to_memory = tmp;
}
patch_abs_32 (hostdata->script, 0, addr_scratch, base + SCRATCHA_REG_800);
patch_abs_32 (hostdata->script, 0, addr_temp, base + TEMP_REG);
patch_abs_rwri_data (hostdata->script, 0, dmode_memory_to_memory, tmp);
patch_abs_rwri_data (hostdata->script, 0, dmode_memory_to_ncr, memory_to_ncr);
patch_abs_rwri_data (hostdata->script, 0, dmode_ncr_to_memory, ncr_to_memory);
patch_abs_32 (hostdata->script, 0, msg_buf,
virt_to_bus((void *)&(hostdata->msg_buf)));
patch_abs_32 (hostdata->script, 0, reconnect_dsa_head,
virt_to_bus((void *)&(hostdata->reconnect_dsa_head)));
patch_abs_32 (hostdata->script, 0, addr_reconnect_dsa_head,
virt_to_bus((void *)&(hostdata->addr_reconnect_dsa_head)));
patch_abs_32 (hostdata->script, 0, reselected_identify,
virt_to_bus((void *)&(hostdata->reselected_identify)));
#if 0
patch_abs_32 (hostdata->script, 0, reselected_tag,
virt_to_bus((void *)&(hostdata->reselected_tag)));
#endif
patch_abs_32 (hostdata->script, 0, test_dest,
virt_to_bus((void*)&hostdata->test_dest));
patch_abs_32 (hostdata->script, 0, test_src,
virt_to_bus(&hostdata->test_source));
patch_abs_rwri_data (hostdata->script, 0, dsa_check_reselect,
(unsigned char)(Ent_dsa_code_check_reselect - Ent_dsa_zero));
#ifdef A_int_EVENT_SELECT
patch_abs_32 (hostdata->script, 0, int_EVENT_SELECT, (u32) EVENT_SELECT);
#endif
#ifdef A_int_EVENT_DISCONNECT
patch_abs_32 (hostdata->script, 0, int_EVENT_DISCONNECT, (u32) EVENT_DISCONNECT);
#endif
#ifdef A_int_EVENT_RESELECT
patch_abs_32 (hostdata->script, 0, int_EVENT_RESELECT, (u32) EVENT_RESELECT);
#endif
#ifdef A_int_EVENT_COMPLETE
patch_abs_32 (hostdata->script, 0, int_EVENT_COMPLETE, (u32) EVENT_COMPLETE);
#endif
#ifdef A_int_EVENT_IDLE
patch_abs_32 (hostdata->script, 0, int_EVENT_IDLE, (u32) EVENT_IDLE);
#endif
#ifdef A_int_EVENT_SELECT_FAILED
patch_abs_32 (hostdata->script, 0, int_EVENT_SELECT_FAILED,
(u32) EVENT_SELECT_FAILED);
#endif
#ifdef A_int_EVENT_BEFORE_SELECT
patch_abs_32 (hostdata->script, 0, int_EVENT_BEFORE_SELECT,
(u32) EVENT_BEFORE_SELECT);
#endif
#ifdef A_int_EVENT_RESELECT_FAILED
patch_abs_32 (hostdata->script, 0, int_EVENT_RESELECT_FAILED,
(u32) EVENT_RESELECT_FAILED);
#endif
hostdata->E_accept_message = Ent_accept_message;
hostdata->E_command_complete = Ent_command_complete;
hostdata->E_cmdout_cmdout = Ent_cmdout_cmdout;
hostdata->E_data_transfer = Ent_data_transfer;
hostdata->E_debug_break = Ent_debug_break;
hostdata->E_dsa_code_template = Ent_dsa_code_template;
hostdata->E_dsa_code_template_end = Ent_dsa_code_template_end;
hostdata->E_end_data_transfer = Ent_end_data_transfer;
hostdata->E_initiator_abort = Ent_initiator_abort;
hostdata->E_msg_in = Ent_msg_in;
hostdata->E_other_transfer = Ent_other_transfer;
hostdata->E_other_in = Ent_other_in;
hostdata->E_other_out = Ent_other_out;
hostdata->E_reject_message = Ent_reject_message;
hostdata->E_respond_message = Ent_respond_message;
hostdata->E_select = Ent_select;
hostdata->E_select_msgout = Ent_select_msgout;
hostdata->E_target_abort = Ent_target_abort;
#ifdef Ent_test_0
hostdata->E_test_0 = Ent_test_0;
#endif
hostdata->E_test_1 = Ent_test_1;
hostdata->E_test_2 = Ent_test_2;
#ifdef Ent_test_3
hostdata->E_test_3 = Ent_test_3;
#endif
hostdata->E_wait_reselect = Ent_wait_reselect;
hostdata->E_dsa_code_begin = Ent_dsa_code_begin;
hostdata->dsa_cmdout = A_dsa_cmdout;
hostdata->dsa_cmnd = A_dsa_cmnd;
hostdata->dsa_datain = A_dsa_datain;
hostdata->dsa_dataout = A_dsa_dataout;
hostdata->dsa_end = A_dsa_end;
hostdata->dsa_msgin = A_dsa_msgin;
hostdata->dsa_msgout = A_dsa_msgout;
hostdata->dsa_msgout_other = A_dsa_msgout_other;
hostdata->dsa_next = A_dsa_next;
hostdata->dsa_select = A_dsa_select;
hostdata->dsa_start = Ent_dsa_code_template - Ent_dsa_zero;
hostdata->dsa_status = A_dsa_status;
hostdata->dsa_jump_dest = Ent_dsa_code_fix_jump - Ent_dsa_zero +
8 ;
if (A_dsa_fields_start != Ent_dsa_code_template_end -
Ent_dsa_zero)
printk("scsi%d : NCR dsa_fields start is %d not %d\n",
host->host_no, A_dsa_fields_start, Ent_dsa_code_template_end -
Ent_dsa_zero);
printk("scsi%d : NCR code relocated to 0x%lx (virt 0x%p)\n", host->host_no,
virt_to_bus(hostdata->script), hostdata->script);
}
static int
NCR53c8xx_run_tests (struct Scsi_Host *host) {
NCR53c7x0_local_declare();
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
unsigned long timeout;
u32 start;
int failed, i;
unsigned long flags;
NCR53c7x0_local_setup(host);
save_flags(flags);
cli();
if (!hostdata->idle) {
printk ("scsi%d : chip not idle, aborting tests\n", host->host_no);
restore_flags(flags);
return -1;
}
if ((hostdata->options & OPTION_DEBUG_TEST1) &&
hostdata->state != STATE_DISABLED) {
hostdata->idle = 0;
hostdata->test_running = 1;
hostdata->test_completed = -1;
hostdata->test_dest = 0;
hostdata->test_source = 0xdeadbeef;
start = virt_to_bus (hostdata->script) + hostdata->E_test_1;
hostdata->state = STATE_RUNNING;
printk ("scsi%d : test 1", host->host_no);
NCR53c7x0_write32 (DSP_REG, start);
printk (" started\n");
sti();
timeout = jiffies + 5 * HZ / 10;
while ((hostdata->test_completed == -1) && jiffies < timeout)
barrier();
failed = 1;
if (hostdata->test_completed == -1)
printk ("scsi%d : driver test 1 timed out%s\n",host->host_no ,
(hostdata->test_dest == 0xdeadbeef) ?
" due to lost interrupt.\n"
"         Please verify that the correct IRQ is being used for your board,\n"
"	      and that the motherboard IRQ jumpering matches the PCI setup on\n"
"         PCI systems.\n"
"         If you are using a NCR53c810 board in a PCI system, you should\n"
"         also verify that the board is jumpered to use PCI INTA, since\n"
"         most PCI motherboards lack support for INTB, INTC, and INTD.\n"
: "");
else if (hostdata->test_completed != 1)
printk ("scsi%d : test 1 bad interrupt value (%d)\n",
host->host_no, hostdata->test_completed);
else
failed = (hostdata->test_dest != 0xdeadbeef);
if (hostdata->test_dest != 0xdeadbeef) {
printk ("scsi%d : driver test 1 read 0x%x instead of 0xdeadbeef indicating a\n"
"         probable cache invalidation problem.  Please configure caching\n"
"         as write-through or disabled\n",
host->host_no, hostdata->test_dest);
}
if (failed) {
printk ("scsi%d : DSP = 0x%p (script at 0x%p, start at 0x%x)\n",
host->host_no, bus_to_virt(NCR53c7x0_read32(DSP_REG)),
hostdata->script, start);
printk ("scsi%d : DSPS = 0x%x\n", host->host_no,
NCR53c7x0_read32(DSPS_REG));
restore_flags(flags);
return -1;
}
hostdata->test_running = 0;
}
if ((hostdata->options & OPTION_DEBUG_TEST2) &&
hostdata->state != STATE_DISABLED) {
u32 dsa[48];
unsigned char identify = IDENTIFY(0, 0);
unsigned char cmd[6];
unsigned char data[36];
unsigned char status = 0xff;
unsigned char msg = 0xff;
cmd[0] = INQUIRY;
cmd[1] = cmd[2] = cmd[3] = cmd[5] = 0;
cmd[4] = sizeof(data);
dsa[2] = 1;
dsa[3] = virt_to_bus(&identify);
dsa[4] = 6;
dsa[5] = virt_to_bus(&cmd);
dsa[6] = sizeof(data);
dsa[7] = virt_to_bus(&data);
dsa[8] = 1;
dsa[9] = virt_to_bus(&status);
dsa[10] = 1;
dsa[11] = virt_to_bus(&msg);
for (i = 0; i < 3; ++i) {
cli();
if (!hostdata->idle) {
printk ("scsi%d : chip not idle, aborting tests\n", host->host_no);
restore_flags(flags);
return -1;
}
dsa[0] = (0x33 << 24) | (i << 16) ;
hostdata->idle = 0;
hostdata->test_running = 2;
hostdata->test_completed = -1;
start = virt_to_bus(hostdata->script) + hostdata->E_test_2;
hostdata->state = STATE_RUNNING;
NCR53c7x0_write32 (DSA_REG, virt_to_bus(dsa));
NCR53c7x0_write32 (DSP_REG, start);
sti();
timeout = jiffies + 5 * HZ;
while ((hostdata->test_completed == -1) && jiffies < timeout)
barrier();
NCR53c7x0_write32 (DSA_REG, 0);
if (hostdata->test_completed == 2) {
data[35] = 0;
printk ("scsi%d : test 2 INQUIRY to target %d, lun 0 : %s\n",
host->host_no, i, data + 8);
printk ("scsi%d : status ", host->host_no);
print_status (status);
printk ("\nscsi%d : message ", host->host_no);
print_msg (&msg);
printk ("\n");
} else if (hostdata->test_completed == 3) {
printk("scsi%d : test 2 no connection with target %d\n",
host->host_no, i);
if (!hostdata->idle) {
printk("scsi%d : not idle\n", host->host_no);
restore_flags(flags);
return -1;
}
} else if (hostdata->test_completed == -1) {
printk ("scsi%d : test 2 timed out\n", host->host_no);
restore_flags(flags);
return -1;
}
hostdata->test_running = 0;
}
}
restore_flags(flags);
return 0;
}
static void
NCR53c8xx_dsa_fixup (struct NCR53c7x0_cmd *cmd) {
Scsi_Cmnd *c = cmd->cmd;
struct Scsi_Host *host = c->host;
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
int i;
memcpy (cmd->dsa, hostdata->script + (hostdata->E_dsa_code_template / 4),
hostdata->E_dsa_code_template_end - hostdata->E_dsa_code_template);
patch_abs_tci_data (cmd->dsa, Ent_dsa_code_template / sizeof(u32),
dsa_temp_lun, c->lun);
patch_abs_32 (cmd->dsa, Ent_dsa_code_template / sizeof(u32),
dsa_temp_addr_next, virt_to_bus(&cmd->dsa_next_addr));
patch_abs_32 (cmd->dsa, Ent_dsa_code_template / sizeof(u32),
dsa_temp_next, virt_to_bus(cmd->dsa) + Ent_dsa_zero -
Ent_dsa_code_template + A_dsa_next);
patch_abs_32 (cmd->dsa, Ent_dsa_code_template / sizeof(u32),
dsa_temp_sync, virt_to_bus((void *)hostdata->sync[c->target].script));
patch_abs_tci_data (cmd->dsa, Ent_dsa_code_template / sizeof(u32),
dsa_temp_target, c->target);
patch_abs_32 (cmd->dsa, Ent_dsa_code_template / sizeof(u32),
dsa_temp_addr_saved_pointer, virt_to_bus(&cmd->saved_data_pointer));
patch_abs_32 (cmd->dsa, Ent_dsa_code_template / sizeof(u32),
dsa_temp_addr_saved_residual, virt_to_bus(&cmd->saved_residual));
patch_abs_32 (cmd->dsa, Ent_dsa_code_template / sizeof(u32),
dsa_temp_addr_residual, virt_to_bus(&cmd->residual));
patch_abs_32 (cmd->dsa, Ent_dsa_code_template / sizeof(u32),
dsa_temp_addr_dsa_value, virt_to_bus(&cmd->dsa_addr));
}
static volatile int process_issue_queue_running = 0;
static __inline__ void
run_process_issue_queue(void) {
unsigned long flags;
save_flags (flags);
cli();
if (!process_issue_queue_running) {
process_issue_queue_running = 1;
process_issue_queue(flags);
}
restore_flags (flags);
}
static void
abnormal_finished (struct NCR53c7x0_cmd *cmd, int result) {
Scsi_Cmnd *c = cmd->cmd;
struct Scsi_Host *host = c->host;
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
unsigned long flags;
int left, found;
volatile struct NCR53c7x0_cmd * linux_search;
volatile struct NCR53c7x0_cmd * volatile *linux_prev;
volatile u32 *ncr_prev, *current, ncr_search;
#if 0
printk ("scsi%d: abnormal finished\n", host->host_no);
#endif
save_flags(flags);
cli();
found = 0;
for (found = 0, left = host->can_queue, current = hostdata->schedule;
left > 0; --left, current += 2)
{
if (issue_to_cmd (host, hostdata, (u32 *) current) == cmd)
{
current[0] = hostdata->NOP_insn;
current[1] = 0xdeadbeef;
++found;
break;
}
}
for (left = host->can_queue,
ncr_search = hostdata->reconnect_dsa_head,
ncr_prev = &hostdata->reconnect_dsa_head;
left >= 0 && ncr_search &&
((char*)bus_to_virt(ncr_search) + hostdata->dsa_start)
!= (char *) cmd->dsa;
ncr_prev = (u32*) ((char*)bus_to_virt(ncr_search) +
hostdata->dsa_next), ncr_search = *ncr_prev, --left);
if (left < 0)
printk("scsi%d: loop detected in ncr reconnect list\n",
host->host_no);
else if (ncr_search)
if (found)
printk("scsi%d: scsi %ld in ncr issue array and reconnect lists\n",
host->host_no, c->pid);
else {
volatile u32 * next = (u32 *)
((char *)bus_to_virt(ncr_search) + hostdata->dsa_next);
*ncr_prev = *next;
found = 1;
}
for (left = host->can_queue, linux_search = hostdata->running_list,
linux_prev = &hostdata->running_list;
left >= 0 && linux_search && linux_search != cmd;
linux_prev = &(linux_search->next),
linux_search = linux_search->next, --left);
if (left < 0)
printk ("scsi%d: loop detected in host running list for scsi pid %ld\n",
host->host_no, c->pid);
else if (linux_search) {
*linux_prev = linux_search->next;
--hostdata->busy[c->target][c->lun];
}
cmd->next = hostdata->free;
hostdata->free = cmd;
c->host_scribble = NULL;
c->result = result;
c->scsi_done(c);
restore_flags(flags);
run_process_issue_queue();
}
static void
intr_break (struct Scsi_Host *host, struct
NCR53c7x0_cmd *cmd) {
NCR53c7x0_local_declare();
struct NCR53c7x0_break *bp;
#if 0
Scsi_Cmnd *c = cmd ? cmd->cmd : NULL;
#endif
u32 *dsp;
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
unsigned long flags;
NCR53c7x0_local_setup(host);
save_flags(flags);
cli();
dsp = (u32 *) bus_to_virt(NCR53c7x0_read32(DSP_REG));
for (bp = hostdata->breakpoints; bp && bp->address != dsp;
bp = bp->next);
if (!bp)
panic("scsi%d : break point interrupt from %p with no breakpoint!",
host->host_no, dsp);
NCR53c7x0_write8 (hostdata->dmode,
NCR53c7x0_read8(hostdata->dmode)|DMODE_MAN);
restore_flags(flags);
}
static void
print_synchronous (const char *prefix, const unsigned char *msg) {
if (msg[4]) {
int Hz = 1000000000 / (msg[3] * 4);
int integer = Hz / 1000000;
int fraction = (Hz - (integer * 1000000)) / 10000;
printk ("%speriod %dns offset %d %d.%02dMHz %s SCSI%s\n",
prefix, (int) msg[3] * 4, (int) msg[4], integer, fraction,
(((msg[3] * 4) < 200) ? "FAST" : "synchronous"),
(((msg[3] * 4) < 200) ? "-II" : ""));
} else
printk ("%sasynchronous SCSI\n", prefix);
}
static void
set_synchronous (struct Scsi_Host *host, int target, int sxfer, int scntl3,
int now_connected) {
NCR53c7x0_local_declare();
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
u32 *script;
NCR53c7x0_local_setup(host);
sxfer &= 0xff;
scntl3 &= 0xff;
hostdata->sync[target].sxfer_sanity = sxfer;
hostdata->sync[target].scntl3_sanity = scntl3;
if ((hostdata->chip != 700) && (hostdata->chip != 70066)) {
hostdata->sync[target].select_indirect = (scntl3 << 24) |
(target << 16) | (sxfer << 8);
script = (u32 *) hostdata->sync[target].script;
if ((hostdata->chip / 100) == 8) {
script[0] = ((DCMD_TYPE_RWRI | DCMD_RWRI_OPC_MODIFY |
DCMD_RWRI_OP_MOVE) << 24) |
(SCNTL3_REG_800 << 16) | (scntl3 << 8);
script[1] = 0;
script += 2;
}
script[0] = ((DCMD_TYPE_RWRI | DCMD_RWRI_OPC_MODIFY |
DCMD_RWRI_OP_MOVE) << 24) |
(SXFER_REG << 16) | (sxfer << 8);
script[1] = 0;
script += 2;
#ifdef DEBUG_SYNC_INTR
if (hostdata->options & OPTION_DEBUG_DISCONNECT) {
script[0] = ((DCMD_TYPE_TCI|DCMD_TCI_OP_INT) << 24) | DBC_TCI_TRUE;
script[1] = DEBUG_SYNC_INTR;
script += 2;
}
#endif
script[0] = ((DCMD_TYPE_TCI|DCMD_TCI_OP_RETURN) << 24) | DBC_TCI_TRUE;
script[1] = 0;
script += 2;
}
if (hostdata->options & OPTION_DEBUG_SYNCHRONOUS)
printk ("scsi%d : target %d sync parameters are sxfer=0x%x, scntl3=0x%x\n",
host->host_no, target, sxfer, scntl3);
if (now_connected) {
if ((hostdata->chip / 100) == 8)
NCR53c7x0_write8(SCNTL3_REG_800, scntl3);
NCR53c7x0_write8(SXFER_REG, sxfer);
}
}
static void
asynchronous (struct Scsi_Host *host, int target) {
NCR53c7x0_local_declare();
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
NCR53c7x0_local_setup(host);
set_synchronous (host, target, 0, hostdata->saved_scntl3,
1);
printk ("scsi%d : setting target %d to asynchronous SCSI\n",
host->host_no, target);
}
static const struct {
int div;
unsigned char scf;
unsigned char tp;
} syncs[] = {
{ 40, 1, 0}, { 50, 1, 1}, { 60, 1, 2},
{ 70, 1, 3}, { 75, 2, 1}, { 80, 1, 4},
{ 90, 1, 5}, { 100, 1, 6}, { 105, 2, 3},
{ 110, 1, 7}, { 120, 2, 4}, { 135, 2, 5},
{ 140, 3, 3}, { 150, 2, 6}, { 160, 3, 4},
{ 165, 2, 7}, { 180, 3, 5}, { 200, 3, 6},
{ 210, 4, 3}, { 220, 3, 7}, { 240, 4, 4},
{ 270, 4, 5}, { 300, 4, 6}, { 330, 4, 7}
};
static void
synchronous (struct Scsi_Host *host, int target, char *msg) {
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
int desire, divisor, i, limit;
unsigned char scntl3, sxfer;
char buf[80];
desire = 1000000000L / (msg[3] * 4);
divisor = (hostdata->scsi_clock * 10) / desire;
if (msg[4] > 8)
msg[4] = 8;
if (hostdata->options & OPTION_DEBUG_SDTR)
printk("scsi%d : optimal synchronous divisor of %d.%01d\n",
host->host_no, divisor / 10, divisor % 10);
limit = (sizeof(syncs) / sizeof(syncs[0]) -1);
for (i = 0; (i < limit) && (divisor > syncs[i].div); ++i);
if (hostdata->options & OPTION_DEBUG_SDTR)
printk("scsi%d : selected synchronous divisor of %d.%01d\n",
host->host_no, syncs[i].div / 10, syncs[i].div % 10);
msg[3] = ((1000000000L / hostdata->scsi_clock) * syncs[i].div / 10 / 4);
if (hostdata->options & OPTION_DEBUG_SDTR)
printk("scsi%d : selected synchronous period of %dns\n", host->host_no,
msg[3] * 4);
scntl3 = (hostdata->chip / 100 == 8) ? ((hostdata->saved_scntl3 &
~SCNTL3_800_SCF_MASK) | (syncs[i].scf << SCNTL3_800_SCF_SHIFT)) : 0;
sxfer = (msg[4] << SXFER_MO_SHIFT) | ((syncs[i].tp) << SXFER_TP_SHIFT);
if (hostdata->options & OPTION_DEBUG_SDTR)
printk ("scsi%d : sxfer=0x%x scntl3=0x%x\n",
host->host_no, (int) sxfer, (int) scntl3);
set_synchronous (host, target, sxfer, scntl3, 1);
sprintf (buf, "scsi%d : setting target %d to ", host->host_no, target);
print_synchronous (buf, msg);
}
static int
NCR53c8x0_dstat_sir_intr (struct Scsi_Host *host, struct
NCR53c7x0_cmd *cmd) {
NCR53c7x0_local_declare();
int print;
Scsi_Cmnd *c = cmd ? cmd->cmd : NULL;
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
u32 dsps,*dsp;
NCR53c7x0_local_setup(host);
dsps = NCR53c7x0_read32(DSPS_REG);
dsp = (u32 *) bus_to_virt(NCR53c7x0_read32(DSP_REG));
if (hostdata->options & OPTION_DEBUG_INTR)
printk ("scsi%d : DSPS = 0x%x\n", host->host_no, dsps);
switch (dsps) {
case A_int_msg_1:
print = 1;
switch (hostdata->msg_buf[0]) {
case MESSAGE_REJECT:
hostdata->dsp = hostdata->script + hostdata->E_accept_message /
sizeof(u32);
hostdata->dsp_changed = 1;
if (cmd && (cmd->flags & CMD_FLAG_SDTR)) {
printk ("scsi%d : target %d rejected SDTR\n", host->host_no,
c->target);
cmd->flags &= ~CMD_FLAG_SDTR;
asynchronous (host, c->target);
print = 0;
}
break;
case INITIATE_RECOVERY:
printk ("scsi%d : extended contingent allegiance not supported yet, rejecting\n",
host->host_no);
hostdata->dsp = hostdata->script + hostdata->E_reject_message /
sizeof(u32);
hostdata->dsp_changed = 1;
break;
default:
printk ("scsi%d : unsupported message, rejecting\n",
host->host_no);
hostdata->dsp = hostdata->script + hostdata->E_reject_message /
sizeof(u32);
hostdata->dsp_changed = 1;
}
if (print) {
printk ("scsi%d : received message", host->host_no);
if (c)
printk (" from target %d lun %d ", c->target, c->lun);
print_msg ((unsigned char *) hostdata->msg_buf);
printk("\n");
}
return SPECIFIC_INT_NOTHING;
case A_int_msg_sdtr:
if (cmd) {
char buf[80];
sprintf (buf, "scsi%d : target %d %s ", host->host_no, c->target,
(cmd->flags & CMD_FLAG_SDTR) ? "accepting" : "requesting");
print_synchronous (buf, (unsigned char *) hostdata->msg_buf);
if (cmd->flags & CMD_FLAG_SDTR) {
cmd->flags &= ~CMD_FLAG_SDTR;
if (hostdata->msg_buf[4])
synchronous (host, c->target, (unsigned char *)
hostdata->msg_buf);
else
asynchronous (host, c->target);
hostdata->dsp = hostdata->script + hostdata->E_accept_message /
sizeof(u32);
hostdata->dsp_changed = 1;
return SPECIFIC_INT_NOTHING;
} else {
if (hostdata->options & OPTION_SYNCHRONOUS) {
cmd->flags |= CMD_FLAG_DID_SDTR;
synchronous (host, c->target, (unsigned char *)
hostdata->msg_buf);
} else {
hostdata->msg_buf[4] = 0;
asynchronous (host, c->target);
}
patch_dsa_32 (cmd->dsa, dsa_msgout_other, 0, 5);
patch_dsa_32 (cmd->dsa, dsa_msgout_other, 1, (u32)
virt_to_bus ((void *)&hostdata->msg_buf));
hostdata->dsp = hostdata->script +
hostdata->E_respond_message / sizeof(u32);
hostdata->dsp_changed = 1;
}
return SPECIFIC_INT_NOTHING;
}
case A_int_msg_wdtr:
hostdata->dsp = hostdata->script + hostdata->E_reject_message /
sizeof(u32);
hostdata->dsp_changed = 1;
return SPECIFIC_INT_NOTHING;
case A_int_err_unexpected_phase:
if (hostdata->options & OPTION_DEBUG_INTR)
printk ("scsi%d : unexpected phase\n", host->host_no);
return SPECIFIC_INT_ABORT;
case A_int_err_selected:
printk ("scsi%d : selected by target %d\n", host->host_no,
(int) NCR53c7x0_read8(SDID_REG_800) &7);
hostdata->dsp = hostdata->script + hostdata->E_target_abort /
sizeof(u32);
hostdata->dsp_changed = 1;
return SPECIFIC_INT_NOTHING;
case A_int_err_unexpected_reselect:
printk ("scsi%d : unexpected reselect by target %d lun %d\n",
host->host_no, (int) NCR53c7x0_read8(SDID_REG_800) & 7,
hostdata->reselected_identify & 7);
hostdata->dsp = hostdata->script + hostdata->E_initiator_abort /
sizeof(u32);
hostdata->dsp_changed = 1;
return SPECIFIC_INT_NOTHING;
case A_int_err_check_condition:
#if 0
if (hostdata->options & OPTION_DEBUG_INTR)
#endif
printk ("scsi%d : CHECK CONDITION\n", host->host_no);
if (!c) {
printk("scsi%d : CHECK CONDITION with no SCSI command\n",
host->host_no);
return SPECIFIC_INT_PANIC;
}
patch_dsa_32 (cmd->dsa, dsa_msgout, 0, 1);
patch_dsa_32 (cmd->dsa, dsa_cmdout, 0, 6);
c->cmnd[0] = REQUEST_SENSE;
c->cmnd[1] &= 0xe0;
c->cmnd[2] = 0;
c->cmnd[3] = 0;
c->cmnd[4] = sizeof(c->sense_buffer);
c->cmnd[5] = 0;
patch_dsa_32 (cmd->dsa, dsa_dataout, 0,
virt_to_bus(hostdata->script) + hostdata->E_other_transfer);
patch_dsa_32 (cmd->dsa, dsa_datain, 0,
virt_to_bus(cmd->data_transfer_start));
cmd->data_transfer_start[0] = (((DCMD_TYPE_BMI | DCMD_BMI_OP_MOVE_I |
DCMD_BMI_IO)) << 24) | sizeof(c->sense_buffer);
cmd->data_transfer_start[1] = (u32) virt_to_bus(c->sense_buffer);
cmd->data_transfer_start[2] = ((DCMD_TYPE_TCI | DCMD_TCI_OP_JUMP)
<< 24) | DBC_TCI_TRUE;
cmd->data_transfer_start[3] = (u32) virt_to_bus(hostdata->script) +
hostdata->E_other_transfer;
cmd->cmd->result = 0xffff;
hostdata->dsp = (u32 *) hostdata->script + hostdata->E_select /
sizeof(u32);
hostdata->dsp_changed = 1;
return SPECIFIC_INT_NOTHING;
case A_int_debug_break:
return SPECIFIC_INT_BREAK;
case A_int_norm_aborted:
hostdata->dsp = (u32 *) hostdata->schedule;
hostdata->dsp_changed = 1;
if (cmd)
abnormal_finished (cmd, DID_ERROR << 16);
return SPECIFIC_INT_NOTHING;
case A_int_test_1:
case A_int_test_2:
hostdata->idle = 1;
hostdata->test_completed = (dsps - A_int_test_1) / 0x00010000 + 1;
if (hostdata->options & OPTION_DEBUG_INTR)
printk("scsi%d : test%d complete\n", host->host_no,
hostdata->test_completed);
return SPECIFIC_INT_NOTHING;
#ifdef A_int_debug_reselected_ok
case A_int_debug_reselected_ok:
if (hostdata->options & (OPTION_DEBUG_SCRIPT|OPTION_DEBUG_INTR|
OPTION_DEBUG_DISCONNECT)) {
u32 *dsa;
dsa = (u32 *) bus_to_virt (NCR53c7x0_read32(DSA_REG));
printk("scsi%d : reselected_ok (DSA = 0x%x (virt 0x%p)\n",
host->host_no, NCR53c7x0_read32(DSA_REG), dsa);
printk("scsi%d : resume address is 0x%x (virt 0x%p)\n",
host->host_no, cmd->saved_data_pointer,
bus_to_virt(cmd->saved_data_pointer));
print_insn (host, hostdata->script + Ent_reselected_ok /
sizeof(u32), "", 1);
printk ("scsi%d : sxfer=0x%x, scntl3=0x%x\n",
host->host_no, NCR53c7x0_read8(SXFER_REG),
NCR53c7x0_read8(SCNTL3_REG_800));
if (c) {
print_insn (host, (u32 *)
hostdata->sync[c->target].script, "", 1);
print_insn (host, (u32 *)
hostdata->sync[c->target].script + 2, "", 1);
}
}
return SPECIFIC_INT_RESTART;
#endif
#ifdef A_int_debug_reselect_check
case A_int_debug_reselect_check:
if (hostdata->options & (OPTION_DEBUG_SCRIPT|OPTION_DEBUG_INTR)) {
u32 *dsa;
#if 0
u32 *code;
#endif
dsa = bus_to_virt (NCR53c7x0_read32(DSA_REG));
printk("scsi%d : reselected_check_next (DSA = 0x%lx (virt 0x%p))\n",
host->host_no, virt_to_bus(dsa), dsa);
if (dsa) {
printk("scsi%d : resume address is 0x%x (virt 0x%p)\n",
host->host_no, cmd->saved_data_pointer,
bus_to_virt (cmd->saved_data_pointer));
#if 0
printk("scsi%d : template code :\n", host->host_no);
for (code = dsa + (Ent_dsa_code_check_reselect - Ent_dsa_zero)
/ sizeof(u32); code < (dsa + Ent_dsa_zero / sizeof(u32));
code += print_insn (host, code, "", 1));
#endif
}
print_insn (host, hostdata->script + Ent_reselected_ok /
sizeof(u32), "", 1);
}
return SPECIFIC_INT_RESTART;
#endif
#ifdef A_int_debug_dsa_schedule
case A_int_debug_dsa_schedule:
if (hostdata->options & (OPTION_DEBUG_SCRIPT|OPTION_DEBUG_INTR)) {
u32 *dsa;
dsa = (u32 *) bus_to_virt (NCR53c7x0_read32(DSA_REG));
printk("scsi%d : dsa_schedule (old DSA = 0x%lx (virt 0x%p))\n",
host->host_no, virt_to_bus(dsa), dsa);
if (dsa)
printk("scsi%d : resume address is 0x%x (virt 0x%p)\n"
"         (temp was 0x%x (virt 0x%p))\n",
host->host_no, cmd->saved_data_pointer,
bus_to_virt (cmd->saved_data_pointer),
NCR53c7x0_read32 (TEMP_REG),
bus_to_virt (NCR53c7x0_read32(TEMP_REG)));
}
return SPECIFIC_INT_RESTART;
#endif
#ifdef A_int_debug_scheduled
case A_int_debug_scheduled:
if (hostdata->options & (OPTION_DEBUG_SCRIPT|OPTION_DEBUG_INTR)) {
printk("scsi%d : new I/O 0x%x (virt 0x%p) scheduled\n",
host->host_no, NCR53c7x0_read32(DSA_REG),
bus_to_virt(NCR53c7x0_read32(DSA_REG)));
}
return SPECIFIC_INT_RESTART;
#endif
#ifdef A_int_debug_idle
case A_int_debug_idle:
if (hostdata->options & (OPTION_DEBUG_SCRIPT|OPTION_DEBUG_INTR)) {
printk("scsi%d : idle\n", host->host_no);
}
return SPECIFIC_INT_RESTART;
#endif
#ifdef A_int_debug_cmd
case A_int_debug_cmd:
if (hostdata->options & (OPTION_DEBUG_SCRIPT|OPTION_DEBUG_INTR)) {
printk("scsi%d : command sent\n");
}
return SPECIFIC_INT_RESTART;
#endif
#ifdef A_int_debug_dsa_loaded
case A_int_debug_dsa_loaded:
if (hostdata->options & (OPTION_DEBUG_SCRIPT|OPTION_DEBUG_INTR)) {
printk("scsi%d : DSA loaded with 0x%x (virt 0x%p)\n", host->host_no,
NCR53c7x0_read32(DSA_REG),
bus_to_virt(NCR53c7x0_read32(DSA_REG)));
}
return SPECIFIC_INT_RESTART;
#endif
#ifdef A_int_debug_reselected
case A_int_debug_reselected:
if (hostdata->options & (OPTION_DEBUG_SCRIPT|OPTION_DEBUG_INTR|
OPTION_DEBUG_DISCONNECT)) {
printk("scsi%d : reselected by target %d lun %d\n",
host->host_no, (int) NCR53c7x0_read8(SDID_REG_800) & ~0x80,
(int) hostdata->reselected_identify & 7);
print_queues(host);
}
return SPECIFIC_INT_RESTART;
#endif
#ifdef A_int_debug_disconnect_msg
case A_int_debug_disconnect_msg:
if (hostdata->options & (OPTION_DEBUG_SCRIPT|OPTION_DEBUG_INTR)) {
if (c)
printk("scsi%d : target %d lun %d disconnecting\n",
host->host_no, c->target, c->lun);
else
printk("scsi%d : unknown target disconnecting\n",
host->host_no);
}
return SPECIFIC_INT_RESTART;
#endif
#ifdef A_int_debug_disconnected
case A_int_debug_disconnected:
if (hostdata->options & (OPTION_DEBUG_SCRIPT|OPTION_DEBUG_INTR|
OPTION_DEBUG_DISCONNECT)) {
printk ("scsi%d : disconnected, new queues are\n",
host->host_no);
print_queues(host);
#if 0
printk ("scsi%d : sxfer=0x%x, scntl3=0x%x\n",
host->host_no, NCR53c7x0_read8(SXFER_REG),
NCR53c7x0_read8(SCNTL3_REG_800));
#endif
if (c) {
print_insn (host, (u32 *)
hostdata->sync[c->target].script, "", 1);
print_insn (host, (u32 *)
hostdata->sync[c->target].script + 2, "", 1);
}
}
return SPECIFIC_INT_RESTART;
#endif
#ifdef A_int_debug_panic
case A_int_debug_panic:
printk("scsi%d : int_debug_panic received\n", host->host_no);
print_lots (host);
return SPECIFIC_INT_PANIC;
#endif
#ifdef A_int_debug_saved
case A_int_debug_saved:
if (hostdata->options & (OPTION_DEBUG_SCRIPT|OPTION_DEBUG_INTR|
OPTION_DEBUG_DISCONNECT)) {
printk ("scsi%d : saved data pointer 0x%x (virt 0x%p)\n",
host->host_no, cmd->saved_data_pointer,
bus_to_virt (cmd->saved_data_pointer));
print_progress (c);
}
return SPECIFIC_INT_RESTART;
#endif
#ifdef A_int_debug_restored
case A_int_debug_restored:
if (hostdata->options & (OPTION_DEBUG_SCRIPT|OPTION_DEBUG_INTR|
OPTION_DEBUG_DISCONNECT)) {
if (cmd) {
int size;
printk ("scsi%d : restored data pointer 0x%x (virt 0x%p)\n",
host->host_no, cmd->saved_data_pointer, bus_to_virt (
cmd->saved_data_pointer));
size = print_insn (host, (u32 *)
bus_to_virt(cmd->saved_data_pointer), "", 1);
size = print_insn (host, (u32 *)
bus_to_virt(cmd->saved_data_pointer) + size, "", 1);
print_progress (c);
}
#if 0
printk ("scsi%d : datapath residual %d\n",
host->host_no, datapath_residual (host)) ;
#endif
}
return SPECIFIC_INT_RESTART;
#endif
#ifdef A_int_debug_sync
case A_int_debug_sync:
if (hostdata->options & (OPTION_DEBUG_SCRIPT|OPTION_DEBUG_INTR|
OPTION_DEBUG_DISCONNECT|OPTION_DEBUG_SDTR)) {
unsigned char sxfer = NCR53c7x0_read8 (SXFER_REG),
scntl3 = NCR53c7x0_read8 (SCNTL3_REG_800);
if (c) {
if (sxfer != hostdata->sync[c->target].sxfer_sanity ||
scntl3 != hostdata->sync[c->target].scntl3_sanity) {
printk ("scsi%d :  sync sanity check failed sxfer=0x%x, scntl3=0x%x",
host->host_no, sxfer, scntl3);
NCR53c7x0_write8 (SXFER_REG, sxfer);
NCR53c7x0_write8 (SCNTL3_REG_800, scntl3);
}
} else
printk ("scsi%d : unknown command sxfer=0x%x, scntl3=0x%x\n",
host->host_no, (int) sxfer, (int) scntl3);
}
return SPECIFIC_INT_RESTART;
#endif
#ifdef A_int_debug_datain
case A_int_debug_datain:
if (hostdata->options & (OPTION_DEBUG_SCRIPT|OPTION_DEBUG_INTR|
OPTION_DEBUG_DISCONNECT|OPTION_DEBUG_SDTR)) {
int size;
printk ("scsi%d : In do_datain (%s) sxfer=0x%x, scntl3=0x%x\n"
"         datapath residual=%d\n",
host->host_no, sbcl_to_phase (NCR53c7x0_read8 (SBCL_REG)),
(int) NCR53c7x0_read8(SXFER_REG),
(int) NCR53c7x0_read8(SCNTL3_REG_800),
datapath_residual (host)) ;
print_insn (host, dsp, "", 1);
size = print_insn (host, (u32 *) bus_to_virt(dsp[1]), "", 1);
print_insn (host, (u32 *) bus_to_virt(dsp[1]) + size, "", 1);
}
return SPECIFIC_INT_RESTART;
#endif
#ifdef A_int_debug_check_dsa
case A_int_debug_check_dsa:
if (NCR53c7x0_read8 (SCNTL1_REG) & SCNTL1_CON) {
int sdid = NCR53c7x0_read8 (SDID_REG_800) & 15;
char *where = dsp - NCR53c7x0_insn_size(NCR53c7x0_read8
(DCMD_REG)) == hostdata->script +
Ent_select_check_dsa / sizeof(u32) ?
"selection" : "reselection";
if (c && sdid != c->target) {
printk ("scsi%d : SDID target %d != DSA target %d at %s\n",
host->host_no, sdid, c->target, where);
print_lots(host);
dump_events (host, 20);
return SPECIFIC_INT_PANIC;
}
}
return SPECIFIC_INT_RESTART;
#endif
default:
if ((dsps & 0xff000000) == 0x03000000) {
printk ("scsi%d : misc debug interrupt 0x%x\n",
host->host_no, dsps);
return SPECIFIC_INT_RESTART;
} else if ((dsps & 0xff000000) == 0x05000000) {
if (hostdata->events) {
struct NCR53c7x0_event *event;
++hostdata->event_index;
if (hostdata->event_index >= hostdata->event_size)
hostdata->event_index = 0;
event = (struct NCR53c7x0_event *) hostdata->events +
hostdata->event_index;
event->event = (enum ncr_event) dsps;
event->dsa = bus_to_virt(NCR53c7x0_read32(DSA_REG));
if (NCR53c7x0_read8 (SCNTL1_REG) & SCNTL1_CON)
event->target = NCR53c7x0_read8(SSID_REG_800);
else
event->target = 255;
if (event->event == EVENT_RESELECT)
event->lun = hostdata->reselected_identify & 0xf;
else if (c)
event->lun = c->lun;
else
event->lun = 255;
do_gettimeofday(&(event->time));
if (c) {
event->pid = c->pid;
memcpy ((void *) event->cmnd, (void *) c->cmnd,
sizeof (event->cmnd));
} else {
event->pid = -1;
}
}
return SPECIFIC_INT_RESTART;
}
printk ("scsi%d : unknown user interrupt 0x%x\n",
host->host_no, (unsigned) dsps);
return SPECIFIC_INT_PANIC;
}
}
#include "53c8xx_u.h"
#ifdef NCR_DEBUG
static const char debugger_help =
"bc <addr> 			- clear breakpoint\n"
"bl				- list breakpoints\n"
"bs <addr>			- set breakpoint\n"
"g				- start\n"
"h				- halt\n"
"?				- this message\n"
"i				- info\n"
"mp <addr> <size> 		- print memory\n"
"ms <addr> <size> <value>	- store memory\n"
"rp <num> <size>		- print register\n"
"rs <num> <size> <value> 	- store register\n"
"s                              - single step\n"
"tb				- begin trace \n"
"te				- end trace\n";
static int debugger_fn_bc (struct Scsi_Host *host, struct debugger_token *token,
u32 args[]) {
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
instance->hostdata;
struct NCR53c7x0_break *bp, **prev;
unsigned long flags;
save_flags(flags);
cli();
for (bp = (struct NCR53c7x0_break *) instance->breakpoints,
prev = (struct NCR53c7x0_break **) &instance->breakpoints;
bp; prev = (struct NCR53c7x0_break **) &(bp->next),
bp = (struct NCR53c7x0_break *) bp->next);
if (!bp) {
restore_flags(flags);
return -EIO;
}
memcpy ((void *) bp->addr, (void *) bp->old, sizeof(bp->old));
if (prev)
*prev = bp->next;
restore_flags(flags);
return 0;
}
static int
debugger_fn_bl (struct Scsi_Host *host, struct debugger_token *token,
u32 args[]) {
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
struct NCR53c7x0_break *bp;
char buf[80];
size_t len;
unsigned long flags;
sprintf (buf, "scsi%d : bp : warning : processor not halted\b",
host->host_no);
debugger_kernel_write (host, buf, strlen(buf));
save_flags(flags);
cli();
for (bp = (struct NCR53c7x0_break *) host->breakpoints;
bp; bp = (struct NCR53c7x0_break *) bp->next); {
sprintf (buf, "scsi%d : bp : success : at %08x, replaces %08x %08x",
bp->addr, bp->old[0], bp->old[1]);
len = strlen(buf);
if ((bp->old[0] & (DCMD_TYPE_MASK << 24)) ==
(DCMD_TYPE_MMI << 24)) {
sprintf(buf + len, "%08x\n", * (u32 *) bp->addr);
} else {
sprintf(buf + len, "\n");
}
len = strlen(buf);
debugger_kernel_write (host, buf, len);
}
restore_flags(flags);
return 0;
}
static int
debugger_fn_bs (struct Scsi_Host *host, struct debugger_token *token,
u32 args[]) {
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
struct NCR53c7x0_break *bp;
char buf[80];
size_t len;
unsigned long flags;
save_flags(flags);
cli();
if (hostdata->state != STATE_HALTED) {
sprintf (buf, "scsi%d : bs : failure : NCR not halted\n", host->host_no);
debugger_kernel_write (host, buf, strlen(buf));
restore_flags(flags);
return -1;
}
if (!(bp = kmalloc (sizeof (struct NCR53c7x0_break)))) {
printk ("scsi%d : kmalloc(%d) of breakpoint structure failed, try again\n",
host->host_no, sizeof(struct NCR53c7x0_break));
restore_flags(flags);
return -1;
}
bp->address = (u32 *) args[0];
memcpy ((void *) bp->old_instruction, (void *) bp->address, 8);
bp->old_size = (((bp->old_instruction[0] >> 24) & DCMD_TYPE_MASK) ==
DCMD_TYPE_MMI ? 3 : 2;
bp->next = hostdata->breakpoints;
hostdata->breakpoints = bp->next;
memcpy ((void *) bp->address, (void *) hostdata->E_debug_break, 8);
restore_flags(flags);
return 0;
}
#define TOKEN(name,nargs) {#name, nargs, debugger_fn_##name}
static const struct debugger_token {
char *name;
int numargs;
int (*fn)(struct debugger_token *token, u32 args[]);
} debugger_tokens[] = {
TOKEN(bc,1), TOKEN(bl,0), TOKEN(bs,1), TOKEN(g,0), TOKEN(halt,0),
{DT_help, "?", 0} , TOKEN(h,0), TOKEN(i,0), TOKEN(mp,2),
TOKEN(ms,3), TOKEN(rp,2), TOKEN(rs,2), TOKEN(s,0), TOKEN(tb,0), TOKEN(te,0)
};
#define NDT sizeof(debugger_tokens / sizeof(struct debugger_token))
static struct Scsi_Host * inode_to_host (struct inode *inode) {
int dev;
struct Scsi_Host *tmp;
for (dev = MINOR(inode->rdev), host = first_host;
(host->hostt == the_template); --dev, host = host->next)
if (!dev) return host;
return NULL;
}
static int
debugger_user_write (struct inode *inode,struct file *filp,
char *buf,int count) {
struct Scsi_Host *host;
struct NCR53c7x0_hostadata *hostdata;
char input_buf[80],
*ptr;
u32 args[3];
int i, j, error, len;
if (!(host = inode_to_host(inode)))
return -ENXIO;
hostdata = (struct NCR53c7x0_hostdata *) host->hostdata;
if (error = verify_area(VERIFY_READ,buf,count))
return error;
if (count > 80)
return -EIO;
memcpy_from_fs(input_buf, buf, count);
if (input_buf[count - 1] != '\n')
return -EIO;
input_buf[count - 1]=0;
for (i = 0; i < NDT; ++i) {
len = strlen (debugger_tokens[i].name);
if (!strncmp(input_buf, debugger_tokens[i].name, len))
break;
};
if (i == NDT)
return -EIO;
for (ptr = input_buf + len, j = 0; j < debugger_tokens[i].nargs && *ptr;) {
if (*ptr == ' ' || *ptr == '\t') {
++ptr;
} else if (isdigit(*ptr)) {
args[j++] = simple_strtoul (ptr, &ptr, 0);
} else {
return -EIO;
}
}
if (j != debugger_tokens[i].nargs)
return -EIO;
return count;
}
static int
debugger_user_read (struct inode *inode,struct file *filp,
char *buf,int count) {
struct Scsi_Host *instance;
}
static int
debugger_kernel_write (struct Scsi_Host *host, char *buf, size_t
buflen) {
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
int copy, left;
unsigned long flags;
save_flags(flags);
cli();
while (buflen) {
left = (hostdata->debug_buf + hostdata->debug_size - 1) -
hostdata->debug_write;
copy = (buflen <= left) ? buflen : left;
memcpy (hostdata->debug_write, buf, copy);
buf += copy;
buflen -= copy;
hostdata->debug_count += copy;
if ((hostdata->debug_write += copy) ==
(hostdata->debug_buf + hostdata->debug_size))
hosdata->debug_write = hostdata->debug_buf;
}
restore_flags(flags);
}
#endif
static void
NCR53c8x0_soft_reset (struct Scsi_Host *host) {
NCR53c7x0_local_declare();
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
NCR53c7x0_local_setup(host);
NCR53c7x0_write8(ISTAT_REG_800, ISTAT_10_SRST);
NCR53c7x0_write8(ISTAT_REG_800, 0);
NCR53c7x0_write8(hostdata->dmode, hostdata->saved_dmode & ~DMODE_MAN);
#ifdef notyet
NCR53c7x0_write8(SCID_REG, (host->this_id & 7)|SCID_800_RRE|SCID_800_SRE);
#else
NCR53c7x0_write8(SCID_REG, (host->this_id & 7)|SCID_800_RRE);
#endif
NCR53c7x0_write8(RESPID_REG_800, hostdata->this_id_mask);
#if 0
NCR53c7x0_write8(STIME0_REG_800,
((selection_timeout << STIME0_800_SEL_SHIFT) & STIME0_800_SEL_MASK)
| ((15 << STIME0_800_HTH_SHIFT) & STIME0_800_HTH_MASK));
#else
NCR53c7x0_write8(STIME0_REG_800,
((selection_timeout << STIME0_800_SEL_SHIFT) & STIME0_800_SEL_MASK));
#endif
NCR53c7x0_write8(STEST3_REG_800, STEST3_800_TE);
NCR53c7x0_write8(DIEN_REG, DIEN_800_MDPE | DIEN_800_BF |
DIEN_ABRT | DIEN_SSI | DIEN_SIR | DIEN_800_IID);
NCR53c7x0_write8(SIEN0_REG_800, ((hostdata->options & OPTION_PARITY) ?
SIEN_PAR : 0) | SIEN_RST | SIEN_UDC | SIEN_SGE | SIEN_MA);
NCR53c7x0_write8(SIEN1_REG_800, SIEN1_800_STO | SIEN1_800_HTH);
NCR53c7x0_write8(DCNTL_REG, hostdata->saved_dcntl);
NCR53c7x0_write8(CTEST4_REG_800, hostdata->saved_ctest4);
NCR53c7x0_write8(STEST3_REG_800, STEST3_800_TE);
}
static struct NCR53c7x0_cmd *
allocate_cmd (Scsi_Cmnd *cmd) {
struct Scsi_Host *host = cmd->host;
struct NCR53c7x0_hostdata *hostdata =
(struct NCR53c7x0_hostdata *) host->hostdata;
void *real;
int size;
struct NCR53c7x0_cmd *tmp;
unsigned long flags;
if (hostdata->options & OPTION_DEBUG_ALLOCATION)
printk ("scsi%d : num_cmds = %d, can_queue = %d\n"
"         target = %d, lun = %d, %s\n",
host->host_no, hostdata->num_cmds, host->can_queue,
cmd->target, cmd->lun, (hostdata->cmd_allocated[cmd->target] &
(1 << cmd->lun)) ? "already allocated" : "not allocated");
if (!(hostdata->cmd_allocated[cmd->target] & (1 << cmd->lun)) &&
#ifdef LINUX_1_2
!in_scan_scsis
#else
cmd->device && cmd->device->has_cmdblocks
#endif
) {
if ((hostdata->extra_allocate + hostdata->num_cmds) < host->can_queue)
hostdata->extra_allocate += host->cmd_per_lun;
hostdata->cmd_allocated[cmd->target] |= (1 << cmd->lun);
}
for (; hostdata->extra_allocate > 0 ; --hostdata->extra_allocate,
++hostdata->num_cmds) {
size = hostdata->max_cmd_size + sizeof (void *);
real = kmalloc (size, GFP_ATOMIC);
if (!real) {
if (hostdata->options & OPTION_DEBUG_ALLOCATION)
printk ("scsi%d : kmalloc(%d) failed\n",
host->host_no, size);
break;
}
tmp = ROUNDUP(real, void *);
tmp->real = real;
tmp->size = size;
#ifdef LINUX_1_2
tmp->free = ((void (*)(void *, int)) kfree_s);
#else
tmp->free = ((void (*)(void *, int)) kfree);
#endif
save_flags (flags);
cli();
tmp->next = hostdata->free;
hostdata->free = tmp;
restore_flags (flags);
}
save_flags(flags);
cli();
tmp = (struct NCR53c7x0_cmd *) hostdata->free;
if (tmp) {
hostdata->free = tmp->next;
}
restore_flags(flags);
if (!tmp)
printk ("scsi%d : can't allocate command for target %d lun %d\n",
host->host_no, cmd->target, cmd->lun);
return tmp;
}
static struct NCR53c7x0_cmd *
create_cmd (Scsi_Cmnd *cmd) {
NCR53c7x0_local_declare();
struct Scsi_Host *host = cmd->host;
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
struct NCR53c7x0_cmd *tmp;
int datain,
dataout;
int data_transfer_instructions,
i;
u32 *cmd_datain,
*cmd_dataout;
#ifdef notyet
unsigned char *msgptr;
int msglen;
#endif
unsigned long flags;
NCR53c7x0_local_setup(cmd->host);
if (!(tmp = allocate_cmd (cmd)))
return NULL;
switch (cmd->cmnd[0]) {
case INQUIRY:
case MODE_SENSE:
case READ_6:
case READ_10:
case READ_CAPACITY:
case REQUEST_SENSE:
datain = 2 * (cmd->use_sg ? cmd->use_sg : 1) + 3;
dataout = 0;
break;
case MODE_SELECT:
case WRITE_6:
case WRITE_10:
case START_STOP:
#if 0
printk("scsi%d : command is ", host->host_no);
print_command(cmd->cmnd);
#endif
#if 0
printk ("scsi%d : %d scatter/gather segments\n", host->host_no,
cmd->use_sg);
#endif
datain = 0;
dataout = 2 * (cmd->use_sg ? cmd->use_sg : 1) + 3;
#if 0
hostdata->options |= OPTION_DEBUG_INTR;
#endif
break;
case TEST_UNIT_READY:
datain = dataout = 0;
break;
default:
datain = dataout = 2 * (cmd->use_sg ? cmd->use_sg : 1) + 3;
}
data_transfer_instructions = datain + dataout;
if (data_transfer_instructions < 2)
data_transfer_instructions = 2;
tmp->saved_data_pointer = virt_to_bus (hostdata->script) +
hostdata->E_data_transfer;
tmp->cmd = cmd;
tmp->next = NULL;
tmp->flags = 0;
tmp->dsa_next_addr = virt_to_bus(tmp->dsa) + hostdata->dsa_next -
hostdata->dsa_start;
tmp->dsa_addr = virt_to_bus(tmp->dsa) - hostdata->dsa_start;
tmp->data_transfer_start = tmp->dsa + (hostdata->dsa_end -
hostdata->dsa_start) / sizeof(u32);
tmp->data_transfer_end = tmp->data_transfer_start +
2 * data_transfer_instructions;
cmd_datain = datain ? tmp->data_transfer_start : NULL;
cmd_dataout = dataout ? (datain ? cmd_datain + 2 * datain : tmp->
data_transfer_start) : NULL;
if (hostdata->dsa_fixup)
hostdata->dsa_fixup(tmp);
patch_dsa_32(tmp->dsa, dsa_next, 0, 0);
patch_dsa_32(tmp->dsa, dsa_cmnd, 0, virt_to_bus(cmd));
if (hostdata->options & OPTION_DEBUG_SYNCHRONOUS)
if (hostdata->sync[cmd->target].select_indirect !=
((hostdata->sync[cmd->target].scntl3_sanity << 24) |
(cmd->target << 16) |
(hostdata->sync[cmd->target].sxfer_sanity << 8))) {
printk ("scsi%d :  sanity check failed select_indirect=0x%x\n",
host->host_no, hostdata->sync[cmd->target].select_indirect);
FATAL(host);
}
patch_dsa_32(tmp->dsa, dsa_select, 0, hostdata->sync[cmd->target].
select_indirect);
if (hostdata->initiate_wdtr & (1 << cmd->target)) {
memcpy ((void *) (tmp->select + 1), (void *) wdtr_message,
sizeof(wdtr_message));
patch_dsa_32(tmp->dsa, dsa_msgout, 0, 1 + sizeof(wdtr_message));
save_flags(flags);
cli();
hostdata->initiate_wdtr &= ~(1 << cmd->target);
restore_flags(flags);
} else if (hostdata->initiate_sdtr & (1 << cmd->target)) {
memcpy ((void *) (tmp->select + 1), (void *) sdtr_message,
sizeof(sdtr_message));
patch_dsa_32(tmp->dsa, dsa_msgout, 0, 1 + sizeof(sdtr_message));
tmp->flags |= CMD_FLAG_SDTR;
save_flags(flags);
cli();
hostdata->initiate_sdtr &= ~(1 << cmd->target);
restore_flags(flags);
}
#if 1
else if (!(hostdata->talked_to & (1 << cmd->target)) &&
!(hostdata->options & OPTION_NO_ASYNC)) {
memcpy ((void *) (tmp->select + 1), (void *) async_message,
sizeof(async_message));
patch_dsa_32(tmp->dsa, dsa_msgout, 0, 1 + sizeof(async_message));
tmp->flags |= CMD_FLAG_SDTR;
}
#endif
else
patch_dsa_32(tmp->dsa, dsa_msgout, 0, 1);
hostdata->talked_to |= (1 << cmd->target);
tmp->select[0] = (hostdata->options & OPTION_DISCONNECT) ?
IDENTIFY (1, cmd->lun) : IDENTIFY (0, cmd->lun);
patch_dsa_32(tmp->dsa, dsa_msgout, 1, virt_to_bus(tmp->select));
patch_dsa_32(tmp->dsa, dsa_cmdout, 0, cmd->cmd_len);
patch_dsa_32(tmp->dsa, dsa_cmdout, 1, virt_to_bus(cmd->cmnd));
patch_dsa_32(tmp->dsa, dsa_dataout, 0, cmd_dataout ?
virt_to_bus (cmd_dataout)
: virt_to_bus (hostdata->script) + hostdata->E_other_transfer);
patch_dsa_32(tmp->dsa, dsa_datain, 0, cmd_datain ?
virt_to_bus (cmd_datain)
: virt_to_bus (hostdata->script) + hostdata->E_other_transfer);
patch_dsa_32(tmp->dsa, dsa_msgin, 0, 1);
patch_dsa_32(tmp->dsa, dsa_msgin, 1, virt_to_bus(&cmd->result) + 1);
patch_dsa_32(tmp->dsa, dsa_status, 0, 1);
patch_dsa_32(tmp->dsa, dsa_status, 1, virt_to_bus(&cmd->result));
patch_dsa_32(tmp->dsa, dsa_msgout_other, 0, 1);
patch_dsa_32(tmp->dsa, dsa_msgout_other, 1,
virt_to_bus(&(hostdata->NCR53c7xx_msg_nop)));
#if 0
if (datain) {
cmd_datain[0] = 0x98080000;
cmd_datain[1] = 0x03ffd00d;
cmd_datain += 2;
}
#endif
for (i = 0; cmd->use_sg ? (i < cmd->use_sg) : !i; cmd_datain += 4,
cmd_dataout += 4, ++i) {
u32 buf = cmd->use_sg ?
virt_to_bus(((struct scatterlist *)cmd->buffer)[i].address) :
virt_to_bus(cmd->request_buffer);
u32 count = cmd->use_sg ?
((struct scatterlist *)cmd->buffer)[i].length :
cmd->request_bufflen;
if (datain) {
cmd_datain[0] = ((DCMD_TYPE_TCI | DCMD_TCI_OP_CALL |
DCMD_TCI_IO) << 24) |
DBC_TCI_WAIT_FOR_VALID | DBC_TCI_COMPARE_PHASE;
cmd_datain[1] = virt_to_bus (hostdata->script) +
hostdata->E_other_in;
cmd_datain[2] = ((DCMD_TYPE_BMI | DCMD_BMI_OP_MOVE_I | DCMD_BMI_IO)
<< 24) | count;
cmd_datain[3] = buf;
#if 0
print_insn (host, cmd_datain, "dynamic ", 1);
print_insn (host, cmd_datain + 2, "dynamic ", 1);
#endif
}
if (dataout) {
cmd_dataout[0] = ((DCMD_TYPE_TCI | DCMD_TCI_OP_CALL) << 24) |
DBC_TCI_WAIT_FOR_VALID | DBC_TCI_COMPARE_PHASE;
cmd_dataout[1] = virt_to_bus(hostdata->script) +
hostdata->E_other_out;
cmd_dataout[2] = ((DCMD_TYPE_BMI | DCMD_BMI_OP_MOVE_I) << 24)
| count;
cmd_dataout[3] = buf;
#if 0
print_insn (host, cmd_dataout, "dynamic ", 1);
print_insn (host, cmd_dataout + 2, "dynamic ", 1);
#endif
}
}
if (datain) {
cmd_datain[0] = ((DCMD_TYPE_TCI | DCMD_TCI_OP_JUMP) << 24) |
DBC_TCI_TRUE;
cmd_datain[1] = virt_to_bus(hostdata->script) +
hostdata->E_other_transfer;
#if 0
print_insn (host, cmd_datain, "dynamic jump ", 1);
#endif
cmd_datain += 2;
}
#if 0
if (datain) {
cmd_datain[0] = 0x98080000;
cmd_datain[1] = 0x03ffdeed;
cmd_datain += 2;
}
#endif
if (dataout) {
cmd_dataout[0] = ((DCMD_TYPE_TCI | DCMD_TCI_OP_JUMP) << 24) |
DBC_TCI_TRUE;
cmd_dataout[1] = virt_to_bus(hostdata->script) +
hostdata->E_other_transfer;
#if 0
print_insn (host, cmd_dataout, "dynamic jump ", 1);
#endif
cmd_dataout += 2;
}
return tmp;
}
int
NCR53c7xx_queue_command (Scsi_Cmnd *cmd, void (* done)(Scsi_Cmnd *)) {
struct Scsi_Host *host = cmd->host;
struct NCR53c7x0_hostdata *hostdata =
(struct NCR53c7x0_hostdata *) host->hostdata;
unsigned long flags;
Scsi_Cmnd *tmp;
cmd->scsi_done = done;
cmd->host_scribble = NULL;
cmd->SCp.ptr = NULL;
cmd->SCp.buffer = NULL;
save_flags(flags);
cli();
if ((hostdata->options & (OPTION_DEBUG_INIT_ONLY|OPTION_DEBUG_PROBE_ONLY))
|| ((hostdata->options & OPTION_DEBUG_TARGET_LIMIT) &&
!(hostdata->debug_lun_limit[cmd->target] & (1 << cmd->lun)))
#ifdef LINUX_1_2
|| cmd->target > 7
#else
|| cmd->target > host->max_id
#endif
|| cmd->target == host->this_id
|| hostdata->state == STATE_DISABLED) {
printk("scsi%d : disabled or bad target %d lun %d\n", host->host_no,
cmd->target, cmd->lun);
cmd->result = (DID_BAD_TARGET << 16);
} else if ((hostdata->options & OPTION_DEBUG_NCOMMANDS_LIMIT) &&
(hostdata->debug_count_limit == 0)) {
printk("scsi%d : maximum commands exceeded\n", host->host_no);
cmd->result = (DID_BAD_TARGET << 16);
cmd->result = (DID_BAD_TARGET << 16);
} else if (hostdata->options & OPTION_DEBUG_READ_ONLY) {
switch (cmd->cmnd[0]) {
case WRITE_6:
case WRITE_10:
printk("scsi%d : WRITE attempted with NO_WRITE debugging flag set\n",
host->host_no);
cmd->result = (DID_BAD_TARGET << 16);
}
} else {
if ((hostdata->options & OPTION_DEBUG_TARGET_LIMIT) &&
hostdata->debug_count_limit != -1)
--hostdata->debug_count_limit;
restore_flags (flags);
cmd->result = 0xffff;
cmd->host_scribble = (unsigned char *) (tmp = create_cmd (cmd));
}
cli();
if (!(hostdata->issue_queue) || (cmd->cmnd[0] == REQUEST_SENSE)) {
cmd->SCp.ptr = (unsigned char *) hostdata->issue_queue;
hostdata->issue_queue = cmd;
} else {
for (tmp = (Scsi_Cmnd *) hostdata->issue_queue; tmp->SCp.ptr;
tmp = (Scsi_Cmnd *) tmp->SCp.ptr);
tmp->SCp.ptr = (unsigned char *) cmd;
}
restore_flags (flags);
run_process_issue_queue();
return 0;
}
static __inline__ void
to_schedule_list (struct Scsi_Host *host, struct NCR53c7x0_hostdata *hostdata,
struct NCR53c7x0_cmd *cmd) {
NCR53c7x0_local_declare();
Scsi_Cmnd *tmp = cmd->cmd;
unsigned long flags;
volatile u32 *current;
int i;
NCR53c7x0_local_setup(host);
#if 0
printk("scsi%d : new dsa is 0x%lx (virt 0x%p)\n", host->host_no,
virt_to_bus(dsa), dsa);
#endif
save_flags(flags);
cli();
if (hostdata->state == STATE_DISABLED) {
printk("scsi%d : driver disabled\n", host->host_no);
tmp->result = (DID_BAD_TARGET << 16);
cmd->next = (struct NCR53c7x0_cmd *) hostdata->free;
hostdata->free = cmd;
tmp->scsi_done(tmp);
restore_flags (flags);
return;
}
for (i = host->can_queue, current = hostdata->schedule;
i > 0 && current[0] != hostdata->NOP_insn;
--i, current += 2 );
if (i > 0) {
++hostdata->busy[tmp->target][tmp->lun];
cmd->next = hostdata->running_list;
hostdata->running_list = cmd;
cmd->dsa [(hostdata->dsa_jump_dest - hostdata->dsa_start) /
sizeof(u32)] = (u32) virt_to_bus ((void *)current);
current[1] =
virt_to_bus ((void *) cmd->dsa) + hostdata->E_dsa_code_begin -
hostdata->E_dsa_code_template;
current[0] = ((DCMD_TYPE_TCI|DCMD_TCI_OP_JUMP) << 24) |
DBC_TCI_TRUE;
} else {
printk ("scsi%d: no free slot\n", host->host_no);
disable(host);
tmp->result = (DID_ERROR << 16);
cmd->next = (struct NCR53c7x0_cmd *) hostdata->free;
hostdata->free = cmd;
tmp->scsi_done(tmp);
restore_flags (flags);
return;
}
if (hostdata->idle) {
hostdata->idle = 0;
hostdata->state = STATE_RUNNING;
NCR53c7x0_write32 (DSP_REG, virt_to_bus ((void *)hostdata->schedule));
} else {
NCR53c7x0_write8(hostdata->istat, ISTAT_10_SIGP);
}
restore_flags(flags);
}
static __inline__ int
busyp (struct Scsi_Host *host, struct NCR53c7x0_hostdata *hostdata,
Scsi_Cmnd *cmd) {
return hostdata->busy[cmd->target][cmd->lun];
}
static void
process_issue_queue (unsigned long flags) {
Scsi_Cmnd *tmp, *prev;
struct Scsi_Host *host;
struct NCR53c7x0_hostdata *hostdata;
int done;
do {
cli();
done = 1;
for (host = first_host; host && host->hostt == the_template;
host = host->next) {
hostdata = (struct NCR53c7x0_hostdata *) host->hostdata;
cli();
if (hostdata->issue_queue) {
if (hostdata->state == STATE_DISABLED) {
tmp = (Scsi_Cmnd *) hostdata->issue_queue;
hostdata->issue_queue = (Scsi_Cmnd *) tmp->SCp.ptr;
tmp->result = (DID_BAD_TARGET << 16);
if (tmp->host_scribble) {
((struct NCR53c7x0_cmd *)tmp->host_scribble)->next =
hostdata->free;
hostdata->free =
(struct NCR53c7x0_cmd *)tmp->host_scribble;
tmp->host_scribble = NULL;
}
tmp->scsi_done (tmp);
done = 0;
} else
for (tmp = (Scsi_Cmnd *) hostdata->issue_queue,
prev = NULL; tmp; prev = tmp, tmp = (Scsi_Cmnd *)
tmp->SCp.ptr)
if (!tmp->host_scribble ||
!busyp (host, hostdata, tmp)) {
if (prev)
prev->SCp.ptr = tmp->SCp.ptr;
else
hostdata->issue_queue = (Scsi_Cmnd *)
tmp->SCp.ptr;
tmp->SCp.ptr = NULL;
if (tmp->host_scribble) {
if (hostdata->options & OPTION_DEBUG_QUEUES)
printk ("scsi%d : moving command for target %d lun %d to start list\n",
host->host_no, tmp->target, tmp->lun);
to_schedule_list (host, hostdata,
(struct NCR53c7x0_cmd *)
tmp->host_scribble);
} else {
if (((tmp->result & 0xff) == 0xff) ||
((tmp->result & 0xff00) == 0xff00)) {
printk ("scsi%d : danger Will Robinson!\n",
host->host_no);
tmp->result = DID_ERROR << 16;
disable (host);
}
tmp->scsi_done(tmp);
}
done = 0;
}
}
if (!done)
restore_flags (flags);
}
} while (!done);
process_issue_queue_running = 0;
}
static void
intr_scsi (struct Scsi_Host *host, struct NCR53c7x0_cmd *cmd) {
NCR53c7x0_local_declare();
struct NCR53c7x0_hostdata *hostdata =
(struct NCR53c7x0_hostdata *) host->hostdata;
unsigned char sstat0_sist0, sist1,
fatal;
int is_8xx_chip;
NCR53c7x0_local_setup(host);
fatal = 0;
is_8xx_chip = ((unsigned) (hostdata->chip - 800)) < 100;
if (is_8xx_chip) {
sstat0_sist0 = NCR53c7x0_read8(SIST0_REG_800);
udelay(1);
sist1 = NCR53c7x0_read8(SIST1_REG_800);
} else {
sstat0_sist0 = NCR53c7x0_read8(SSTAT0_REG);
sist1 = 0;
}
if (hostdata->options & OPTION_DEBUG_INTR)
printk ("scsi%d : SIST0 0x%0x, SIST1 0x%0x\n", host->host_no,
sstat0_sist0, sist1);
if ((is_8xx_chip && (sist1 & SIST1_800_STO)) ||
(!is_8xx_chip && (sstat0_sist0 & SSTAT0_700_STO))) {
fatal = 1;
if (hostdata->options & OPTION_DEBUG_INTR) {
printk ("scsi%d : Selection Timeout\n", host->host_no);
if (cmd) {
printk("scsi%d : target %d, lun %d, command ",
host->host_no, cmd->cmd->target, cmd->cmd->lun);
print_command (cmd->cmd->cmnd);
printk("scsi%d : dsp = 0x%x (virt 0x%p)\n", host->host_no,
NCR53c7x0_read32(DSP_REG),
bus_to_virt(NCR53c7x0_read32(DSP_REG)));
} else {
printk("scsi%d : no command\n", host->host_no);
}
}
if (1) {
hostdata->idle = 1;
hostdata->expecting_sto = 0;
if (hostdata->test_running) {
hostdata->test_running = 0;
hostdata->test_completed = 3;
} else if (cmd) {
abnormal_finished(cmd, DID_BAD_TARGET << 16);
}
#if 0
hostdata->intrs = 0;
#endif
}
}
if (sstat0_sist0 & SSTAT0_UDC) {
fatal = 1;
if (cmd) {
printk("scsi%d : target %d lun %d unexpected disconnect\n",
host->host_no, cmd->cmd->target, cmd->cmd->lun);
print_lots (host);
abnormal_finished(cmd, DID_ERROR << 16);
} else
printk("scsi%d : unexpected disconnect (no command)\n",
host->host_no);
hostdata->dsp = (u32 *) hostdata->schedule;
hostdata->dsp_changed = 1;
}
if (sstat0_sist0 & SSTAT0_PAR) {
fatal = 1;
if (cmd && cmd->cmd) {
printk("scsi%d : target %d lun %d parity error.\n",
host->host_no, cmd->cmd->target, cmd->cmd->lun);
abnormal_finished (cmd, DID_PARITY << 16);
} else
printk("scsi%d : parity error\n", host->host_no);
hostdata->dsp = hostdata->script + hostdata->E_initiator_abort /
sizeof(u32);
hostdata->dsp_changed = 1;
}
if (sstat0_sist0 & SSTAT0_SGE) {
fatal = 1;
printk("scsi%d : gross error\n", host->host_no);
if ((hostdata->chip / 100) == 8) {
NCR53c7x0_write8 (STEST2_REG_800, STEST2_800_ROF);
}
hostdata->dsp = hostdata->script + hostdata->E_initiator_abort /
sizeof(u32);
hostdata->dsp_changed = 1;
}
if (sstat0_sist0 & SSTAT0_MA) {
fatal = 1;
if (hostdata->options & OPTION_DEBUG_INTR)
printk ("scsi%d : SSTAT0_MA\n", host->host_no);
intr_phase_mismatch (host, cmd);
}
#if 0
if (sstat0_sist0 & SIST0_800_RSL)
printk ("scsi%d : Oh no Mr. Bill!\n", host->host_no);
#endif
if (fatal) {
if (!hostdata->dstat_valid) {
hostdata->dstat = NCR53c7x0_read8(DSTAT_REG);
hostdata->dstat_valid = 1;
}
if (!(hostdata->dstat & DSTAT_DFE)) {
printk ("scsi%d : DMA FIFO not empty\n", host->host_no);
if (NCR53c7x0_read8 (CTEST2_REG_800) & CTEST2_800_DDIR) {
printk ("scsi%d: Flushing DMA FIFO\n",
host->host_no);
NCR53c7x0_write8 (CTEST3_REG_800, CTEST3_800_FLF);
while (!((hostdata->dstat = NCR53c7x0_read8(DSTAT_REG)) &
DSTAT_DFE));
} else {
NCR53c7x0_write8 (CTEST3_REG_800, CTEST3_800_CLF);
while (NCR53c7x0_read8 (CTEST3_REG_800) & CTEST3_800_CLF);
}
hostdata->dstat |= DSTAT_DFE;
}
}
}
static void
NCR53c7x0_intr (int irq, void *dev_id, struct pt_regs * regs) {
NCR53c7x0_local_declare();
struct Scsi_Host *host;
unsigned char istat;
struct NCR53c7x0_hostdata *hostdata;
struct NCR53c7x0_cmd *cmd,
**cmd_prev_ptr;
u32 *dsa;
int done = 1;
int interrupted = 0;
int have_intfly;
unsigned long flags;
#ifdef NCR_DEBUG
char buf[80];
size_t buflen;
#endif
do {
done = 1;
for (host = first_host; host; host = host->next)
if (host->hostt == the_template && host->irq == irq) {
NCR53c7x0_local_setup(host);
hostdata = (struct NCR53c7x0_hostdata *) host->hostdata;
hostdata->dsp_changed = 0;
interrupted = 0;
have_intfly = 0;
do {
int is_8xx_chip;
hostdata->dstat_valid = 0;
interrupted = 0;
istat = NCR53c7x0_read8(hostdata->istat);
is_8xx_chip = ((unsigned) (hostdata->chip - 800)) < 100;
if ((hostdata->options & OPTION_INTFLY) &&
(is_8xx_chip && (istat & ISTAT_800_INTF))) {
char search_found = 0;
done = 0;
interrupted = 1;
NCR53c7x0_write8(hostdata->istat, istat|ISTAT_800_INTF);
if (hostdata->options & OPTION_DEBUG_INTR)
printk ("scsi%d : INTFLY\n", host->host_no);
save_flags(flags);
cli();
restart:
for (cmd_prev_ptr = (struct NCR53c7x0_cmd **)
&(hostdata->running_list), cmd =
(struct NCR53c7x0_cmd *) hostdata->running_list; cmd ;
cmd_prev_ptr = (struct NCR53c7x0_cmd **) &(cmd->next),
cmd = (struct NCR53c7x0_cmd *) cmd->next) {
Scsi_Cmnd *tmp;
if (!cmd) {
printk("scsi%d : very weird.\n", host->host_no);
break;
}
if (!(tmp = cmd->cmd)) {
printk("scsi%d : weird.  NCR53c7x0_cmd has no Scsi_Cmnd\n",
host->host_no);
continue;
}
#if 0
printk ("scsi%d : looking at result of 0x%x\n",
host->host_no, cmd->cmd->result);
#endif
if (((tmp->result & 0xff) == 0xff) ||
((tmp->result & 0xff00) == 0xff00))
continue;
search_found = 1;
if (cmd_prev_ptr)
*cmd_prev_ptr = (struct NCR53c7x0_cmd *) cmd->next;
--hostdata->busy[tmp->target][tmp->lun];
cmd->next = hostdata->free;
hostdata->free = cmd;
tmp->host_scribble = NULL;
if (hostdata->options & OPTION_DEBUG_INTR) {
printk ("scsi%d : command complete : pid %lu, id %d,lun %d result 0x%x ",
host->host_no, tmp->pid, tmp->target, tmp->lun, tmp->result);
print_command (tmp->cmnd);
}
#if 0
hostdata->options &= ~OPTION_DEBUG_INTR;
#endif
tmp->scsi_done(tmp);
goto restart;
}
restore_flags(flags);
if (!search_found && !have_intfly) {
printk ("scsi%d : WARNING : INTFLY with no completed commands.\n",
host->host_no);
} else if (!have_intfly) {
have_intfly = 1;
run_process_issue_queue();
}
}
if (istat & (ISTAT_SIP|ISTAT_DIP)) {
done = 0;
interrupted = 1;
hostdata->state = STATE_HALTED;
if (NCR53c7x0_read8 ((hostdata->chip / 100) == 8 ?
SSTAT1_REG : SSTAT2_REG) & SSTAT2_FF_MASK)
printk ("scsi%d : SCSI FIFO not empty\n",
host->host_no);
if (hostdata->options & OPTION_700) {
cmd = (struct NCR53c7x0_cmd *) hostdata->current;
} else {
dsa = bus_to_virt(NCR53c7x0_read32(DSA_REG));
for (cmd = (struct NCR53c7x0_cmd *)
hostdata->running_list; cmd &&
(dsa + (hostdata->dsa_start / sizeof(u32))) !=
cmd->dsa;
cmd = (struct NCR53c7x0_cmd *)(cmd->next));
}
if (hostdata->options & OPTION_DEBUG_INTR) {
if (cmd) {
printk("scsi%d : interrupt for pid %lu, id %d, lun %d ",
host->host_no, cmd->cmd->pid, (int) cmd->cmd->target,
(int) cmd->cmd->lun);
print_command (cmd->cmd->cmnd);
} else {
printk("scsi%d : no active command\n", host->host_no);
}
}
if (istat & ISTAT_SIP) {
if (hostdata->options & OPTION_DEBUG_INTR)
printk ("scsi%d : ISTAT_SIP\n", host->host_no);
intr_scsi (host, cmd);
}
if (istat & ISTAT_DIP) {
if (hostdata->options & OPTION_DEBUG_INTR)
printk ("scsi%d : ISTAT_DIP\n", host->host_no);
intr_dma (host, cmd);
}
if (!hostdata->dstat_valid) {
hostdata->dstat = NCR53c7x0_read8(DSTAT_REG);
hostdata->dstat_valid = 1;
}
if (!(hostdata->dstat & DSTAT_DFE)) {
printk ("scsi%d : DMA FIFO not empty\n", host->host_no);
if (NCR53c7x0_read8 (CTEST2_REG_800) & CTEST2_800_DDIR) {
printk ("scsi%d: Flushing DMA FIFO\n",
host->host_no);
NCR53c7x0_write8 (CTEST3_REG_800, CTEST3_800_FLF);
while (!((hostdata->dstat = NCR53c7x0_read8(DSTAT_REG)) &
DSTAT_DFE));
} else
{
NCR53c7x0_write8 (CTEST3_REG_800, CTEST3_800_CLF);
while (NCR53c7x0_read8 (CTEST3_REG_800) & CTEST3_800_CLF);
}
hostdata->dstat |= DSTAT_DFE;
}
}
} while (interrupted);
if (hostdata->intrs != -1)
hostdata->intrs++;
#if 0
if (hostdata->intrs > 40) {
printk("scsi%d : too many interrupts, halting", host->host_no);
disable(host);
}
#endif
if (!hostdata->idle && hostdata->state == STATE_HALTED) {
if (!hostdata->dsp_changed) {
hostdata->dsp = (u32 *)
bus_to_virt(NCR53c7x0_read32(DSP_REG));
}
#if 0
printk("scsi%d : new dsp is 0x%lx (virt 0x%p)\n",
host->host_no, virt_to_bus(hostdata->dsp), hostdata->dsp);
#endif
hostdata->state = STATE_RUNNING;
NCR53c7x0_write32 (DSP_REG, virt_to_bus(hostdata->dsp));
}
}
} while (!done);
}
static int
abort_connected (struct Scsi_Host *host) {
#ifdef NEW_ABORT
NCR53c7x0_local_declare();
#endif
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
static int counter = 5;
#ifdef NEW_ABORT
int sstat, phase, offset;
u32 *script;
NCR53c7x0_local_setup(host);
#endif
if (--counter <= 0) {
disable(host);
return 0;
}
printk ("scsi%d : DANGER : abort_connected() called \n",
host->host_no);
#ifdef NEW_ABORT
sstat = (NCR53c8x0_read8 ((chip / 100) == 8 ? SSTAT1_REG : SSTAT2_REG);
offset = OFFSET (sstat & SSTAT2_FF_MASK) >> SSTAT2_FF_SHIFT;
phase = sstat & SSTAT2_PHASE_MASK;
script = hostdata->abort_script = kmalloc (
8 * (
1 +
(!offset ? 1 : offset) +
1 ),
GFP_ATOMIC);
#else
hostdata->dsp = hostdata->script + hostdata->E_initiator_abort /
sizeof(u32);
#endif
hostdata->dsp_changed = 1;
return 0;
}
static int
datapath_residual (struct Scsi_Host *host) {
NCR53c7x0_local_declare();
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
int count, synchronous, sstat;
NCR53c7x0_local_setup(host);
count = ((NCR53c7x0_read8 (DFIFO_REG) & DFIFO_10_BO_MASK) -
(NCR53c7x0_read32 (DBC_REG) & DFIFO_10_BO_MASK)) & DFIFO_10_BO_MASK;
synchronous = NCR53c7x0_read8 (SXFER_REG) & SXFER_MO_MASK;
if (NCR53c7x0_read8 (CTEST2_REG_800) & CTEST2_800_DDIR) {
if (synchronous)
count += (NCR53c7x0_read8 ((hostdata->chip / 100) == 8 ?
SSTAT1_REG : SSTAT2_REG) & SSTAT2_FF_MASK) >> SSTAT2_FF_SHIFT;
else
if (NCR53c7x0_read8 ((hostdata->chip / 100) == 8 ?
SSTAT0_REG : SSTAT1_REG) & SSTAT1_ILF)
++count;
} else {
sstat = ((hostdata->chip / 100) == 8) ? NCR53c7x0_read8 (SSTAT0_REG) :
NCR53c7x0_read8 (SSTAT1_REG);
if (sstat & SSTAT1_OLF)
++count;
if (synchronous && (sstat & SSTAT1_ORF))
++count;
}
return count;
}
static const char *
sbcl_to_phase (int sbcl) {
switch (sbcl & SBCL_PHASE_MASK) {
case SBCL_PHASE_DATAIN:
return "DATAIN";
case SBCL_PHASE_DATAOUT:
return "DATAOUT";
case SBCL_PHASE_MSGIN:
return "MSGIN";
case SBCL_PHASE_MSGOUT:
return "MSGOUT";
case SBCL_PHASE_CMDOUT:
return "CMDOUT";
case SBCL_PHASE_STATIN:
return "STATUSIN";
default:
return "unknown";
}
}
static const char *
sstat2_to_phase (int sstat) {
switch (sstat & SSTAT2_PHASE_MASK) {
case SSTAT2_PHASE_DATAIN:
return "DATAIN";
case SSTAT2_PHASE_DATAOUT:
return "DATAOUT";
case SSTAT2_PHASE_MSGIN:
return "MSGIN";
case SSTAT2_PHASE_MSGOUT:
return "MSGOUT";
case SSTAT2_PHASE_CMDOUT:
return "CMDOUT";
case SSTAT2_PHASE_STATIN:
return "STATUSIN";
default:
return "unknown";
}
}
static void
intr_phase_mismatch (struct Scsi_Host *host, struct NCR53c7x0_cmd *cmd) {
NCR53c7x0_local_declare();
u32 dbc_dcmd, *dsp, *dsp_next;
unsigned char dcmd, sbcl;
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
int residual;
enum {ACTION_ABORT, ACTION_ABORT_PRINT, ACTION_CONTINUE} action =
ACTION_ABORT_PRINT;
const char *where = NULL;
NCR53c7x0_local_setup(host);
dsp_next = bus_to_virt(NCR53c7x0_read32(DSP_REG));
dbc_dcmd = NCR53c7x0_read32(DBC_REG);
dcmd = (dbc_dcmd & 0xff000000) >> 24;
dsp = dsp_next - NCR53c7x0_insn_size(dcmd);
sbcl = NCR53c7x0_read8(SBCL_REG) & SBCL_PHASE_MASK;
if (!cmd) {
action = ACTION_ABORT_PRINT;
where = "no current command";
} else if (((dsp >= cmd->data_transfer_start &&
dsp < cmd->data_transfer_end)) || dsp == (cmd->residual + 2)) {
if ((dcmd & (DCMD_TYPE_MASK|DCMD_BMI_OP_MASK|DCMD_BMI_INDIRECT|
DCMD_BMI_MSG|DCMD_BMI_CD)) == (DCMD_TYPE_BMI|
DCMD_BMI_OP_MOVE_I)) {
residual = datapath_residual (host);
if (hostdata->options & OPTION_DEBUG_DISCONNECT)
printk ("scsi%d : handling residual transfer (+ %d bytes from DMA FIFO)\n",
host->host_no, residual);
if (dsp != cmd->residual + 2) {
cmd->residual[0] = ((DCMD_TYPE_TCI | DCMD_TCI_OP_CALL |
((dcmd & DCMD_BMI_IO) ? DCMD_TCI_IO : 0)) << 24) |
DBC_TCI_WAIT_FOR_VALID | DBC_TCI_COMPARE_PHASE;
cmd->residual[1] = virt_to_bus(hostdata->script)
+ ((dcmd & DCMD_BMI_IO)
? hostdata->E_other_in : hostdata->E_other_out);
}
cmd->residual[2] = dbc_dcmd + residual;
cmd->residual[3] = NCR53c7x0_read32(DNAD_REG) - residual;
if (dsp != cmd->residual + 2) {
cmd->residual[4] = ((DCMD_TYPE_TCI|DCMD_TCI_OP_JUMP)
<< 24) | DBC_TCI_TRUE;
cmd->residual[5] = virt_to_bus(dsp_next);
}
hostdata->dsp = cmd->residual;
hostdata->dsp_changed = 1;
action = ACTION_CONTINUE;
} else {
where = "non-BMI dynamic DSA code";
action = ACTION_ABORT_PRINT;
}
} else if (dsp == (hostdata->script + hostdata->E_select_msgout / 4)) {
NCR53c7x0_write8 (SOCL_REG, 0);
switch (sbcl) {
case SBCL_PHASE_CMDOUT:
hostdata->dsp = dsp + 2 ;
hostdata->dsp_changed = 1;
printk ("scsi%d : target %d ignored SDTR and went into COMMAND OUT\n",
host->host_no, cmd->cmd->target);
cmd->flags &= ~CMD_FLAG_SDTR;
action = ACTION_CONTINUE;
break;
case SBCL_PHASE_MSGIN:
hostdata->dsp = hostdata->script + hostdata->E_msg_in /
sizeof(u32);
hostdata->dsp_changed = 1;
action = ACTION_CONTINUE;
break;
default:
where="select message out";
action = ACTION_ABORT_PRINT;
}
} else if (dsp == hostdata->script + hostdata->E_cmdout_cmdout / sizeof
(u32)) {
hostdata->dsp = hostdata->script + hostdata->E_data_transfer /
sizeof (u32);
hostdata->dsp_changed = 1;
action = ACTION_CONTINUE;
#ifdef notyet
} else if (dsp == hostdata->script + hostdata->E_reply_message) {
switch (sbcl) {
#endif
} else {
where = "unknown location";
action = ACTION_ABORT_PRINT;
}
if (!hostdata->dstat_valid) {
hostdata->dstat = NCR53c7x0_read8(DSTAT_REG);
hostdata->dstat_valid = 1;
}
if (!(hostdata->dstat & DSTAT_DFE)) {
if (NCR53c7x0_read8 (CTEST2_REG_800) & CTEST2_800_DDIR) {
printk ("scsi%d: Flushing DMA FIFO\n",
host->host_no);
NCR53c7x0_write8 (CTEST3_REG_800, CTEST3_800_FLF);
while (!((hostdata->dstat = NCR53c7x0_read8(DSTAT_REG)) &
DSTAT_DFE));
} else {
NCR53c7x0_write8 (CTEST3_REG_800, CTEST3_800_CLF);
while (NCR53c7x0_read8 (CTEST3_REG_800) & CTEST3_800_CLF);
}
hostdata->dstat |= DSTAT_DFE;
}
switch (action) {
case ACTION_ABORT_PRINT:
printk("scsi%d : %s : unexpected phase %s.\n",
host->host_no, where ? where : "unknown location",
sbcl_to_phase(sbcl));
print_lots (host);
case ACTION_ABORT:
abort_connected (host);
break;
case ACTION_CONTINUE:
break;
}
#if 0
if (hostdata->dsp_changed) {
printk("scsi%d: new dsp 0x%p\n", host->host_no, hostdata->dsp);
print_insn (host, hostdata->dsp, "", 1);
}
#endif
}
static void
intr_bf (struct Scsi_Host *host, struct NCR53c7x0_cmd *cmd) {
NCR53c7x0_local_declare();
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
u32 *dsp,
*next_dsp,
*dsa,
dbc_dcmd;
unsigned short pci_status;
int tmp;
unsigned long flags;
char *reason = NULL;
enum {MAYBE, ALWAYS, NEVER} retry = MAYBE;
int report = 0;
NCR53c7x0_local_setup(host);
dbc_dcmd = NCR53c7x0_read32 (DBC_REG);
next_dsp = bus_to_virt (NCR53c7x0_read32(DSP_REG));
dsp = next_dsp - NCR53c7x0_insn_size ((dbc_dcmd >> 24) & 0xff);
dsa = bus_to_virt (NCR53c7x0_read32(DSA_REG));
if ((hostdata->chip / 100) == 8) {
save_flags (flags);
cli();
tmp = pcibios_read_config_word (hostdata->pci_bus,
hostdata->pci_device_fn, PCI_STATUS, &pci_status);
restore_flags (flags);
if (tmp == PCIBIOS_SUCCESSFUL) {
if (pci_status & PCI_STATUS_REC_TARGET_ABORT) {
reason = "PCI target abort";
pci_status &= ~PCI_STATUS_REC_TARGET_ABORT;
} else if (pci_status & PCI_STATUS_REC_MASTER_ABORT) {
reason = "No device asserted PCI DEVSEL within five bus clocks";
pci_status &= ~PCI_STATUS_REC_MASTER_ABORT;
} else if (pci_status & PCI_STATUS_PARITY) {
report = 1;
pci_status &= ~PCI_STATUS_PARITY;
}
} else {
printk ("scsi%d : couldn't read status register : %s\n",
host->host_no, pcibios_strerror (tmp));
retry = NEVER;
}
}
#ifndef notyet
report = 1;
#endif
if (report && reason) {
printk(KERN_ALERT "scsi%d : BUS FAULT reason = %s\n",
host->host_no, reason ? reason : "unknown");
print_lots (host);
}
#ifndef notyet
retry = NEVER;
#endif
if (retry == NEVER) {
printk(KERN_ALERT "          mail drew@PoohSticks.ORG\n");
FATAL (host);
}
}
static void
intr_dma (struct Scsi_Host *host, struct NCR53c7x0_cmd *cmd) {
NCR53c7x0_local_declare();
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
unsigned char dstat;
u32 *dsp,
*next_dsp,
*dsa,
dbc_dcmd;
int tmp;
unsigned long flags;
NCR53c7x0_local_setup(host);
if (!hostdata->dstat_valid) {
hostdata->dstat = NCR53c7x0_read8(DSTAT_REG);
hostdata->dstat_valid = 1;
}
dstat = hostdata->dstat;
if (hostdata->options & OPTION_DEBUG_INTR)
printk("scsi%d : DSTAT=0x%x\n", host->host_no, (int) dstat);
dbc_dcmd = NCR53c7x0_read32 (DBC_REG);
next_dsp = bus_to_virt(NCR53c7x0_read32(DSP_REG));
dsp = next_dsp - NCR53c7x0_insn_size ((dbc_dcmd >> 24) & 0xff);
dsa = bus_to_virt(NCR53c7x0_read32(DSA_REG));
if (dstat & DSTAT_ABRT) {
#if 0
if ((hostdata->options & OPTION_700) && (hostdata->state ==
STATE_ABORTING)) {
} else
#endif
{
printk(KERN_ALERT "scsi%d : unexpected abort interrupt at\n"
"         ", host->host_no);
print_insn (host, dsp, KERN_ALERT "s ", 1);
FATAL (host);
}
}
if (dstat & DSTAT_SSI) {
if (hostdata->options & OPTION_DEBUG_TRACE) {
} else if (hostdata->options & OPTION_DEBUG_SINGLE) {
print_insn (host, dsp, "s ", 0);
save_flags(flags);
cli();
NCR53c7x0_write8 (DCNTL_REG, (NCR53c7x0_read8(DCNTL_REG) &
~DCNTL_SSM) | DCNTL_STD);
restore_flags(flags);
} else {
printk(KERN_ALERT "scsi%d : unexpected single step interrupt at\n"
"         ", host->host_no);
print_insn (host, dsp, KERN_ALERT "", 1);
printk(KERN_ALERT "         mail drew@PoohSticks.ORG\n");
FATAL (host);
}
}
if (dstat & DSTAT_OPC) {
if (((dsp >= (hostdata->script + hostdata->E_select / sizeof(u32))) &&
(dsp <= (hostdata->script + hostdata->E_select_msgout /
sizeof(u32) + 8))) || (hostdata->test_running == 2)) {
if (hostdata->options & OPTION_DEBUG_INTR)
printk ("scsi%d : ignoring DSTAT_IID for SSTAT_STO\n",
host->host_no);
if (hostdata->expecting_iid) {
hostdata->expecting_iid = 0;
hostdata->idle = 1;
if (hostdata->test_running == 2) {
hostdata->test_running = 0;
hostdata->test_completed = 3;
} else if (cmd)
abnormal_finished (cmd, DID_BAD_TARGET << 16);
} else {
hostdata->expecting_sto = 1;
}
} else if (dbc_dcmd == 0x48000000 && (NCR53c7x0_read8 (SBCL_REG) &
SBCL_REQ)) {
if (!(hostdata->options & OPTION_NO_PRINT_RACE))
{
printk("scsi%d: REQ before WAIT DISCONNECT IID\n",
host->host_no);
hostdata->options |= OPTION_NO_PRINT_RACE;
}
} else {
printk(KERN_ALERT "scsi%d : illegal instruction\n", host->host_no);
print_lots (host);
printk(KERN_ALERT "         mail drew@PoohSticks.ORG with ALL\n"
"         boot messages and diagnostic output\n");
FATAL (host);
}
}
if (dstat & DSTAT_800_BF) {
intr_bf (host, cmd);
}
if (dstat & DSTAT_SIR) {
if (hostdata->options & OPTION_DEBUG_INTR)
printk ("scsi%d : DSTAT_SIR\n", host->host_no);
switch ((tmp = hostdata->dstat_sir_intr (host, cmd))) {
case SPECIFIC_INT_NOTHING:
case SPECIFIC_INT_RESTART:
break;
case SPECIFIC_INT_ABORT:
abort_connected(host);
break;
case SPECIFIC_INT_PANIC:
printk(KERN_ALERT "scsi%d : failure at ", host->host_no);
print_insn (host, dsp, KERN_ALERT "", 1);
printk(KERN_ALERT "          dstat_sir_intr() returned SPECIFIC_INT_PANIC\n");
FATAL (host);
break;
case SPECIFIC_INT_BREAK:
intr_break (host, cmd);
break;
default:
printk(KERN_ALERT "scsi%d : failure at ", host->host_no);
print_insn (host, dsp, KERN_ALERT "", 1);
printk(KERN_ALERT"          dstat_sir_intr() returned unknown value %d\n",
tmp);
FATAL (host);
}
}
if ((hostdata->chip / 100) == 8 && (dstat & DSTAT_800_MDPE)) {
printk(KERN_ALERT "scsi%d : Master Data Parity Error\n",
host->host_no);
FATAL (host);
}
}
static int
print_insn (struct Scsi_Host *host, const u32 *insn,
const char *prefix, int kernel) {
char buf[160],
*tmp;
unsigned char dcmd;
int size;
if (MAP_NR(insn) < 1 || MAP_NR(insn + 8) > MAP_NR(high_memory) ||
((((dcmd = (insn[0] >> 24) & 0xff) & DCMD_TYPE_MMI) == DCMD_TYPE_MMI) &&
MAP_NR(insn + 12) > MAP_NR(high_memory))) {
size = 0;
sprintf (buf, "%s%p: address out of range\n",
prefix, insn);
} else {
sprintf(buf, "%s0x%lx (virt 0x%p) : 0x%08x 0x%08x (virt 0x%p)",
(prefix ? prefix : ""), virt_to_bus((void *) insn), insn,
insn[0], insn[1], bus_to_virt (insn[1]));
tmp = buf + strlen(buf);
if ((dcmd & DCMD_TYPE_MASK) == DCMD_TYPE_MMI) {
sprintf (tmp, " 0x%08x (virt 0x%p)\n", insn[2],
bus_to_virt(insn[2]));
size = 3;
} else {
sprintf (tmp, "\n");
size = 2;
}
}
if (kernel)
printk ("%s", buf);
#ifdef NCR_DEBUG
else {
size_t len = strlen(buf);
debugger_kernel_write(host, buf, len);
}
#endif
return size;
}
static const char *
ncr_state (int state) {
switch (state) {
case STATE_HALTED: return "halted";
case STATE_WAITING: return "waiting";
case STATE_RUNNING: return "running";
case STATE_ABORTING: return "aborting";
case STATE_DISABLED: return "disabled";
default: return "unknown";
}
}
int
NCR53c7xx_abort (Scsi_Cmnd *cmd) {
NCR53c7x0_local_declare();
struct Scsi_Host *host = cmd->host;
struct NCR53c7x0_hostdata *hostdata = host ? (struct NCR53c7x0_hostdata *)
host->hostdata : NULL;
unsigned long flags;
struct NCR53c7x0_cmd *curr, **prev;
Scsi_Cmnd *me, **last;
#if 0
static long cache_pid = -1;
#endif
if (!host) {
printk ("Bogus SCSI command pid %ld; no host structure\n",
cmd->pid);
return SCSI_ABORT_ERROR;
} else if (!hostdata) {
printk ("Bogus SCSI host %d; no hostdata\n", host->host_no);
return SCSI_ABORT_ERROR;
}
NCR53c7x0_local_setup(host);
if (NCR53c7x0_read8(hostdata->istat) &
(ISTAT_DIP|ISTAT_SIP|
(hostdata->chip / 100 == 8 ? ISTAT_800_INTF : 0))) {
printk ("scsi%d : dropped interrupt for command %ld\n", host->host_no,
cmd->pid);
NCR53c7x0_intr (host->irq, NULL, NULL);
return SCSI_ABORT_BUSY;
}
save_flags(flags);
cli();
#if 0
if (cache_pid == cmd->pid)
panic ("scsi%d : bloody fetus %d\n", host->host_no, cmd->pid);
else
cache_pid = cmd->pid;
#endif
for (me = (Scsi_Cmnd *) hostdata->issue_queue,
last = (Scsi_Cmnd **) &(hostdata->issue_queue);
me && me != cmd; last = (Scsi_Cmnd **)&(me->SCp.ptr),
me = (Scsi_Cmnd *)me->SCp.ptr);
if (me) {
*last = (Scsi_Cmnd *) me->SCp.ptr;
if (me->host_scribble) {
((struct NCR53c7x0_cmd *)me->host_scribble)->next = hostdata->free;
hostdata->free = (struct NCR53c7x0_cmd *) me->host_scribble;
me->host_scribble = NULL;
}
cmd->result = DID_ABORT << 16;
cmd->scsi_done(cmd);
printk ("scsi%d : found command %ld in Linux issue queue\n",
host->host_no, me->pid);
restore_flags(flags);
run_process_issue_queue();
return SCSI_ABORT_SUCCESS;
}
for (curr = (struct NCR53c7x0_cmd *) hostdata->running_list,
prev = (struct NCR53c7x0_cmd **) &(hostdata->running_list);
curr && curr->cmd != cmd; prev = (struct NCR53c7x0_cmd **)
&(curr->next), curr = (struct NCR53c7x0_cmd *) curr->next);
if (curr) {
if ((cmd->result & 0xff) != 0xff && (cmd->result & 0xff00) != 0xff00) {
if (prev)
*prev = (struct NCR53c7x0_cmd *) curr->next;
curr->next = (struct NCR53c7x0_cmd *) hostdata->free;
cmd->host_scribble = NULL;
hostdata->free = curr;
cmd->scsi_done(cmd);
printk ("scsi%d : found finished command %ld in running list\n",
host->host_no, cmd->pid);
restore_flags(flags);
return SCSI_ABORT_NOT_RUNNING;
} else {
printk ("scsi%d : DANGER : command running, can not abort.\n",
cmd->host->host_no);
restore_flags(flags);
return SCSI_ABORT_BUSY;
}
}
curr = (struct NCR53c7x0_cmd *) cmd->host_scribble;
if (curr) {
curr->next = hostdata->free;
hostdata->free = curr;
cmd->host_scribble = NULL;
}
if (((cmd->result & 0xff00) == 0xff00) ||
((cmd->result & 0xff) == 0xff)) {
printk ("scsi%d : did this command ever run?\n", host->host_no);
cmd->result = DID_ABORT << 16;
} else {
printk ("scsi%d : probably lost INTFLY, normal completion\n",
host->host_no);
--hostdata->busy[cmd->target][cmd->lun];
}
restore_flags(flags);
cmd->scsi_done(cmd);
return SCSI_ABORT_NOT_RUNNING;
}
int
NCR53c7xx_reset (Scsi_Cmnd *cmd, unsigned int reset_flags) {
NCR53c7x0_local_declare();
unsigned long flags;
int found = 0;
struct NCR53c7x0_cmd * c;
Scsi_Cmnd *tmp;
Scsi_Cmnd *nuke_list = NULL;
struct Scsi_Host *host = cmd->host;
struct NCR53c7x0_hostdata *hostdata =
(struct NCR53c7x0_hostdata *) host->hostdata;
NCR53c7x0_local_setup(host);
save_flags(flags);
cli();
ncr_halt (host);
print_lots (host);
dump_events (host, 30);
ncr_scsi_reset (host);
for (tmp = nuke_list = return_outstanding_commands (host, 1 ,
0 ); tmp; tmp = (Scsi_Cmnd *) tmp->SCp.buffer)
if (tmp == cmd) {
found = 1;
break;
}
if (!found) {
c = (struct NCR53c7x0_cmd *) cmd->host_scribble;
if (c) {
cmd->host_scribble = NULL;
c->next = hostdata->free;
hostdata->free = c;
} else
printk ("scsi%d: lost command %ld\n", host->host_no, cmd->pid);
cmd->SCp.buffer = (struct scatterlist *) nuke_list;
nuke_list = cmd;
}
NCR53c7x0_driver_init (host);
hostdata->soft_reset (host);
if (hostdata->resets == 0)
disable(host);
else if (hostdata->resets != -1)
--hostdata->resets;
sti();
for (; nuke_list; nuke_list = tmp) {
tmp = (Scsi_Cmnd *) nuke_list->SCp.buffer;
nuke_list->result = DID_RESET << 16;
nuke_list->scsi_done (nuke_list);
}
restore_flags(flags);
return SCSI_RESET_SUCCESS;
}
static int
insn_to_offset (Scsi_Cmnd *cmd, u32 *insn) {
struct NCR53c7x0_hostdata *hostdata =
(struct NCR53c7x0_hostdata *) cmd->host->hostdata;
struct NCR53c7x0_cmd *ncmd =
(struct NCR53c7x0_cmd *) cmd->host_scribble;
int offset = 0, buffers;
struct scatterlist *segment;
char *ptr;
int found = 0;
if (!check_address ((unsigned long) ncmd, sizeof (struct NCR53c7x0_cmd)) &&
((insn >= ncmd->data_transfer_start &&
insn < ncmd->data_transfer_end) ||
(insn >= ncmd->residual &&
insn < (ncmd->residual +
sizeof(ncmd->residual))))) {
ptr = bus_to_virt(insn[3]);
if ((buffers = cmd->use_sg)) {
for (offset = 0,
segment = (struct scatterlist *) cmd->buffer;
buffers && !((found = ((ptr >= segment->address) &&
(ptr < (segment->address + segment->length)))));
--buffers, offset += segment->length, ++segment)
#if 0
printk("scsi%d: comparing 0x%p to 0x%p\n",
cmd->host->host_no, saved, segment->address);
#else
;
#endif
offset += ptr - segment->address;
} else {
found = 1;
offset = ptr - (char *) (cmd->request_buffer);
}
} else if ((insn >= hostdata->script +
hostdata->E_data_transfer / sizeof(u32)) &&
(insn <= hostdata->script +
hostdata->E_end_data_transfer / sizeof(u32))) {
found = 1;
offset = 0;
}
return found ? offset : -1;
}
static void
print_progress (Scsi_Cmnd *cmd) {
NCR53c7x0_local_declare();
struct NCR53c7x0_cmd *ncmd =
(struct NCR53c7x0_cmd *) cmd->host_scribble;
int offset, i;
char *where;
u32 *ptr;
NCR53c7x0_local_setup (cmd->host);
for (i = 0; i < 2; ++i) {
if (check_address ((unsigned long) ncmd,
sizeof (struct NCR53c7x0_cmd)) == -1)
continue;
if (!i) {
where = "saved";
ptr = bus_to_virt(ncmd->saved_data_pointer);
} else {
where = "active";
ptr = bus_to_virt (NCR53c7x0_read32 (DSP_REG) -
NCR53c7x0_insn_size (NCR53c7x0_read8 (DCMD_REG)) *
sizeof(u32));
}
offset = insn_to_offset (cmd, ptr);
if (offset != -1)
printk ("scsi%d : %s data pointer at offset %d\n",
cmd->host->host_no, where, offset);
else {
int size;
printk ("scsi%d : can't determine %s data pointer offset\n",
cmd->host->host_no, where);
if (ncmd) {
size = print_insn (cmd->host,
bus_to_virt(ncmd->saved_data_pointer), "", 1);
print_insn (cmd->host,
bus_to_virt(ncmd->saved_data_pointer) + size * sizeof(u32),
"", 1);
}
}
}
}
static void
print_dsa (struct Scsi_Host *host, u32 *dsa, const char *prefix) {
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
int i, len;
char *ptr;
Scsi_Cmnd *cmd;
if (check_address ((unsigned long) dsa, hostdata->dsa_end -
hostdata->dsa_start) == -1) {
printk("scsi%d : bad dsa virt 0x%p\n", host->host_no, dsa);
return;
}
printk("%sscsi%d : dsa at phys 0x%lx (virt 0x%p)\n"
"        + %d : dsa_msgout length = %u, data = 0x%x (virt 0x%p)\n" ,
prefix ? prefix : "",
host->host_no, virt_to_bus (dsa), dsa, hostdata->dsa_msgout,
dsa[hostdata->dsa_msgout / sizeof(u32)],
dsa[hostdata->dsa_msgout / sizeof(u32) + 1],
bus_to_virt (dsa[hostdata->dsa_msgout / sizeof(u32) + 1]));
if (dsa[hostdata->dsa_msgout / sizeof(u32)] <
sizeof (hostdata->free->select))
for (i = dsa[hostdata->dsa_msgout / sizeof(u32)],
ptr = bus_to_virt (dsa[hostdata->dsa_msgout / sizeof(u32) + 1]);
i > 0 && !check_address ((unsigned long) ptr, 1);
ptr += len, i -= len) {
printk("               ");
len = print_msg (ptr);
printk("\n");
if (!len)
break;
}
printk("        + %d : select_indirect = 0x%x\n",
hostdata->dsa_select, dsa[hostdata->dsa_select / sizeof(u32)]);
cmd = (Scsi_Cmnd *) bus_to_virt(dsa[hostdata->dsa_cmnd / sizeof(u32)]);
printk("        + %d : dsa_cmnd = 0x%x ", hostdata->dsa_cmnd,
(u32) virt_to_bus(cmd));
if (cmd) {
printk("               result = 0x%x, target = %d, lun = %d, cmd = ",
cmd->result, cmd->target, cmd->lun);
print_command(cmd->cmnd);
} else
printk("\n");
printk("        + %d : dsa_next = 0x%x\n", hostdata->dsa_next,
dsa[hostdata->dsa_next / sizeof(u32)]);
if (cmd) {
printk("scsi%d target %d : sxfer_sanity = 0x%x, scntl3_sanity = 0x%x\n"
"                   script : ",
host->host_no, cmd->target,
hostdata->sync[cmd->target].sxfer_sanity,
hostdata->sync[cmd->target].scntl3_sanity);
for (i = 0; i < (sizeof(hostdata->sync[cmd->target].script) / 4); ++i)
printk ("0x%x ", hostdata->sync[cmd->target].script[i]);
printk ("\n");
print_progress (cmd);
}
}
static void
print_queues (struct Scsi_Host *host) {
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
u32 *dsa, *next_dsa;
volatile u32 *current;
int left;
Scsi_Cmnd *cmd, *next_cmd;
unsigned long flags;
printk ("scsi%d : issue queue\n", host->host_no);
for (left = host->can_queue, cmd = (Scsi_Cmnd *) hostdata->issue_queue;
left >= 0 && cmd;
cmd = next_cmd) {
next_cmd = (Scsi_Cmnd *) cmd->SCp.ptr;
save_flags(flags);
cli();
if (cmd->host_scribble) {
if (check_address ((unsigned long) (cmd->host_scribble),
sizeof (cmd->host_scribble)) == -1)
printk ("scsi%d: scsi pid %ld bad pointer to NCR53c7x0_cmd\n",
host->host_no, cmd->pid);
else
print_dsa (host, ((struct NCR53c7x0_cmd *) cmd->host_scribble)
-> dsa, "");
} else
printk ("scsi%d : scsi pid %ld for target %d lun %d has no NCR53c7x0_cmd\n",
host->host_no, cmd->pid, cmd->target, cmd->lun);
restore_flags(flags);
}
if (left <= 0) {
printk ("scsi%d : loop detected in issue queue\n",
host->host_no);
}
printk ("scsi%d : schedule dsa array :\n", host->host_no);
for (left = host->can_queue, current = hostdata->schedule;
left > 0; current += 2, --left)
if (current[0] != hostdata->NOP_insn)
print_dsa (host, bus_to_virt (current[1] -
(hostdata->E_dsa_code_begin -
hostdata->E_dsa_code_template)), "");
printk ("scsi%d : end schedule dsa array\n", host->host_no);
printk ("scsi%d : reconnect_dsa_head :\n", host->host_no);
for (left = host->can_queue,
dsa = bus_to_virt (hostdata->reconnect_dsa_head);
left >= 0 && dsa;
dsa = next_dsa) {
save_flags (flags);
cli();
if (check_address ((unsigned long) dsa, sizeof(dsa)) == -1) {
printk ("scsi%d: bad DSA pointer 0x%p", host->host_no,
dsa);
next_dsa = NULL;
}
else
{
next_dsa = bus_to_virt(dsa[hostdata->dsa_next / sizeof(u32)]);
print_dsa (host, dsa, "");
}
restore_flags(flags);
}
printk ("scsi%d : end reconnect_dsa_head\n", host->host_no);
if (left < 0)
printk("scsi%d: possible loop in ncr reconnect list\n",
host->host_no);
}
static void
print_lots (struct Scsi_Host *host) {
NCR53c7x0_local_declare();
struct NCR53c7x0_hostdata *hostdata =
(struct NCR53c7x0_hostdata *) host->hostdata;
u32 *dsp_next, *dsp, *dsa, dbc_dcmd;
unsigned char dcmd, sbcl;
int i, size;
NCR53c7x0_local_setup(host);
if ((dsp_next = bus_to_virt(NCR53c7x0_read32 (DSP_REG)))) {
dbc_dcmd = NCR53c7x0_read32(DBC_REG);
dcmd = (dbc_dcmd & 0xff000000) >> 24;
dsp = dsp_next - NCR53c7x0_insn_size(dcmd);
dsa = bus_to_virt(NCR53c7x0_read32(DSA_REG));
sbcl = NCR53c7x0_read8 (SBCL_REG);
printk ("scsi%d : DCMD|DBC=0x%x, DNAD=0x%x (virt 0x%p)\n"
"         DSA=0x%lx (virt 0x%p)\n"
"         DSPS=0x%x, TEMP=0x%x (virt 0x%p), DMODE=0x%x\n"
"         SXFER=0x%x, SCNTL3=0x%x\n"
"         %s%s%sphase=%s, %d bytes in SCSI FIFO\n"
"         STEST0=0x%x\n",
host->host_no, dbc_dcmd, NCR53c7x0_read32(DNAD_REG),
bus_to_virt(NCR53c7x0_read32(DNAD_REG)),
virt_to_bus(dsa), dsa,
NCR53c7x0_read32(DSPS_REG), NCR53c7x0_read32(TEMP_REG),
bus_to_virt (NCR53c7x0_read32(TEMP_REG)),
(int) NCR53c7x0_read8(hostdata->dmode),
(int) NCR53c7x0_read8(SXFER_REG),
(int) NCR53c7x0_read8(SCNTL3_REG_800),
(sbcl & SBCL_BSY) ? "BSY " : "",
(sbcl & SBCL_SEL) ? "SEL " : "",
(sbcl & SBCL_REQ) ? "REQ " : "",
sstat2_to_phase(NCR53c7x0_read8 (((hostdata->chip / 100) == 8) ?
SSTAT1_REG : SSTAT2_REG)),
(NCR53c7x0_read8 ((hostdata->chip / 100) == 8 ?
SSTAT1_REG : SSTAT2_REG) & SSTAT2_FF_MASK) >> SSTAT2_FF_SHIFT,
NCR53c7x0_read8 (STEST0_REG_800));
printk ("scsi%d : DSP 0x%lx (virt 0x%p) ->\n", host->host_no,
virt_to_bus(dsp), dsp);
for (i = 6; i > 0; --i, dsp += size)
size = print_insn (host, dsp, "", 1);
if (NCR53c7x0_read8 (SCNTL1_REG) & SCNTL1_CON) {
printk ("scsi%d : connected (SDID=0x%x, SSID=0x%x)\n",
host->host_no, NCR53c7x0_read8 (SDID_REG_800),
NCR53c7x0_read8 (SSID_REG_800));
print_dsa (host, dsa, "");
}
#if 1
print_queues (host);
#endif
}
}
static int
shutdown (struct Scsi_Host *host) {
NCR53c7x0_local_declare();
unsigned long flags;
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
NCR53c7x0_local_setup(host);
save_flags (flags);
cli();
ncr_halt (host);
ncr_scsi_reset (host);
hostdata->soft_reset(host);
disable (host);
restore_flags (flags);
return 0;
}
static void
ncr_scsi_reset (struct Scsi_Host *host) {
NCR53c7x0_local_declare();
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
unsigned long flags;
int sien = 0;
NCR53c7x0_local_setup(host);
save_flags (flags);
cli();
if ((hostdata->chip / 100) == 8) {
sien = NCR53c7x0_read8(SIEN0_REG_800);
NCR53c7x0_write8(SIEN0_REG_800, sien & ~SIEN_RST);
}
NCR53c7x0_write8(SCNTL1_REG, SCNTL1_RST);
udelay(25);
NCR53c7x0_write8(SCNTL1_REG, 0);
if ((hostdata->chip / 100) == 8) {
NCR53c7x0_write8(SIEN0_REG_800, sien);
}
restore_flags (flags);
}
static void
hard_reset (struct Scsi_Host *host) {
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
unsigned long flags;
save_flags (flags);
cli();
ncr_scsi_reset(host);
NCR53c7x0_driver_init (host);
if (hostdata->soft_reset)
hostdata->soft_reset (host);
restore_flags(flags);
}
static Scsi_Cmnd *
return_outstanding_commands (struct Scsi_Host *host, int free, int issue) {
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
struct NCR53c7x0_cmd *c;
int i;
u32 *current;
Scsi_Cmnd *list = NULL, *tmp;
for (c = (struct NCR53c7x0_cmd *) hostdata->running_list; c;
c = (struct NCR53c7x0_cmd *) c->next) {
if (c->cmd->SCp.buffer) {
printk ("scsi%d : loop detected in running list!\n", host->host_no);
break;
} else {
printk ("The sti() implicit in a printk() prevents hangs\n");
break;
}
c->cmd->SCp.buffer = (struct scatterlist *) list;
list = c->cmd;
if (free) {
c->next = hostdata->free;
hostdata->free = c;
}
}
if (free) {
for (i = 0, current = (u32 *) hostdata->schedule;
i < host->can_queue; ++i, current += 2) {
current[0] = hostdata->NOP_insn;
current[1] = 0xdeadbeef;
}
hostdata->current = NULL;
}
if (issue) {
for (tmp = (Scsi_Cmnd *) hostdata->issue_queue; tmp; tmp = tmp->next) {
if (tmp->SCp.buffer) {
printk ("scsi%d : loop detected in issue queue!\n",
host->host_no);
break;
}
tmp->SCp.buffer = (struct scatterlist *) list;
list = tmp;
}
if (free)
hostdata->issue_queue = NULL;
}
return list;
}
static int
disable (struct Scsi_Host *host) {
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
unsigned long flags;
Scsi_Cmnd *nuke_list, *tmp;
save_flags(flags);
cli();
if (hostdata->state != STATE_HALTED)
ncr_halt (host);
nuke_list = return_outstanding_commands (host, 1 , 1 );
hard_reset (host);
hostdata->state = STATE_DISABLED;
restore_flags(flags);
printk ("scsi%d : nuking commands\n", host->host_no);
for (; nuke_list; nuke_list = tmp) {
tmp = (Scsi_Cmnd *) nuke_list->SCp.buffer;
nuke_list->result = DID_ERROR << 16;
nuke_list->scsi_done(nuke_list);
}
printk ("scsi%d : done. \n", host->host_no);
printk (KERN_ALERT "scsi%d : disabled.  Unload and reload\n",
host->host_no);
return 0;
}
static int
ncr_halt (struct Scsi_Host *host) {
NCR53c7x0_local_declare();
unsigned long flags;
unsigned char istat, tmp;
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
int stage;
NCR53c7x0_local_setup(host);
save_flags(flags);
cli();
for (stage = 0;;) {
if (stage == 1) {
NCR53c7x0_write8(hostdata->istat, ISTAT_ABRT);
++stage;
}
istat = NCR53c7x0_read8 (hostdata->istat);
if (istat & ISTAT_SIP) {
if ((hostdata->chip / 100) == 8) {
tmp = NCR53c7x0_read8(SIST0_REG_800);
udelay(1);
tmp = NCR53c7x0_read8(SIST1_REG_800);
} else {
tmp = NCR53c7x0_read8(SSTAT0_REG);
}
} else if (istat & ISTAT_DIP) {
tmp = NCR53c7x0_read8(DSTAT_REG);
if (stage == 2) {
if (tmp & DSTAT_ABRT) {
NCR53c7x0_write8(hostdata->istat, 0);
++stage;
} else {
printk(KERN_ALERT "scsi%d : could not halt NCR chip\n",
host->host_no);
disable (host);
}
}
}
if (!(istat & (ISTAT_SIP|ISTAT_DIP)))
if (stage == 0)
++stage;
else if (stage == 3)
break;
}
hostdata->state = STATE_HALTED;
restore_flags(flags);
#if 0
print_lots (host);
#endif
return 0;
}
static const char *
event_name (int event) {
switch (event) {
case EVENT_NONE: return "none";
case EVENT_ISSUE_QUEUE: return "to issue queue";
case EVENT_START_QUEUE: return "to start queue";
case EVENT_SELECT: return "selected";
case EVENT_DISCONNECT: return "disconnected";
case EVENT_RESELECT: return "reselected";
case EVENT_COMPLETE: return "completed";
case EVENT_IDLE: return "idle";
case EVENT_SELECT_FAILED: return "select failed";
case EVENT_BEFORE_SELECT: return "before select";
case EVENT_RESELECT_FAILED: return "reselect failed";
default: return "unknown";
}
}
static void
dump_events (struct Scsi_Host *host, int count) {
struct NCR53c7x0_hostdata *hostdata = (struct NCR53c7x0_hostdata *)
host->hostdata;
struct NCR53c7x0_event event;
int i;
unsigned long flags;
if (hostdata->events) {
if (count > hostdata->event_size)
count = hostdata->event_size;
for (i = hostdata->event_index; count > 0;
i = (i ? i - 1 : hostdata->event_size -1), --count) {
save_flags(flags);
cli();
#if 0
event = hostdata->events[i];
#else
memcpy ((void *) &event, (void *) &(hostdata->events[i]),
sizeof(event));
#endif
restore_flags(flags);
printk ("scsi%d : %s event %d at %ld secs %ld usecs target %d lun %d\n",
host->host_no, event_name (event.event), count,
(long) event.time.tv_sec, (long) event.time.tv_usec,
event.target, event.lun);
if (event.dsa)
printk ("         event for dsa 0x%lx (virt 0x%p)\n",
virt_to_bus(event.dsa), event.dsa);
if (event.pid != -1) {
printk ("         event for pid %ld ", event.pid);
print_command (event.cmnd);
}
}
}
}
static int
check_address (unsigned long addr, int size) {
return (MAP_NR(addr) < 1 || MAP_NR(addr + size) > MAP_NR(high_memory) ?
-1 : 0);
}
#ifdef MODULE
int
NCR53c7x0_release(struct Scsi_Host *host) {
struct NCR53c7x0_hostdata *hostdata =
(struct NCR53c7x0_hostdata *) host->hostdata;
struct NCR53c7x0_cmd *cmd, *tmp;
shutdown (host);
if (host->irq != IRQ_NONE)
{
int irq_count;
struct Scsi_Host *tmp;
for (irq_count = 0, tmp = first_host; tmp; tmp = tmp->next)
if (tmp->hostt == the_template && tmp->irq == host->irq)
++irq_count;
if (irq_count == 1)
free_irq(host->irq, NULL);
}
if (host->dma_channel != DMA_NONE)
free_dma(host->dma_channel);
if (host->io_port)
release_region(host->io_port, host->n_io_port);
for (cmd = (struct NCR53c7x0_cmd *) hostdata->free; cmd; cmd = tmp,
--hostdata->num_cmds) {
tmp = (struct NCR53c7x0_cmd *) cmd->next;
cmd->next = NULL;
if (cmd->free)
cmd->free ((void *) cmd->real, cmd->size);
}
if (hostdata->num_cmds)
printk ("scsi%d : leaked %d NCR53c7x0_cmd structures\n",
host->host_no, hostdata->num_cmds);
if (hostdata->events)
vfree ((void *)hostdata->events);
return 1;
}
Scsi_Host_Template driver_template = NCR53c7xx;
#include "scsi_module.c"
#endif