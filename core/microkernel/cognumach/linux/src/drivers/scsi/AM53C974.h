#ifndef AM53C974_H
#define AM53C974_H
#include <scsi/scsicam.h>
#define DEFAULT_SYNC_NEGOTIATION_ENABLED 0
#define DEFAULT_RATE 5
#define DEFAULT_SYNC_OFFSET 0
#define AM53C974_DRIVER_REVISION_MAJOR 0
#define AM53C974_DRIVER_REVISION_MINOR 5
#define SEPARATOR_LINE \
"--------------------------------------------------------------------------\n"
#define AM53C974_DEBUG_ABORT
#define DEF_CLK 40
#define MIN_PERIOD 4
#define MAX_PERIOD 13
#define MAX_OFFSET 15
#define DEF_SCSI_TIMEOUT 245
#define DEF_STP 8
#define DEF_SOF_RAD 0
#define DEF_SOF_RAA 0
#define DEF_ETM 0
#define DEF_PERE 1
#define DEF_CLKF 0
#define DEF_ENF 1
#define DEF_ADIDCHK 0
#define DEF_FASTSCSI 1
#define DEF_FASTCLK 1
#define DEF_GLITCH 1
#define DEF_PWD 0
#define DEF_RAE 0
#define DEF_RADE 1
#ifndef PCI_VENDOR_ID_AMD
#define PCI_VENDOR_ID_AMD 0x1022
#define PCI_DEVICE_ID_AMD_SCSI 0x2020
#endif
#define PCI_BASE_MASK 0xFFFFFFE0
#define PCI_COMMAND_PERREN 0x40
#define PCI_SCRATCH_REG_0 0x40
#define PCI_SCRATCH_REG_1 0x42
#define PCI_SCRATCH_REG_2 0x44
#define PCI_SCRATCH_REG_3 0x46
#define PCI_SCRATCH_REG_4 0x48
#define PCI_SCRATCH_REG_5 0x4A
#define PCI_SCRATCH_REG_6 0x4C
#define PCI_SCRATCH_REG_7 0x4E
#define CTCLREG 0x00
#define CTCMREG 0x04
#define CTCHREG 0x38
#define STCLREG 0x00
#define STCMREG 0x04
#define STCHREG 0x38
#define FFREG 0x08
#define STIMREG 0x14
#define SDIDREG 0x10
#define SDIREG_MASK 0x07
#define STPREG 0x18
#define STPREG_STP 0x1F
#define CLKFREG 0x24
#define CLKFREG_MASK 0x07
#define CMDREG 0x0C
#define CMDREG_DMA 0x80
#define CMDREG_IT 0x10
#define CMDREG_ICCS 0x11
#define CMDREG_MA 0x12
#define CMDREG_TPB 0x98
#define CMDREG_SATN 0x1A
#define CMDREG_RATN 0x1B
#define CMDREG_SOAS 0x41
#define CMDREG_SAS 0x42
#define CMDREG_SASS 0x43
#define CMDREG_ESR 0x44
#define CMDREG_DSR 0x45
#define CMDREG_SA3S 0x46
#define CMDREG_NOP 0x00
#define CMDREG_CFIFO 0x01
#define CMDREG_RDEV 0x02
#define CMDREG_RBUS 0x03
#define STATREG 0x10
#define STATREG_INT 0x80
#define STATREG_IOE 0x40
#define STATREG_PE 0x20
#define STATREG_CTZ 0x10
#define STATREG_MSG 0x04
#define STATREG_CD 0x02
#define STATREG_IO 0x01
#define STATREG_PHASE 0x07
#define INSTREG 0x14
#define INSTREG_SRST 0x80
#define INSTREG_ICMD 0x40
#define INSTREG_DIS 0x20
#define INSTREG_SR 0x10
#define INSTREG_SO 0x08
#define INSTREG_RESEL 0x04
#define ISREG 0x18
#define ISREG_SOF 0x08
#define ISREG_IS 0x07
#define ISREG_OK_NO_STOP 0x04
#define ISREG_OK_STOP 0x01
#define CFIREG 0x1C
#define CFIREG_IS 0xE0
#define CFIREG_CF 0x1F
#define SOFREG 0x1C
#define SOFREG_RAD 0xC0
#define SOFREG_RAA 0x30
#define SOFREG_SO 0x0F
#define CNTLREG1 0x20
#define CNTLREG1_ETM 0x80
#define CNTLREG1_DISR 0x40
#define CNTLREG1_PERE 0x10
#define CNTLREG1_SID 0x07
#define CNTLREG2 0x2C
#define CNTLREG2_ENF 0x40
#define CNTLREG3 0x30
#define CNTLREG3_ADIDCHK 0x80
#define CNTLREG3_FASTSCSI 0x10
#define CNTLREG3_FASTCLK 0x08
#define CNTLREG4 0x34
#define CNTLREG4_GLITCH 0xC0
#define CNTLREG4_PWD 0x20
#define CNTLREG4_RAE 0x08
#define CNTLREG4_RADE 0x04
#define CNTLREG4_RES 0x10
#define DMACMD 0x40
#define DMACMD_DIR 0x80
#define DMACMD_INTE_D 0x40
#define DMACMD_INTE_P 0x20
#define DMACMD_MDL 0x10
#define DMACMD_DIAG 0x04
#define DMACMD_IDLE 0x00
#define DMACMD_BLAST 0x01
#define DMACMD_ABORT 0x02
#define DMACMD_START 0x03
#define DMASTATUS 0x54
#define DMASTATUS_BCMPLT 0x20
#define DMASTATUS_SCSIINT 0x10
#define DMASTATUS_DONE 0x08
#define DMASTATUS_ABORT 0x04
#define DMASTATUS_ERROR 0x02
#define DMASTATUS_PWDN 0x02
#define DMASTC 0x44
#define DMASPA 0x48
#define DMAWBC 0x4C
#define DMAWAC 0x50
#define DMASMDLA 0x58
#define DMAWMAC 0x5C
#define PHASE_MSGIN 0x07
#define PHASE_MSGOUT 0x06
#define PHASE_RES_1 0x05
#define PHASE_RES_0 0x04
#define PHASE_STATIN 0x03
#define PHASE_CMDOUT 0x02
#define PHASE_DATAIN 0x01
#define PHASE_DATAOUT 0x00
struct AM53C974_hostdata {
volatile unsigned in_reset:1;
volatile unsigned aborted:1;
volatile unsigned selecting:1;
volatile unsigned disconnecting: 1;
volatile unsigned dma_busy:1;
volatile unsigned char msgout[10];
volatile unsigned char last_message[10];
volatile Scsi_Cmnd *issue_queue;
volatile Scsi_Cmnd *disconnected_queue;
volatile Scsi_Cmnd *sel_cmd;
volatile Scsi_Cmnd *connected;
volatile unsigned char busy[8];
unsigned char sync_per[8];
unsigned char sync_off[8];
unsigned char sync_neg[8];
unsigned char sync_en[8];
unsigned char max_rate[8];
unsigned char max_offset[8];
};
#define AM53C974 { \
NULL, \
NULL, \
NULL, \
NULL, \
"AM53C974", \
AM53C974_detect, \
NULL, \
AM53C974_info, \
AM53C974_command, \
AM53C974_queue_command, \
AM53C974_abort, \
AM53C974_reset, \
NULL, \
scsicam_bios_param, \
12, \
-1, \
SG_ALL, \
1, \
0, \
0, \
DISABLE_CLUSTERING \
}
void AM53C974_setup(char *str, int *ints);
int AM53C974_detect(Scsi_Host_Template *tpnt);
int AM53C974_biosparm(Disk *disk, int dev, int *info_array);
const char *AM53C974_info(struct Scsi_Host *);
int AM53C974_command(Scsi_Cmnd *SCpnt);
int AM53C974_queue_command(Scsi_Cmnd *cmd, void (*done)(Scsi_Cmnd *));
int AM53C974_abort(Scsi_Cmnd *cmd);
int AM53C974_reset (Scsi_Cmnd *cmd, unsigned int flags);
#define AM53C974_local_declare() unsigned long io_port
#define AM53C974_setio(instance) io_port = instance->io_port
#define AM53C974_read_8(addr) inb(io_port + (addr))
#define AM53C974_write_8(addr,x) outb((x), io_port + (addr))
#define AM53C974_read_16(addr) inw(io_port + (addr))
#define AM53C974_write_16(addr,x) outw((x), io_port + (addr))
#define AM53C974_read_32(addr) inl(io_port + (addr))
#define AM53C974_write_32(addr,x) outl((x), io_port + (addr))
#define AM53C974_poll_int() { do { statreg = AM53C974_read_8(STATREG); } \
while (!(statreg & STATREG_INT)) ; \
AM53C974_read_8(INSTREG) ; }
#define AM53C974_cfifo() (AM53C974_read_8(CFIREG) & CFIREG_CF)
#define TAG_NEXT -1
#define TAG_NONE -2
typedef struct _override_t {
int host_scsi_id;
int target_scsi_id;
int max_rate;
int max_offset;
} override_t;
#define AM53C974_PCIREG_OPEN() outb(0xF1, 0xCF8); outb(0, 0xCFA)
#define AM53C974_PCIREG_CLOSE() outb(0, 0xCF8)
#define AM53C974_PCIREG_READ_BYTE(instance,a) ( inb((a) + (instance)->io_port) )
#define AM53C974_PCIREG_READ_WORD(instance,a) ( inw((a) + (instance)->io_port) )
#define AM53C974_PCIREG_READ_DWORD(instance,a) ( inl((a) + (instance)->io_port) )
#define AM53C974_PCIREG_WRITE_BYTE(instance,x,a) ( outb((x), (a) + (instance)->io_port) )
#define AM53C974_PCIREG_WRITE_WORD(instance,x,a) ( outw((x), (a) + (instance)->io_port) )
#define AM53C974_PCIREG_WRITE_DWORD(instance,x,a) ( outl((x), (a) + (instance)->io_port) )
typedef struct _pci_config_t {
union {
unsigned int device_vendor;
struct {
unsigned short vendor;
unsigned short device;
} dv;
} dv_id;
#define _device_vendor dv_id.device_vendor
#define _vendor dv_id.dv.vendor
#define _device dv_id.dv.device
union {
unsigned int status_command;
struct {
unsigned short command;
unsigned short status;
} sc;
} stat_cmd;
#define _status_command stat_cmd.status_command
#define _command stat_cmd.sc.command
#define _status stat_cmd.sc.status
union {
unsigned int class_revision;
struct {
unsigned char rev_id;
unsigned char prog_if;
unsigned char sub_class;
unsigned char base_class;
} cr;
} class_rev;
#define _class_revision class_rev.class_revision
#define _rev_id class_rev.cr.rev_id
#define _prog_if class_rev.cr.prog_if
#define _sub_class class_rev.cr.sub_class
#define _base_class class_rev.cr.base_class
union {
unsigned int bist_header_latency_cache;
struct {
unsigned char cache_line_size;
unsigned char latency_timer;
unsigned char header_type;
unsigned char bist;
} bhlc;
} bhlc;
#define _bist_header_latency_cache bhlc.bist_header_latency_cache
#define _cache_line_size bhlc.bhlc.cache_line_size
#define _latency_timer bhlc.bhlc.latency_timer
#define _header_type bhlc.bhlc.header_type
#define _bist bhlc.bhlc.bist
unsigned int _base0;
unsigned int _base1;
unsigned int _base2;
unsigned int _base3;
unsigned int _base4;
unsigned int _base5;
unsigned int rsvd1;
unsigned int rsvd2;
unsigned int _baserom;
unsigned int rsvd3;
unsigned int rsvd4;
union {
unsigned int max_min_ipin_iline;
struct {
unsigned char int_line;
unsigned char int_pin;
unsigned char min_gnt;
unsigned char max_lat;
} mmii;
} mmii;
#define _max_min_ipin_iline mmii.max_min_ipin_iline
#define _int_line mmii.mmii.int_line
#define _int_pin mmii.mmii.int_pin
#define _min_gnt mmii.mmii.min_gnt
#define _max_lat mmii.mmii.max_lat
unsigned short _ioaddr;
unsigned int _pcibus;
unsigned int _cardnum;
} pci_config_t;
#endif