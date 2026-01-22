#ifndef _GDTH_H
#define _GDTH_H
#include <linux/version.h>
#include <linux/types.h>
#ifndef NULL
#define NULL 0
#endif
#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif
#define GDTH_VERSION_STR        "1.07"
#define GDTH_VERSION            1
#define GDTH_SUBVERSION         7
#define PROTOCOL_VERSION        1
#define GDT_ISA         0x01
#define GDT_EISA        0x02
#define GDT_PCI         0x03
#define GDT_PCINEW      0x04
#define GDT_PCIMPR      0x05
#define GDT3_ID         0x0130941c
#define GDT3A_ID        0x0230941c
#define GDT3B_ID        0x0330941c
#define GDT2_ID         0x0120941c
#ifndef PCI_VENDOR_ID_VORTEX
#define PCI_VENDOR_ID_VORTEX            0x1119
#endif
#ifndef PCI_DEVICE_ID_VORTEX_GDT60x0
#define PCI_DEVICE_ID_VORTEX_GDT60x0    0
#define PCI_DEVICE_ID_VORTEX_GDT6000B   1
#define PCI_DEVICE_ID_VORTEX_GDT6x10    2
#define PCI_DEVICE_ID_VORTEX_GDT6x20    3
#define PCI_DEVICE_ID_VORTEX_GDT6530    4
#define PCI_DEVICE_ID_VORTEX_GDT6550    5
#define PCI_DEVICE_ID_VORTEX_GDT6x17    6
#define PCI_DEVICE_ID_VORTEX_GDT6x27    7
#define PCI_DEVICE_ID_VORTEX_GDT6537    8
#define PCI_DEVICE_ID_VORTEX_GDT6557    9
#define PCI_DEVICE_ID_VORTEX_GDT6x15    10
#define PCI_DEVICE_ID_VORTEX_GDT6x25    11
#define PCI_DEVICE_ID_VORTEX_GDT6535    12
#define PCI_DEVICE_ID_VORTEX_GDT6555    13
#endif
#ifndef PCI_DEVICE_ID_VORTEX_GDT6x17RP
#define PCI_DEVICE_ID_VORTEX_GDT6x17RP  0x100
#define PCI_DEVICE_ID_VORTEX_GDT6x27RP  0x101
#define PCI_DEVICE_ID_VORTEX_GDT6537RP  0x102
#define PCI_DEVICE_ID_VORTEX_GDT6557RP  0x103
#define PCI_DEVICE_ID_VORTEX_GDT6x11RP  0x104
#define PCI_DEVICE_ID_VORTEX_GDT6x21RP  0x105
#define PCI_DEVICE_ID_VORTEX_GDT6x17RP1 0x110
#define PCI_DEVICE_ID_VORTEX_GDT6x27RP1 0x111
#define PCI_DEVICE_ID_VORTEX_GDT6537RP1 0x112
#define PCI_DEVICE_ID_VORTEX_GDT6557RP1 0x113
#define PCI_DEVICE_ID_VORTEX_GDT6x11RP1 0x114
#define PCI_DEVICE_ID_VORTEX_GDT6x21RP1 0x115
#define PCI_DEVICE_ID_VORTEX_GDT6x17RP2 0x120
#define PCI_DEVICE_ID_VORTEX_GDT6x27RP2 0x121
#define PCI_DEVICE_ID_VORTEX_GDT6537RP2 0x122
#define PCI_DEVICE_ID_VORTEX_GDT6557RP2 0x123
#define PCI_DEVICE_ID_VORTEX_GDT6x11RP2 0x124
#define PCI_DEVICE_ID_VORTEX_GDT6x21RP2 0x125
#endif
#ifndef PCI_DEVICE_ID_VORTEX_GDT6519RD
#define PCI_DEVICE_ID_VORTEX_GDT6519RD  0x210
#define PCI_DEVICE_ID_VORTEX_GDT6529RD  0x211
#endif
#ifndef PCI_DEVICE_ID_VORTEX_GDTMAXRP
#define PCI_DEVICE_ID_VORTEX_GDTMAXRP  	0x2ff
#endif
#define GDTH_SCRATCH    4096
#define GDTH_MAXCMDS    124
#define GDTH_MAXC_P_L   16
#define MAXOFFSETS      128
#define MAXHA           8
#define MAXID           16
#define MAXLUN          8
#define MAXBUS          6
#define MAX_HDRIVES     35
#define MAX_EVENTS      100
#define MAXCYLS         1024
#define HEADS           64
#define SECS            32
#define MEDHEADS        127
#define MEDSECS         63
#define BIGHEADS        255
#define BIGSECS         63
#define UNUSED_CMND     ((Scsi_Cmnd *)-1)
#define INTERNAL_CMND   ((Scsi_Cmnd *)-2)
#define SCREEN_CMND     ((Scsi_Cmnd *)-3)
#define SPECIAL_SCP(p)  (p==UNUSED_CMND || p==INTERNAL_CMND || p==SCREEN_CMND)
#define EMPTY_DTYP      0
#define CACHE_DTYP      1
#define RAW_DTYP        2
#define SIOP_DTYP       3
#define SCSIRAWSERVICE  3
#define CACHESERVICE    9
#define SCREENSERVICE   11
#define MSG_INV_HANDLE  -1
#define MSGLEN          16
#define MSG_SIZE        34
#define MSG_REQUEST     0
#define SECTOR_SIZE     0x200
#define DPMEM_MAGIC	0xC0FFEE11
#define IC_HEADER_BYTES 48
#define IC_QUEUE_BYTES  4
#define DPMEM_COMMAND_OFFSET    IC_HEADER_BYTES+IC_QUEUE_BYTES*MAXOFFSETS
#define GDT_INIT        0
#define GDT_READ        1
#define GDT_WRITE       2
#define GDT_INFO        3
#define GDT_FLUSH       4
#define GDT_IOCTL       5
#define GDT_DEVTYPE     9
#define GDT_MOUNT       10
#define GDT_UNMOUNT     11
#define GDT_SET_FEAT    12
#define GDT_GET_FEAT    13
#define GDT_RESERVE     14
#define GDT_WRITE_THR   16
#define GDT_EXT_INFO    18
#define GDT_RESET       19
#define SCSI_CHAN_CNT   5
#define GET_IOCHAN_DESC	0x5e
#define L_CTRL_PATTERN  0x20000000L
#define CACHE_INFO      4
#define CACHE_CONFIG    5
#define BOARD_INFO	0x28
#define IO_CHANNEL      0x00020000L
#define INVALID_CHANNEL 0x0000ffffL
#define GDTIOCTL_MASK       ('J'<<8)
#define GDTIOCTL_GENERAL    (GDTIOCTL_MASK | 0)
#define GDTIOCTL_DRVERS     (GDTIOCTL_MASK | 1)
#define GDTIOCTL_CTRTYPE    (GDTIOCTL_MASK | 2)
#define GDTIOCTL_CTRCNT     (GDTIOCTL_MASK | 5)
#define GDTIOCTL_LOCKDRV    (GDTIOCTL_MASK | 6)
#define GDTIOCTL_LOCKCHN    (GDTIOCTL_MASK | 7)
#define GDTIOCTL_EVENT      (GDTIOCTL_MASK | 8)
#define S_OK            1
#define S_BSY           7
#define S_RAW_SCSI      12
#define S_RAW_ILL       0xff
#define INIT_RETRIES    10000
#define INIT_TIMEOUT    100000
#define POLL_TIMEOUT    10000
#define DEFAULT_PRI     0x20
#define IOCTL_PRI       0x10
#define DATA_IN         0x01000000L
#define DATA_OUT        0x00000000L
#define ID0REG          0x0c80
#define EINTENABREG     0x0c89
#define SEMA0REG        0x0c8a
#define SEMA1REG        0x0c8b
#define LDOORREG        0x0c8d
#define EDENABREG       0x0c8e
#define EDOORREG        0x0c8f
#define MAILBOXREG      0x0c90
#define EISAREG         0x0cc0
#define LINUX_OS        8
#define SCATTER_GATHER  1
#define GDTH_MAXSG      32
#define SECS32          0x1f
#define BIOS_ID_OFFS    0x10
#define LOCALBOARD      0
#define ASYNCINDEX      0
#define SPEZINDEX       1
#define GDT_WR_THROUGH  0x100
#pragma pack(1)
typedef struct {
char        buffer[GDTH_SCRATCH];
} gdth_scratch_str;
typedef struct {
ulong       msg_handle;
ulong       msg_len;
ulong       msg_alen;
unchar      msg_answer;
unchar      msg_ext;
unchar      msg_reserved[2];
char        msg_text[MSGLEN+2];
} gdth_msg_str;
typedef struct {
ulong       channel_no;
ulong       drive_cnt;
unchar      siop_id;
unchar      siop_state;
} gdth_getch_str;
typedef struct {
ulong	version;
unchar	list_entries;
unchar	first_chan;
unchar	last_chan;
unchar	chan_count;
ulong	list_offset;
struct {
unchar	proc_id;
unchar	proc_defect;
unchar	reserved[2];
} list[MAXBUS];
} gdth_iochan_str;
typedef struct {
ulong       version;
ushort      state;
ushort      strategy;
ushort      write_back;
ushort      block_size;
} gdth_cpar_str;
typedef struct {
ulong       csize;
ulong       read_cnt;
ulong       write_cnt;
ulong       tr_hits;
ulong       sec_hits;
ulong       sec_miss;
} gdth_cstat_str;
typedef struct {
gdth_cpar_str   cpar;
gdth_cstat_str  cstat;
} gdth_cinfo_str;
typedef struct {
ulong	ser_no;
unchar	oem_id[2];
ushort	ep_flags;
ulong	proc_id;
ulong	memsize;
unchar	mem_banks;
unchar	chan_type;
unchar	chan_count;
unchar	rdongle_pres;
ulong	epr_fw_ver;
ulong	upd_fw_ver;
ulong	upd_revision;
char	type_string[16];
char	raid_string[16];
unchar	update_pres;
unchar	xor_pres;
unchar	prom_type;
unchar	prom_count;
ulong	dup_pres;
ulong	chan_pres;
ulong	mem_pres;
unchar	ft_bus_system;
unchar	subtype_valid;
unchar	board_subtype;
unchar	ramparity_pres;
} gdth_binfo_str;
typedef struct {
ulong       sg_ptr;
ulong       sg_len;
} gdth_sg_str;
typedef struct {
ulong       BoardNode;
ulong       CommandIndex;
ushort      OpCode;
union {
struct {
ushort      DeviceNo;
ulong       BlockNo;
ulong       BlockCnt;
ulong       DestAddr;
ulong       sg_canz;
gdth_sg_str sg_lst[GDTH_MAXSG];
} cache;
struct {
ushort      param_size;
ulong       subfunc;
ulong       channel;
ulong       p_param;
} ioctl;
struct {
ushort      reserved;
ulong       msg_handle;
ulong       msg_addr;
} screen;
struct {
ushort      reserved;
ulong       direction;
ulong       mdisc_time;
ulong       mcon_time;
ulong       sdata;
ulong       sdlen;
ulong       clen;
unchar      cmd[12];
unchar      target;
unchar      lun;
unchar      bus;
unchar      priority;
ulong       sense_len;
ulong       sense_data;
struct raw  *link_p;
ulong       sg_ranz;
gdth_sg_str sg_lst[GDTH_MAXSG];
} raw;
} u;
unchar      Service;
ushort      Status;
ulong       Info;
Scsi_Cmnd   *RequestBuffer;
} gdth_cmd_str;
#define ES_ASYNC    1
#define ES_DRIVER   2
#define ES_TEST     3
#define ES_SYNC     4
typedef struct {
ushort                  size;
union {
char                stream[16];
struct {
ushort          ionode;
ushort          service;
ulong           index;
} driver;
struct {
ushort          ionode;
ushort          service;
ushort          status;
ulong           info;
unchar          scsi_coord[3];
} async;
struct {
ushort          ionode;
ushort          service;
ushort          status;
ulong           info;
ushort          hostdrive;
unchar          scsi_coord[3];
unchar          sense_key;
} sync;
struct {
ulong           l1, l2, l3, l4;
} test;
} eu;
} gdth_evt_data;
typedef struct {
ulong           first_stamp;
ulong           last_stamp;
ushort          same_count;
ushort          event_source;
ushort          event_idx;
unchar          application;
unchar          reserved;
gdth_evt_data   event_data;
} gdth_evt_str;
typedef struct {
unchar              S_Cmd_Indx;
unchar volatile     S_Status;
ushort              reserved1;
ulong               S_Info[4];
unchar volatile     Sema0;
unchar              reserved2[3];
unchar              Cmd_Index;
unchar              reserved3[3];
ushort volatile     Status;
ushort              Service;
ulong               Info[2];
struct {
ushort          offset;
ushort          serv_id;
} comm_queue[MAXOFFSETS];
ulong               bios_reserved[2];
unchar              gdt_dpr_cmd[1];
} gdt_dpr_if;
typedef struct {
ulong       magic;
ushort      need_deinit;
unchar      switch_support;
unchar      padding[9];
unchar      os_used[16];
unchar      unused[28];
unchar      fw_magic;
} gdt_pci_sram;
typedef struct {
unchar      os_used[16];
ushort      need_deinit;
unchar      switch_support;
unchar      padding;
} gdt_eisa_sram;
typedef struct {
union {
struct {
unchar      bios_used[0x3c00-32];
ulong       magic;
ushort      need_deinit;
unchar      switch_support;
unchar      padding[9];
unchar      os_used[16];
} dp_sram;
unchar          bios_area[0x4000];
} bu;
union {
gdt_dpr_if      ic;
unchar          if_area[0x3000];
} u;
struct {
unchar          memlock;
unchar          event;
unchar          irqen;
unchar          irqdel;
unchar volatile Sema1;
unchar          rq;
} io;
} gdt2_dpram_str;
typedef struct {
union {
gdt_dpr_if      ic;
unchar          if_area[0xff0-sizeof(gdt_pci_sram)];
} u;
gdt_pci_sram        gdt6sr;
struct {
unchar          unused0[1];
unchar volatile Sema1;
unchar          unused1[3];
unchar          irqen;
unchar          unused2[2];
unchar          event;
unchar          unused3[3];
unchar          irqdel;
unchar          unused4[3];
} io;
} gdt6_dpram_str;
typedef struct {
unchar              cfg_reg;
unchar              unused1[0x3f];
unchar volatile     sema0_reg;
unchar volatile     sema1_reg;
unchar              unused2[2];
ushort volatile     status;
ushort              service;
ulong               info[2];
unchar              unused3[0x10];
unchar              ldoor_reg;
unchar              unused4[3];
unchar volatile     edoor_reg;
unchar              unused5[3];
unchar              control0;
unchar              control1;
unchar              unused6[0x16];
} gdt6c_plx_regs;
typedef struct {
union {
gdt_dpr_if      ic;
unchar          if_area[0x4000-sizeof(gdt_pci_sram)];
} u;
gdt_pci_sram        gdt6sr;
} gdt6c_dpram_str;
typedef struct {
unchar              unused1[16];
unchar volatile     sema0_reg;
unchar              unused2;
unchar volatile     sema1_reg;
unchar              unused3;
ushort volatile     status;
ushort              service;
ulong               info[2];
unchar              ldoor_reg;
unchar              unused4[11];
unchar volatile     edoor_reg;
unchar              unused5[7];
unchar              edoor_en_reg;
unchar              unused6[27];
ulong               unused7[1004];
} gdt6m_i960_regs;
typedef struct {
gdt6m_i960_regs     i960r;
union {
gdt_dpr_if      ic;
unchar          if_area[0x3000-sizeof(gdt_pci_sram)];
} u;
gdt_pci_sram        gdt6sr;
} gdt6m_dpram_str;
typedef struct {
ushort      device_id;
unchar      bus;
unchar      device_fn;
ulong       dpmem;
ulong       io;
ulong       io_mm;
ulong       bios;
unchar      irq;
} gdth_pci_str;
typedef struct {
unchar              bus_cnt;
unchar              type;
ushort              raw_feat;
ulong               stype;
ushort              cache_feat;
ushort		bmic;
void               	*brd;
ulong               brd_phys;
gdt6c_plx_regs      *plx;
gdth_cmd_str        *pccb;
gdth_scratch_str    *pscratch;
unchar              irq;
unchar              drq;
ushort              status;
ulong               info;
ulong               info2;
Scsi_Cmnd           *req_first;
struct {
unchar          type;
unchar          heads;
unchar          secs;
unchar          lock;
ushort          hostdrive;
ushort          devtype;
ulong           size;
} id[MAXBUS][MAXID];
ushort              cmd_cnt;
ushort              cmd_len;
ushort              cmd_offs_dpmem;
ushort              ic_all_size;
unchar              reserved;
unchar              mode;
ushort              param_size;
gdth_cpar_str       cpar;
char		ctr_name[16];
} gdth_ha_str;
typedef struct {
ushort      hanum;
ushort      busnum;
} gdth_num_str;
typedef struct {
gdth_num_str        numext;
gdth_ha_str         haext;
gdth_cmd_str        cmdext;
gdth_scratch_str    dmaext;
} gdth_ext_str;
typedef struct {
unchar      type_qual;
unchar      modif_rmb;
unchar      version;
unchar      resp_aenc;
unchar      add_length;
unchar      reserved1;
unchar      reserved2;
unchar      misc;
unchar      vendor[8];
unchar      product[16];
unchar      revision[4];
} gdth_inq_data;
typedef struct {
ulong       last_block_no;
ulong       block_length;
} gdth_rdcap_data;
typedef struct {
unchar      errorcode;
unchar      segno;
unchar      key;
ulong       info;
unchar      add_length;
ulong       cmd_info;
unchar      adsc;
unchar      adsq;
unchar      fruc;
unchar      key_spec[3];
} gdth_sense_data;
typedef struct {
struct {
unchar  data_length;
unchar  med_type;
unchar  dev_par;
unchar  bd_length;
} hd;
struct {
unchar  dens_code;
unchar  block_count[3];
unchar  reserved;
unchar  block_length[3];
} bd;
} gdth_modep_data;
typedef struct {
ulong       b[10];
} gdth_stackframe;
#pragma pack()
typedef struct {
unchar	hanum;
unchar	bus;
unchar	id;
} gdth_reserve_str;
int gdth_detect(Scsi_Host_Template *);
int gdth_release(struct Scsi_Host *);
int gdth_command(Scsi_Cmnd *);
int gdth_queuecommand(Scsi_Cmnd *,void (*done)(Scsi_Cmnd *));
int gdth_abort(Scsi_Cmnd *);
#if LINUX_VERSION_CODE >= 0x010346
int gdth_reset(Scsi_Cmnd *, unsigned int reset_flags);
#else
int gdth_reset(Scsi_Cmnd *);
#endif
const char *gdth_info(struct Scsi_Host *);
#if LINUX_VERSION_CODE >= 0x02015F
int gdth_bios_param(Disk *,kdev_t,int *);
extern struct proc_dir_entry proc_scsi_gdth;
int gdth_proc_info(char *,char **,off_t,int,int,int);
int gdth_eh_abort(Scsi_Cmnd *scp);
int gdth_eh_device_reset(Scsi_Cmnd *scp);
int gdth_eh_bus_reset(Scsi_Cmnd *scp);
int gdth_eh_host_reset(Scsi_Cmnd *scp);
#define GDTH { proc_dir:        &proc_scsi_gdth,                 \
proc_info:       gdth_proc_info,                  \
name:            "GDT SCSI Disk Array Controller",\
detect:          gdth_detect,                     \
release:         gdth_release,                    \
info:            gdth_info,                       \
command:         gdth_command,                    \
queuecommand:    gdth_queuecommand,               \
eh_abort_handler: gdth_eh_abort,                  \
eh_device_reset_handler: gdth_eh_device_reset,    \
eh_bus_reset_handler: gdth_eh_bus_reset,          \
eh_host_reset_handler: gdth_eh_host_reset,        \
abort:           gdth_abort,                      \
reset:           gdth_reset,                      \
bios_param:      gdth_bios_param,                 \
can_queue:       GDTH_MAXCMDS,                    \
this_id:         -1,                              \
sg_tablesize:    GDTH_MAXSG,                      \
cmd_per_lun:     GDTH_MAXC_P_L,                   \
present:         0,                               \
unchecked_isa_dma: 1,                             \
use_clustering:  ENABLE_CLUSTERING,               \
use_new_eh_code: 1        }
#elif LINUX_VERSION_CODE >= 0x010300
int gdth_bios_param(Disk *,kdev_t,int *);
extern struct proc_dir_entry proc_scsi_gdth;
int gdth_proc_info(char *,char **,off_t,int,int,int);
#define GDTH { NULL, NULL,                              \
&proc_scsi_gdth,                     \
gdth_proc_info,                      \
"GDT SCSI Disk Array Controller",    \
gdth_detect,                         \
gdth_release,                        \
gdth_info,                           \
gdth_command,                        \
gdth_queuecommand,                   \
gdth_abort,                          \
gdth_reset,                          \
NULL,                                \
gdth_bios_param,                     \
GDTH_MAXCMDS,                        \
-1,                                  \
GDTH_MAXSG,                          \
GDTH_MAXC_P_L,                       \
0,                                   \
1,                                   \
ENABLE_CLUSTERING}
#else
int gdth_bios_param(Disk *,int,int *);
#define GDTH { NULL, NULL,                              \
"GDT SCSI Disk Array Controller",    \
gdth_detect,                         \
gdth_release,                        \
gdth_info,                           \
gdth_command,                        \
gdth_queuecommand,                   \
gdth_abort,                          \
gdth_reset,                          \
NULL,                                \
gdth_bios_param,                     \
GDTH_MAXCMDS,                        \
-1,                                  \
GDTH_MAXSG,                          \
GDTH_MAXC_P_L,                       \
0,                                   \
1,                                   \
ENABLE_CLUSTERING}
#endif
#endif