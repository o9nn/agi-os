#ifndef NCR53C8XX_H
#define NCR53C8XX_H
#define SCSI_NCR_DRIVER_NAME "ncr53c8xx - revision 2.5f.1"
#if !defined(LINUX_VERSION_CODE)
#include <linux/version.h>
#endif
#include <linux/config.h>
#define LinuxVersionCode(v, p, s) (((v)<<16)+((p)<<8)+(s))
#if !defined(LINUX_VERSION_CODE)
#define LINUX_VERSION_CODE LinuxVersionCode(1,2,13)
#endif
#if LINUX_VERSION_CODE < LinuxVersionCode(1,3,0)
# define SCSI_NCR_IOMAPPED
#endif
#if LINUX_VERSION_CODE >= LinuxVersionCode(1,3,0)
#endif
#if LINUX_VERSION_CODE >= LinuxVersionCode(1,3,72)
# define SCSI_NCR_SHARE_IRQ
#endif
#define SCSI_NCR_BOOT_COMMAND_LINE_SUPPORT
#define SCSI_NCR_DEBUG_INFO_SUPPORT
#define SCSI_NCR_PCI_FIX_UP_SUPPORT
#ifdef SCSI_NCR_PROC_INFO_SUPPORT
# define SCSI_NCR_PROFILE_SUPPORT
# define SCSI_NCR_USER_COMMAND_SUPPORT
# define SCSI_NCR_USER_INFO_SUPPORT
#endif
#ifdef CONFIG_SCSI_NCR53C8XX_NVRAM_DETECT
#define SCSI_NCR_NVRAM_SUPPORT
#endif
#define SCSI_NCR_SETUP_SPECIAL_FEATURES (3)
#define SCSI_NCR_SETUP_ULTRA_SCSI (2)
#define SCSI_NCR_MAX_SYNC (40)
#ifdef CONFIG_SCSI_NCR53C8XX_MAX_TAGS
#if CONFIG_SCSI_NCR53C8XX_MAX_TAGS < 2
#define SCSI_NCR_MAX_TAGS (2)
#elif CONFIG_SCSI_NCR53C8XX_MAX_TAGS > 12
#define SCSI_NCR_MAX_TAGS (12)
#else
#define SCSI_NCR_MAX_TAGS CONFIG_SCSI_NCR53C8XX_MAX_TAGS
#endif
#else
#define SCSI_NCR_MAX_TAGS (4)
#endif
#ifdef CONFIG_SCSI_NCR53C8XX_TAGGED_QUEUE
#define SCSI_NCR_SETUP_DEFAULT_TAGS SCSI_NCR_MAX_TAGS
#else
#define SCSI_NCR_SETUP_DEFAULT_TAGS (0)
#endif
#if defined(CONFIG_SCSI_NCR53C8XX_IOMAPPED)
#define SCSI_NCR_IOMAPPED
#elif defined(__alpha__) || defined(__powerpc__)
#define SCSI_NCR_IOMAPPED
#endif
#ifndef CONFIG_SCSI_NCR53C8XX_SYNC
#define CONFIG_SCSI_NCR53C8XX_SYNC (5)
#elif CONFIG_SCSI_NCR53C8XX_SYNC > SCSI_NCR_MAX_SYNC
#define SCSI_NCR_SETUP_DEFAULT_SYNC SCSI_NCR_MAX_SYNC
#endif
#if CONFIG_SCSI_NCR53C8XX_SYNC == 0
#define SCSI_NCR_SETUP_DEFAULT_SYNC (255)
#elif CONFIG_SCSI_NCR53C8XX_SYNC <= 5
#define SCSI_NCR_SETUP_DEFAULT_SYNC (50)
#elif CONFIG_SCSI_NCR53C8XX_SYNC <= 20
#define SCSI_NCR_SETUP_DEFAULT_SYNC (250/(CONFIG_SCSI_NCR53C8XX_SYNC))
#elif CONFIG_SCSI_NCR53C8XX_SYNC <= 33
#define SCSI_NCR_SETUP_DEFAULT_SYNC (11)
#else
#define SCSI_NCR_SETUP_DEFAULT_SYNC (10)
#endif
#ifdef CONFIG_SCSI_NCR53C8XX_NO_DISCONNECT
#define SCSI_NCR_SETUP_DISCONNECTION (0)
#else
#define SCSI_NCR_SETUP_DISCONNECTION (1)
#endif
#ifdef CONFIG_SCSI_NCR53C8XX_FORCE_SYNC_NEGO
#define SCSI_NCR_SETUP_FORCE_SYNC_NEGO (1)
#else
#define SCSI_NCR_SETUP_FORCE_SYNC_NEGO (0)
#endif
#ifdef CONFIG_SCSI_NCR53C8XX_DISABLE_MPARITY_CHECK
#define SCSI_NCR_SETUP_MASTER_PARITY (0)
#else
#define SCSI_NCR_SETUP_MASTER_PARITY (1)
#endif
#ifdef CONFIG_SCSI_NCR53C8XX_DISABLE_PARITY_CHECK
#define SCSI_NCR_SETUP_SCSI_PARITY (0)
#else
#define SCSI_NCR_SETUP_SCSI_PARITY (1)
#endif
#ifdef CONFIG_SCSI_NCR53C8XX_SYMBIOS_COMPAT
#define SCSI_NCR_SETUP_LED_PIN (1)
#define SCSI_NCR_SETUP_DIFF_SUPPORT (3)
#else
#define SCSI_NCR_SETUP_LED_PIN (0)
#define SCSI_NCR_SETUP_DIFF_SUPPORT (0)
#endif
#define SCSI_NCR_SETUP_SETTLE_TIME (2)
#define SCSI_NCR_ALWAYS_SIMPLE_TAG
#define SCSI_NCR_MAX_SCATTER (127)
#define SCSI_NCR_MAX_TARGET (16)
#define SCSI_NCR_MAX_HOST (2)
#define SCSI_NCR_TIMEOUT_ALERT (3*HZ)
#define SCSI_NCR_CAN_QUEUE (7*SCSI_NCR_MAX_TAGS)
#define SCSI_NCR_CMD_PER_LUN (SCSI_NCR_MAX_TAGS)
#define SCSI_NCR_SG_TABLESIZE (SCSI_NCR_MAX_SCATTER)
#define SCSI_NCR_TIMER_INTERVAL ((HZ+5-1)/5)
#if 1
#define SCSI_NCR_MAX_LUN (8)
#else
#define SCSI_NCR_MAX_LUN (1)
#endif
#if defined(HOSTS_C) || defined(MODULE)
#if LINUX_VERSION_CODE >= LinuxVersionCode(1,3,98)
#include <scsi/scsicam.h>
#else
#include <linux/scsicam.h>
#endif
int ncr53c8xx_abort(Scsi_Cmnd *);
int ncr53c8xx_detect(Scsi_Host_Template *tpnt);
int ncr53c8xx_queue_command(Scsi_Cmnd *, void (*done)(Scsi_Cmnd *));
#if LINUX_VERSION_CODE >= LinuxVersionCode(1,3,98)
int ncr53c8xx_reset(Scsi_Cmnd *, unsigned int);
#else
int ncr53c8xx_reset(Scsi_Cmnd *);
#endif
#ifdef MODULE
int ncr53c8xx_release(struct Scsi_Host *);
#else
#define ncr53c8xx_release NULL
#endif
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,1,75)
#define NCR53C8XX { name: SCSI_NCR_DRIVER_NAME, \
detect: ncr53c8xx_detect, \
release: ncr53c8xx_release, \
queuecommand: ncr53c8xx_queue_command,\
abort: ncr53c8xx_abort, \
reset: ncr53c8xx_reset, \
bios_param: scsicam_bios_param, \
can_queue: SCSI_NCR_CAN_QUEUE, \
this_id: 7, \
sg_tablesize: SCSI_NCR_SG_TABLESIZE, \
cmd_per_lun: SCSI_NCR_CMD_PER_LUN, \
use_clustering: DISABLE_CLUSTERING}
#elif LINUX_VERSION_CODE >= LinuxVersionCode(1,3,0)
#define NCR53C8XX { NULL, NULL, NULL, NULL, \
SCSI_NCR_DRIVER_NAME, ncr53c8xx_detect, \
ncr53c8xx_release, NULL, NULL, \
ncr53c8xx_queue_command,ncr53c8xx_abort, \
ncr53c8xx_reset, NULL, scsicam_bios_param, \
SCSI_NCR_CAN_QUEUE, 7, \
SCSI_NCR_SG_TABLESIZE, SCSI_NCR_CMD_PER_LUN, \
0, 0, DISABLE_CLUSTERING}
#else
#define NCR53C8XX { NULL, NULL, \
SCSI_NCR_DRIVER_NAME, ncr53c8xx_detect, \
ncr53c8xx_release, NULL, NULL, \
ncr53c8xx_queue_command,ncr53c8xx_abort, \
ncr53c8xx_reset, NULL, scsicam_bios_param, \
SCSI_NCR_CAN_QUEUE, 7, \
SCSI_NCR_SG_TABLESIZE, SCSI_NCR_CMD_PER_LUN, \
0, 0, DISABLE_CLUSTERING}
#endif
#endif
#ifndef HOSTS_C
#ifdef __BIG_ENDIAN
#if LINUX_VERSION_CODE < LinuxVersionCode(2,1,0)
#error "BIG ENDIAN byte ordering needs kernel version >= 2.1.0"
#endif
#ifdef __powerpc__
#define inw_l2b inw
#define inl_l2b inl
#define outw_b2l outw
#define outl_b2l outl
#else
#error "Support for BIG ENDIAN is only available for the PowerPC"
#endif
#else
#define inw_raw inw
#define inl_raw inl
#define outw_raw outw
#define outl_raw outl
#define readw_raw readw
#define readl_raw readl
#define writew_raw writew
#define writel_raw writel
#endif
#ifdef SCSI_NCR_BIG_ENDIAN
#error "The NCR in BIG ENDIAN adressing mode is not (yet) supported"
#endif
#ifndef PCI_DEVICE_ID_NCR_53C810
#define PCI_DEVICE_ID_NCR_53C810 1
#endif
#ifndef PCI_DEVICE_ID_NCR_53C810AP
#define PCI_DEVICE_ID_NCR_53C810AP 5
#endif
#ifndef PCI_DEVICE_ID_NCR_53C815
#define PCI_DEVICE_ID_NCR_53C815 4
#endif
#ifndef PCI_DEVICE_ID_NCR_53C820
#define PCI_DEVICE_ID_NCR_53C820 2
#endif
#ifndef PCI_DEVICE_ID_NCR_53C825
#define PCI_DEVICE_ID_NCR_53C825 3
#endif
#ifndef PCI_DEVICE_ID_NCR_53C860
#define PCI_DEVICE_ID_NCR_53C860 6
#endif
#ifndef PCI_DEVICE_ID_NCR_53C875
#define PCI_DEVICE_ID_NCR_53C875 0xf
#endif
#ifndef PCI_DEVICE_ID_NCR_53C875J
#define PCI_DEVICE_ID_NCR_53C875J 0x8f
#endif
#ifndef PCI_DEVICE_ID_NCR_53C885
#define PCI_DEVICE_ID_NCR_53C885 0xd
#endif
#ifndef PCI_DEVICE_ID_NCR_53C895
#define PCI_DEVICE_ID_NCR_53C895 0xc
#endif
#ifndef PCI_DEVICE_ID_NCR_53C896
#define PCI_DEVICE_ID_NCR_53C896 0xb
#endif
typedef struct {
unsigned short device_id;
unsigned short revision_id;
char *name;
unsigned char burst_max;
unsigned char offset_max;
unsigned char nr_divisor;
unsigned int features;
#define FE_LED0 (1<<0)
#define FE_WIDE (1<<1)
#define FE_ULTRA (1<<2)
#define FE_ULTRA2 (1<<3)
#define FE_DBLR (1<<4)
#define FE_QUAD (1<<5)
#define FE_ERL (1<<6)
#define FE_CLSE (1<<7)
#define FE_WRIE (1<<8)
#define FE_ERMP (1<<9)
#define FE_BOF (1<<10)
#define FE_DFS (1<<11)
#define FE_PFEN (1<<12)
#define FE_LDSTR (1<<13)
#define FE_RAM (1<<14)
#define FE_CLK80 (1<<15)
#define FE_CACHE_SET (FE_ERL|FE_CLSE|FE_WRIE|FE_ERMP)
#define FE_SCSI_SET (FE_WIDE|FE_ULTRA|FE_ULTRA2|FE_DBLR|FE_QUAD|F_CLK80)
#define FE_SPECIAL_SET (FE_CACHE_SET|FE_BOF|FE_DFS|FE_LDSTR|FE_PFEN|FE_RAM)
} ncr_chip;
#define FE_CACHE0_SET (FE_CACHE_SET & ~FE_ERL)
#define SCSI_NCR_CHIP_TABLE \
{ \
{PCI_DEVICE_ID_NCR_53C810, 0x0f, "810", 4, 8, 4, \
FE_ERL} \
, \
{PCI_DEVICE_ID_NCR_53C810, 0xff, "810a", 4, 8, 4, \
FE_CACHE_SET|FE_LDSTR|FE_PFEN|FE_BOF} \
, \
{PCI_DEVICE_ID_NCR_53C815, 0xff, "815", 4, 8, 4, \
FE_ERL|FE_BOF} \
, \
{PCI_DEVICE_ID_NCR_53C820, 0xff, "820", 4, 8, 4, \
FE_WIDE|FE_ERL} \
, \
{PCI_DEVICE_ID_NCR_53C825, 0x0f, "825", 4, 8, 4, \
FE_WIDE|FE_ERL|FE_BOF} \
, \
{PCI_DEVICE_ID_NCR_53C825, 0xff, "825a", 6, 8, 4, \
FE_WIDE|FE_CACHE0_SET|FE_BOF|FE_DFS|FE_LDSTR|FE_PFEN|FE_RAM} \
, \
{PCI_DEVICE_ID_NCR_53C860, 0xff, "860", 4, 8, 5, \
FE_ULTRA|FE_CLK80|FE_CACHE_SET|FE_BOF|FE_LDSTR|FE_PFEN} \
, \
{PCI_DEVICE_ID_NCR_53C875, 0x01, "875", 6, 16, 5, \
FE_WIDE|FE_ULTRA|FE_CLK80|FE_CACHE0_SET|FE_BOF|FE_DFS|FE_LDSTR|FE_PFEN|FE_RAM}\
, \
{PCI_DEVICE_ID_NCR_53C875, 0xff, "875", 6, 16, 5, \
FE_WIDE|FE_ULTRA|FE_DBLR|FE_CACHE0_SET|FE_BOF|FE_DFS|FE_LDSTR|FE_PFEN|FE_RAM}\
, \
{PCI_DEVICE_ID_NCR_53C875J,0xff, "875J", 6, 16, 5, \
FE_WIDE|FE_ULTRA|FE_DBLR|FE_CACHE0_SET|FE_BOF|FE_DFS|FE_LDSTR|FE_PFEN|FE_RAM}\
, \
{PCI_DEVICE_ID_NCR_53C885, 0xff, "885", 6, 16, 5, \
FE_WIDE|FE_ULTRA|FE_DBLR|FE_CACHE0_SET|FE_BOF|FE_DFS|FE_LDSTR|FE_PFEN|FE_RAM}\
, \
{PCI_DEVICE_ID_NCR_53C895, 0xff, "895", 7, 31, 7, \
FE_WIDE|FE_ULTRA2|FE_QUAD|FE_CACHE_SET|FE_BOF|FE_DFS|FE_LDSTR|FE_PFEN|FE_RAM}\
, \
{PCI_DEVICE_ID_NCR_53C896, 0xff, "896", 7, 31, 7, \
FE_WIDE|FE_ULTRA2|FE_QUAD|FE_CACHE_SET|FE_BOF|FE_DFS|FE_LDSTR|FE_PFEN|FE_RAM}\
}
#define SCSI_NCR_CHIP_IDS \
{ \
PCI_DEVICE_ID_NCR_53C810, \
PCI_DEVICE_ID_NCR_53C815, \
PCI_DEVICE_ID_NCR_53C820, \
PCI_DEVICE_ID_NCR_53C825, \
PCI_DEVICE_ID_NCR_53C860, \
PCI_DEVICE_ID_NCR_53C875, \
PCI_DEVICE_ID_NCR_53C875J, \
PCI_DEVICE_ID_NCR_53C885, \
PCI_DEVICE_ID_NCR_53C895, \
PCI_DEVICE_ID_NCR_53C896 \
}
#define SCSI_NCR_DRIVER_SETUP \
{ \
SCSI_NCR_SETUP_MASTER_PARITY, \
SCSI_NCR_SETUP_SCSI_PARITY, \
SCSI_NCR_SETUP_DISCONNECTION, \
SCSI_NCR_SETUP_SPECIAL_FEATURES, \
SCSI_NCR_SETUP_ULTRA_SCSI, \
SCSI_NCR_SETUP_FORCE_SYNC_NEGO, \
0, \
0, \
1, \
1, \
SCSI_NCR_SETUP_DEFAULT_TAGS, \
SCSI_NCR_SETUP_DEFAULT_SYNC, \
0x00, \
7, \
SCSI_NCR_SETUP_LED_PIN, \
1, \
SCSI_NCR_SETUP_SETTLE_TIME, \
SCSI_NCR_SETUP_DIFF_SUPPORT, \
0, \
1 \
}
#define SCSI_NCR_DRIVER_SAFE_SETUP \
{ \
0, \
1, \
0, \
0, \
0, \
0, \
0, \
0, \
1, \
2, \
0, \
255, \
0x00, \
255, \
0, \
0, \
10, \
1, \
1, \
1 \
}
#define INQ7_SftRe 1
#define INQ7_CmdQueue (1<<1)
#define INQ7_Reserved (1<<2)
#define INQ7_Linked (1<<3)
#define INQ7_Sync (1<<4)
#define INQ7_WBus16 (1<<5)
#define INQ7_WBus32 (1<<6)
#define INQ7_RelAdr (1<<7)
#define INQ7_IdeLike 0
#define INQ7_Scsi1Like INQ7_IdeLike
#define INQ7_Perfect 0xff
#define INQ7_Questionnable ~(INQ7_CmdQueue|INQ7_Sync)
#define INQ7_VeryQuestionnable \
~(INQ7_CmdQueue|INQ7_Sync|INQ7_WBus16|INQ7_WBus32)
#define INQ7_Default INQ7_Perfect
#define NCR53C8XX_TARGET_CAPABILITIES \
\
{ \
{ \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
} \
}, \
\
{ \
{ \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
INQ7_Default, \
} \
}
#if LINUX_VERSION_CODE >= LinuxVersionCode(1,3,0)
#if defined(CONFIG_SCSI_NCR53C7xx) || !defined(CONFIG_SCSI_NCR53C8XX)
#define PROC_SCSI_NCR53C8XX PROC_SCSI_NCR53C7xx
#endif
#endif
struct ncr_reg {
u_char nc_scntl0;
u_char nc_scntl1;
#define ISCON 0x10
#define CRST 0x08
u_char nc_scntl2;
#define SDU 0x80
#define CHM 0x40
#define WSS 0x08
#define WSR 0x01
u_char nc_scntl3;
#define EWS 0x08
#define ULTRA 0x80
u_char nc_scid;
#define RRE 0x40
#define SRE 0x20
u_char nc_sxfer;
u_char nc_sdid;
u_char nc_gpreg;
u_char nc_sfbr;
u_char nc_socl;
#define CREQ 0x80
#define CACK 0x40
#define CBSY 0x20
#define CSEL 0x10
#define CATN 0x08
#define CMSG 0x04
#define CC_D 0x02
#define CI_O 0x01
u_char nc_ssid;
u_char nc_sbcl;
u_char nc_dstat;
#define DFE 0x80
#define MDPE 0x40
#define BF 0x20
#define ABRT 0x10
#define SSI 0x08
#define SIR 0x04
#define IID 0x01
u_char nc_sstat0;
#define ILF 0x80
#define ORF 0x40
#define OLF 0x20
#define AIP 0x10
#define LOA 0x08
#define WOA 0x04
#define IRST 0x02
#define SDP 0x01
u_char nc_sstat1;
#define FF3210 0xf0
u_char nc_sstat2;
#define ILF1 0x80
#define ORF1 0x40
#define OLF1 0x20
#define DM 0x04
#define LDSC 0x02
u_int32 nc_dsa;
u_char nc_istat;
#define CABRT 0x80
#define SRST 0x40
#define SIGP 0x20
#define SEM 0x10
#define CON 0x08
#define INTF 0x04
#define SIP 0x02
#define DIP 0x01
u_char nc_15_;
u_char nc_16_;
u_char nc_17_;
u_char nc_ctest0;
u_char nc_ctest1;
u_char nc_ctest2;
#define CSIGP 0x40
u_char nc_ctest3;
#define FLF 0x08
#define CLF 0x04
#define FM 0x02
#define WRIE 0x01
u_int32 nc_temp;
u_char nc_dfifo;
u_char nc_ctest4;
#define BDIS 0x80
#define MPEE 0x08
u_char nc_ctest5;
#define DFS 0x20
u_char nc_ctest6;
u_int32 nc_dbc;
u_int32 nc_dnad;
u_int32 nc_dsp;
u_int32 nc_dsps;
u_int32 nc_scratcha;
u_char nc_dmode;
#define BL_2 0x80
#define BL_1 0x40
#define ERL 0x08
#define ERMP 0x04
#define BOF 0x02
u_char nc_dien;
u_char nc_dwt;
u_char nc_dcntl;
#define CLSE 0x80
#define PFF 0x40
#define PFEN 0x20
#define SSM 0x10
#define IRQM 0x08
#define STD 0x04
#define IRQD 0x02
#define NOCOM 0x01
u_int32 nc_adder;
u_short nc_sien;
u_short nc_sist;
#define SBMC 0x1000
#define STO 0x0400
#define GEN 0x0200
#define HTH 0x0100
#define MA 0x80
#define CMP 0x40
#define SEL 0x20
#define RSL 0x10
#define SGE 0x08
#define UDC 0x04
#define RST 0x02
#define PAR 0x01
u_char nc_slpar;
u_char nc_swide;
u_char nc_macntl;
u_char nc_gpcntl;
u_char nc_stime0;
u_char nc_stime1;
u_short nc_respid;
u_char nc_stest0;
u_char nc_stest1;
#define DBLEN 0x08
#define DBLSEL 0x04
u_char nc_stest2;
#define ROF 0x40
#define EXT 0x02
u_char nc_stest3;
#define TE 0x80
#define HSC 0x20
#define CSF 0x02
u_short nc_sidl;
u_char nc_stest4;
#define SMODE 0xc0
#define SMODE_HVD 0x40
#define SMODE_SE 0x80
#define SMODE_LVD 0xc0
#define LCKFRQ 0x20
u_char nc_53_;
u_short nc_sodl;
u_short nc_56_;
u_short nc_sbdl;
u_short nc_5a_;
u_char nc_scr0;
u_char nc_scr1;
u_char nc_scr2;
u_char nc_scr3;
};
#define REGJ(p,r) (offsetof(struct ncr_reg, p ## r))
#define REG(r) REGJ (nc_, r)
#ifndef TARGET_MODE
#define TARGET_MODE 0
#endif
typedef u_int32 ncrcmd;
#define SCR_DATA_OUT 0x00000000
#define SCR_DATA_IN 0x01000000
#define SCR_COMMAND 0x02000000
#define SCR_STATUS 0x03000000
#define SCR_ILG_OUT 0x04000000
#define SCR_ILG_IN 0x05000000
#define SCR_MSG_OUT 0x06000000
#define SCR_MSG_IN 0x07000000
#define SCR_MOVE_ABS(l) ((0x08000000 ^ (TARGET_MODE << 1ul)) | (l))
#define SCR_MOVE_IND(l) ((0x28000000 ^ (TARGET_MODE << 1ul)) | (l))
#define SCR_MOVE_TBL (0x18000000 ^ (TARGET_MODE << 1ul))
struct scr_tblmove {
u_int32 size;
u_int32 addr;
};
#define SCR_SEL_ABS 0x40000000
#define SCR_SEL_ABS_ATN 0x41000000
#define SCR_SEL_TBL 0x42000000
#define SCR_SEL_TBL_ATN 0x43000000
struct scr_tblsel {
u_char sel_0;
u_char sel_sxfer;
u_char sel_id;
u_char sel_scntl3;
};
#define SCR_JMP_REL 0x04000000
#define SCR_ID(id) (((u_int32)(id)) << 16)
#define SCR_WAIT_DISC 0x48000000
#define SCR_WAIT_RESEL 0x50000000
#define SCR_SET(f) (0x58000000 | (f))
#define SCR_CLR(f) (0x60000000 | (f))
#define SCR_CARRY 0x00000400
#define SCR_TRG 0x00000200
#define SCR_ACK 0x00000040
#define SCR_ATN 0x00000008
#define SCR_NO_FLUSH 0x01000000
#define SCR_COPY(n) (0xc0000000 | SCR_NO_FLUSH | (n))
#define SCR_COPY_F(n) (0xc0000000 | (n))
#define SCR_REG_OFS(ofs) ((ofs) << 16ul)
#define SCR_SFBR_REG(reg,op,data) \
(0x68000000 | (SCR_REG_OFS(REG(reg))) | (op) | ((data)<<8ul))
#define SCR_REG_SFBR(reg,op,data) \
(0x70000000 | (SCR_REG_OFS(REG(reg))) | (op) | ((data)<<8ul))
#define SCR_REG_REG(reg,op,data) \
(0x78000000 | (SCR_REG_OFS(REG(reg))) | (op) | ((data)<<8ul))
#define SCR_LOAD 0x00000000
#define SCR_SHL 0x01000000
#define SCR_OR 0x02000000
#define SCR_XOR 0x03000000
#define SCR_AND 0x04000000
#define SCR_SHR 0x05000000
#define SCR_ADD 0x06000000
#define SCR_ADDC 0x07000000
#define SCR_FROM_REG(reg) \
SCR_REG_SFBR(reg,SCR_OR,0)
#define SCR_TO_REG(reg) \
SCR_SFBR_REG(reg,SCR_OR,0)
#define SCR_LOAD_REG(reg,data) \
SCR_REG_REG(reg,SCR_LOAD,data)
#define SCR_LOAD_SFBR(data) \
(SCR_REG_SFBR (gpreg, SCR_LOAD, data))
#define SCR_NO_OP 0x80000000
#define SCR_JUMP 0x80080000
#define SCR_JUMPR 0x80880000
#define SCR_CALL 0x88080000
#define SCR_CALLR 0x88880000
#define SCR_RETURN 0x90080000
#define SCR_INT 0x98080000
#define SCR_INT_FLY 0x98180000
#define IFFALSE(arg) (0x00080000 | (arg))
#define IFTRUE(arg) (0x00000000 | (arg))
#define WHEN(phase) (0x00030000 | (phase))
#define IF(phase) (0x00020000 | (phase))
#define DATA(D) (0x00040000 | ((D) & 0xff))
#define MASK(D,M) (0x00040000 | (((M ^ 0xff) & 0xff) << 8ul)|((D) & 0xff))
#define CARRYSET (0x00200000)
#define M_COMPLETE (0x00)
#define M_EXTENDED (0x01)
#define M_SAVE_DP (0x02)
#define M_RESTORE_DP (0x03)
#define M_DISCONNECT (0x04)
#define M_ID_ERROR (0x05)
#define M_ABORT (0x06)
#define M_REJECT (0x07)
#define M_NOOP (0x08)
#define M_PARITY (0x09)
#define M_LCOMPLETE (0x0a)
#define M_FCOMPLETE (0x0b)
#define M_RESET (0x0c)
#define M_ABORT_TAG (0x0d)
#define M_CLEAR_QUEUE (0x0e)
#define M_INIT_REC (0x0f)
#define M_REL_REC (0x10)
#define M_TERMINATE (0x11)
#define M_SIMPLE_TAG (0x20)
#define M_HEAD_TAG (0x21)
#define M_ORDERED_TAG (0x22)
#define M_IGN_RESIDUE (0x23)
#define M_IDENTIFY (0x80)
#define M_X_MODIFY_DP (0x00)
#define M_X_SYNC_REQ (0x01)
#define M_X_WIDE_REQ (0x03)
#define S_GOOD (0x00)
#define S_CHECK_COND (0x02)
#define S_COND_MET (0x04)
#define S_BUSY (0x08)
#define S_INT (0x10)
#define S_INT_COND_MET (0x14)
#define S_CONFLICT (0x18)
#define S_TERMINATED (0x20)
#define S_QUEUE_FULL (0x28)
#define S_ILLEGAL (0xff)
#define S_SENSE (0x80)
#endif
#endif