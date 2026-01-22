#ifndef SYM53C8XX_DEFS_H
#define SYM53C8XX_DEFS_H
#if !defined(LINUX_VERSION_CODE)
#include <linux/version.h>
#endif
#include <linux/config.h>
#ifndef LinuxVersionCode
#define LinuxVersionCode(v, p, s) (((v)<<16)+((p)<<8)+(s))
#endif
#ifdef CONFIG_SCSI_NCR53C8XX_PQS_PDS
#define SCSI_NCR_PQS_PDS_SUPPORT
#endif
#ifndef CONFIG_SCSI_NCR53C8XX_NVRAM_DETECT
#define CONFIG_SCSI_NCR53C8XX_NVRAM_DETECT
#endif
#define SCSI_NCR_BOOT_COMMAND_LINE_SUPPORT
#define SCSI_NCR_DEBUG_INFO_SUPPORT
#define SCSI_NCR_PCI_FIX_UP_SUPPORT
#ifdef SCSI_NCR_PROC_INFO_SUPPORT
# define SCSI_NCR_USER_COMMAND_SUPPORT
# define SCSI_NCR_USER_INFO_SUPPORT
#endif
#ifdef CONFIG_SCSI_NCR53C8XX_INTEGRITY_CHECK
# define SCSI_NCR_ENABLE_INTEGRITY_CHECK
#endif
#ifdef CONFIG_SCSI_NCR53C8XX_NVRAM_DETECT
#define SCSI_NCR_NVRAM_SUPPORT
#endif
#define SCSI_NCR_SETUP_SPECIAL_FEATURES (3)
#define SCSI_NCR_SETUP_ULTRA_SCSI (3)
#define SCSI_NCR_MAX_SYNC (80)
#ifdef CONFIG_SCSI_NCR53C8XX_MAX_TAGS
#if CONFIG_SCSI_NCR53C8XX_MAX_TAGS < 2
#define SCSI_NCR_MAX_TAGS (2)
#elif CONFIG_SCSI_NCR53C8XX_MAX_TAGS > 256
#define SCSI_NCR_MAX_TAGS (256)
#else
#define SCSI_NCR_MAX_TAGS CONFIG_SCSI_NCR53C8XX_MAX_TAGS
#endif
#else
#define SCSI_NCR_MAX_TAGS (8)
#endif
#ifdef CONFIG_SCSI_NCR53C8XX_DEFAULT_TAGS
#define SCSI_NCR_SETUP_DEFAULT_TAGS CONFIG_SCSI_NCR53C8XX_DEFAULT_TAGS
#elif defined CONFIG_SCSI_NCR53C8XX_TAGGED_QUEUE
#define SCSI_NCR_SETUP_DEFAULT_TAGS SCSI_NCR_MAX_TAGS
#else
#define SCSI_NCR_SETUP_DEFAULT_TAGS (0)
#endif
#if defined(CONFIG_SCSI_NCR53C8XX_IOMAPPED)
#define SCSI_NCR_IOMAPPED
#elif defined(__alpha__)
#define SCSI_NCR_IOMAPPED
#elif defined(__powerpc__)
#define SCSI_NCR_IOMAPPED
#define SCSI_NCR_PCI_MEM_NOT_SUPPORTED
#elif defined(__sparc__)
#undef SCSI_NCR_IOMAPPED
#endif
#undef SCSI_NCR_USE_64BIT_DAC
#if defined(CONFIG_SCSI_NCR53C8XX_IARB)
#define SCSI_NCR_IARB_SUPPORT
#endif
#undef SCSI_NCR_USE_64BIT_DAC
#ifndef CONFIG_SCSI_NCR53C8XX_SYNC
#define CONFIG_SCSI_NCR53C8XX_SYNC (20)
#elif CONFIG_SCSI_NCR53C8XX_SYNC > SCSI_NCR_MAX_SYNC
#undef CONFIG_SCSI_NCR53C8XX_SYNC
#define CONFIG_SCSI_NCR53C8XX_SYNC SCSI_NCR_MAX_SYNC
#endif
#if CONFIG_SCSI_NCR53C8XX_SYNC == 0
#define SCSI_NCR_SETUP_DEFAULT_SYNC (255)
#elif CONFIG_SCSI_NCR53C8XX_SYNC <= 5
#define SCSI_NCR_SETUP_DEFAULT_SYNC (50)
#elif CONFIG_SCSI_NCR53C8XX_SYNC <= 20
#define SCSI_NCR_SETUP_DEFAULT_SYNC (250/(CONFIG_SCSI_NCR53C8XX_SYNC))
#elif CONFIG_SCSI_NCR53C8XX_SYNC <= 33
#define SCSI_NCR_SETUP_DEFAULT_SYNC (11)
#elif CONFIG_SCSI_NCR53C8XX_SYNC <= 40
#define SCSI_NCR_SETUP_DEFAULT_SYNC (10)
#else
#define SCSI_NCR_SETUP_DEFAULT_SYNC (9)
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
#define SCSI_NCR_SETUP_DIFF_SUPPORT (4)
#else
#define SCSI_NCR_SETUP_LED_PIN (0)
#define SCSI_NCR_SETUP_DIFF_SUPPORT (0)
#endif
#define SCSI_NCR_SETUP_SETTLE_TIME (2)
#ifndef SCSI_NCR_PCIQ_WORK_AROUND_OPT
#define SCSI_NCR_PCIQ_WORK_AROUND_OPT 1
#endif
#if SCSI_NCR_PCIQ_WORK_AROUND_OPT == 1
#define SCSI_NCR_PCIQ_MAY_NOT_FLUSH_PW_UPSTREAM
#define SCSI_NCR_PCIQ_MAY_REORDER_WRITES
#define SCSI_NCR_PCIQ_MAY_MISS_COMPLETIONS
#elif SCSI_NCR_PCIQ_WORK_AROUND_OPT == 2
#define SCSI_NCR_PCIQ_MAY_NOT_FLUSH_PW_UPSTREAM
#define SCSI_NCR_PCIQ_MAY_REORDER_WRITES
#define SCSI_NCR_PCIQ_MAY_MISS_COMPLETIONS
#define SCSI_NCR_PCIQ_BROKEN_INTR
#elif SCSI_NCR_PCIQ_WORK_AROUND_OPT == 3
#define SCSI_NCR_PCIQ_SYNC_ON_INTR
#endif
#define SCSI_NCR_ALWAYS_SIMPLE_TAG
#define SCSI_NCR_MAX_SCATTER (127)
#define SCSI_NCR_MAX_TARGET (16)
#define SCSI_NCR_CAN_QUEUE (8*SCSI_NCR_MAX_TAGS + 2*SCSI_NCR_MAX_TARGET)
#define SCSI_NCR_CMD_PER_LUN (SCSI_NCR_MAX_TAGS)
#define SCSI_NCR_SG_TABLESIZE (SCSI_NCR_MAX_SCATTER)
#define SCSI_NCR_TIMER_INTERVAL (HZ)
#if 1
#define SCSI_NCR_MAX_LUN (16)
#else
#define SCSI_NCR_MAX_LUN (1)
#endif
#ifndef HOSTS_C
#define ktime_get(o) (jiffies + (u_long) o)
#define ktime_exp(b) ((long)(jiffies) - (long)(b) >= 0)
#define ktime_dif(a, b) ((long)(a) - (long)(b))
#define ktime_add(a, o) ((a) + (u_long)(o))
#define ktime_sub(a, o) ((a) - (u_long)(o))
#ifdef __BIG_ENDIAN
#if LINUX_VERSION_CODE < LinuxVersionCode(2,1,0)
#error "BIG ENDIAN byte ordering needs kernel version >= 2.1.0"
#endif
#define inw_l2b inw
#define inl_l2b inl
#define outw_b2l outw
#define outl_b2l outl
#define readw_l2b readw
#define readl_l2b readl
#define writew_b2l writew
#define writel_b2l writel
#else
#if defined(__i386__)
#define inw_raw inw
#define inl_raw inl
#define outw_raw outw
#define outl_raw outl
#define readb_raw(a) (*(volatile unsigned char *) (a))
#define readw_raw(a) (*(volatile unsigned short *) (a))
#define readl_raw(a) (*(volatile unsigned int *) (a))
#define writeb_raw(b,a) ((*(volatile unsigned char *) (a)) = (b))
#define writew_raw(b,a) ((*(volatile unsigned short *) (a)) = (b))
#define writel_raw(b,a) ((*(volatile unsigned int *) (a)) = (b))
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
#endif
#ifdef SCSI_NCR_BIG_ENDIAN
#error "The NCR in BIG ENDIAN addressing mode is not (yet) supported"
#endif
#if defined __i386__
#define MEMORY_BARRIER() do { ; } while(0)
#elif defined __powerpc__
#define MEMORY_BARRIER() __asm__ volatile("eieio; sync" : : : "memory")
#elif defined __ia64__
#define MEMORY_BARRIER() __asm__ volatile("mf.a; mf" : : : "memory")
#else
#define MEMORY_BARRIER() mb()
#endif
#if defined(SCSI_NCR_BIG_ENDIAN)
#define ncr_offb(o) (((o)&~3)+((~((o)&3))&3))
#define ncr_offw(o) (((o)&~3)+((~((o)&3))&2))
#else
#define ncr_offb(o) (o)
#define ncr_offw(o) (o)
#endif
#if defined(__BIG_ENDIAN) && !defined(SCSI_NCR_BIG_ENDIAN)
#define cpu_to_scr(dw) cpu_to_le32(dw)
#define scr_to_cpu(dw) le32_to_cpu(dw)
#elif defined(__LITTLE_ENDIAN) && defined(SCSI_NCR_BIG_ENDIAN)
#define cpu_to_scr(dw) cpu_to_be32(dw)
#define scr_to_cpu(dw) be32_to_cpu(dw)
#else
#define cpu_to_scr(dw) (dw)
#define scr_to_cpu(dw) (dw)
#endif
#if defined(SCSI_NCR_IOMAPPED)
#define INB_OFF(o) inb (np->base_io + ncr_offb(o))
#define OUTB_OFF(o, val) outb ((val), np->base_io + ncr_offb(o))
#if defined(__BIG_ENDIAN) && !defined(SCSI_NCR_BIG_ENDIAN)
#define INW_OFF(o) inw_l2b (np->base_io + ncr_offw(o))
#define INL_OFF(o) inl_l2b (np->base_io + (o))
#define OUTW_OFF(o, val) outw_b2l ((val), np->base_io + ncr_offw(o))
#define OUTL_OFF(o, val) outl_b2l ((val), np->base_io + (o))
#elif defined(__LITTLE_ENDIAN) && defined(SCSI_NCR_BIG_ENDIAN)
#define INW_OFF(o) inw_b2l (np->base_io + ncr_offw(o))
#define INL_OFF(o) inl_b2l (np->base_io + (o))
#define OUTW_OFF(o, val) outw_l2b ((val), np->base_io + ncr_offw(o))
#define OUTL_OFF(o, val) outl_l2b ((val), np->base_io + (o))
#else
#define INW_OFF(o) inw_raw (np->base_io + ncr_offw(o))
#define INL_OFF(o) inl_raw (np->base_io + (o))
#define OUTW_OFF(o, val) outw_raw ((val), np->base_io + ncr_offw(o))
#define OUTL_OFF(o, val) outl_raw ((val), np->base_io + (o))
#endif
#else
#define INB_OFF(o) readb((char *)np->reg + ncr_offb(o))
#define OUTB_OFF(o, val) writeb((val), (char *)np->reg + ncr_offb(o))
#if defined(__BIG_ENDIAN) && !defined(SCSI_NCR_BIG_ENDIAN)
#define INW_OFF(o) readw_l2b((char *)np->reg + ncr_offw(o))
#define INL_OFF(o) readl_l2b((char *)np->reg + (o))
#define OUTW_OFF(o, val) writew_b2l((val), (char *)np->reg + ncr_offw(o))
#define OUTL_OFF(o, val) writel_b2l((val), (char *)np->reg + (o))
#elif defined(__LITTLE_ENDIAN) && defined(SCSI_NCR_BIG_ENDIAN)
#define INW_OFF(o) readw_b2l((char *)np->reg + ncr_offw(o))
#define INL_OFF(o) readl_b2l((char *)np->reg + (o))
#define OUTW_OFF(o, val) writew_l2b((val), (char *)np->reg + ncr_offw(o))
#define OUTL_OFF(o, val) writel_l2b((val), (char *)np->reg + (o))
#else
#define INW_OFF(o) readw_raw((char *)np->reg + ncr_offw(o))
#define INL_OFF(o) readl_raw((char *)np->reg + (o))
#define OUTW_OFF(o, val) writew_raw((val), (char *)np->reg + ncr_offw(o))
#define OUTL_OFF(o, val) writel_raw((val), (char *)np->reg + (o))
#endif
#endif
#define INB(r) INB_OFF (offsetof(struct ncr_reg,r))
#define INW(r) INW_OFF (offsetof(struct ncr_reg,r))
#define INL(r) INL_OFF (offsetof(struct ncr_reg,r))
#define OUTB(r, val) OUTB_OFF (offsetof(struct ncr_reg,r), (val))
#define OUTW(r, val) OUTW_OFF (offsetof(struct ncr_reg,r), (val))
#define OUTL(r, val) OUTL_OFF (offsetof(struct ncr_reg,r), (val))
#define OUTONB(r, m) OUTB(r, INB(r) | (m))
#define OUTOFFB(r, m) OUTB(r, INB(r) & ~(m))
#define OUTONW(r, m) OUTW(r, INW(r) | (m))
#define OUTOFFW(r, m) OUTW(r, INW(r) & ~(m))
#define OUTONL(r, m) OUTL(r, INL(r) | (m))
#define OUTOFFL(r, m) OUTL(r, INL(r) & ~(m))
#define OUTL_DSP(v) \
do { \
MEMORY_BARRIER(); \
OUTL (nc_dsp, (v)); \
} while (0)
#define OUTONB_STD() \
do { \
MEMORY_BARRIER(); \
OUTONB (nc_dcntl, (STD|NOCOM)); \
} while (0)
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
#ifndef PCI_DEVICE_ID_NCR_53C895A
#define PCI_DEVICE_ID_NCR_53C895A 0x12
#endif
#ifndef PCI_DEVICE_ID_NCR_53C1510D
#define PCI_DEVICE_ID_NCR_53C1510D 0xa
#endif
#ifndef PCI_DEVICE_ID_LSI_53C1010
#define PCI_DEVICE_ID_LSI_53C1010 0x20
#endif
#ifndef PCI_DEVICE_ID_LSI_53C1010_66
#define PCI_DEVICE_ID_LSI_53C1010_66 0x21
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
#define FE_RAM8K (1<<16)
#define FE_64BIT (1<<17)
#define FE_IO256 (1<<18)
#define FE_NOPM (1<<19)
#define FE_LEDC (1<<20)
#define FE_DIFF (1<<21)
#define FE_ULTRA3 (1<<22)
#define FE_66MHZ (1<<23)
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
FE_WIDE|FE_ERL|FE_BOF|FE_DIFF} \
, \
{PCI_DEVICE_ID_NCR_53C825, 0xff, "825a", 6, 8, 4, \
FE_WIDE|FE_CACHE0_SET|FE_BOF|FE_DFS|FE_LDSTR|FE_PFEN|FE_RAM|FE_DIFF} \
, \
{PCI_DEVICE_ID_NCR_53C860, 0xff, "860", 4, 8, 5, \
FE_ULTRA|FE_CLK80|FE_CACHE_SET|FE_BOF|FE_LDSTR|FE_PFEN} \
, \
{PCI_DEVICE_ID_NCR_53C875, 0x01, "875", 6, 16, 5, \
FE_WIDE|FE_ULTRA|FE_CLK80|FE_CACHE0_SET|FE_BOF|FE_DFS|FE_LDSTR|FE_PFEN|\
FE_RAM|FE_DIFF} \
, \
{PCI_DEVICE_ID_NCR_53C875, 0x0f, "875", 6, 16, 5, \
FE_WIDE|FE_ULTRA|FE_DBLR|FE_CACHE0_SET|FE_BOF|FE_DFS|FE_LDSTR|FE_PFEN| \
FE_RAM|FE_DIFF} \
, \
{PCI_DEVICE_ID_NCR_53C875, 0x1f, "876", 6, 16, 5, \
FE_WIDE|FE_ULTRA|FE_DBLR|FE_CACHE0_SET|FE_BOF|FE_DFS|FE_LDSTR|FE_PFEN| \
FE_RAM|FE_DIFF} \
, \
{PCI_DEVICE_ID_NCR_53C875, 0x2f, "875E", 6, 16, 5, \
FE_WIDE|FE_ULTRA|FE_DBLR|FE_CACHE0_SET|FE_BOF|FE_DFS|FE_LDSTR|FE_PFEN| \
FE_RAM|FE_DIFF} \
, \
{PCI_DEVICE_ID_NCR_53C875, 0xff, "876", 6, 16, 5, \
FE_WIDE|FE_ULTRA|FE_DBLR|FE_CACHE0_SET|FE_BOF|FE_DFS|FE_LDSTR|FE_PFEN| \
FE_RAM|FE_DIFF} \
, \
{PCI_DEVICE_ID_NCR_53C875J,0xff, "875J", 6, 16, 5, \
FE_WIDE|FE_ULTRA|FE_DBLR|FE_CACHE0_SET|FE_BOF|FE_DFS|FE_LDSTR|FE_PFEN| \
FE_RAM} \
, \
{PCI_DEVICE_ID_NCR_53C885, 0xff, "885", 6, 16, 5, \
FE_WIDE|FE_ULTRA|FE_DBLR|FE_CACHE0_SET|FE_BOF|FE_DFS|FE_LDSTR|FE_PFEN| \
FE_RAM|FE_DIFF} \
, \
{PCI_DEVICE_ID_NCR_53C895, 0xff, "895", 6, 31, 7, \
FE_WIDE|FE_ULTRA2|FE_QUAD|FE_CACHE_SET|FE_BOF|FE_DFS|FE_LDSTR|FE_PFEN| \
FE_RAM} \
, \
{PCI_DEVICE_ID_NCR_53C896, 0xff, "896", 6, 31, 7, \
FE_WIDE|FE_ULTRA2|FE_QUAD|FE_CACHE_SET|FE_BOF|FE_DFS|FE_LDSTR|FE_PFEN| \
FE_RAM|FE_RAM8K|FE_64BIT|FE_IO256|FE_NOPM|FE_LEDC} \
, \
{PCI_DEVICE_ID_NCR_53C895A, 0xff, "895a", 6, 31, 7, \
FE_WIDE|FE_ULTRA2|FE_QUAD|FE_CACHE_SET|FE_BOF|FE_DFS|FE_LDSTR|FE_PFEN| \
FE_RAM|FE_RAM8K|FE_64BIT|FE_IO256|FE_NOPM|FE_LEDC} \
, \
{PCI_DEVICE_ID_NCR_53C1510D, 0xff, "1510D", 7, 31, 7, \
FE_WIDE|FE_ULTRA2|FE_QUAD|FE_CACHE_SET|FE_BOF|FE_DFS|FE_LDSTR|FE_PFEN| \
FE_RAM|FE_IO256} \
, \
{PCI_DEVICE_ID_LSI_53C1010, 0xff, "1010", 6, 31, 7, \
FE_WIDE|FE_QUAD|FE_CACHE_SET|FE_BOF|FE_DFS|FE_LDSTR|FE_PFEN| \
FE_RAM|FE_RAM8K|FE_64BIT|FE_IO256|FE_NOPM|FE_LEDC|FE_ULTRA3} \
, \
{PCI_DEVICE_ID_LSI_53C1010_66, 0xff, "1010_66", 6, 31, 7, \
FE_WIDE|FE_QUAD|FE_CACHE_SET|FE_BOF|FE_DFS|FE_LDSTR|FE_PFEN| \
FE_RAM|FE_RAM8K|FE_64BIT|FE_IO256|FE_NOPM|FE_LEDC|FE_ULTRA3|FE_66MHZ} \
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
PCI_DEVICE_ID_NCR_53C896, \
PCI_DEVICE_ID_NCR_53C895A, \
PCI_DEVICE_ID_NCR_53C1510D, \
PCI_DEVICE_ID_LSI_53C1010, \
PCI_DEVICE_ID_LSI_53C1010_66 \
}
#define SCSI_NCR_MAX_EXCLUDES 8
struct ncr_driver_setup {
u_char master_parity;
u_char scsi_parity;
u_char disconnection;
u_char special_features;
u_char ultra_scsi;
u_char force_sync_nego;
u_char reverse_probe;
u_char pci_fix_up;
u_char use_nvram;
u_char verbose;
u_char default_tags;
u_short default_sync;
u_short debug;
u_char burst_max;
u_char led_pin;
u_char max_wide;
u_char settle_delay;
u_char diff_support;
u_char irqm;
u_char bus_check;
u_char optimize;
u_char recovery;
u_char host_id;
u_short iarb;
u_long excludes[SCSI_NCR_MAX_EXCLUDES];
char tag_ctrl[100];
};
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
0, \
SCSI_NCR_SETUP_DEFAULT_TAGS, \
SCSI_NCR_SETUP_DEFAULT_SYNC, \
0x00, \
7, \
SCSI_NCR_SETUP_LED_PIN, \
1, \
SCSI_NCR_SETUP_SETTLE_TIME, \
SCSI_NCR_SETUP_DIFF_SUPPORT, \
0, \
1, \
0, \
0, \
255, \
0x00 \
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
1, \
0, \
0, \
255 \
}
#ifdef SCSI_NCR_NVRAM_SUPPORT
#define SYMBIOS_NVRAM_SIZE 368
#define SYMBIOS_NVRAM_ADDRESS 0x100
struct Symbios_nvram {
u_short type;
u_short byte_count;
u_short checksum;
u_char v_major;
u_char v_minor;
u_int32 boot_crc;
u_short flags;
#define SYMBIOS_SCAM_ENABLE (1)
#define SYMBIOS_PARITY_ENABLE (1<<1)
#define SYMBIOS_VERBOSE_MSGS (1<<2)
#define SYMBIOS_CHS_MAPPING (1<<3)
#define SYMBIOS_NO_NVRAM (1<<3)
u_short flags1;
#define SYMBIOS_SCAN_HI_LO (1)
u_short term_state;
#define SYMBIOS_TERM_CANT_PROGRAM (0)
#define SYMBIOS_TERM_ENABLED (1)
#define SYMBIOS_TERM_DISABLED (2)
u_short rmvbl_flags;
#define SYMBIOS_RMVBL_NO_SUPPORT (0)
#define SYMBIOS_RMVBL_BOOT_DEVICE (1)
#define SYMBIOS_RMVBL_MEDIA_INSTALLED (2)
u_char host_id;
u_char num_hba;
u_char num_devices;
u_char max_scam_devices;
u_char num_valid_scam_devives;
u_char rsvd;
struct Symbios_host{
u_short type;
u_short device_id;
u_short vendor_id;
u_char bus_nr;
u_char device_fn;
u_short word8;
u_short flags;
#define SYMBIOS_INIT_SCAN_AT_BOOT (1)
u_short io_port;
} host[4];
struct Symbios_target {
u_char flags;
#define SYMBIOS_DISCONNECT_ENABLE (1)
#define SYMBIOS_SCAN_AT_BOOT_TIME (1<<1)
#define SYMBIOS_SCAN_LUNS (1<<2)
#define SYMBIOS_QUEUE_TAGS_ENABLED (1<<3)
u_char rsvd;
u_char bus_width;
u_char sync_offset;
u_short sync_period;
u_short timeout;
} target[16];
struct Symbios_scam {
u_short id;
u_short method;
#define SYMBIOS_SCAM_DEFAULT_METHOD (0)
#define SYMBIOS_SCAM_DONT_ASSIGN (1)
#define SYMBIOS_SCAM_SET_SPECIFIC_ID (2)
#define SYMBIOS_SCAM_USE_ORDER_GIVEN (3)
u_short status;
#define SYMBIOS_SCAM_UNKNOWN (0)
#define SYMBIOS_SCAM_DEVICE_NOT_FOUND (1)
#define SYMBIOS_SCAM_ID_NOT_SET (2)
#define SYMBIOS_SCAM_ID_VALID (3)
u_char target_id;
u_char rsvd;
} scam[4];
u_char spare_devices[15*8];
u_char trailer[6];
};
typedef struct Symbios_nvram Symbios_nvram;
typedef struct Symbios_host Symbios_host;
typedef struct Symbios_target Symbios_target;
typedef struct Symbios_scam Symbios_scam;
#define TEKRAM_NVRAM_SIZE 64
#define TEKRAM_93C46_NVRAM_ADDRESS 0
#define TEKRAM_24C16_NVRAM_ADDRESS 0x40
struct Tekram_nvram {
struct Tekram_target {
u_char flags;
#define TEKRAM_PARITY_CHECK (1)
#define TEKRAM_SYNC_NEGO (1<<1)
#define TEKRAM_DISCONNECT_ENABLE (1<<2)
#define TEKRAM_START_CMD (1<<3)
#define TEKRAM_TAGGED_COMMANDS (1<<4)
#define TEKRAM_WIDE_NEGO (1<<5)
u_char sync_index;
u_short word2;
} target[16];
u_char host_id;
u_char flags;
#define TEKRAM_MORE_THAN_2_DRIVES (1)
#define TEKRAM_DRIVES_SUP_1GB (1<<1)
#define TEKRAM_RESET_ON_POWER_ON (1<<2)
#define TEKRAM_ACTIVE_NEGATION (1<<3)
#define TEKRAM_IMMEDIATE_SEEK (1<<4)
#define TEKRAM_SCAN_LUNS (1<<5)
#define TEKRAM_REMOVABLE_FLAGS (3<<6)
u_char boot_delay_index;
u_char max_tags_index;
u_short flags1;
#define TEKRAM_F2_F6_ENABLED (1)
u_short spare[29];
};
typedef struct Tekram_nvram Tekram_nvram;
typedef struct Tekram_target Tekram_target;
#endif
struct ncr_reg {
u_char nc_scntl0;
u_char nc_scntl1;
#define ISCON 0x10
#define CRST 0x08
#define IARB 0x02
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
u_char nc_dsa;
u_char nc_dsa1;
u_char nc_dsa2;
u_char nc_dsa3;
u_char nc_istat;
#define CABRT 0x80
#define SRST 0x40
#define SIGP 0x20
#define SEM 0x10
#define CON 0x08
#define INTF 0x04
#define SIP 0x02
#define DIP 0x01
u_char nc_istat1;
u_char nc_mbox0;
u_char nc_mbox1;
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
u_char nc_scratcha;
u_char nc_scratcha1;
u_char nc_scratcha2;
u_char nc_scratcha3;
u_char nc_dmode;
#define BL_2 0x80
#define BL_1 0x40
#define ERL 0x08
#define ERMP 0x04
#define BOF 0x02
u_char nc_dien;
u_char nc_sbr;
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
#define SCLK 0x80
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
u_char nc_ccntl0;
#define ENPMJ 0x80
#define PMJCTL 0x40
#define ENNDJ 0x20
#define DISFC 0x10
#define DILS 0x02
#define DPR 0x01
u_char nc_ccntl1;
#define ZMOD 0x80
#define DIC 0x10
#define DDAC 0x08
#define XTIMOD 0x04
#define EXTIBMV 0x02
#define EXDBMV 0x01
u_short nc_sbdl;
u_short nc_5a_;
u_char nc_scr0;
u_char nc_scr1;
u_char nc_scr2;
u_char nc_scr3;
u_char nc_scrx[64];
u_int32 nc_mmrs;
u_int32 nc_mmws;
u_int32 nc_sfs;
u_int32 nc_drs;
u_int32 nc_sbms;
u_int32 nc_dbms;
u_int32 nc_dnad64;
u_short nc_scntl4;
#define U3EN 0x80
#define AIPEN 0x40
#define XCLKH_DT 0x08
#define XCLKH_ST 0x04
u_char nc_aipcntl0;
u_char nc_aipcntl1;
u_int32 nc_pmjad1;
u_int32 nc_pmjad2;
u_char nc_rbc;
u_char nc_rbc1;
u_char nc_rbc2;
u_char nc_rbc3;
u_char nc_ua;
u_char nc_ua1;
u_char nc_ua2;
u_char nc_ua3;
u_int32 nc_esa;
u_char nc_ia;
u_char nc_ia1;
u_char nc_ia2;
u_char nc_ia3;
u_int32 nc_sbc;
u_int32 nc_csbc;
u_short nc_crcpad;
u_char nc_crccntl0;
#define SNDCRC 0x10
u_char nc_crccntl1;
u_int32 nc_crcdata;
u_int32 nc_e8_;
u_int32 nc_ec_;
u_short nc_dfbc;
};
#define REGJ(p,r) (offsetof(struct ncr_reg, p ## r))
#define REG(r) REGJ (nc_, r)
typedef u_int32 ncrcmd;
#define SCR_DATA_OUT 0x00000000
#define SCR_DATA_IN 0x01000000
#define SCR_COMMAND 0x02000000
#define SCR_STATUS 0x03000000
#define SCR_DT_DATA_OUT 0x04000000
#define SCR_DT_DATA_IN 0x05000000
#define SCR_MSG_OUT 0x06000000
#define SCR_MSG_IN 0x07000000
#define SCR_ILG_OUT 0x04000000
#define SCR_ILG_IN 0x05000000
#define OPC_MOVE 0x08000000
#define SCR_MOVE_ABS(l) ((0x00000000 | OPC_MOVE) | (l))
#define SCR_MOVE_IND(l) ((0x20000000 | OPC_MOVE) | (l))
#define SCR_MOVE_TBL (0x10000000 | OPC_MOVE)
#define SCR_CHMOV_ABS(l) ((0x00000000) | (l))
#define SCR_CHMOV_IND(l) ((0x20000000) | (l))
#define SCR_CHMOV_TBL (0x10000000)
struct scr_tblmove {
u_int32 size;
u_int32 addr;
};
#define SCR_SEL_ABS 0x40000000
#define SCR_SEL_ABS_ATN 0x41000000
#define SCR_SEL_TBL 0x42000000
#define SCR_SEL_TBL_ATN 0x43000000
struct scr_tblsel {
u_char sel_scntl4;
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
#define SCR_REG_OFS(ofs) ((((ofs) & 0x7f) << 16ul) + ((ofs) & 0x80))
#define SCR_SFBR_REG(reg,op,data) \
(0x68000000 | (SCR_REG_OFS(REG(reg))) | (op) | (((data)&0xff)<<8ul))
#define SCR_REG_SFBR(reg,op,data) \
(0x70000000 | (SCR_REG_OFS(REG(reg))) | (op) | (((data)&0xff)<<8ul))
#define SCR_REG_REG(reg,op,data) \
(0x78000000 | (SCR_REG_OFS(REG(reg))) | (op) | (((data)&0xff)<<8ul))
#define SCR_LOAD 0x00000000
#define SCR_SHL 0x01000000
#define SCR_OR 0x02000000
#define SCR_XOR 0x03000000
#define SCR_AND 0x04000000
#define SCR_SHR 0x05000000
#define SCR_ADD 0x06000000
#define SCR_ADDC 0x07000000
#define SCR_SFBR_DATA (0x00800000>>8ul)
#define SCR_FROM_REG(reg) \
SCR_REG_SFBR(reg,SCR_OR,0)
#define SCR_TO_REG(reg) \
SCR_SFBR_REG(reg,SCR_OR,0)
#define SCR_LOAD_REG(reg,data) \
SCR_REG_REG(reg,SCR_LOAD,data)
#define SCR_LOAD_SFBR(data) \
(SCR_REG_SFBR (gpreg, SCR_LOAD, data))
#define SCR_REG_OFS2(ofs) (((ofs) & 0xff) << 16ul)
#define SCR_NO_FLUSH2 0x02000000
#define SCR_DSA_REL2 0x10000000
#define SCR_LOAD_R(reg, how, n) \
(0xe1000000 | how | (SCR_REG_OFS2(REG(reg))) | (n))
#define SCR_STORE_R(reg, how, n) \
(0xe0000000 | how | (SCR_REG_OFS2(REG(reg))) | (n))
#define SCR_LOAD_ABS(reg, n) SCR_LOAD_R(reg, SCR_NO_FLUSH2, n)
#define SCR_LOAD_REL(reg, n) SCR_LOAD_R(reg, SCR_NO_FLUSH2|SCR_DSA_REL2, n)
#define SCR_LOAD_ABS_F(reg, n) SCR_LOAD_R(reg, 0, n)
#define SCR_LOAD_REL_F(reg, n) SCR_LOAD_R(reg, SCR_DSA_REL2, n)
#define SCR_STORE_ABS(reg, n) SCR_STORE_R(reg, SCR_NO_FLUSH2, n)
#define SCR_STORE_REL(reg, n) SCR_STORE_R(reg, SCR_NO_FLUSH2|SCR_DSA_REL2,n)
#define SCR_STORE_ABS_F(reg, n) SCR_STORE_R(reg, 0, n)
#define SCR_STORE_REL_F(reg, n) SCR_STORE_R(reg, SCR_DSA_REL2, n)
#define SCR_NO_OP 0x80000000
#define SCR_JUMP 0x80080000
#define SCR_JUMP64 0x80480000
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
#define M_X_PPR_REQ (0x04)
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