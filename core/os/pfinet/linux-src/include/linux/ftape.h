#ifndef _FTAPE_H
#define _FTAPE_H
#define FTAPE_VERSION "ftape v3.04d 25/11/97"
#define KERNEL_VER(major,minor,sublvl) (((major)<<16)+((minor)<<8)+(sublvl))
#ifdef __KERNEL__
#include <linux/sched.h>
#include <linux/mm.h>
#endif
#include <linux/types.h>
#include <linux/version.h>
#include <linux/config.h>
#if LINUX_VERSION_CODE <= KERNEL_VER(1,2,13)
typedef daddr_t __kernel_daddr_t;
#endif
#include <linux/mtio.h>
#define FT_SECTOR(x)		(x+1)
#define FT_SECTOR_SIZE		1024
#define FT_SECTORS_PER_SEGMENT	  32
#define FT_ECC_SECTORS		   3
#define FT_SEGMENT_SIZE		((FT_SECTORS_PER_SEGMENT - FT_ECC_SECTORS) * FT_SECTOR_SIZE)
#define FT_BUFF_SIZE    (FT_SECTORS_PER_SEGMENT * FT_SECTOR_SIZE)
#define FTAPE_SEL_A     0
#define FTAPE_SEL_B     1
#define FTAPE_SEL_C     2
#define FTAPE_SEL_D     3
#define FTAPE_SEL_MASK     3
#define FTAPE_SEL(unit) ((unit) & FTAPE_SEL_MASK)
#define FTAPE_NO_REWIND 4
typedef union {
struct {
__u8 error;
__u8 command;
} error;
long space;
} ft_drive_error;
typedef union {
struct {
__u8 drive_status;
__u8 drive_config;
__u8 tape_status;
} status;
long space;
} ft_drive_status;
#ifdef __KERNEL__
#define FT_RQM_DELAY    12
#define FT_MILLISECOND  1
#define FT_SECOND       1000
#define FT_FOREVER      -1
#ifndef HZ
#error "HZ undefined."
#endif
#define FT_USPT         (1000000/HZ)
#ifdef TESTING
#define FT_SOFT_RETRIES 1
#define FT_RETRIES_ON_ECC_ERROR 3
#else
#define FT_SOFT_RETRIES 6
#define FT_RETRIES_ON_ECC_ERROR 3
#endif
#ifndef THE_FTAPE_MAINTAINER
#define THE_FTAPE_MAINTAINER "the ftape maintainer"
#endif
#ifndef CONFIG_FT_NR_BUFFERS
# define CONFIG_FT_NR_BUFFERS 3
#endif
#ifndef CONFIG_FT_FDC_THR
# define CONFIG_FT_FDC_THR 8
#endif
#ifndef CONFIG_FT_FDC_MAX_RATE
# define CONFIG_FT_FDC_MAX_RATE 2000
#endif
#ifndef CONFIG_FT_FDC_BASE
# define CONFIG_FT_FDC_BASE 0
#endif
#ifndef CONFIG_FT_FDC_IRQ
# define CONFIG_FT_FDC_IRQ  0
#endif
#ifndef CONFIG_FT_FDC_DMA
# define CONFIG_FT_FDC_DMA  0
#endif
#ifdef CONFIG_FT_PROBE_FC10
# undef CONFIG_FT_PROBE_FC10
# define CONFIG_FT_PROBE_FC10 1
#else
# define CONFIG_FT_PROBE_FC10 0
#endif
#ifdef CONFIG_FT_MACH2
# undef CONFIG_FT_MACH2
# define CONFIG_FT_MACH2 1
#else
# define CONFIG_FT_MACH2 0
#endif
#if CONFIG_FT_PROBE_FC10 == 1
# if CONFIG_FT_FDC_BASE == 0
#  undef  CONFIG_FT_FDC_BASE
#  define CONFIG_FT_FDC_BASE 0x180
# endif
# if CONFIG_FT_FDC_IRQ == 0
#  undef  CONFIG_FT_FDC_IRQ
#  define CONFIG_FT_FDC_IRQ 9
# endif
# if CONFIG_FT_FDC_DMA == 0
#  undef  CONFIG_FT_FDC_DMA
#  define CONFIG_FT_FDC_DMA 3
# endif
#elif CONFIG_FT_MACH2 == 1
# if CONFIG_FT_FDC_BASE == 0
#  undef  CONFIG_FT_FDC_BASE
#  define CONFIG_FT_FDC_BASE 0x1E0
# endif
# if CONFIG_FT_FDC_IRQ == 0
#  undef  CONFIG_FT_FDC_IRQ
#  define CONFIG_FT_FDC_IRQ 6
# endif
# if CONFIG_FT_FDC_DMA == 0
#  undef  CONFIG_FT_FDC_DMA
#  define CONFIG_FT_FDC_DMA 2
# endif
#elif CONFIG_FT_ALT_FDC == 1
# if CONFIG_FT_FDC_BASE == 0
#  undef  CONFIG_FT_FDC_BASE
#  define CONFIG_FT_FDC_BASE 0x370
# endif
# if CONFIG_FT_FDC_IRQ == 0
#  undef  CONFIG_FT_FDC_IRQ
#  define CONFIG_FT_FDC_IRQ 6
# endif
# if CONFIG_FT_FDC_DMA == 0
#  undef  CONFIG_FT_FDC_DMA
#  define CONFIG_FT_FDC_DMA 2
# endif
#else
# if CONFIG_FT_FDC_BASE == 0
#  undef  CONFIG_FT_FDC_BASE
#  define CONFIG_FT_FDC_BASE 0x3f0
# endif
# if CONFIG_FT_FDC_IRQ == 0
#  undef  CONFIG_FT_FDC_IRQ
#  define CONFIG_FT_FDC_IRQ 6
# endif
# if CONFIG_FT_FDC_DMA == 0
#  undef  CONFIG_FT_FDC_DMA
#  define CONFIG_FT_FDC_DMA 2
# endif
#endif
#define ABS(a)          ((a) < 0 ? -(a) : (a))
#define NR_ITEMS(x)     (int)(sizeof(x)/ sizeof(*x))
extern int ftape_init(void);
#endif
#endif