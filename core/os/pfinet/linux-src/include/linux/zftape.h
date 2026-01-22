#ifndef _ZFTAPE_H
#define _ZFTAPE_H
#define ZFTAPE_VERSION "zftape for " FTAPE_VERSION
#include <linux/ftape.h>
#define ZFTAPE_LABEL "Ftape - The Linux Floppy Tape Project!"
#define ZFT_Q80_MODE (1 << 3)
#define ZFT_ZIP_MODE (1 << 4)
#define ZFT_RAW_MODE (1 << 5)
#define ZFT_MINOR_OP_MASK (ZFT_Q80_MODE | \
ZFT_ZIP_MODE | \
ZFT_RAW_MODE)
#define ZFT_MINOR_MASK (FTAPE_SEL_MASK | \
ZFT_MINOR_OP_MASK | \
FTAPE_NO_REWIND)
#ifdef ZFT_OBSOLETE
struct mtblksz {
unsigned int mt_blksz;
};
#define MTIOC_ZFTAPE_GETBLKSZ _IOR('m', 104, struct mtblksz)
#endif
#ifdef __KERNEL__
extern int zft_init(void);
extern inline __s64 zft_div_blksz(__s64 value, __u32 blk_sz)
{
if (blk_sz == 1) {
return value;
} else {
return (__s64)(((__u32)(value >> 10) + (blk_sz >> 10) - 1)
/ (blk_sz >> 10));
}
}
extern inline __s64 zft_mul_blksz(__s64 value, __u32 blk_sz)
{
if (blk_sz == 1) {
return value;
} else {
return(__s64)(((__u32)(value)*(blk_sz>>10))<<10);
}
}
#endif
#endif