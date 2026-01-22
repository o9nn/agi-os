#ifndef _FTAPE_HEADER_SEGMENT_H
#define _FTAPE_HEADER_SEGMENT_H
#define FT_SIGNATURE 0
#define FT_FMT_CODE 4
#define FT_REV_LEVEL 5
#define FT_HSEG_1 6
#define FT_HSEG_2 8
#define FT_FRST_SEG 10
#define FT_LAST_SEG 12
#define FT_FMT_DATE 14
#define FT_WR_DATE 18
#define FT_SPT 24
#define FT_TPC 26
#define FT_FHM 27
#define FT_FTM 28
#define FT_FSM 29
#define FT_LABEL 30
#define FT_LABEL_DATE 74
#define FT_LABEL_SZ (FT_LABEL_DATE - FT_LABEL)
#define FT_CMAP_START 78
#define FT_FMT_ERROR 128
#define FT_SEG_CNT 130
#define FT_INIT_DATE 138
#define FT_FMT_CNT 142
#define FT_FSL_CNT 144
#define FT_MK_CODE 146
#define FT_LOT_CODE 190
#define FT_6_HSEG_1 234
#define FT_6_HSEG_2 238
#define FT_6_FRST_SEG 242
#define FT_6_LAST_SEG 246
#define FT_FSL 256
#define FT_HEADER_END 256
#define FT_HSEG_MAGIC 0xaa55aa55
#define FT_D2G_MAGIC 0x82288228
#define FT_YEAR_SHIFT 25
#define FT_YEAR_MASK 0xfe000000
#define FT_YEAR_0 1970
#define FT_YEAR_MAX 127
#define FT_YEAR(year) ((((year)-FT_YEAR_0)<<FT_YEAR_SHIFT)&FT_YEAR_MASK)
#define FT_TIME_SHIFT 0
#define FT_TIME_MASK 0x01FFFFFF
#define FT_TIME_MAX 0x01ea6dff
#define FT_TIME(mo,d,h,m,s) \
((((s)+60*((m)+60*((h)+24*((d)+31*(mo))))) & FT_TIME_MASK))
#define FT_TIME_STAMP(y,mo,d,h,m,s) (FT_YEAR(y) | FT_TIME(mo,d,h,m,s))
typedef enum {
fmt_normal = 2,
fmt_1100ft = 3,
fmt_var = 4,
fmt_425ft = 5,
fmt_big = 6
} ft_format_type;
#define FT_FSL_SIZE (2 * FT_SECTOR_SIZE - FT_HEADER_END)
#define FT_FSL_MAX_ENTRIES (FT_FSL_SIZE/sizeof(__u32))
typedef struct ft_fsl_entry {
__u16 segment;
__u16 date;
} __attribute__ ((packed)) ft_fsl_entry;
#define FT_FSL_TIME_STAMP(y,m,d) \
(((((y) - FT_YEAR_0)<<9)&0xfe00) | (((m)<<5)&0x01e0) | ((d)&0x001f))
#endif