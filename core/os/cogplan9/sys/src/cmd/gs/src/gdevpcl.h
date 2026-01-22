#ifndef gdevpcl_INCLUDED
#  define gdevpcl_INCLUDED
#define PAPER_SIZE_EXECUTIVE 1
#define PAPER_SIZE_LETTER 2
#define PAPER_SIZE_LEGAL 3
#define PAPER_SIZE_LEDGER 6
#define PAPER_SIZE_A4 26
#define PAPER_SIZE_A3 27
#define PAPER_SIZE_A2 28
#define PAPER_SIZE_A1 29
#define PAPER_SIZE_A0 30
#define PAPER_SIZE_JIS_B5 45
#define PAPER_SIZE_JIS_B4 46
#define PAPER_SIZE_JPOST 71
#define PAPER_SIZE_JPOSTD 72
#define PAPER_SIZE_MONARCH 80
#define PAPER_SIZE_COM10 81
#define PAPER_SIZE_DL 90
#define PAPER_SIZE_C5 91
#define PAPER_SIZE_B5 100
int gdev_pcl_paper_size(gx_device *);
dev_proc_map_rgb_color(gdev_pcl_3bit_map_rgb_color);
dev_proc_map_color_rgb(gdev_pcl_3bit_map_color_rgb);
typedef ulong word;
int
gdev_pcl_mode2compress(const word * row, const word * end_row, byte * compressed),
gdev_pcl_mode2compress_padded(const word * row, const word * end_row, byte * compressed, bool pad),
gdev_pcl_mode3compress(int bytecount, const byte * current, byte * previous, byte * compressed),
gdev_pcl_mode9compress(int bytecount, const byte * current, const byte * previous, byte * compressed);
#endif