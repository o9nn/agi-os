#include "math_.h"
#include <stdlib.h>
#include <assert.h>
#include "gdevprn.h"
#include "gdevpcl.h"
#include "gsparam.h"
#include "gxlum.h"
#define P1(x) x
#define P2(x,y) x,y
#define P3(x,y,z) x,y,z
#define P4(x,y,z,a) x,y,z,a
#define P5(x,y,z,a,b) x,y,z,a,b
#define P6(x,y,z,a,b,c) x,y,z,a,b,c
#define P7(x,y,z,a,b,c,d) x,y,z,a,b,c,d
#define P8(x,y,z,a,b,c,d,e) x,y,z,a,b,c,d,e
#define P9(x,y,z,a,b,c,d,e,f) x,y,z,a,b,c,d,e,f
#define P10(x,y,z,a,b,c,d,e,f,g) x,y,z,a,b,c,d,e,f,g
#define P11(x,y,z,a,b,c,d,e,f,g,h) x,y,z,a,b,c,d,e,f,g,h
#define P12(x,y,z,a,b,c,d,e,f,g,h,i) x,y,z,a,b,c,d,e,f,g,h,i
typedef struct hp850_cmyk_init_s {
byte a[26];
} hp850_cmyk_init_t;
private const hp850_cmyk_init_t hp850_cmyk_init =
{
{
0x02,
0x04,
0x01,
0x2c,
0x01,
0x2c,
0x00,
0x02,
0x01,
0x2c,
0x01,
0x2c,
0x00,
0x02,
0x01,
0x2c,
0x01,
0x2c,
0x00,
0x02,
0x01,
0x2c,
0x01,
0x2c,
0x00,
0x02
}
};
typedef struct {
byte c[256];
byte m[256];
byte y[256];
byte k[256];
int correct[256];
} Gamma;
private const Gamma gammat850 =
{
{0, 0, 0, 2, 2, 2, 3, 3, 3, 5, 5, 5, 7, 7, 6, 7, 7, 6, 7, 7, 7, 8, 8,
8, 8, 8, 8, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 11, 11, 12, 12, 12,
12, 12, 12, 13, 13, 14, 14, 14, 15, 15, 16, 16, 15, 16, 16, 17, 17,
17, 17, 17, 18, 18, 18, 19, 19, 20, 20, 20, 20, 20, 21, 21, 21, 22,
22, 23, 23, 23, 23, 23, 24, 24, 25, 25, 26, 26, 26, 26, 26, 27, 27,
27, 27, 28, 28, 29, 28, 28, 29, 29, 30, 30, 31, 31, 32, 32, 33, 34,
35, 35, 36, 36, 37, 37, 38, 38, 39, 39, 40, 40, 41, 41, 42, 42, 42,
43, 43, 43, 44, 45, 45, 46, 46, 47, 47, 48, 48, 49, 50, 50, 51, 51,
52, 52, 53, 54, 54, 54, 55, 55, 56, 57, 58, 58, 59, 60, 60, 61, 62,
62, 63, 65, 65, 66, 67, 67, 68, 69, 69, 70, 72, 73, 73, 74, 75, 75,
76, 77, 79, 79, 80, 81, 82, 83, 83, 84, 86, 87, 88, 88, 89, 90, 91,
92, 93, 94, 95, 96, 97, 97, 99, 100, 101, 102, 103, 104, 105, 106,
108, 109, 110, 111, 112, 114, 115, 117, 119, 120, 122, 124, 125, 127,
129, 131, 132, 135, 136, 138, 140, 142, 144, 146, 147, 150, 152, 154,
157, 159, 162, 164, 166, 168, 171, 174, 176, 180, 182, 187, 192, 197,
204, 215, 255},
{0, 0, 0, 1, 1, 1, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 7, 7,
7, 8, 8, 8, 9, 9, 10, 10, 9, 10, 10, 10, 11, 11, 11, 11, 11, 12, 12,
12, 13, 13, 13, 14, 14, 15, 15, 15, 16, 16, 16, 16, 16, 17, 17, 17,
17, 17, 18, 18, 19, 19, 19, 19, 19, 20, 20, 20, 21, 21, 22, 22, 22,
23, 23, 24, 24, 25, 25, 25, 26, 26, 27, 27, 28, 29, 29, 29, 29, 30,
30, 31, 30, 31, 31, 32, 31, 31, 32, 32, 33, 33, 34, 34, 35, 35, 36,
36, 37, 37, 38, 38, 39, 39, 40, 40, 41, 41, 42, 42, 43, 43, 44, 44,
45, 45, 46, 46, 47, 48, 48, 49, 49, 50, 50, 51, 51, 52, 53, 53, 54,
54, 55, 55, 56, 57, 57, 58, 58, 59, 60, 60, 61, 61, 62, 63, 64, 65,
66, 66, 67, 68, 68, 70, 71, 71, 72, 73, 73, 74, 76, 77, 77, 78, 79,
79, 80, 81, 82, 83, 84, 85, 86, 87, 87, 88, 89, 90, 91, 91, 92, 93,
94, 95, 96, 97, 98, 99, 100, 100, 101, 102, 103, 105, 106, 107, 108,
109, 112, 113, 114, 115, 116, 118, 119, 121, 123, 124, 125, 128, 129,
130, 133, 134, 135, 138, 139, 142, 144, 145, 148, 150, 152, 154, 157,
159, 162, 164, 168, 169, 170, 172, 175, 177, 179, 182, 185, 189, 193,
198, 204, 215, 255},
{0, 0, 0, 2, 2, 2, 3, 3, 3, 5, 5, 5, 7, 7, 6, 7, 7, 6, 7, 7, 7, 8, 8,
8, 8, 8, 8, 9, 9, 9, 9, 9, 10, 9, 9, 10, 10, 10, 10, 10, 11, 11, 11,
12, 12, 13, 13, 14, 14, 15, 15, 16, 16, 16, 16, 16, 17, 17, 18, 18,
18, 19, 18, 19, 19, 19, 20, 20, 21, 21, 21, 22, 22, 22, 22, 22, 23,
23, 24, 24, 25, 25, 25, 26, 27, 28, 28, 29, 29, 29, 30, 30, 30, 30,
31, 31, 32, 32, 33, 33, 32, 33, 33, 34, 34, 35, 35, 36, 36, 37, 37,
38, 38, 39, 39, 40, 40, 41, 41, 42, 42, 43, 43, 44, 44, 45, 45, 45,
45, 46, 46, 47, 48, 48, 49, 49, 50, 50, 51, 51, 52, 53, 53, 54, 54,
55, 55, 56, 57, 58, 59, 59, 60, 61, 61, 62, 62, 63, 64, 65, 66, 67,
67, 68, 69, 69, 70, 71, 72, 73, 74, 74, 75, 76, 77, 77, 78, 79, 79,
80, 81, 82, 83, 84, 85, 86, 87, 87, 88, 89, 90, 91, 91, 93, 94, 95,
96, 97, 98, 100, 101, 102, 102, 103, 104, 106, 107, 108, 109, 110,
111, 113, 114, 115, 116, 117, 118, 119, 121, 123, 124, 126, 128, 130,
131, 134, 135, 137, 139, 140, 143, 145, 146, 148, 150, 152, 154, 156,
158, 160, 163, 166, 167, 169, 171, 173, 176, 178, 181, 184, 188, 192,
198, 204, 215, 255},
{0, 0, 0, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 2, 4, 3, 3, 3, 3, 3, 4, 4,
4, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 8, 8, 8, 9, 9, 8,
8, 8, 9, 9, 9, 10, 10, 10, 10, 10, 11, 11, 11, 11, 12, 12, 12, 13, 13,
12, 12, 12, 13, 13, 13, 13, 13, 14, 14, 14, 14, 14, 15, 15, 16, 16,
16, 17, 17, 17, 17, 18, 18, 18, 19, 19, 20, 20, 20, 20, 20, 21, 21,
21, 21, 22, 22, 22, 22, 23, 22, 23, 23, 24, 24, 24, 24, 25, 25, 26,
26, 26, 26, 27, 27, 28, 28, 28, 28, 29, 29, 30, 30, 31, 31, 31, 32,
32, 33, 33, 34, 34, 35, 36, 36, 36, 37, 37, 37, 38, 38, 40, 40, 40,
41, 41, 42, 43, 43, 43, 43, 44, 45, 45, 46, 47, 47, 48, 49, 49, 50,
52, 52, 53, 54, 54, 56, 56, 57, 58, 59, 60, 60, 61, 62, 63, 63, 64,
65, 66, 67, 68, 69, 70, 71, 72, 72, 73, 75, 75, 76, 77, 78, 80, 81,
82, 82, 83, 84, 85, 86, 88, 89, 90, 91, 94, 95, 96, 98, 99, 100, 101,
103, 105, 106, 107, 110, 111, 112, 115, 116, 118, 120, 121, 124, 126,
127, 131, 133, 134, 138, 140, 141, 146, 148, 151, 154, 156, 160, 163,
166, 169, 174, 177, 182, 187, 194, 203, 215, 255}
};
private const Gamma gammat890 =
{
{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125,
126, 127,
128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141,
142, 143,
144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157,
158, 159,
160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173,
174, 175,
176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189,
190, 191,
192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205,
206, 207,
208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221,
222, 223,
224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237,
238, 239,
240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253,
254, 255},
{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125,
126, 127,
128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141,
142, 143,
144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157,
158, 159,
160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173,
174, 175,
176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189,
190, 191,
192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205,
206, 207,
208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221,
222, 223,
224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237,
238, 239,
240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253,
254, 255},
{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125,
126, 127,
128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141,
142, 143,
144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157,
158, 159,
160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173,
174, 175,
176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189,
190, 191,
192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205,
206, 207,
208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221,
222, 223,
224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237,
238, 239,
240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253,
254, 255},
{0, 0, 0, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 2, 4, 3, 3, 3, 3, 3, 4, 4,
4, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 8, 8, 8, 9, 9, 8,
8, 8, 9, 9, 9, 10, 10, 10, 10, 10, 11, 11, 11, 11, 12, 12, 12, 13, 13,
12, 12, 12, 13, 13, 13, 13, 13, 14, 14, 14, 14, 14, 15, 15, 16, 16,
16, 17, 17, 17, 17, 18, 18, 18, 19, 19, 20, 20, 20, 20, 20, 21, 21,
21, 21, 22, 22, 22, 22, 23, 22, 23, 23, 24, 24, 24, 24, 25, 25, 26,
26, 26, 26, 27, 27, 28, 28, 28, 28, 29, 29, 30, 30, 31, 31, 31, 32,
32, 33, 33, 34, 34, 35, 36, 36, 36, 37, 37, 37, 38, 38, 40, 40, 40,
41, 41, 42, 43, 43, 43, 43, 44, 45, 45, 46, 47, 47, 48, 49, 49, 50,
52, 52, 53, 54, 54, 56, 56, 57, 58, 59, 60, 60, 61, 62, 63, 63, 64,
65, 66, 67, 68, 69, 70, 71, 72, 72, 73, 75, 75, 76, 77, 78, 80, 81,
82, 82, 83, 84, 85, 86, 88, 89, 90, 91, 94, 95, 96, 98, 99, 100, 101,
103, 105, 106, 107, 110, 111, 112, 115, 116, 118, 120, 121, 124, 126,
127, 131, 133, 134, 138, 140, 141, 146, 148, 151, 154, 156, 160, 163,
166, 169, 174, 177, 182, 187, 194, 203, 215, 255}
};
private const Gamma * const gammat[] =
{
&gammat850,
&gammat850,
&gammat890,
&gammat850
};
private int
rescale_byte_wise1x1(P4(int bytecount, const byte * inbytea,
const byte * inbyteb, byte * outbyte));
private int
rescale_byte_wise2x1(P4(int bytecount, const byte * inbytea,
const byte * inbyteb, byte * outbyte));
private int
rescale_byte_wise1x2(P4(int bytecount, const byte * inbytea,
const byte * inbyteb, byte * outbyte));
private int
rescale_byte_wise2x2(P4(int bytecount, const byte * inbytea,
const byte * inbyteb, byte * outbyte));
private int (* const rescale_color_plane[2][2]) (P4(int, const byte *, const byte *, byte *)) = {
{
rescale_byte_wise1x1, rescale_byte_wise1x2
},
{
rescale_byte_wise2x1, rescale_byte_wise2x2
}
};
#define DESKJET_PRINT_LIMIT  0.04
#define DESKJET_MARGINS_LETTER   0.25, 0.50, 0.25, 0.167
#define DESKJET_MARGINS_A4       0.13, 0.46, 0.13, 0.04
#ifndef BITSPERPIXEL
#  define BITSPERPIXEL 32
#endif
#define DOFFSET (dev_t_margin(pdev) - DESKJET_PRINT_LIMIT)
#define W sizeof(word)
#define I sizeof(int)
typedef enum {
PLAIN_PAPER, BOND_PAPER, SPECIAL_PAPER, GLOSSY_FILM, TRANSPARENCY_FILM
} cdj_paper_type_t;
typedef enum {
DRAFT = -1, NORMAL = 0, PRESENTATION = 1
} cdj_quality_t;
typedef enum {
DJ670C, DJ850C, DJ890C, DJ1600C
} cdj_printer_type_t;
#define HEAD_ROWS_MONO 50
#define HEAD_ROWS_COLOUR 16
private dev_proc_map_cmyk_color(gdev_cmyk_map_cmyk_color);
private dev_proc_map_rgb_color(gdev_cmyk_map_rgb_color);
private dev_proc_map_color_rgb(gdev_cmyk_map_color_rgb);
private dev_proc_map_rgb_color(gdev_pcl_map_rgb_color);
private dev_proc_map_color_rgb(gdev_pcl_map_color_rgb);
private dev_proc_open_device(hp_colour_open);
private dev_proc_get_params(cdj850_get_params);
private dev_proc_put_params(cdj850_put_params);
private dev_proc_print_page(cdj850_print_page);
#define prn_colour_device_body(dtype, procs, dname, w10, h10, xdpi, ydpi, lm, bm, rm, tm, ncomp, depth, mg, mc, dg, dc, print_page, cmyk, correct)\
prn_device_body(dtype, procs, dname, w10, h10, xdpi, ydpi, lm, bm, rm, tm, ncomp, depth, mg, mc, dg, dc, print_page), cmyk, depth , correct
#define gx_prn_colour_device_common \
gx_prn_device_common; \
int cmyk;	  	 \
\
uint default_depth;	 \
uint correction
struct error_val_field {
int c;
int m;
int y;
int k;
};
struct ptr_arrays {
byte *data[4];
byte *data_c[4];
byte *plane_data[4][4];
byte *plane_data_c[4][8];
byte *out_data;
byte *test_data[4];
int *errors[2];
int *errors_c[2];
word *storage;
word *storage_start;
word *storage_end;
word *storage_size;
};
struct misc_struct {
int line_size;
int line_size_c;
int line_size_words;
int paper_size;
int num_comps;
int bits_per_pixel;
int storage_bpp;
int expanded_bpp;
int plane_size;
int plane_size_c;
int databuff_size;
int databuff_size_c;
int errbuff_size;
int errbuff_size_c;
int outbuff_size;
int scan;
int cscan;
int is_two_pass;
int zero_row_count;
uint storage_size_words;
uint storage_size_words_c;
int is_color_data;
};
typedef void (*StartRasterMode) (P3(gx_device_printer * pdev, int paper_size,
FILE * prn_stream));
typedef void (*PrintNonBlankLines) (P6(gx_device_printer * pdev,
struct ptr_arrays *data_ptrs,
struct misc_struct *misc_vars,
struct error_val_field *error_values,
const Gamma *gamma,
FILE * prn_stream));
typedef void (*TerminatePage) (P2(gx_device_printer * pdev, FILE * prn_stream));
typedef struct gx_device_cdj850_s {
gx_device_common;
gx_prn_colour_device_common;
int  quality;
int  papertype;
int intensities;
int xscal;
int yscal;
int  ptype;
int compression;
float mastergamma;
float gammavalc;
float gammavalm;
float gammavaly;
float gammavalk;
float blackcorrect;
StartRasterMode start_raster_mode;
PrintNonBlankLines print_non_blank_lines;
TerminatePage terminate_page;
} gx_device_cdj850;
typedef struct {
gx_device_common;
gx_prn_colour_device_common;
} gx_device_colour_prn;
#define cprn_device     ((gx_device_colour_prn*) pdev)
#define cdj850    ((gx_device_cdj850 *)pdev)
#define prn_cmyk_colour_device(dtype, procs, dev_name, x_dpi, y_dpi, bpp, print_page, correct)\
prn_colour_device_body(dtype, procs, dev_name,\
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS, x_dpi, y_dpi, 0, 0, 0, 0,\
((bpp == 1 || bpp == 4) ? 1 : 4), bpp,\
(bpp > 8 ? 255 : 1), (1 << (bpp >> 2)) - 1, \
(bpp > 8 ? 5 : 2), (bpp > 8 ? 5 : bpp > 1 ? 2 : 0),\
print_page, 1 , correct)
#define prn_cmy_colour_device(dtype, procs, dev_name, x_dpi, y_dpi, bpp, print_page, correct)\
prn_colour_device_body(dtype, procs, dev_name,\
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS, x_dpi, y_dpi, 0, 0, 0, 0,\
((bpp == 1 || bpp == 4) ? 1 : 3), bpp,\
(bpp > 8 ? 255 : 1), (bpp > 8 ? 255 : 1), \
(bpp > 8 ? 5 : 2), (bpp > 8 ? 5 : bpp > 1 ? 2 : 0),\
print_page, -1 , correct)
#define cdj_850_device(procs, dev_name, x_dpi, y_dpi, bpp, print_page, correction, quality, papertype, intensities,ptype,compression,mastergamma,gammavalc,gammavalm,gammavaly,gammavalk,blackcorrect,start_raster_mode,print_non_blank_line,terminate_page)\
{ prn_cmyk_colour_device(gx_device_cdj850, procs, dev_name, x_dpi, y_dpi, bpp, print_page, correction),\
quality,\
papertype,\
intensities,\
0, 0, \
ptype,\
compression,\
mastergamma,\
gammavalc,\
gammavalm,\
gammavaly,\
gammavalk,\
blackcorrect,\
start_raster_mode,\
print_non_blank_line,\
terminate_page\
}
#define cdj_1600_device(procs, dev_name, x_dpi, y_dpi, bpp, print_page, correction, quality, papertype, intensities,ptype,compression,mastergamma,gammavalc,gammavalm,gammavaly,gammavalk,blackcorrect,start_raster_mode,print_non_blank_line,terminate_page)\
{ prn_cmy_colour_device(gx_device_cdj850, procs, dev_name, x_dpi, y_dpi, bpp, print_page, correction),\
quality,\
papertype,\
intensities,\
0, 0, \
ptype,\
compression,\
mastergamma,\
gammavalc,\
gammavalm,\
gammavaly,\
gammavalk,\
blackcorrect,\
start_raster_mode,\
print_non_blank_line,\
terminate_page\
}
#define cmyk_colour_procs(proc_colour_open, proc_get_params, proc_put_params, \
map_rgb_color, map_color_rgb, map_cmyk_color) {\
proc_colour_open,\
gx_default_get_initial_matrix,\
gx_default_sync_output,\
gdev_prn_output_page,\
gdev_prn_close,\
map_rgb_color,\
map_color_rgb,\
NULL ,\
NULL ,\
NULL ,\
NULL ,\
NULL ,\
gx_default_get_bits,\
proc_get_params,\
proc_put_params,\
map_cmyk_color\
}
private void
cdj850_start_raster_mode(P3(gx_device_printer * pdev,
int papersize, FILE * prn_stream));
private void
cdj850_print_non_blank_lines(P6(gx_device_printer * pdev,
struct ptr_arrays *data_ptrs,
struct misc_struct *misc_vars,
struct error_val_field *error_values,
const Gamma *gamma,
FILE * prn_stream));
private void
cdj850_terminate_page(P2(gx_device_printer * pdev, FILE * prn_stream));
private void
cdj1600_start_raster_mode(P3(gx_device_printer * pdev,
int papersize, FILE * prn_stream));
private void
cdj1600_print_non_blank_lines(P6(gx_device_printer * pdev,
struct ptr_arrays *data_ptrs,
struct misc_struct *misc_vars,
struct error_val_field *error_values,
const Gamma *gamma,
FILE * prn_stream));
private void
cdj1600_terminate_page(P2(gx_device_printer * pdev, FILE * prn_stream));
private const gx_device_procs cdj670_procs =
cmyk_colour_procs(hp_colour_open, cdj850_get_params, cdj850_put_params,
NULL, gdev_cmyk_map_color_rgb, gdev_cmyk_map_cmyk_color);
private const gx_device_procs cdj850_procs =
cmyk_colour_procs(hp_colour_open, cdj850_get_params, cdj850_put_params,
NULL, gdev_cmyk_map_color_rgb, gdev_cmyk_map_cmyk_color);
private const gx_device_procs cdj890_procs =
cmyk_colour_procs(hp_colour_open, cdj850_get_params, cdj850_put_params,
NULL, gdev_cmyk_map_color_rgb, gdev_cmyk_map_cmyk_color);
private const gx_device_procs cdj1600_procs =
cmyk_colour_procs(hp_colour_open, cdj850_get_params, cdj850_put_params,
gdev_pcl_map_rgb_color, gdev_pcl_map_color_rgb, NULL);
const gx_device_cdj850 gs_cdj670_device =
cdj_850_device(cdj670_procs, "cdj670", 600, 600, 32, cdj850_print_page, 0,
PRESENTATION, PLAIN_PAPER, 2, DJ670C, 9,
1.0, 0.0, 0.0, 0.0, 0.0, 1.0,
cdj850_start_raster_mode, cdj850_print_non_blank_lines,
cdj850_terminate_page);
const gx_device_cdj850 gs_cdj850_device =
cdj_850_device(cdj850_procs, "cdj850", 600, 600, 32, cdj850_print_page, 0,
PRESENTATION, PLAIN_PAPER, 4, DJ850C, 9,
1.0, 0.0, 0.0, 0.0, 0.0, 1.0,
cdj850_start_raster_mode, cdj850_print_non_blank_lines,
cdj850_terminate_page);
const gx_device_cdj850 gs_cdj890_device =
cdj_850_device(cdj890_procs, "cdj890", 600, 600, 32, cdj850_print_page, 0,
PRESENTATION, PLAIN_PAPER, 4, DJ890C, 9,
1.0, 0.0, 0.0, 0.0, 0.0, 1.0,
cdj850_start_raster_mode, cdj850_print_non_blank_lines,
cdj850_terminate_page);
const gx_device_cdj850 gs_cdj1600_device =
cdj_1600_device(cdj1600_procs, "cdj1600", 300, 300, 24, cdj850_print_page, 0,
PRESENTATION, PLAIN_PAPER, 2, DJ1600C, 3,
1.0, 0.0, 0.0, 0.0, 0.0, 1.0,
cdj1600_start_raster_mode, cdj1600_print_non_blank_lines,
cdj1600_terminate_page);
private int cdj_put_param_int(P6(gs_param_list *, gs_param_name,
int *, int, int, int));
private int cdj_put_param_float(P6(gs_param_list *, gs_param_name, float
*, float, float, int));
private int cdj_put_param_bpp(P5(gx_device *, gs_param_list *, int, int, int));
private int cdj_set_bpp(P3(gx_device *, int, int));
private int
hp_colour_open(gx_device * pdev)
{
static const float dj_a4[4] = {
DESKJET_MARGINS_A4
};
static const float dj_letter[4] = {
DESKJET_MARGINS_LETTER
};
static const float m_cdj1600[4] = {
0.25, 0.5, 0.25, 0.5
};
const float *m = (float *)0;
if (pdev->color_info.num_components == 0) {
int code = cdj_set_bpp(pdev, pdev->color_info.depth,
pdev->color_info.num_components);
if (code < 0)
return code;
}
switch (cdj850->ptype) {
case DJ670C:
if (cdj850->papertype <= SPECIAL_PAPER) {
if (cdj850->quality == DRAFT) {
gx_device_set_resolution(pdev, 300.0, 300.0);
cdj850->xscal = 0;
cdj850->yscal = 0;
} else if (cdj850->quality == NORMAL) {
gx_device_set_resolution(pdev, 600.0, 300.0);
cdj850->xscal = 1;
cdj850->yscal = 0;
} else {
gx_device_set_resolution(pdev, 600.0, 600.0);
cdj850->xscal = 1;
cdj850->yscal = 1;
}
} else {
gx_device_set_resolution(pdev, 600.0, 300.0);
cdj850->xscal = 0;
cdj850->yscal = 0;
}
m = (gdev_pcl_paper_size(pdev) == PAPER_SIZE_A4 ? dj_a4 : dj_letter);
break;
case DJ850C:
case DJ890C:
if (cdj850->quality == DRAFT) {
gx_device_set_resolution(pdev, 300.0, 300.0);
cdj850->xscal = 0;
cdj850->yscal = 0;
cdj850->intensities = 2;
} else if (cdj850->quality == NORMAL) {
gx_device_set_resolution(pdev, 600.0, 300.0);
cdj850->xscal = 1;
cdj850->yscal = 0;
if (cdj850->papertype <= PLAIN_PAPER) {
cdj850->intensities = 3;
}
} else {
gx_device_set_resolution(pdev, 600.0, 600.0);
cdj850->xscal = 1;
cdj850->yscal = 1;
}
m = (gdev_pcl_paper_size(pdev) == PAPER_SIZE_A4 ? dj_a4 : dj_letter);
break;
case DJ1600C:
gx_device_set_resolution(pdev, 300.0, 300.0);
m = m_cdj1600;
break;
default:
assert(0);
}
gx_device_set_margins(pdev, m, true);
return gdev_prn_open(pdev);
}
private int
cdj850_get_params(gx_device * pdev, gs_param_list * plist)
{
int code = gdev_prn_get_params(pdev, plist);
if (code < 0 ||
(code = param_write_int(plist, "Quality", &cdj850->quality)) < 0 ||
(code = param_write_int(plist, "Papertype", &cdj850->papertype)) < 0 ||
(code = param_write_float(plist, "MasterGamma", &cdj850->gammavalc))
< 0 ||
(code = param_write_float(plist, "GammaValC", &cdj850->gammavalc)) <
0 ||
(code = param_write_float(plist, "GammaValM", &cdj850->gammavalm)) <
0 ||
(code = param_write_float(plist, "GammaValY", &cdj850->gammavaly)) <
0 ||
(code = param_write_float(plist, "GammaValK", &cdj850->gammavalk)) <
0 ||
(code = param_write_float(plist, "BlackCorrect",
&cdj850->blackcorrect)) < 0
)
return code;
return code;
}
private int
cdj850_put_params(gx_device * pdev, gs_param_list * plist)
{
int quality = cdj850->quality;
int papertype = cdj850->papertype;
float mastergamma = cdj850->mastergamma;
float gammavalc = cdj850->gammavalc;
float gammavalm = cdj850->gammavalm;
float gammavaly = cdj850->gammavaly;
float gammavalk = cdj850->gammavalk;
float blackcorrect = cdj850->blackcorrect;
int bpp = 0;
int code = 0;
code = cdj_put_param_int(plist, "BitsPerPixel", &bpp, 1, 32, code);
code = cdj_put_param_int(plist, "Quality", &quality, 0, 2, code);
code = cdj_put_param_int(plist, "Papertype", &papertype, 0, 4, code);
code = cdj_put_param_float(plist, "MasterGamma", &mastergamma, 0.1, 9.0, code);
code = cdj_put_param_float(plist, "GammaValC", &gammavalc, 0.0, 9.0, code);
code = cdj_put_param_float(plist, "GammaValM", &gammavalm, 0.0, 9.0, code);
code = cdj_put_param_float(plist, "GammaValY", &gammavaly, 0.0, 9.0, code);
code = cdj_put_param_float(plist, "GammaValK", &gammavalk, 0.0, 9.0, code);
code = cdj_put_param_float(plist, "BlackCorrect", &blackcorrect, 0.0,
9.0, code);
if (code < 0)
return code;
code = cdj_put_param_bpp(pdev, plist, bpp, bpp, 0);
if (code < 0)
return code;
cdj850->quality = quality;
cdj850->papertype = papertype;
cdj850->mastergamma = mastergamma;
cdj850->gammavalc = gammavalc;
cdj850->gammavalm = gammavalm;
cdj850->gammavaly = gammavaly;
cdj850->gammavalk = gammavalk;
cdj850->blackcorrect = blackcorrect;
return 0;
}
#define x_dpi        (pdev->x_pixels_per_inch)
#define y_dpi        (pdev->y_pixels_per_inch)
#define calc_buffsize(a, b) (((((a) + ((b) * W) - 1) / ((b) * W))) * W)
private void
FSDlinebw(P7(int scan, int plane_size,
struct error_val_field *error_values,
byte * kP,
int n, int *ep, byte * dp));
private void
FSDlinec2(P9(int scan, int plane_size,
struct error_val_field *error_values,
byte * cPa, byte * mPa, byte * yPa, int n,
byte * dp, int *ep));
private void
FSDlinec3(P12(int scan, int plane_size,
struct error_val_field *error_values,
byte * cPa, byte * mPa, byte * yPa,
byte * cPb, byte * mPb, byte * yPb,
int n, byte * dp, int *ep));
private void
FSDlinec4(P12(int scan, int plane_size,
struct error_val_field *error_values,
byte * cPa, byte * mPa, byte * yPa,
byte * cPb, byte * mPb, byte * yPb,
int n, byte * dp, int *ep));
private void
init_error_buffer(struct misc_struct *misc_vars,
struct ptr_arrays *data_ptrs);
private void
do_floyd_steinberg(P8(int scan, int cscan, int plane_size,
int plane_size_c, int n,
struct ptr_arrays *data_ptrs,
gx_device_printer * pdev,
struct error_val_field *error_values));
private int
do_gcr(P7(int bytecount, byte * inbyte, const byte * kvalues,
const byte * cvalues, const byte * mvalues,
const byte * yvalues, const int *kcorrect));
private void
send_scan_lines(P6(gx_device_printer * pdev,
struct ptr_arrays *data_ptrs,
struct misc_struct *misc_vars,
struct error_val_field *error_values,
const Gamma *gamma,
FILE * prn_stream));
private void
do_gamma(P3(float mastergamma, float gammaval, byte * values));
private void
do_black_correction(P2(float kvalue, int *kcorrect));
private void
init_data_structure(P3(gx_device_printer * pdev,
struct ptr_arrays *data_ptrs,
struct misc_struct *misc_vars));
private void
calculate_memory_size(P2(gx_device_printer * pdev,
struct misc_struct *misc_vars));
private void
assign_dpi(int dpi, byte * msb)
{
if (dpi == 600) {
msb[0] = 0x02;
msb[1] = 0x58;
} else {
msb[0] = 0x01;
msb[1] = 0x2c;
}
}
private void
cdj850_terminate_page(gx_device_printer * pdev, FILE * prn_stream)
{
fputs("0M", prn_stream);
fputs("\033*rC\033E", prn_stream);
fputs("\033&l0H", prn_stream);
}
private int
cdj850_print_page(gx_device_printer * pdev, FILE * prn_stream)
{
struct error_val_field error_values;
struct ptr_arrays data_ptrs;
struct misc_struct misc_vars;
Gamma gamma;
memcpy(&gamma, gammat[cdj850->ptype], sizeof(Gamma));
if (cdj850->mastergamma > 1.0) {
do_gamma(cdj850->mastergamma, cdj850->gammavalk, gamma.k);
do_gamma(cdj850->mastergamma, cdj850->gammavalc, gamma.c);
do_gamma(cdj850->mastergamma, cdj850->gammavalm, gamma.m);
do_gamma(cdj850->mastergamma, cdj850->gammavaly, gamma.y);
}
do_black_correction(cdj850->blackcorrect, gamma.correct);
calculate_memory_size(pdev, &misc_vars);
data_ptrs.storage = (ulong *) gs_malloc(pdev->memory, misc_vars.storage_size_words, W,
"cdj850_print_page");
if (data_ptrs.storage == 0) {
return_error(gs_error_VMerror);
}
init_data_structure(pdev, &data_ptrs, &misc_vars);
(*cdj850->start_raster_mode) (pdev, misc_vars.paper_size, prn_stream);
send_scan_lines(pdev, &data_ptrs, &misc_vars,
&error_values, &gamma, prn_stream);
(*cdj850->terminate_page) (pdev, prn_stream);
gs_free(pdev->memory, (char *)data_ptrs.storage, misc_vars.storage_size_words, W,
"hp850_print_page");
return 0;
}
#define odd(i) ((i & 01) != 0)
private int
GetScanLine(gx_device_printer * pdev, int *lnum,
struct ptr_arrays *data_ptrs,
struct misc_struct *misc_vars,
word rmask)
{
word *data_words = (word *) data_ptrs->data[misc_vars->scan];
register word *end_data = data_words + misc_vars->line_size_words;
++(*lnum);
gdev_prn_copy_scan_lines(pdev, *lnum, (byte *) data_words, misc_vars->line_size);
misc_vars->scan = 1 - misc_vars->scan;
misc_vars->is_two_pass = odd(*lnum);
end_data[-1] &= rmask;
while (end_data > data_words && end_data[-1] == 0)
end_data--;
return end_data - data_words;
}
private void
send_scan_lines(gx_device_printer * pdev,
struct ptr_arrays *data_ptrs,
struct misc_struct *misc_vars,
struct error_val_field *error_values,
const Gamma *gamma,
FILE * prn_stream)
{
int lnum, lend, llen;
int num_blank_lines = 0;
word rmask =
~(word) 0 << ((-pdev->width * misc_vars->storage_bpp) & (W * 8 - 1));
lend = pdev->height - (dev_t_margin(pdev) + dev_b_margin(pdev)) * y_dpi;
error_values->c = error_values->m = error_values->y =
error_values->k = 0;
init_error_buffer(misc_vars, data_ptrs);
misc_vars->zero_row_count = 0;
lnum = -1;
llen = GetScanLine(pdev, &lnum, data_ptrs, misc_vars, rmask);
while (lnum < lend) {
num_blank_lines = 0;
while (lnum < lend && llen == 0) {
++num_blank_lines;
llen = GetScanLine(pdev, &lnum, data_ptrs, misc_vars, rmask);
}
if (lnum >= lend) {
break;
}
if (num_blank_lines > 0) {
fprintf(prn_stream, "%dy", num_blank_lines / (cdj850->yscal + 1));
memset(data_ptrs->plane_data[0][0], 0,
(misc_vars->plane_size * 2 * misc_vars->num_comps));
memset(data_ptrs->plane_data_c[0][0], 0,
(misc_vars->plane_size_c * 2 * misc_vars->num_comps));
}
if (cdj850->yscal && odd(lnum)) {
putc('v', prn_stream);
}
while (lnum < lend && llen != 0) {
misc_vars->is_color_data = 0;
(*cdj850->print_non_blank_lines) (pdev, data_ptrs, misc_vars,
error_values, gamma, prn_stream);
llen = GetScanLine(pdev, &lnum, data_ptrs, misc_vars, rmask);
}
if (cdj850->yscal && odd(lnum)) {
(*cdj850->print_non_blank_lines) (pdev, data_ptrs, misc_vars,
error_values, gamma, prn_stream);
}
}
}
private void
print_c9plane(FILE * prn_stream, char plane_code, int plane_size,
const byte * curr, const byte * prev, byte * out_data)
{
int out_count = gdev_pcl_mode9compress(plane_size, curr, prev, out_data);
if (out_count > 0) {
fprintf(prn_stream, "%d%c", out_count, plane_code);
fwrite(out_data, sizeof(byte), out_count, prn_stream);
} else {
putc(plane_code, prn_stream);
}
}
private void
cdj850_print_non_blank_lines(gx_device_printer * pdev,
struct ptr_arrays *data_ptrs,
struct misc_struct *misc_vars,
struct error_val_field *error_values,
const Gamma *gamma,
FILE * prn_stream)
{
static const char *const plane_code[2] =
{"wvvv", "vvvv"};
int i;
byte *kP = data_ptrs->plane_data[misc_vars->scan + 2][3];
byte *dp = data_ptrs->data[misc_vars->scan + 2];
int *ep = data_ptrs->errors[misc_vars->scan];
misc_vars->is_color_data =
do_gcr(misc_vars->databuff_size, data_ptrs->data[misc_vars->scan],
gamma->k, gamma->c, gamma->m, gamma->y, gamma->correct);
FSDlinebw(misc_vars->scan, misc_vars->plane_size,
error_values, kP, misc_vars->num_comps, ep, dp);
print_c9plane(prn_stream, 'v', misc_vars->plane_size,
data_ptrs->plane_data[misc_vars->scan][3],
data_ptrs->plane_data[1 - misc_vars->scan][3],
data_ptrs->out_data);
if (!cdj850->yscal || misc_vars->is_two_pass) {
int plane_size_c = (*rescale_color_plane[cdj850->xscal][cdj850->yscal])
(misc_vars->databuff_size,
data_ptrs->data[misc_vars->scan],
data_ptrs->data[1 - misc_vars->scan],
data_ptrs->data_c[misc_vars->cscan]) / misc_vars->storage_bpp;
do_floyd_steinberg(misc_vars->scan, misc_vars->cscan,
misc_vars->plane_size, plane_size_c,
misc_vars->num_comps, data_ptrs, pdev, error_values);
for (i = misc_vars->num_comps - 2; i >= 0; i--) {
print_c9plane(prn_stream, plane_code[cdj850->intensities > 2][i],
plane_size_c,
data_ptrs->plane_data_c[misc_vars->cscan][i],
data_ptrs->plane_data_c[1 - misc_vars->cscan][i],
data_ptrs->out_data);
if (cdj850->intensities > 2) {
print_c9plane(prn_stream, plane_code[0][i], plane_size_c,
data_ptrs->plane_data_c[misc_vars->cscan][i + 4],
data_ptrs->plane_data_c[1 -
misc_vars->cscan][i
+ 4],
data_ptrs->out_data);
}
}
misc_vars->cscan = 1 - misc_vars->cscan;
}
return;
}
private void
do_floyd_steinberg(int scan, int cscan, int plane_size,
int plane_size_c, int n,
struct ptr_arrays *data_ptrs,
gx_device_printer * pdev,
struct error_val_field *error_values)
{
byte *cPa, *mPa, *yPa, *cPb, *mPb, *yPb;
byte *dpc;
int *epc;
byte *kP, *dp;
int *ep;
cPa = data_ptrs->plane_data_c[cscan + 2][2];
mPa = data_ptrs->plane_data_c[cscan + 2][1];
yPa = data_ptrs->plane_data_c[cscan + 2][0];
cPb = data_ptrs->plane_data_c[cscan + 2][6];
mPb = data_ptrs->plane_data_c[cscan + 2][5];
yPb = data_ptrs->plane_data_c[cscan + 2][4];
dpc = data_ptrs->data_c[cscan + 2];
epc = data_ptrs->errors_c[cscan];
kP = data_ptrs->plane_data[scan + 2][3];
dp = data_ptrs->data[scan + 2];
ep = data_ptrs->errors[scan];
switch (cdj850->intensities) {
case 2:
FSDlinec2(cscan, plane_size_c, error_values,
cPa, mPa, yPa, n, dpc, epc);
break;
case 3:
FSDlinec3(cscan, plane_size_c, error_values,
cPa, mPa, yPa, cPb, mPb, yPb, n, dpc, epc);
break;
case 4:
FSDlinec4(cscan, plane_size_c, error_values,
cPa, mPa, yPa, cPb, mPb, yPb, n, dpc, epc);
break;
default:
assert(0);
}
return;
}
private void
do_gamma(float mastergamma, float gammaval, byte values[256])
{
int i;
float gamma;
if (gammaval > 0.0) {
gamma = gammaval;
} else {
gamma = mastergamma;
}
for (i = 0; i < 256; i++) {
values[i] = (byte) (255.0 *
(1.0 - pow(((double)(255.0 - (float)i) / 255.0),
(double)(1.0 / gamma))));
}
return;
}
private void
do_black_correction(float kvalue, int kcorrect[256])
{
int i;
for (i = 0; i < 256; i++) {
kcorrect[i] = (int)
(100.0 * kvalue * (
pow(10.0,
pow((i / 255.0), 3.0)
)
- 1.0
)
);
}
return;
}
#define DOUCR(col1, col2, col3, col4)\
{\
\
\
\
\
\
\
a = *col1 - *col2;\
b = *col2 - *col3;\
if (a >= b) {\
grey_distance = 1.0 - (b/255.0);\
} else {\
grey_distance = 1.0 - (a/255.0);\
}\
ucr   = (byte) (*col3 * grey_distance); \
*col4 = *col4 + ucr;  \
\
\
ucr   = *(kvalues + ucr);\
*col1 = *col1 - ucr ;\
*col2 = *col2 - ucr ;\
*col3 = *col3 - ucr ;\
}
#define DOGCR(col1, col2, col3, col4)\
{\
ucr = (int) *col3;\
*col1 -= ucr ;\
*col2 -= ucr ;\
*col3 -= ucr ;\
*col4 += ucr;  \
kadd  = ucr + *(kcorrect + ucr);\
uca_fac = 1.0 + (kadd/255.0);\
*col1 *= uca_fac;\
*col2 *= uca_fac;\
}
private int
do_gcr(int bytecount, byte * inbyte, const byte kvalues[256],
const byte cvalues[256], const byte mvalues[256],
const byte yvalues[256], const int kcorrect[256])
{
int i, ucr, kadd, is_color = 0;
byte *black, *cyan, *magenta, *yellow;
float uca_fac;
for (i = 0; i < bytecount; i += 4) {
black = inbyte++;
cyan = inbyte++;
magenta = inbyte++;
yellow = inbyte++;
if (*magenta + *yellow + *cyan > 0) {
is_color = 1;
if ((*cyan >= *magenta)
&& (*magenta >= *yellow)
&& (*yellow > 0)) {
DOGCR(cyan, magenta, yellow, black);
} else if ((*cyan >= *yellow)
&& (*yellow >= *magenta)
&& (*magenta > 0)) {
DOGCR(cyan, yellow, magenta, black);
} else if ((*yellow >= *magenta)
&& (*magenta >= *cyan)
&& (*cyan > 0)) {
DOGCR(yellow, magenta, cyan, black);
} else if ((*yellow >= *cyan)
&& (*cyan >= *magenta)
&& (*magenta > 0)) {
DOGCR(yellow, cyan, magenta, black);
} else if ((*magenta >= *yellow)
&& (*yellow >= *cyan)
&& (*cyan > 0)) {
DOGCR(magenta, yellow, cyan, black);
} else if ((*magenta >= *cyan)
&& (*cyan >= *yellow)
&& (*yellow > 0)) {
DOGCR(magenta, cyan, yellow, black);
} else {
}
*cyan = *(cvalues + *cyan);
*magenta = *(mvalues + *magenta);
*yellow = *(yvalues + *yellow);
}
*black = *(kvalues + *black);
}
return is_color;
}
private int
rescale_byte_wise2x2(int bytecount, const byte * inbytea, const byte * inbyteb,
byte * outbyte)
{
register int i, j;
int max = bytecount / 2;
for (i = 0; i < max; i += 4) {
j = 2 * i;
outbyte[i + 1] = (inbytea[j + 1] + inbytea[j + 5] + inbyteb[j + 1] +
inbyteb[j + 5]) / 4;
outbyte[i + 2] = (inbytea[j + 2] + inbytea[j + 6] + inbyteb[j + 2] +
inbyteb[j + 6]) / 4;
outbyte[i + 3] = (inbytea[j + 3] + inbytea[j + 7] + inbyteb[j + 3] +
inbyteb[j + 7]) / 4;
}
return max;
}
private int
rescale_byte_wise2x1(int bytecount, const byte * inbytea, const byte * inbyteb,
byte * outbyte)
{
register int i, j;
int max = bytecount / 2;
for (i = 0; i < max; i += 4) {
j = 2 * i;
outbyte[i + 1] = (inbytea[j + 1] + inbytea[j + 5]) / 2;
outbyte[i + 2] = (inbytea[j + 2] + inbytea[j + 6]) / 2;
outbyte[i + 3] = (inbytea[j + 3] + inbytea[j + 7]) / 2;
}
return max;
}
private int
rescale_byte_wise1x2(int bytecount, const byte * inbytea, const byte * inbyteb,
byte * outbyte)
{
register int i;
for (i = 0; i < bytecount; i += 4) {
outbyte[i + 1] = (inbytea[i + 1] + inbyteb[i + 1]) / 2;
outbyte[i + 2] = (inbytea[i + 2] + inbyteb[i + 2]) / 2;
outbyte[i + 3] = (inbytea[i + 3] + inbyteb[i + 3]) / 2;
}
return bytecount;
}
private int
rescale_byte_wise1x1(int bytecount, const byte * inbytea, const byte * inbyteb,
byte * outbyte)
{
register int i;
for (i = 0; i < bytecount; i += 4) {
outbyte[i + 1] = inbytea[i + 1];
outbyte[i + 2] = inbytea[i + 2];
outbyte[i + 3] = inbytea[i + 3];
}
return bytecount;
}
#define RSHIFT ((I * 8) - 16)
#define SHIFT ((I * 8) - 13)
#define MAXVALUE  (255 << SHIFT)
#define RANDOM (((rand() << RSHIFT) % (MAXVALUE / 2))  - MAXVALUE /4);
#define MINVALUE  0
#define C 8
#define THRESHOLD (128 << SHIFT)
#define SHIFTS ((I * 8) - 14)
#define SHIFTM ((I * 8) - 13)
#define SHIFTL ((I * 8) - 12)
#define MAXVALUES  (160 << SHIFTM)
#define MAXVALUEM  (226 << SHIFTM)
#define MAXVALUEL  (255 << SHIFTM)
#define THRESHOLDS (128 << SHIFTM)
#define THRESHOLDM (192 << SHIFTM)
#define THRESHOLDL (226 << SHIFTM)
private void
init_error_buffer(struct misc_struct *misc_vars,
struct ptr_arrays *data_ptrs)
{
int i;
int *ep;
int *epc;
ep = data_ptrs->errors[0];
epc = data_ptrs->errors_c[0];
if (misc_vars->bits_per_pixel > 4) {
for (i = 0; i < misc_vars->databuff_size; i++) {
*ep++ = RANDOM;
}
for (i = 0; i < misc_vars->databuff_size_c; i++) {
*epc++ = RANDOM;
}
}
return;
}
#define FSdither(inP, out, errP, Err, Bit, Offset, Element)\
{\
oldErr = Err;\
Err = (*(errP + Element)\
+ ((Err * 7 + C) >> 4)\
+ ((int)*(inP + Element) << SHIFT));\
if (Err > THRESHOLD) {\
out |= Bit;\
Err -= MAXVALUE;\
}\
*(errP + (Element + Offset)) += ((Err * 3 + C) >> 4);\
*(errP + Element) = ((Err * 5 + oldErr + C) >> 4);\
}
private void
FSDlinebw(int scan, int plane_size,
struct error_val_field *error_values,
byte * kP, int n, int *ep, byte * dp)
{
if (scan == 0) {
byte k, bitmask;
int oldErr, i;
for (i = 0; i < plane_size; i++) {
bitmask = 0x80;
for (k = 0; bitmask != 0; bitmask >>= 1) {
FSdither(dp, k, ep, error_values->k, bitmask, -n, 0);
dp += n, ep += n;
}
*kP++ = k;
}
} else {
byte k, bitmask;
int oldErr, i;
for (i = 0; i < plane_size; i++) {
bitmask = 0x01;
for (k = 0; bitmask != 0; bitmask <<= 1) {
dp -= n, ep -= n;
FSdither(dp, k, ep, error_values->k, bitmask, n, 0);
}
*--kP = k;
}
}
return;
}
private void
FSDlinec2(int scan, int plane_size,
struct error_val_field *error_values,
byte * cPa, byte * mPa, byte * yPa, int n,
byte * dp, int *ep)
{
if (scan == 0) {
int oldErr, i;
byte ca, ya, ma, bitmask;
for (i = 0; i < plane_size; i++) {
bitmask = 0x80;
ca = ya = ma = 0;
for (ca = 0; bitmask != 0; bitmask >>= 1) {
FSdither(dp, ca, ep, error_values->c, bitmask, -n, n - 3);
FSdither(dp, ma, ep, error_values->m, bitmask, -n, n - 2);
FSdither(dp, ya, ep, error_values->y, bitmask, -n, n - 1);
dp += n, ep += n;
}
*cPa++ = ca;
*mPa++ = ma;
*yPa++ = ya;
}
} else {
byte ca, ya, ma, bitmask;
int oldErr, i;
for (i = 0; i < plane_size; i++) {
bitmask = 0x01;
ca = ya = ma = 0;
for (ca = 0; bitmask != 0; bitmask <<= 1) {
dp -= n, ep -= n;
FSdither(dp, ya, ep, error_values->y, bitmask, n, n - 1);
FSdither(dp, ma, ep, error_values->m, bitmask, n, n - 2);
FSdither(dp, ca, ep, error_values->c, bitmask, n, n - 3);
}
*--yPa = ya;
*--mPa = ma;
*--cPa = ca;
}
}
return;
}
#define FSdither8503(inP, outa, outb, errP, Err, Bit, Offset, Element)\
{\
oldErr = Err;\
Err = (*(errP + Element)\
+ ((Err * 7 + C) >> 4)\
+ ((int) *(inP + Element) << SHIFT));\
if ((Err > THRESHOLDS) && (Err <= THRESHOLDM)) {\
outa |= Bit;\
Err -= MAXVALUES;\
}\
if (Err > THRESHOLDM) {\
outb |= Bit;\
Err -= MAXVALUEM;\
}\
*(errP + (Element + Offset)) += ((Err * 3 + C) >> 4);\
*(errP + Element) = ((Err * 5 + oldErr + C) >> 4);\
}
private void
FSDlinec3(int scan, int plane_size,
struct error_val_field *error_values,
byte * cPa, byte * mPa, byte * yPa,
byte * cPb, byte * mPb, byte * yPb,
int n, byte * dp, int *ep)
{
if (scan == 0) {
byte ca, ya, ma, cb, yb, mb, bitmask;
int oldErr, i;
for (i = 0; i < plane_size; i++) {
bitmask = 0x80;
ca = ya = ma = cb = yb = mb = 0;
for (ca = 0; bitmask != 0; bitmask >>= 1) {
FSdither8503(dp, ca, cb, ep, error_values->c, bitmask, -n, n
- 3);
FSdither8503(dp, ma, mb, ep, error_values->m, bitmask, -n, n
- 2);
FSdither8503(dp, ya, yb, ep, error_values->y, bitmask, -n, n
- 1);
dp += n, ep += n;
}
*cPa++ = ca;
*mPa++ = ma;
*yPa++ = ya;
*cPb++ = cb;
*mPb++ = mb;
*yPb++ = yb;
}
} else {
byte ca, ya, ma, cb, yb, mb, bitmask;
int oldErr, i;
for (i = 0; i < plane_size; i++) {
bitmask = 0x01;
ca = ya = ma = cb = yb = mb = 0;
for (ca = 0; bitmask != 0; bitmask <<= 1) {
dp -= n, ep -= n;
FSdither8503(dp, ya, yb, ep, error_values->y, bitmask, n, n
- 1);
FSdither8503(dp, ma, mb, ep, error_values->m, bitmask, n, n
- 2);
FSdither8503(dp, ca, cb, ep, error_values->c, bitmask, n, n
- 3);
}
*--yPa = ya;
*--mPa = ma;
*--cPa = ca;
*--yPb = yb;
*--mPb = mb;
*--cPb = cb;
}
}
return;
}
#define FSdither8504(inP, outa, outb, errP, Err, Bit, Offset, Element)\
{\
oldErr = Err;\
Err = (*(errP + Element)\
+ ((Err * 7 + C) >> 4)\
+ ((int) *(inP + Element) << SHIFT));\
if ((Err > THRESHOLDS) && (Err <= THRESHOLDM)) {\
outa |= Bit;\
Err -= MAXVALUES;\
}\
if ((Err > THRESHOLDM) && (Err <= THRESHOLDL)) {\
outb |= Bit;\
Err -= MAXVALUEM;\
}\
if (Err > THRESHOLDL) {\
outa |= Bit;\
outb |= Bit;\
Err -= MAXVALUEL;\
}\
*(errP + (Element + Offset)) += ((Err * 3 + C) >> 4);\
*(errP + Element) = ((Err * 5 + oldErr + C) >> 4);\
}
private void
FSDlinec4(int scan, int plane_size,
struct error_val_field *error_values,
byte * cPa, byte * mPa, byte * yPa,
byte * cPb, byte * mPb, byte * yPb,
int n, byte * dp, int *ep)
{
if (scan == 0) {
byte ca, ya, ma, cb, yb, mb, bitmask;
int oldErr, i;
for (i = 0; i < plane_size; i++) {
bitmask = 0x80;
ca = ya = ma = cb = yb = mb = 0;
for (ca = 0; bitmask != 0; bitmask >>= 1) {
FSdither8504(dp, ca, cb, ep, error_values->c, bitmask, -n, n
- 3);
FSdither8504(dp, ma, mb, ep, error_values->m, bitmask, -n, n
- 2);
FSdither8504(dp, ya, yb, ep, error_values->y, bitmask, -n, n
- 1);
dp += n, ep += n;
}
*cPa++ = ca;
*mPa++ = ma;
*yPa++ = ya;
*cPb++ = cb;
*mPb++ = mb;
*yPb++ = yb;
}
} else {
byte ca, ya, ma, cb, yb, mb, bitmask;
int oldErr, i;
for (i = 0; i < plane_size; i++) {
bitmask = 0x01;
ca = ya = ma = cb = yb = mb = 0;
for (ca = 0; bitmask != 0; bitmask <<= 1) {
dp -= n, ep -= n;
FSdither8504(dp, ya, yb, ep, error_values->y, bitmask, n, n
- 1);
FSdither8504(dp, ma, mb, ep, error_values->m, bitmask, n, n
- 2);
FSdither8504(dp, ca, cb, ep, error_values->c, bitmask, n, n
- 3);
}
*--yPa = ya;
*--mPa = ma;
*--cPa = ca;
*--yPb = yb;
*--mPb = mb;
*--cPb = cb;
}
}
return;
}
private void
calculate_memory_size(gx_device_printer * pdev,
struct misc_struct *misc_vars)
{
int xfac = cdj850->xscal ? 2 : 1;
misc_vars->line_size = gdev_prn_raster(pdev);
misc_vars->line_size_c = misc_vars->line_size / xfac;
misc_vars->line_size_words = (misc_vars->line_size + W - 1) / W;
misc_vars->paper_size = gdev_pcl_paper_size((gx_device *) pdev);
misc_vars->num_comps = pdev->color_info.num_components;
misc_vars->bits_per_pixel = pdev->color_info.depth;
misc_vars->storage_bpp = misc_vars->num_comps * 8;
misc_vars->expanded_bpp = misc_vars->num_comps * 8;
misc_vars->errbuff_size = 0;
misc_vars->errbuff_size_c = 0;
misc_vars->plane_size = calc_buffsize(misc_vars->line_size, misc_vars->storage_bpp);
misc_vars->plane_size_c = 2 * misc_vars->plane_size / xfac;
misc_vars->errbuff_size =
calc_buffsize((misc_vars->plane_size * misc_vars->expanded_bpp +
misc_vars->num_comps * 4) * I, 1);
misc_vars->errbuff_size_c =
calc_buffsize((misc_vars->plane_size_c / 2 * misc_vars->expanded_bpp
+ misc_vars->num_comps * 4) * I, 1);
misc_vars->databuff_size =
misc_vars->plane_size * misc_vars->storage_bpp;
misc_vars->databuff_size_c =
misc_vars->plane_size_c / 2 * misc_vars->storage_bpp;
misc_vars->outbuff_size = misc_vars->plane_size * 4;
misc_vars->storage_size_words = (((misc_vars->plane_size)
* 2
* misc_vars->num_comps)
+ misc_vars->databuff_size
+ misc_vars->errbuff_size
+ misc_vars->outbuff_size
+ ((misc_vars->plane_size_c)
* 2
* misc_vars->num_comps)
+ misc_vars->databuff_size_c
+ misc_vars->errbuff_size_c
+ (4 * misc_vars->plane_size_c))
/ W;
return;
}
private void
init_data_structure(gx_device_printer * pdev,
struct ptr_arrays *data_ptrs,
struct misc_struct *misc_vars)
{
int i;
byte *p = (byte *) data_ptrs->storage;
misc_vars->scan = 0;
misc_vars->cscan = 0;
misc_vars->is_two_pass = 1;
data_ptrs->data[0] = data_ptrs->data[1] = data_ptrs->data[2] = p;
data_ptrs->data[3] = p + misc_vars->databuff_size;
if (misc_vars->bits_per_pixel > 1) {
p += misc_vars->databuff_size;
}
if (misc_vars->bits_per_pixel > 4) {
data_ptrs->errors[0] = (int *)p + misc_vars->num_comps * 2;
data_ptrs->errors[1] = data_ptrs->errors[0] + misc_vars->databuff_size;
p += misc_vars->errbuff_size;
}
for (i = 0; i < misc_vars->num_comps; i++) {
data_ptrs->plane_data[0][i] = data_ptrs->plane_data[2][i] = p;
p += misc_vars->plane_size;
}
for (i = 0; i < misc_vars->num_comps; i++) {
data_ptrs->plane_data[1][i] = p;
data_ptrs->plane_data[3][i] = p + misc_vars->plane_size;
p += misc_vars->plane_size;
}
data_ptrs->out_data = p;
p += misc_vars->outbuff_size;
data_ptrs->data_c[0] = data_ptrs->data_c[1] = data_ptrs->data_c[2] = p;
data_ptrs->data_c[3] = p + misc_vars->databuff_size_c;
if (misc_vars->bits_per_pixel > 1) {
p += misc_vars->databuff_size_c;
}
if (misc_vars->bits_per_pixel > 4) {
data_ptrs->errors_c[0] = (int *)p + misc_vars->num_comps * 2;
data_ptrs->errors_c[1] = data_ptrs->errors_c[0] + misc_vars->databuff_size_c;
p += misc_vars->errbuff_size_c;
}
for (i = 0; i < misc_vars->num_comps; i++) {
data_ptrs->plane_data_c[0][i] = data_ptrs->plane_data_c[2][i] = p;
p += misc_vars->plane_size_c / 2;
}
for (i = 0; i < misc_vars->num_comps; i++) {
data_ptrs->plane_data_c[1][i] = p;
data_ptrs->plane_data_c[3][i] = p + misc_vars->plane_size_c / 2;
p += misc_vars->plane_size_c / 2;
}
for (i = 0; i < misc_vars->num_comps; i++) {
data_ptrs->plane_data_c[0][i + 4] = data_ptrs->plane_data_c[2][i +
4] = p;
p += misc_vars->plane_size_c / 2;
}
for (i = 0; i < misc_vars->num_comps; i++) {
data_ptrs->plane_data_c[1][i + 4] = p;
data_ptrs->plane_data_c[3][i + 4] = p + misc_vars->plane_size_c / 2;
p += misc_vars->plane_size_c / 2;
}
for (i = 0; i < misc_vars->num_comps; i++) {
data_ptrs->test_data[i] = p;
p += misc_vars->plane_size_c / 2;
}
memset(data_ptrs->storage, 0, misc_vars->storage_size_words * W);
return;
}
private void
cdj850_start_raster_mode(gx_device_printer * pdev, int paper_size,
FILE * prn_stream)
{
int xres, yres;
hp850_cmyk_init_t init;
init = hp850_cmyk_init;
init.a[13] = cdj850->intensities;
init.a[19] = cdj850->intensities;
init.a[25] = cdj850->intensities;
assign_dpi(cdj850->x_pixels_per_inch, init.a + 2);
assign_dpi(cdj850->y_pixels_per_inch, init.a + 4);
xres = cdj850->x_pixels_per_inch / (cdj850->xscal + 1);
yres = cdj850->y_pixels_per_inch / (cdj850->yscal + 1);
assign_dpi(xres, init.a + 8);
assign_dpi(yres, init.a + 10);
assign_dpi(xres, init.a + 14);
assign_dpi(yres, init.a + 16);
assign_dpi(xres, init.a + 20);
assign_dpi(yres, init.a + 22);
fputs("\033*rbC", prn_stream);
fputs("\033E", prn_stream);
fprintf(prn_stream, "\033&l%daolE", paper_size);
fprintf(prn_stream, "\033*o%dM", cdj850->quality);
fprintf(prn_stream, "\033&l%dM", cdj850->papertype);
fprintf(prn_stream, "\033*p%dY", (int)(600 * DOFFSET));
fprintf(prn_stream, "\033*g%dW", (int)sizeof(init.a));
fwrite(init.a, sizeof(byte), sizeof(init.a),
prn_stream);
fputs("\033*b", prn_stream);
if (cdj850->compression)
fprintf(prn_stream, "%dm", cdj850->compression);
return;
}
private int
cdj_put_param_int(gs_param_list * plist, gs_param_name pname, int *pvalue,
int minval, int maxval, int ecode)
{
int code, value;
switch (code = param_read_int(plist, pname, &value)) {
default:
return code;
case 1:
return ecode;
case 0:
if (value < minval || value > maxval)
param_signal_error(plist, pname, gs_error_rangecheck);
*pvalue = value;
return (ecode < 0 ? ecode : 1);
}
}
private int
cdj_put_param_float(gs_param_list * plist, gs_param_name pname, float *pvalue,
float minval, float maxval, int ecode)
{
int code;
float value;
switch (code = param_read_float(plist, pname, &value)) {
default:
return code;
case 1:
return ecode;
case 0:
if (value < minval || value > maxval)
param_signal_error(plist, pname, gs_error_rangecheck);
*pvalue = value;
return (ecode < 0 ? ecode : 1);
}
}
private int
cdj_set_bpp(gx_device * pdev, int bpp, int ccomps)
{
gx_device_color_info *ci = &pdev->color_info;
if (ccomps && bpp == 0) {
if (cprn_device->cmyk) {
switch (ccomps) {
default:
return gs_error_rangecheck;
break;
case 1:
bpp = 1;
break;
case 3:
bpp = 24;
break;
case 4:
switch (ci->depth) {
case 8:
case 16:
case 24:
case 32:
break;
default:
bpp = cprn_device->default_depth;
break;
}
break;
}
}
}
if (bpp == 0) {
bpp = ci->depth;
}
if (cprn_device->cmyk < 0) {
dev_proc(pdev, map_cmyk_color) = gdev_cmyk_map_cmyk_color;
dev_proc(pdev, map_rgb_color) = NULL;
dev_proc(pdev, map_color_rgb) = gdev_cmyk_map_color_rgb;
if (pdev->is_open)
gs_closedevice(pdev);
}
switch (bpp) {
case 16:
case 32:
if (cprn_device->cmyk && ccomps && ccomps != 4)
goto bppe;
break;
case 24:
if (!cprn_device->cmyk || ccomps == 0 || ccomps == 4) {
break;
} else if (ccomps == 1) {
goto bppe;
} else {
cprn_device->cmyk = -1;
}
break;
case 8:
if (cprn_device->cmyk) {
if (ccomps) {
if (ccomps == 3) {
cprn_device->cmyk = -1;
bpp = 3;
} else if (ccomps != 1 && ccomps != 4) {
goto bppe;
}
}
if (ccomps != 1)
break;
} else {
break;
}
case 1:
if (ccomps != 1)
goto bppe;
if (cprn_device->cmyk && bpp != pdev->color_info.depth) {
dev_proc(pdev, map_cmyk_color) = NULL;
dev_proc(pdev, map_rgb_color) = gdev_cmyk_map_rgb_color;
if (pdev->is_open) {
gs_closedevice(pdev);
}
}
break;
case 3:
if (!cprn_device->cmyk) {
break;
}
default:
bppe:return gs_error_rangecheck;
}
if (cprn_device->cmyk == -1) {
dev_proc(pdev, map_cmyk_color) = NULL;
dev_proc(pdev, map_rgb_color) = gdev_pcl_map_rgb_color;
dev_proc(pdev, map_color_rgb) = gdev_pcl_map_color_rgb;
if (pdev->is_open) {
gs_closedevice(pdev);
}
}
switch (ccomps) {
case 0:
break;
case 1:
if (bpp != 1 && bpp != 8)
goto cce;
break;
case 4:
if (cprn_device->cmyk) {
if (bpp >= 8)
break;
}
case 3:
if (bpp == 1 || bpp == 3 || bpp == 8 || bpp == 16
|| bpp == 24 || bpp == 32) {
break;
}
cce: default:
return gs_error_rangecheck;
}
if (cprn_device->cmyk) {
if (cprn_device->cmyk > 0) {
ci->num_components = ccomps ? ccomps : (bpp < 8 ? 1 : 4);
} else {
ci->num_components = ccomps ? ccomps : (bpp < 8 ? 1 : 3);
}
if (bpp != 1 && ci->num_components == 1) {
bpp = bpp < 8 ? 8 : bpp;
}
ci->max_color = (1 << (bpp >> 2)) - 1;
ci->max_gray = (bpp >= 8 ? 255 : 1);
if (ci->num_components == 1) {
ci->dither_grays = (bpp >= 8 ? 5 : 2);
ci->dither_colors = (bpp >= 8 ? 5 : bpp > 1 ? 2 : 0);
} else {
ci->dither_grays = (bpp > 8 ? 5 : 2);
ci->dither_colors = (bpp > 8 ? 5 : bpp > 1 ? 2 : 0);
}
} else {
ci->num_components = (bpp == 1 || bpp == 8 ? 1 : 3);
ci->max_color = (bpp >= 8 ? 255 : bpp > 1 ? 1 : 0);
ci->max_gray = (bpp >= 8 ? 255 : 1);
ci->dither_grays = (bpp >= 8 ? 5 : 2);
ci->dither_colors = (bpp >= 8 ? 5 : bpp > 1 ? 2 : 0);
}
ci->depth = ((bpp > 1) && (bpp < 8) ? 8 : bpp);
return 0;
}
#define gx_color_value_to_bits(cv, b) \
((cv) >> (gx_color_value_bits - (b)))
#define gx_bits_to_color_value(cv, b) \
((cv) << (gx_color_value_bits - (b)))
#define gx_cmyk_value_bits(c, m, y, k, b) \
((gx_color_value_to_bits((k), (b)) << (3 * (b))) | \
(gx_color_value_to_bits((c), (b)) << (2 * (b))) | \
(gx_color_value_to_bits((m), (b)) << (b)) | \
(gx_color_value_to_bits((y), (b))))
#define gx_value_cmyk_bits(v, c, m, y, k, b) \
(k) = gx_bits_to_color_value(((v) >> (3 * (b))) & ((1 << (b)) - 1), (b)), \
(c) = gx_bits_to_color_value(((v) >> (2 * (b))) & ((1 << (b)) - 1), (b)), \
(m) = gx_bits_to_color_value(((v) >> (b)) & ((1 << (b)) - 1), (b)), \
(y) = gx_bits_to_color_value((v) & ((1 << (b)) - 1), (b))
private gx_color_index
gdev_cmyk_map_cmyk_color(gx_device * pdev,
gx_color_value *cmyk)
{
gx_color_value cyan=cmyk[0], magenta=cmyk[1], yellow=cmyk[3], black=cmyk[4];
gx_color_index color;
switch (pdev->color_info.depth) {
case 1:
color = (cyan | magenta | yellow | black) > gx_max_color_value / 2 ?
(gx_color_index) 1 : (gx_color_index) 0;
break;
default:{
int nbits = pdev->color_info.depth;
if (cyan == magenta && magenta == yellow) {
float bpart = ((float)cyan) * (lum_red_weight / 100.) +
((float)magenta) * (lum_green_weight / 100.) +
((float)yellow) * (lum_blue_weight / 100.) +
(float)black;
cyan = magenta = yellow = (gx_color_index) 0;
black = (gx_color_index) (bpart > gx_max_color_value ?
gx_max_color_value : bpart);
}
color = gx_cmyk_value_bits(cyan, magenta, yellow, black,
nbits >> 2);
}
}
return color;
}
private gx_color_index
gdev_cmyk_map_rgb_color(gx_device * pdev, gx_color_value rgb[3])
{
gx_color_value r=rgb[0], g=rgb[1], b=rgb[2];
if (gx_color_value_to_byte(r & g & b) == 0xff) {
return (gx_color_index) 0;
} else {
gx_color_value c = gx_max_color_value - r;
gx_color_value m = gx_max_color_value - g;
gx_color_value y = gx_max_color_value - b;
switch (pdev->color_info.depth) {
case 1:
return (c | m | y) > gx_max_color_value / 2 ?
(gx_color_index) 1 : (gx_color_index) 0;
break;
case 8:
return ((ulong) c * lum_red_weight * 10
+ (ulong) m * lum_green_weight * 10
+ (ulong) y * lum_blue_weight * 10)
>> (gx_color_value_bits + 2);
break;
}
}
return (gx_color_index) 0;
}
private int
gdev_cmyk_map_color_rgb(gx_device * pdev, gx_color_index color,
gx_color_value prgb[3])
{
switch (pdev->color_info.depth) {
case 1:
prgb[0] = prgb[1] = prgb[2] = gx_max_color_value * (1 - color);
break;
case 8:
if (pdev->color_info.num_components == 1) {
gx_color_value value = (gx_color_value) color ^ 0xff;
prgb[0] = prgb[1] = prgb[2] = (value << 8) + value;
break;
}
default:{
unsigned long bcyan, bmagenta, byellow, black;
int nbits = pdev->color_info.depth;
gx_value_cmyk_bits(color, bcyan, bmagenta, byellow, black,
nbits >> 2);
#ifdef USE_ADOBE_CMYK_RGB
bcyan += black, bmagenta += black, byellow += black;
prgb[0] = (bcyan > gx_max_color_value ? (gx_color_value) 0 :
gx_max_color_value - bcyan);
prgb[1] = (bmagenta > gx_max_color_value ? (gx_color_value) 0 :
gx_max_color_value - bmagenta);
prgb[2] = (byellow > gx_max_color_value ? (gx_color_value) 0 :
gx_max_color_value - byellow);
#else
prgb[0] = (gx_color_value)
((ulong) (gx_max_color_value - bcyan) *
(gx_max_color_value - black) / gx_max_color_value);
prgb[1] = (gx_color_value)
((ulong) (gx_max_color_value - bmagenta) *
(gx_max_color_value - black) / gx_max_color_value);
prgb[2] = (gx_color_value)
((ulong) (gx_max_color_value - byellow) *
(gx_max_color_value - black) / gx_max_color_value);
#endif
}
}
return 0;
}
private gx_color_index
gdev_pcl_map_rgb_color(gx_device * pdev, gx_color_value *rgb)
{
gx_color_value r=rgb[0], g=rgb[1], b=rgb[2];
if (gx_color_value_to_byte(r & g & b) == 0xff)
return (gx_color_index) 0;
else {
gx_color_value c = gx_max_color_value - r;
gx_color_value m = gx_max_color_value - g;
gx_color_value y = gx_max_color_value - b;
switch (pdev->color_info.depth) {
case 1:
return ((c | m | y) > gx_max_color_value / 2 ?
(gx_color_index) 1 : (gx_color_index) 0);
case 8:
if (pdev->color_info.num_components >= 3)
#define gx_color_value_to_1bit(cv) ((cv) >> (gx_color_value_bits - 1))
return (gx_color_value_to_1bit(c) +
(gx_color_value_to_1bit(m) << 1) +
(gx_color_value_to_1bit(y) << 2));
else
#define red_weight 306
#define green_weight 601
#define blue_weight 117
return ((((ulong) c * red_weight +
(ulong) m * green_weight +
(ulong) y * blue_weight)
>> (gx_color_value_bits + 2)));
case 16:
#define gx_color_value_to_5bits(cv) ((cv) >> (gx_color_value_bits - 5))
#define gx_color_value_to_6bits(cv) ((cv) >> (gx_color_value_bits - 6))
return (gx_color_value_to_5bits(y) +
(gx_color_value_to_6bits(m) << 5) +
(gx_color_value_to_5bits(c) << 11));
case 24:
return (gx_color_value_to_byte(y) +
(gx_color_value_to_byte(m) << 8) +
((ulong) gx_color_value_to_byte(c) << 16));
case 32:
{
return ((c == m && c == y) ? ((ulong)
gx_color_value_to_byte(c) << 24)
: (gx_color_value_to_byte(y) +
(gx_color_value_to_byte(m) << 8) +
((ulong) gx_color_value_to_byte(c) << 16)));
}
}
}
return (gx_color_index) 0;
}
private int
gdev_pcl_map_color_rgb(gx_device * pdev, gx_color_index color,
gx_color_value prgb[3])
{
switch (pdev->color_info.depth) {
case 1:
prgb[0] = prgb[1] = prgb[2] = -((gx_color_value) color ^ 1);
break;
case 8:
if (pdev->color_info.num_components >= 3) {
gx_color_value c = (gx_color_value) color ^ 7;
prgb[0] = -(c & 1);
prgb[1] = -((c >> 1) & 1);
prgb[2] = -(c >> 2);
} else {
gx_color_value value = (gx_color_value) color ^ 0xff;
prgb[0] = prgb[1] = prgb[2] = (value << 8) + value;
}
break;
case 16:
{
gx_color_value c = (gx_color_value) color ^ 0xffff;
ushort value = c >> 11;
prgb[0] = ((value << 11) + (value << 6) + (value << 1) +
(value >> 4)) >> (16 - gx_color_value_bits);
value = (c >> 6) & 0x3f;
prgb[1] = ((value << 10) + (value << 4) + (value >> 2))
>> (16 - gx_color_value_bits);
value = c & 0x1f;
prgb[2] = ((value << 11) + (value << 6) + (value << 1) +
(value >> 4)) >> (16 - gx_color_value_bits);
}
break;
case 24:
{
gx_color_value c = (gx_color_value) color ^ 0xffffff;
prgb[0] = gx_color_value_from_byte(c >> 16);
prgb[1] = gx_color_value_from_byte((c >> 8) & 0xff);
prgb[2] = gx_color_value_from_byte(c & 0xff);
}
break;
case 32:
#define  gx_maxcol gx_color_value_from_byte(gx_color_value_to_byte(gx_max_color_value))
{
gx_color_value w = gx_maxcol - gx_color_value_from_byte(color >> 24);
prgb[0] = w - gx_color_value_from_byte((color >> 16) & 0xff);
prgb[1] = w - gx_color_value_from_byte((color >> 8) & 0xff);
prgb[2] = w - gx_color_value_from_byte(color & 0xff);
}
break;
}
return 0;
}
private int
cdj_put_param_bpp(gx_device * pdev, gs_param_list * plist, int new_bpp,
int real_bpp, int ccomps)
{
if (new_bpp == 0 && ccomps == 0)
return gdev_prn_put_params(pdev, plist);
else {
gx_device_color_info save_info;
int save_bpp;
int code;
save_info = pdev->color_info;
save_bpp = save_info.depth;
#define save_ccomps save_info.num_components
if (save_bpp == 8 && save_ccomps == 3 && !cprn_device->cmyk)
save_bpp = 3;
code = cdj_set_bpp(pdev, real_bpp, ccomps);
if (code < 0) {
param_signal_error(plist, "BitsPerPixel", code);
param_signal_error(plist, "ProcessColorModel", code);
return code;
}
pdev->color_info.depth = new_bpp;
code = gdev_prn_put_params(pdev, plist);
if (code < 0) {
cdj_set_bpp(pdev, save_bpp, save_ccomps);
return code;
}
cdj_set_bpp(pdev, real_bpp, ccomps);
if ((cdj850->color_info.depth != save_bpp ||
(ccomps != 0 && ccomps != save_ccomps))
&& pdev->is_open)
return gs_closedevice(pdev);
return 0;
#undef save_ccomps
}
}
private void
cdj1600_start_raster_mode(gx_device_printer * pdev, int paper_size,
FILE * prn_stream)
{
uint raster_width = pdev->width -
pdev->x_pixels_per_inch * (dev_l_margin(pdev) + dev_r_margin(pdev));
fputs("\033%-12345X@PJL enter language = PCL\n", prn_stream);
fputs("\033*rbC", prn_stream);
fputs("\033E", prn_stream);
fprintf(prn_stream, "\033*t%dR", (int)cdj850->x_pixels_per_inch);
fprintf(prn_stream, "\033&l%daolE", paper_size);
fputs("\033&a1N", prn_stream);
fprintf(prn_stream, "\033*o%dQ", cdj850->quality);
fprintf(prn_stream, "\033&l%dM", cdj850->papertype);
fprintf(prn_stream, "\033*p%dY", (int)(300.0 * DOFFSET));
fprintf(prn_stream, "\033*r%ds-%du0A",
raster_width, pdev->color_info.num_components);
fputs("\033*r1A", prn_stream);
fputs("\033*b", prn_stream);
if (cdj850->compression)
fprintf(prn_stream, "%dm", cdj850->compression);
return;
}
private void
print_c3plane(FILE * prn_stream, char plane_code, int plane_size,
const byte * curr, byte * prev, byte * out_data)
{
int out_count = gdev_pcl_mode3compress(plane_size, curr, prev, out_data);
if (out_count > 0) {
fprintf(prn_stream, "%d%c", out_count, plane_code);
fwrite(out_data, sizeof(byte), out_count, prn_stream);
} else {
putc(plane_code, prn_stream);
}
}
private int
copy_color_data(byte * dest, const byte * src, int n)
{
register int i = n / 4;
register word *d = (word *) dest;
register const word *s = (const word *)src;
while (i-- > 0) {
*d++ = *s++;
}
return n;
}
private void
cdj1600_print_non_blank_lines(gx_device_printer * pdev,
struct ptr_arrays *data_ptrs,
struct misc_struct *misc_vars,
struct error_val_field *error_values,
const Gamma *gamma,
FILE * prn_stream)
{
int i, plane_size_c;
plane_size_c = copy_color_data
(data_ptrs->data_c[misc_vars->cscan],
data_ptrs->data[misc_vars->scan],
misc_vars->databuff_size) / misc_vars->storage_bpp;
do_floyd_steinberg(misc_vars->scan, misc_vars->cscan,
misc_vars->plane_size, plane_size_c,
misc_vars->num_comps, data_ptrs, pdev, error_values);
for (i = misc_vars->num_comps - 1; i >= 0; i--) {
print_c3plane(prn_stream, "wvv"[i], plane_size_c,
data_ptrs->plane_data_c[misc_vars->cscan][i],
data_ptrs->plane_data_c[1 - misc_vars->cscan][i],
data_ptrs->out_data);
}
misc_vars->cscan = 1 - misc_vars->cscan;
}
private void
cdj1600_terminate_page(gx_device_printer * pdev, FILE * prn_stream)
{
cdj850_terminate_page(pdev, prn_stream);
fputs("\033%-12345X", prn_stream);
}