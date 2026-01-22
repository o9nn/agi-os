#ifndef   gdevstc_INCLUDED
#  define gdevstc_INCLUDED
#include "gdevprn.h"
#include "gsparam.h"
#include "gsstate.h"
#if     arch_log2_sizeof_int < 2
typedef unsigned long stc_pixel;
#else
typedef unsigned int  stc_pixel;
#endif
typedef struct stc_s {
long            flags;
int             bits;
const struct stc_dither_s  *dither;
float          *am;
float          *extc[4];
uint            sizc[4];
gx_color_value *code[4];
float          *extv[4];
uint            sizv[4];
byte           *vals[4];
stc_pixel  white_run[3];
stc_pixel  white_end[3];
gs_param_string_array
algorithms;
gs_param_string escp_init;
gs_param_string escp_release;
int             escp_width;
int             escp_height;
int             escp_top;
int             escp_bottom;
int             alg_item;
int             prt_buf;
int             prt_size;
int             escp_size;
int             seed_size;
int             escp_u;
int             escp_c;
int             escp_v;
int             escp_h;
int             escp_m;
int             escp_lf;
int             prt_y;
int             stc_y;
int             buf_y;
int             prt_scans;
int            *prt_width;
byte          **prt_data;
byte           *escp_data;
byte           *seed_row[4];
} stc_t;
typedef struct stcolor_device_s {
gx_device_common;
gx_prn_device_common;
stc_t stc;
} stcolor_device;
#define STCDFLAG0  0x000001L
#define STCDFLAG1  0x000002L
#define STCDFLAG2  0x000004L
#define STCDFLAG3  0x000008L
#define STCDFLAG4  0x000010L
#define STCCMYK10  0x000020L
#define STCUNIDIR  0x000040L
#define STCUWEAVE  0x000080L
#define STCNWEAVE  0x000100L
#define STCOK4GO   0x000200L
#define STCCOMP    0x000C00L
#define STCPLAIN   0x000400L
#define STCDELTA   0x000800L
#define STCMODEL   0x00f000L
#define STCST800   0x001000L
#define STCSTCII   0x002000L
#define STCBAND    0x010000L
#define STCHEIGHT  0x020000L
#define STCWIDTH   0x040000L
#define STCTOP     0x080000L
#define STCBOTTOM  0x100000L
#define STCINIT    0x200000L
#define STCRELEASE 0x400000L
#define STCPRINT   0x800000L
#define stc_proc_dither(name) \
int name(stcolor_device *sdev,int npixel,byte *in,byte *buf,byte *out)
typedef struct stc_dither_s {
const char *name;
stc_proc_dither((*fun));
uint        flags;
uint        bufadd;
double      minmax[2];
} stc_dither_t;
#define BLACK   1
#define RED     4
#define GREEN   2
#define BLUE    1
#define CYAN    8
#define MAGENTA 4
#define YELLOW  2
#define STC_TYPESWITCH(Dither,Action)             \
switch((Dither)->flags & STC_TYPE)  {          \
case STC_BYTE: Action(byte); break;            \
case STC_LONG: Action(long); break;            \
default:       Action(float); break;}
stc_proc_dither(stc_gsmono);
stc_proc_dither(stc_fs);
stc_proc_dither(stc_fscmyk);
stc_proc_dither(stc_gsrgb);
stc_proc_dither(stc_fs2);
#define DeviceGray  1
#define DeviceRGB   3
#define DeviceCMYK  4
#define STC_BYTE    8
#define STC_LONG   16
#define STC_FLOAT  24
#define STC_TYPE   24
#define STC_CMYK10 32
#define STC_DIRECT 64
#define STC_WHITE 128
#define STC_SCAN  256
#define STC_MODI \
{"gsmono", stc_gsmono, DeviceGray|STC_BYTE,0,{0.0,1.0}},\
{"gsrgb" , stc_gsrgb , DeviceRGB |STC_BYTE,0,{0.0,1.0}},\
{"fsmono", stc_fs, \
DeviceGray|STC_LONG|1*STC_SCAN,3+3*1,{0.0,16777215.0}},\
{"fsrgb",  stc_fs, \
DeviceRGB |STC_LONG|1*STC_SCAN,3+3*3,{0.0,16777215.0}},\
{"fsx4",   stc_fs, \
DeviceCMYK|STC_LONG|1*STC_SCAN,3+3*4,{0.0,16777215.0}},\
{"fscmyk", stc_fscmyk, \
DeviceCMYK|STC_LONG|1*STC_SCAN,3+3*4,{0.0,16777215.0}},\
{"fs2", stc_fs2, \
DeviceRGB |STC_BYTE|STC_WHITE|1*STC_SCAN,0,{0.0,255.0}},
#ifndef   X_DPI
#define   X_DPI   360
#endif
#ifndef   Y_DPI
#define   Y_DPI   360
#endif
#ifndef   STC_L_MARGIN
#  define STC_L_MARGIN 0.125
#endif
#ifndef   STC_B_MARGIN
#  define STC_B_MARGIN 0.555
#endif
#ifndef   STC_R_MARGIN
#  ifdef A4
#    define STC_R_MARGIN 0.175
#  else
#    define STC_R_MARGIN 0.375
#  endif
#endif
#ifndef   STC_T_MARGIN
#  define STC_T_MARGIN 0.125
#endif
#endif