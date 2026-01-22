#ifndef   UPD_SIGNAL
#ifdef      __unix__
#define       UPD_SIGNAL 1
#else
#define       UPD_SIGNAL 0
#endif
#endif
#ifndef   UPD_MESSAGES
#define   UPD_MESSAGES UPD_M_ERROR
#endif
#include "stdint_.h"
#ifndef   hess_test_INCLUDED
#include "gdevprn.h"
#include "gsparam.h"
#include <stdlib.h>
#include <limits.h>
#include <ctype.h>
#endif
#if       UPD_SIGNAL
#include <signal.h>
#endif
typedef struct upd_s upd_t,*upd_p;
typedef const upd_t *upd_pc;
typedef struct upd_device_s {
gx_device_common;
gx_prn_device_common;
gs_param_string upd_version;
upd_p           upd;
} upd_device;
private dev_proc_print_page(upd_print_page);
private dev_proc_open_device(upd_open);
private dev_proc_close_device(upd_close);
private dev_proc_get_params(upd_get_params);
private dev_proc_put_params(upd_put_params);
private dev_proc_encode_color( upd_rgb_1color);
private dev_proc_decode_color( upd_1color_rgb);
private dev_proc_encode_color( upd_rgb_3color);
private dev_proc_decode_color( upd_3color_rgb);
private dev_proc_encode_color( upd_rgb_4color);
private dev_proc_decode_color(upd_4color_rgb);
private dev_proc_encode_color(upd_cmyk_icolor);
private dev_proc_decode_color( upd_icolor_rgb);
private dev_proc_encode_color(upd_cmyk_kcolor);
private dev_proc_decode_color( upd_kcolor_rgb);
private dev_proc_encode_color(upd_rgb_ovcolor);
#define upd_ovcolor_rgb upd_icolor_rgb
private dev_proc_encode_color(upd_rgb_novcolor);
#define upd_novcolor_rgb upd_icolor_rgb
private int             upd_procs_map( upd_device *udev);
#define upd_set_dev_proc(dev, p, proc) \
((dev)->std_procs.p = (dev)->orig_procs.p = (proc))
private gx_device_procs upd_procs = {
upd_open,
gx_default_get_initial_matrix,
gx_default_sync_output,
gdev_prn_output_page,
upd_close,
gx_default_map_rgb_color,
gx_default_map_color_rgb,
NULL,
NULL,
NULL,
NULL,
NULL,
gx_default_get_bits,
upd_get_params,
upd_put_params,
gx_default_map_cmyk_color
};
upd_device far_data gs_uniprint_device = {
prn_device_body(upd_device, upd_procs,
"uniprint",
DEFAULT_WIDTH_10THS,
DEFAULT_HEIGHT_10THS,
72, 72,
0.0, 0.0, 0.0, 0.0,
1,
1,
1,
0,
2,
0,
upd_print_page),
{ NULL, 0, true },
NULL
};
static const char *const upd_version = "upVersion";
static const char *const upd_mapper[] = { "upColorModel",
#define MAP_GRAY        1
"DeviceGray",
#define MAP_RGBW        2
"DeviceRGBW",
#define MAP_RGB         3
"DeviceRGB",
#define MAP_CMYK        4
"DeviceCMYK",
#define MAP_CMYKGEN     5
"DeviceCMYKgenerate",
#define MAP_RGBOV       6
"DeviceRGB2CMYK",
#define MAP_RGBNOV      7
"DeviceRGB2CMY_K",
NULL
};
static const char *const upd_render[] = { "upRendering",
#define RND_FSCOMP      1
"ErrorDiffusion",
#define RND_FSCMYK      2
"FSCMYK32",
#define RND_FSCMY_K     3
"FSCMY_K",
NULL
};
static const char *const upd_format[] = { "upOutputFormat",
#define FMT_RAS         1
"SunRaster",
#define FMT_EPSON       2
"Epson",
#define FMT_ESCP2Y      3
"EscP2",
#define FMT_ESCP2XY     4
"EscP2XY",
#define FMT_RTL         5
"Pcl",
#define FMT_CANON       6
"Canon",
#define FMT_ESCNMY      7
"EscNozzleMap",
NULL
};
static const char *const *const upd_choice[] = {
#define C_MAPPER        0
upd_mapper,
#define C_RENDER        1
upd_render,
#define C_FORMAT        2
upd_format
};
static const char *const upd_flags[] = {
#define B_REVDIR            ((uint32_t) 1<<0)
"upFSReverseDirection",
#define B_FIXDIR            ((uint32_t) 1<<1)
"upFSFixedDirection",
#define B_FSWHITE           ((uint32_t) 1<<2)
"upFSProcessWhiteSpace",
#define B_FSZERO            ((uint32_t) 1<<3)
"upFSZeroInit",
#define B_PAGEWIDTH         ((uint32_t) 1<<4)
"upAdjustPageWidthCommand",
#define B_PAGELENGTH        ((uint32_t) 1<<5)
"upAdjustPageLengthCommand",
#define B_TOPMARGIN         ((uint32_t) 1<<6)
"upAdjustTopMarginCommand",
#define B_BOTTOMMARGIN      ((uint32_t) 1<<7)
"upAdjustBottomMarginCommand",
#define B_RESOLUTION        ((uint32_t) 1<<8)
"upAdjustResolutionCommand",
#define B_MEDIASIZE         ((uint32_t) 1<<9)
"upAdjustMediaSize",
#define B_XABS              ((uint32_t) 1<<10)
"upFormatXabsolute",
#define B_YABS              ((uint32_t) 1<<11)
"upFormatYabsolute",
#define B_MAP               ((uint32_t) 1<<12)
"upColorModelInitialized",
#define B_BUF               ((uint32_t) 1<<13)
"upRasterBufferInitialized",
#define B_RENDER            ((uint32_t) 1<<14)
"upRenderingInitialized",
#define B_FORMAT            ((uint32_t) 1<<15)
"upOutputFormatInitialized",
#define B_ABORT             ((uint32_t) 1<<16)
"upOutputAborted",
#define B_ERROR             ((uint32_t) 1<<17)
"upErrorDetected",
#define B_OPEN              ((uint32_t) 1<<18)
"upWroteData",
#define B_YFLIP             ((uint32_t) 1<<19)
"upYFlip",
#define B_REDUCEK           ((uint32_t) 1<<20)
"upFSReduceK"
};
#define B_OK4GO  (B_MAP | B_BUF | B_RENDER | B_FORMAT)
static const char *const upd_ints[] = {
#define I_PWIDTH            0
"upOutputWidth",
#define I_PHEIGHT           1
"upOutputHeight",
#define I_OCOMP             2
"upOutputComponents",
#define I_NSCNBUF           3
"upOutputBuffers",
#define I_XSTEP             4
"upOutputXStep",
#define I_XOFS              5
"upOutputXOffset",
#define I_YSTEP             6
"upOutputYStep",
#define I_YOFS              7
"upOutputYOffset",
#define I_PINS2WRITE        8
"upOutputPins",
#define I_NXPASS            9
"upWeaveXPasses",
#define I_NYPASS           10
"upWeaveYPasses",
#define I_NPASS            11
"upWeavePasses",
#define I_BEG_Y            12
"upWeaveInitialScan",
#define I_END_Y            13
"upWeaveFinalScan",
#define I_BEGSKIP          14
"upWeaveYOffset",
#define I_ROWS             15
"upNozzleMapRowsPerPass",
#define I_PATRPT           16
"upNozzleMapPatternRepeat"
};
static const char *const upd_int_a[] = {
#define IA_COLOR_INFO       0
"upColorInfo",
#define IA_COMPBITS         1
"upComponentBits",
#define IA_COMPSHIFT        2
"upComponentShift",
#define IA_COMPORDER        3
"upOutputComponentOrder",
#define IA_STD_DY           4
"upWeaveYFeeds",
#define IA_STD_IX           5
"upWeaveXStarts",
#define IA_BEG_DY           6
"upWeaveInitialYFeeds",
#define IA_BEG_IX           7
"upWeaveInitialXStarts",
#define IA_BEGBOT           8
"upWeaveInitialPins",
#define IA_END_DY           9
"upWeaveFinalYFeeds",
#define IA_END_IX          10
"upWeaveFinalXStarts",
#define IA_ENDTOP          11
"upWeaveFinalPins",
#define IA_ROWMASK         12
"upNozzleMapRowMask",
#define IA_SCNOFS       13
"upNozzleMapMaskScanOffset"
};
static const char *const upd_strings[] = {
#define S_MODEL             0
"upModel",
#define S_OPEN              1
"upBeginJobCommand",
#define S_CLOSE             2
"upEndJobCommand",
#define S_BEGIN             3
"upBeginPageCommand",
#define  S_END              4
"upEndPageCommand",
#define  S_ABORT            5
"upAbortCommand",
#define S_XMOVE             6
"upXMoveCommand",
#define S_XSTEP             7
"upXStepCommand",
#define S_SETLF             8
"upSetLineFeedCommand",
#define S_YMOVE             9
"upYMoveCommand",
#define S_YSTEP            10
"upYStepCommand"
};
static const char *const upd_string_a[] = {
#define SA_SETCOMP          0
"upSelectComponentCommands",
#define SA_WRITECOMP        1
"upWriteComponentCommands"
};
static const char *const upd_float_a[] = {
#define FA_WXFER            0
"upWhiteTransfer",
#define FA_RXFER            1
"upRedTransfer",
#define FA_GXFER            2
"upGreenTransfer",
#define FA_BXFER            3
"upBlueTransfer",
#define FA_KXFER            4
"upBlackTransfer",
#define FA_CXFER            5
"upCyanTransfer",
#define FA_MXFER            6
"upMagentaTransfer",
#define FA_YXFER            7
"upYellowTransfer",
#define FA_MARGINS          8
"upMargins",
#define FA_MAP              9
"upColorMap"
};
#undef INT32_MIN
#undef INT32_MAX
#undef UINT32_MAX
#if     arch_log2_sizeof_int < 2
#define                   INT32_MIN  LONG_MIN
#define                   INT32_MAX  LONG_MAX
#define                  UINT32_MAX ULONG_MAX
#else
#define                   INT32_MIN   INT_MIN
#define                   INT32_MAX   INT_MAX
#define                  UINT32_MAX  UINT_MAX
#endif
typedef struct updcmap_s {
gx_color_value      *code;
uint32_t               bitmsk;
int                  bitshf;
int                  xfer;
int                  bits;
int                  comp;
bool                 rise;
} updcmap_t, *updcmap_p;
typedef const updcmap_t *updcmap_pc;
typedef struct updcomp_s {
int32_t                offset;
int32_t                scale;
int32_t                threshold;
int32_t                spotsize;
uint32_t               bitmsk;
int                  bitshf;
int                  bits;
int                  cmap;
} updcomp_t, *updcomp_p;
typedef struct updscan_s {
byte   *bytes;
int    *xbegin;
int    *xend;
} updscan_t, *updscan_p;
#define UPD_CMAP_MAX     4
#define UPD_VALPTR_MAX  32
#define upd_proc_pxlget(name) uint32_t name(upd_p upd)
#define upd_proc_render(name) int name(upd_p upd)
#define upd_proc_writer(name) int name(upd_p upd,FILE *out)
struct upd_s {
int                   *choice;
int                   *ints;
gs_param_int_array    *int_a;
gs_param_string       *strings;
gs_param_string_array *string_a;
gs_param_float_array  *float_a;
updcmap_t              cmap[UPD_CMAP_MAX];
byte                  *gsbuf;
byte                  *gsscan;
byte                  *pxlptr;
upd_proc_pxlget(     (*pxlget));
upd_proc_render(     (*render));
upd_proc_writer(     (*writer));
updscan_p             *scnbuf;
int32_t                 *valbuf;
void                  *valptr[UPD_VALPTR_MAX];
byte                  *outbuf;
upd_proc_render(     (*start_render));
upd_proc_writer(     (*start_writer));
uint32_t                 flags;
int                    pdwidth;
int                    pdheight;
uint                   ngsbuf;
int                    gswidth;
int                    gsheight;
int                    rwidth;
int                    pwidth;
int                    pheight;
int                    ncomp;
int                    nmap;
uint                   nvalbuf;
int                    nscnbuf;
int                    ocomp;
int                    nbytes;
int                    nlimits;
int                    scnmsk;
uint                   noutbuf;
int                    ixpass;
int                    ipass;
int                    icomp;
int                    lf;
int                    xprinter;
int                    yscan;
int                    yprinter;
int                    yscnbuf;
};
#define UPD_M_NONE      0x0000
#define UPD_M_ERROR     0x0001
#define UPD_M_WARNING   0x0002
#define UPD_M_TOPCALLS  0x0004
#define UPD_M_MAPCALLS  0x0008
#define UPD_M_SETUP     0x0010
#define UPD_M_FSBUF     0x0020
#define UPD_M_FMTVARS   0x0040
private int             upd_open_map( upd_device *udev);
private int             upd_close_map(upd_device *udev);
private inline uint32_t   upd_truncate(upd_pc,int,gx_color_value);
private inline gx_color_value
upd_expand(upd_pc upd,int i,gx_color_index ci0)
{
const updcmap_pc cmap = upd->cmap + i;
uint32_t ci = (uint32_t)((ci0 >> cmap->bitshf) & cmap->bitmsk);
if(!cmap->rise) ci = cmap->bitmsk - ci;
if(gx_color_value_bits > cmap->bits) return cmap->code[ci];
else                                 return (gx_color_value) ci;
}
private void            upd_open_render(   upd_device *udev);
private void            upd_close_render(  upd_device *udev);
private void            upd_open_fscomp(   upd_device *udev);
private int             upd_fscomp(        upd_p upd);
private void            upd_close_fscomp(  upd_device *udev);
private void            upd_open_fscmyk(   upd_device *udev);
private int             upd_fscmyk(        upd_p upd);
private void            upd_open_fscmy_k(  upd_device *udev);
private int             upd_fscmy_k(       upd_p upd);
private int             upd_open_writer(   upd_device *udev);
private void            upd_close_writer(  upd_device *udev);
#if UPD_SIGNAL
private void            upd_signal_handler(int sig);
#endif
private int             upd_open_rascomp(   upd_device *udev);
private int             upd_start_rascomp(  upd_p upd, FILE *out);
private int             upd_rascomp(        upd_p upd, FILE *out);
private void            upd_limits(        upd_p upd, bool check);
private int             upd_open_wrtescp(  upd_device *udev);
private int             upd_wrtescp(       upd_p upd, FILE *out);
private int             upd_rle(byte *out,const byte *in,int nbytes);
private int             upd_open_wrtescp2( upd_device *udev);
private int             upd_wrtescp2(      upd_p upd, FILE *out);
private int             upd_wrtescp2x(     upd_p upd, FILE *out);
private int             upd_open_wrtrtl(   upd_device *udev);
private int             upd_wrtrtl(        upd_p upd, FILE *out);
private int             upd_open_wrtcanon( upd_device *udev);
private int             upd_wrtcanon(      upd_p upd, FILE *out);
private int             upd_wrtescnm(      upd_p upd, FILE *out);
private uint32_t upd_pxlfwd(upd_p upd);
private uint32_t upd_pxlrev(upd_p upd);
#define upd_pxlget(UPD) (*UPD->pxlget)(UPD)
private void *upd_cast(const void *);
#define UPD_MM_GET_ARRAY(mem, Which,Nelts)                                 \
Which = NULL;                                                      \
if(0 < (Nelts)) {                                                  \
byte *tmp = gs_malloc(mem, Nelts,sizeof(Which[0]),"uniprint/params");\
if(tmp) {                                                       \
memset(tmp,0,(Nelts)*sizeof(Which[0]));                      \
Which = (void *) tmp;                                        \
} else {                                                        \
return_error(gs_error_VMerror);                             \
}                                                               \
}
#define UPD_MM_DEL_ARRAY(mem, Which,Nelts,Delete)                            \
if(Which && 0 < (Nelts)) {                                           \
uint ii;                                                          \
for(ii = 0; (Nelts) > ii; ++ii) Delete(mem, Which[ii]);                \
gs_free(mem, upd_cast(Which),Nelts,sizeof(Which[0]),"uniprint/params");\
}                                                                    \
Which = 0
#define UPD_MM_DEL_VALUE(mem, Which)
#define UPD_MM_DEL_PARAM(mem, Which)  {                                  \
if(Which.data && Which.size)                                     \
gs_free(mem, upd_cast(Which.data),Which.size,sizeof(Which.data[0]),\
"uniprint/params");                                        \
}
#define UPD_MM_DEL_APARAM(mem, Which) {                                  \
if(Which.data && Which.size) {                                   \
uint iii;                                                     \
for(iii = 0; iii < Which.size; ++iii)                         \
UPD_MM_DEL_PARAM(mem, Which.data[iii]);                         \
gs_free(mem, upd_cast(Which.data),Which.size,sizeof(Which.data[0]),\
"uniprint/params");                                        \
}                                                                \
}
#define UPD_MM_CPY_ARRAY(mem, To,From,Nelts,Copy)                \
UPD_MM_GET_ARRAY(mem, To,Nelts);                              \
if(To && From) {                                         \
uint ii;                                              \
for(ii = 0; (Nelts) > ii; ++ii) Copy(mem, To[ii],From[ii]);\
}
#define UPD_MM_CPY_VALUE(mem,To,From)  To = From
#define UPD_MM_CPY_VALUE_3(mem,To,From)  To = From
#define UPD_MM_CPY_PARAM(mem, To, From)                                       \
if(From.data && From.size) {                                         \
UPD_MM_GET_ARRAY(mem, To.data,From.size);                              \
if(To.data) {                                                     \
To.size = From.size;                                           \
memcpy(upd_cast(To.data),From.data,To.size*sizeof(To.data[0]));\
}                                                                 \
}
#define UPD_MM_CPY_APARAM(mem, To,From)                                     \
if(From.data && From.size) {                                        \
UPD_MM_GET_ARRAY(mem, To.data,From.size);			       \
if(To.data) {                                                    \
gs_param_string *tmp2 = (gs_param_string *) upd_cast(To.data);\
uint iii;                                                     \
To.size = From.size;                                          \
for(iii = 0; To.size > iii; ++iii)                            \
UPD_MM_CPY_PARAM(mem, tmp2[iii],From.data[iii]);	       \
}                                                                \
}
static const char rcsid[] = "$Revision: 1.16 $";
static const float upd_data_xfer[2] = { 0.0, 1.0 };
private void *
upd_cast(const void *data)
{
return (void *) data;
}
#if UPD_SIGNAL
static upd_p sigupd = NULL;
private void
upd_signal_handler(int sig)
{
if(sigupd) sigupd->flags |= B_ABORT;
}
#endif
private int
upd_print_page(gx_device_printer *pdev, FILE *out)
{
upd_device *const udev  = (upd_device *) pdev;
const upd_p       upd   = udev->upd;
const int *const  ints  = upd ? upd->ints : NULL;
int error,need,yfill;
#if UPD_SIGNAL
void (*oldint )(int) = NULL;
void (*oldterm)(int) = NULL;
upd_p  oldupd            = sigupd;
#endif
if(!upd || B_OK4GO != (upd->flags & (B_OK4GO | B_ERROR))) {
#if UPD_MESSAGES & (UPD_M_ERROR | UPD_M_TOPCALLS)
errprintf("CALL-REJECTED upd_print_page(0x%05lx,0x%05lx)\n",
(long) udev,(long) out);
#endif
return gs_error_undefined;
}
#if UPD_MESSAGES & UPD_M_TOPCALLS
errprintf("CALL: upd_print_page(0x%05lx,0x%05lx)\n",
(long) udev,(long) out);
#endif
#if UPD_SIGNAL
sigupd  = upd;
oldint  = signal(SIGINT, upd_signal_handler);
oldterm = signal(SIGTERM,upd_signal_handler);
#endif
if(!(upd->flags & B_OPEN)) {
if(0   <  upd->strings[S_OPEN].size)
fwrite(upd->strings[S_OPEN].data,1,upd->strings[S_OPEN].size,out);
upd->flags |= B_OPEN;
}
if(0  <   upd->strings[S_BEGIN].size)
fwrite(upd->strings[S_BEGIN].data,1,upd->strings[S_BEGIN].size,out);
upd->xprinter  = 0;
upd->yscan     = 0;
upd->yprinter  = 0;
upd->yscnbuf   = 0;
if(upd->start_render) (*upd->start_render)(upd);
if(upd->start_writer) (*upd->start_writer)(upd,out);
need = ints[I_NYPASS] * ints[I_PINS2WRITE];
if(0 >= need) need = 1;
upd->ipass  =  0;
upd->ixpass =  0;
upd->icomp  = -1;
upd->lf     = -1;
while(upd->pheight > upd->yscan) {
if(ints[I_BEGSKIP] > upd->yscan) yfill = 0;
else                             yfill = upd->yscan - ints[I_BEGSKIP];
for(yfill += upd->nscnbuf; upd->yscnbuf < yfill; upd->yscnbuf++) {
if(upd->gsheight > upd->yscnbuf)  {
if(0 > (*dev_proc(udev,get_bits))((gx_device *) udev,
upd->yscnbuf,upd->gsbuf,&upd->gsscan)) {
#if UPD_MESSAGES & UPD_M_WARNING
errprintf("get_bits aborted with error, yscnbuf = %4d\n",
upd->yscnbuf);
#endif
break;
}
} else {
memset(upd->gsscan = upd->gsbuf,0,upd->ngsbuf);
}
if(0 > (*upd->render)(upd)) {
#if UPD_MESSAGES & UPD_M_WARNING
errprintf("Rendering aborted with error, yscnbuf = %4d\n",
upd->yscnbuf);
#endif
break;
}
}
if((upd->yscnbuf ^ yfill) & upd->scnmsk) break;
while((upd->yscan - ints[I_BEGSKIP] + need) < upd->yscnbuf) {
(*upd->writer)(upd,out);
if(upd->yscan >= upd->pheight) break;
if(upd->flags  & B_ABORT ) {
#if UPD_MESSAGES & UPD_M_WARNING
errprintf("Printing aborted upon interrupt, yscan = %4d\n",
upd->yscan);
#endif
break;
}
}
if((upd->yscan - ints[I_BEGSKIP] + need) < upd->yscnbuf) break;
}
if((upd->pheight > upd->yscan) &&
(0  <  upd->strings[S_ABORT].size)) {
fwrite(upd->strings[S_ABORT].data,1,upd->strings[S_ABORT].size,out);
upd->flags &= ~B_OPEN;
} else if(0  <   upd->strings[S_END].size) {
fwrite(upd->strings[S_END].data,1,upd->strings[S_END].size,out);
}
if((NULL != udev->fname  ) && strchr(udev->fname,'%')) {
if(0  <   upd->strings[S_CLOSE].size)
fwrite(upd->strings[S_CLOSE].data,1,upd->strings[S_CLOSE].size,out);
upd->flags &= ~B_OPEN;
}
fflush(out);
if(upd->pheight > upd->yscan) error = gs_error_interrupt;
else if(ferror(out))          error = gs_error_ioerror;
else                          error = 0;
#if UPD_MESSAGES & UPD_M_TOPCALLS
errprintf("RETURN: %d = upd_print_page(0x%05lx,0x%05lx)\n",
error,(long) udev,(long)out);
#endif
#if UPD_SIGNAL
sigupd = oldupd;
(void) signal(SIGINT ,oldint);
(void) signal(SIGTERM,oldterm);
#endif
return error;
}
private int
upd_open(gx_device *pdev)
{
upd_device *const udev    =  (upd_device *) pdev;
const upd_p       upd     =  udev->upd;
int              error;
#if UPD_MESSAGES & UPD_M_TOPCALLS
errprintf("CALL: upd_open(0x%05lx)\n",(long) pdev);
#endif
if((NULL != upd) &&
(NULL != upd->float_a[FA_MARGINS].data) &&
(4    == upd->float_a[FA_MARGINS].size)    ) {
static float m[4];
m[1] = upd->float_a[FA_MARGINS].data[1] / 72.0;
m[3] = upd->float_a[FA_MARGINS].data[3] / 72.0;
if(B_YFLIP & upd->flags) {
m[0] = upd->float_a[FA_MARGINS].data[2] / 72.0;
m[2] = upd->float_a[FA_MARGINS].data[0] / 72.0;
} else {
m[0] = upd->float_a[FA_MARGINS].data[0] / 72.0;
m[2] = upd->float_a[FA_MARGINS].data[2] / 72.0;
}
gx_device_set_margins((gx_device *) udev, m, true);
}
error = gdev_prn_open(pdev);
if(upd) {
upd->flags &= ~B_OK4GO;
if(0 > error) upd->flags |= B_ERROR;
if(gs_error_VMerror == upd_open_map(udev)) error = gs_error_VMerror;
upd->gswidth  = udev->width -
(int)((dev_l_margin(udev)+dev_r_margin(udev))*udev->x_pixels_per_inch);
upd->gsheight = udev->height -
(int)((dev_t_margin(udev)+dev_b_margin(udev))*udev->y_pixels_per_inch);
upd->ngsbuf = 0;
upd->gsbuf  = NULL;
if(B_MAP & upd->flags) {
uint want  = gx_device_raster(pdev,true);
upd->gsbuf = gs_malloc(pdev->memory, want,1,"upd/gsbuf");
if(upd->gsbuf) {
upd->ngsbuf = want;
upd->flags |= B_BUF;
} else {
error = gs_error_VMerror;
upd->flags |= B_ERROR;
}
}
upd_open_render(udev);
if(gs_error_VMerror == upd_open_writer(udev)) error = gs_error_VMerror;
udev->upd->pdwidth  = udev->width;
udev->upd->pdheight = udev->height;
#if UPD_MESSAGES & UPD_M_SETUP
if((upd->flags & (B_OK4GO | B_ERROR)) == B_OK4GO) {
int i,j,l,ln,lv;
errprintf("\nupd->flags    = 0x%05lx\n",(unsigned long)upd->flags);
errprintf(  "upd->pdwidth  = %5d\n",upd->pdwidth);
errprintf(  "upd->pdheight = %5d\n",upd->pdheight);
errprintf(  "upd->ngsbuf   = %5u\n",upd->ngsbuf);
errprintf(  "upd->gswidth  = %5d\n",upd->gswidth);
errprintf(  "upd->gsheight = %5d\n",upd->gsheight);
errprintf(  "upd->rwidth   = %5d\n",upd->rwidth);
errprintf(  "upd->pwidth   = %5d\n",upd->pwidth);
errprintf(  "upd->pheight  = %5d\n",upd->pheight);
errprintf(  "upd->nvalbuf  = %5u\n",upd->nvalbuf);
errprintf(  "upd->nscnbuf  = %5d\n",upd->nscnbuf);
errprintf(  "upd->ncomp    = %5d\n",upd->ncomp);
errprintf(  "upd->ocomp    = %5d\n",upd->ocomp);
errprintf(  "upd->nbytes   = %5d\n",upd->nbytes);
errprintf(  "upd->nlimits  = %5d\n",upd->nlimits);
errprintf(  "upd->scnmsk   = %5d\n",upd->scnmsk);
errprintf(  "upd->noutbuf  = %5u\n",upd->noutbuf);
errprintf(  "upd->ixpass   = %5d\n",upd->ixpass);
errprintf(  "upd->ipass    = %5d\n",upd->ipass);
errprintf(  "upd->icomp    = %5d\n",upd->icomp);
errprintf(  "upd->lf       = %5d\n",upd->lf);
errprintf(  "upd->xprinter = %5d\n",upd->xprinter);
errprintf(  "upd->yscan    = %5d\n",upd->yscan);
errprintf(  "upd->yprinter = %5d\n",upd->yprinter);
errprintf(  "upd->yscnbuf  = %5d\n",upd->yscnbuf);
ln = 13;
lv = 5;
for(i = 0; countof(upd_choice) > i; ++i) {
if(!upd_choice[i]) continue;
l = strlen(upd_choice[i][0]);
if(ln < l) ln = l;
for(j = 1; upd_choice[i][j]; ++j) {
l = strlen(upd_choice[i][j]);
if(lv < l) lv = l;
}
}
for(i = 0; countof(upd_flags) > i; ++i) {
if(upd_flags[i]) {
l = strlen(upd_flags[i]);
if(ln < l) ln = l;
}
}
for(i = 0; countof(upd_ints) > i; ++i) {
if(upd_ints[i]) {
l = strlen(upd_ints[i]);
if(ln < l) ln = l;
}
}
for(i = 0; countof(upd_int_a) > i; ++i) {
if(upd_int_a[i]) {
l = strlen(upd_int_a[i]);
if(ln < l) ln = l;
}
}
for(i = 0; countof(upd_strings) > i; ++i) {
if(upd_strings[i]) {
l = strlen(upd_strings[i]);
if(ln < l) ln = l;
}
}
for(i = 0; countof(upd_string_a) > i; ++i) {
if(upd_string_a[i]) {
l = strlen(upd_string_a[i]);
if(ln < l) ln = l;
}
}
for(i = 0; countof(upd_float_a) > i; ++i) {
if(upd_float_a[i]) {
l = strlen(upd_float_a[i]);
if(ln < l) ln = l;
}
}
for(i = 0; countof(upd_choice) > i; ++i) {
if(upd_choice[i]) {
errprintf("%*s = %-*s (%2d)\n",ln,upd_choice[i][0],
lv,upd_choice[i][upd->choice[i]],upd->choice[i]);
} else {
errprintf("%*s[%2d] = %2d\n",ln-4,"upd_choice",i,
upd->choice[i]);
}
}
for(i = 0; countof(upd_flags) > i; ++i) {
if(upd_flags[i]) {
errprintf("%*s = %s\n",ln,upd_flags[i],
((uint32_t) 1 << i) & upd->flags ? "true" : "false");
} else {
errprintf("%*s[%2d] = %s\n",ln-4,"upd_flags",i,
((uint32_t) 1 << i) & upd->flags ? "true" : "false");
}
}
for(i = 0; countof(upd_ints) > i; ++i) {
if(upd_ints[i]) {
errprintf("%*s = %5d\n",ln,upd_ints[i],upd->ints[i]);
} else {
errprintf("%*s[%2d] = %5d\n",ln-4,"upd_ints",i,upd->ints[i]);
}
}
}
errprintf("\n%sready to print\n\n",
B_OK4GO != (upd->flags & (B_OK4GO | B_ERROR)) ?
"NOT " : "");
#endif
}
#if UPD_MESSAGES & UPD_M_TOPCALLS
errprintf("RETURN: %d = upd_open(0x%05lx)\n",
error,(long) pdev);
#endif
return error;
}
private int
upd_close(gx_device *pdev)
{
upd_device *const udev    =  (upd_device *) pdev;
const upd_p       upd     =  udev->upd;
int         error = 0;
int         code;
#if UPD_MESSAGES & UPD_M_TOPCALLS
errprintf("CALL: upd_close(0x%05lx)\n",(long)pdev);
#endif
if( upd && (( B_OPEN | B_OK4GO) ==
((B_OPEN | B_OK4GO | B_ERROR) & upd->flags))) {
if(udev->file && upd->strings && 0 < upd->strings[S_CLOSE].size)
fwrite(upd->strings[S_CLOSE].data,1,
upd->strings[S_CLOSE].size,udev->file);
upd->flags &= ~B_OPEN;
}
if(upd) {
upd_close_writer(udev);
if(upd->gsbuf)
gs_free(pdev->memory, upd->gsbuf,upd->ngsbuf,1,"uniprint/gsbuf");
upd->gsbuf  = NULL;
upd->ngsbuf = 0;
upd->flags &= ~B_BUF;
upd_close_render(udev);
upd_close_map(udev);
UPD_MM_DEL_ARRAY(pdev->memory, upd->choice,  countof(upd_choice),  UPD_MM_DEL_VALUE);
UPD_MM_DEL_ARRAY(pdev->memory, upd->ints,    countof(upd_ints),    UPD_MM_DEL_VALUE);
UPD_MM_DEL_ARRAY(pdev->memory, upd->int_a,   countof(upd_int_a),   UPD_MM_DEL_PARAM);
UPD_MM_DEL_ARRAY(pdev->memory, upd->strings, countof(upd_strings), UPD_MM_DEL_PARAM);
UPD_MM_DEL_ARRAY(pdev->memory, upd->string_a,countof(upd_string_a),UPD_MM_DEL_APARAM);
UPD_MM_DEL_ARRAY(pdev->memory, upd->float_a, countof(upd_float_a), UPD_MM_DEL_PARAM);
gs_free(pdev->memory, upd,sizeof(upd[0]),1,"uniprint");
udev->upd = NULL;
}
code = gdev_prn_close(pdev);
error = error > code ? code : error;
#if UPD_MESSAGES & UPD_M_TOPCALLS
errprintf("RETURN: %d = upd_close(0x%05lx)\n",
error,(long) pdev);
#endif
return error;
}
#if UPD_MESSAGES & UPD_M_TOPCALLS
#define UPD_EXIT_GET(Err,Dev,List)                                      \
if(0 > Err) {                                                        \
errprintf("RETURN-%d: %d upd_get_params(0x%05lx,0x%05lx)\n", \
__LINE__,Err,(long) Dev,(long) List);                          \
return_error(Err);                                                \
}
#else
#define UPD_EXIT_GET(Err,Dev,List) if(0 > Err) return_error(Err);
#endif
private int
upd_get_params(gx_device *pdev, gs_param_list *plist)
{
upd_device *const udev    =  (upd_device *) pdev;
const upd_p       upd     =  udev->upd;
int               error,i;
#if UPD_MESSAGES & UPD_M_TOPCALLS
errprintf("CALL: upd_get_params(0x%05lx,0x%05lx)\n",
(long) udev,(long) plist);
#endif
error = gdev_prn_get_params((gx_device *)udev,plist);
UPD_EXIT_GET(error,udev,plist);
if(upd_version) {
udev->upd_version.data       = (const byte *) rcsid;
udev->upd_version.size       = strlen(rcsid);
udev->upd_version.persistent = true;
error = param_write_string(plist,upd_version,&udev->upd_version);
UPD_EXIT_GET(error,udev,plist);
}
for(i = 0; i < countof(upd_choice); ++i) {
if(!upd_choice[i]) continue;
if(upd && upd->choice && upd->choice[i]) {
gs_param_string name;
name.data       = (const byte *) upd_choice[i][upd->choice[i]];
name.size       = strlen((const char *) name.data);
name.persistent = true;
error = param_write_name(plist,upd_choice[i][0],&name);
} else {
error = param_write_null(plist,upd_choice[i][0]);
}
UPD_EXIT_GET(error,udev,plist);
}
for(i = 0; i < countof(upd_flags); ++i) {
if(!upd_flags[i]) continue;
if(upd) {
bool value = upd->flags & ((uint32_t) 1 << i);
error = param_write_bool(plist,upd_flags[i],&value);
} else {
error = param_write_null(plist,upd_flags[i]);
}
UPD_EXIT_GET(error,udev,plist);
}
for(i = 0; i < countof(upd_ints); ++i) {
if(!upd_ints[i]) continue;
if(upd && upd->ints && upd->ints[i]) {
int value = upd->ints[i];
error = param_write_int( plist,upd_ints[i],&value);
} else {
error = param_write_null(plist,upd_ints[i]);
}
UPD_EXIT_GET(error,udev,plist);
}
for(i = 0; i < countof(upd_int_a); ++i) {
if(!upd_int_a[i]) continue;
if(upd && upd->int_a && upd->int_a[i].size) {
error = param_write_int_array( plist,upd_int_a[i],(upd->int_a+i));
} else {
error = param_write_null(plist,upd_int_a[i]);
}
UPD_EXIT_GET(error,udev,plist);
}
for(i = 0; i < countof(upd_strings); ++i) {
if(!upd_strings[i]) continue;
if(upd && upd->strings && upd->strings[i].size) {
error = param_write_string( plist,upd_strings[i],(upd->strings+i));
} else {
error = param_write_null(plist,upd_strings[i]);
}
UPD_EXIT_GET(error,udev,plist);
}
for(i = 0; i < countof(upd_string_a); ++i) {
if(!upd_string_a[i]) continue;
if(upd && upd->string_a && upd->string_a[i].size) {
error =
param_write_string_array( plist,upd_string_a[i],(upd->string_a+i));
} else {
error = param_write_null(plist,upd_string_a[i]);
}
UPD_EXIT_GET(error,udev,plist);
}
for(i = 0; i < countof(upd_float_a); ++i) {
if(!upd_float_a[i]) continue;
if(upd && upd->float_a && upd->float_a[i].size) {
error =
param_write_float_array( plist,upd_float_a[i],(upd->float_a+i));
} else {
error = param_write_null(plist,upd_float_a[i]);
}
UPD_EXIT_GET(error,udev,plist);
}
#if UPD_MESSAGES & UPD_M_TOPCALLS
errprintf("RETURN: %d = upd_get_params(0x%05lx,0x%05lx)\n",
error,(long) udev,(long) plist);
#endif
return error;
}
#undef UPD_EXIT_GET
private int
upd_put_params(gx_device *pdev, gs_param_list *plist)
{
upd_device *const      udev       = (upd_device *) pdev;
upd_p                  upd        = udev->upd;
int                    error      = 0, code,i;
float                  MarginsHWResolution[2],Margins[2];
gx_device_color_info   color_info;
uint32_t                 flags      = 0;
int                   *choice     = NULL;
int                   *ints       = NULL;
gs_param_int_array    *int_a      = NULL;
gs_param_string       *strings    = NULL;
gs_param_string_array *string_a   = NULL;
gs_param_float_array  *float_a    = NULL, mfa;
#define UPD_PUT_FLAGS       0x0002
#define UPD_PUT_CHOICE      0x0004
#define UPD_PUT_INTS        0x0008
#define UPD_PUT_INT_A       0x0010
#define UPD_PUT_STRINGS     0x0020
#define UPD_PUT_STRING_A    0x0040
#define UPD_PUT_FLOAT_A     0x0080
#define UPD_PUT_CHANGEDSIZE 0x0100
#if UPD_MESSAGES & UPD_M_TOPCALLS
errprintf("CALL: upd_put_params(0x%05lx,0x%05lx)\n",
(long)udev,(long)plist);
#endif
if(upd && (B_OPEN & udev->upd->flags) && (NULL != udev->file)) {
gs_param_string fname = { NULL, 0, false };
code = param_read_string(plist,"OutputFile",&fname);
if((1 != code) && (0 != code)) {
code = param_read_null(plist,"OutputFile");
if(0 == code) {
fname.data = (const byte *) "";
fname.size = 0;
}
}
if((0 == code) &&
strncmp((const char *)fname.data,udev->fname,fname.size)) {
if(upd->strings && 0 < udev->upd->strings[S_CLOSE].size)
fwrite(upd->strings[S_CLOSE].data,1,
upd->strings[S_CLOSE].size,udev->file);
upd->flags &= ~B_OPEN;
}
}
#if UPD_MESSAGES & UPD_M_SETUP
#define UPD_PARAM_READ(Param_read,Name,Object)       \
code = Param_read(plist,Name,&Object);            \
if(0 > code) {                                    \
code = param_read_null(plist,Name);            \
if(0 == code) memset(&Object,0,sizeof(Object));\
}                                                 \
if(!code) errprintf(                         \
"upd_put_params: retrieved parameter \"%s\"\n",\
Name);                                         \
if(0 > code) {                                    \
param_signal_error(plist,Name,code);           \
if(error > code) error = code;                 \
}
#else
#define UPD_PARAM_READ(Param_read,Name,Object)       \
code = Param_read(plist,Name,&Object);            \
if(0 > code) {                                    \
code = param_read_null(plist,Name);            \
if(0 == code) memset(&Object,0,sizeof(Object));\
}                                                 \
if(0 > code) {                                    \
param_signal_error(plist,Name,code);           \
if(error > code) error = code;                 \
}
#endif
UPD_PARAM_READ(param_read_string,upd_version,udev->upd_version)
MarginsHWResolution[0] = udev->MarginsHWResolution[0];
MarginsHWResolution[1] = udev->MarginsHWResolution[1];
Margins[0] = udev->Margins[0];
Margins[1] = udev->Margins[1];
color_info = udev->color_info;
if(upd) {
flags = upd->flags;
UPD_MM_CPY_ARRAY(udev->memory, choice,  upd->choice,  countof(upd_choice),
UPD_MM_CPY_VALUE);
UPD_MM_CPY_ARRAY(udev->memory, ints,    upd->ints,    countof(upd_ints),
UPD_MM_CPY_VALUE);
UPD_MM_CPY_ARRAY(udev->memory, int_a,   upd->int_a,   countof(upd_int_a),
UPD_MM_CPY_PARAM);
UPD_MM_CPY_ARRAY(udev->memory, strings, upd->strings, countof(upd_strings),
UPD_MM_CPY_PARAM);
UPD_MM_CPY_ARRAY(udev->memory, string_a,upd->string_a,countof(upd_string_a),
UPD_MM_CPY_APARAM);
UPD_MM_CPY_ARRAY(udev->memory, float_a, upd->float_a, countof(upd_float_a),
UPD_MM_CPY_PARAM);
} else {
flags = 0;
UPD_MM_GET_ARRAY(udev->memory, choice,  countof(upd_choice));
UPD_MM_GET_ARRAY(udev->memory, ints,    countof(upd_ints));
UPD_MM_GET_ARRAY(udev->memory, int_a,   countof(upd_int_a));
UPD_MM_GET_ARRAY(udev->memory, strings, countof(upd_strings));
UPD_MM_GET_ARRAY(udev->memory, string_a,countof(upd_string_a));
UPD_MM_GET_ARRAY(udev->memory, float_a, countof(upd_float_a));
}
for(i = 0; countof(upd_choice) > i; ++i) {
gs_param_string value = { NULL, 0, false};
if(!upd_choice[i][0]) continue;
UPD_PARAM_READ(param_read_name,upd_choice[i][0],value);
if(0 == code) {
if(0 <= error) error |= UPD_PUT_CHOICE;
choice[i] = 0;
if(0 < value.size) {
int j;
for(j = 1; upd_choice[i][j]; ++j) {
if((strlen(upd_choice[i][j]) == value.size) &&
(0 == strncmp(upd_choice[i][j],
(const char *) value.data,value.size))) {
choice[i] = j;
break;
}
}
}
}
}
for(i = 0; countof(upd_flags) > i; ++i) {
uint32_t bit  = (uint32_t) 1 << i;
bool   flag = flags & bit ? true : false;
if(!upd_flags[i]) continue;
UPD_PARAM_READ(param_read_bool,upd_flags[i],flag);
if(0 == code) {
if(0 <= error) error |= UPD_PUT_FLAGS;
if(flag) flags |=  bit;
else     flags &= ~bit;
}
}
for(i = 0; countof(upd_ints) > i; ++i) {
int value = ints[i];
if(!upd_ints[i]) continue;
UPD_PARAM_READ(param_read_int,upd_ints[i],value);
if(0 == code) {
if(0 <= error) error |= UPD_PUT_INTS;
ints[i] = value;
}
}
for(i = 0; countof(upd_int_a) > i; ++i) {
gs_param_int_array value = int_a[i];
if(!upd_int_a[i]) continue;
UPD_PARAM_READ(param_read_int_array,upd_int_a[i],value);
if(0 == code) {
if(0 <= error) error |= UPD_PUT_INT_A;
UPD_MM_DEL_PARAM(udev->memory, int_a[i]);
if(!value.size) {
value.data = NULL;
int_a[i]   = value;
} else {
UPD_MM_CPY_PARAM(udev->memory, int_a[i],value);
}
}
}
for(i = 0; countof(upd_strings) > i; ++i) {
gs_param_string value = strings[i];
if(!upd_strings[i]) continue;
UPD_PARAM_READ(param_read_string,upd_strings[i],value);
if(0 == code) {
if(0 <= error) error |= UPD_PUT_STRINGS;
UPD_MM_DEL_PARAM(udev->memory, strings[i]);
if(!value.size) {
value.data = NULL;
strings[i]   = value;
} else {
UPD_MM_CPY_PARAM(udev->memory, strings[i],value);
}
}
}
for(i = 0; countof(upd_string_a) > i; ++i) {
gs_param_string_array value = string_a[i];
if(!upd_string_a[i]) continue;
UPD_PARAM_READ(param_read_string_array,upd_string_a[i],value);
if(0 == code) {
if(0 <= error) error |= UPD_PUT_STRING_A;
UPD_MM_DEL_APARAM(udev->memory, string_a[i]);
if(!value.size) {
value.data  = NULL;
string_a[i] = value;
} else {
UPD_MM_CPY_APARAM(udev->memory, string_a[i],value);
}
}
}
for(i = 0; countof(upd_float_a) > i; ++i) {
gs_param_float_array value = float_a[i];
if(!upd_float_a[i]) continue;
UPD_PARAM_READ(param_read_float_array,upd_float_a[i],value);
if(0 == code) {
if(0 <= error) error |= UPD_PUT_FLOAT_A;
UPD_MM_DEL_PARAM(udev->memory, float_a[i]);
if(!value.size) {
value.data = NULL;
float_a[i] = value;
} else {
UPD_MM_CPY_PARAM(udev->memory, float_a[i],value);
}
}
}
if(0 < error) {
int *ip,*ip2,ncomp,nbits;
if(6 > int_a[IA_COLOR_INFO].size) {
UPD_MM_DEL_PARAM(udev->memory, int_a[IA_COLOR_INFO]);
UPD_MM_GET_ARRAY(udev->memory, int_a[IA_COLOR_INFO].data,6);
int_a[IA_COLOR_INFO].size = 6;
}
ip = (int *) upd_cast(int_a[IA_COLOR_INFO].data);
if(0 == ip[0]) {
switch(choice[C_MAPPER]) {
case MAP_GRAY:     ip[0] = 1; break;
case MAP_RGBW:     ip[0] = 3; break;
case MAP_RGB:      ip[0] = 3; break;
case MAP_CMYK:     ip[0] = 4; break;
case MAP_CMYKGEN:  ip[0] = 4; break;
case MAP_RGBOV:    ip[0] = 3; break;
case MAP_RGBNOV:   ip[0] = 3; break;
default:           ip[0] = color_info.num_components; break;
}
}
switch(choice[C_MAPPER]) {
case MAP_GRAY:     ncomp = 1; break;
case MAP_RGBW:     ncomp = 4; break;
case MAP_RGB:      ncomp = 3; break;
case MAP_CMYK:     ncomp = 4; break;
case MAP_CMYKGEN:  ncomp = 4; break;
case MAP_RGBOV:    ncomp = 4; break;
case MAP_RGBNOV:   ncomp = 4; break;
default:           ncomp = ip[0]; break;
}
if(UPD_CMAP_MAX < ncomp) ncomp = UPD_CMAP_MAX;
if(ncomp > int_a[IA_COMPBITS].size) {
UPD_MM_GET_ARRAY(udev->memory, ip2,ncomp);
nbits = 32 / ncomp;
if(8 < nbits) nbits = 8;
for(i = 0; i < ncomp; ++i) ip2[i] = nbits;
UPD_MM_DEL_PARAM(udev->memory, int_a[IA_COMPBITS]);
int_a[IA_COMPBITS].data = ip2;
int_a[IA_COMPBITS].size = ncomp;
}
if(ncomp > int_a[IA_COMPSHIFT].size) {
nbits = 0;
for(i = 0; i < ncomp; ++i) nbits += int_a[IA_COMPBITS].data[i];
UPD_MM_GET_ARRAY(udev->memory, ip2,ncomp);
for(i = 0; i < ncomp; ++i) {
ip2[i] = nbits - int_a[IA_COMPBITS].data[i];
nbits -= int_a[IA_COMPBITS].data[i];
}
UPD_MM_DEL_PARAM(udev->memory, int_a[IA_COMPSHIFT]);
int_a[IA_COMPSHIFT].data = ip2;
int_a[IA_COMPSHIFT].size = ncomp;
}
if(0 == ip[1]) {
nbits = 0;
for(i = 0; i < ncomp; ++i) {
if(nbits < (int_a[IA_COMPBITS].data[i] +
int_a[IA_COMPSHIFT].data[i]))
nbits =  int_a[IA_COMPBITS].data[i] +
int_a[IA_COMPSHIFT].data[i];
}
if(      1 >= nbits) nbits =  1;
else if( 2 >= nbits) nbits =  2;
else if( 4 >= nbits) nbits =  4;
else if( 8 >= nbits) nbits =  8;
else if(16 >= nbits) nbits = 16;
else if(24 >= nbits) nbits = 24;
else                 nbits = 32;
ip[1] = nbits;
}
if(0 == ip[2]) {
nbits = 0;
for(i = 0; i < ncomp; ++i) if(nbits < int_a[IA_COMPBITS].data[i])
nbits = int_a[IA_COMPBITS].data[i];
if(nbits > 8) nbits = 8;
ip[2] = (1 << nbits) - 1;
}
if(0 == ip[3] && 1 < ip[0]) {
nbits = 0;
for(i = 0; i < ip[0]; ++i) nbits += int_a[IA_COMPBITS].data[i];
if(nbits > 8) nbits = 8;
ip[3] = (1 << nbits) - 1;
}
if(0 == ip[4]) {
nbits = 0;
for(i = 0; i < ncomp; ++i) if(nbits < int_a[IA_COMPBITS].data[i])
nbits = int_a[IA_COMPBITS].data[i];
if(2 < nbits) ip[4] = 256;
else          ip[4] = 2;
}
if(0 == ip[5] && 1 < ip[0]) {
nbits = 0;
for(i = 0; i < ncomp; ++i) if(nbits < int_a[IA_COMPBITS].data[i])
nbits = int_a[IA_COMPBITS].data[i];
if(2 < nbits) ip[5] = 256;
else          ip[5] = 2;
}
udev->color_info.num_components = ip[0];
udev->color_info.depth          = ip[1];
udev->color_info.max_gray       = (gx_color_value) ip[2];
udev->color_info.max_color      = (gx_color_value) ip[3];
udev->color_info.dither_grays   = (gx_color_value) ip[4];
udev->color_info.dither_colors  = (gx_color_value) ip[5];
if((0 == param_read_float_array(plist,"HWResolution",&mfa)) &&
(2 == mfa.size) && (0 != mfa.data)) {
udev->MarginsHWResolution[0] = mfa.data[0];
udev->MarginsHWResolution[1] = mfa.data[1];
} else {
udev->MarginsHWResolution[0] = udev->HWResolution[0];
udev->MarginsHWResolution[1] = udev->HWResolution[1];
}
if((0 == param_read_float_array(plist,".HWMargins",&mfa)) &&
(4 == mfa.size) && (0 != mfa.data)) {
udev->Margins[0] = -mfa.data[0] * udev->MarginsHWResolution[0] / 72.0;
udev->Margins[1] = -mfa.data[3] * udev->MarginsHWResolution[1] / 72.0;
}
}
code = gdev_prn_put_params((gx_device *)udev,plist);
if(0 > code) error = code;
if( udev->upd &&
((udev->width  != udev->upd->pdwidth) ||
(udev->height != udev->upd->pdheight)  ))
error |= UPD_PUT_CHANGEDSIZE;
if(0 < error && udev->is_open) {
code = gs_closedevice((gx_device *)udev);
if(0 > code) error = code;
}
if(0 < error) {
if(!(upd = udev->upd)) {
UPD_MM_GET_ARRAY(udev->memory, udev->upd,1);
upd = udev->upd;
} else {
UPD_MM_DEL_ARRAY(udev->memory, upd->choice,  countof(upd_choice),  UPD_MM_DEL_VALUE);
UPD_MM_DEL_ARRAY(udev->memory, upd->ints,    countof(upd_ints),    UPD_MM_DEL_VALUE);
UPD_MM_DEL_ARRAY(udev->memory, upd->int_a,   countof(upd_int_a),   UPD_MM_DEL_PARAM);
UPD_MM_DEL_ARRAY(udev->memory, upd->strings, countof(upd_strings), UPD_MM_DEL_PARAM);
UPD_MM_DEL_ARRAY(udev->memory, upd->string_a,countof(upd_string_a),UPD_MM_DEL_APARAM);
UPD_MM_DEL_ARRAY(udev->memory, upd->float_a, countof(upd_float_a), UPD_MM_DEL_PARAM);
}
upd->choice   = choice;
upd->flags    = flags;
upd->ints     = ints;
upd->int_a    = int_a;
upd->strings  = strings;
upd->string_a = string_a;
upd->float_a  = float_a;
if(0 < error) error = 0;
} else {
udev->Margins[0] =             Margins[0];
udev->Margins[1] =             Margins[1];
udev->MarginsHWResolution[0] = MarginsHWResolution[0];
udev->MarginsHWResolution[1] = MarginsHWResolution[1];
udev->color_info = color_info;
UPD_MM_DEL_ARRAY(udev->memory, choice,  countof(upd_choice),  UPD_MM_DEL_VALUE);
UPD_MM_DEL_ARRAY(udev->memory, ints,    countof(upd_ints),    UPD_MM_DEL_VALUE);
UPD_MM_DEL_ARRAY(udev->memory, int_a,   countof(upd_int_a),   UPD_MM_DEL_PARAM);
UPD_MM_DEL_ARRAY(udev->memory, strings, countof(upd_strings), UPD_MM_DEL_PARAM);
UPD_MM_DEL_ARRAY(udev->memory, string_a,countof(upd_string_a),UPD_MM_DEL_APARAM);
UPD_MM_DEL_ARRAY(udev->memory, float_a, countof(upd_float_a), UPD_MM_DEL_PARAM);
}
upd_procs_map(udev);
#if UPD_MESSAGES & UPD_M_TOPCALLS
errprintf("RETURN: %d = upd_put_params(0x%05lx,0x%05lx)\n",
error,(long) udev, (long) plist);
#endif
return error;
}
private gx_color_index
upd_cmyk_icolor(gx_device *pdev, const gx_color_value cv[])
{
const upd_p     upd = ((upd_device *)pdev)->upd;
gx_color_index  rv;
gx_color_value c, m, y, k;
c = cv[0]; m = cv[1]; y = cv[2]; k = cv[3];
if((c == m) && (m == y)) {
rv = upd_truncate(upd,0,(gx_color_value)(c > k ? c : k));
} else {
rv  = upd_truncate(upd,0,k) | upd_truncate(upd,1,c)
| upd_truncate(upd,2,m) | upd_truncate(upd,3,y);
if(rv == gx_no_color_index) rv ^= 1;
}
#if UPD_MESSAGES & UPD_M_MAPCALLS
errprintf(
"cmyk_icolor: (%5.1f,%5.1f,%5.1f,%5.1f) : (%5.1f,%5.1f,%5.1f,%5.1f) : 0x%0*lx\n",
255.0 * (double) c / (double) gx_max_color_value,
255.0 * (double) m / (double) gx_max_color_value,
255.0 * (double) y / (double) gx_max_color_value,
255.0 * (double) k / (double) gx_max_color_value,
255.0 * (double) ((rv >> upd->cmap[1].bitshf) & upd->cmap[1].bitmsk)
/ (double) upd->cmap[1].bitmsk,
255.0 * (double) ((rv >> upd->cmap[2].bitshf) & upd->cmap[2].bitmsk)
/ (double) upd->cmap[2].bitmsk,
255.0 * (double) ((rv >> upd->cmap[3].bitshf) & upd->cmap[3].bitmsk)
/ (double) upd->cmap[3].bitmsk,
255.0 * (double) ((rv >> upd->cmap[0].bitshf) & upd->cmap[0].bitmsk)
/ (double) upd->cmap[0].bitmsk,
(pdev->color_info.depth + 3)>>2,rv);
#endif
return rv;
}
private int
upd_icolor_rgb(gx_device *pdev, gx_color_index color, gx_color_value prgb[3])
{
const upd_p     upd = ((upd_device *)pdev)->upd;
gx_color_value c,m,y,k;
k = upd_expand(upd,0,color);
c = upd_expand(upd,1,color);
m = upd_expand(upd,2,color);
y = upd_expand(upd,3,color);
prgb[0] = gx_max_color_value - c;
if(prgb[0] > k) prgb[0] -= k;
else            prgb[0]  = 0;
prgb[1] = gx_max_color_value - m;
if(prgb[1] > k) prgb[1] -= k;
else            prgb[1]  = 0;
prgb[2] = gx_max_color_value - y;
if(prgb[2] > k) prgb[2] -= k;
else            prgb[2]  = 0;
#if UPD_MESSAGES & UPD_M_MAPCALLS
errprintf(
"icolor_rgb: 0x%0*lx -> (%5.1f,%5.1f,%5.1f,%5.1f) -> (%5.1f,%5.1f,%5.1f,%5.1f) -> (%5.1f,%5.1f,%5.1f)\n",
(pdev->color_info.depth + 3)>>2,color,
255.0 * (double) ((color >> upd->cmap[1].bitshf) & upd->cmap[1].bitmsk)
/ (double) upd->cmap[1].bitmsk,
255.0 * (double) ((color >> upd->cmap[2].bitshf) & upd->cmap[2].bitmsk)
/ (double) upd->cmap[2].bitmsk,
255.0 * (double) ((color >> upd->cmap[3].bitshf) & upd->cmap[3].bitmsk)
/ (double) upd->cmap[3].bitmsk,
255.0 * (double) ((color >> upd->cmap[0].bitshf) & upd->cmap[0].bitmsk)
/ (double) upd->cmap[0].bitmsk,
255.0 * (double)   c     / (double) gx_max_color_value,
255.0 * (double)   m     / (double) gx_max_color_value,
255.0 * (double)   y     / (double) gx_max_color_value,
255.0 * (double)   k     / (double) gx_max_color_value,
255.0 * (double) prgb[0] / (double) gx_max_color_value,
255.0 * (double) prgb[1] / (double) gx_max_color_value,
255.0 * (double) prgb[2] / (double) gx_max_color_value);
#endif
return 0;
}
private gx_color_index
upd_rgb_1color(gx_device *pdev, const gx_color_value cv[])
{
const upd_p     upd = ((upd_device *)pdev)->upd;
gx_color_index  rv;
gx_color_value g;
g = cv[0];
rv = upd_truncate(upd,0,g);
#if UPD_MESSAGES & UPD_M_MAPCALLS
errprintf(
"rgb_1color: (%5.1f) : (%5.1f) : 0x%0*lx\n",
255.0 * (double) g  / (double) gx_max_color_value,
255.0 * (double) ((rv >> upd->cmap[0].bitshf) & upd->cmap[0].bitmsk)
/ (double) upd->cmap[0].bitmsk,
(pdev->color_info.depth + 3)>>2,rv);
#endif
return rv;
}
private int
upd_1color_rgb(gx_device *pdev, gx_color_index color, gx_color_value cv[1])
{
const upd_p     upd = ((upd_device *)pdev)->upd;
cv[0] = upd_expand(upd,0,color);
#if UPD_MESSAGES & UPD_M_MAPCALLS
errprintf("1color_rgb: 0x%0*lx -> %5.1f -> (%5.1f,%5.1f,%5.1f)\n",
(pdev->color_info.depth + 3)>>2,color,
255.0 * (double) ((color >> upd->cmap[0].bitshf) & upd->cmap[0].bitmsk)
/ (double) upd->cmap[0].bitmsk,
255.0 * (double) prgb[0] / (double) gx_max_color_value,
255.0 * (double) prgb[0] / (double) gx_max_color_value,
255.0 * (double) prgb[0] / (double) gx_max_color_value);
#endif
return 0;
}
private gx_color_index
upd_rgb_3color(gx_device *pdev, const gx_color_value cv[])
{
const upd_p     upd = ((upd_device *)pdev)->upd;
gx_color_index  rv;
gx_color_value r, g, b;
r = cv[0]; g = cv[1]; b = cv[2];
rv = upd_truncate(upd,0,r) | upd_truncate(upd,1,g) | upd_truncate(upd,2,b);
if(rv == gx_no_color_index) rv ^= 1;
#if UPD_MESSAGES & UPD_M_MAPCALLS
errprintf(
"rgb_3color: (%5.1f,%5.1f,%5.1f) : (%5.1f,%5.1f,%5.1f) : 0x%0*lx\n",
255.0 * (double) r / (double) gx_max_color_value,
255.0 * (double) g / (double) gx_max_color_value,
255.0 * (double) b / (double) gx_max_color_value,
255.0 * (double) ((rv >> upd->cmap[0].bitshf) & upd->cmap[0].bitmsk)
/ (double) upd->cmap[0].bitmsk,
255.0 * (double) ((rv >> upd->cmap[1].bitshf) & upd->cmap[1].bitmsk)
/ (double) upd->cmap[1].bitmsk,
255.0 * (double) ((rv >> upd->cmap[2].bitshf) & upd->cmap[2].bitmsk)
/ (double) upd->cmap[2].bitmsk,
(pdev->color_info.depth + 3)>>2,rv);
#endif
return rv;
}
private int
upd_3color_rgb(gx_device *pdev, gx_color_index color, gx_color_value prgb[3])
{
const upd_p     upd = ((upd_device *)pdev)->upd;
prgb[0] = upd_expand(upd,0,color);
prgb[1] = upd_expand(upd,1,color);
prgb[2] = upd_expand(upd,2,color);
#if UPD_MESSAGES & UPD_M_MAPCALLS
errprintf(
"3color_rgb: 0x%0*lx -> (%5.1f,%5.1f,%5.1f) -> (%5.1f,%5.1f,%5.1f)\n",
(pdev->color_info.depth + 3)>>2,color,
255.0 * (double) ((color >> upd->cmap[0].bitshf) & upd->cmap[0].bitmsk)
/ (double) upd->cmap[0].bitmsk,
255.0 * (double) ((color >> upd->cmap[1].bitshf) & upd->cmap[1].bitmsk)
/ (double) upd->cmap[1].bitmsk,
255.0 * (double) ((color >> upd->cmap[2].bitshf) & upd->cmap[2].bitmsk)
/ (double) upd->cmap[2].bitmsk,
255.0 * (double) prgb[0] / (double) gx_max_color_value,
255.0 * (double) prgb[1] / (double) gx_max_color_value,
255.0 * (double) prgb[2] / (double) gx_max_color_value);
#endif
return 0;
}
private gx_color_index
upd_rgb_4color(gx_device *pdev, const gx_color_value cv[])
{
const upd_p     upd = ((upd_device *)pdev)->upd;
gx_color_index  rv;
gx_color_value r, g, b;
r = cv[0]; g = cv[1]; b = cv[2];
if((r == g) && (g == b)) {
rv = upd_truncate(upd,0,r);
} else {
gx_color_value w = g < r ? g : r; w = w < b ? w : b;
rv = upd_truncate(upd,0,w) | upd_truncate(upd,1,r) |
upd_truncate(upd,2,g) | upd_truncate(upd,3,b);
if(rv == gx_no_color_index) rv ^= 1;
}
#if UPD_MESSAGES & UPD_M_MAPCALLS
errprintf(
"rgb_4color: (%5.1f,%5.1f,%5.1f) : (%5.1f,%5.1f,%5.1f,%5.1f) : 0x%0*lx\n",
255.0 * (double) r / (double) gx_max_color_value,
255.0 * (double) g / (double) gx_max_color_value,
255.0 * (double) b / (double) gx_max_color_value,
255.0 * (double) ((rv >> upd->cmap[1].bitshf) & upd->cmap[1].bitmsk)
/ (double) upd->cmap[1].bitmsk,
255.0 * (double) ((rv >> upd->cmap[2].bitshf) & upd->cmap[2].bitmsk)
/ (double) upd->cmap[2].bitmsk,
255.0 * (double) ((rv >> upd->cmap[3].bitshf) & upd->cmap[3].bitmsk)
/ (double) upd->cmap[3].bitmsk,
255.0 * (double) ((rv >> upd->cmap[0].bitshf) & upd->cmap[0].bitmsk)
/ (double) upd->cmap[0].bitmsk,
(pdev->color_info.depth + 3)>>2,rv);
#endif
return rv;
}
private int
upd_4color_rgb(gx_device *pdev, gx_color_index color, gx_color_value prgb[3])
{
const upd_p     upd = ((upd_device *)pdev)->upd;
prgb[0] = upd_expand(upd,1,color);
prgb[1] = upd_expand(upd,2,color);
prgb[2] = upd_expand(upd,3,color);
if(!(prgb[0] || prgb[1] || prgb[2]))
prgb[0] = prgb[1] = prgb[2] = upd_expand(upd,0,color);
#if UPD_MESSAGES & UPD_M_MAPCALLS
errprintf(
"4color_rgb: 0x%0*lx -> (%5.1f,%5.1f,%5.1f,%5.1f) -> (%5.1f,%5.1f,%5.1f)\n",
(pdev->color_info.depth + 3)>>2,color,
255.0 * (double) ((color >> upd->cmap[1].bitshf) & upd->cmap[1].bitmsk)
/ (double) upd->cmap[1].bitmsk,
255.0 * (double) ((color >> upd->cmap[2].bitshf) & upd->cmap[2].bitmsk)
/ (double) upd->cmap[2].bitmsk,
255.0 * (double) ((color >> upd->cmap[3].bitshf) & upd->cmap[3].bitmsk)
/ (double) upd->cmap[3].bitmsk,
255.0 * (double) ((color >> upd->cmap[0].bitshf) & upd->cmap[0].bitmsk)
/ (double) upd->cmap[0].bitmsk,
255.0 * (double) prgb[0] / (double) gx_max_color_value,
255.0 * (double) prgb[1] / (double) gx_max_color_value,
255.0 * (double) prgb[2] / (double) gx_max_color_value);
#endif
return 0;
}
private gx_color_index
upd_cmyk_kcolor(gx_device *pdev, const gx_color_value cv[])
{
const upd_p     upd = ((upd_device *)pdev)->upd;
gx_color_index  rv;
gx_color_value  black;
gx_color_value c, m, y, k;
c = cv[0]; m = cv[1]; y = cv[2]; k = cv[3];
if((c == m) && (m == y)) {
black = c > k ? c : k;
rv = upd_truncate(upd,0,black);
} else {
if(k && !(c | m | y)) {
black = k;
} else {
black = c     < m ? c     : m;
black = black < y ? black : y;
}
rv  = upd_truncate(upd,0,black) | upd_truncate(upd,1,c)
| upd_truncate(upd,2,m)     | upd_truncate(upd,3,y);
if(rv == gx_no_color_index) rv ^= 1;
}
#if UPD_MESSAGES & UPD_M_MAPCALLS
errprintf(
"cmyk_kcolor: (%5.1f,%5.1f,%5.1f,%5.1f) : (%5.1f,%5.1f,%5.1f,%5.1f) : 0x%0*lx\n",
255.0 * (double) c / (double) gx_max_color_value,
255.0 * (double) m / (double) gx_max_color_value,
255.0 * (double) y / (double) gx_max_color_value,
255.0 * (double) k / (double) gx_max_color_value,
255.0 * (double) ((rv >> upd->cmap[1].bitshf) & upd->cmap[1].bitmsk)
/ (double) upd->cmap[1].bitmsk,
255.0 * (double) ((rv >> upd->cmap[2].bitshf) & upd->cmap[2].bitmsk)
/ (double) upd->cmap[2].bitmsk,
255.0 * (double) ((rv >> upd->cmap[3].bitshf) & upd->cmap[3].bitmsk)
/ (double) upd->cmap[3].bitmsk,
255.0 * (double) ((rv >> upd->cmap[0].bitshf) & upd->cmap[0].bitmsk)
/ (double) upd->cmap[0].bitmsk,
(pdev->color_info.depth + 3)>>2,rv);
#endif
return rv;
}
private int
upd_kcolor_rgb(gx_device *pdev, gx_color_index color, gx_color_value prgb[3])
{
const upd_p     upd = ((upd_device *)pdev)->upd;
gx_color_value c,m,y,k;
k = upd_expand(upd,0,color);
c = upd_expand(upd,1,color);
m = upd_expand(upd,2,color);
y = upd_expand(upd,3,color);
if(!(c | m | y )) {
prgb[2] = prgb[1] = prgb[0] = gx_max_color_value - k;
} else {
prgb[0] = gx_max_color_value - c;
prgb[1] = gx_max_color_value - m;
prgb[2] = gx_max_color_value - y;
}
#if UPD_MESSAGES & UPD_M_MAPCALLS
errprintf(
"kcolor_rgb: 0x%0*lx -> (%5.1f,%5.1f,%5.1f,%5.1f) -> (%5.1f,%5.1f,%5.1f,%5.1f) -> (%5.1f,%5.1f,%5.1f)\n",
(pdev->color_info.depth + 3)>>2,color,
255.0 * (double) ((color >> upd->cmap[1].bitshf) & upd->cmap[1].bitmsk)
/ (double) upd->cmap[1].bitmsk,
255.0 * (double) ((color >> upd->cmap[2].bitshf) & upd->cmap[2].bitmsk)
/ (double) upd->cmap[2].bitmsk,
255.0 * (double) ((color >> upd->cmap[3].bitshf) & upd->cmap[3].bitmsk)
/ (double) upd->cmap[3].bitmsk,
255.0 * (double) ((color >> upd->cmap[0].bitshf) & upd->cmap[0].bitmsk)
/ (double) upd->cmap[0].bitmsk,
255.0 * (double)   c     / (double) gx_max_color_value,
255.0 * (double)   m     / (double) gx_max_color_value,
255.0 * (double)   y     / (double) gx_max_color_value,
255.0 * (double)   k     / (double) gx_max_color_value,
255.0 * (double) prgb[0] / (double) gx_max_color_value,
255.0 * (double) prgb[1] / (double) gx_max_color_value,
255.0 * (double) prgb[2] / (double) gx_max_color_value);
#endif
return 0;
}
private gx_color_index
upd_rgb_ovcolor(gx_device *pdev, const gx_color_value cv[])
{
const upd_p     upd = ((upd_device *)pdev)->upd;
gx_color_index  rv;
gx_color_value  c,m,y,black;
gx_color_value r, g, b;
r = cv[0]; g = cv[1]; b = cv[2];
if((r == g) && (g == b)) {
black  = gx_max_color_value - r;
rv     = upd_truncate(upd,0,black);
c = m = y = 0;
} else {
c = gx_max_color_value - r;
m = gx_max_color_value - g;
y = gx_max_color_value - b;
black = c     < m ? c     : m;
black = black < y ? black : y;
if(black != gx_max_color_value) {
float tmp,d;
d   = (float)(gx_max_color_value - black);
tmp = (float) (c-black) / d;
if(      0.0 > tmp) tmp = 0.0;
else if( 1.0 < tmp) tmp = 1.0;
c   = (gx_color_value)(tmp * gx_max_color_value + 0.499);
tmp = (float) (m-black) / d;
if(      0.0 > tmp) tmp = 0.0;
else if( 1.0 < tmp) tmp = 1.0;
m   = (gx_color_value)(tmp * gx_max_color_value + 0.499);
tmp = (float) (y-black) / d;
if(      0.0 > tmp) tmp = 0.0;
else if( 1.0 < tmp) tmp = 1.0;
y   = (gx_color_value)(tmp * gx_max_color_value + 0.499);
} else {
c = m = y = gx_max_color_value;
}
rv = upd_truncate(upd,0,black) | upd_truncate(upd,1,c) |
upd_truncate(upd,2,m)     | upd_truncate(upd,3,y);
if(rv == gx_no_color_index) rv ^= 1;
}
#if UPD_MESSAGES & UPD_M_MAPCALLS
errprintf(
"rgb_ovcolor: (%5.1f,%5.1f,%5.1f) : (%5.1f,%5.1f,%5.1f,%5.1f) : 0x%0*lx\n",
255.0 * (double) r / (double) gx_max_color_value,
255.0 * (double) g / (double) gx_max_color_value,
255.0 * (double) b / (double) gx_max_color_value,
255.0 * (double) ((rv >> upd->cmap[1].bitshf) & upd->cmap[1].bitmsk)
/ (double) upd->cmap[1].bitmsk,
255.0 * (double) ((rv >> upd->cmap[2].bitshf) & upd->cmap[2].bitmsk)
/ (double) upd->cmap[2].bitmsk,
255.0 * (double) ((rv >> upd->cmap[3].bitshf) & upd->cmap[3].bitmsk)
/ (double) upd->cmap[3].bitmsk,
255.0 * (double) ((rv >> upd->cmap[0].bitshf) & upd->cmap[0].bitmsk)
/ (double) upd->cmap[0].bitmsk,
(pdev->color_info.depth + 3)>>2,rv);
#endif
return rv;
}
private gx_color_index
upd_rgb_novcolor(gx_device *pdev, const gx_color_value cv[])
{
const upd_p     upd = ((upd_device *)pdev)->upd;
gx_color_index  rv;
gx_color_value  c,m,y,black;
gx_color_value r, g, b;
r = cv[0]; g = cv[1]; b = cv[2];
if((r == g) && (g == b)) {
black  = gx_max_color_value - r;
rv     = upd_truncate(upd,0,black);
c = m = y = 0;
} else {
c = gx_max_color_value - r;
m = gx_max_color_value - g;
y = gx_max_color_value - b;
black = c     < m ? c     : m;
black = black < y ? black : y;
c     = c - black;
m     = m - black;
y     = y - black;
rv = upd_truncate(upd,0,black) | upd_truncate(upd,1,c) |
upd_truncate(upd,2,m)     | upd_truncate(upd,3,y);
if(rv == gx_no_color_index) rv ^= 1;
}
#if UPD_MESSAGES & UPD_M_MAPCALLS
errprintf(
"rgb_ovcolor: (%5.1f,%5.1f,%5.1f) : (%5.1f,%5.1f,%5.1f,%5.1f) : 0x%0*lx\n",
255.0 * (double) r / (double) gx_max_color_value,
255.0 * (double) g / (double) gx_max_color_value,
255.0 * (double) b / (double) gx_max_color_value,
255.0 * (double) ((rv >> upd->cmap[1].bitshf) & upd->cmap[1].bitmsk)
/ (double) upd->cmap[1].bitmsk,
255.0 * (double) ((rv >> upd->cmap[2].bitshf) & upd->cmap[2].bitmsk)
/ (double) upd->cmap[2].bitmsk,
255.0 * (double) ((rv >> upd->cmap[3].bitshf) & upd->cmap[3].bitmsk)
/ (double) upd->cmap[3].bitmsk,
255.0 * (double) ((rv >> upd->cmap[0].bitshf) & upd->cmap[0].bitmsk)
/ (double) upd->cmap[0].bitmsk,
(pdev->color_info.depth + 3)>>2,rv);
#endif
return rv;
}
private uint32_t
upd_truncate(upd_pc upd,int i,gx_color_value v) {
const updcmap_pc cmap = upd->cmap + i;
int32_t           s;
gx_color_value *p;
if(0 == cmap->bits) {
v = 0;
} else if(gx_color_value_bits > cmap->bits) {
p = cmap->code + ((cmap->bitmsk + 1) >> 1);
s =              ((cmap->bitmsk + 1) >> 2);
while(s > 0) {
if(v > *p) {
p += s;
} else if(v < p[-1]) {
p -= s;
} else {
if((v-p[-1]) < (p[0]-v)) p -= 1;
break;
}
s >>= 1;
}
if((v-p[-1]) < (p[0]-v)) p -= 1;
v = p - cmap->code;
}
if(!cmap->rise) v = cmap->bitmsk - v;
return ((uint32_t) v) << cmap->bitshf;
}
private int
upd_open_map(upd_device *udev)
{
const upd_p      upd   = udev->upd;
int imap;
for(imap = 0; UPD_CMAP_MAX > imap; ++imap) upd->cmap[imap].code   = NULL;
upd->ncomp = 0;
if(B_ERROR & upd->flags)    imap = 0;
if(imap) {
for(imap = 0; UPD_CMAP_MAX > imap; ++imap) {
upd->cmap[imap].xfer = -1;
upd->cmap[imap].bits =  0;
}
switch(upd->choice[C_MAPPER]) {
case MAP_GRAY:
upd->cmap[0].xfer = FA_WXFER;
break;
case MAP_RGBW:
upd->cmap[0].xfer = FA_WXFER;
upd->cmap[1].xfer = FA_RXFER;
upd->cmap[2].xfer = FA_GXFER;
upd->cmap[3].xfer = FA_BXFER;
break;
case MAP_RGB:
upd->cmap[0].xfer = FA_RXFER;
upd->cmap[1].xfer = FA_GXFER;
upd->cmap[2].xfer = FA_BXFER;
break;
case MAP_CMYK:
upd->cmap[0].xfer = FA_KXFER;
upd->cmap[1].xfer = FA_CXFER;
upd->cmap[2].xfer = FA_MXFER;
upd->cmap[3].xfer = FA_YXFER;
break;
case MAP_CMYKGEN:
upd->cmap[0].xfer = FA_KXFER;
upd->cmap[1].xfer = FA_CXFER;
upd->cmap[2].xfer = FA_MXFER;
upd->cmap[3].xfer = FA_YXFER;
break;
case MAP_RGBOV:
upd->cmap[0].xfer = FA_KXFER;
upd->cmap[1].xfer = FA_CXFER;
upd->cmap[2].xfer = FA_MXFER;
upd->cmap[3].xfer = FA_YXFER;
break;
case MAP_RGBNOV:
upd->cmap[0].xfer = FA_KXFER;
upd->cmap[1].xfer = FA_CXFER;
upd->cmap[2].xfer = FA_MXFER;
upd->cmap[3].xfer = FA_YXFER;
break;
default:
#if         UPD_MESSAGES & UPD_M_WARNING
if(upd_choice[C_MAPPER][0])
errprintf(
"upd_open_map: unsupported %s=%d\n",
upd_choice[C_MAPPER][0],upd->choice[C_MAPPER]);
else
errprintf(
"upd_open_map: unsupported choce[%d]=%d\n",
C_MAPPER,upd->choice[C_MAPPER]);
#endif
imap = 0;
break;
}
}
if(imap) {
#if      UPD_MESSAGES & UPD_M_WARNING
uint32_t used = 0,bitmsk;
#endif
bool success = true;
for(imap = 0; UPD_CMAP_MAX > imap; ++imap) {
if(0 > upd->cmap[imap].xfer) continue;
if((0                     > upd->int_a[IA_COMPBITS].data[imap])  ||
(gx_color_value_bits   < upd->int_a[IA_COMPBITS].data[imap])  ||
(0                     > upd->int_a[IA_COMPSHIFT].data[imap]) ||
(upd->int_a[IA_COMPBITS].data[imap] >
(udev->color_info.depth - upd->int_a[IA_COMPSHIFT].data[imap]))) {
#if         UPD_MESSAGES & UPD_M_WARNING
errprintf(
"upd_open_map: %d Bits << %d is illegal for %d. Component\n",
upd->int_a[IA_COMPBITS].data[imap],
upd->int_a[IA_COMPSHIFT].data[imap],imap+1);
#endif
success = false;
} else {
int         n;
const float *now;
float       last;
if((NULL == upd->float_a[upd->cmap[imap].xfer].data) ||
(2    >  upd->float_a[upd->cmap[imap].xfer].size)   ) {
float *fp;
UPD_MM_DEL_PARAM(udev->memory, upd->float_a[upd->cmap[imap].xfer]);
UPD_MM_GET_ARRAY(udev->memory, fp,2);
fp[0] = 0.0;
fp[1] = 1.0;
upd->float_a[upd->cmap[imap].xfer].data = fp;
upd->float_a[upd->cmap[imap].xfer].size = 2;
}
n    = upd->float_a[upd->cmap[imap].xfer].size-1;
now  = upd->float_a[upd->cmap[imap].xfer].data;
last = now[n];
if(     *now < last) {
last = *now++;
while(n--) {
if(last >= *now) break;
last = *now++;
}
} else if(*now > last) {
last = *now++;
while(n--) {
if(last <= *now) break;
last = *now++;
}
}
if(0 <= n) {
#if            UPD_MESSAGES & UPD_M_WARNING
errprintf(
"upd_open_map: %d. Component has non monoton Xfer\n",imap+1);
#endif
success = false;
} else {
#if            UPD_MESSAGES & UPD_M_WARNING
bitmsk   = ((uint32_t) 1 << upd->int_a[IA_COMPBITS].data[imap]) -1;
bitmsk <<= upd->int_a[IA_COMPSHIFT].data[imap];
if(used & bitmsk) errprintf(
"upd_open_map: %d. Component overlaps with others\n",imap+1);
used |= bitmsk;
#endif
}
}
}
if(!success) imap = 0;
}
if(imap) {
for(imap = 0; UPD_CMAP_MAX > imap; ++imap) {
if(0 > upd->cmap[imap].xfer) continue;
upd->cmap[imap].bits     = upd->int_a[IA_COMPBITS].data[imap];
upd->cmap[imap].bitshf   = upd->int_a[IA_COMPSHIFT].data[imap];
upd->cmap[imap].bitmsk   = 1;
upd->cmap[imap].bitmsk <<= upd->cmap[imap].bits;
upd->cmap[imap].bitmsk  -= 1;
upd->cmap[imap].rise     =
upd->float_a[upd->cmap[imap].xfer].data[0] <
upd->float_a[upd->cmap[imap].xfer].data[
upd->float_a[upd->cmap[imap].xfer].size-1] ?
true : false;
upd->cmap[imap].code     = gs_malloc(udev->memory, upd->cmap[imap].bitmsk+1,
sizeof(upd->cmap[imap].code[0]),"upd/code");
if(!upd->cmap[imap].code) break;
}
if(UPD_CMAP_MAX > imap) {
imap = 0;
#if      UPD_MESSAGES & UPD_M_ERROR
errprintf("upd_open_map: could not allocate code-arrays\n");
#        endif
}
}
if(imap) {
for(imap = 0; UPD_CMAP_MAX > imap; ++imap) {
const updcmap_p cmap = upd->cmap + imap;
uint32_t ly,iy;
float ystep,xstep,fx,fy;
double offset,scale;
#define  XFVAL(I) ((upd->float_a[cmap->xfer].data[I]-offset)*scale)
if(0 > cmap->xfer) continue;
cmap->code[cmap->bitmsk] = gx_max_color_value;
if(!cmap->bits) continue;
offset = upd->float_a[cmap->xfer].data[0];
if(     0.0 > offset) offset = 0.0;
else if(1.0 < offset) offset = 1.0;
scale  = upd->float_a[cmap->xfer].data[upd->float_a[cmap->xfer].size-1];
if(     0.0 > scale ) scale  = 0.0;
else if(1.0 < scale ) scale  = 1.0;
if(scale != offset) scale = 1.0 / (scale - offset);
else                scale = 0.0;
ystep = (float) 1.0 / (float) cmap->bitmsk;
xstep = (float) 1.0 / (float)(upd->float_a[cmap->xfer].size - 1);
iy = 0;
for(ly = 0; ly <= cmap->bitmsk; ++ly) {
fy = ystep * ly;
while(((iy+2) < upd->float_a[cmap->xfer].size) &&
(fy > XFVAL(iy+1))) ++iy;
fx  = iy + (fy - XFVAL(iy))/(XFVAL(iy+1) - XFVAL(iy));
fx *= xstep * gx_max_color_value;
fx  = fx < 0.0 ? 0.0 :
(fx > gx_max_color_value ? gx_max_color_value : fx);
cmap->code[ly] = (gx_color_value)fx;
if((fx - cmap->code[ly]) >= 0.5) cmap->code[ly] += 1;
}
#undef   XFVAL
}
}
if(imap) {
switch(upd->choice[C_MAPPER]) {
case MAP_GRAY:
if(1 > imap) imap = 0;
upd->ncomp = 1;
break;
case MAP_RGBW:
if(4 > imap) imap = 0;
upd->ncomp = 4;
break;
case MAP_RGB:
if(3 > imap) imap = 0;
upd->ncomp = 3;
break;
case MAP_CMYK:
if(4 > imap) imap = 0;
upd->ncomp = 4;
break;
case MAP_CMYKGEN:
if(4 > imap) imap = 0;
upd->ncomp = 4;
break;
case MAP_RGBOV:
if(4 > imap) imap = 0;
upd->ncomp = 4;
break;
case MAP_RGBNOV:
if(4 > imap) imap = 0;
upd->ncomp = 4;
break;
default:
imap = 0;
#if        UPD_MESSAGES & UPD_M_WARNING
errprintf(
"upd_open: Mapping %d unknown\n",upd->choice[C_MAPPER]);
#endif
break;
}
}
if(!imap) {
upd_close_map(udev);
} else {
upd->flags |= B_MAP;
upd_procs_map(udev);
}
return imap ? 1 : -1;
}
private int
upd_procs_map(upd_device *udev)
{
int imap;
if( udev->upd &&
(udev->upd->flags & B_MAP)) imap = udev->upd->choice[C_MAPPER];
else                           imap = 0;
switch(imap) {
case MAP_GRAY:
set_dev_proc(udev,encode_color, upd_rgb_1color);
set_dev_proc(udev,decode_color, upd_1color_rgb);
set_dev_proc(udev,map_rgb_color, upd_rgb_1color);
set_dev_proc(udev,map_cmyk_color,gx_default_map_cmyk_color);
set_dev_proc(udev,map_color_rgb, upd_1color_rgb);
break;
case MAP_RGBW:
set_dev_proc(udev,encode_color, upd_rgb_4color);
set_dev_proc(udev,decode_color, upd_4color_rgb);
set_dev_proc(udev,map_rgb_color, upd_rgb_4color);
set_dev_proc(udev,map_cmyk_color,gx_default_map_cmyk_color);
set_dev_proc(udev,map_color_rgb, upd_4color_rgb);
break;
case MAP_RGB:
set_dev_proc(udev,encode_color, upd_rgb_3color);
set_dev_proc(udev,decode_color, upd_3color_rgb);
set_dev_proc(udev,map_rgb_color, upd_rgb_3color);
set_dev_proc(udev,map_cmyk_color,gx_default_map_cmyk_color);
set_dev_proc(udev,map_color_rgb, upd_3color_rgb);
break;
case MAP_CMYK:
set_dev_proc(udev,encode_color, upd_cmyk_icolor);
set_dev_proc(udev,decode_color, upd_icolor_rgb);
set_dev_proc(udev,map_rgb_color, gx_default_map_rgb_color);
set_dev_proc(udev,map_cmyk_color,upd_cmyk_icolor);
set_dev_proc(udev,map_color_rgb, upd_icolor_rgb);
break;
case MAP_CMYKGEN:
set_dev_proc(udev,encode_color, upd_cmyk_kcolor);
set_dev_proc(udev,decode_color, upd_kcolor_rgb);
set_dev_proc(udev,map_rgb_color, gx_default_map_rgb_color);
set_dev_proc(udev,map_cmyk_color,upd_cmyk_kcolor);
set_dev_proc(udev,map_color_rgb, upd_kcolor_rgb);
break;
case MAP_RGBOV:
set_dev_proc(udev,encode_color, upd_rgb_ovcolor);
set_dev_proc(udev,decode_color, upd_ovcolor_rgb);
set_dev_proc(udev,map_rgb_color, upd_rgb_ovcolor);
set_dev_proc(udev,map_cmyk_color,gx_default_map_cmyk_color);
set_dev_proc(udev,map_color_rgb, upd_ovcolor_rgb);
break;
case MAP_RGBNOV:
set_dev_proc(udev,encode_color, upd_rgb_novcolor);
set_dev_proc(udev,decode_color, upd_novcolor_rgb);
set_dev_proc(udev,map_rgb_color, upd_rgb_novcolor);
set_dev_proc(udev,map_cmyk_color,gx_default_map_cmyk_color);
set_dev_proc(udev,map_color_rgb, upd_novcolor_rgb);
break;
default:
set_dev_proc(udev,encode_color, gx_default_map_rgb_color);
set_dev_proc(udev,decode_color, gx_default_map_color_rgb);
set_dev_proc(udev,map_rgb_color, gx_default_map_rgb_color);
set_dev_proc(udev,map_cmyk_color,gx_default_map_cmyk_color);
set_dev_proc(udev,map_color_rgb, gx_default_map_color_rgb);
break;
}
return 0;
}
private int
upd_close_map(upd_device *udev)
{
const upd_p      upd   = udev->upd;
int imap;
if(upd) {
for(imap = 0; UPD_CMAP_MAX > imap; ++imap) {
if(upd->cmap[imap].code)
gs_free(udev->memory, upd->cmap[imap].code,sizeof(upd->cmap[imap].code[0]),
upd->cmap[imap].bitmsk+1,"upd/code");
upd->cmap[imap].code   = NULL;
upd->cmap[imap].bitmsk = 0;
upd->cmap[imap].bitshf = 0;
upd->cmap[imap].bits   = 0;
upd->cmap[imap].rise   = false;
}
upd->flags &= ~B_MAP;
}
upd_procs_map(udev);
return 0;
}
private void
upd_open_render(upd_device *udev)
{
const upd_p upd = udev->upd;
int  icomp;
upd->flags       &= ~B_RENDER;
upd->valbuf       = NULL;
upd->nvalbuf      = 0;
upd->render       = NULL;
upd->start_render = NULL;
for(icomp = 0; UPD_VALPTR_MAX > icomp; ++icomp) upd->valptr[icomp] = NULL;
if( (B_BUF | B_MAP) ==
((B_BUF | B_MAP | B_ERROR) & upd->flags)) {
upd->rwidth = upd->gswidth;
if((0            < upd->ints[I_PWIDTH]) &&
(upd->gswidth > upd->ints[I_PWIDTH])   )
upd->rwidth  = upd->ints[I_PWIDTH];
switch(upd->choice[C_RENDER]) {
case RND_FSCOMP:
upd_open_fscomp(udev);
break;
case RND_FSCMYK:
upd_open_fscmyk(udev);
break;
case RND_FSCMY_K:
upd_open_fscmy_k(udev);
break;
default:
#if UPD_MESSAGES & UPD_M_WARNING
errprintf("upd_open_render: Unknown rendering type %d\n",
upd->choice[C_RENDER]);
#endif
break;
}
}
if(B_RENDER != ((B_ERROR | B_RENDER) & upd->flags))
upd_close_render(udev);
return;
}
private void
upd_close_render(upd_device *udev)
{
const upd_p upd = udev->upd;
if(upd) {
int icomp;
if((upd->render == upd_fscomp) ||
(upd->render == upd_fscmyk)   )  upd_close_fscomp(udev);
if((0 < upd->nvalbuf) && upd->valbuf)
gs_free(udev->memory, upd->valbuf,upd->nvalbuf,sizeof(upd->valbuf[0]),"upd/valbuf");
upd->valbuf  = NULL;
upd->nvalbuf = 0;
upd->flags       &= ~B_RENDER;
upd->render       = NULL;
upd->start_render = NULL;
for(icomp = 0; UPD_VALPTR_MAX > icomp; ++icomp) upd->valptr[icomp] = NULL;
}
return;
}
#if UPD_MESSAGES & UPD_M_FSBUF
static int32_t fs_emin[UPD_VALPTR_MAX],fs_emax[UPD_VALPTR_MAX];
#endif
private void
upd_open_fscomp(upd_device *udev)
{
const upd_p upd = udev->upd;
int icomp,order[UPD_CMAP_MAX];
#if UPD_MESSAGES & UPD_M_FSBUF
for(icomp = 0; UPD_VALPTR_MAX < icomp; ++icomp)
fs_emin[icomp] = fs_emax[icomp] = 0;
#endif
icomp = upd->ncomp;
if((0              >= icomp) ||
(UPD_VALPTR_MAX <  icomp) ||
(UPD_CMAP_MAX   <  icomp)   ) icomp      = 0;
if(icomp) {
if(upd->ncomp <= upd->int_a[IA_COMPORDER].size) {
bool success = true;
for(icomp = 0; upd->ncomp > icomp; ++icomp) {
order[icomp] = upd->int_a[IA_COMPORDER].data[icomp];
if((0            >  order[icomp]) ||
(UPD_CMAP_MAX <= order[icomp])   ) {
success = false;
#if UPD_MESSAGES & UPD_M_WARNING
errprintf(
"upd_open_fscomp: %d is illegal component-index\n",
order[icomp]);
#endif
}
}
if(!success) icomp = 0;
} else {
for(icomp = 0; UPD_CMAP_MAX > icomp; ++icomp) order[icomp] = icomp;
}
}
if(icomp) {
for(icomp = 0; upd->ncomp > icomp; ++icomp) {
upd->valptr[icomp] = gs_malloc(udev->memory, 1,sizeof(updcomp_t),"upd/fscomp");
if(NULL == upd->valptr[icomp]) {
#if UPD_MESSAGES & UPD_M_ERROR
errprintf(
"upd_open_fscomp: could not allocate %d. updcomp\n",
icomp);
#endif
icomp = 0;
break;
}
}
}
if(icomp) {
uint need;
need  = (2 + upd->rwidth) * upd->ncomp;
upd->valbuf = gs_malloc(udev->memory, need,sizeof(upd->valbuf[0]),"upd/valbuf");
if(upd->valbuf) {
upd->nvalbuf = need;
memset(upd->valbuf,0,need*sizeof(upd->valbuf[0]));
} else {
#if UPD_MESSAGES & UPD_M_ERROR
errprintf(
"upd_open_fscomp: could not allocate %u words for valbuf\n",need);
#endif
icomp = 0;
}
}
if(icomp) {
for(icomp = 0; upd->ncomp > icomp; ++icomp) {
const updcomp_p comp   = upd->valptr[icomp];
const int32_t     nsteps = upd->cmap[order[icomp]].bitmsk;
float ymin,ymax;
int32_t highmod,highval;
int i;
comp->threshold = nsteps;
comp->spotsize  = nsteps;
comp->offset    = 0;
comp->scale     = 1;
comp->cmap      = order[icomp];
upd->cmap[comp->cmap].comp = icomp;
comp->bits      = upd->cmap[comp->cmap].bits;
comp->bitshf    = upd->cmap[comp->cmap].bitshf;
comp->bitmsk    = upd->cmap[comp->cmap].bitmsk;
if(!nsteps) continue;
if(upd->cmap[comp->cmap].rise) {
ymin = upd->float_a[upd->cmap[comp->cmap].xfer].data[0];
ymax = upd->float_a[upd->cmap[comp->cmap].xfer].data[
upd->float_a[upd->cmap[comp->cmap].xfer].size-1];
} else {
ymax = upd->float_a[upd->cmap[comp->cmap].xfer].data[0];
ymin = upd->float_a[upd->cmap[comp->cmap].xfer].data[
upd->float_a[upd->cmap[comp->cmap].xfer].size-1];
}
if(0.0 > ymin) {
ymin = 0.0;
if(0.0 > ymax) ymax = 1.0 / (float) (nsteps+1);
}
if(1.0 < ymax) ymax = 1.0;
comp->spotsize = ((int32_t) 1 << 28) - 1;
for(i = 0; i < 32; ++i) {
highval = (int32_t)((ymax-ymin) * (double) comp->spotsize + 0.5);
if(!(highmod = highval % nsteps)) break;
highval += nsteps - highmod;
comp->spotsize = (int32_t)((double) highval / (ymax-ymin) + 0.5);
if(!(comp->spotsize % 2)) comp->spotsize++;
}
comp->offset    = (int32_t)(ymin * (double) comp->spotsize + (double) 0.5);
comp->scale     = highval / nsteps;
comp->threshold = comp->spotsize / 2;
#if UPD_MESSAGES & UPD_M_SETUP
errprintf(
"Values for %d. Component after %d iterations\n",comp->cmap+1,i);
errprintf(
"steps:     %10ld, Bits: %d\n",(long) comp->bitmsk,comp->bits);
errprintf(
"xfer:      %10d Points, %s\n",
upd->float_a[upd->cmap[comp->cmap].xfer].size,
upd->cmap[comp->cmap].rise ? "rising" : "falling");
errprintf(
"offset:    %10d 0x%08x\n",comp->offset,comp->offset);
errprintf(
"scale:     %10d 0x%08x\n",comp->scale,comp->scale);
errprintf(
"threshold: %10d 0x%08x\n",comp->threshold,comp->threshold);
errprintf(
"spotsize:  %10d 0x%08x\n",comp->spotsize,comp->spotsize);
#endif
}
}
if(icomp && !(B_FSZERO & upd->flags)) {
for(icomp = 0; icomp < upd->ncomp; ++icomp) {
const updcomp_p comp = upd->valptr[icomp];
int i;
int32_t lv = INT32_MAX, hv = INT32_MIN, v;
float scale;
for(i = icomp; i < upd->nvalbuf; i += upd->ncomp) {
v = rand();
if(lv > v) lv = v;
if(hv < v) hv = v;
upd->valbuf[i] = v;
}
scale = (float) comp->threshold / (float) (hv - lv);
lv   += (int32_t)(comp->threshold / (2*scale));
for(i = icomp; i < upd->nvalbuf; i += upd->ncomp)
upd->valbuf[i] = (int32_t)(scale * (upd->valbuf[i] - lv));
}
}
upd->render = upd_fscomp;
if(icomp) upd->flags |=  B_RENDER;
else      upd->flags &= ~B_RENDER;
return;
}
private void
upd_close_fscomp(upd_device *udev)
{
const upd_p upd = udev->upd;
int icomp;
#if UPD_MESSAGES & UPD_M_FSBUF
if(upd && (upd->flags & B_RENDER)) {
for(icomp = 0; icomp < upd->ncomp; ++icomp) {
updcomp_p comp = upd->valptr[icomp];
if(!comp) continue;
if(!comp->spotsize) continue;
errprintf("%d. Component: %6.3f <= error <= %6.3f\n",
icomp+1,
(double) fs_emin[icomp] / (double) comp->spotsize,
(double) fs_emax[icomp] / (double) comp->spotsize);
}
}
#endif
for(icomp = 0; UPD_VALPTR_MAX > icomp; ++icomp) {
if(!upd->valptr[icomp]) continue;
gs_free(udev->memory, upd->valptr[icomp],1,sizeof(updcomp_t),"upd/fscomp");
upd->valptr[icomp] = NULL;
}
}
#if   UPD_MESSAGES & UPD_M_FSBUF
#define FS_M_ROWERR(I)                                        \
if(fs_emin[I] > rowerr[I]) fs_emin[I] = rowerr[I]; \
if(fs_emax[I] < rowerr[I]) fs_emax[I] = rowerr[I];
#else
#define FS_M_ROWERR(I) ;
#endif
#define FS_GOAL(Raw,I)                                                     \
pixel[I] = (int32_t)(Raw) * comp[I]->scale +    comp[I]->offset           \
+ rowerr[I]  + colerr[I] -       ((colerr[I]+4)>>3);           \
if(         pixel[I] < 0)                    pixel[I] = 0;              \
else if(    pixel[I] >    comp[I]->spotsize) pixel[I] = comp[I]->spotsize;
#define FS_DIST(I)                                                    \
if(!first) rowerr[I-dir] += ((3*pixel[I]+8)>>4);         \
rowerr[I    ]  = ((5*pixel[I]  )>>4)          \
+ (( colerr[I]+4)>>3);  \
colerr[I    ]  = pixel[I]               \
- ((5*pixel[I]  )>>4)                    \
- ((3*pixel[I]+8)>>4);
#define S_FSTEP                                \
rowerr += dir;                              \
first   = false;                            \
if(0 > dir) {                  \
if(!(bit <<= 1)) { bit = 0x01; ibyte--; }\
} else {                       \
if(!(bit >>= 1)) { bit = 0x80; ibyte++; }\
}
private int
upd_fscomp(upd_p upd)
{
const updscan_p  scan    = upd->scnbuf[upd->yscnbuf & upd->scnmsk];
const updcomp_p *comp    = (updcomp_p *) upd->valptr;
int32_t *const     pixel  = upd->valbuf;
int32_t *const     colerr = pixel  + upd->ncomp;
int32_t           *rowerr = colerr + upd->ncomp;
int              pwidth = upd->rwidth;
int              dir,ibyte;
int              iblack,bblack,pxlset;
uint32_t       ci;
byte         bit;
bool         first = true;
switch(upd->ncomp) {
case 4:  memset(scan[3].bytes,0,upd->nbytes);
case 3:  memset(scan[2].bytes,0,upd->nbytes);
memset(scan[1].bytes,0,upd->nbytes);
default: memset(scan[0].bytes,0,upd->nbytes);
}
if(upd->flags &   B_REVDIR) {
if(upd->flags & B_YFLIP) {
dir     = upd->ncomp;
bit     = 0x80;
ibyte   = 0;
} else {
dir     =  -upd->ncomp;
rowerr +=   upd->ncomp * (pwidth-1);
bit     =   0x80 >>     ((pwidth-1) & 7);
ibyte   =                (pwidth-1) >> 3;
}
if(!(upd->flags & B_FSWHITE)) {
upd_pxlfwd(upd);
while((0 < pwidth) && !upd_pxlget(upd)) pwidth--;
}
upd_pxlrev(upd);
} else {
if(upd->flags & B_YFLIP) {
dir     =  -upd->ncomp;
rowerr +=   upd->ncomp * (pwidth-1);
bit     =   0x80 >>     ((pwidth-1) & 7);
ibyte   =                (pwidth-1) >> 3;
} else {
dir     = upd->ncomp;
bit     = 0x80;
ibyte   = 0;
}
if(!(upd->flags & B_FSWHITE)) {
upd_pxlrev(upd);
while((0 < pwidth) && !upd_pxlget(upd)) pwidth--;
}
upd_pxlfwd(upd);
}
if(!(upd->flags & B_FIXDIR)) upd->flags ^= B_REVDIR;
if(!(upd->flags & B_FSWHITE)) {
upd_proc_pxlget((*fun)) = upd->pxlget;
byte             *ptr   = upd->pxlptr;
while((0 < pwidth) && !upd_pxlget(upd)) {
pwidth--;
fun = upd->pxlget;
ptr = upd->pxlptr;
S_FSTEP
}
upd->pxlget = fun;
upd->pxlptr = ptr;
}
iblack = -1;
bblack =  0;
if((4 == upd->ncomp) && (B_REDUCEK & upd->flags)) {
iblack = upd->cmap[0].comp;
bblack = 1<<iblack;
}
first = true;
while(0 < pwidth--) {
pxlset = 0;
ci = upd_pxlget(upd);
switch(upd->ncomp) {
case 4:  FS_M_ROWERR(3)
FS_GOAL(comp[3]->bitmsk & (ci >> comp[3]->bitshf),3)
if(pixel[3] >  comp[3]->threshold) {
pixel[3] -= comp[3]->spotsize;
scan[3].bytes[ibyte] |= bit;
pxlset  |= 8;
}
FS_DIST(3)
case 3:  FS_M_ROWERR(2)
FS_GOAL(comp[2]->bitmsk & (ci >> comp[2]->bitshf),2)
if(pixel[2] >  comp[2]->threshold) {
pixel[2] -= comp[2]->spotsize;
scan[2].bytes[ibyte] |= bit;
pxlset  |= 4;
}
FS_DIST(2)
FS_M_ROWERR(1)
FS_GOAL(comp[1]->bitmsk & (ci >> comp[1]->bitshf),1)
if(pixel[1] >  comp[1]->threshold) {
pixel[1] -= comp[1]->spotsize;
scan[1].bytes[ibyte] |= bit;
pxlset  |= 2;
}
FS_DIST(1)
default: FS_M_ROWERR(0)
FS_GOAL(comp[0]->bitmsk & (ci >> comp[0]->bitshf),0)
if(pixel[0] >  comp[0]->threshold) {
pixel[0] -= comp[0]->spotsize;
scan[0].bytes[ibyte] |= bit;
pxlset  |= 1;
}
FS_DIST(0)
}
if(bblack) {
if(pxlset & bblack) pxlset |= 15;
switch(pxlset) {
case  0:
case  1:
case  2:
case  4:
case  8:
case  3:
case  5:
case  9:
case  6:
case 10:
case 12:
break;
default:
scan[0].bytes[ibyte]      &= ~bit;
scan[1].bytes[ibyte]      &= ~bit;
scan[2].bytes[ibyte]      &= ~bit;
scan[3].bytes[ibyte]      &= ~bit;
scan[iblack].bytes[ibyte] |=  bit;
break;
}
}
S_FSTEP
}
if(0 < upd->nlimits) upd_limits(upd,true);
return 0;
}
private void
upd_open_fscmyk(upd_device *udev)
{
const upd_p upd = udev->upd;
upd_open_fscomp(udev);
if((B_RENDER & upd->flags) &&
(4 == upd->ncomp) &&
(8 <= upd->cmap[0].bits) && (24 == upd->cmap[0].bitshf) &&
(8 <= upd->cmap[1].bits) && (16 == upd->cmap[1].bitshf) &&
(8 <= upd->cmap[2].bits) && ( 8 == upd->cmap[2].bitshf) &&
(8 <= upd->cmap[3].bits) && ( 0 == upd->cmap[3].bitshf)   ) {
upd->render = upd_fscmyk;
} else {
upd->flags &= ~B_RENDER;
}
}
private int
upd_fscmyk(upd_p upd)
{
const updscan_p  scan   = upd->scnbuf[upd->yscnbuf & upd->scnmsk];
int32_t *const     pixel  = upd->valbuf;
const updcomp_p *comp   = (updcomp_p *) upd->valptr;
int32_t *const     colerr = pixel  + 4;
int32_t           *rowerr = colerr + 4;
int32_t            pwidth = upd->rwidth;
int              dir,ibyte;
byte             bit,*data;
bool             first = false;
memset(scan[0].bytes,0,upd->nbytes);
memset(scan[1].bytes,0,upd->nbytes);
memset(scan[2].bytes,0,upd->nbytes);
memset(scan[3].bytes,0,upd->nbytes);
if(upd->flags &   B_REVDIR) {
if(!(upd->flags & B_FSWHITE)) {
data = upd->gsscan;
while(0 < pwidth && !*(uint32_t *)data) pwidth--, data += 4;
if(0 >= pwidth) {
if(0 < upd->nlimits) upd_limits(upd,false);
return 0;
}
}
data        = upd->gsscan + 4 * (upd->rwidth-1);
} else {
if(!(upd->flags & B_FSWHITE)) {
data = upd->gsscan + 4 * (upd->rwidth-1);
while(0 < pwidth && !*(uint32_t *)data) pwidth--, data -= 4;
if(0 >= pwidth) {
if(0 < upd->nlimits) upd_limits(upd,false);
return 0;
}
}
data        = upd->gsscan;
}
if(!(B_REVDIR & upd->flags) == !(B_YFLIP  & upd->flags)) {
dir         = 4;
bit         = 0x80;
ibyte       = 0;
} else {
dir         =  -4;
rowerr     +=   4 *             (upd->rwidth-1);
bit         =   0x80 >>        ((upd->rwidth-1) & 7);
ibyte       =                   (upd->rwidth-1) >> 3;
}
if(!(upd->flags & B_FIXDIR)) upd->flags ^= B_REVDIR;
if(!(upd->flags & B_FSWHITE)) {
while(0 < pwidth && !*((uint32_t *)data)) {
pwidth--;
if(B_YFLIP  & upd->flags) data -= dir;
else                      data += dir;
S_FSTEP
}
}
first = true;
while(0 < pwidth--) {
FS_M_ROWERR(upd->cmap[0].comp) FS_GOAL(data[0],upd->cmap[0].comp);
if(data[1] || data[2] || data[3]) {
FS_M_ROWERR(upd->cmap[1].comp) FS_GOAL(data[1],upd->cmap[1].comp)
FS_M_ROWERR(upd->cmap[2].comp) FS_GOAL(data[2],upd->cmap[2].comp)
FS_M_ROWERR(upd->cmap[3].comp) FS_GOAL(data[3],upd->cmap[3].comp)
if(pixel[upd->cmap[0].comp] > comp[upd->cmap[0].comp]->threshold) {
pixel[0] -= comp[0]->spotsize;
pixel[1] -= comp[1]->spotsize;
pixel[2] -= comp[2]->spotsize;
pixel[3] -= comp[3]->spotsize;
scan[upd->cmap[0].comp].bytes[ibyte] |= bit;
} else {
if(( data[0] < data[1]) &&
(pixel[upd->cmap[1].comp] >
comp[upd->cmap[1].comp]->threshold)) {
pixel[upd->cmap[1].comp] -= comp[upd->cmap[1].comp]->spotsize;
scan[upd->cmap[1].comp].bytes[ibyte] |= bit;
}
if(( data[0] < data[2]) &&
(pixel[upd->cmap[2].comp] >
comp[upd->cmap[2].comp]->threshold)) {
pixel[upd->cmap[2].comp] -= comp[upd->cmap[2].comp]->spotsize;
scan[upd->cmap[2].comp].bytes[ibyte] |= bit;
}
if(( data[0] < data[3]) &&
(pixel[upd->cmap[3].comp] >
comp[upd->cmap[3].comp]->threshold)) {
pixel[upd->cmap[3].comp] -= comp[upd->cmap[3].comp]->spotsize;
scan[upd->cmap[3].comp].bytes[ibyte] |= bit;
}
}
FS_DIST(upd->cmap[3].comp)
FS_DIST(upd->cmap[2].comp)
FS_DIST(upd->cmap[1].comp)
} else if(pixel[upd->cmap[0].comp] > comp[upd->cmap[0].comp]->threshold) {
scan[upd->cmap[0].comp].bytes[ibyte] |= bit;
pixel[upd->cmap[0].comp] -= comp[upd->cmap[0].comp]->spotsize;
}
FS_DIST(upd->cmap[0].comp)
S_FSTEP
if(upd->flags & B_YFLIP) data -= dir;
else                     data += dir;
}
if(0 < upd->nlimits) upd_limits(upd,true);
return 0;
}
private void
upd_open_fscmy_k(upd_device *udev)
{
const upd_p upd = udev->upd;
upd_open_fscomp(udev);
if((B_RENDER & upd->flags) &&
(4 == upd->ncomp)) {
upd->render = upd_fscmy_k;
} else {
upd->flags &= ~B_RENDER;
}
}
private int
upd_fscmy_k(upd_p upd)
{
const updscan_p  scan    = upd->scnbuf[upd->yscnbuf & upd->scnmsk];
const updcomp_p *comp    = (updcomp_p *) upd->valptr;
int32_t *const     pixel  = upd->valbuf;
int32_t *const     colerr = pixel  + upd->ncomp;
int32_t           *rowerr = colerr + upd->ncomp;
int              pwidth = upd->rwidth;
int              dir,ibyte;
uint32_t       ci;
byte         bit;
bool         first = true;
memset(scan[3].bytes,0,upd->nbytes);
memset(scan[2].bytes,0,upd->nbytes);
memset(scan[1].bytes,0,upd->nbytes);
memset(scan[0].bytes,0,upd->nbytes);
if(upd->flags &   B_REVDIR) {
if(upd->flags & B_YFLIP) {
dir     = 4;
bit     = 0x80;
ibyte   = 0;
} else {
dir     =  -4;
rowerr +=   4 * (pwidth-1);
bit     =   0x80 >>     ((pwidth-1) & 7);
ibyte   =                (pwidth-1) >> 3;
}
if(!(upd->flags & B_FSWHITE)) {
upd_pxlfwd(upd);
while((0 < pwidth) && !upd_pxlget(upd)) pwidth--;
}
upd_pxlrev(upd);
} else {
if(upd->flags & B_YFLIP) {
dir     =  -4;
rowerr +=   4          * (pwidth-1);
bit     =   0x80 >>     ((pwidth-1) & 7);
ibyte   =                (pwidth-1) >> 3;
} else {
dir     = 4;
bit     = 0x80;
ibyte   = 0;
}
if(!(upd->flags & B_FSWHITE)) {
upd_pxlrev(upd);
while((0 < pwidth) && !upd_pxlget(upd)) pwidth--;
}
upd_pxlfwd(upd);
}
if(!(upd->flags & B_FIXDIR)) upd->flags ^= B_REVDIR;
if(!(upd->flags & B_FSWHITE)) {
upd_proc_pxlget((*fun)) = upd->pxlget;
byte             *ptr   = upd->pxlptr;
while((0 < pwidth) && !upd_pxlget(upd)) {
pwidth--;
fun = upd->pxlget;
ptr = upd->pxlptr;
S_FSTEP
}
upd->pxlget = fun;
upd->pxlptr = ptr;
}
first = true;
while(0 < pwidth--) {
ci = upd_pxlget(upd);
FS_M_ROWERR(0) FS_GOAL(comp[0]->bitmsk & (ci >> comp[0]->bitshf),0)
FS_M_ROWERR(1) FS_GOAL(comp[1]->bitmsk & (ci >> comp[1]->bitshf),1)
FS_M_ROWERR(2) FS_GOAL(comp[2]->bitmsk & (ci >> comp[2]->bitshf),2)
FS_M_ROWERR(3) FS_GOAL(comp[3]->bitmsk & (ci >> comp[3]->bitshf),3)
if(pixel[0] >  comp[0]->threshold) {
pixel[0]             -= comp[0]->spotsize;
scan[0].bytes[ibyte] |= bit;
} else {
if((pixel[1] <= comp[1]->threshold) ||
(pixel[2] <= comp[2]->threshold) ||
(pixel[3] <= comp[3]->threshold)   ) {
if(pixel[1] >               comp[1]->threshold) {
pixel[1]              -= comp[1]->spotsize;
scan[1].bytes[ibyte] |= bit;
}
if(pixel[2] >               comp[2]->threshold) {
pixel[2]              -= comp[2]->spotsize;
scan[2].bytes[ibyte] |= bit;
}
if(pixel[3] >               comp[3]->threshold) {
pixel[3]              -= comp[3]->spotsize;
scan[3].bytes[ibyte] |= bit;
}
} else {
pixel[1]              -= comp[1]->spotsize;
pixel[2]              -= comp[2]->spotsize;
pixel[3]              -= comp[3]->spotsize;
scan[0].bytes[ibyte] |= bit;
}
}
FS_DIST(0)
FS_DIST(1)
FS_DIST(2)
FS_DIST(3)
S_FSTEP
}
if(0 < upd->nlimits) upd_limits(upd,true);
return 0;
}
private int
upd_open_writer(upd_device *udev)
{
const upd_p upd                 = udev->upd;
bool        success             = true;
upd->start_writer = NULL;
upd->writer       = NULL;
upd->scnbuf       = NULL;
upd->nscnbuf      = 0;
upd->nbytes       = 0;
upd->nlimits      = 0;
upd->outbuf       = NULL;
upd->noutbuf      = 0;
if(B_RENDER != ((B_RENDER | B_ERROR) & upd->flags))
success = false;
upd->ocomp = upd->ncomp;
if(0 < upd->ints[I_OCOMP]) upd->ocomp = upd->ints[I_OCOMP];
if(success) {
if(1 >  upd->ints[I_NYPASS]) upd->ints[I_NYPASS] = 1;
if(1 >  upd->ints[I_NXPASS]) upd->ints[I_NXPASS] = 1;
if(1 >  upd->ints[I_PINS2WRITE]) upd->ints[I_PINS2WRITE] = 1;
if((upd->ints[I_NXPASS] * upd->ints[I_NYPASS]) > upd->ints[I_NPASS])
upd->ints[I_NPASS] = upd->ints[I_NXPASS] * upd->ints[I_NYPASS];
if(upd->ints[I_NPASS] > upd->int_a[IA_STD_DY].size) {
int ix,iy,*ip;
UPD_MM_DEL_PARAM(udev->memory, upd->int_a[IA_STD_DY]);
UPD_MM_GET_ARRAY(udev->memory, ip,upd->ints[I_NPASS]);
upd->int_a[IA_STD_DY].data = ip;
upd->int_a[IA_STD_DY].size = upd->ints[I_NPASS];
for(iy = 1; iy < upd->ints[I_NYPASS]; ++iy) {
for(ix = 1; ix < upd->ints[I_NXPASS]; ++ix) *ip++ = 0;
*ip++ = 1;
}
for(ix = 1; ix < upd->ints[I_NXPASS]; ++ix) *ip++ = 0;
*ip = upd->ints[I_NYPASS] * upd->ints[I_PINS2WRITE]
- upd->ints[I_NYPASS] + 1;
upd->ints[I_BEG_Y] = 0;
upd->ints[I_END_Y] = upd->ints[I_PHEIGHT] ?
upd->ints[I_PHEIGHT] : upd->gsheight;
}
if(0 >= upd->ints[I_BEG_Y]) {
if(0 <  upd->int_a[IA_BEG_DY].size) {
int i,sum = 0;
for(i = 0; i < upd->int_a[IA_BEG_DY].size; ++i)
sum +=  upd->int_a[IA_BEG_DY].data[i];
upd->ints[I_BEG_Y] = sum;
} else {
upd->ints[I_BEG_Y] = 0;
}
}
if(0 >= upd->int_a[IA_ENDTOP].size ||
0 >= upd->int_a[IA_END_DY].size   ) upd->ints[I_END_Y] =
upd->ints[I_PHEIGHT] ? upd->ints[I_PHEIGHT] : upd->gsheight;
if(0 >= upd->ints[I_END_Y]) upd->ints[I_END_Y] = upd->ints[I_PHEIGHT] ?
upd->ints[I_PHEIGHT] : upd->gsheight;
if(0 >= upd->int_a[IA_STD_IX].size) {
int ix,i,*ip;
UPD_MM_DEL_PARAM(udev->memory, upd->int_a[IA_STD_IX]);
UPD_MM_GET_ARRAY(udev->memory, ip,upd->int_a[IA_STD_DY].size);
upd->int_a[IA_STD_IX].data = ip;
upd->int_a[IA_STD_IX].size = upd->int_a[IA_STD_DY].size;
for(i = 0, ix = 0; i < upd->int_a[IA_STD_IX].size; ++i) {
*ip++ = ix++;
if(ix == upd->ints[I_NXPASS]) ix = 0;
}
}
if((0 >= upd->int_a[IA_BEG_IX].size) &&
(0 <  upd->int_a[IA_BEG_DY].size)   ) {
int ix,i,*ip;
UPD_MM_DEL_PARAM(udev->memory, upd->int_a[IA_BEG_IX]);
UPD_MM_GET_ARRAY(udev->memory, ip,upd->int_a[IA_BEG_DY].size);
upd->int_a[IA_BEG_IX].data = ip;
upd->int_a[IA_BEG_IX].size = upd->int_a[IA_BEG_DY].size;
for(i = 0, ix = 0; i < upd->int_a[IA_BEG_IX].size; ++i) {
*ip++ = ix++;
if(ix == upd->ints[I_NXPASS]) ix = 0;
}
}
if((0 >= upd->int_a[IA_END_IX].size) &&
(0 <  upd->int_a[IA_END_DY].size)   ) {
int ix,i,*ip;
UPD_MM_DEL_PARAM(udev->memory, upd->int_a[IA_END_IX]);
UPD_MM_GET_ARRAY(udev->memory, ip,upd->int_a[IA_END_DY].size);
upd->int_a[IA_END_IX].data = ip;
upd->int_a[IA_END_IX].size = upd->int_a[IA_END_DY].size;
for(i = 0, ix = 0; i < upd->int_a[IA_END_IX].size; ++i) {
*ip++ = ix++;
if(ix == upd->ints[I_NXPASS]) ix = 0;
}
}
}
if(upd->ints[I_NPASS] > upd->int_a[IA_STD_DY].size) {
#if UPD_MESSAGES & UPD_M_WARNING
errprintf(
"upd_open_writer: Only %d instead of %d normal Feeds\n",
(int) upd->int_a[IA_STD_DY].size,upd->ints[I_NPASS]);
#endif
success = false;
} else if(upd->int_a[IA_STD_IX].size < upd->int_a[IA_STD_DY].size) {
#if UPD_MESSAGES & UPD_M_WARNING
errprintf(
"upd_open_writer: Only %d instead of %d normal Xstarts\n",
(int) upd->int_a[IA_STD_IX].size,
(int) upd->int_a[IA_STD_DY].size);
#endif
success = false;
}
#if UPD_MESSAGES & UPD_M_WARNING
if(success) {
int i,sum = 0;
for(i = 0; upd->ints[I_NPASS] > i; ++i)
sum += upd->int_a[IA_STD_DY].data[i];
if((upd->ints[I_NYPASS]*upd->ints[I_PINS2WRITE]) != sum)
errprintf(
"upd_open_writer: Sum of normal Feeds is %d rather than %d\n",
sum,upd->ints[I_NYPASS]*upd->ints[I_PINS2WRITE]);
}
#endif
if(upd->int_a[IA_BEG_IX].size < upd->int_a[IA_BEG_DY].size) {
#if UPD_MESSAGES & UPD_M_WARNING
errprintf(
"upd_open_writer: Only %d instead of %d initial Xstarts\n",
(int) upd->int_a[IA_BEG_IX].size,
(int) upd->int_a[IA_BEG_DY].size);
#endif
success = false;
}
if(upd->int_a[IA_BEGBOT].size < upd->int_a[IA_BEG_DY].size) {
#if UPD_MESSAGES & UPD_M_WARNING
errprintf(
"upd_open_writer: Only %d instead of %d initial Pins\n",
(int) upd->int_a[IA_BEGBOT].size,
(int) upd->int_a[IA_BEG_DY].size);
#endif
success = false;
} else {
int i;
for(i = 0; i < upd->int_a[IA_BEG_DY].size; ++i)
if((upd->int_a[IA_BEGBOT].data[i] > upd->ints[I_PINS2WRITE]) ||
(upd->int_a[IA_BEGBOT].data[i] < 0                      )   ) break;
if(i < upd->int_a[IA_BEG_DY].size) {
#if UPD_MESSAGES & UPD_M_WARNING
errprintf(
"upd_open_writer: Only %d is invalid initial Pins\n",
upd->int_a[IA_BEGBOT].data[i]);
#endif
success = false;
}
}
#if UPD_MESSAGES & UPD_M_WARNING
if(success) {
int i,sum = 0;
for(i = 0;  upd->int_a[IA_BEG_DY].size > i; ++i)
sum += upd->int_a[IA_BEG_DY].data[i];
if(upd->ints[I_BEG_Y] != sum)
errprintf(
"upd_open_writer: Sum of initial Feeds is %d rather than %d\n",
sum,upd->ints[I_BEG_Y]);
}
#endif
if(upd->int_a[IA_END_IX].size < upd->int_a[IA_END_DY].size) {
#if UPD_MESSAGES & UPD_M_WARNING
errprintf(
"upd_open_writer: Only %d instead of %d final Xstarts\n",
(int) upd->int_a[IA_END_IX].size,
(int) upd->int_a[IA_END_DY].size);
#endif
success = false;
}
if(upd->int_a[IA_ENDTOP].size < upd->int_a[IA_END_DY].size) {
#if UPD_MESSAGES & UPD_M_WARNING
errprintf(
"upd_open_writer: Only %d instead of %d Final Pins\n",
(int) upd->int_a[IA_ENDTOP].size,
(int) upd->int_a[IA_END_DY].size);
#endif
success = false;
} else {
int i;
for(i = 0; i < upd->int_a[IA_END_DY].size; ++i)
if((upd->int_a[IA_ENDTOP].data[i] > upd->ints[I_PINS2WRITE]) ||
(upd->int_a[IA_ENDTOP].data[i] < 0                      )   ) break;
if(i < upd->int_a[IA_END_DY].size) {
#if UPD_MESSAGES & UPD_M_WARNING
errprintf(
"upd_open_writer: Only %d is invalid initial Pins\n",
upd->int_a[IA_ENDTOP].data[i]);
#endif
success = false;
}
}
if((0 < upd->string_a[SA_SETCOMP].size) &&
(upd->ocomp > upd->string_a[SA_SETCOMP].size)) {
#if UPD_MESSAGES & UPD_M_WARNING
errprintf(
"upd_open_writer: Only %d SETCOMP-Commands (%d required)\n",
(int) upd->string_a[SA_SETCOMP].size,upd->ocomp);
#endif
success = false;
}
if(success) {
int32_t want,use;
want  = upd->ints[I_NYPASS];
want *= upd->ints[I_PINS2WRITE];
if(upd->ints[I_NSCNBUF] > want) want = upd->ints[I_NSCNBUF];
if(1 > want)                         want = 1;
for(use = 1; 0 < use; use <<= 1) if(use > want) break;
if(use <= INT_MAX) upd->nscnbuf = upd->ints[I_NSCNBUF] = use;
else               success      = false;
}
if(success) {
if(0 < upd->ints[I_PWIDTH]) upd->pwidth = upd->ints[I_PWIDTH];
else                        upd->pwidth = upd->gswidth;
upd->nbytes  = (upd->pwidth+CHAR_BIT*sizeof(upd->scnbuf[0]->bytes[0]) - 1)
/                   (CHAR_BIT*sizeof(upd->scnbuf[0]->bytes[0]));
upd->scnmsk  = upd->nscnbuf - 1;
if(0 < upd->ints[I_PHEIGHT]) upd->pheight = upd->ints[I_PHEIGHT];
else                         upd->pheight = upd->gsheight;
}
if(success) {
switch(upd->choice[C_FORMAT]) {
case FMT_RAS:
if(0 > upd_open_rascomp(udev)) success = false;
break;
case FMT_EPSON:
if(0 > upd_open_wrtescp(udev)) success = false;
break;
case FMT_ESCP2Y:
case FMT_ESCP2XY:
case FMT_ESCNMY:
if(0 > upd_open_wrtescp2(udev)) success = false;
break;
case FMT_RTL:
if(0 > upd_open_wrtrtl(udev))   success = false;
break;
case FMT_CANON:
if(0 > upd_open_wrtcanon(udev)) success = false;
break;
default:
success = false;
#if UPD_MESSAGES & UPD_M_WARNING
errprintf("upd_open_writer: Unknown writer-type %d\n",
upd->choice[C_FORMAT]);
#endif
break;
}
}
if(success && (0 < upd->noutbuf)) {
upd->outbuf = gs_malloc(udev->memory, upd->noutbuf,sizeof(upd->outbuf[0]),"upd/outbuf");
if(!upd->outbuf) success = false;
}
if(success) {
upd->scnbuf = gs_malloc(udev->memory, upd->nscnbuf,sizeof(upd->scnbuf[0]),"upd/scnbuf");
if(NULL == upd->scnbuf) {
success = false;
} else {
int ibuf;
for(ibuf = 0; ibuf < upd->nscnbuf; ++ibuf) {
if(success) upd->scnbuf[ibuf] =
gs_malloc(udev->memory, upd->ocomp,sizeof(upd->scnbuf[0][0]),"upd/scnbuf[]");
else upd->scnbuf[ibuf] = NULL;
if(!upd->scnbuf[ibuf]) {
success = false;
} else {
int icomp;
for(icomp = 0; icomp < upd->ocomp; ++icomp) {
if(success) upd->scnbuf[ibuf][icomp].bytes =
gs_malloc(udev->memory, upd->nbytes,sizeof(upd->scnbuf[0][0].bytes[0]),
"upd/bytes");
else        upd->scnbuf[ibuf][icomp].bytes = NULL;
if(!upd->scnbuf[ibuf][icomp].bytes) success = false;
if(0 < upd->nlimits) {
upd->scnbuf[ibuf][icomp].xbegin = gs_malloc(udev->memory, upd->nlimits,
sizeof(upd->scnbuf[0][0].xbegin[0]),"upd/xbegin");
if(!upd->scnbuf[ibuf][icomp].xbegin) success = false;
upd->scnbuf[ibuf][icomp].xend   = gs_malloc(udev->memory, upd->nlimits,
sizeof(upd->scnbuf[0][0].xend[0]),"upd/xend");
if(!upd->scnbuf[ibuf][icomp].xbegin) success = false;
} else {
upd->scnbuf[ibuf][icomp].xbegin = NULL;
upd->scnbuf[ibuf][icomp].xend   = NULL;
}
}
}
}
}
}
if(success) upd->flags |= B_FORMAT;
else        upd_close_writer(udev);
return success ? 1 : -1;
}
private void
upd_close_writer(upd_device *udev)
{
const upd_p upd = udev->upd;
if(upd) {
int ibuf,icomp;
if((0 < upd->noutbuf) && upd->outbuf)
gs_free(udev->memory, upd->outbuf,upd->noutbuf,sizeof(upd->outbuf[0]),"upd/outbuf");
upd->noutbuf = 0;
upd->outbuf  = NULL;
if((0 < upd->nscnbuf) && upd->scnbuf) {
for(ibuf = 0; upd->nscnbuf > ibuf; ++ibuf) {
if(!upd->scnbuf[ibuf]) continue;
for(icomp = 0; icomp < upd->ocomp; ++icomp) {
if((0 < upd->nbytes) && upd->scnbuf[ibuf][icomp].bytes)
gs_free(udev->memory, upd->scnbuf[ibuf][icomp].bytes,upd->nbytes,
sizeof(upd->scnbuf[ibuf][icomp].words[0]),"upd/bytes");
upd->scnbuf[ibuf][icomp].bytes = NULL;
if((0 < upd->nlimits) && upd->scnbuf[ibuf][icomp].xbegin)
gs_free(udev->memory, upd->scnbuf[ibuf][icomp].xbegin,upd->nlimits,
sizeof(upd->scnbuf[ibuf][icomp].xbegin[0]),"upd/xbegin");
upd->scnbuf[ibuf][icomp].xbegin = NULL;
if((0 < upd->nlimits) && upd->scnbuf[ibuf][icomp].xend)
gs_free(udev->memory, upd->scnbuf[ibuf][icomp].xend,upd->nlimits,
sizeof(upd->scnbuf[ibuf][icomp].xend[0]),"upd/xend");
upd->scnbuf[ibuf][icomp].xend = NULL;
}
if(icomp)
gs_free(udev->memory, upd->scnbuf[ibuf],upd->ocomp,sizeof(upd->scnbuf[0][0]),
"upd/scnbuf[]");
upd->scnbuf[ibuf] = NULL;
}
gs_free(udev->memory, upd->scnbuf,upd->nscnbuf,sizeof(upd->scnbuf[0]),"upd/scnbuf");
}
upd->flags &= ~B_FORMAT;
}
}
private void
upd_limits(upd_p upd, bool check)
{
updscan_p  scans = upd->scnbuf[upd->yscnbuf & upd->scnmsk], scan;
int   xs,x,xe,icomp,pass;
byte *bytes,bit;
for(icomp = 0; icomp < upd->ocomp; ++icomp) {
scan = scans + icomp;
for(pass = 0; pass < upd->nlimits; ++pass) {
scan->xbegin[pass] = upd->pwidth;
scan->xend[  pass] = -1;
}
}
if(check) {
for(icomp = 0; icomp < upd->ocomp; ++icomp) {
scan  = scans + icomp;
bytes = scan->bytes;
for(xs = 0; xs < upd->nbytes  && !bytes[xs];   ++xs);
if(xs < upd->nbytes) {
for(xe = upd->nbytes; xs < xe && !bytes[xe-1]; --xe);
for(pass = 0; pass < upd->nlimits; ++pass) {
x = ((xs<<3)/upd->nlimits)*upd->nlimits + pass;
while((x >> 3) < xs) x += upd->nlimits;
bit = 0x80 >> (x & 7);
while(x < scan->xbegin[pass]) {
if(bytes[x>>3] & bit) scan->xbegin[pass] = x;
x  += upd->nlimits;
bit = 0x80 >> (x & 7);
}
x = (((xe<<3)|7)/upd->nlimits)*upd->nlimits + pass;
while((x >> 3) < xe) x += upd->nlimits;
while((x >> 3) > xe) x -= upd->nlimits;
bit = 0x80 >> (xs & 7);
while(x > scan->xend[pass]) {
if(bytes[x>>3] & bit) scan->xend[pass] = x;
x -= upd->nlimits;
bit = 0x80 >> (x & 7);
}
}
}
}
}
}
private int
upd_open_rascomp(upd_device *udev)
{
const upd_p upd = udev->upd;
int32_t noutbuf;
int error = 0;
noutbuf = upd->pwidth;
if(1 < upd->ncomp) noutbuf *= 8;
noutbuf = ((noutbuf+15)>>4)<<1;
if(INT_MAX >= noutbuf) {
upd->noutbuf = noutbuf;
upd->start_writer = upd_start_rascomp;
upd->writer       = upd_rascomp;
} else {
error = -1;
}
return error;
}
#if arch_is_big_endian
#define put32(I32,Out)       \
fwrite(&I32,1,4,Out)
#else
#define put32(I32,Out)       \
putc(((I32)>>24)&255,Out),\
putc(((I32)>>16)&255,Out),\
putc(((I32)>> 8)&255,Out),\
putc( (I32)     &255,Out)
#endif
private int
upd_start_rascomp(upd_p upd, FILE *out) {
if(0 == upd->strings[S_BEGIN].size) {
int32_t val;
val = 0x59a66a95;
put32(val,out);
val = upd->pwidth;
put32(val,out);
val = upd->pheight;
put32(val,out);
if(1 < upd->ncomp) val = 8;
else               val = 1;
put32(val,out);
val *= upd->pwidth;
val = ((val+15)>>4)<<1;
val *= upd->pheight;
put32(val,out);
val = 1;
put32(val,out);
val = 1;
put32(val,out);
val = 3 * (1 << upd->ncomp);
put32(val,out);
if(1 == upd->ncomp) {
const updcomp_p comp = upd->valptr[0];
if(upd->cmap[comp->cmap].rise) {
putc((char) 0x00,out); putc((char) 0xff,out);
putc((char) 0x00,out); putc((char) 0xff,out);
putc((char) 0x00,out); putc((char) 0xff,out);
} else {
putc((char) 0xff,out); putc((char) 0x00,out);
putc((char) 0xff,out); putc((char) 0x00,out);
putc((char) 0xff,out); putc((char) 0x00,out);
}
} else if(3 == upd->ncomp) {
int rgb;
for( rgb = 0; rgb < 3; ++rgb) {
int entry;
for(entry = 0; entry < 8; ++entry) {
byte xval = upd->cmap[rgb].rise ? 0x00 : 0xff;
if(entry & (1<<upd->cmap[rgb].comp)) xval ^= 0xff;
putc(xval,out);
}
}
} else {
int rgb;
for(rgb = 16; 0 <= rgb; rgb -= 8) {
int entry;
for(entry = 0; entry < 16; ++entry) {
uint32_t rgbval = 0;
if(entry & (1<<upd->cmap[0].comp)) {
rgbval = 0xffffff;
} else {
if(entry & (1<<upd->cmap[1].comp)) rgbval |= 0xff0000;
if(entry & (1<<upd->cmap[2].comp)) rgbval |= 0x00ff00;
if(entry & (1<<upd->cmap[3].comp)) rgbval |= 0x0000ff;
}
if(!upd->cmap[1].rise) rgbval ^= 0xff0000;
if(!upd->cmap[2].rise) rgbval ^= 0x00ff00;
if(!upd->cmap[3].rise) rgbval ^= 0x0000ff;
if(!(upd->choice[C_MAPPER] == MAP_RGBW)) rgbval ^= 0xffffff;
putc((rgbval>>rgb)&255,out);
}
}
}
}
memset(upd->outbuf,0,upd->noutbuf);
return 0;
}
private int
upd_rascomp(upd_p upd, FILE *out) {
updscan_p scan = upd->scnbuf[upd->yscan & upd->scnmsk];
uint bits = upd->pwidth;
if(1 == upd->ncomp) {
uint nbytes;
nbytes = (bits+7)>>3;
memcpy(upd->outbuf,scan->bytes,nbytes);
if((bits &= 7)) upd->outbuf[nbytes-1] &= ((byte) 0xff) << (8-bits);
} else {
byte  *buf   = upd->outbuf, bit = 0x80;
int    ibyte = 0;
while(0 < bits--) {
byte val = 0;
switch(upd->ncomp) {
case 4:  if(scan[3].bytes[ibyte] & bit) val |= 8;
case 3:  if(scan[2].bytes[ibyte] & bit) val |= 4;
if(scan[1].bytes[ibyte] & bit) val |= 2;
case 1:  if(scan[0].bytes[ibyte] & bit) val |= 1;
}
*buf++ = val;
if(!(bit >>= 1)) {
bit    = 0x80;
ibyte += 1;
}
}
}
fwrite(upd->outbuf,1,upd->noutbuf,out);
upd->yscan += 1;
return 0;
}
private int
upd_open_wrtescp(upd_device *udev)
{
const upd_p      upd  = udev->upd;
int              error = 0;
if((B_PAGELENGTH & upd->flags) &&
(0 < upd->strings[S_BEGIN].size)) {
int   i,state = 0,value = 0;
byte *bp = (byte *) upd_cast(upd->strings[S_BEGIN].data);
for(i = 0; i < upd->strings[S_BEGIN].size; ++i) {
switch(state) {
case  0:
if(0x1b == bp[i]) state = 1;
break;
case  1:
if('C'  == bp[i]) state = 2;
else              state = 0;
break;
case  2:
if(bp[i]) {
value = (int)(0.5 + udev->height * (float) bp[i]
/ udev->y_pixels_per_inch);
if(       0 >= value) bp[i] = 1;
else if(128 >  value) bp[i] = value;
else                  bp[i] = 127;
state = 0;
} else {
state = 3;
}
break;
case  3:
value = (int)(0.5 + udev->height / udev->y_pixels_per_inch);
if(       0 >= value) bp[i] = 1;
else if( 22 >  value) bp[i] = value;
else                  bp[i] = 22;
state = 0;
break;
}
}
}
if((0 == upd->strings[S_SETLF].size) &&
(0 == upd->strings[S_YMOVE].size)   ) {
#if UPD_MESSAGES & UPD_M_WARNING
errprintf(
"ESC/P-Open: Either SETLF- or YMOVE-Command must be present\n");
#endif
error = -1;
}
if(((1 <  upd->ints[I_XSTEP]        ) &&
(0 == upd->strings[S_XSTEP].size)   ) ||
((1 < upd->ints[I_NXPASS]        ) &&
(0 == upd->strings[S_XMOVE].size) &&
(0 == upd->strings[S_XSTEP].size)   )   ) {
#if UPD_MESSAGES & UPD_M_WARNING
errprintf(
"ESC/P-Open: Missing XSTEP- and/or XMOVE-Command\n");
#endif
error = -1;
}
if(upd->ncomp > upd->string_a[SA_WRITECOMP].size) {
#if UPD_MESSAGES & UPD_M_WARNING
errprintf(
"ESC/P-Open: WRITECOMP-Commands must be given\n");
#endif
error = -1;
}
if(0 <= error) {
int32_t i,noutbuf,need;
if(0 < upd->strings[S_YMOVE].size) {
noutbuf = upd->strings[S_YMOVE].size + 2;
} else {
int nmax = upd->pheight;
if(      1 < upd->ints[I_YSTEP]) nmax /=  upd->ints[I_YSTEP];
else if(-1 > upd->ints[I_YSTEP]) nmax *= -upd->ints[I_YSTEP];
noutbuf  = 2 * upd->strings[S_SETLF].size + 2;
noutbuf += nmax/255 + 1;
}
if(1 < upd->ints[I_YSTEP])
noutbuf += (upd->ints[I_YSTEP]-1) * upd->strings[S_YSTEP].size;
noutbuf +=  upd->strings[S_XMOVE].size + 2;
if(1 < upd->ints[I_XSTEP])
noutbuf += (upd->ints[I_XSTEP]-1) * upd->strings[S_XSTEP].size;
if(0 < upd->string_a[SA_SETCOMP].size) {
need = 0;
for(i = 0; i < upd->ocomp; ++i)
if(need < upd->string_a[SA_SETCOMP].data[i].size)
need = upd->string_a[SA_SETCOMP].data[i].size;
noutbuf += need;
}
need = 0;
for(i = 0; i < upd->ocomp; ++i)
if(need < upd->string_a[SA_WRITECOMP].data[i].size)
need = upd->string_a[SA_WRITECOMP].data[i].size;
noutbuf += need + 2;
noutbuf += ((upd->ints[I_PINS2WRITE] + 7) / 8)
* ((upd->pwidth + upd->ints[I_NXPASS] - 1)/upd->ints[I_NXPASS]);
if((0 < noutbuf) && (noutbuf <= INT_MAX)) {
upd->noutbuf      = noutbuf;
upd->writer       = upd_wrtescp;
upd->nlimits      = upd->ints[I_NXPASS];
error             = 1;
} else {
error = -1;
#if      UPD_MESSAGES & UPD_M_WARNING
errprintf(
"ESC/P-Open: %ld is unreasonable size of Outputbuffer\n",
(long) noutbuf);
#endif
}
}
return error;
}
private int
upd_wrtescp(upd_p upd, FILE *out)
{
int  pinbot,pin,pintop,xbegin,x,xend,icomp,ybegin,yend,y,ioutbuf,n,ixpass;
byte *obytes,bit;
updscan_p scan;
if(upd->yscan < upd->ints[I_BEG_Y]) {
ixpass = upd->int_a[IA_BEG_IX].data[upd->ipass];
pintop = 0;
pinbot = upd->int_a[IA_BEGBOT].data[upd->ipass];
} else if(upd->yscan >= upd->ints[I_END_Y]) {
ixpass = upd->int_a[IA_END_IX].data[upd->ipass];
pinbot = upd->ints[I_PINS2WRITE];
pintop = pinbot - upd->int_a[IA_ENDTOP].data[upd->ipass];
} else {
ixpass = upd->int_a[IA_STD_IX].data[upd->ipass];
pintop = 0;
pinbot = upd->ints[I_PINS2WRITE];
}
ybegin =  pintop * upd->ints[I_NYPASS] + upd->yscan - upd->ints[I_BEGSKIP];
yend   =  pinbot * upd->ints[I_NYPASS] + upd->yscan - upd->ints[I_BEGSKIP];
xbegin = upd->pwidth;
xend   = -1;
for(y = ybegin; y < yend; y += upd->ints[I_NYPASS]) {
if(0 > y) continue;
scan = upd->scnbuf[y & upd->scnmsk];
for(icomp = 0; icomp < upd->ocomp; ++icomp) {
if(xbegin > scan[icomp].xbegin[ixpass])
xbegin = scan[icomp].xbegin[ixpass];
if(xend   < scan[icomp].xend[  ixpass])
xend   = scan[icomp].xend[  ixpass];
}
}
if(xbegin <= xend) {
ioutbuf = 0;
if(0 == upd->strings[S_XMOVE].size) xbegin = ixpass;
if(upd->yscan != upd->yprinter) {
if(B_YABS & upd->flags) y = upd->yscan + upd->ints[I_YOFS];
else                    y = upd->yscan - upd->yprinter;
if(      1 < upd->ints[I_YSTEP]) {
n      =  y / upd->ints[I_YSTEP];
y     -=  n * upd->ints[I_YSTEP];
} else if(-1 > upd->ints[I_YSTEP]) {
n      = y * -upd->ints[I_YSTEP];
y      = 0;
} else {
n      = y;
y      = 0;
}
if(n) {
if(0 < upd->strings[S_YMOVE].size) {
memcpy(upd->outbuf+ioutbuf,
upd->strings[S_YMOVE].data,
upd->strings[S_YMOVE].size);
ioutbuf += upd->strings[S_YMOVE].size;
upd->outbuf[ioutbuf++] =  n     & 0xff;
upd->outbuf[ioutbuf++] = (n>>8) & 0xff;
} else {
while(n) {
int n2do = n > 255 ? 255 : n;
if(upd->lf != n2do) {
memcpy(upd->outbuf+ioutbuf,
upd->strings[S_SETLF].data,
upd->strings[S_SETLF].size);
ioutbuf += upd->strings[S_SETLF].size;
upd->outbuf[ioutbuf++] = n2do;
upd->lf                = n2do;
}
upd->outbuf[ioutbuf++] = '\n';
n -= n2do;
}
}
}
if(0 < upd->strings[S_YSTEP].size) {
while(y--) {
memcpy(upd->outbuf+ioutbuf,
upd->strings[S_YSTEP].data,
upd->strings[S_YSTEP].size);
ioutbuf += upd->strings[S_YSTEP].size;
}
}
upd->yprinter = upd->yscan;
}
for(icomp = 0; icomp < upd->ocomp; ++icomp) {
for(y = ybegin; y < yend; y += upd->ints[I_NYPASS]) {
if(0 > y) continue;
scan = upd->scnbuf[y & upd->scnmsk]+icomp;
if(0 <= scan->xend[ixpass]) break;
}
if(y >= yend) continue;
if((0 < upd->string_a[SA_SETCOMP].size) &&
(upd->icomp != icomp               )   ) {
upd->icomp = icomp;
if(0 < upd->string_a[SA_SETCOMP].data[icomp].size) {
memcpy(upd->outbuf+ioutbuf,
upd->string_a[SA_SETCOMP].data[icomp].data,
upd->string_a[SA_SETCOMP].data[icomp].size);
ioutbuf += upd->string_a[SA_SETCOMP].data[icomp].size;
}
}
if(xbegin != upd->xprinter) {
if(0 == upd->strings[S_XMOVE].size) {
upd->outbuf[ioutbuf++] = '\r';
upd->xprinter          =  0;
n = 0;
x = ixpass;
} else {
if(B_XABS & upd->flags) n = x = xbegin + upd->ints[I_XOFS];
else                    n = x = xbegin - upd->xprinter;
if(        1 < upd->ints[I_XSTEP]) {
if(0 > n) {
n  -= upd->ints[I_XSTEP];
x  -= n;
}
if(n) n  /= upd->ints[I_XSTEP];
if(x) x  %= upd->ints[I_XSTEP];
} else if(-1 > upd->ints[I_XSTEP]) {
n *= -upd->ints[I_XSTEP];
x  = 0;
}
if(n) {
memcpy(upd->outbuf+ioutbuf,
upd->strings[S_XMOVE].data,
upd->strings[S_XMOVE].size);
ioutbuf += upd->strings[S_XMOVE].size;
upd->outbuf[ioutbuf++] =  n     & 0xff;
upd->outbuf[ioutbuf++] = (n>>8) & 0xff;
}
}
if(x && 0 < upd->strings[S_XSTEP].size) {
while(x--) {
memcpy(upd->outbuf+ioutbuf,
upd->strings[S_XSTEP].data,
upd->strings[S_XSTEP].size);
ioutbuf += upd->strings[S_XSTEP].size;
}
}
}
upd->xprinter = xend+1;
if(0 < upd->string_a[SA_WRITECOMP].data[icomp].size) {
memcpy(upd->outbuf+ioutbuf,
upd->string_a[SA_WRITECOMP].data[icomp].data,
upd->string_a[SA_WRITECOMP].data[icomp].size);
ioutbuf += upd->string_a[SA_WRITECOMP].data[icomp].size;
}
n = (xend - xbegin) / upd->ints[I_NXPASS] + 1;;
upd->outbuf[ioutbuf++] =  n     & 255;
upd->outbuf[ioutbuf++] = (n>>8) & 255;
obytes   =  upd->outbuf+ioutbuf;
n       *= (upd->ints[I_PINS2WRITE]+7)>>3;
memset(obytes,0,n);
ioutbuf += n;
for(x = xbegin; x <= xend; x += upd->ints[I_NXPASS]) {
bit     = 0x80 >> (pintop & 7);
obytes += pintop>>3;
for(pin = pintop, y = ybegin; pin < pinbot;
pin++,        y += upd->ints[I_NYPASS]) {
if(0 <= y) {
scan = upd->scnbuf[y & upd->scnmsk]+icomp;
if(scan->bytes[x>>3] & (0x80 >> (x & 7))) *obytes |= bit;
}
if(!(bit >>= 1)) { obytes++; bit = 0x80; }
}
obytes += (upd->ints[I_PINS2WRITE]-pinbot+7)>>3;
}
fwrite(upd->outbuf,1,ioutbuf,out);
ioutbuf = 0;
}
}
if(upd->yscan < upd->ints[I_BEG_Y]) {
upd->yscan += upd->int_a[IA_BEG_DY].data[upd->ipass++];
if(     upd->ints[I_BEG_Y] <= upd->yscan) upd->ipass = 0;
else if(upd->int_a[IA_BEG_DY].size <= upd->ipass) upd->ipass = 0;
} else if(upd->yscan >= upd->ints[I_END_Y]) {
upd->yscan += upd->int_a[IA_END_DY].data[upd->ipass++];
if(upd->int_a[IA_END_DY].size <= upd->ipass) upd->ipass = 0;
} else {
upd->yscan += upd->int_a[IA_STD_DY].data[upd->ipass++];
if(upd->int_a[IA_STD_DY].size <= upd->ipass) upd->ipass = 0;
if(upd->yscan >= upd->ints[I_END_Y])         upd->ipass = 0;
}
return 0;
}
private int
upd_open_wrtescp2(upd_device *udev)
{
const upd_p      upd             = udev->upd;
int              error           = 0;
float            pixels_per_inch = 360.0;
if(0 < upd->strings[S_BEGIN].size) {
int   i,state = 0,value = 0;
byte *bp = (byte *) upd_cast(upd->strings[S_BEGIN].data);
for(i = 0; i < upd->strings[S_BEGIN].size; ++i) {
switch(state) {
case  0:
if(0x1b == bp[i]) state = 1;
break;
case  1:
if('('  == bp[i]) state = 2;
else              state = 0;
break;
case  2:
switch(bp[i]) {
case 'U': state =  3; break;
case 'C': state =  6; break;
case 'c': state = 10; break;
default:  state =  0; break;
}
break;
case  3:
if(1 == bp[i]) state = 4;
else           state = 0;
break;
case  4:
if(0 == bp[i]) state = 5;
else           state = 0;
break;
case  5:
pixels_per_inch = 3600.0 / (float) bp[i];
state = 0;
break;
case  6:
if(2 == bp[i]) state = 7;
else           state = 0;
break;
case  7:
if(0 == bp[i]) state = 8;
else           state = 0;
break;
case  8:
if(B_PAGELENGTH & upd->flags) {
value = (int)(0.5 + udev->height
* pixels_per_inch / udev->y_pixels_per_inch);
bp[i] =  value     & 0xff;
}
state = 9;
break;
case  9:
if(B_PAGELENGTH & upd->flags) {
bp[i] = (value>>8) & 0xff;
}
state = 0;
break;
case 10:
if(4 == bp[i]) state = 11;
else           state =  0;
break;
case 11:
if(0 == bp[i]) state = 12;
else           state =  0;
break;
case  12:
if(B_TOPMARGIN & upd->flags) {
value =  (int)(dev_t_margin(udev) * pixels_per_inch);
bp[i] =  value     & 0xff;
}
state = 13;
break;
case  13:
if(B_TOPMARGIN & upd->flags) {
bp[i] = (value>>8) & 0xff;
}
state = 14;
break;
case  14:
if(B_BOTTOMMARGIN & upd->flags) {
value = (int)(0.5 + udev->height
* pixels_per_inch / udev->y_pixels_per_inch
- dev_b_margin(udev) * pixels_per_inch);
bp[i] =  value     & 0xff;
}
state = 15;
break;
case  15:
if(B_BOTTOMMARGIN & upd->flags) {
bp[i] = (value>>8) & 0xff;
}
state =  0;
break;
}
}
}
if(0 == upd->strings[S_YMOVE].size) {
byte *bp;
UPD_MM_DEL_PARAM(udev->memory, upd->strings[S_YMOVE]);
UPD_MM_GET_ARRAY(udev->memory, bp,5);
upd->strings[S_YMOVE].data = bp;
upd->strings[S_YMOVE].size = 5;
*bp++ = 0x1b;
*bp++ = '(';
*bp++ = upd->flags & B_YABS ? 'V' : 'v';
*bp++ =  2;
*bp++ =  0;
}
if((1 < upd->ints[I_XSTEP]) && (0 == upd->strings[S_XSTEP].size)) {
#if UPD_MESSAGES & UPD_M_WARNING
errprintf(
"ESC/P2-Open: XSTEP-Command required for XSTEP=%d\n",
upd->ints[I_XSTEP]);
#endif
error = -1;
} else if((1 <  upd->ints[I_NXPASS]       ) &&
(0 == upd->strings[S_XMOVE].size) &&
(0 == upd->strings[S_XSTEP].size)   ) {
byte *bp;
int ratio;
ratio = (int)((udev->y_pixels_per_inch + 0.5) / udev->x_pixels_per_inch);
if(0 == upd->ints[I_XSTEP]) {
if(ratio > 1) upd->ints[I_XSTEP] = -ratio;
} else {
ratio = -upd->ints[I_XSTEP];
}
if(2 == upd->ints[I_NXPASS]) {
UPD_MM_DEL_PARAM(udev->memory, upd->strings[S_XSTEP]);
UPD_MM_GET_ARRAY(udev->memory, bp,4);
upd->strings[S_XSTEP].size = 4;
upd->strings[S_XSTEP].data = bp;
*bp++ = 0x1b;
*bp++ = '\\';
*bp++ =  ratio     & 0xff;
*bp++ = (ratio>>8) & 0xff;
} else {
UPD_MM_DEL_PARAM(udev->memory, upd->strings[S_XMOVE]);
UPD_MM_GET_ARRAY(udev->memory, bp,2);
upd->strings[S_XMOVE].size = 2;
upd->strings[S_XMOVE].data = bp;
*bp++  = 0x1b;
*bp++  = upd->flags & B_XABS ? '$' : '\\';
}
}
switch(upd->choice[C_FORMAT]){
case FMT_ESCNMY:
if( 0 == upd->ints[I_ROWS] ){
upd->ints[I_ROWS] = 1;
}
if( 0 == upd->ints[I_PATRPT] ){
upd->ints[I_PATRPT] = 1;
}
if( upd->ints[I_PATRPT] != upd->int_a[IA_ROWMASK].size ) {
int i, *bp;
UPD_MM_DEL_PARAM(udev->memory, upd->int_a[IA_ROWMASK]);
UPD_MM_GET_ARRAY(udev->memory, bp,upd->ints[I_PATRPT]);
upd->int_a[IA_ROWMASK].size = upd->ints[I_PATRPT];
upd->int_a[IA_ROWMASK].data = bp;
for (i = 0 ; i < upd->ints[I_PATRPT] ; i++){
*bp++  = 1;
}
}
if( upd->ints[I_PATRPT] != upd->int_a[IA_SCNOFS].size ) {
int i, *bp;
UPD_MM_DEL_PARAM(udev->memory, upd->int_a[IA_SCNOFS]);
UPD_MM_GET_ARRAY(udev->memory, bp,upd->ints[I_PATRPT]);
upd->int_a[IA_SCNOFS].size = upd->ints[I_PATRPT];
upd->int_a[IA_SCNOFS].data = bp;
for (i = 0 ; i < upd->ints[I_PATRPT] ; i++){
*bp++  = i;
}
}
break;
case FMT_ESCP2Y:
case FMT_ESCP2XY:
break;
}
if((0 == upd->string_a[SA_WRITECOMP].size) &&
(0 == upd->string_a[SA_SETCOMP].size  )   ) {
byte *bp;
gs_param_string *ap;
int   i;
if(4 == upd->ocomp) {
UPD_MM_DEL_APARAM(udev->memory, upd->string_a[SA_SETCOMP]);
UPD_MM_GET_ARRAY(udev->memory, ap,4);
upd->string_a[SA_SETCOMP].data = ap;
upd->string_a[SA_SETCOMP].size = 4;
for(i = 0; i < 4; ++i) {
UPD_MM_GET_ARRAY(udev->memory, bp,3);
ap[i].size = 3;
ap[i].data = bp;
*bp++ = 0x1b;
*bp++ = 'r';
switch(((updcomp_p)upd->valptr[i])->cmap) {
case 0: *bp++ = 0; break;
case 1: *bp++ = 2; break;
case 2: *bp++ = 1; break;
case 3: *bp++ = 4; break;
}
}
}
UPD_MM_DEL_APARAM(udev->memory, upd->string_a[SA_WRITECOMP]);
UPD_MM_GET_ARRAY(udev->memory, ap,upd->ocomp);
upd->string_a[SA_WRITECOMP].data = ap;
upd->string_a[SA_WRITECOMP].size = upd->ncomp;
for(i = 0; i < upd->ocomp; ++i) {
UPD_MM_GET_ARRAY(udev->memory, bp,6);
ap[i].size = 6;
ap[i].data = bp;
*bp++ = 0x1b;
*bp++ = '.';
*bp++ =  1;
switch(upd->choice[C_FORMAT]){
case FMT_ESCP2Y:
case FMT_ESCP2XY:
*bp++ = (byte)(3600.0 * upd->ints[I_NYPASS] /
udev->y_pixels_per_inch + 0.5);
*bp++ = (byte)(3600.0 * upd->ints[I_NXPASS] /
udev->x_pixels_per_inch + 0.5);
*bp++ = upd->ints[I_PINS2WRITE];
break;
case FMT_ESCNMY:
*bp++ = 10;
*bp++ = 10;
*bp++ = upd->ints[I_ROWS];
break;
}
}
}
if(upd->ocomp > upd->string_a[SA_WRITECOMP].size) {
#if UPD_MESSAGES & UPD_M_WARNING
errprintf(
"ESC/P2-Open: WRITECOMP-Commands must be given\n");
#endif
error = -1;
}
switch(upd->choice[C_FORMAT]) {
case FMT_ESCP2Y:
if(1 < upd->ints[I_NXPASS]) {
#if         UPD_MESSAGES & UPD_M_WARNING
errprintf(
"ESC/P2-Open: FMT_ESCP2Y cannot handle multiple X-Passes\n");
#endif
error = -1;
} else {
upd->writer = upd_wrtescp2;
}
break;
case FMT_ESCP2XY:
upd->writer  = upd_wrtescp2x;
upd->nlimits = upd->ints[I_NXPASS];
#if      UPD_MESSAGES & UPD_M_WARNING
if(1 == upd->ints[I_NXPASS])
errprintf(
"ESC/P2-Open: FMT_ESCP2XY should not be used with 1X-Pass\n");
#endif
break;
case FMT_ESCNMY:
if(1 < upd->ints[I_NXPASS]) {
#if         UPD_MESSAGES & UPD_M_WARNING
errprintf(
"ESC/P2-Open: FMT_ESCNMY cannot handle multiple X-Passes\n");
#endif
error = -1;
} else {
upd->writer = upd_wrtescnm;
}
break;
default:
#if      UPD_MESSAGES & UPD_M_WARNING
errprintf(
"ESC/P2-Open: %d is not a ESC/P2-Format\n",
upd->choice[C_FORMAT]);
#endif
error = - 1;
break;
}
if(0 <= error) {
int32_t i,noutbuf,need;
if(0 < upd->strings[S_YMOVE].size) {
noutbuf = upd->strings[S_YMOVE].size + 2;
} else {
int nmax = upd->pheight;
if(      1 < upd->ints[I_YSTEP]) nmax /=  upd->ints[I_YSTEP];
else if(-1 > upd->ints[I_YSTEP]) nmax *= -upd->ints[I_YSTEP];
noutbuf  = 2 * upd->strings[S_SETLF].size + 2;
noutbuf += nmax/255 + 1;
}
if(1 < upd->ints[I_YSTEP])
noutbuf += (upd->ints[I_YSTEP]-1) * upd->strings[S_YSTEP].size;
if(0 == upd->strings[S_XMOVE].size) {
noutbuf += 1;
noutbuf += (upd->ints[I_NXPASS]-1) * upd->strings[S_XSTEP].size;
} else {
noutbuf +=  upd->strings[S_XMOVE].size + 2;
if(1 < upd->ints[I_XSTEP])
noutbuf += (upd->ints[I_XSTEP]-1) * upd->strings[S_XSTEP].size;
}
if(0 < upd->string_a[SA_SETCOMP].size) {
need = 0;
for(i = 0; i < upd->ocomp; ++i)
if(need < upd->string_a[SA_SETCOMP].data[i].size)
need = upd->string_a[SA_SETCOMP].data[i].size;
noutbuf += need;
}
need = 0;
for(i = 0; i < upd->ocomp; ++i)
if(need < upd->string_a[SA_WRITECOMP].data[i].size)
need = upd->string_a[SA_WRITECOMP].data[i].size;
noutbuf += need + 2;
noutbuf += 2*upd->nbytes + (upd->nbytes + 127) / 128;
upd->noutbuf      = noutbuf;
error             = 1;
}
return error;
}
private int
upd_wrtescp2(upd_p upd, FILE *out)
{
int  pinbot,pin,pintop,xbegin,x,xend,icomp,ybegin,yend,y,ioutbuf,n;
byte *obytes;
updscan_p scan;
if(upd->yscan < upd->ints[I_BEG_Y]) {
pintop = 0;
pinbot = upd->int_a[IA_BEGBOT].data[upd->ipass];
} else if(upd->yscan >= upd->ints[I_END_Y]) {
pinbot = upd->ints[I_PINS2WRITE];
pintop = pinbot - upd->int_a[IA_ENDTOP].data[upd->ipass];
} else {
pintop = 0;
pinbot = upd->ints[I_PINS2WRITE];
}
ybegin =  pintop * upd->ints[I_NYPASS] + upd->yscan - upd->ints[I_BEGSKIP];
yend   =  pinbot * upd->ints[I_NYPASS] + upd->yscan - upd->ints[I_BEGSKIP];
xbegin = upd->nbytes;
xend   = -1;
for(y = ybegin; y < yend; y += upd->ints[I_NYPASS]) {
if(0 > y) continue;
scan = upd->scnbuf[y & upd->scnmsk];
for(icomp = 0; icomp < upd->ocomp; ++icomp) {
obytes = scan[icomp].bytes;
for(x = 0; x < xbegin && !obytes[x]; x++);
if(x < xbegin) xbegin = x;
if(x < upd->nbytes) {
for(x = upd->nbytes-1; x > xend && !obytes[x]; x--);
if(x > xend) xend = x;
}
}
}
if(xbegin <= xend) {
ioutbuf = 0;
if(0 == upd->strings[S_XMOVE].size) xbegin = 0;
if(upd->yscan != upd->yprinter) {
if(B_YABS & upd->flags) y = upd->yscan + upd->ints[I_YOFS];
else                    y = upd->yscan - upd->yprinter;
if(      1 < upd->ints[I_YSTEP]) {
n      =  y / upd->ints[I_YSTEP];
y     -=  n * upd->ints[I_YSTEP];
} else if(-1 > upd->ints[I_YSTEP]) {
n      = y * -upd->ints[I_YSTEP];
y      = 0;
} else {
n      = y;
y      = 0;
}
if(n) {
memcpy(upd->outbuf+ioutbuf,
upd->strings[S_YMOVE].data,upd->strings[S_YMOVE].size);
ioutbuf += upd->strings[S_YMOVE].size;
upd->outbuf[ioutbuf++] =  n     & 0xff;
upd->outbuf[ioutbuf++] = (n>>8) & 0xff;
}
if(0 < upd->strings[S_YSTEP].size) {
while(y--) {
memcpy(upd->outbuf+ioutbuf,
upd->strings[S_YSTEP].data,
upd->strings[S_YSTEP].size);
ioutbuf += upd->strings[S_YSTEP].size;
}
}
upd->yprinter = upd->yscan;
}
for(icomp = 0; icomp < upd->ocomp; ++icomp) {
for(y = ybegin; y < yend; y += upd->ints[I_NYPASS]) {
if(0 > y) continue;
obytes = upd->scnbuf[y & upd->scnmsk][icomp].bytes;
for(x = xbegin; x <= xend && !obytes[x]; ++x);
if(             x <= xend) break;
}
if(y >= yend) continue;
if((0 < upd->string_a[SA_SETCOMP].size) &&
(upd->icomp != icomp               )   ) {
upd->icomp = icomp;
if(0 < upd->string_a[SA_SETCOMP].data[icomp].size) {
memcpy(upd->outbuf+ioutbuf,
upd->string_a[SA_SETCOMP].data[icomp].data,
upd->string_a[SA_SETCOMP].data[icomp].size);
ioutbuf += upd->string_a[SA_SETCOMP].data[icomp].size;
}
}
if(xbegin != upd->xprinter) {
if(0 == upd->strings[S_XMOVE].size) {
upd->outbuf[ioutbuf++] = '\r';
upd->xprinter          =  0;
n = 0;
x = 0;
} else {
if(B_XABS & upd->flags) n = x = xbegin + upd->ints[I_XOFS];
else                    n = x = xbegin - upd->xprinter;
if(        1 < upd->ints[I_XSTEP]) {
if(0 > n) {
n  -= upd->ints[I_XSTEP];
x  -= n;
}
if(n) n  /= upd->ints[I_XSTEP];
if(x) x  %= upd->ints[I_XSTEP];
} else if(-1 > upd->ints[I_XSTEP]) {
n *= -upd->ints[I_XSTEP];
x  = 0;
}
if(n) {
memcpy(upd->outbuf+ioutbuf,
upd->strings[S_XMOVE].data,
upd->strings[S_XMOVE].size);
ioutbuf += upd->strings[S_XMOVE].size;
upd->outbuf[ioutbuf++] =  n     & 0xff;
upd->outbuf[ioutbuf++] = (n>>8) & 0xff;
}
}
if(x && 0 < upd->strings[S_XSTEP].size) {
while(x--) {
memcpy(upd->outbuf+ioutbuf,
upd->strings[S_XSTEP].data,
upd->strings[S_XSTEP].size);
ioutbuf += upd->strings[S_XSTEP].size;
}
}
}
upd->xprinter = xend+1;
if(0 < upd->string_a[SA_WRITECOMP].data[icomp].size) {
memcpy(upd->outbuf+ioutbuf,
upd->string_a[SA_WRITECOMP].data[icomp].data,
upd->string_a[SA_WRITECOMP].data[icomp].size);
ioutbuf += upd->string_a[SA_WRITECOMP].data[icomp].size;
}
n = xend + 1 - xbegin;
upd->outbuf[ioutbuf++] = (n<<3) & 255;
upd->outbuf[ioutbuf++] = (n>>5) & 255;
for(pin = 0; pin < pintop; ++pin) {
ioutbuf += upd_rle(upd->outbuf+ioutbuf,NULL,n);
fwrite(upd->outbuf,1,ioutbuf,out);
ioutbuf = 0;
}
for(y = ybegin; 0 > y;    y += upd->ints[I_NYPASS]) {
ioutbuf += upd_rle(upd->outbuf+ioutbuf,NULL,n);
fwrite(upd->outbuf,1,ioutbuf,out);
ioutbuf = 0;
}
for(; y < yend; y += upd->ints[I_NYPASS]) {
ioutbuf += upd_rle(upd->outbuf+ioutbuf,
upd->scnbuf[y & upd->scnmsk][icomp].bytes+xbegin,n);
fwrite(upd->outbuf,1,ioutbuf,out);
ioutbuf = 0;
}
for(pin = pinbot; pin < upd->ints[I_PINS2WRITE]; ++pin) {
ioutbuf += upd_rle(upd->outbuf+ioutbuf,NULL,n);
fwrite(upd->outbuf,1,ioutbuf,out);
ioutbuf = 0;
}
}
}
if(upd->yscan < upd->ints[I_BEG_Y]) {
upd->yscan += upd->int_a[IA_BEG_DY].data[upd->ipass++];
if(     upd->ints[I_BEG_Y] <= upd->yscan) upd->ipass = 0;
else if(upd->int_a[IA_BEG_DY].size <= upd->ipass) upd->ipass = 0;
} else if(upd->yscan >= upd->ints[I_END_Y]) {
upd->yscan += upd->int_a[IA_END_DY].data[upd->ipass++];
if(upd->int_a[IA_END_DY].size <= upd->ipass) upd->ipass = 0;
} else {
upd->yscan += upd->int_a[IA_STD_DY].data[upd->ipass++];
if(upd->int_a[IA_STD_DY].size <= upd->ipass) upd->ipass = 0;
if(upd->yscan >= upd->ints[I_END_Y])         upd->ipass = 0;
}
return 0;
}
private int
upd_wrtescnm(upd_p upd, FILE *out)
{
int  pinbot,pin,pintop,xbegin,x,xend,icomp,ybegin,yend,y,ioutbuf,n;
int  irow,imask,iyofs;
byte *obytes;
updscan_p scan;
if(upd->yscan < upd->ints[I_BEG_Y]) {
pintop = 0;
pinbot = upd->int_a[IA_BEGBOT].data[upd->ipass];
} else if(upd->yscan >= upd->ints[I_END_Y]) {
pinbot = upd->ints[I_PINS2WRITE];
pintop = pinbot - upd->int_a[IA_ENDTOP].data[upd->ipass];
} else {
pintop = 0;
pinbot = upd->ints[I_PINS2WRITE];
}
ybegin =  pintop * upd->ints[I_NYPASS] + upd->yscan - upd->ints[I_BEGSKIP];
yend   =  pinbot * upd->ints[I_NYPASS] + upd->yscan - upd->ints[I_BEGSKIP];
xbegin = upd->nbytes;
xend   = -1;
for(y = ybegin; y < yend; y += upd->ints[I_NYPASS]) {
if(0 > y) continue;
scan = upd->scnbuf[y & upd->scnmsk];
for(icomp = 0; icomp < upd->ocomp; ++icomp) {
obytes = scan[icomp].bytes;
for(x = 0; x < xbegin && !obytes[x]; x++);
if(x < xbegin) xbegin = x;
if(x < upd->nbytes) {
for(x = upd->nbytes-1; x > xend && !obytes[x]; x--);
if(x > xend) xend = x;
}
}
}
if(xbegin <= xend) {
ioutbuf = 0;
if(0 == upd->strings[S_XMOVE].size) xbegin = 0;
if(upd->yscan != upd->yprinter) {
if(B_YABS & upd->flags) y = upd->yscan + upd->ints[I_YOFS];
else                    y = upd->yscan - upd->yprinter;
if(      1 < upd->ints[I_YSTEP]) {
n      =  y / upd->ints[I_YSTEP];
y     -=  n * upd->ints[I_YSTEP];
} else if(-1 > upd->ints[I_YSTEP]) {
n      = y * -upd->ints[I_YSTEP];
y      = 0;
} else {
n      = y;
y      = 0;
}
if(n) {
memcpy(upd->outbuf+ioutbuf,
upd->strings[S_YMOVE].data,upd->strings[S_YMOVE].size);
ioutbuf += upd->strings[S_YMOVE].size;
upd->outbuf[ioutbuf++] =  n     & 0xff;
upd->outbuf[ioutbuf++] = (n>>8) & 0xff;
}
if(0 < upd->strings[S_YSTEP].size) {
while(y--) {
memcpy(upd->outbuf+ioutbuf,
upd->strings[S_YSTEP].data,
upd->strings[S_YSTEP].size);
ioutbuf += upd->strings[S_YSTEP].size;
}
}
upd->yprinter = upd->yscan;
}
icomp=0;
if((0 < upd->string_a[SA_SETCOMP].size) ) {
upd->icomp = icomp;
if(0 < upd->string_a[SA_SETCOMP].data[icomp].size) {
memcpy(upd->outbuf+ioutbuf,
upd->string_a[SA_SETCOMP].data[icomp].data,
upd->string_a[SA_SETCOMP].data[icomp].size);
ioutbuf += upd->string_a[SA_SETCOMP].data[icomp].size;
}
}
if(xbegin != upd->xprinter) {
if(0 == upd->strings[S_XMOVE].size) {
upd->outbuf[ioutbuf++] = '\r';
upd->xprinter          =  0;
n = 0;
x = 0;
} else {
if(B_XABS & upd->flags) n = x = xbegin + upd->ints[I_XOFS];
else                    n = x = xbegin - upd->xprinter;
if(        1 < upd->ints[I_XSTEP]) {
if(0 > n) {
n  -= upd->ints[I_XSTEP];
x  -= n;
}
if(n) n  /= upd->ints[I_XSTEP];
if(x) x  %= upd->ints[I_XSTEP];
} else if(-1 > upd->ints[I_XSTEP]) {
n *= -upd->ints[I_XSTEP];
x  = 0;
}
if(n) {
memcpy(upd->outbuf+ioutbuf,
upd->strings[S_XMOVE].data,
upd->strings[S_XMOVE].size);
ioutbuf += upd->strings[S_XMOVE].size;
upd->outbuf[ioutbuf++] =  n     & 0xff;
upd->outbuf[ioutbuf++] = (n>>8) & 0xff;
}
}
if(x && 0 < upd->strings[S_XSTEP].size) {
while(x--) {
memcpy(upd->outbuf+ioutbuf,
upd->strings[S_XSTEP].data,
upd->strings[S_XSTEP].size);
ioutbuf += upd->strings[S_XSTEP].size;
}
}
}
upd->xprinter = xend+1;
if(0 < upd->string_a[SA_WRITECOMP].data[icomp].size) {
memcpy(upd->outbuf+ioutbuf,
upd->string_a[SA_WRITECOMP].data[icomp].data,
upd->string_a[SA_WRITECOMP].data[icomp].size);
ioutbuf += upd->string_a[SA_WRITECOMP].data[icomp].size;
}
n = xend + 1 - xbegin;
upd->outbuf[ioutbuf++] = (n<<3) & 255;
upd->outbuf[ioutbuf++] = (n>>5) & 255;
irow=0;
for(pin = 0; pin < pintop; ++pin) {
int i;
for(i=0 ; i < upd->ints[I_PATRPT]; i++){
if(irow >= upd->ints[I_ROWS]) break;
ioutbuf += upd_rle(upd->outbuf+ioutbuf,NULL,n);
fwrite(upd->outbuf,1,ioutbuf,out);
irow++;
ioutbuf = 0;
}
}
for(y = ybegin; 0 > y;    y += upd->ints[I_NYPASS]) {
int i;
for(i=0 ; i < upd->ints[I_PATRPT]; i++){
if(irow >= upd->ints[I_ROWS]) break;
ioutbuf += upd_rle(upd->outbuf+ioutbuf,NULL,n);
fwrite(upd->outbuf,1,ioutbuf,out);
ioutbuf = 0;
irow++;
}
}
for(; y < yend; y += upd->ints[I_NYPASS]) {
int i,masklen=upd->ints[I_PATRPT],yinc=0;
for(i=0 ; (i < upd->ints[I_PATRPT]); i++){
if(irow >= upd->ints[I_ROWS]) break;
imask = irow%masklen;
icomp = upd->int_a[IA_ROWMASK].data[imask];
if(icomp == 0) {
ioutbuf += upd_rle(upd->outbuf+ioutbuf,NULL,n);
} else {
--icomp;
iyofs = upd->int_a[IA_SCNOFS].data[imask];
ioutbuf += upd_rle(upd->outbuf+ioutbuf,
upd->scnbuf[(y+iyofs) & upd->scnmsk][icomp].bytes+xbegin,n);
yinc+=upd->ints[I_NYPASS];
}
fwrite(upd->outbuf,1,ioutbuf,out);
ioutbuf = 0;
irow++;
}
if (upd->ints[I_NYPASS] < upd->ints[I_PATRPT]) {
y+=yinc;
if (y > 0)
y-=upd->ints[I_NYPASS];
}
}
for(pin = pinbot; pin < upd->ints[I_PINS2WRITE]; ++pin) {
int i;
for(i=0 ; i < upd->ints[I_PATRPT]; i++){
if(irow >= upd->ints[I_ROWS]) break;
ioutbuf += upd_rle(upd->outbuf+ioutbuf,NULL,n);
fwrite(upd->outbuf,1,ioutbuf,out);
ioutbuf = 0;
irow++;
}
}
if (irow < upd->ints[I_ROWS]) {
for( ; irow < upd->ints[I_ROWS]; irow++){
ioutbuf += upd_rle(upd->outbuf+ioutbuf,NULL,n);
fwrite(upd->outbuf,1,ioutbuf,out);
ioutbuf = 0;
}
}
}
if(upd->yscan < upd->ints[I_BEG_Y]) {
upd->yscan += upd->int_a[IA_BEG_DY].data[upd->ipass++];
if(     upd->ints[I_BEG_Y] <= upd->yscan) upd->ipass = 0;
else if(upd->int_a[IA_BEG_DY].size <= upd->ipass) upd->ipass = 0;
} else if(upd->yscan >= upd->ints[I_END_Y]) {
upd->yscan += upd->int_a[IA_END_DY].data[upd->ipass++];
if(upd->int_a[IA_END_DY].size <= upd->ipass) upd->ipass = 0;
} else {
upd->yscan += upd->int_a[IA_STD_DY].data[upd->ipass++];
if(upd->int_a[IA_STD_DY].size <= upd->ipass) upd->ipass = 0;
if(upd->yscan >= upd->ints[I_END_Y])         upd->ipass = 0;
}
return 0;
}
private int
upd_wrtescp2x(upd_p upd, FILE *out)
{
int  pinbot,pin,pintop,xbegin,x,xend,icomp,ybegin,yend,y,ioutbuf,n,ixpass;
byte *obytes,bit;
updscan_p scan;
if(upd->yscan < upd->ints[I_BEG_Y]) {
ixpass = upd->int_a[IA_BEG_IX].data[upd->ipass];
pintop = 0;
pinbot = upd->int_a[IA_BEGBOT].data[upd->ipass];
} else if(upd->yscan >= upd->ints[I_END_Y]) {
ixpass = upd->int_a[IA_END_IX].data[upd->ipass];
pinbot = upd->ints[I_PINS2WRITE];
pintop = pinbot - upd->int_a[IA_ENDTOP].data[upd->ipass];
} else {
ixpass = upd->int_a[IA_STD_IX].data[upd->ipass];
pintop = 0;
pinbot = upd->ints[I_PINS2WRITE];
}
ybegin =  pintop * upd->ints[I_NYPASS] + upd->yscan - upd->ints[I_BEGSKIP];
yend   =  pinbot * upd->ints[I_NYPASS] + upd->yscan - upd->ints[I_BEGSKIP];
xbegin = upd->pwidth;
xend   = -1;
for(y = ybegin; y < yend; y += upd->ints[I_NYPASS]) {
if(0 > y) continue;
scan = upd->scnbuf[y & upd->scnmsk];
for(icomp = 0; icomp < upd->ocomp; ++icomp) {
if(xbegin > scan[icomp].xbegin[ixpass])
xbegin = scan[icomp].xbegin[ixpass];
if(xend   < scan[icomp].xend[  ixpass])
xend   = scan[icomp].xend[  ixpass];
}
}
if(xbegin <= xend) {
ioutbuf = upd->nbytes;
if(0 == upd->strings[S_XMOVE].size) xbegin = ixpass;
if(upd->yscan != upd->yprinter) {
if(B_YABS & upd->flags) y = upd->yscan + upd->ints[I_YOFS];
else                    y = upd->yscan - upd->yprinter;
if(      1 < upd->ints[I_YSTEP]) {
n      =  y / upd->ints[I_YSTEP];
y     -=  n * upd->ints[I_YSTEP];
} else if(-1 > upd->ints[I_YSTEP]) {
n      = y * -upd->ints[I_YSTEP];
y      = 0;
} else {
n      = y;
y      = 0;
}
if(n) {
memcpy(upd->outbuf+ioutbuf,
upd->strings[S_YMOVE].data,upd->strings[S_YMOVE].size);
ioutbuf += upd->strings[S_YMOVE].size;
upd->outbuf[ioutbuf++] =  n     & 0xff;
upd->outbuf[ioutbuf++] = (n>>8) & 0xff;
}
if(0 < upd->strings[S_YSTEP].size) {
while(y--) {
memcpy(upd->outbuf+ioutbuf,
upd->strings[S_YSTEP].data,
upd->strings[S_YSTEP].size);
ioutbuf += upd->strings[S_YSTEP].size;
}
}
upd->yprinter = upd->yscan;
}
for(icomp = 0; icomp < upd->ocomp; ++icomp) {
for(y = ybegin; y < yend; y += upd->ints[I_NYPASS]) {
if(0 > y) continue;
scan = upd->scnbuf[y & upd->scnmsk]+icomp;
if(0 <= scan->xend[ixpass]) break;
}
if(y >= yend) continue;
if((0 < upd->string_a[SA_SETCOMP].size) &&
(upd->icomp != icomp               )   ) {
upd->icomp = icomp;
if(0 < upd->string_a[SA_SETCOMP].data[icomp].size) {
memcpy(upd->outbuf+ioutbuf,
upd->string_a[SA_SETCOMP].data[icomp].data,
upd->string_a[SA_SETCOMP].data[icomp].size);
ioutbuf += upd->string_a[SA_SETCOMP].data[icomp].size;
}
}
if(xbegin != upd->xprinter) {
if(0 == upd->strings[S_XMOVE].size) {
upd->outbuf[ioutbuf++] = '\r';
upd->xprinter          =  0;
n = 0;
x = ixpass;
} else {
if(B_XABS & upd->flags) n = x = xbegin + upd->ints[I_XOFS];
else                    n = x = xbegin - upd->xprinter;
if(        1 < upd->ints[I_XSTEP]) {
if(0 > n) {
n  -= upd->ints[I_XSTEP];
x  -= n;
}
if(n) n  /= upd->ints[I_XSTEP];
if(x) x  %= upd->ints[I_XSTEP];
} else if(-1 > upd->ints[I_XSTEP]) {
n *= -upd->ints[I_XSTEP];
x  = 0;
}
if(n) {
memcpy(upd->outbuf+ioutbuf,
upd->strings[S_XMOVE].data,
upd->strings[S_XMOVE].size);
ioutbuf += upd->strings[S_XMOVE].size;
upd->outbuf[ioutbuf++] =  n     & 0xff;
upd->outbuf[ioutbuf++] = (n>>8) & 0xff;
}
}
if(x && 0 < upd->strings[S_XSTEP].size) {
while(x--) {
memcpy(upd->outbuf+ioutbuf,
upd->strings[S_XSTEP].data,
upd->strings[S_XSTEP].size);
ioutbuf += upd->strings[S_XSTEP].size;
}
}
}
upd->xprinter = xend+1;
if(0 < upd->string_a[SA_WRITECOMP].data[icomp].size) {
memcpy(upd->outbuf+ioutbuf,
upd->string_a[SA_WRITECOMP].data[icomp].data,
upd->string_a[SA_WRITECOMP].data[icomp].size);
ioutbuf += upd->string_a[SA_WRITECOMP].data[icomp].size;
}
n = ((xend - xbegin) / upd->ints[I_NXPASS] + 8) & ~7;
upd->outbuf[ioutbuf++] =  n     & 255;
upd->outbuf[ioutbuf++] = (n>>8) & 255;
n >>= 3;
for(pin = 0; pin < pintop; ++pin) {
ioutbuf += upd_rle(upd->outbuf+ioutbuf,NULL,n);
fwrite(upd->outbuf+upd->nbytes,1,ioutbuf-upd->nbytes,out);
ioutbuf = upd->nbytes;
}
for(y = ybegin; 0 > y;    y += upd->ints[I_NYPASS]) {
ioutbuf += upd_rle(upd->outbuf+ioutbuf,NULL,n);
fwrite(upd->outbuf+upd->nbytes,1,ioutbuf-upd->nbytes,out);
ioutbuf = upd->nbytes;
}
for(;           y < yend; y += upd->ints[I_NYPASS]) {
byte * ibytes = upd->scnbuf[y & upd->scnmsk][icomp].bytes;
obytes = upd->outbuf;
memset(obytes,0,upd->nbytes);
bit = 0x80;
for(x = xbegin; x <= xend; x += upd->ints[I_NXPASS]) {
if(ibytes[x>>3] & (0x80 >> (x & 7))) *obytes |= bit;
if(!(bit >>= 1)) { obytes++; bit = 0x80; }
}
ioutbuf += upd_rle(upd->outbuf+ioutbuf,upd->outbuf,n);
fwrite(upd->outbuf+upd->nbytes,1,ioutbuf-upd->nbytes,out);
ioutbuf = upd->nbytes;
}
for(pin = pinbot; pin < upd->ints[I_PINS2WRITE]; ++pin) {
ioutbuf += upd_rle(upd->outbuf+ioutbuf,NULL,n);
fwrite(upd->outbuf+upd->nbytes,1,ioutbuf-upd->nbytes,out);
ioutbuf = upd->nbytes;
}
}
}
if(upd->yscan < upd->ints[I_BEG_Y]) {
upd->yscan += upd->int_a[IA_BEG_DY].data[upd->ipass++];
if(     upd->ints[I_BEG_Y] <= upd->yscan) upd->ipass = 0;
else if(upd->int_a[IA_BEG_DY].size <= upd->ipass) upd->ipass = 0;
} else if(upd->yscan >= upd->ints[I_END_Y]) {
upd->yscan += upd->int_a[IA_END_DY].data[upd->ipass++];
if(upd->int_a[IA_END_DY].size <= upd->ipass) upd->ipass = 0;
} else {
upd->yscan += upd->int_a[IA_STD_DY].data[upd->ipass++];
if(upd->int_a[IA_STD_DY].size <= upd->ipass) upd->ipass = 0;
if(upd->yscan >= upd->ints[I_END_Y])         upd->ipass = 0;
}
return 0;
}
private int
upd_rle(byte *out,const byte *in,int nbytes)
{
int used = 0;
int crun,cdata;
byte run;
if(in != NULL) {
crun = 1;
while(nbytes > 0) {
run = in[0];
while((nbytes > crun) && (run == in[crun])) if(++crun == 128) break;
if((crun > 2) || (crun == nbytes)) {
*out++  = (257 - crun) & 0xff; *out++ = run; used += 2;
nbytes -= crun; in    += crun;
crun = 1;
} else {
for(cdata = crun; (nbytes > cdata) && (crun < 4);) {
if(run  == in[cdata]) crun += 1;
else run = in[cdata], crun  = 1;
if(++cdata == 128) break;
}
if(crun < 3) crun   = 0;
else         cdata -= crun;
*out++ = cdata-1;     used++;
memcpy(out,in,cdata); used += cdata; out   += cdata;
nbytes -= cdata; in    += cdata;
}
}
} else {
while(nbytes > 0) {
crun    = nbytes > 128 ? 128 : nbytes;
nbytes -= crun;
*out++  = (257 - crun) & 0xff;
*out++  = 0;
used   += 2;
}
}
return used;
}
private int
upd_open_wrtrtl(upd_device *udev)
{
const upd_p      upd  = udev->upd;
int              error = 0;
if(0 < upd->strings[S_BEGIN].size) {
int   i,j,state;
char  cv[24];
byte  *bp;
uint  ncv,nbp;
j     = -1;
state = 0;
for(i = 0; i < upd->strings[S_BEGIN].size; ++i) {
const int c = upd->strings[S_BEGIN].data[i];
switch(state) {
case  0:
if(        c == 0x1b) state =  1;
break;
case  1:
if(        c == 0x2a) state =  2;
else if(   c == 0x25) state =  5;
else                  state =  0;
break;
case  2:
j = i;
if(        c == 0x72) state =  3;
else if(   c == 0x74) state =  4;
else                  state =  0;
break;
case  3:
if(       (B_PAGEWIDTH  & upd->flags) &&
((c == 0x73) || (c == 0x53))  ) {
sprintf(cv,"%d",upd->pwidth);
ncv = strlen(cv);
nbp = (j+1) + ncv + (upd->strings[S_BEGIN].size-i);
UPD_MM_GET_ARRAY(udev->memory, bp,nbp);
if(0 <= j) memcpy(bp,upd->strings[S_BEGIN].data,j+1);
memcpy(bp+j+1,    cv,ncv);
memcpy(bp+j+1+ncv,upd->strings[S_BEGIN].data+i,
upd->strings[S_BEGIN].size-i);
i = j+1+ncv;
UPD_MM_DEL_PARAM(udev->memory, upd->strings[S_BEGIN]);
upd->strings[S_BEGIN].data = bp;
upd->strings[S_BEGIN].size = nbp;
} else if((B_PAGELENGTH & upd->flags) &&
((c == 0x74) || (c == 0x54))  ) {
sprintf(cv,"%d",upd->pheight);
ncv = strlen(cv);
nbp = (j+1) + ncv + (upd->strings[S_BEGIN].size-i);
UPD_MM_GET_ARRAY(udev->memory, bp,nbp);
if(0 <= j) memcpy(bp,upd->strings[S_BEGIN].data,j+1);
memcpy(bp+j+1,    cv,ncv);
memcpy(bp+j+1+ncv,upd->strings[S_BEGIN].data+i,
upd->strings[S_BEGIN].size-i);
i = j+1+ncv;
UPD_MM_DEL_PARAM(udev->memory, upd->strings[S_BEGIN]);
upd->strings[S_BEGIN].data = bp;
upd->strings[S_BEGIN].size = nbp;
}
if(       (0x40 < c) && (c < 0x5b))  state = 0;
else if(!((0x2f < c) && (c < 0x3a))) j     = i;
break;
case  4:
if(        (B_RESOLUTION  & upd->flags) &&
((c == 0x72) || (c == 0x52))  ) {
sprintf(cv,"%d",(int)
((udev->y_pixels_per_inch < udev->x_pixels_per_inch ?
udev->x_pixels_per_inch : udev->y_pixels_per_inch)
+0.5));
ncv = strlen(cv);
nbp = (j+1) + ncv + (upd->strings[S_BEGIN].size-i);
UPD_MM_GET_ARRAY(udev->memory, bp,nbp);
if(0 <= j) memcpy(bp,upd->strings[S_BEGIN].data,j+1);
memcpy(bp+j+1,    cv,ncv);
memcpy(bp+j+1+ncv,upd->strings[S_BEGIN].data+i,
upd->strings[S_BEGIN].size-i);
i = j+1+ncv;
UPD_MM_DEL_PARAM(udev->memory, upd->strings[S_BEGIN]);
upd->strings[S_BEGIN].data = bp;
upd->strings[S_BEGIN].size = nbp;
}
if(       (0x40 < c) && (c < 0x5b))  state = 0;
else if(!((0x2f < c) && (c < 0x3a))) j     = i;
break;
case  5:
if( c == 0x2d) state =  6;
else           state =  0;
break;
case  6:
if( c == 0x31) state =  7;
else           state =  0;
break;
case  7:
if( c == 0x32) state =  8;
else           state =  0;
break;
case  8:
if( c == 0x33) state =  9;
else           state =  0;
break;
case  9:
if( c == 0x34) state = 10;
else           state =  0;
break;
case 10:
if( c == 0x35) state = 11;
else           state =  0;
break;
case 11:
if( c == 0x58) state = 12;
else           state =  0;
break;
case 12:
if( c == 0x40) state = 13;
else           state =  0;
break;
case 13:
if( c == 0x50) state = 14;
else           state =  0;
break;
case 14:
if( c == 0x4a) state = 15;
else           state =  0;
break;
case 15:
if( c == 0x4c) state = 16;
else           state =  0;
break;
case 16:
if((c == 0x20) || (c == 0x09)) state = 19;
else if(           c == 0x0d ) state = 17;
else if(           c == 0x0a ) state = 12;
else                           state =  0;
break;
case 17:
if( c == 0x0a) state = 12;
else           state =  0;
break;
case 18:
if( c == 0x0a) state = 12;
break;
case 19:
if(     (c == 0x53) || (c == 0x73)) state = 20;
else if( c == 0x0a                ) state = 12;
else if( c == 0x0d                ) state = 17;
break;
case 20:
if(     (c == 0x45) || (c == 0x65)) state = 21;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 21:
if(     (c == 0x54) || (c == 0x74)) state = 22;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 22:
if(     (c == 0x20) || (c == 0x09)) state = 23;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 23:
if(     (c == 0x50) || (c == 0x70)) state = 24;
else if((c == 0x52) || (c == 0x72)) state = 41;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 24:
if(     (c == 0x41) || (c == 0x61)) state = 25;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 25:
if(     (c == 0x50) || (c == 0x70)) state = 26;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 26:
if(     (c == 0x45) || (c == 0x65)) state = 27;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 27:
if(     (c == 0x52) || (c == 0x72)) state = 28;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 28:
if(     (c == 0x4c) || (c == 0x6c)) state = 29;
else if((c == 0x57) || (c == 0x77)) state = 36;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 29:
if(     (c == 0x45) || (c == 0x65)) state = 30;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 30:
if(     (c == 0x4e) || (c == 0x6e)) state = 31;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 31:
if(     (c == 0x47) || (c == 0x67)) state = 32;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 32:
if(     (c == 0x54) || (c == 0x74)) state = 33;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 33:
if(     (c == 0x48) || (c == 0x68)) state = 34;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 34:
j = i;
if(      c == 0x3d                ) state = 51;
else if( c == 0x0a                ) state = 12;
else if((c != 0x20) && (c != 0x09)) state = 18;
break;
case 51:
if(     c == 0x0a)                  state = 12;
else if((c == 0x20) || (c == 0x09)) j     = i;
else if(( 0x30 > c) || ( c > 0x39)) state = 18;
else                                state = 35;
break;
case 35:
if((0x30 > c) || (c > 0x39)) {
if(B_PAGELENGTH  & upd->flags) {
sprintf(cv,"%d",(int)
(720.0 * udev->height / udev->y_pixels_per_inch + 0.5));
ncv = strlen(cv);
nbp = (j+1) + ncv + (upd->strings[S_BEGIN].size-i);
UPD_MM_GET_ARRAY(udev->memory, bp,nbp);
if(0 <= j) memcpy(bp,upd->strings[S_BEGIN].data,j+1);
memcpy(bp+j+1,    cv,ncv);
memcpy(bp+j+1+ncv,upd->strings[S_BEGIN].data+i,
upd->strings[S_BEGIN].size-i);
i = j+1+ncv;
UPD_MM_DEL_PARAM(udev->memory, upd->strings[S_BEGIN]);
upd->strings[S_BEGIN].data = bp;
upd->strings[S_BEGIN].size = nbp;
}
if( c == 0x0a ) state = 12;
else            state = 18;
}
break;
case 36:
if(     (c == 0x49) || (c == 0x69)) state = 37;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 37:
if(     (c == 0x44) || (c == 0x64)) state = 38;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 38:
if(     (c == 0x54) || (c == 0x74)) state = 39;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 39:
if(     (c == 0x48) || (c == 0x68)) state = 52;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 52:
j = i;
if(      c == 0x3d                ) state = 53;
else if( c == 0x0a                ) state = 12;
else if((c != 0x20) && (c != 0x09)) state = 18;
break;
case 53:
if(     c == 0x0a)                  state = 12;
else if((c == 0x20) || (c == 0x09)) j     = i;
else if(( 0x30 > c) || ( c > 0x39)) state = 18;
else                                state = 40;
break;
case 40:
if((0x30 > c) || (c > 0x39)) {
if(B_PAGEWIDTH  & upd->flags) {
sprintf(cv,"%d",(int)
(720.0 * udev->width / udev->x_pixels_per_inch + 0.5));
ncv = strlen(cv);
nbp = (j+1) + ncv + (upd->strings[S_BEGIN].size-i);
UPD_MM_GET_ARRAY(udev->memory, bp,nbp);
if(0 <= j) memcpy(bp,upd->strings[S_BEGIN].data,j+1);
memcpy(bp+j+1,    cv,ncv);
memcpy(bp+j+1+ncv,upd->strings[S_BEGIN].data+i,
upd->strings[S_BEGIN].size-i);
i = j+1+ncv;
UPD_MM_DEL_PARAM(udev->memory, upd->strings[S_BEGIN]);
upd->strings[S_BEGIN].data = bp;
upd->strings[S_BEGIN].size = nbp;
}
if( c == 0x0a ) state = 12;
else            state = 18;
}
break;
case 41:
if(     (c == 0x45) || (c == 0x65)) state = 42;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 42:
if(     (c == 0x53) || (c == 0x73)) state = 43;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 43:
if(     (c == 0x4f) || (c == 0x6f)) state = 44;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 44:
if(     (c == 0x4c) || (c == 0x6c)) state = 45;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 45:
if(     (c == 0x55) || (c == 0x75)) state = 46;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 46:
if(     (c == 0x54) || (c == 0x74)) state = 47;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 47:
if(     (c == 0x49) || (c == 0x69)) state = 48;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 48:
if(     (c == 0x4f) || (c == 0x6f)) state = 49;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 49:
if(     (c == 0x4e) || (c == 0x6e)) state = 54;
else if( c == 0x0a                ) state = 12;
else                                state = 18;
break;
case 54:
j = i;
if(      c == 0x3d                ) state = 55;
else if( c == 0x0a                ) state = 12;
else if((c != 0x20) && (c != 0x09)) state = 18;
break;
case 55:
if(     c == 0x0a)                  state = 12;
else if((c == 0x20) || (c == 0x09)) j     = i;
else if(( 0x30 > c) || ( c > 0x39)) state = 18;
else                                state = 50;
break;
case 50:
if((0x30 > c) || (c > 0x39)) {
if(B_RESOLUTION  & upd->flags) {
sprintf(cv,"%d",(int)
((udev->y_pixels_per_inch < udev->x_pixels_per_inch ?
udev->x_pixels_per_inch : udev->y_pixels_per_inch)
+0.5));
ncv = strlen(cv);
nbp = (j+1) + ncv + (upd->strings[S_BEGIN].size-i);
UPD_MM_GET_ARRAY(udev->memory, bp,nbp);
if(0 <= j) memcpy(bp,upd->strings[S_BEGIN].data,j+1);
memcpy(bp+j+1,    cv,ncv);
memcpy(bp+j+1+ncv,upd->strings[S_BEGIN].data+i,
upd->strings[S_BEGIN].size-i);
i = j+1+ncv;
UPD_MM_DEL_PARAM(udev->memory, upd->strings[S_BEGIN]);
upd->strings[S_BEGIN].data = bp;
upd->strings[S_BEGIN].size = nbp;
}
if( c == 0x0a ) state = 12;
else            state = 18;
}
break;
default:
#if UPD_MESSAGES & UPD_M_ERROR
errprintf("UNIPRINT-Coding error, wrrtl, state = %d\n",state);
#endif
state = 0;
break;
}
}
}
if(upd->ocomp > upd->string_a[SA_WRITECOMP].size) {
#if UPD_MESSAGES & UPD_M_WARNING
errprintf(
"PCL-Open: WRITECOMP-Commands must be given\n");
#endif
error = -1;
}
if(0 <= error) {
int32_t ny,noutbuf;
char  tmp[16];
if(0 < upd->strings[S_YMOVE].size) {
sprintf(tmp,"%d",upd->pheight);
ny = upd->strings[S_YMOVE].size + strlen(tmp);
} else {
ny = 1 + upd->string_a[SA_WRITECOMP].data[upd->ocomp-1].size;
ny *= upd->pheight;
}
noutbuf = upd->nbytes + (upd->nbytes + 127) / 128;
if(ny > noutbuf) noutbuf = ny;
noutbuf += 16;
if((0 < noutbuf) && (noutbuf <= INT_MAX)) {
upd->noutbuf      = noutbuf;
upd->writer       = upd_wrtrtl;
error             = 1;
} else {
error = -1;
#if      UPD_MESSAGES & UPD_M_WARNING
errprintf(
"PCL-Open: %ld is unreasonable size of Outputbuffer\n",
(long) noutbuf);
#endif
}
}
return error;
}
private int
upd_wrtrtl(upd_p upd, FILE *out)
{
const updscan_p scan = upd->scnbuf[upd->yscan & upd->scnmsk];
int  x,xend,icomp,ioutbuf;
byte *data;
xend   = -1;
for(icomp = 0; icomp < upd->ocomp; ++icomp) {
data = scan[icomp].bytes;
for(x = upd->nbytes-1; 0 <= x; --x) if(data[x]) break;
if(x > xend) xend  = x;
}
if(0 <= xend) {
ioutbuf = 0;
xend   += 1;
if(upd->yscan != upd->yprinter) {
if(1 < upd->strings[S_YMOVE].size) {
sprintf((char *)upd->outbuf+ioutbuf,
(const char *) upd->strings[S_YMOVE].data,
upd->yscan - upd->yprinter);
ioutbuf += strlen((char *)upd->outbuf+ioutbuf);
} else {
while(upd->yscan > upd->yprinter) {
for(icomp = 0; icomp < upd->ocomp; ++icomp) {
sprintf((char *)upd->outbuf+ioutbuf,
(const char *) upd->string_a[SA_WRITECOMP].data[icomp].data,0);
ioutbuf += strlen((char *)upd->outbuf+ioutbuf);
}
fwrite(upd->outbuf,1,ioutbuf,out);
ioutbuf = 0;
upd->yprinter += 1;
}
}
upd->yprinter = upd->yscan;
fwrite(upd->outbuf,1,ioutbuf,out);
ioutbuf = 0;
}
for(icomp = 0; icomp < upd->ocomp; ++icomp) {
data = scan[icomp].bytes;
for(x = 0; x <= xend; ++x) if(data[x]) break;
if(x <= xend) {
ioutbuf = upd_rle(upd->outbuf,scan[icomp].bytes,xend);
fprintf(out,
(const char *)upd->string_a[SA_WRITECOMP].data[icomp].data,ioutbuf);
fwrite(upd->outbuf,1,ioutbuf,out);
} else {
fprintf(out,
(const char *)upd->string_a[SA_WRITECOMP].data[icomp].data,0);
}
}
upd->yprinter += 1;
}
upd->yscan += 1;
return 0;
}
private int
upd_open_wrtcanon(upd_device *udev)
{
const upd_p upd = udev->upd;
int error = 0;
upd->noutbuf = upd->nbytes + (upd->nbytes + 127) / 128;
upd->writer  = upd_wrtcanon;
error        = 1;
return error;
}
#define LOW(b)     ((b)&0xFF)
#define HIGH(b)    ((b)>>8)
#define ESC 0x1B
#define CR  0x0D
private int
upd_wrtcanon(upd_p upd, FILE *out)
{
const updscan_p scan = upd->scnbuf[upd->yscan & upd->scnmsk];
int x, xend, icomp, ioutbuf, step, ioutbuf1;
byte *data;
xend = -1;
for(icomp = 0; icomp < upd->ocomp; ++icomp) {
data = scan[icomp].bytes;
for(x = upd->nbytes-1; 0 <= x; --x) if(data[x]) break;
if(x > xend) xend  = x;
}
if(0 <= xend) {
ioutbuf = 0;
xend   += 1;
if(upd->yscan != upd->yprinter) {
step = upd->yscan - upd->yprinter;
fputc(ESC,        out);
fputc('(',        out);
fputc('e',        out);
fputc(2,          out);
fputc(0,          out);
fputc(HIGH(step), out);
fputc(LOW(step),  out);
upd->yprinter = upd->yscan;
}
for(icomp = 0; icomp < upd->ocomp; ++icomp) {
data = scan[icomp].bytes;
for(x = 0; x <= xend; ++x) if(data[x]) break;
if(x <= xend) {
ioutbuf = upd_rle(upd->outbuf, scan[icomp].bytes, xend);
} else {
ioutbuf = 0;
}
ioutbuf1 = ioutbuf + 1;
fputc(ESC,            out);
fputc('(',            out);
fputc('A',            out);
fputc(LOW(ioutbuf1),  out);
fputc(HIGH(ioutbuf1), out);
switch(upd->ocomp) {
case 1:  fputc('K',out); break;
case 3:
case 4:  fputc("YMCK"[icomp],out); break;
default: fputc('K',out); break;
}
fwrite(upd->outbuf, 1, ioutbuf, out);
fputc(CR,             out);
}
fputc(ESC,        out);
fputc('(',        out);
fputc('e',        out);
fputc(2,          out);
fputc(0,          out);
fputc(HIGH(1),    out);
fputc(LOW(1),     out);
upd->yprinter += 1;
}
upd->yscan += 1;
return 0;
}
private upd_proc_pxlget(upd_pxlgetnix);
private upd_proc_pxlget(upd_pxlget1f1);
private upd_proc_pxlget(upd_pxlget1f2);
private upd_proc_pxlget(upd_pxlget1f3);
private upd_proc_pxlget(upd_pxlget1f4);
private upd_proc_pxlget(upd_pxlget1f5);
private upd_proc_pxlget(upd_pxlget1f6);
private upd_proc_pxlget(upd_pxlget1f7);
private upd_proc_pxlget(upd_pxlget1f8);
private upd_proc_pxlget(upd_pxlget1r1);
private upd_proc_pxlget(upd_pxlget1r2);
private upd_proc_pxlget(upd_pxlget1r3);
private upd_proc_pxlget(upd_pxlget1r4);
private upd_proc_pxlget(upd_pxlget1r5);
private upd_proc_pxlget(upd_pxlget1r6);
private upd_proc_pxlget(upd_pxlget1r7);
private upd_proc_pxlget(upd_pxlget1r8);
private upd_proc_pxlget(upd_pxlget2f1);
private upd_proc_pxlget(upd_pxlget2f2);
private upd_proc_pxlget(upd_pxlget2f3);
private upd_proc_pxlget(upd_pxlget2f4);
private upd_proc_pxlget(upd_pxlget2r1);
private upd_proc_pxlget(upd_pxlget2r2);
private upd_proc_pxlget(upd_pxlget2r3);
private upd_proc_pxlget(upd_pxlget2r4);
private upd_proc_pxlget(upd_pxlget4f1);
private upd_proc_pxlget(upd_pxlget4f2);
private upd_proc_pxlget(upd_pxlget4r1);
private upd_proc_pxlget(upd_pxlget4r2);
private upd_proc_pxlget(upd_pxlget8f);
private upd_proc_pxlget(upd_pxlget8r);
private upd_proc_pxlget(upd_pxlget16f);
private upd_proc_pxlget(upd_pxlget16r);
private upd_proc_pxlget(upd_pxlget24f);
private upd_proc_pxlget(upd_pxlget24r);
private upd_proc_pxlget(upd_pxlget32f);
private upd_proc_pxlget(upd_pxlget32r);
private uint32_t
upd_pxlfwd(upd_p upd)
{
if(!(upd->pxlptr = upd->gsscan)) {
upd->pxlget = upd_pxlgetnix;
} else {
switch(upd->int_a[IA_COLOR_INFO].data[1]) {
case  1: upd->pxlget = upd_pxlget1f1; break;
case  2: upd->pxlget = upd_pxlget2f1; break;
case  4: upd->pxlget = upd_pxlget4f1; break;
case  8: upd->pxlget = upd_pxlget8f;  break;
case 16: upd->pxlget = upd_pxlget16f; break;
case 24: upd->pxlget = upd_pxlget24f; break;
case 32: upd->pxlget = upd_pxlget32f; break;
default:
#if UPD_MESSAGES & UPD_M_ERROR
errprintf("upd_pxlfwd: unsupported depth (%d)\n",
upd->int_a[IA_COLOR_INFO].data[1]);
#endif
upd->pxlget = upd_pxlgetnix;
break;
}
}
return (uint32_t) 0;
}
private uint32_t
upd_pxlget1f1(upd_p upd)
{
upd->pxlget = upd_pxlget1f2;
return *upd->pxlptr   & 0x80 ? (uint32_t) 1 : (uint32_t) 0;
}
private uint32_t
upd_pxlget1f2(upd_p upd)
{
upd->pxlget = upd_pxlget1f3;
return *upd->pxlptr   & 0x40 ? (uint32_t) 1 : (uint32_t) 0;
}
private uint32_t
upd_pxlget1f3(upd_p upd)
{
upd->pxlget = upd_pxlget1f4;
return *upd->pxlptr   & 0x20 ? (uint32_t) 1 : (uint32_t) 0;
}
private uint32_t
upd_pxlget1f4(upd_p upd)
{
upd->pxlget = upd_pxlget1f5;
return *upd->pxlptr   & 0x10 ? (uint32_t) 1 : (uint32_t) 0;
}
private uint32_t
upd_pxlget1f5(upd_p upd)
{
upd->pxlget = upd_pxlget1f6;
return *upd->pxlptr   & 0x08 ? (uint32_t) 1 : (uint32_t) 0;
}
private uint32_t
upd_pxlget1f6(upd_p upd)
{
upd->pxlget = upd_pxlget1f7;
return *upd->pxlptr   & 0x04 ? (uint32_t) 1 : (uint32_t) 0;
}
private uint32_t
upd_pxlget1f7(upd_p upd)
{
upd->pxlget = upd_pxlget1f8;
return *upd->pxlptr   & 0x02 ? (uint32_t) 1 : (uint32_t) 0;
}
private uint32_t
upd_pxlget1f8(upd_p upd)
{
upd->pxlget = upd_pxlget1f1;
return *upd->pxlptr++ & 0x01 ? (uint32_t) 1 : (uint32_t) 0;
}
private uint32_t
upd_pxlget2f1(upd_p upd)
{
upd->pxlget = upd_pxlget2f2;
return ((uint32_t) (*upd->pxlptr  ) & (uint32_t) 0xC0) >> 6;
}
private uint32_t
upd_pxlget2f2(upd_p upd)
{
upd->pxlget = upd_pxlget2f3;
return ((uint32_t) (*upd->pxlptr  ) & (uint32_t) 0x30) >> 4;
}
private uint32_t
upd_pxlget2f3(upd_p upd)
{
upd->pxlget = upd_pxlget2f4;
return ((uint32_t) (*upd->pxlptr  ) & (uint32_t) 0x0C) >> 2;
}
private uint32_t
upd_pxlget2f4(upd_p upd)
{
upd->pxlget = upd_pxlget2f1;
return  (uint32_t) (*upd->pxlptr++) & (uint32_t) 0x03;
}
private uint32_t
upd_pxlget4f1(upd_p upd)
{
upd->pxlget = upd_pxlget4f2;
return ((uint32_t) (*upd->pxlptr  ) & (uint32_t) 0xF0) >> 4;
}
private uint32_t
upd_pxlget4f2(upd_p upd)
{
upd->pxlget = upd_pxlget4f1;
return  (uint32_t) (*upd->pxlptr++) & (uint32_t) 0x0F;
}
private uint32_t
upd_pxlget8f(upd_p upd)
{
return (uint32_t) (*upd->pxlptr++);
}
private uint32_t
upd_pxlget16f(upd_p upd)
{
uint32_t ci  = (uint32_t) (*upd->pxlptr++) << 8;
ci |=                   *upd->pxlptr++;
return         ci;
}
private uint32_t
upd_pxlget24f(upd_p upd)
{
uint32_t ci  = (uint32_t) (*upd->pxlptr++) << 16;
ci |= (uint32_t) (*upd->pxlptr++) <<  8;
ci |=           *upd->pxlptr++;
return ci;
}
private uint32_t
upd_pxlget32f(upd_p upd)
{
uint32_t ci  = (uint32_t) (*upd->pxlptr++) << 24;
ci |= (uint32_t) (*upd->pxlptr++) << 16;
ci |= (uint32_t) (*upd->pxlptr++) <<  8;
ci |=                   *upd->pxlptr++;
return         ci;
}
private uint32_t
upd_pxlgetnix(upd_p upd)
{
return (uint32_t) 0;
}
private uint32_t
upd_pxlrev(upd_p upd)
{
const uint width = upd->pwidth < upd->gswidth ? upd->pwidth : upd->gswidth;
if(!(upd->pxlptr = upd->gsscan)) {
upd->pxlget = upd_pxlgetnix;
} else {
uint32_t ofs = (uint32_t) upd->int_a[IA_COLOR_INFO].data[1] * (width-1);
upd->pxlptr += ofs>>3;
ofs &= 7;
switch(upd->int_a[IA_COLOR_INFO].data[1]) {
case  1: switch(ofs) {
case 0:  upd->pxlget = upd_pxlget1r1; break;
case 1:  upd->pxlget = upd_pxlget1r2; break;
case 2:  upd->pxlget = upd_pxlget1r3; break;
case 3:  upd->pxlget = upd_pxlget1r4; break;
case 4:  upd->pxlget = upd_pxlget1r5; break;
case 5:  upd->pxlget = upd_pxlget1r6; break;
case 6:  upd->pxlget = upd_pxlget1r7; break;
case 7:  upd->pxlget = upd_pxlget1r8; break;
} break;
case  2: switch(ofs) {
case 0:  upd->pxlget = upd_pxlget2r1; break;
case 2:  upd->pxlget = upd_pxlget2r2; break;
case 4:  upd->pxlget = upd_pxlget2r3; break;
case 6:  upd->pxlget = upd_pxlget2r4; break;
} break;
case  4: switch(ofs) {
case 0:  upd->pxlget = upd_pxlget4r1; break;
case 4:  upd->pxlget = upd_pxlget4r2; break;
} break;
case  8: upd->pxlget = upd_pxlget8r;  break;
case 16:
upd->pxlget  = upd_pxlget16r;
upd->pxlptr += 1;
break;
case 24:
upd->pxlget = upd_pxlget24r;
upd->pxlptr += 2;
break;
case 32:
upd->pxlget = upd_pxlget32r;
upd->pxlptr += 3;
break;
default:
#if UPD_MESSAGES & UPD_M_ERROR
errprintf("upd_pxlrev: unsupported depth (%d)\n",
upd->int_a[IA_COLOR_INFO].data[1]);
#endif
upd->pxlget = upd_pxlgetnix;
break;
}
}
return (uint32_t) 0;
}
private uint32_t
upd_pxlget1r1(upd_p upd)
{
upd->pxlget = upd_pxlget1r8;
return *upd->pxlptr-- & 0x80 ? (uint32_t) 1 : (uint32_t) 0;
}
private uint32_t
upd_pxlget1r2(upd_p upd)
{
upd->pxlget = upd_pxlget1r1;
return *upd->pxlptr   & 0x40 ? (uint32_t) 1 : (uint32_t) 0;
}
private uint32_t
upd_pxlget1r3(upd_p upd)
{
upd->pxlget = upd_pxlget1r2;
return *upd->pxlptr   & 0x20 ? (uint32_t) 1 : (uint32_t) 0;
}
private uint32_t
upd_pxlget1r4(upd_p upd)
{
upd->pxlget = upd_pxlget1r3;
return *upd->pxlptr   & 0x10 ? (uint32_t) 1 : (uint32_t) 0;
}
private uint32_t
upd_pxlget1r5(upd_p upd)
{
upd->pxlget = upd_pxlget1r4;
return *upd->pxlptr   & 0x08 ? (uint32_t) 1 : (uint32_t) 0;
}
private uint32_t
upd_pxlget1r6(upd_p upd)
{
upd->pxlget = upd_pxlget1r5;
return *upd->pxlptr   & 0x04 ? (uint32_t) 1 : (uint32_t) 0;
}
private uint32_t
upd_pxlget1r7(upd_p upd)
{
upd->pxlget = upd_pxlget1r6;
return *upd->pxlptr   & 0x02 ? (uint32_t) 1 : (uint32_t) 0;
}
private uint32_t
upd_pxlget1r8(upd_p upd)
{
upd->pxlget = upd_pxlget1r7;
return *upd->pxlptr   & 0x01 ? (uint32_t) 1 : (uint32_t) 0;
}
private uint32_t
upd_pxlget2r1(upd_p upd)
{
upd->pxlget = upd_pxlget2r4;
return ((uint32_t) (*upd->pxlptr--) & (uint32_t) 0xC0) >> 6;
}
private uint32_t
upd_pxlget2r2(upd_p upd)
{
upd->pxlget = upd_pxlget2r1;
return ((uint32_t) (*upd->pxlptr  ) & (uint32_t) 0x30) >> 4;
}
private uint32_t
upd_pxlget2r3(upd_p upd)
{
upd->pxlget = upd_pxlget2r2;
return ((uint32_t) (*upd->pxlptr  ) & (uint32_t) 0x0C) >> 2;
}
private uint32_t
upd_pxlget2r4(upd_p upd)
{
upd->pxlget = upd_pxlget2r3;
return  (uint32_t) (*upd->pxlptr  ) & (uint32_t) 0x03;
}
private uint32_t
upd_pxlget4r1(upd_p upd)
{
upd->pxlget = upd_pxlget4r2;
return ((uint32_t) (*upd->pxlptr--) & (uint32_t) 0xF0) >> 4;
}
private uint32_t
upd_pxlget4r2(upd_p upd)
{
upd->pxlget = upd_pxlget4r1;
return  (uint32_t) (*upd->pxlptr  ) & (uint32_t) 0x0F;
}
private uint32_t
upd_pxlget8r(upd_p upd)
{
return (uint32_t) (*upd->pxlptr--);
}
private uint32_t
upd_pxlget16r(upd_p upd)
{
uint32_t ci  =                   *upd->pxlptr--;
ci |= (uint32_t) (*upd->pxlptr--) << 8;
return         ci;
}
private uint32_t
upd_pxlget24r(upd_p upd)
{
uint32_t ci  =           *upd->pxlptr--;
ci |= (uint32_t) (*upd->pxlptr--) <<  8;
ci |= (uint32_t) (*upd->pxlptr--) << 16;
return ci;
}
private uint32_t
upd_pxlget32r(upd_p upd)
{
uint32_t ci  =                   *upd->pxlptr--;
ci |= (uint32_t) (*upd->pxlptr--) <<  8;
ci |= (uint32_t) (*upd->pxlptr--) << 16;
ci |= (uint32_t) (*upd->pxlptr--) << 24;
return         ci;
}