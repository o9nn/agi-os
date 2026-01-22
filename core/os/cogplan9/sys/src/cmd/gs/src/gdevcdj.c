#include "std.h"
#include <stdlib.h>
#include "gdevprn.h"
#include "gdevpcl.h"
#include "gsparam.h"
#include "gsstate.h"
#include "gxlum.h"
#include "gdevbjc.h"
#define BEGIN_ARRAY_PARAM(pread, pname, pa, psize, e)\
switch ( ncode = pread(plist, (oname = pname), &pa) )\
{\
case 0:\
if ( pa.size != psize )\
code = gs_error_rangecheck;\
else {
#define END_PARAM(pa, e)\
}\
goto e;\
default:\
code = ncode;\
e: param_signal_error(plist, oname, code);\
case 1:\
pa.data = 0; \
}
private int cdj_param_check_bytes(gs_param_list *, gs_param_name, const byte *, uint, bool);
private int cdj_param_check_float(gs_param_list *, gs_param_name, floatp, bool);
#define cdj_param_check_string(plist, pname, str, is_defined)\
cdj_param_check_bytes(plist, pname, (const byte *)(str), strlen(str),\
is_defined)
#define DESKJET_PRINT_LIMIT 0.04
#define PAINTJET_PRINT_LIMIT 0.0
#define ESC_P_PRINT_LIMIT 0.335
#define DESKJET_MARGINS_LETTER (float)0.25, (float)0.50, (float)0.25, (float)0.167
#define DESKJET_MARGINS_A4 (float)0.125, (float)0.50, (float)0.143, (float)0.167
#define LJET4_MARGINS (float)0.26, (float)0.0, (float)0.0, (float)0.0
#define PAINTJET_MARGINS (float)0.167, (float)0.167, (float)0.167, (float)0.167
#define DESIGNJET_MARGINS (float)0.167, (float)0.167, (float)0.167, (float)0.167
#define ESC_P_MARGINS_LETTER (float)0.134, (float)(0.276+0.2), \
(float)(0.366+0.01), (float)0.335
#define ESC_P_MARGINS_A4 (float)0.134, (float)(0.276+0.2), \
(float)(0.166+0.01), (float)0.335
#ifndef BITSPERPIXEL
# define BITSPERPIXEL 24
#endif
#define W size_of(word)
#define I size_of(int)
#define invert_word(v)\
((v) >> 24) + (((v) >> 8) & 0xff00L) +\
(((word)(v) << 8) & 0xff0000L) + ((word)(v) << 24)
#define DJ500C 0
#define DJ550C 1
#define PJXL300 2
#define PJ180 3
#define PJXL180 4
#define DECLJ250 5
#define DNJ650C 6
#define LJ4DITH 7
#define ESC_P 8
#define BJC600 9
#define BJC800 10
#define HEAD_ROWS_MONO 50
#define HEAD_ROWS_COLOUR 16
private dev_proc_map_cmyk_color (gdev_cmyk_map_cmyk_color);
private dev_proc_map_rgb_color (gdev_cmyk_map_rgb_color);
private dev_proc_map_rgb_color (gdev_pcl_map_rgb_color);
private dev_proc_map_color_rgb (gdev_pcl_map_color_rgb);
private dev_proc_decode_color (gdev_cmyk_map_color_cmyk);
private dev_proc_open_device(dj500c_open);
private dev_proc_open_device(dj550c_open);
private dev_proc_open_device(dnj650c_open);
private dev_proc_open_device(lj4dith_open);
private dev_proc_open_device(pj_open);
private dev_proc_open_device(pjxl_open);
private dev_proc_open_device(pjxl300_open);
private dev_proc_open_device(escp_open);
private dev_proc_open_device(bjc_open);
private dev_proc_print_page(declj250_print_page);
private dev_proc_print_page(dj500c_print_page);
private dev_proc_print_page(dj550c_print_page);
private dev_proc_print_page(dnj650c_print_page);
private dev_proc_print_page(lj4dith_print_page);
private dev_proc_print_page(pj_print_page);
private dev_proc_print_page(pjxl_print_page);
private dev_proc_print_page(pjxl300_print_page);
private dev_proc_print_page(escp_print_page);
private dev_proc_print_page(bjc_print_page);
private dev_proc_get_params(cdj_get_params);
private dev_proc_get_params(pjxl_get_params);
private dev_proc_get_params(bjc_get_params);
#define ep_get_params cdj_get_params
private dev_proc_put_params(cdj_put_params);
private dev_proc_put_params(pj_put_params);
private dev_proc_put_params(pjxl_put_params);
private dev_proc_put_params(bjc_put_params);
#define ep_put_params cdj_put_params
#define gx_prn_colour_device_common \
gx_prn_device_common; \
short cmyk; \
\
uint default_depth; \
uint correction
typedef struct gx_device_cdj_s gx_device_cdj;
struct gx_device_cdj_s {
gx_device_common;
gx_prn_colour_device_common;
int shingling;
int depletion;
};
typedef struct gx_device_pjxl_s gx_device_pjxl;
struct gx_device_pjxl_s {
gx_device_common;
gx_prn_colour_device_common;
int printqual;
int rendertype;
};
typedef struct gx_device_hp_s gx_device_hp;
struct gx_device_hp_s {
gx_device_common;
gx_prn_colour_device_common;
};
typedef struct gx_device_hp_s gx_device_pj;
typedef struct gx_device_bjc600_s gx_device_bjc600;
typedef struct gx_device_bjc800_s gx_device_bjc800;
typedef struct gx_device_bjc800_s gx_device_bjc;
#define bjc_params_common \
bool manualFeed; \
int mediaType; \
bool mediaWeight_isSet; \
int mediaWeight; \
int printQuality; \
bool ditheringType; \
int colorComponents; \
int printColors
typedef struct {
bjc_params_common;
bool monochromePrint;
} bjc600_params;
typedef struct {
bjc_params_common;
} bjc_params;
typedef bjc_params bjc800_params;
#define gx_bjc_device_common \
gx_device_common; \
gx_prn_colour_device_common; \
int ptype; \
float printLimit
struct gx_device_bjc600_s {
gx_bjc_device_common;
bjc600_params bjc_p;
};
struct gx_device_bjc800_s {
gx_bjc_device_common;
bjc800_params bjc_p;
};
typedef struct {
gx_device_common;
gx_prn_colour_device_common;
} gx_device_colour_prn;
#define cprn_device ((gx_device_colour_prn*) pdev)
#define cdj ((gx_device_cdj *)pdev)
#define pjxl ((gx_device_pjxl *)pdev)
#define pj ((gx_device_pj *)pdev)
#define bjc ((gx_device_bjc*) pdev)
#define bjc600 ((gx_device_bjc600*) pdev)
#define bjc800 ((gx_device_bjc800*) pdev)
#define bjcparams (bjc->bjc_p)
#define bjc600params (bjc600->bjc_p)
#define bjc800params (bjc800->bjc_p)
#define bjcversion(p) (((gx_device_bjc*) pdev)->ptype == BJC800 ? \
BJC_BJC800_VERSION : BJC_BJC600_VERSION)
#define bjcversionstring(p) (((gx_device_bjc*) pdev)->ptype == BJC800 ? \
BJC_BJC800_VERSIONSTR : BJC_BJC600_VERSIONSTR)
#define bjcthickpaper(l) \
(bjcparams.mediaWeight_isSet && bjcparams.mediaWeight > l)
#define bjc600thickpaper() bjcthickpaper(BJC600_MEDIAWEIGHT_THICKLIMIT)
#define bjc800thickpaper() bjcthickpaper(BJC800_MEDIAWEIGHT_THICKLIMIT)
#define prn_colour_device_body(dtype, procs, dname, w10, h10, xdpi, ydpi, lm, bm, rm, tm, ncomp, depth, mg, mc, dg, dc, print_page, cmyk, correct)\
prn_device_body(dtype, procs, dname, w10, h10, xdpi, ydpi, lm, bm, rm, tm, ncomp, depth, mg, mc, dg, dc, print_page), cmyk, depth , correct
#define prn_hp_colour_device(dtype, procs, dev_name, x_dpi, y_dpi, bpp, print_page, correct)\
prn_colour_device_body(dtype, procs, dev_name,\
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS, x_dpi, y_dpi, 0, 0, 0, 0,\
(bpp == 32 ? 4 : (bpp == 1 || bpp == 8) ? 1 : 3), bpp,\
(bpp >= 8 ? 255 : 1), (bpp >= 8 ? 255 : bpp > 1 ? 1 : 0),\
(bpp >= 8 ? 256 : 2), (bpp >= 8 ? 256 : bpp > 1 ? 2 : 0),\
print_page, 0 , correct)
#define prn_cmyk_colour_device(dtype, procs, dev_name, x_dpi, y_dpi, bpp, print_page, correct)\
prn_colour_device_body(dtype, procs, dev_name,\
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS, x_dpi, y_dpi, 0, 0, 0, 0,\
((bpp == 1 || bpp == 4) ? 1 : 4), bpp,\
(bpp > 8 ? 255 : 1), (1 << (bpp >> 2)) - 1, \
(bpp > 8 ? 256 : 2), (bpp > 8 ? 256 : bpp > 1 ? 2 : 0),\
print_page, 1 , correct)
#define bjc_device(dtype, p, d, x, y, b, pp, c) \
prn_cmyk_colour_device(dtype, p, d, x, y, b, pp, c)
#define cdj_device(procs, dev_name, x_dpi, y_dpi, bpp, print_page, correction, shingling, depletion)\
{ prn_hp_colour_device(gx_device_cdj, procs, dev_name, x_dpi, y_dpi, bpp, print_page, correction),\
shingling,\
depletion\
}
#define pjxl_device(procs, dev_name, x_dpi, y_dpi, bpp, print_page, printqual, rendertype)\
{ prn_hp_colour_device(gx_device_pjxl, procs, dev_name, x_dpi, y_dpi, bpp, print_page, 0), \
printqual,\
rendertype\
}
#define pj_device(procs, dev_name, x_dpi, y_dpi, bpp, print_page)\
{ prn_hp_colour_device(gx_device_pj, procs, dev_name, x_dpi, y_dpi, bpp, print_page, 0) }
#define bjc600_device(procs, dev_name, x_dpi, y_dpi, bpp, print_page, t, mf, mt, mws, mw, pq, dt, cc, pc, mp) \
{ bjc_device(gx_device_bjc600, procs, dev_name, x_dpi, y_dpi, bpp, print_page, 0),\
t, 0., { mf, mt, mws, mw, pq, dt, cc, pc, mp }\
}
#define bjc800_device(procs, dev_name, x_dpi, y_dpi, bpp, print_page, t, mf, mt, mws, mw, pq, dt, cc, pc) \
{ bjc_device(gx_device_bjc800, procs, dev_name, x_dpi, y_dpi, bpp, print_page, 0),\
t, 0., { mf, mt, mws, mw, pq, dt, cc, pc }\
}
#define hp_colour_procs(proc_colour_open, proc_get_params, proc_put_params) {\
proc_colour_open,\
gx_default_get_initial_matrix,\
gx_default_sync_output,\
gdev_prn_output_page,\
gdev_prn_close,\
gdev_pcl_map_rgb_color,\
gdev_pcl_map_color_rgb,\
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
gx_default_get_bits,\
proc_get_params,\
proc_put_params\
}
#define cmyk_colour_procs(proc_colour_open, proc_get_params, proc_put_params) {\
proc_colour_open,\
gx_default_get_initial_matrix,\
gx_default_sync_output,\
gdev_prn_output_page,\
gdev_prn_close,\
NULL ,\
NULL ,\
NULL ,\
NULL ,\
NULL ,\
NULL ,\
NULL ,\
gx_default_get_bits,\
proc_get_params,\
proc_put_params,\
gdev_cmyk_map_cmyk_color,\
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
gdev_cmyk_map_cmyk_color, \
gdev_cmyk_map_color_cmyk \
}
private gx_device_procs cdj500_procs =
hp_colour_procs(dj500c_open, cdj_get_params, cdj_put_params);
private gx_device_procs cdj550_procs =
hp_colour_procs(dj550c_open, cdj_get_params, cdj_put_params);
#ifdef USE_CDJ550_CMYK
private gx_device_procs cdj550cmyk_procs =
cmyk_colour_procs(dj550c_open, cdj_get_params, cdj_put_params);
#endif
private gx_device_procs dnj650c_procs =
hp_colour_procs(dnj650c_open, cdj_get_params, cdj_put_params);
private gx_device_procs lj4dith_procs =
hp_colour_procs(lj4dith_open, cdj_get_params, cdj_put_params);
private gx_device_procs pj_procs =
hp_colour_procs(pj_open, gdev_prn_get_params, pj_put_params);
private gx_device_procs pjxl_procs =
hp_colour_procs(pjxl_open, pjxl_get_params, pjxl_put_params);
private gx_device_procs pjxl300_procs =
hp_colour_procs(pjxl300_open, pjxl_get_params, pjxl_put_params);
private gx_device_procs bjc_procs =
cmyk_colour_procs(bjc_open, bjc_get_params, bjc_put_params);
private gx_device_procs escp_procs =
hp_colour_procs(escp_open, ep_get_params, ep_put_params);
gx_device_cdj far_data gs_cdjmono_device =
cdj_device(cdj500_procs, "cdjmono", 300, 300, 1,
dj500c_print_page, 4, 0, 1);
gx_device_cdj far_data gs_cdeskjet_device =
cdj_device(cdj500_procs, "cdeskjet", 300, 300, 3,
dj500c_print_page, 4, 2, 1);
gx_device_cdj far_data gs_cdjcolor_device =
cdj_device(cdj500_procs, "cdjcolor", 300, 300, 24,
dj500c_print_page, 4, 2, 1);
gx_device_cdj far_data gs_cdj500_device =
cdj_device(cdj500_procs, "cdj500", 300, 300, BITSPERPIXEL,
dj500c_print_page, 4, 2, 1);
gx_device_cdj far_data gs_cdj550_device =
cdj_device(cdj550_procs, "cdj550", 300, 300, BITSPERPIXEL,
dj550c_print_page, 0, 2, 1);
#ifdef USE_CDJ550_CMYK
gx_device_cdj far_data gs_cdj550cmyk_device = {
prn_cmyk_colour_device(cdj550cmyk_procs, "cdj550cmyk", 300, 300,
BITSPERPIXEL, dj550c_print_page, 0), 2, 1
};
#endif
gx_device_pj far_data gs_declj250_device =
pj_device(pj_procs, "declj250", 180, 180, BITSPERPIXEL,
declj250_print_page);
gx_device_cdj far_data gs_dnj650c_device =
cdj_device(dnj650c_procs, "dnj650c", 300, 300, BITSPERPIXEL,
dnj650c_print_page, 0, 2, 1);
gx_device_cdj far_data gs_lj4dith_device =
cdj_device(lj4dith_procs, "lj4dith", 600, 600, 8,
lj4dith_print_page, 4, 0, 1);
gx_device_pj far_data gs_pj_device =
pj_device(pj_procs, "pj", 180, 180, BITSPERPIXEL,
pj_print_page);
gx_device_pjxl far_data gs_pjxl_device =
pjxl_device(pjxl_procs, "pjxl", 180, 180, BITSPERPIXEL,
pjxl_print_page, 0, 0);
gx_device_pjxl far_data gs_pjxl300_device =
pjxl_device(pjxl300_procs, "pjxl300", 300, 300, BITSPERPIXEL,
pjxl300_print_page, 0, 0);
gx_device_cdj far_data gs_escp_device =
cdj_device(escp_procs, "escp", 360, 360, 8,
escp_print_page, 0, 0, 1);
gx_device_cdj far_data gs_escpc_device =
cdj_device(escp_procs, "escpc", 360, 360, 24,
escp_print_page, 0, 0, 1);
gx_device_bjc600 far_data gs_bjc600_device =
bjc600_device(
bjc_procs,
BJC_BJC600,
BJC600_DEFAULT_RESOLUTION,
BJC600_DEFAULT_RESOLUTION,
BJC600_DEFAULT_BITSPERPIXEL,
bjc_print_page,
BJC600,
BJC600_DEFAULT_MANUALFEED,
BJC600_DEFAULT_MEDIATYPE,
BJC600_DEFAULT_SETMEDIAWEIGHT,
BJC600_DEFAULT_MEDIAWEIGHT,
BJC600_DEFAULT_PRINTQUALITY,
BJC600_DEFAULT_DITHERINGTYPE,
BJC600_DEFAULT_COLORCOMPONENTS,
BJC600_DEFAULT_PRINTCOLORS,
BJC600_DEFAULT_MONOCHROMEPRINT);
gx_device_bjc800 far_data gs_bjc800_device =
bjc800_device(
bjc_procs,
BJC_BJC800,
BJC800_DEFAULT_RESOLUTION,
BJC800_DEFAULT_RESOLUTION,
BJC800_DEFAULT_BITSPERPIXEL,
bjc_print_page,
BJC800,
BJC800_DEFAULT_MANUALFEED,
BJC800_DEFAULT_MEDIATYPE,
BJC800_DEFAULT_SETMEDIAWEIGHT,
BJC800_DEFAULT_MEDIAWEIGHT,
BJC800_DEFAULT_PRINTQUALITY,
BJC800_DEFAULT_DITHERINGTYPE,
BJC600_DEFAULT_COLORCOMPONENTS,
BJC800_DEFAULT_PRINTCOLORS);
private int gdev_pcl_mode1compress(const byte *, const byte *, byte *);
private int hp_colour_open(gx_device *, int);
private int hp_colour_print_page(gx_device_printer *, FILE *, int);
private int cdj_put_param_int(gs_param_list *, gs_param_name, int *, int, int, int);
private uint gdev_prn_rasterwidth(const gx_device_printer *, int);
private int cdj_put_param_bpp(gx_device *, gs_param_list *, int, int, int);
private int cdj_set_bpp(gx_device *, int, int);
private void cdj_expand_line(word *, int, short, int, int);
private int bjc_fscmyk(byte**, byte*[4][4], int**, int, int);
typedef struct {
const char* p_name;
int p_value;
} stringParamDescription;
private const byte* paramValueToString(const stringParamDescription*, int);
private int paramStringValue(const stringParamDescription*,
const byte*, int, int*);
private int put_param_string(gs_param_list*, const byte*,
gs_param_string*, const stringParamDescription*, int *, int);
private int get_param_string(gs_param_list*, const byte*,
gs_param_string*, const stringParamDescription*, int, bool, int);
private int
dj500c_open(gx_device *pdev)
{ return hp_colour_open(pdev, DJ500C);
}
private int
dj550c_open(gx_device *pdev)
{ return hp_colour_open(pdev, DJ550C);
}
private int
dnj650c_open(gx_device *pdev)
{ return hp_colour_open(pdev, DNJ650C);
}
private int
lj4dith_open(gx_device *pdev)
{ return hp_colour_open(pdev, LJ4DITH);
}
private int
pjxl300_open(gx_device *pdev)
{ return hp_colour_open(pdev, PJXL300);
}
private int
pj_open(gx_device *pdev)
{ return hp_colour_open(pdev, PJ180);
}
private int
pjxl_open(gx_device *pdev)
{ return hp_colour_open(pdev, PJXL180);
}
private int
escp_open(gx_device *pdev)
{ return hp_colour_open(pdev, ESC_P);
}
private int
bjc_open(gx_device *pdev)
{ return hp_colour_open(pdev, bjc->ptype);
}
private int
hp_colour_open(gx_device *pdev, int ptype)
{
static const float dj_a4[4] = { DESKJET_MARGINS_A4 };
static const float dj_letter[4] = { DESKJET_MARGINS_LETTER };
static const float lj4_all[4] = { LJET4_MARGINS };
static const float pj_all[4] = { PAINTJET_MARGINS };
static const float dnj_all[4] = { DESIGNJET_MARGINS };
static const float ep_a4[4] = { ESC_P_MARGINS_A4 };
static const float ep_letter[4] = { ESC_P_MARGINS_LETTER };
static float bjc_a3[4] = { BJC_MARGINS_A3 };
static float bjc_letter[4] = { BJC_MARGINS_LETTER };
static float bjc_a4[4] = { BJC_MARGINS_A4 };
const float *m = (float *) 0;
if (pdev->color_info.num_components == 0)
{ int code = cdj_set_bpp(pdev, pdev->color_info.depth,
pdev->color_info.num_components);
if ( code < 0 )
return code;
}
switch (ptype) {
case DJ500C:
case DJ550C:
m = (gdev_pcl_paper_size(pdev) == PAPER_SIZE_A4 ? dj_a4 :
dj_letter);
break;
case DNJ650C:
m = dnj_all;
break;
case LJ4DITH:
m = lj4_all;
break;
case PJ180:
case PJXL300:
case PJXL180:
m = pj_all;
break;
case ESC_P:
m = (gdev_pcl_paper_size(pdev) == PAPER_SIZE_A4 ? ep_a4 :
ep_letter);
break;
case BJC600:
case BJC800:
switch (gdev_pcl_paper_size(pdev)) {
case PAPER_SIZE_LEGAL:
case PAPER_SIZE_LETTER:
m = bjc_letter;
break;
case PAPER_SIZE_A0:
case PAPER_SIZE_A1:
case PAPER_SIZE_A3:
m = bjc_a3;
break;
default:
m = bjc_a4;
}
#ifndef USE_FIXED_MARGINS
if (ptype == BJC800) {
((float *) m)[1] = (float)BJC_HARD_LOWER_LIMIT;
}
#endif
bjc->printLimit = m[3];
#ifdef BJC_DEFAULT_CENTEREDAREA
if (m[3] < m[1]) {
((float *) m)[3] = m[1];
} else {
((float *) m)[1] = m[3];
}
#endif
break;
{
float *bjcm = (float *) m;
byte pdimen = (byte)
(pdev->height / pdev->y_pixels_per_inch * 10.
- bjcm[3] * 10. - bjcm[1] * 10. + .5) + 1;
do {
--pdimen;
bjcm[1] = pdev->height / pdev->y_pixels_per_inch
- bjcm[3] - (float) pdimen / 10.;
} while (bjcm[1] < BJC_LOWER_LIMIT);
}
break;
}
gx_device_set_margins(pdev, m, true);
return gdev_prn_open(pdev);
}
private int
cdj_get_params(gx_device *pdev, gs_param_list *plist)
{ int code = gdev_prn_get_params(pdev, plist);
if ( code < 0 ||
(code = param_write_int(plist, "BlackCorrect", (int *)&cdj->correction)) < 0 ||
(code = param_write_int(plist, "Shingling", &cdj->shingling)) < 0 ||
(code = param_write_int(plist, "Depletion", &cdj->depletion)) < 0
)
return code;
return code;
}
private int
cdj_put_params(gx_device *pdev, gs_param_list *plist)
{ int correction = cdj->correction;
int shingling = cdj->shingling;
int depletion = cdj->depletion;
int bpp = 0;
int code = 0;
code = cdj_put_param_int(plist, "BlackCorrect", &correction, 0, 9, code);
code = cdj_put_param_int(plist, "Shingling", &shingling, 0, 2, code);
code = cdj_put_param_int(plist, "Depletion", &depletion, 1, 3, code);
code = cdj_put_param_int(plist, "BitsPerPixel", &bpp, 1, 32, code);
if ( code < 0 )
return code;
code = cdj_put_param_bpp(pdev, plist, bpp, bpp, 0);
if ( code < 0 )
return code;
cdj->correction = correction;
cdj->shingling = shingling;
cdj->depletion = depletion;
return 0;
}
private int
pjxl_get_params(gx_device *pdev, gs_param_list *plist)
{ int code = gdev_prn_get_params(pdev, plist);
if ( code < 0 ||
(code = param_write_int(plist, "PrintQuality", &pjxl->printqual)) < 0 ||
(code = param_write_int(plist, "RenderType", &pjxl->rendertype)) < 0
)
return code;
return code;
}
private int
pjxl_put_params(gx_device *pdev, gs_param_list *plist)
{ int printqual = pjxl->printqual;
int rendertype = pjxl->rendertype;
int bpp = 0, real_bpp = 0;
int code = 0;
code = cdj_put_param_int(plist, "PrintQuality", &printqual, -1, 1, code);
code = cdj_put_param_int(plist, "RenderType", &rendertype, 0, 10, code);
code = cdj_put_param_int(plist, "BitsPerPixel", &bpp, 1, 32, code);
if ( code < 0 )
return code;
real_bpp = bpp;
if ( rendertype > 0 )
{
if ( bpp > 0 && bpp < 16 )
real_bpp = 24;
}
code = cdj_put_param_bpp(pdev, plist, bpp, real_bpp, 0);
if ( code < 0 )
return code;
pjxl->printqual = printqual;
pjxl->rendertype = rendertype;
return 0;
}
private int
pj_put_params(gx_device *pdev, gs_param_list *plist)
{ int bpp = 0;
int code = cdj_put_param_int(plist, "BitsPerPixel", &bpp, 1, 32, 0);
if ( code < 0 )
return code;
return cdj_put_param_bpp(pdev, plist, bpp, bpp, 0);
}
private stringParamDescription bjc_processColorsStrings[] = {
{ "DeviceGray", 1 },
{ "DeviceRGB", 3 },
{ "DeviceCMYK", 4 },
{ 0 }
};
private stringParamDescription bjc_mediaTypeStrings[] = {
{ "PlainPaper", BJC_MEDIA_PLAINPAPER },
{ "CoatedPaper", BJC_MEDIA_COATEDPAPER },
{ "TransparencyFilm", BJC_MEDIA_TRANSPARENCYFILM },
{ "Envelope", BJC_MEDIA_ENVELOPE },
{ "Card", BJC_MEDIA_CARD},
{ "Other", BJC_MEDIA_OTHER },
{ 0 }
};
private stringParamDescription bjc600_printQualityStrings[] = {
{ "Normal", 0 },
{ "High", 1 },
{ "Draft", 2 },
{ 0 }
};
private stringParamDescription bjc800_printQualityStrings[] = {
{ "Normal", 0 },
{ "High", 1 },
{ "Low", 3 },
{ "Draft", 4 },
{ 0 },
};
private stringParamDescription bjc_ditheringTypeStrings[] = {
{ "None", BJC_DITHER_NONE },
{ "Floyd-Steinberg", BJC_DITHER_FS },
{ 0 }
};
private int
bjc_get_params(gx_device *pdev, gs_param_list *plist)
{
int code = gdev_prn_get_params(pdev, plist);
int ncode;
gs_param_string pmedia;
gs_param_string pquality;
gs_param_string dithering;
if (code < 0) return_error(code);
if ((ncode = param_write_bool(plist, BJC_OPTION_MANUALFEED,
&bjcparams.manualFeed)) < 0) {
code = ncode;
}
code = get_param_string(plist, (unsigned char *)BJC_OPTION_MEDIATYPE, &pmedia,
bjc_mediaTypeStrings, bjcparams.mediaType, true, code);
code = get_param_string(plist, (unsigned char *)BJC_OPTION_PRINTQUALITY, &pquality,
(bjc->ptype == BJC800 ? bjc800_printQualityStrings :
bjc600_printQualityStrings), bjcparams.printQuality,
true, code);
code = get_param_string(plist, (unsigned char *)BJC_OPTION_DITHERINGTYPE, &dithering,
bjc_ditheringTypeStrings, bjcparams.ditheringType, true, code);
if ((ncode = param_write_int(plist, BJC_OPTION_PRINTCOLORS,
&bjcparams.printColors)) < 0) {
code = ncode;
}
if ((ncode = (bjcparams.mediaWeight_isSet ?
param_write_int(plist, BJC_OPTION_MEDIAWEIGHT,
&bjcparams.mediaWeight) :
param_write_null(plist, BJC_OPTION_MEDIAWEIGHT))) < 0) {
code = ncode;
}
if (bjc->ptype != BJC800) {
if ((ncode = param_write_bool(plist, BJC_OPTION_MONOCHROMEPRINT,
&bjc600params.monochromePrint)) < 0) {
code = ncode;
}
}
{
float version;
gs_param_string versionString;
bool bTrue = true;
version = bjcversion(pdev);
versionString.data = (byte *)bjcversionstring(pdev);
versionString.size = strlen((char *)versionString.data);
versionString.persistent = true;
if ((ncode = param_write_float(plist, BJC_DEVINFO_VERSION,
&version)) < 0) {
code = ncode;
}
if ((ncode = param_write_string(plist, BJC_DEVINFO_VERSIONSTRING,
&versionString)) < 0) {
code = ncode;
}
if ((ncode = param_write_bool(plist, BJC_DEVINFO_OUTPUTFACEUP,
&bTrue)) < 0) {
code = ncode;
}
}
return code;
}
private int
bjc_put_params(gx_device *pdev, gs_param_list *plist)
{
int bpp = 0, ccomps = 0;
int code = 0;
int ncode;
bool aBool = true;
const char* oname = (const char*) 0;
bjc600_params new600Params;
bjc800_params new800Params;
bjc_params* params;
gs_param_string pprocesscolors;
gs_param_string pmedia;
gs_param_string pquality;
gs_param_float_array hwra;
if (bjc->ptype != BJC800) {
new600Params = bjc600params;
params = (bjc_params*) &new600Params;
} else {
new800Params = bjc800params;
params = (bjc_params*) &new800Params;
}
if ((code = cdj_put_param_int(plist, "BitsPerPixel",
&bpp, 1, 32, code)) != 1) {
bpp = pdev->color_info.depth;
}
if ((code = put_param_string(plist, (unsigned char *)"ProcessColorModel",
&pprocesscolors, bjc_processColorsStrings, &ccomps, code)) != 1) {
ccomps = pdev->color_info.num_components;
}
if ((ncode = param_read_bool(plist, oname = BJC_OPTION_MANUALFEED,
&params->manualFeed)) < 0) {
param_signal_error(plist, oname, code = ncode);
}
code = put_param_string(plist, (unsigned char *)BJC_OPTION_MEDIATYPE, &pmedia,
bjc_mediaTypeStrings, &params->mediaType, code);
code = cdj_put_param_int(plist, BJC_OPTION_PRINTCOLORS,
&params->printColors, 0, 15, code);
code = put_param_string(plist, (unsigned char *)BJC_OPTION_PRINTQUALITY, &pquality,
(bjc->ptype == BJC800 ? bjc800_printQualityStrings :
bjc600_printQualityStrings), &params->printQuality, code);
switch (ncode = param_read_int(plist,
oname = BJC_OPTION_MEDIAWEIGHT, &params->mediaWeight)) {
case 0:
if (params->mediaWeight <= 0) {
ncode = gs_error_rangecheck;
} else {
params->mediaWeight_isSet = 1;
break;
}
goto mwe;
default:
if ((ncode = param_read_null(plist, oname)) == 0) {
params->mediaWeight_isSet = 0;
break;
}
mwe: param_signal_error(plist, oname, code = ncode);
case 1:
break;
}
if (bjc->ptype != BJC800) {
bjc600_params* params600 = (bjc600_params*) params;
if ((ncode = param_read_bool(plist,
oname = BJC_OPTION_MONOCHROMEPRINT,
&params600->monochromePrint)) < 0) {
param_signal_error(plist, oname, code = ncode);
}
}
if ((ncode = cdj_param_check_float(plist, BJC_DEVINFO_VERSION,
bjcversion(pdev), true)) < 0) {
code = ncode;
}
if ((ncode = cdj_param_check_string(plist, BJC_DEVINFO_VERSIONSTRING,
bjcversionstring(pdev), true)) < 0) {
code = ncode;
}
if ((ncode = param_read_bool(plist, oname = BJC_DEVINFO_OUTPUTFACEUP,
&aBool)) < 0) {
param_signal_error(plist, oname, code = ncode);
} else if (aBool != true) {
param_signal_error(plist, oname, code = ncode = gs_error_rangecheck);
}
BEGIN_ARRAY_PARAM(param_read_float_array, "HWResolution", hwra, 2, hwre)
if ( hwra.data[0] <= 0 || hwra.data[1] <= 0 ||
hwra.data[0] != hwra.data[1] )
ncode = gs_error_rangecheck;
else {
#ifdef BJC_STRICT
if (hwra.data[0] != BJC_RESOLUTION_LOW &&
hwra.data[0] != BJC_RESOLUTION_NORMAL &&
hwra.data[0] != BJC_RESOLUTION_HIGH) {
ncode = gs_error_rangecheck;
}
#else
{
int n;
for (n = 0; n < 8 * sizeof(n) / BJC_RESOLUTION_BASE; ++n) {
float res = (float)(BJC_RESOLUTION_BASE * (1 << n));
if (res == hwra.data[0]) break;
if (res > hwra.data[0]) {
ncode = gs_error_rangecheck;
}
}
if (n == 8 * sizeof(n)) {
ncode = gs_error_rangecheck;
}
}
#endif
if (ncode < 0) {
code = ncode;
} else {
break;
}
}
END_PARAM(hwra, hwre)
if ((ncode = cdj_put_param_bpp(pdev, plist, bpp, bpp, ccomps)) < 0) {
code = ncode;
}
if (code < 0)
return code;
if (bpp == 1) {
params->ditheringType = BJC_DITHER_NONE;
}
if (bjc->ptype != BJC800) {
bjc600params = new600Params;
} else {
bjc800params = new800Params;
}
return code;
}
private int
dj500c_print_page(gx_device_printer * pdev, FILE * prn_stream)
{
return hp_colour_print_page(pdev, prn_stream, DJ500C);
}
private int
dj550c_print_page(gx_device_printer * pdev, FILE * prn_stream)
{
return hp_colour_print_page(pdev, prn_stream, DJ550C);
}
private int
dnj650c_print_page(gx_device_printer * pdev, FILE * prn_stream)
{
return hp_colour_print_page(pdev, prn_stream, DNJ650C);
}
private int
lj4dith_print_page(gx_device_printer * pdev, FILE * prn_stream)
{
return hp_colour_print_page(pdev, prn_stream, LJ4DITH);
}
private int
pjxl300_print_page(gx_device_printer * pdev, FILE * prn_stream)
{ int ret_code;
fputs("\033%-12345X@PJL enter language = PCL\n", prn_stream);
ret_code = hp_colour_print_page(pdev, prn_stream, PJXL300);
fputs("\033%-12345X", prn_stream);
return ret_code;
}
private int
pjxl_print_page(gx_device_printer * pdev, FILE * prn_stream)
{
return hp_colour_print_page(pdev, prn_stream, PJXL180);
}
private int
pj_print_page(gx_device_printer * pdev, FILE * prn_stream)
{
return hp_colour_print_page(pdev, prn_stream, PJ180);
}
private int
declj250_print_page(gx_device_printer * pdev, FILE * prn_stream)
{ int ret_code;
fputs("\033%8", prn_stream);
ret_code = hp_colour_print_page(pdev, prn_stream, DECLJ250);
fputs("\033%@", prn_stream);
return ret_code;
}
private int
escp_print_page(gx_device_printer * pdev, FILE * prn_stream)
{
return hp_colour_print_page(pdev, prn_stream, ESC_P);
}
private int
bjc_print_page(gx_device_printer * pdev, FILE * prn_stream)
{
return hp_colour_print_page(pdev, prn_stream, bjc->ptype);
}
#define SHIFT ((I * 8) - 13)
#define RSHIFT ((I * 8) - 16)
#define RANDOM (((rand() << RSHIFT) % (MAXVALUE / 2)) - MAXVALUE / 4);
#define MINVALUE 0
#define MAXVALUE (255 << SHIFT)
#define THRESHOLD (128 << SHIFT)
#define C 8
#define FSdither(inP, out, errP, Err, Bit, Offset, Element)\
oldErr = Err;\
Err = (errP[Element] + ((Err * 7 + C) >> 4) + ((int)inP[Element] << SHIFT));\
if (Err > THRESHOLD) {\
out |= Bit;\
Err -= MAXVALUE;\
}\
errP[Element + Offset] += ((Err * 3 + C) >> 4);\
errP[Element] = ((Err * 5 + oldErr + C) >> 4);
#ifdef NEW_DITHER
#define FSDline(scan, i, j, plane_size, cErr, mErr, yErr, kErr, cP, mP, yP, kP, n)\
{\
if (scan == 0) { \
for (i = 0; i < plane_size; i++) {\
byte c, y, m, k, bitmask;\
int oldErr;\
bitmask = 0x80;\
for (c = m = y = k = 0; bitmask != 0; bitmask >>= 1) {\
if (n >= 4) {\
FSdither(dp, k, ep, kErr, bitmask, -n, 0);\
}\
if (n >= 3) {\
FSdither(dp, c, ep, cErr, bitmask, -n, n - 3);\
FSdither(dp, m, ep, mErr, bitmask, -n, n - 2);\
}\
FSdither(dp, y, ep, yErr, bitmask, -n, n - 1);\
dp += n, ep += n;\
}\
if (n >= 4)\
*kP++ = k;\
if (n >= 3) {\
*cP++ = c;\
*mP++ = m;\
}\
*yP++ = y;\
}\
} else { \
for (i = 0; i < plane_size; i++) {\
byte c, y, m, k, bitmask;\
int oldErr;\
bitmask = 0x01;\
for (c = m = y = k = 0; bitmask != 0; bitmask <<= 1) {\
dp -= n, ep -= n;\
FSdither(dp, y, ep, yErr, bitmask, n, n - 1);\
if (n >= 3) {\
FSdither(dp, m, ep, mErr, bitmask, n, n - 2);\
FSdither(dp, c, ep, cErr, bitmask, n, n - 3);\
}\
if (n >= 4) {\
FSdither(dp, k, ep, kErr, bitmask, n, 0);\
}\
}\
*--yP = y;\
if (n >= 3)\
{ *--mP = m;\
*--cP = c;\
}\
if (n >= 4)\
*--kP = k;\
}\
}\
}
#else
#define FSDline(scan, i, j, plane_size, cErr, mErr, yErr, kErr, cP, mP, yP, kP, n)\
{\
if (scan == 0) { \
for (i = 0; i < plane_size; i++) {\
byte c, y, m, k, bitmask;\
int oldErr;\
bitmask = 0x80;\
for (c = m = y = k = 0; bitmask != 0; bitmask >>= 1) {\
if (n >= 4) {\
if (*dp) {\
FSdither(dp, k, ep, kErr, bitmask, -n, 0);\
cErr = mErr = yErr = 0;\
} else {\
FSdither(dp, c, ep, cErr, bitmask, -n, n - 3);\
FSdither(dp, m, ep, mErr, bitmask, -n, n - 2);\
FSdither(dp, y, ep, yErr, bitmask, -n, n - 1);\
}\
} else {\
if (n >= 3) {\
FSdither(dp, c, ep, cErr, bitmask, -n, n - 3);\
FSdither(dp, m, ep, mErr, bitmask, -n, n - 2);\
}\
FSdither(dp, y, ep, yErr, bitmask, -n, n - 1);\
}\
dp += n, ep += n;\
}\
if (n >= 4)\
*kP++ = k;\
if (n >= 3) {\
*cP++ = c;\
*mP++ = m;\
}\
*yP++ = y;\
}\
} else { \
for (i = 0; i < plane_size; i++) {\
byte c, y, m, k, bitmask;\
int oldErr;\
bitmask = 0x01;\
for (c = m = y = k = 0; bitmask != 0; bitmask <<= 1) {\
dp -= n, ep -= n;\
if (n >= 4) {\
if (*dp) {\
FSdither(dp, k, ep, kErr, bitmask, n, 0);\
cErr = mErr = yErr = 0;\
} else {\
FSdither(dp, y, ep, yErr, bitmask, n, n - 1);\
FSdither(dp, m, ep, mErr, bitmask, n, n - 2);\
FSdither(dp, c, ep, cErr, bitmask, n, n - 3);\
}\
} else {\
FSdither(dp, y, ep, yErr, bitmask, n, n - 1);\
if (n >= 3) {\
FSdither(dp, m, ep, mErr, bitmask, n, n - 2);\
FSdither(dp, c, ep, cErr, bitmask, n, n - 3);\
}\
}\
}\
*--yP = y;\
if (n >= 3)\
{ *--mP = m;\
*--cP = c;\
}\
if (n >= 4)\
*--kP = k;\
}\
}\
}
#endif
#define CPbit(inP, out, Bit, Element)\
if (inP[Element]) {\
out |= Bit;\
}
#define COPYline(scan, i, j, plane_size, cP, mP, yP, kP, n)\
{\
if (scan == 0) { \
for (i = 0; i < plane_size; i++) {\
byte c, y, m, k, bitmask;\
bitmask = 0x80;\
for (c = m = y = k = 0; bitmask != 0; bitmask >>= 1) {\
if (n >= 4) {\
CPbit(dp, k, bitmask, 0);\
} \
if (n >= 3) {\
CPbit(dp, c, bitmask, n - 3);\
CPbit(dp, m, bitmask, n - 2);\
}\
CPbit(dp, y, bitmask, n - 1);\
dp += n, ep += n;\
}\
if (n >= 4)\
*kP++ = k;\
if (n >= 3) {\
*cP++ = c;\
*mP++ = m;\
}\
*yP++ = y;\
}\
} else { \
for (i = 0; i < plane_size; i++) {\
byte c, y, m, k, bitmask;\
bitmask = 0x01;\
for (c = m = y = k = 0; bitmask != 0; bitmask <<= 1) {\
dp -= n, ep -= n;\
if (n >= 4) {\
CPbit(dp, k, bitmask, 0);\
}\
if (n >= 3) {\
CPbit(dp, m, bitmask, n - 2);\
CPbit(dp, c, bitmask, n - 3);\
}\
CPbit(dp, y, bitmask, n - 1);\
}\
*--yP = y;\
if (n >= 3)\
{ *--mP = m;\
*--cP = c;\
}\
if (n >= 4)\
*--kP = k;\
}\
}\
}
#define x_dpi (pdev->x_pixels_per_inch)
#define y_dpi (pdev->y_pixels_per_inch)
#define CONFIG_16BIT "\033*v6W\000\003\000\005\006\005"
#define CONFIG_24BIT "\033*v6W\000\003\000\010\010\010"
#define calc_buffsize(a, b) (((((a) + ((b) * W) - 1) / ((b) * W))) * W)
#define fputshort(n, f) fputc((n)%256,f);fputc((n)/256,f)
private int
bjc_cmd(byte cmd, int argsize, byte* arg, gx_device_printer* pdev,
FILE* f)
{
fputs("\033(", f);
putc(cmd, f);
fputshort(argsize, f);
fwrite(arg, sizeof(byte), argsize, f);
return 0;
}
private int
bjc_raster_cmd_sub(char c, int rastsize, byte* data, FILE* f)
{
fputs("\033(A", f);
fputshort(rastsize + 1, f);
putc(c, f);
fwrite(data, sizeof(byte), rastsize, f);
putc('\015', f);
return 0;
}
private int
bjc_raster_cmd(int c_id, int rastsize, byte* data, gx_device_printer* pdev,
FILE* f)
{
if (bjcparams.printColors == BJC_COLOR_ALLBLACK) {
bjc_raster_cmd_sub('K', rastsize, data, f);
} else if (pdev->color_info.num_components == 1) {
if (bjcparams.printColors & BJC_COLOR_BLACK) {
bjc_raster_cmd_sub('K', rastsize, data, f);
} else {
if (bjcparams.printColors & BJC_COLOR_YELLOW)
bjc_raster_cmd_sub('Y', rastsize, data, f);
if (bjcparams.printColors & BJC_COLOR_MAGENTA)
bjc_raster_cmd_sub('M', rastsize, data, f);
if (bjcparams.printColors & BJC_COLOR_CYAN)
bjc_raster_cmd_sub('C', rastsize, data, f);
}
}else {
private byte ymckCodes[] = {
BJC_COLOR_YELLOW,
BJC_COLOR_MAGENTA,
BJC_COLOR_CYAN,
BJC_COLOR_BLACK,
};
if (bjcparams.printColors & (int) ymckCodes[c_id]) {
bjc_raster_cmd_sub("YMCK"[c_id], rastsize, data, f);
}
}
return 0;
}
private int
bjc_init_page(gx_device_printer* pdev, FILE* f)
{
byte pagemargins[3], resolution[4], paperloading[2];
pagemargins[0] = (byte) ((float) pdev->height / pdev->y_pixels_per_inch
* 10 + .5);
pagemargins[1] = (byte) 1;
pagemargins[2] = (byte) ((pdev->width / pdev->x_pixels_per_inch * 10) -
pdev->HWMargins[0] / 7.2 - pdev->HWMargins[2] / 7.2 + .5);
switch (bjc->ptype) {
case BJC800:
if (pagemargins[2] > 114) pagemargins[2] = 114;
break;
default:
if (pagemargins[2] > 80) pagemargins[2] = 80;
break;
}
resolution[0] = (byte) ((int)pdev->y_pixels_per_inch / 256);
resolution[1] = (byte) ((int)pdev->y_pixels_per_inch % 256);
resolution[2] = (byte) ((int)pdev->x_pixels_per_inch / 256);
resolution[3] = (byte) ((int)pdev->x_pixels_per_inch % 256);
paperloading[0] = 0x10 + ((1 - bjcparams.manualFeed) << 2);
paperloading[1] = bjcparams.mediaType << 4;
fputs("\033[K", f);
fputshort(2, f);
fputc(0x00, f);
fputc(0x0f, f);
bjc_cmd('a', 1, (byte*) "\001", pdev, f);
bjc_cmd('g', 3, pagemargins, pdev, f);
bjc_cmd('b', 1, (byte*) "\001", pdev, f);
bjc_cmd('l', 2, paperloading, pdev, f);
#ifndef BJC_INIT_800_AS_600
if (bjc->ptype == BJC800) {
#else
if (0) {
#endif
byte printmode[2];
printmode[0] = bjcparams.printQuality;
switch (bjcparams.printQuality) {
case BJC_QUALITY_DRAFT:
printmode[0] = 4;
break;
}
printmode[1] = (bjcparams.mediaType >= BJC_MEDIA_ENVELOPE ? 1 :
bjc800thickpaper());
bjc_cmd('c', 2, printmode, pdev, f);
} else {
byte printmeth[3];
printmeth[0] = 0x10 + ((1 - bjcparams.manualFeed) << 2);
printmeth[1] = (bjcparams.mediaType << 4) + bjcparams.printQuality;
printmeth[2] = (bjcparams.printQuality == BJC_QUALITY_HIGH ?
0x10 : 0) + (bjcparams.mediaType >= BJC_MEDIA_ENVELOPE ? 1 :
bjc600thickpaper());
bjc_cmd('c', 3, printmeth, pdev, f);
}
bjc_cmd('d', 4, resolution, pdev, f);
return 0;
}
private int
bjc_v_skip(int n, gx_device_printer* pdev, FILE* f)
{
if (n) {
fputs("\033(e", f);
putc(2, f);
putc(0, f);
putc(n / 256, f);
putc(n % 256, f);
}
return 0;
}
private int
bjc_finish_page(gx_device_printer* pdev, FILE* f)
{
bjc_cmd('a', 1, (byte*) "\000", pdev, f);
bjc_cmd('b', 1, (byte*) "\000", pdev, f);
fputc('\014', f);
fputs("\033@", f);
return 0;
}
private int
bjc_compress(const byte *row, const byte *end_row, byte *compressed)
{
register const byte *exam = row;
register byte *cptr = compressed;
while ( exam < end_row ) {
const byte *compr = exam;
const byte *end_dis;
const byte *next;
register byte test, test2;
test = *exam;
while ( exam < end_row ) {
test2 = *++exam;
if ( test == test2 )
break;
test = test2;
}
end_dis = exam - 1;
if ( exam == end_row ) {
next = --end_row;
} else {
next = exam + 1;
while ( next < end_row && *next == test ) next++;
}
for ( ; ; ) {
uint count = end_dis - compr;
switch ( count ) {
case 6: cptr[6] = compr[5];
case 5: cptr[5] = compr[4];
case 4: cptr[4] = compr[3];
case 3: cptr[3] = compr[2];
case 2: cptr[2] = compr[1];
case 1: cptr[1] = compr[0];
*cptr = count - 1;
cptr += count + 1;
case 0:
break;
default:
if ( count > 128 ) count = 128;
*cptr++ = count - 1;
memcpy(cptr, compr, count);
cptr += count, compr += count;
continue;
}
break;
}
{
int count = next - end_dis;
if (next < end_row || test != 0)
while ( count > 0 ) {
int this = (count > 128 ? 128 : count);
*cptr++ = 257 - this;
*cptr++ = (byte)test;
count -= this;
}
exam = next;
}
}
return cptr - compressed;
}
private word *ep_storage;
private uint ep_storage_size_words;
private byte *ep_raster_buf[4][BJC_HEAD_ROWS], *ep_print_buf;
private int ep_num_comps, ep_plane_size, img_rows=BJC_HEAD_ROWS;
#define row_bytes (img_rows / 8)
#define row_words (row_bytes / sizeof(word))
#define min_rows (32)
private int
ep_print_image(FILE *prn_stream, char cmd, byte *data, int size)
{
static int ln_idx=0, vskip1=0, vskip2=0, real_rows;
int i;
static const char color[4] = {4,1,2,0};
switch (cmd) {
case 3:
case 2:
case 1:
case 0:
memcpy(ep_raster_buf[((int) cmd)][ln_idx+vskip2], data, size);
return 0;
case 'B':
if (!ln_idx) {
vskip1 += size;
} else if (size >= img_rows - (ln_idx+vskip2) || ln_idx+vskip2 >= min_rows) {
vskip2 += size;
ep_print_image(prn_stream, 'F', 0, 0);
} else {
vskip2 += size;
}
return 0;
case 'I':
ln_idx += vskip2 + 1;
vskip2 = 0;
if (ln_idx < img_rows) return 0;
case 'F':
if (!ln_idx) return 0;
while (vskip1 >= (255*2)) {
fputs("\033J\377", prn_stream);
vskip1 -= (255*2);
}
if (vskip1 > 255) {
fputs("\033J\200", prn_stream);
vskip1 -= 256;
}
if (vskip1) {
fputs("\033|J", prn_stream); putc(0, prn_stream); putc(vskip1, prn_stream);
}
if (ln_idx > 56) {
real_rows = 64;
} else if (ln_idx > 48) {
real_rows = 56;
} else if (ln_idx > 32) {
real_rows = 48;
} else {
real_rows = 32;
}
for (i = 0; i < ep_num_comps; i++) {
int lnum, hskip, print_size, img_rows;
byte *p0, *p1, *p2, *p3;
byte *inp, *inbuf, *outp, *outbuf;
img_rows = real_rows;
outbuf = ep_print_buf;
for (lnum=0; lnum < img_rows; lnum+=8, outbuf++) {
inbuf = inp = ep_raster_buf[i][lnum];
for (outp = outbuf; inp < inbuf+ep_plane_size; inp++, outp += img_rows) {
memflip8x8(inp, ep_plane_size, outp, row_bytes);
}
}
if (ep_num_comps == 1) {
putc('\015', prn_stream);
} else {
fputs("\015\033r", prn_stream);
putc(color[i], prn_stream);
}
*(outp = ep_print_buf + ep_plane_size * img_rows) = 1;
p0 = p3 = ep_print_buf;
while (p0 < outp) {
static const word zeros[8] = {0,0,0,0,0,0,0,0};
if (p3 < outp) {
for (p1 = p3; !memcmp(p3, zeros, row_bytes*2); p3 += row_bytes*2);
p2 = p3;
redo:
for (p3 += row_bytes; memcmp(p3, zeros, row_bytes); p3 += row_bytes);
if (p3 < outp && memcmp(p3+row_bytes, zeros, row_bytes)) goto redo;
} else p1 = p2 = outp;
if (p0 < p1) {
print_size = ((p1 < outp) ? p1 : outp) - p0;
fputs("\033|B", prn_stream); putc(img_rows, prn_stream);
fputshort(print_size, prn_stream);
fwrite(p0, sizeof(byte), print_size, prn_stream);
}
if (p1 < p2) {
hskip = (((p2 < outp) ? p2 : outp) - p1) / row_bytes / 2;
fputs("\033\\", prn_stream);
fputshort(hskip, prn_stream);
}
p0 = p2;
}
}
return ep_print_image(prn_stream, 'R', 0, vskip2 + ln_idx);
case 'R':
ln_idx = 0;
vskip1 = size;
vskip2 = 0;
memset(ep_storage, 0, ep_storage_size_words * W);
return 0;
default:
errprintf("ep_print_image: illegal command character `%c'.\n", cmd);
return 1;
}
}
private int
hp_colour_print_page(gx_device_printer * pdev, FILE * prn_stream, int ptype)
{
uint raster_width = gdev_prn_rasterwidth(pdev, 1);
int line_size = gdev_prn_raster(pdev);
int line_size_words = (line_size + W - 1) / W;
int paper_size = gdev_pcl_paper_size((gx_device *)pdev);
int num_comps = pdev->color_info.num_components;
int bits_per_pixel = pdev->color_info.depth;
int storage_bpp = bits_per_pixel;
int expanded_bpp = bits_per_pixel;
int plane_size, databuff_size;
int combined_escapes = 1;
int errbuff_size = 0;
int outbuff_size = 0;
int compression = 0;
int scan = 0;
int *errors[2];
const char *cid_string = (const char*) 0;
byte *data[4], *plane_data[4][4], *out_data;
byte *out_row, *out_row_alt;
word *storage;
uint storage_size_words;
switch (ptype) {
case DJ550C:
if (num_comps == 3 && !cprn_device->cmyk)
num_comps = 4;
break;
case ESC_P:
if (bits_per_pixel == 24)
num_comps = 3;
else
if (num_comps != 1)
num_comps = 4;
break;
case PJXL300:
case PJXL180:
if (pjxl->rendertype > 0) {
if (bits_per_pixel < 16)
pjxl->rendertype = 0;
else {
cid_string = (bits_per_pixel == 16) ? CONFIG_16BIT : CONFIG_24BIT;
bits_per_pixel = storage_bpp = expanded_bpp = 1;
num_comps = 1;
}
}
break;
}
if (cprn_device->cmyk <= 0) {
if (storage_bpp == 8 && num_comps >= 3)
bits_per_pixel = expanded_bpp = 3;
}
plane_size = calc_buffsize(line_size, storage_bpp);
ep_plane_size = plane_size;
if (bits_per_pixel == 1) {
databuff_size = 0;
outbuff_size = plane_size * 4;
}
if (bits_per_pixel > 4) {
storage_bpp = expanded_bpp =
num_comps * 8;
if (cprn_device->cmyk > 0) {
errbuff_size = 4 * (5 + 1 + 1 + line_size + 1 + 2) * I;
} else {
errbuff_size =
calc_buffsize((plane_size * expanded_bpp + num_comps * 4) * I, 1);
}
}
databuff_size = plane_size * storage_bpp;
storage_size_words = ((plane_size + plane_size) * num_comps +
databuff_size + errbuff_size + outbuff_size) / W;
storage = (ulong *) gs_malloc(pdev->memory, storage_size_words, W, "hp_colour_print_page");
ep_storage_size_words = (plane_size * (num_comps + 1)) / W * img_rows
+ 16;
ep_storage = (word *) gs_malloc(pdev->memory, ep_storage_size_words, W, "ep_print_buffer");
if (storage == 0 || ep_storage == 0)
return_error(gs_error_VMerror);
else {
int i, j;
byte *p = out_data = out_row = (byte *)storage;
byte *ep_p = (byte *)ep_storage;
data[0] = data[1] = data[2] = p;
data[3] = p + databuff_size;
out_row_alt = out_row + plane_size * 2;
if (bits_per_pixel > 1) {
p += databuff_size;
}
if (bits_per_pixel > 4) {
errors[0] = (int *)p + num_comps * 2;
errors[1] = errors[0] + databuff_size;
p += errbuff_size;
}
for (i = 0; i < num_comps; i++) {
plane_data[0][i] = plane_data[2][i] = p;
p += plane_size;
}
for (i = 0; i < num_comps; i++) {
plane_data[1][i] = p;
plane_data[3][i] = p + plane_size;
p += plane_size;
}
if (bits_per_pixel == 1) {
out_data = out_row = p;
out_row_alt = out_row + plane_size * 2;
data[1] += databuff_size;
data[3] += databuff_size;
}
for (i = 0; i < num_comps; i++) {
for (j = 0; j < img_rows; j++) {
ep_raster_buf[i][j] = ep_p;
ep_p += plane_size;
}
ep_print_buf = (byte *)((word)(ep_p + sizeof(word)) & ~(sizeof(word)-1));
}
ep_num_comps = num_comps;
}
if (ptype == BJC600 || ptype == BJC800) {
bjc_init_page(pdev, prn_stream);
} else {
if (ptype == LJ4DITH) {
fputs("\033*rB", prn_stream);
} else {
fputs("\033*rbC", prn_stream);
}
fprintf(prn_stream, "\033*t%dR", (int)x_dpi);
}
memset(storage, 0, storage_size_words * W);
#define DOFFSET (dev_t_margin(pdev) - DESKJET_PRINT_LIMIT)
#define POFFSET (dev_t_margin(pdev) - PAINTJET_PRINT_LIMIT)
#define EOFFSET (dev_t_margin(pdev) - ESC_P_PRINT_LIMIT)
#define BOFFSET (dev_t_margin(pdev) - bjc->printLimit)
switch (ptype) {
case LJ4DITH:
fprintf(prn_stream, "\033&l26A\033&l0o0e0L\033*r0F" );
fprintf(prn_stream, "\033*p0x0Y" );
fprintf(prn_stream, "\033&u600D\033*r1A" );
compression = 3;
combined_escapes = 0;
break;
case DJ500C:
case DJ550C:
fprintf(prn_stream, "\033&l%daolE", paper_size);
fprintf(prn_stream, "\033*o%dd%dQ", cdj->depletion, cdj->shingling);
fprintf(prn_stream, "\033*p%dY", (int)(300 * DOFFSET));
fprintf(prn_stream, "\033*r%ds-%du0A", raster_width, num_comps);
compression = 9;
break;
case DNJ650C:
if (pdev->x_pixels_per_inch == 600) {
fprintf(prn_stream,"\033%%-12345X@PJL SET RESOLUTION = 600\n");
}
fprintf (prn_stream, "\033%%0B");
fprintf (prn_stream, "BP5,1");
fprintf (prn_stream, "PS%d,%d",
(int)((pdev->height/pdev->y_pixels_per_inch)*1016),
(int)((pdev->width/pdev->x_pixels_per_inch)*1016));
fprintf (prn_stream, "PU");
fprintf (prn_stream, "PA%d,%d", 0, 0);
fprintf (prn_stream, "\033%%1A");
fprintf (prn_stream, "\033&a1N");
if (pdev->x_pixels_per_inch == 600)
fprintf (prn_stream, "\033*t600R");
{ static const char temp[] = {
033, '*', 'v', '6', 'W',
000 ,
000 ,
003 ,
010 ,
010 ,
010
};
fwrite (temp, 1, sizeof(temp), prn_stream);
}
fprintf(prn_stream, "\033*r%dS", raster_width);
fprintf(prn_stream, "\033*r1A");
compression = 1;
combined_escapes = 0;
break;
case PJXL300:
fprintf(prn_stream, "\033&l%daolE", paper_size);
fprintf(prn_stream, "\033&a1N");
fprintf(prn_stream, "\033*o%dQ", pjxl->printqual);
fprintf(prn_stream, "\033*p%dY", (int)(300 * POFFSET));
if (pjxl->rendertype > 0) {
fprintf(prn_stream, "\033*t%dJ", pjxl->rendertype);
fputs(cid_string, prn_stream);
fprintf(prn_stream, "\033*r%ds1A", raster_width);
} else {
fprintf(prn_stream, "\033*r%ds-%du0A", raster_width, num_comps);
}
combined_escapes = 0;
break;
case PJXL180:
fprintf(prn_stream, "\033&l%daolE", paper_size);
fprintf(prn_stream, "\033*o%dQ", pjxl->printqual);
fprintf(prn_stream, "\033*p%dY", (int)(180 * POFFSET));
if (pjxl->rendertype > 0) {
fprintf(prn_stream, "\033*t%dJ", pjxl->rendertype);
fputs(cid_string, prn_stream);
fprintf(prn_stream, "\033*r%ds1A", raster_width);
} else {
fprintf(prn_stream, "\033*r%ds%du0A", raster_width, num_comps);
}
break;
case PJ180:
case DECLJ250:
fprintf(prn_stream, "\033&lL");
fprintf(prn_stream, "\033&a%dV", (int)(720 * POFFSET));
fprintf(prn_stream, "\033*r%ds%du0A", raster_width, num_comps);
if (ptype == DECLJ250) {
combined_escapes = 0;
ptype = PJ180;
}
compression = 1;
break;
case ESC_P:
if ((int)(EOFFSET*360)) fprintf(prn_stream, "\033|J%c%c", 0, (int)(360*EOFFSET));
combined_escapes = 0;
break;
case BJC600:
case BJC800:
bjc_v_skip((int)(pdev->HWResolution[1] * BOFFSET), pdev, prn_stream);
combined_escapes = 0;
compression = 2;
break;
}
if (combined_escapes) {
fputs("\033*b", prn_stream);
if (compression)
fprintf(prn_stream, "%dm", compression);
}
else if (ptype == BJC600 || ptype == BJC800)
;
else
if (compression)
fprintf(prn_stream, "\033*b%dM", compression);
{
int cErr, mErr, yErr, kErr;
int this_pass, lnum, i;
int start_rows;
int lend, num_blank_lines = 0;
word rmask = ~(word) 0 << ((-pdev->width * storage_bpp) & (W * 8 - 1));
lend = pdev->height -
(int)((dev_t_margin(pdev) + dev_b_margin(pdev)) * y_dpi);
switch (ptype) {
case BJC600:
case BJC800:
start_rows = BJC_HEAD_ROWS;
break;
case PJ180:
case PJXL180:
case PJXL300:
start_rows = -1;
break;
default:
start_rows = (num_comps == 1) ? HEAD_ROWS_MONO - 1 :
HEAD_ROWS_COLOUR - 1;
break;
}
cErr = mErr = yErr = kErr = 0;
if (bits_per_pixel > 4) {
if (cprn_device->cmyk > 0 && expanded_bpp == 32) {
bjc_fscmyk(data, plane_data, errors, plane_size, -1);
} else {
int *ep = errors[0];
for (i = 0; i < databuff_size; i++) {
*ep++ = RANDOM;
}
}
}
this_pass = start_rows;
for (lnum = 0; lnum < lend; lnum++) {
word *data_words = (word *)data[scan];
register word *end_data = data_words + line_size_words;
gdev_prn_copy_scan_lines(pdev, lnum, data[scan], line_size);
end_data[-1] &= rmask;
while (end_data > data_words && end_data[-1] == 0)
end_data--;
if (ptype != DNJ650C)
if (end_data == data_words) {
num_blank_lines++;
continue;
}
if (num_blank_lines > 0) {
if (ptype == ESC_P) {
ep_print_image(prn_stream, 'B', 0, num_blank_lines);
} else if (ptype == BJC600 || ptype == BJC800) {
bjc_v_skip(num_blank_lines, pdev, prn_stream);
} else if (num_blank_lines < this_pass) {
this_pass -= num_blank_lines;
if (combined_escapes) {
fputc('y', prn_stream);
for (; num_blank_lines; num_blank_lines--)
fputc('w', prn_stream);
} else {
#if 0
fputs("\033*b1Y", prn_stream);
if ( num_blank_lines > 1 )
fprintf(prn_stream, "\033*b%dY", num_blank_lines - 1);
num_blank_lines = 0;
#else
fputs("\033*bY", prn_stream);
if (ptype == DNJ650C) {
fprintf (prn_stream, "\033*b%dY", num_blank_lines);
num_blank_lines = 0;
}
else {
for (; num_blank_lines; num_blank_lines--)
fputs("\033*bW", prn_stream);
}
#endif
}
} else {
if (combined_escapes)
fprintf(prn_stream, "%dy", num_blank_lines);
else
fprintf(prn_stream, "\033*b%dY", num_blank_lines);
}
memset(plane_data[1 - scan][0], 0, plane_size * num_comps);
num_blank_lines = 0;
this_pass = start_rows;
}
{
register byte *kP = plane_data[scan + 2][3];
register byte *cP = plane_data[scan + 2][2];
register byte *mP = plane_data[scan + 2][1];
register byte *yP = plane_data[scan + 2][0];
register byte *dp = data[scan + 2];
register int *ep = errors[scan];
int zero_row_count;
int i, j;
byte *odp;
if (this_pass)
this_pass--;
else
this_pass = start_rows;
if (expanded_bpp > bits_per_pixel) {
cdj_expand_line(data_words, line_size,
cprn_device->cmyk,
bits_per_pixel, expanded_bpp);
}
switch (expanded_bpp) {
case 3:
for (i = 0, odp = plane_data[scan][0]; i < databuff_size;
i += 8, odp++) {
#define spread3(c)\
{ 0, c, c*0x100, c*0x101, c*0x10000L, c*0x10001L, c*0x10100L, c*0x10101L }
static ulong spr40[8] = spread3(0x40);
static ulong spr08[8] = spread3(8);
static ulong spr02[8] = spread3(2);
register byte *dp = data[scan] + i;
register ulong pword =
(spr40[dp[0]] << 1) +
(spr40[dp[1]]) +
(spr40[dp[2]] >> 1) +
(spr08[dp[3]] << 1) +
(spr08[dp[4]]) +
(spr08[dp[5]] >> 1) +
(spr02[dp[6]]) +
(spr02[dp[7]] >> 1);
odp[0] = (byte) (pword >> 16);
odp[plane_size] = (byte) (pword >> 8);
odp[plane_size * 2] = (byte) (pword);
}
break;
case 8:
switch (ptype) {
case BJC600:
case BJC800:
if (bjcparams.ditheringType == BJC_DITHER_NONE) {
COPYline(scan, i, j, plane_size, cP, mP, yP, kP, 1);
break;
}
default:
FSDline(scan, i, j, plane_size, cErr, mErr, yErr, kErr,
cP, mP, yP, kP, 1);
}
break;
case 24:
FSDline(scan, i, j, plane_size, cErr, mErr, yErr, kErr,
cP, mP, yP, kP, 3);
break;
case 32:
if (cprn_device->cmyk > 0) {
bjc_fscmyk(data, plane_data, errors, plane_size, scan);
} else {
FSDline(scan, i, j, plane_size, cErr, mErr, yErr, kErr,
cP, mP, yP, kP, 4);
}
break;
}
if (num_comps == 4 && (cprn_device->cmyk <= 0 || expanded_bpp != 32)) {
register word *kp = (word *)plane_data[scan][3];
register word *cp = (word *)plane_data[scan][2];
register word *mp = (word *)plane_data[scan][1];
register word *yp = (word *)plane_data[scan][0];
if (bits_per_pixel > 4) {
for (i = 0; i < plane_size / W; i++) {
word bits = *cp & *mp & *yp;
*kp++ |= bits;
bits = ~bits;
*cp++ &= bits;
*mp++ &= bits;
*yp++ &= bits;
}
} else {
for (i = 0; i < plane_size / W; i++) {
word bits = *cp & *mp & *yp;
*kp++ = bits;
bits = ~bits;
*cp++ &= bits;
*mp++ &= bits;
*yp++ &= bits;
}
}
}
for (zero_row_count = 0, i = num_comps - 1; i >= 0; i--) {
int output_plane = 1;
int out_count = 0;
switch (ptype) {
case DJ500C:
case DJ550C:
out_count = gdev_pcl_mode9compress(plane_size,
plane_data[scan][i],
plane_data[1 - scan][i],
out_data);
if (out_count == 0)
{ output_plane = 0;
if (i == 0)
fputc('w', prn_stream);
else
zero_row_count++;
}
else
{ for (; zero_row_count; zero_row_count--)
fputc('v', prn_stream);
}
break;
case PJ180:
case DNJ650C:
if (num_comps > 1)
{ word *wp = (word *)plane_data[scan][i];
for (j = 0; j < plane_size / W; j++, wp++)
*wp = ~*wp;
}
out_count = gdev_pcl_mode1compress((const byte *)
plane_data[scan][i],
(const byte *)
plane_data[scan][i] + plane_size - 1,
out_data);
break;
case PJXL180:
if (num_comps > 1)
{ word *wp = (word *)plane_data[scan][i];
for (j = 0; j < plane_size / W; j++, wp++)
*wp = ~*wp;
}
case PJXL300:
case LJ4DITH:
{ const byte *plane = plane_data[scan][i];
byte *prev_plane = plane_data[1 - scan][i];
const word *row = (word *)plane;
const word *end_row = row + plane_size/W;
int count2 = gdev_pcl_mode2compress(row, end_row, out_row_alt);
int count3 = gdev_pcl_mode3compress(plane_size, plane, prev_plane, out_row);
int penalty = combined_escapes ? strlen("#m") : strlen("\033*b#M");
int penalty2 = (compression == 2 ? 0 : penalty);
int penalty3 = (compression == 3 ? 0 : penalty);
if (count3 + penalty3 < count2 + penalty2)
{ if ( compression != 3 ) {
if (combined_escapes)
fputs("3m", prn_stream);
else
fputs("\033*b3M", prn_stream);
compression = 3;
}
out_data = out_row;
out_count = count3;
}
else
{ if ( compression != 2 ) {
if (combined_escapes)
fputs("2m", prn_stream);
else
fputs("\033*b2M", prn_stream);
compression = 2;
}
out_data = out_row_alt;
out_count = count2;
}
}
break;
case BJC600:
case BJC800:
{ const byte *plane = (byte *)plane_data[scan][i];
int count2 = bjc_compress(plane, plane + plane_size, out_row_alt);
out_data = out_row_alt;
out_count = count2;
}
break;
}
if (output_plane) {
if (combined_escapes)
fprintf(prn_stream, "%d%c", out_count, "wvvv"[i]);
else if (ptype == BJC600 || ptype == BJC800) {
if (out_count)
bjc_raster_cmd(num_comps == 1 ? 3 : i,
out_count, out_data, pdev, prn_stream);
if (i == 0) bjc_v_skip(1, pdev, prn_stream);
} else if (ptype == ESC_P)
ep_print_image(prn_stream, (char)i, plane_data[scan][i], plane_size);
else
fprintf(prn_stream, "\033*b%d%c", out_count, "WVVV"[i]);
if (ptype < ESC_P)
fwrite(out_data, sizeof(byte), out_count, prn_stream);
}
}
if (ptype == ESC_P)
ep_print_image(prn_stream, 'I', 0, 0);
scan = 1 - scan;
}
}
}
if (combined_escapes)
fputs("0M", prn_stream);
if (ptype == BJC600 || ptype == BJC800) {
bjc_finish_page(pdev, prn_stream);
}
else if (ptype != ESC_P)
fputs("\033*rbC\033E", prn_stream);
if (ptype == PJ180)
fputc('\f', prn_stream);
else if (ptype == DNJ650C)
fputs ("\033*rC\033%0BPG;", prn_stream);
else if (ptype == BJC600 || ptype == BJC800)
;
else if (ptype == ESC_P) {
ep_print_image(prn_stream, 'F', 0, 0);
fputs("\014\033@", prn_stream);
} else
fputs("\033&l0H", prn_stream);
gs_free(pdev->memory, (char *) ep_storage, ep_storage_size_words, W, "ep_print_buffer");
gs_free(pdev->memory, (char *) storage, storage_size_words, W, "hp_colour_print_page");
return 0;
}
private int
gdev_pcl_mode1compress(const byte *row, const byte *end_row, byte *compressed)
{ register const byte *in = row;
register byte *out = compressed;
while ( in < end_row )
{ byte test = *in++;
const byte *run = in;
while ( in < end_row && *in == test ) in++;
while ( in - run > 255 )
{ *out++ = 255;
*out++ = test;
run += 256;
}
*out++ = in - run;
*out++ = test;
}
return out - compressed;
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
gdev_cmyk_map_cmyk_color(gx_device* pdev, const gx_color_value cv[])
{
gx_color_value cyan, magenta, yellow, black;
gx_color_index color;
cyan = cv[0]; magenta = cv[1]; yellow = cv[2]; black = cv[3];
switch (pdev->color_info.depth) {
case 1:
color = (cyan | magenta | yellow | black) > gx_max_color_value / 2 ?
(gx_color_index) 1 : (gx_color_index) 0;
break;
default: {
int nbits = pdev->color_info.depth;
color = gx_cmyk_value_bits(cyan, magenta, yellow, black,
nbits >> 2);
}
}
return color;
}
private gx_color_index
gdev_cmyk_map_rgb_color(gx_device *pdev, const gx_color_value cv[])
{
gx_color_value r, g, b;
r = cv[0]; g = cv[1]; b = cv[2];
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
gdev_cmyk_map_color_cmyk(gx_device *pdev, gx_color_index color, gx_color_value prgb[3])
{
switch (pdev->color_info.depth) {
case 1:
prgb[0] = gx_max_color_value * (1 - color);
break;
case 8:
if (pdev->color_info.num_components == 1) {
gx_color_value value = (gx_color_value) color ^ 0xff;
prgb[0] = (value << 8) + value;
break;
}
default: {
unsigned long bcyan, bmagenta, byellow, black;
int nbits = pdev->color_info.depth;
gx_value_cmyk_bits(color, bcyan, bmagenta, byellow, black,
nbits >> 2);
prgb[0] = bcyan;
prgb[1] = bmagenta;
prgb[2] = byellow;
prgb[3] = black;
}
}
return 0;
}
#define bg_and_ucr(c, c_v, m, m_v, y, y_v, k) \
do { \
register byte cv = c_v, mv = m_v, yv = y_v, kv; \
\
kv = (cv > mv ? mv : cv); \
kv = (yv > k ? k : y); \
y = yv - kv; m = mv - kv; c = cv -kv; k = kv; \
} while (0)
private gx_color_index
gdev_pcl_map_rgb_color(gx_device *pdev, const gx_color_value cv[])
{
gx_color_value r, g, b;
r = cv[0]; g = cv[1]; b = cv[2];
if (gx_color_value_to_byte(r & g & b) == 0xff)
return (gx_color_index)0;
else {
int correction = cprn_device->correction;
gx_color_value c = gx_max_color_value - r;
gx_color_value m = gx_max_color_value - g;
gx_color_value y = gx_max_color_value - b;
if (correction) {
ulong maxval, minval, range;
maxval = c >= m ? (c >= y ? c : y) : (m >= y ? m : y);
if (maxval > 0) {
minval = c <= m ? (c <= y ? c : y) : (m <= y? m : y);
range = maxval - minval;
#define shift (gx_color_value_bits - 12)
c = ((c >> shift) * (range + (maxval * correction))) /
((maxval * (correction + 1)) >> shift);
}
}
switch (pdev->color_info.depth) {
case 1:
return ((c | m | y) > gx_max_color_value / 2 ?
(gx_color_index)1 : (gx_color_index)0);
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
return ((((ulong)c * red_weight +
(ulong)m * green_weight +
(ulong)y * blue_weight)
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
((ulong)gx_color_value_to_byte(c) << 16));
case 32:
{ return ((c == m && c == y) ? ((ulong)gx_color_value_to_byte(c) << 24)
: (gx_color_value_to_byte(y) +
(gx_color_value_to_byte(m) << 8) +
((ulong)gx_color_value_to_byte(c) << 16)));
}
}
}
return (gx_color_index)0;
}
private int
gdev_pcl_map_color_rgb(gx_device *pdev, gx_color_index color,
gx_color_value prgb[3])
{
switch (pdev->color_info.depth) {
case 1:
prgb[0] = prgb[1] = prgb[2] = -((gx_color_value)color ^ 1);
break;
case 8:
if (pdev->color_info.num_components >= 3)
{ gx_color_value c = (gx_color_value)color ^ 7;
prgb[0] = -(c & 1);
prgb[1] = -((c >> 1) & 1);
prgb[2] = -(c >> 2);
}
else
{ gx_color_value value = (gx_color_value)color ^ 0xff;
prgb[0] = prgb[1] = prgb[2] = (value << 8) + value;
}
break;
case 16:
{ gx_color_value c = (gx_color_value)color ^ 0xffff;
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
{ gx_color_index c = color ^ 0xffffff;
prgb[0] = gx_color_value_from_byte((gx_color_value)(c >> 16));
prgb[1] = gx_color_value_from_byte((gx_color_value)((c >> 8) & 0xff));
prgb[2] = gx_color_value_from_byte((gx_color_value)(c & 0xff));
}
break;
case 32:
#define gx_maxcol gx_color_value_from_byte(gx_color_value_to_byte(gx_max_color_value))
{ gx_color_value w = gx_maxcol - gx_color_value_from_byte(color >> 24);
prgb[0] = w - gx_color_value_from_byte((color >> 16) & 0xff);
prgb[1] = w - gx_color_value_from_byte((color >> 8) & 0xff);
prgb[2] = w - gx_color_value_from_byte(color & 0xff);
}
break;
}
return 0;
}
private void
cdj_expand_line(word *line, int linesize, short cmyk, int bpp, int ebpp)
{
int endline = linesize;
byte *start = (byte *)line;
register byte *in, *out;
if (cmyk > 0) {
if (bpp == 8) {
in = start + endline;
out = start + (endline *= 2);
while (in > start) {
register byte b0;
register byte bs0, bs1, bs2, bs3;
b0 = *--in;
bs0 = b0 & 0x03;
bs1 = (b0 >> 2) & 0x03;
bs2 = (b0 >> 4) & 0x03;
bs3 = (b0 >> 6) & 0x03;
*--out = (bs0 << 2) + bs0 + (bs1 << 6) + (bs1 << 4);
*--out = (bs2 << 2) + bs2 + (bs3 << 6) + (bs3 << 4);
}
}
if (bpp == 24) {
endline = (endline + 2) / 3;
in = start + endline * 3;
out = start + endline * 4;
while (in > start) {
register byte b0, b1, b2;
b0 = *--in;
b1 = *--in;
b2 = *--in;
*--out = (b0 << 2) + ((b0 >> 4) & 0x03);
*--out = ((b1 & 0x0f) << 4) + ((b0 >> 6) << 2)
+ ((b1 >> 2) & 0x03);
*--out = ((b2 & 0x03) << 6) + ((b1 >> 4) << 2) + (b2 & 0x03);
*--out = (b2 & 0xfc) + ((b2 >> 6) & 0x03);
}
} else if (ebpp == 32) {
endline = (endline + 1) / 2;
in = start + endline * 2;
out = start + (endline *= 4);
while (in > start) {
register byte b0, b1;
b0 = *--in;
b1 = *--in;
*--out = (b0 << 4) + ((b0 >> 4) & 0x07);
*--out = (b0 & 0xf0) + ((b0 >> 4) & 0xf);
*--out = (b1 << 4) + ((b1 >> 4) & 0x0f);
*--out = (b1 & 0xf0) + ((b1 >> 4) & 0xf);
}
}
} else {
if (bpp == 16)
{ register byte b0, b1;
endline = ((endline + 1) / 2);
in = start + endline * 2;
out = start + (endline *= 3);
while (in > start)
{ b0 = *--in;
b1 = *--in;
*--out = (b0 << 3) + ((b0 >> 2) & 0x7);
*--out = (b1 << 5) + ((b0 >> 3) & 0x1c) + ((b1 >> 1) & 0x3);
*--out = (b1 & 0xf8) + (b1 >> 5);
}
}
if (ebpp == 32)
{ register byte c, m, y;
endline = ((endline + 2) / 3);
in = start + endline * 3;
out = start + endline * 4;
while (in > start)
{
y = *--in;
m = *--in;
c = *--in;
if (c == y && c == m) {
*--out = 0, *--out = 0, *--out = 0;
*--out = c;
} else {
*--out = y, *--out = m, *--out = c;
*--out = 0;
}
}
}
}
}
private int
cdj_put_param_int(gs_param_list *plist, gs_param_name pname, int *pvalue,
int minval, int maxval, int ecode)
{ int code, value;
switch ( code = param_read_int(plist, pname, &value) )
{
default:
return code;
case 1:
return ecode;
case 0:
if ( value < minval || value > maxval )
param_signal_error(plist, pname, gs_error_rangecheck);
*pvalue = value;
return (ecode < 0 ? ecode : 1);
}
}
private int
cdj_set_bpp(gx_device *pdev, int bpp, int ccomps)
{ gx_device_color_info *ci = &pdev->color_info;
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
dev_proc(pdev, encode_color) = gdev_cmyk_map_cmyk_color;
dev_proc(pdev, map_rgb_color) = NULL;
dev_proc(pdev, decode_color) = gdev_cmyk_map_color_cmyk;
if (pdev->is_open) gs_closedevice(pdev);
}
switch ( bpp )
{
case 16:
case 32:
if (cprn_device->cmyk && ccomps && ccomps != 4) goto bppe;
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
if (ccomps != 1) break;
} else {
break;
}
case 1:
if (ccomps != 1) goto bppe;
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
bppe: return gs_error_rangecheck;
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
if (bpp != 1 && bpp != 8) goto cce;
break;
case 4:
if (cprn_device->cmyk) {
if (bpp >= 8) break;
}
case 3:
if (bpp == 1 || bpp == 3 || bpp == 8 || bpp == 16
|| bpp == 24 || bpp == 32) {
break;
}
cce: default: return gs_error_rangecheck;
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
ci->dither_grays = (bpp >= 8 ? 256 : 2);
ci->dither_colors = (bpp >= 8 ? 256 : bpp > 1 ? 2 : 0);
} else {
ci->dither_grays = (bpp > 8 ? 256 : 2);
ci->dither_colors = (bpp > 8 ? 256 : bpp > 1 ? 2 : 0);
}
} else {
ci->num_components = (bpp == 1 || bpp == 8 ? 1 : 3);
ci->max_color = (bpp >= 8 ? 255 : bpp > 1 ? 1 : 0);
ci->max_gray = (bpp >= 8 ? 255 : 1);
ci->dither_grays = (bpp >= 8 ? 256 : 2);
ci->dither_colors = (bpp >= 8 ? 256 : bpp > 1 ? 2 : 0);
}
ci->depth = ((bpp > 1) && (bpp < 8) ? 8 : bpp);
return 0;
}
private int
cdj_put_param_bpp(gx_device *pdev, gs_param_list *plist, int new_bpp,
int real_bpp, int ccomps)
{
if (new_bpp == 0 && ccomps == 0)
return gdev_prn_put_params(pdev, plist);
else
{
gx_device_color_info save_info;
int save_bpp;
int code;
save_info = pdev->color_info;
save_bpp = save_info.depth;
#define save_ccomps save_info.num_components
if ( save_bpp == 8 && save_ccomps == 3 && !cprn_device->cmyk)
save_bpp = 3;
code = cdj_set_bpp(pdev, real_bpp, ccomps);
if ( code < 0 ) {
param_signal_error(plist, "BitsPerPixel", code);
param_signal_error(plist, "ProcessColorModel", code);
return code;
}
pdev->color_info.depth = new_bpp;
code = gdev_prn_put_params(pdev, plist);
if ( code < 0 )
{ cdj_set_bpp(pdev, save_bpp, save_ccomps);
return code;
}
cdj_set_bpp(pdev, real_bpp, ccomps);
if ((cdj->color_info.depth != save_bpp ||
(ccomps != 0 && ccomps != save_ccomps))
&& pdev->is_open )
return gs_closedevice(pdev);
return 0;
#undef save_ccomps
}
}
private uint
gdev_prn_rasterwidth(const gx_device_printer *pdev, int pixelcount)
{
ulong raster_width = (ulong)(pdev->width -
pdev->x_pixels_per_inch * (dev_l_margin(pdev) + dev_r_margin(pdev)));
return (pixelcount ?
(uint)raster_width :
(uint)((raster_width * pdev->color_info.depth + 7) >> 3));
}
private const byte*
paramValueToString(const stringParamDescription* params, int value)
{
for (; params->p_name; ++params) {
if (params->p_value == value) {
return (const byte *)params->p_name;
}
}
return (const byte*) 0;
}
private int
paramStringValue(const stringParamDescription* params,
const byte* name, int namelen, int* value)
{
for (; params->p_name; ++params) {
if (strncmp(params->p_name, (char *)name, namelen) == 0 &&
params->p_name[namelen] == 0) {
*value = params->p_value;
return 1;
}
}
return 0;
}
private int
put_param_string(gs_param_list* plist,
const byte* pname, gs_param_string* pstring,
const stringParamDescription* params, int *pvalue, int code)
{
int ncode;
if ((ncode = param_read_string(plist, (char *)pname, pstring)) < 0) {
param_signal_error(plist, (char *)pname, code = ncode);
} else if (ncode == 1) {
pstring->data = 0, pstring->size = 0;
} else {
int value = 0;
if (paramStringValue(params, pstring->data, pstring->size,
&value) == 0) {
param_signal_error(plist, (char *)pname, code = gs_error_rangecheck);
} else {
*pvalue = value;
}
}
return code;
}
private int
get_param_string(gs_param_list* plist,
const byte* pname, gs_param_string* pstring,
const stringParamDescription* params, int pvalue, bool persist, int code)
{
int ncode;
pstring->data = paramValueToString(params, pvalue);
if (pstring->data == (byte*) 0) {
param_signal_error(plist, (char *)pname, ncode = gs_error_unknownerror);
} else {
pstring->size = strlen((char *)pstring->data);
pstring->persistent = persist;
}
if ((ncode = param_write_string(plist, (char *)pname, pstring)) < 0) {
code = ncode;
}
return code;
}
private int
cdj_param_check_bytes(gs_param_list *plist, gs_param_name pname,
const byte *str, uint size, bool is_defined)
{ int code;
gs_param_string new_value;
switch ( code = param_read_string(plist, pname, &new_value) )
{
case 0:
if ( is_defined && new_value.size == size &&
!memcmp((const char *)str, (const char *)new_value.data,
size)
)
break;
code = gs_note_error(gs_error_rangecheck);
goto e;
default:
if ( param_read_null(plist, pname) == 0 )
return 1;
e: param_signal_error(plist, pname, code);
case 1:
;
}
return code;
}
private int
cdj_param_check_float(gs_param_list *plist, gs_param_name pname, floatp fval,
bool is_defined)
{ int code;
float new_value;
switch ( code = param_read_float(plist, pname, &new_value) )
{
case 0:
if ( is_defined && new_value == fval)
break;
code = gs_note_error(gs_error_rangecheck);
goto e;
default:
if ( param_read_null(plist, pname) == 0 )
return 1;
e: param_signal_error(plist, pname, code);
case 1:
;
}
return code;
}
#define FSerror(Val,Erow,Ecol) (Val + Erow + ((7 * Ecol)>>4))
#define FSdecide(Error,Threshold,Spotsize,Pixel,Bit) \
if(Error > Threshold) {\
Pixel |= Bit;\
Error -= Spotsize;\
}
#define FSdiffuse(Error,Erow,Ecol,Eprev)\
Eprev += (3 * Error + 8)>>4;\
Erow = (5 * Error + Ecol + 8)>>4;\
Ecol = Error;
#define DIRECTION direction[0]
#define CMYK_THRESHOLD(I) threshold[I]
#define SPOTSIZE(I) spotsize[I]
#define EMIN(I) emin[I]
#define EMAX(I) emax[I]
#define NPIXEL (plane_size * 8)
#define IDX_C 1
#define IDX_M 2
#define IDX_Y 3
#define IDX_K 0
#define ODX_C 2
#define ODX_M 1
#define ODX_Y 0
#define ODX_K 3
private int
bjc_fscmyk(byte** inplanes, byte* outplanes[4][4], int** errplanes,
int plane_size, int scan) {
byte* err = (byte*) errplanes[0];
if(scan < 0) {
int p,i,v;
int *direction,*threshold,*spotsize,*emin,*emax;
int *errv,*errc;
direction = (int *) err;
threshold = direction + 4;
spotsize = threshold + 4;
emin = spotsize + 4;
emax = emin + 4;
errc = emax + 4;
errv = errc + 2*4;
DIRECTION = -1;
for(i = 0; i < 4; ++i) {
int j;
float maxv = 1.0;
CMYK_THRESHOLD(i) = (int)(127.0 / maxv + 0.5);
SPOTSIZE(i) = ((int) CMYK_THRESHOLD(i)<<1)+1;
j = CMYK_THRESHOLD(i);
errc[3] = 0;
FSdiffuse(CMYK_THRESHOLD(i),errv[0],errc[0],errv[-4]);
FSdiffuse(CMYK_THRESHOLD(i),errv[0],errc[0],errv[-4]);
EMAX(i) = errv[0];
errc[0] = 0;
FSdiffuse((-CMYK_THRESHOLD(i)),errv[0],errc[0],errv[-4]);
FSdiffuse((-CMYK_THRESHOLD(i)),errv[0],errc[0],errv[-4]);
EMIN(i) = errv[0];
}
#ifdef CDJ_DEBUG_FS
for(i = 0; i < 4; ++i) errprintf(
"CMYK_THRESHOLD(%d)=%5d, spotsize(%d)=%5d, emin(%d)=%5d, emax(%d)=%5d\n",
i,CMYK_THRESHOLD(i),i,SPOTSIZE(i),i,EMIN(i),i,EMAX(i));
#endif
for(i = 0; i < 4; ++i) errc[i] = 0;
for(p = 0; p < NPIXEL; ++p) {
for(i = 0; i < 4; ++i) {
if (0) v = 0;
else v = (rand() % SPOTSIZE(i)) - CMYK_THRESHOLD(i);
FSdiffuse(v,errv[i],errc[i],errv[i-4]);
}
errv += i;
}
} else {
int w,p,dir,thedir;
byte *out[4],pixel[4],bit;
int *direction = (int *) err;
int *threshold = direction + 4;
int *spotsize = threshold + 4;
int *emin = spotsize + 4;
int *emax = emin + 4;
int *errc = emax + 4;
int *errv = errc + 2*4;
int kerr,cerr,merr,yerr;
byte* in;
if (0) {
cerr = merr = yerr = kerr = 0;
} else {
cerr = errc[0];
merr = errc[1];
yerr = errc[2];
kerr = errc[3];
}
out[0] = outplanes[scan + 2][ODX_C];
out[1] = outplanes[scan + 2][ODX_M];
out[2] = outplanes[scan + 2][ODX_Y];
out[3] = outplanes[scan + 2][ODX_K];
pixel[0] = pixel[1] = pixel[2] = pixel[3] = 0;
if(DIRECTION < 0) {
w = NPIXEL;
in = inplanes[2] + 4 * (NPIXEL - 1);
errv += (w-1)<<2;
dir = -4;
thedir = -1;
for (p = 0; p < 4; ++p) {
out[p] += plane_size - 1;
}
} else {
w = 1;
in = inplanes[3] - 4 * NPIXEL;
dir = 4;
thedir = 1;
for (p = 0; p < 4; ++p) {
out[p] -= plane_size;
}
}
if (1) DIRECTION = -DIRECTION;
bit = 0x80>>((w-1) & 7);
w = (w+7)>>3;
for(p = NPIXEL; p; --p) {
int cmy = in[IDX_C] | in[IDX_M] | in[IDX_Y];
int kv = FSerror(in[IDX_K],errv[3],kerr);
int cv;
FSdecide(kv,CMYK_THRESHOLD(3),SPOTSIZE(3),pixel[3],bit);
if(cmy) {
if(pixel[3] & bit) {
FSdiffuse(kv,errv[3],kerr,errv[3-dir]);
cv = FSerror(in[IDX_C],errv[0],cerr);
cv -= SPOTSIZE(0);
if ((cv+CMYK_THRESHOLD(0)) < 0) cv = -CMYK_THRESHOLD(0);
FSdiffuse(cv,errv[0],cerr,errv[0-dir]);
cv = FSerror(in[IDX_M],errv[1],merr);
cv -= SPOTSIZE(1);
if ((cv+CMYK_THRESHOLD(1)) < 0) cv = -CMYK_THRESHOLD(1);
FSdiffuse(cv,errv[1],merr,errv[1-dir]);
cv = FSerror(in[IDX_Y],errv[2],yerr);
cv -= SPOTSIZE(2);
if ((cv+CMYK_THRESHOLD(2)) < 0) cv = -CMYK_THRESHOLD(2);
FSdiffuse(cv,errv[2],yerr,errv[2-dir]);
} else {
cv = FSerror(in[IDX_C],errv[0],cerr);
FSdecide(cv,CMYK_THRESHOLD(0),SPOTSIZE(0),pixel[0],bit);
FSdiffuse(cv,errv[0],cerr,errv[0-dir]);
cv = FSerror(in[IDX_M],errv[1],merr);
FSdecide(cv,CMYK_THRESHOLD(1),SPOTSIZE(1),pixel[1],bit);
FSdiffuse(cv,errv[1],merr,errv[1-dir]);
cv = FSerror(in[IDX_Y],errv[2],yerr);
FSdecide(cv,CMYK_THRESHOLD(2),SPOTSIZE(2),pixel[2],bit);
FSdiffuse(cv,errv[2],yerr,errv[2-dir]);
if(pixel[0] & pixel[1] & pixel[2] & bit) {
pixel[0] &= ~bit;
pixel[1] &= ~bit;
pixel[2] &= ~bit;
pixel[3] |= bit;
kv -= SPOTSIZE(3);
if ((kv+CMYK_THRESHOLD(3)) < 0) kv = -CMYK_THRESHOLD(0);
FSdiffuse(kv,errv[3],kerr,errv[3-dir]);
}
}
} else {
FSdiffuse(kv,errv[3],kerr,errv[3-dir]);
if( errv[0] > EMAX(0)) errv[0] = EMAX(0);
else if(errv[0] < EMIN(0)) errv[0] = EMIN(0);
if( errv[1] > EMAX(1)) errv[1] = EMAX(1);
else if(errv[1] < EMIN(1)) errv[1] = EMIN(1);
if( errv[2] > EMAX(2)) errv[2] = EMAX(2);
else if(errv[2] < EMIN(2)) errv[2] = EMIN(2);
}
bit = dir > 0 ? (bit>>1) : (bit<<1);
if(bit == 0) {
*out[0] = pixel[0];
*out[1] = pixel[1];
*out[2] = pixel[2];
*out[3] = pixel[3];
out[0] += thedir; out[1] += thedir;
out[2] += thedir; out[3] += thedir;
pixel[0] = pixel[1] = pixel[2] = pixel[3] = 0;
if(dir > 0) bit = 0x80;
else bit = 0x01;
w += dir>>2;
}
in += dir;
errv += dir;
}
if (1) {
cerr = errc[0] = cerr;
merr = errc[1] = merr;
yerr = errc[2] = yerr;
kerr = errc[3] = kerr;
}
}
return 0;
}