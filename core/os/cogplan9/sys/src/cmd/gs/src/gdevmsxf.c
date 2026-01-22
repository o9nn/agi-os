#include "ctype_.h"
#include "math_.h"
#include "memory_.h"
#include "string_.h"
#include "gdevmswn.h"
#include "gsutil.h"
#include "gxxfont.h"
#include "gsstruct.h"
extern const byte gs_map_std_to_iso[256];
private xfont_proc_lookup_font(win_lookup_font);
private xfont_proc_char_xglyph(win_char_xglyph);
private xfont_proc_char_metrics(win_char_metrics);
private xfont_proc_render_char(win_render_char);
private xfont_proc_release(win_release);
private const gx_xfont_procs win_xfont_procs =
{
win_lookup_font,
win_char_xglyph,
win_char_metrics,
win_render_char,
win_release
};
const gx_xfont_procs *
win_get_xfont_procs(gx_device * dev)
{
return &win_xfont_procs;
}
typedef struct win_xfont_s win_xfont;
struct win_xfont_s {
gx_xfont_common common;
LOGFONT lf;
TEXTMETRIC tm;
HFONT hFont;
gx_device_win *dev;
int invert_y;
int y_offset;
};
gs_private_st_dev_ptrs1(st_win_xfont, win_xfont, "win_xfont",
win_xfont_enum_ptrs, win_xfont_reloc_ptrs, dev);
#define wxf ((win_xfont *)xf)
private HDC near win_get_dc(gx_device_win *);
private void near win_release_dc(gx_device_win *, HDC);
private int win_select_font(HDC, win_xfont *);
private const byte far_data gs_map_symbol_to_oem[256] =
{
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
32, 33, 0, 35, 0, 37, 38, 0, 40, 41, 0, 43, 44, 0, 46, 47,
48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
0, 0, 0, 0, 0, 0, 231, 225, 0, 0, 0, 0, 0, 0, 0, 0,
226, 232, 0, 227, 0, 0, 0, 233, 0, 0, 0, 91, 0, 93, 0, 95,
0, 223, 224, 0, 234, 0, 236, 0, 0, 0, 0, 0, 0, 229, 0, 0,
0, 0, 0, 228, 230, 0, 0, 0, 0, 0, 0, 123, 124, 125, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 242, 0, 235, 0, 5, 4, 3, 6, 29, 27, 24, 26, 25,
247, 240, 0, 241, 0, 0, 0, 7, 245, 0, 239, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 238, 0, 0, 0, 0, 0, 0, 237, 0,
0, 0, 0, 0, 0, 0, 250, 248, 169, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 243, 0, 244, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};
private const byte far_data gs_map_iso_to_oem[256] =
{
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 0, 46, 47,
48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
32, 172, 154, 155, 0, 156, 0, 21, 0, 0, 165, 173, 169, 45, 0, 0,
247, 240, 252, 0, 0, 229, 20, 249, 0, 0, 166, 174, 171, 170, 0, 167,
0, 0, 0, 0, 141, 142, 145, 128, 0, 143, 0, 0, 0, 0, 0, 0,
0, 164, 0, 0, 0, 0, 152, 0, 0, 0, 0, 0, 153, 0, 0, 0,
133, 159, 131, 0, 132, 134, 144, 135, 138, 130, 136, 137, 140, 160, 0, 139,
0, 163, 148, 161, 146, 0, 147, 245, 0, 150, 162, 149, 129, 0, 0, 151
};
typedef struct font_entry_s {
const char *key;
const char *value;
uint pitchAndFamily;
} font_entry;
private const font_entry font_names[] =
{
{"Courier", "Courier New", FIXED_PITCH | FF_MODERN},
{"Helvetica", "Arial", VARIABLE_PITCH | FF_SWISS},
{"Helvetica", "Helv", VARIABLE_PITCH | FF_SWISS},
{"Times", "Times New Roman", VARIABLE_PITCH | FF_ROMAN},
{"Times", "Tms Rmn", VARIABLE_PITCH | FF_ROMAN}
};
private int map_logical_font(HDC, win_xfont *);
gx_xfont *
win_lookup_font(gx_device * dev, const byte * fname, uint len,
int encoding_index, const gs_uid * puid, const gs_matrix * pmat,
gs_memory_t * mem)
{
win_xfont f;
win_xfont *wf;
uint name_len = min(len, LF_FACESIZE - 1);
const font_entry *pfe;
HDC hdc;
if (pmat->xy != 0 || pmat->yx != 0 || pmat->xx <= 0 ||
fabs(fabs(pmat->yy) - pmat->xx) > 0.00002
)
return NULL;
f.lf.lfHeight = (long)(pmat->xx * 1000);
if (f.lf.lfHeight < 6 || f.lf.lfHeight >= 36)
return NULL;
f.lf.lfWidth = 0;
f.lf.lfEscapement = 0;
f.lf.lfOrientation = 0;
f.lf.lfWeight =
(string_match(fname, len, "*Bold*", 6, NULL) ?
FW_BOLD : FW_REGULAR);
f.lf.lfItalic =
string_match(fname, len, "*Italic*", 8, NULL) ||
string_match(fname, len, "*Oblique*", 9, NULL);
f.lf.lfUnderline = 0;
f.lf.lfStrikeOut = 0;
f.lf.lfCharSet =
(encoding_index == 2 ? SYMBOL_CHARSET : ANSI_CHARSET);
f.lf.lfOutPrecision = OUT_CHARACTER_PRECIS;
f.lf.lfClipPrecision = CLIP_STROKE_PRECIS;
f.lf.lfQuality = PROOF_QUALITY;
f.hFont = 0;
f.invert_y = pmat->yy >= 0;
hdc = win_get_dc(wdev);
if (hdc == NULL)
return NULL;
for (pfe = font_names; pfe != &font_names[countof(font_names)]; pfe++)
if (!strncmp(pfe->key, fname, strlen(pfe->key))) {
strcpy(f.lf.lfFaceName, pfe->value);
f.lf.lfPitchAndFamily = pfe->pitchAndFamily;
if (map_logical_font(hdc, &f))
break;
}
if (f.hFont == 0) {
uint len;
memcpy(f.lf.lfFaceName, fname, name_len);
for (len = 0; len < name_len; len++)
if (!isalnum(fname[len]))
break;
f.lf.lfFaceName[len] = 0;
f.lf.lfPitchAndFamily = 0;
if (!map_logical_font(hdc, &f)) {
win_release_dc(wdev, hdc);
return NULL;
}
}
GetTextMetrics(hdc, &f.tm);
win_release_dc(wdev, hdc);
f.y_offset = (!f.invert_y ? f.tm.tmAscent : f.tm.tmDescent);
wf = gs_alloc_struct(mem, win_xfont, &st_win_xfont, "win_lookup_font");
if (wf == 0) {
DeleteObject(f.hFont);
return NULL;
}
f.common.procs = &win_xfont_procs;
f.dev = wdev;
*wf = f;
return (gx_xfont *) wf;
}
private int
map_logical_font(HDC hdc, win_xfont * xf)
{
char szFaceName[LF_FACESIZE];
xf->hFont = CreateFontIndirect(&xf->lf);
if (xf->hFont == 0)
return 0;
SelectObject(hdc, xf->hFont);
GetTextFace(hdc, sizeof(szFaceName), szFaceName);
if (!strncmp(xf->lf.lfFaceName, szFaceName, strlen(xf->lf.lfFaceName)))
return 1;
DeleteObject(xf->hFont);
xf->hFont = 0;
return 0;
}
gx_xglyph
win_char_xglyph(gx_xfont * xf, gs_char chr, int encoding_index,
gs_glyph glyph, const gs_const_string *glyph_name)
{
if (chr == gs_no_char)
return gx_no_xglyph;
if (encoding_index == 0) {
chr = gs_map_std_to_iso[chr];
encoding_index = 1;
}
if (wxf->hFont == NULL) {
HDC hdc = win_get_dc(wxf->dev);
int code;
if (hdc == NULL)
return gx_no_xglyph;
code = win_select_font(hdc, wxf);
win_release_dc(wxf->dev, hdc);
if (code < 0)
return gx_no_xglyph;
}
switch (wxf->tm.tmCharSet) {
case ANSI_CHARSET:
if (encoding_index == 1 && (chr < 0x7f || chr > 0x9f ||
chr == 0x91 || chr == 0x92)
)
break;
return gx_no_xglyph;
case OEM_CHARSET:
switch (encoding_index) {
case 1:
chr = gs_map_iso_to_oem[chr];
break;
case 2:
chr = gs_map_symbol_to_oem[chr];
break;
default:
return gx_no_xglyph;
}
break;
default:
return gx_no_xglyph;
}
return (chr != 0 && chr >= wxf->tm.tmFirstChar &&
chr <= wxf->tm.tmLastChar ?
(gx_xglyph) chr : gx_no_xglyph);
}
int
win_char_metrics(gx_xfont * xf, gx_xglyph xg, int wmode,
gs_point * pwidth, gs_int_rect * pbbox)
{
int code;
HDC hdc;
char chr = (char)xg;
if (wmode != 0)
return gs_error_undefined;
hdc = win_get_dc(wxf->dev);
if (hdc == NULL)
return gs_error_limitcheck;
if ((code = win_select_font(hdc, wxf)) < 0) {
win_release_dc(wxf->dev, hdc);
return code;
}
#ifdef __WIN32__
{
SIZE sz;
GetTextExtentPoint(hdc, &chr, 1, &sz);
pwidth->x = sz.cx;
}
#else
{
DWORD extent;
extent = GetTextExtent(hdc, &chr, 1);
pwidth->x = LOWORD(extent);
}
#endif
win_release_dc(wxf->dev, hdc);
pwidth->y = 0;
pbbox->p.x = 0;
pbbox->q.x = (int)pwidth->x;
if (wxf->invert_y) {
pbbox->p.y = -wxf->tm.tmDescent;
pbbox->q.y = wxf->tm.tmAscent;
} else {
pbbox->p.y = -wxf->tm.tmAscent;
pbbox->q.y = wxf->tm.tmDescent;
}
return 0;
}
int
win_render_char(gx_xfont * xf, gx_xglyph xg, gx_device * dev,
int xo, int yo, gx_color_index color, int required)
{
char chr = (char)xg;
int code;
#ifdef NOTUSED
if (dev->dname == gs_mswin_device.dname &&
wdev->hdctext != NULL && !wxf->invert_y
) {
HDC hdc = wdev->hdctext;
PALETTEENTRY *pal = &wdev->limgpalette->palPalEntry[color];
if ((code = win_select_font(hdc, wxf)) < 0)
return code;
SetTextColor(hdc, RGB(pal->peRed, pal->peGreen, pal->peBlue));
SetBkMode(hdc, TRANSPARENT);
TextOut(hdc, xo, yo - wxf->y_offset, &chr, 1);
} else
#endif
if (!required)
code = -1;
else {
gs_point wxy;
gs_int_rect bbox;
int w, h, wbm, raster;
gx_device_win *fdev = wxf->dev;
HBITMAP hbm;
byte *bits;
code = (*xf->common.procs->char_metrics) (xf, xg, 0,
&wxy, &bbox);
if (code < 0)
return code;
w = bbox.q.x - bbox.p.x;
h = bbox.q.y - bbox.p.y;
wbm = ROUND_UP(w, align_bitmap_mod * 8);
raster = wbm >> 3;
bits = gs_malloc(dev->memory, h, raster, "win_render_char");
if (bits == 0)
return gs_error_limitcheck;
hbm = CreateBitmap(wbm, h, 1, 1, NULL);
if (hbm == NULL) {
code = gs_error_limitcheck;
} else {
HDC hdcwin = win_get_dc(fdev);
HDC hdcbit = CreateCompatibleDC(hdcwin);
dev_proc_copy_mono((*copy_mono)) =
dev_proc(dev, copy_mono);
int y = yo - wxf->y_offset;
SetMapMode(hdcbit, GetMapMode(hdcwin));
win_select_font(hdcbit, wxf);
SelectObject(hdcbit, hbm);
PatBlt(hdcbit, 0, 0, wbm, h, rop_write_0s);
SetTextColor(hdcbit, 0xffffffL);
SetBkMode(hdcbit, TRANSPARENT);
TextOut(hdcbit, 0, 0, &chr, 1);
GetBitmapBits(hbm, (DWORD) raster * h, bits);
DeleteDC(hdcbit);
win_release_dc(fdev, hdcwin);
DeleteObject(hbm);
if (!wxf->invert_y)
code = (*copy_mono) (dev, bits, 0,
raster, gx_no_bitmap_id,
xo, y, w, h,
gx_no_color_index, color);
else {
int i;
y += h - 1;
for (i = 0; i < h; i++)
(*copy_mono) (dev, bits + i * raster,
0, raster, gx_no_bitmap_id,
xo, y - i, w, 1,
gx_no_color_index, color);
}
}
gs_free(dev->memory, bits, h, raster, "win_render_char");
}
return (code < 0 ? code : 0);
}
private int
win_release(gx_xfont * xf, gs_memory_t * mem)
{
if (wxf->hFont) {
DeleteObject(wxf->hFont);
wxf->hFont = 0;
}
if (mem != NULL)
gs_free_object(mem, xf, "win_release");
return 0;
}
#undef wdev
#undef wxf
private HDC near
win_get_dc(gx_device_win * wdev)
{
return GetDC(HWND_DESKTOP);
}
private void near
win_release_dc(gx_device_win * wdev, HDC hdc)
{
ReleaseDC(HWND_DESKTOP, hdc);
}
private int
win_select_font(HDC hdc, win_xfont * wxf)
{
HFONT hFont = wxf->hFont;
if (hFont == NULL) {
wxf->hFont = CreateFontIndirect(&wxf->lf);
if (wxf->hFont == NULL)
return gs_error_limitcheck;
}
SelectObject(hdc, wxf->hFont);
return 0;
}