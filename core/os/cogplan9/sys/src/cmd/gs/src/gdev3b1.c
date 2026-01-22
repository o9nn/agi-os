#include "gx.h"
#include "gxdevice.h"
#include "gserrors.h"
#include <errno.h>
#include <sys/window.h>
#include <sys/termio.h>
typedef struct gx_device_att3b1_s {
gx_device_common;
int fd;
uchar *screen;
ushort line_size;
ulong screen_size;
int page_num;
#ifdef ATT3B1_PERF
char *no_output, *no_fill, *no_copy;
#endif
} gx_device_att3b1;
#define att3b1dev ((gx_device_att3b1 *)dev)
#define XDPI	100
#define YDPI	72
#define XSIZE (8.5 * XDPI)
#define YSIZE (11 * YDPI)
static const ushort masks[] = { 0,
0x0001, 0x0003, 0x0007, 0x000f,
0x001f, 0x003f, 0x007f, 0x00ff,
0x01ff, 0x03ff, 0x07ff, 0x0fff,
0x1fff, 0x3fff, 0x7fff, 0xffff,
};
static uchar reverse_bits[256] = {
0, 128, 64, 192, 32, 160, 96, 224, 16, 144, 80, 208, 48, 176, 112, 240,
8, 136, 72, 200, 40, 168, 104, 232, 24, 152, 88, 216, 56, 184, 120, 248,
4, 132, 68, 196, 36, 164, 100, 228, 20, 148, 84, 212, 52, 180, 116, 244,
12, 140, 76, 204, 44, 172, 108, 236, 28, 156, 92, 220, 60, 188, 124, 252,
2, 130, 66, 194, 34, 162, 98, 226, 18, 146, 82, 210, 50, 178, 114, 242,
10, 138, 74, 202, 42, 170, 106, 234, 26, 154, 90, 218, 58, 186, 122, 250,
6, 134, 70, 198, 38, 166, 102, 230, 22, 150, 86, 214, 54, 182, 118, 246,
14, 142, 78, 206, 46, 174, 110, 238, 30, 158, 94, 222, 62, 190, 126, 254,
1, 129, 65, 193, 33, 161, 97, 225, 17, 145, 81, 209, 49, 177, 113, 241,
9, 137, 73, 201, 41, 169, 105, 233, 25, 153, 89, 217, 57, 185, 121, 249,
5, 133, 69, 197, 37, 165, 101, 229, 21, 149, 85, 213, 53, 181, 117, 245,
13, 141, 77, 205, 45, 173, 109, 237, 29, 157, 93, 221, 61, 189, 125, 253,
3, 131, 67, 195, 35, 163, 99, 227, 19, 147, 83, 211, 51, 179, 115, 243,
11, 139, 75, 203, 43, 171, 107, 235, 27, 155, 91, 219, 59, 187, 123, 251,
7, 135, 71, 199, 39, 167, 103, 231, 23, 151, 87, 215, 55, 183, 119, 247,
15, 143, 79, 207, 47, 175, 111, 239, 31, 159, 95, 223, 63, 191, 127, 255
};
dev_proc_open_device(att3b1_open);
dev_proc_close_device(att3b1_close);
dev_proc_fill_rectangle(att3b1_fill_rectangle);
dev_proc_copy_mono(att3b1_copy_mono);
dev_proc_output_page(att3b1_output_page);
private gx_device_procs att3b1_procs = {
att3b1_open,
gx_default_get_initial_matrix,
gx_default_sync_output,
att3b1_output_page,
att3b1_close,
gx_default_map_rgb_color,
gx_default_map_color_rgb,
att3b1_fill_rectangle,
gx_default_tile_rectangle,
att3b1_copy_mono,
gx_default_copy_color,
gx_default_draw_line,
gx_default_get_bits
};
gx_device_att3b1 gs_att3b1_device = {
std_device_std_body(gx_device_att3b1, &att3b1_procs, "att3b1",
XSIZE, YSIZE, XDPI, YDPI),
{ 0 },
-1, 0, 0,
0, 0,
#ifdef ATT3B1_PERF
0, 0, 0,
#endif
};
int
att3b1_open(gx_device *dev)
{
struct uwdata uw;
#ifdef ATT3B1_PERF
char *getenv(const char *);
#endif
if (att3b1dev->fd >= 0) {
close(att3b1dev->fd);
att3b1dev->fd = -1;
}
if (att3b1dev->screen != NULL) {
gs_free(dev->memory, (char *)att3b1dev->screen,
att3b1dev->screen_size, 1, "att3b1_open");
att3b1dev->screen = 0;
att3b1dev->screen_size = 0;
}
att3b1dev->fd = open("/dev/tty", 2);
if (att3b1dev->fd < 0) {
lprintf1("att3b1_open: open /dev/tty failed [%d]\n", errno);
return_error(gs_error_ioerror);
}
if (ioctl(att3b1dev->fd, WIOCGETD, &uw) < 0) {
lprintf1("att3b1_open: can not obtain window data [%d]\n", errno);
lprintf("att3b1_open: the att3b1 device requires a console window\n");
att3b1_close(dev);
return_error(gs_error_ioerror);
}
att3b1dev->line_size = ((att3b1dev->width + 15) / 16) * 2;
att3b1dev->screen_size = att3b1dev->line_size * att3b1dev->height;
att3b1dev->screen =
(uchar *)gs_malloc(dev->memory, att3b1dev->screen_size, 1, "att3b1_open");
if (att3b1dev->screen == NULL) {
att3b1_close(dev);
return_error(gs_error_VMerror);
}
att3b1dev->page_num = 1;
#ifdef ATT3B1_PERF
att3b1dev->no_output = getenv("GS_NOOUTPUT");
att3b1dev->no_fill = getenv("GS_NOFILL");
att3b1dev->no_copy = getenv("GS_NOCOPY");
#endif
return 0;
}
int
att3b1_close(gx_device *dev)
{
if (att3b1dev->fd >= 0) {
close(att3b1dev->fd);
att3b1dev->fd = -1;
}
if (att3b1dev->screen != NULL) {
gs_free(dev->memory, (char *)att3b1dev->screen,
att3b1dev->screen_size, 1, "att3b1_close");
att3b1dev->screen = 0;
att3b1dev->screen_size = 0;
}
return 0;
}
int
att3b1_fill_rectangle(gx_device *dev, int x, int y, int w, int h,
gx_color_index colour)
{
uint o, b, wl, wr, w2;
ushort *p, *q, maskl, maskr;
#ifdef ATT3B1_PERF
if (att3b1dev->no_fill) return 0;
#endif
fit_fill(dev, x, y, w, h);
p = (ushort *)&att3b1dev->screen[(ushort)y*att3b1dev->line_size] +
(uint)x/16;
o = (uint)x % 16;
b = 16 - o;
wl = ((uint)w < b) ? (uint)w : b;
maskl = masks[wl] << o;
w -= wl;
wr = (uint)w % 16;
maskr = masks[wr];
if (colour == 0) {
maskl = ~maskl;
maskr = ~maskr;
while (h-- > 0) {
q = p;
w2 = w;
*q++ &= maskl;
while (w2 >= 16) {
*q++ = 0;
w2 -= 16;
}
*q &= maskr;
p += (att3b1dev->line_size / 2);
}
}
else {
while (h-- > 0) {
q = p;
w2 = w;
*q++ |= maskl;
while (w2 >= 16) {
*q++ = 0xffff;
w2 -= 16;
}
*q |= maskr;
p += (att3b1dev->line_size / 2);
}
}
return 0;
}
#ifdef __GNUC__
#define rotate(value, count) \
asm("ror%.l	%2,%0" : "=d" (value) : "0" (value), "d" (count))
#else
#define rotate(value, count) \
value = (value >> count) | (value << (32-count))
#endif
int
att3b1_copy_mono(gx_device *dev, const uchar *data,
int data_x, int raster, gx_bitmap_id id,
int x, int y, int width, int height,
gx_color_index colour0, gx_color_index colour1)
{
const ushort *src_p, *src_q;
ushort *dst_p, *dst_q;
ulong bits, mask, *p;
uint src_o, src_b, dst_o, dst_b, op;
uint w1, w2;
#ifdef ATT3B1_PERF
if (att3b1dev->no_copy) return 0;
#endif
if (colour1 == colour0)
return att3b1_fill_rectangle(dev, x, y, width, height, colour0);
fit_copy(dev, data, data_x, raster, id, x, y, width, height);
src_p = ((const ushort *)data) + (uint)data_x/16;
src_o = (uint)data_x % 16;
src_b = 16 - src_o;
dst_p = (ushort *)&att3b1dev->screen[(ushort)y*att3b1dev->line_size] +
(uint)x/16;
dst_o = (uint)x % 16;
dst_b = 16 - dst_o;
op = (int)colour0 * 3 + (int)colour1 + 4;
while (height-- > 0) {
w2 = width;
src_q = src_p;
dst_q = dst_p;
while (w2 > 0) {
w1 = (w2 < 16) ? w2 : 16;
mask = masks[w1];
if (src_o == 0)
bits = *src_q++;
else {
bits = *((ulong *)src_q) >> src_b;
bits &= 0xffff;
src_q++;
}
if (w1 <= 8)
bits = reverse_bits[bits>>8];
else
bits = (reverse_bits[bits&0xff] << 8) | reverse_bits[bits>>8];
p = (ulong *)dst_q;
switch(op) {
case 1:
bits = ~(bits & mask);
rotate(bits,dst_b);
*p &= bits;
break;
case 2:
bits = bits & mask;
rotate(bits,dst_b);
*p |= bits;
break;
case 3:
bits = bits | ~mask;
rotate(bits,dst_b);
*p &= bits;
break;
case 5:
rotate(bits,dst_b);
rotate(mask,dst_b);
*p = (*p & ~mask) | (bits & mask);
break;
case 6:
bits = ~bits & mask;
rotate(bits,dst_b);
*p |= bits;
break;
case 7:
rotate(bits,dst_b);
rotate(mask,dst_b);
*p = (*p & ~mask) | (~bits & mask);
break;
}
dst_q++;
w2 -= w1;
}
src_p += (raster / 2);
dst_p += (att3b1dev->line_size / 2);
}
return 0;
}
static int getKeyboard(gx_device *);
const char *help_msg[] = {
"h, j, k, l, UP, DOWN, LEFT, RIGHT  move the page (0.25\" h, 0.5\" v)",
"H, J, K, L, BEG, END               move to far edge of the page",
"^U, ^D, ROLL UP, ROLL DOWN	        scroll up or down (1/2 screen height)",
"^F, ^B, PAGE UP, PAGE DOWN	        scroll up or down (full screen height)",
"c, C                               centre page horizontally, vertically",
"<, >, ^, _                         fine movements (single pixel)",
"^L, ^R, r, HOME                    move to default position",
"=, MARK                            make current position the default",
"I                                  invert the image (black <-> white)",
"q, x, ^C, EXIT, CANCL, n, f, NEXT,",
"    SPACE, RETURN, ENTER           end the page",
"?, HELP                            help screen",
};
static void
do_help(gx_device *dev)
{
int i;
struct utdata ut;
write(att3b1dev->fd, "\033[2J\033[H", 7);
for (i=0; i < sizeof(help_msg)/sizeof(help_msg[0]); ++i) {
write(att3b1dev->fd, help_msg[i], strlen(help_msg[i]));
write(att3b1dev->fd, "\n", 1);
}
ut.ut_num = WTXTSLK1;
strcpy(ut.ut_text, "Press any key to continue");
ioctl(att3b1dev->fd, WIOCSETTEXT, &ut);
i = getKeyboard(dev);
write(att3b1dev->fd, "\033[2J\033[99;1H", 11);
}
private int
att3b1_do_output_page(gx_device *dev, int num_copies, int flush)
{
struct urdata ur;
struct utdata ut, ut_orig;
struct uwdata uw;
int uflags;
struct termio old, new;
int xorigin, yorigin;
static int def_xorigin = 0, def_yorigin = 0;
int screen_width, screen_height;
int inverted = 0;
int error = 0;
int ch;
ushort *p;
ushort save_image[WINWIDTH * WINHEIGHT / 16];
#ifdef ATT3B1_PERF
if (att3b1dev->no_output) return 0;
#endif
if (ioctl(att3b1dev->fd, WIOCGETD, &uw) < 0) {
lprintf1("att3b1_output_page: window WIOCGETD ioctl failed [%d]\n",
errno);
att3b1_close(dev);
return_error(gs_error_ioerror);
}
write(att3b1dev->fd, "\a\033[=1C", 6);
uflags = uw.uw_uflags;
if (!(uflags & NBORDER)) {
uw.uw_uflags = BORDHSCROLL | BORDVSCROLL | BORDHELP | BORDCANCEL;
ioctl(att3b1dev->fd, WIOCSETD, &uw);
}
ut_orig.ut_num = WTXTSLK1;
ioctl(att3b1dev->fd, WIOCGETTEXT, &ut_orig);
memset(save_image, '\0', sizeof(save_image));
ur.ur_srcbase = 0;
ur.ur_srcwidth = 0;
ur.ur_srcx = 0;
ur.ur_srcy = 0;
ur.ur_dstbase = save_image;
ur.ur_dstwidth = WINWIDTH / 8;
ur.ur_dstx = 0;
ur.ur_dsty = 0;
ur.ur_width = uw.uw_width;
ur.ur_height = uw.uw_height;
ur.ur_srcop = SRCSRC;
ur.ur_dstop = DSTSRC;
ur.ur_pattern = 0;
ioctl(att3b1dev->fd, WIOCRASTOP, &ur);
ioctl(att3b1dev->fd, TCGETA, &old);
new = old;
new.c_lflag &= ~(ISIG | ICANON | ECHO | ECHOE | ECHOK | ECHONL);
new.c_cc[VMIN] = 1;
ioctl(att3b1dev->fd, TCSETAF, &new);
screen_width = (uw.uw_width < att3b1dev->width) ? uw.uw_width
: att3b1dev->width;
screen_height = (uw.uw_height < att3b1dev->height) ? uw.uw_height
: att3b1dev->height;
write(att3b1dev->fd, "\033[2J", 4);
ur.ur_srcwidth = att3b1dev->line_size;
ur.ur_width = screen_width;
ur.ur_height = screen_height;
ur.ur_dstbase = 0;
ur.ur_dstwidth = 0;
xorigin = def_xorigin;
yorigin = def_yorigin;
while (1) {
ur.ur_srcbase = (ushort *)att3b1dev->screen + (xorigin >> 4);
ur.ur_srcx = xorigin & 15;
ur.ur_srcy = yorigin;
if (ioctl(att3b1dev->fd, WIOCRASTOP, &ur) < 0) {
lprintf1(
"att3b1_output_page: window WIOCRASTOP ioctl failed [%d]\n",
errno);
error = gs_error_ioerror;
}
ut.ut_num = WTXTSLK1;
sprintf(ut.ut_text,
"%s %d, top right (%d,%d), size (%d,%d), press '?' for help.",
flush ? "Showpage" : "Copypage", att3b1dev->page_num, xorigin, yorigin,
att3b1dev->width, att3b1dev->height);
ioctl(att3b1dev->fd, WIOCSETTEXT, &ut);
ch = error ? 'q' : getKeyboard(dev);
switch(ch) {
case 'h':
xorigin -= ((uint)(int)att3b1dev->x_pixels_per_inch+3)/4;
break;
case 'k':
yorigin -= ((uint)(int)att3b1dev->y_pixels_per_inch+1)/2;
break;
case 'l':
xorigin += ((uint)(int)att3b1dev->x_pixels_per_inch+3)/4;
break;
case 'j':
yorigin += ((uint)(int)att3b1dev->y_pixels_per_inch+1)/2;
break;
case 'H':
xorigin = 0;
break;
case 'K':
yorigin = 0;
break;
case 'L':
xorigin = att3b1dev->width - screen_width;
break;
case 'J':
yorigin = att3b1dev->height - screen_height;
break;
case '<':
xorigin -= 1;
break;
case '>':
xorigin += 1;
break;
case '^':
yorigin -= 1;
break;
case '_':
yorigin += 1;
break;
case '\025':
yorigin -= screen_height/2;
break;
case '\004':
yorigin += screen_height/2;
break;
case '\002':
yorigin -= screen_height;
break;
case '\006':
yorigin += screen_height;
break;
case '\f':
case 'r' :
case '\022':
xorigin = def_xorigin;
yorigin = def_yorigin;
break;
case 'c':
xorigin = (att3b1dev->width - screen_width) / 2;
break;
case 'C':
yorigin = (att3b1dev->height - screen_height) / 2;
break;
case '=':
def_xorigin = xorigin;
def_yorigin = yorigin;
break;
case 'I':
for (p = (ushort *)att3b1dev->screen;
p < (ushort *)&att3b1dev->screen[att3b1dev->screen_size]; ++p)
*p = ~ *p;
inverted = !inverted;
break;
case '?':
do_help(dev);
break;
case -1:
error = gs_error_ioerror;
case 'q':
case 'x':
case '\003':
case 'n':
case 'f':
case ' ':
case '\n':
case '\r':
if (flush)
att3b1dev->page_num++;
else if (inverted)
for (p = (ushort *)att3b1dev->screen;
p < (ushort *)&att3b1dev->screen[att3b1dev->screen_size]; ++p)
*p = ~ *p;
if (!(uflags & NBORDER)) {
ioctl(att3b1dev->fd, WIOCGETD, &uw);
uw.uw_uflags = uflags;
ioctl(att3b1dev->fd, WIOCSETD, &uw);
}
ur.ur_srcbase = save_image;
ur.ur_srcwidth = WINWIDTH / 8;
ur.ur_width = uw.uw_width;
ur.ur_height = uw.uw_height;
ur.ur_srcx = 0;
ur.ur_srcy = 0;
ioctl(att3b1dev->fd, WIOCRASTOP, &ur);
ioctl(att3b1dev->fd, WIOCSETTEXT, &ut_orig);
ioctl(att3b1dev->fd, TCSETAF, &old);
write(att3b1dev->fd, "\033[=0C", 5);
if (error) {
att3b1_close(dev);
return_error(error);
}
else
return 0;
}
if (xorigin >= att3b1dev->width - screen_width)
xorigin = att3b1dev->width - screen_width;
if (xorigin < 0)
xorigin = 0;
if (yorigin >= att3b1dev->height - screen_height)
yorigin = att3b1dev->height - screen_height;
if (yorigin < 0)
yorigin = 0;
}
}
int
att3b1_output_page(gx_device *dev, int num_copies, int flush)
{
int code = att3b1_do_output_page(dev, num_copies, flush);
if (code >= 0)
code = gx_finish_output_page(dev, num_copies, flush);
return code;
}
static int
get_char(gx_device *dev)
{
char ch;
int count;
count = read(att3b1dev->fd, &ch, 1);
if (count == 0)
return 'q';
else if (count < 0)
return -1;
else
return ch;
}
static int
getKeyboard(gx_device *dev)
{
char ch;
ch = get_char(dev);
if (ch != '\033')
return ch;
switch (get_char(dev)) {
case '[':
switch(get_char(dev)) {
case 'A':
return 'k';
case 'T':
return '\025';
case 'B':
return 'j';
case 'S':
return '\004';
case 'C':
return 'l';
case 'D':
return 'h';
case 'H':
return 'r';
case 'U':
return '\006';
case 'V':
return '\002';
}
break;
case 'O':
switch(get_char(dev)) {
case 'm':
case 'M':
return '?';
case 'k':
case 'K':
case 'w':
case 'W':
return 'q';
}
break;
case 'N':
switch(get_char(dev)) {
case 'h':
return 'f';
case 'i':
return '=';
case 'L':
return 'l';
case 'K':
return 'h';
}
break;
case '9':
return 'K';
case '0':
return 'J';
}
return '\0';
}