#include <assert-backtrace.h>
#include <errno.h>
#include <error.h>
#include <fcntl.h>
#include <unistd.h>
#include <iconv.h>
#include <argp.h>
#include <string.h>
#include <stdint.h>
#include <sys/io.h>
#include <sys/mman.h>
#include <sys/fcntl.h>
#include <pthread.h>
#include <hurd/console.h>
#include "driver.h"
#include "timer.h"
#include "fb.h"
#include "vga-hw.h"
#include "vga-support.h"
#include "bdf.h"
#include "vga-dynafont.h"
#include "vga-dynacolor.h"
#include "unicode.h"
#define VGA_DISP_WIDTH 80
#define VGA_DISP_HEIGHT 25
#define DEFAULT_VGA_FONT DEFAULT_VGA_FONT_DIR "vga-system.bdf"
static char *vga_display_font;
#define DEFAULT_VGA_FONT_ITALIC DEFAULT_VGA_FONT_DIR "vga-system-italic.bdf"
static char *vga_display_font_italic;
#define DEFAULT_VGA_FONT_BOLD DEFAULT_VGA_FONT_DIR "vga-system-bold.bdf"
static char *vga_display_font_bold;
#define DEFAULT_VGA_FONT_BOLD_ITALIC \
DEFAULT_VGA_FONT_DIR "vga-system-bold-italic.bdf"
static char *vga_display_font_bold_italic;
static int vga_display_max_glyphs;
static int vga_display_font_width;
static struct timer_list vga_display_timer;
static pthread_mutex_t vga_display_lock;
static struct display_ops vga_display_ops;
static unsigned int current_width;
static unsigned int current_height;
static int cursor_state;
static int cursor_hidden;
struct refchr
{
unsigned int used : 1;
unsigned int chr : 9;
unsigned int attr : 8;
};
typedef struct vga_mousecursor
{
float posx;
float posy;
char oldcolor;
int visible;
int enabled;
} vga_mousecursor_t;
struct vga_display
{
dynafont_t df;
int df_size;
int df_width;
dynacolor_t dc;
unsigned int width;
unsigned int height;
int cur_conchar_attr_init;
conchar_attr_t cur_conchar_attr;
char cur_attr;
vga_mousecursor_t mousecursor;
struct refchr refmatrix[VGA_DISP_HEIGHT][VGA_DISP_WIDTH];
};
struct driver_ops driver_vga_ops;
static void
vga_display_invert_border (void)
{
unsigned char col[3];
pthread_mutex_lock (&vga_display_lock);
vga_read_palette (0, col, 1);
col[0] = 0xff - col[0];
col[1] = 0xff - col[1];
col[2] = 0xff - col[2];
vga_write_palette (0, col, 1);
pthread_mutex_unlock (&vga_display_lock);
}
static int
vga_display_flash_off (void *dummy)
{
vga_display_invert_border ();
return 0;
}
static error_t
vga_display_flash (void *handle)
{
if (timer_remove (&vga_display_timer))
vga_display_invert_border ();
vga_display_invert_border ();
vga_display_timer.expires = fetch_jiffies () + 10;
timer_add (&vga_display_timer);
return 0;
}
static void
hide_mousecursor (struct vga_display *disp)
{
char *oldpos = vga_videomem + 2 * ((int) disp->mousecursor.posy * disp->width
+ (int) disp->mousecursor.posx) + 1;
if (!disp->mousecursor.visible)
return;
*oldpos = disp->mousecursor.oldcolor;
disp->mousecursor.visible = 0;
}
static void
draw_mousecursor (struct vga_display *disp)
{
char *newpos = vga_videomem + 2 * ((int) disp->mousecursor.posy * disp->width
+ (int) disp->mousecursor.posx) + 1;
if (disp->mousecursor.visible)
return;
disp->mousecursor.oldcolor = *newpos;
*newpos = (127) ^ *newpos;
disp->mousecursor.visible = 1;
}
static const char doc[] = "VGA Driver";
static const struct argp_option options[] =
{
{"font", 'f', "FONT", 0, "Use FONT for normal text"},
{"font-italic", 'i', "FONT", 0, "Use FONT for italic text"},
{"font-bold", 'b', "FONT", 0, "Use FONT for bold text"},
{"font-bold-italic",'a', "FONT", 0,
"Use FONT for text that is both bold and italic"},
{"max-colors", 'm', 0 , 0,
"Prefer a lot of colors above a lot of glyphs"},
{"max-glyphs", 'g', 0 , 0,
"Prefer a lot of glyphs above a lot of colors"},
{"font-width", 'w', "NUM" , 0, "Force using NUM pixel-wide glyphs"},
{ 0 }
};
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
int *pos = (int *) state->input;
switch (key)
{
case 'f':
vga_display_font = strdup (arg);
if (! vga_display_font)
return 0;
break;
case 'i':
vga_display_font_italic = strdup (arg);
if (! vga_display_font_italic)
return 0;
break;
case 'b':
vga_display_font_bold = strdup (arg);
if (! vga_display_font_bold)
return 0;
break;
case 'a':
vga_display_font_bold_italic = strdup (arg);
if (! vga_display_font_bold_italic)
return 0;
break;
case 'm':
vga_display_max_glyphs = 0;
break;
case 'g':
vga_display_max_glyphs = 1;
break;
case 'w':
vga_display_font_width = atoi (arg);
break;
case ARGP_KEY_END:
break;
default:
return ARGP_ERR_UNKNOWN;
}
*pos = state->next;
return 0;
}
static struct argp argp = {options, parse_opt, 0, doc};
static error_t
vga_display_init (void **handle, int no_exit, int argc, char *argv[],
int *next)
{
error_t err;
struct vga_display *vgadisp;
int pos = 1;
fb_get_multiboot_params();
pthread_mutex_init (&vga_display_lock, NULL);
timer_clear (&vga_display_timer);
vga_display_timer.fnc = &vga_display_flash_off;
err = argp_parse (&argp, argc, argv, ARGP_IN_ORDER | ARGP_NO_EXIT
| ARGP_SILENT, 0, &pos);
*next += pos - 1;
if (err && err != EINVAL)
return err;
if (fb_type != MULTIBOOT_FRAMEBUFFER_TYPE_EGA_TEXT)
return fb_display_init (handle, &driver_vga_ops);
vgadisp = calloc (1, sizeof *vgadisp);
if (!vgadisp)
return ENOMEM;
vgadisp->df_size = vga_display_max_glyphs ? 512 : 256;
vgadisp->df_width = vga_display_font_width;
vgadisp->width = VGA_DISP_WIDTH;
vgadisp->height = VGA_DISP_HEIGHT;
*handle = vgadisp;
return 0;
}
static error_t
vga_display_start (void *handle)
{
error_t err;
struct vga_display *disp = handle;
bdf_font_t font = NULL;
bdf_font_t font_italic = NULL;
bdf_font_t font_bold = NULL;
bdf_font_t font_bold_italic = NULL;
FILE *font_file;
err = vga_init ();
if (err)
return err;
dynacolor_init ();
#define LOAD_FONT(x,y) \
do { \
font_file = fopen (vga_display_##x ?: DEFAULT_VGA_##y, "r"); \
if (font_file) \
{ \
bdf_error_t bdferr = bdf_read (font_file, &x, NULL); \
if (bdferr) \
x = NULL; \
else \
bdf_sort_glyphs (x); \
fclose (font_file); \
} \
} while (0)
LOAD_FONT (font, FONT);
LOAD_FONT (font_italic, FONT_ITALIC);
LOAD_FONT (font_bold, FONT_BOLD);
LOAD_FONT (font_bold_italic, FONT_BOLD_ITALIC);
err = dynafont_new (font, font_italic, font_bold, font_bold_italic,
disp->df_size, disp->df_width, &disp->df);
if (err)
{
free (disp);
vga_fini ();
return err;
}
dynafont_activate (disp->df);
disp->dc = (disp->df_size == 512) ? dynacolor_init_8 : dynacolor_init_16;
dynacolor_activate (&disp->dc);
err = driver_add_display (&vga_display_ops, disp);
if (err)
{
dynafont_free (disp->df);
dynacolor_fini ();
vga_fini ();
free (disp);
}
return err;
}
static error_t
vga_display_fini (void *handle, int force)
{
struct vga_display *disp = handle;
driver_remove_display (&vga_display_ops, disp);
if (timer_remove (&vga_display_timer))
vga_display_flash_off (0);
dynafont_free (disp->df);
free (disp);
dynacolor_fini ();
vga_fini ();
free (vga_display_font);
free (vga_display_font_italic);
free (vga_display_font_bold);
free (vga_display_font_bold_italic);
return 0;
}
static void
vga_display_restore_status (void *handle)
{
outb (VGA_GFX_MISC_ADDR, VGA_GFX_ADDR_REG);
outb (VGA_GFX_MISC_CHAINOE | VGA_GFX_MISC_B8TOBF, VGA_GFX_DATA_REG);
}
static error_t
vga_display_set_cursor_status (void *handle, uint32_t state)
{
struct vga_display *disp = handle;
if (!cursor_hidden)
{
if (state != CONS_CURSOR_INVISIBLE)
dynafont_set_cursor (disp->df,
state == CONS_CURSOR_VERY_VISIBLE ? 1 : 0);
vga_display_cursor (state == CONS_CURSOR_INVISIBLE ? 0 : 1);
}
cursor_state = state;
return 0;
}
static error_t
vga_display_set_cursor_pos (void *handle, uint32_t col, uint32_t row)
{
struct vga_display *disp = handle;
unsigned int pos = row * disp->width + col;
if (col < disp->width && row < disp->height)
{
vga_set_cursor_pos (pos);
if (cursor_hidden)
{
cursor_hidden = 0;
vga_display_set_cursor_status (handle, cursor_state);
}
}
else if (!cursor_hidden)
{
cursor_hidden = 1;
vga_display_cursor (CONS_CURSOR_INVISIBLE);
}
return 0;
}
static error_t
vga_display_scroll (void *handle, int delta)
{
struct vga_display *disp = handle;
int count = abs(delta) * disp->width;
int i;
struct refchr *refpos;
hide_mousecursor (disp);
if (current_height > disp->height)
return ENOTSUP;
if (delta > 0)
{
vga_memmove (vga_videomem, vga_videomem + 2 * count,
2 * disp->width * (disp->height - delta));
refpos = &disp->refmatrix[0][0];
}
else
{
vga_memmove (vga_videomem + 2 * count, vga_videomem,
2 * disp->width * (disp->height + delta));
refpos = &disp->refmatrix[disp->height + delta][0];
}
for (i = 0; i < count; i++)
{
if (refpos->used)
{
dynafont_release (disp->df, refpos->chr);
dynacolor_release (disp->dc, refpos->attr & 7);
dynacolor_release (disp->dc, (refpos->attr >> 4) & 7);
}
refpos++;
}
if (delta > 0)
{
memmove (&disp->refmatrix[0][0], &disp->refmatrix[0][0] + count,
sizeof (struct refchr) * disp->width * (disp->height - delta));
refpos = &disp->refmatrix[disp->height - delta][0];
}
else
{
memmove (&disp->refmatrix[0][0] + count, &disp->refmatrix[0][0],
sizeof (struct refchr) * disp->width * (disp->height + delta));
refpos = &disp->refmatrix[0][0];
}
for (i = 0; i < count; i++)
(refpos++)->used = 0;
return 0;
}
#if 0
static void
vga_display_change_font (void *handle, bdf_font_t font)
{
struct vga_display *disp = handle;
dynafont_change_font (disp->df, font);
}
#endif
static inline char
vga_display_recalculate_attr (dynacolor_t *dc, conchar_attr_t attr)
{
char vga_attr;
signed char res_fgcol;
signed char res_bgcol;
signed char fgcol;
signed char bgcol;
if (attr.reversed)
{
fgcol = attr.bgcol;
bgcol = attr.fgcol;
}
else
{
fgcol = attr.fgcol;
bgcol = attr.bgcol;
}
if (attr.concealed)
fgcol = bgcol;
else
{
switch (attr.intensity)
{
case CONS_ATTR_INTENSITY_BOLD:
fgcol |= 1 << 3;
break;
case CONS_ATTR_INTENSITY_DIM:
fgcol = CONS_COLOR_BLACK | 1 << 3;
break;
case CONS_ATTR_INTENSITY_NORMAL:
break;
}
}
pthread_mutex_lock (&vga_display_lock);
res_bgcol = dynacolor_lookup (*dc, bgcol);
res_fgcol = dynacolor_lookup (*dc, fgcol);
pthread_mutex_unlock (&vga_display_lock);
if (res_bgcol == -1 || res_fgcol == -1)
dynacolor_replace_colors (dc, fgcol, bgcol, &res_fgcol, &res_bgcol);
vga_attr = res_bgcol << 4 | res_fgcol;
vga_attr |= attr.blinking << 7;
return vga_attr;
}
static error_t
vga_display_clear (void *handle, size_t length, uint32_t col, uint32_t row)
{
struct vga_display *disp = handle;
struct refchr *refpos = &disp->refmatrix[row][0];
int cols;
if (col >= disp->width)
{
col = disp->width - col;
row++;
}
refpos += col;
if (row >= disp->height)
return 0;
cols = length / current_width;
length = (length % current_width) + cols * disp->width ;
if (length > (disp->width * disp->height - (row * disp->width + col)) - col)
length = disp->width * disp->height - (row * disp->width + col) - col;
while (length > 0)
{
if (refpos->used)
{
dynafont_release (disp->df, refpos->chr);
dynacolor_release (disp->dc, refpos->attr & 7);
dynacolor_release (disp->dc, (refpos->attr >> 4) & 7);
refpos->used = 0;
}
refpos++;
length--;
}
return 0;
}
static error_t
vga_display_write (void *handle, conchar_t *str, size_t length,
uint32_t col, uint32_t row)
{
struct vga_display *disp = handle;
char *pos;
struct refchr *refpos = &disp->refmatrix[row][col];
char *mouse_cursor_pos;
if (disp->width < current_width && col >= disp->width)
{
size_t skip = current_width - disp->width;
str += skip;
length -= skip;
col = 0;
row++;
}
pos = vga_videomem + 2 * (row * disp->width + col);
mouse_cursor_pos = (vga_videomem + 2
* ((int) disp->mousecursor.posy
* disp->width + (int) disp->mousecursor.posx) + 1);
while (length--)
{
int charval = dynafont_lookup (disp->df, str);
col++;
if (col > current_width)
{
size_t skip = disp->width - current_width;
pos += skip * 2;
refpos += skip;
col = 1;
row++;
}
else if (disp->width < current_width && col == disp->width)
{
size_t skip = current_width - disp->width;
str += skip;
length -= skip;
col = 1;
row++;
}
if (row >= disp->height)
return 0;
if (!disp->cur_conchar_attr_init
|| !conchar_attr_equal (&disp->cur_conchar_attr, &str->attr))
{
if (!disp->cur_conchar_attr_init)
disp->cur_conchar_attr_init = 1;
disp->cur_conchar_attr = str->attr;
disp->cur_attr = vga_display_recalculate_attr (&disp->dc, str->attr);
}
else
{
dynacolor_add_ref (disp->dc, disp->cur_attr & 7);
dynacolor_add_ref (disp->dc, (disp->cur_attr >> 4) & 7);
}
*(pos++) = charval & 0xff;
if (pos == mouse_cursor_pos)
disp->mousecursor.visible = 0;
*(pos++) = disp->cur_attr
| (disp->df_size == 512 ? (charval >> 5) & 0x8 : 0);
if (refpos->used)
{
dynafont_release (disp->df, refpos->chr);
dynacolor_release (disp->dc, refpos->attr & 7);
dynacolor_release (disp->dc, (refpos->attr >> 4) & 7);
}
refpos->used = 1;
refpos->chr = charval;
refpos->attr = disp->cur_attr;
refpos++;
str++;
}
return 0;
}
static error_t
vga_set_dimension (void *handle, unsigned int width, unsigned int height)
{
if (current_width && current_height)
vga_display_clear (handle, current_width * current_height, 0, 0);
current_width = width;
current_height = height;
return 0;
}
static error_t
vga_display_update (void *handle)
{
struct vga_display *disp = handle;
if (disp->mousecursor.enabled)
draw_mousecursor (disp);
return 0;
}
static error_t
vga_set_mousecursor_pos (void *handle, float x, float y)
{
struct vga_display *disp = handle;
if (disp->mousecursor.visible && x == (int) disp->mousecursor.posx
&& y == (int) disp->mousecursor.posy)
return 0;
hide_mousecursor (disp);
disp->mousecursor.posx = x;
disp->mousecursor.posy = y;
if (disp->mousecursor.enabled)
draw_mousecursor (disp);
return 0;
}
static error_t
vga_set_mousecursor_status (void *handle, int status)
{
struct vga_display *disp = handle;
disp->mousecursor.enabled = status;
if (!status)
hide_mousecursor (disp);
else
draw_mousecursor (disp);
return 0;
}
struct driver_ops driver_vga_ops =
{
vga_display_init,
vga_display_start,
vga_display_fini,
NULL,
vga_display_restore_status
};
static struct display_ops vga_display_ops =
{
vga_display_set_cursor_pos,
vga_display_set_cursor_status,
vga_display_scroll,
vga_display_clear,
vga_display_write,
vga_display_update,
vga_display_flash,
NULL,
vga_set_dimension,
vga_set_mousecursor_pos,
vga_set_mousecursor_status
};