#include <assert-backtrace.h>
#include <hurd/console.h>
#include "vga-hw.h"
#include "vga-support.h"
#include "vga-dynacolor.h"
dynacolor_t dynacolor_init_8 = DYNACOLOR_INIT_8;
dynacolor_t dynacolor_init_16 = DYNACOLOR_INIT_16;
static const unsigned char std_palette[16][DYNACOLOR_COMPONENTS] =
{
{ 0, 0, 0 },
{ 42, 0, 0 },
{ 0, 42, 0 },
{ 42, 21, 0 },
{ 0, 0, 42 },
{ 42, 0, 42 },
{ 0, 42, 42 },
{ 42, 42, 42 },
{ 21, 21, 21 },
{ 63, 21, 21 },
{ 21, 63, 21 },
{ 63, 63, 21 },
{ 21, 21, 63 },
{ 63, 21, 63 },
{ 21, 63, 63 },
{ 63, 63, 63 }
};
static dynacolor_t *active_dynacolor;
static unsigned char saved_palette[16][DYNACOLOR_COMPONENTS];
static unsigned char saved_palette_attr[16] =
{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
void
dynacolor_init (void)
{
vga_exchange_palette_attributes (0, saved_palette_attr, 16);
vga_read_palette (0, saved_palette[0], 16);
vga_write_palette (0, std_palette[0], 16);
}
void
dynacolor_fini (void)
{
vga_write_palette (0, saved_palette[0], 16);
vga_exchange_palette_attributes (0, saved_palette_attr, 16);
}
void
dynacolor_activate (dynacolor_t *dc)
{
if (dc == active_dynacolor)
return;
if (dc->ref[0] < 0 && (!active_dynacolor || active_dynacolor->ref[0] >= 0))
{
vga_write_palette (0, std_palette[0], 16);
}
else if (dc->ref[0] >= 0
&& (!active_dynacolor || active_dynacolor->ref[0] < 0))
{
int i;
for (i = 0; i < 16; i++)
if (dc->col[i] >= 0)
{
vga_write_palette (dc->col[i], std_palette[i], 1);
vga_write_palette (8 + dc->col[i], std_palette[i], 1);
}
}
active_dynacolor = dc;
}
signed char
dynacolor_allocate (dynacolor_t *dc, unsigned char col)
{
int i;
for (i = 0; i < 8; i++)
if (dc->ref[i] == 0)
{
int j;
for (j = 0; j < 16; j++)
if (dc->col[j] == i)
{
dc->col[j] = -1;
break;
}
dc->ref[i] = 1;
dc->col[col] = i;
if (active_dynacolor == dc)
{
vga_write_palette (0 + i, std_palette[col], 1);
vga_write_palette (8 + i, std_palette[col], 1);
}
return i;
}
return -1;
}
void
dynacolor_replace_colors (dynacolor_t *dc,
signed char fgcol, signed char bgcol,
signed char *r_fgcol, signed char *r_bgcol)
{
static signed char pref[16][9] =
{
{ CONS_COLOR_BLACK | (1 << 3), CONS_COLOR_BLUE,
CONS_COLOR_YELLOW, CONS_COLOR_RED, CONS_COLOR_MAGENTA,
CONS_COLOR_GREEN, CONS_COLOR_CYAN, CONS_COLOR_WHITE,
CONS_COLOR_BLUE | (1 << 3) },
{ CONS_COLOR_RED | (1 << 3), CONS_COLOR_YELLOW,
CONS_COLOR_MAGENTA, CONS_COLOR_BLUE, CONS_COLOR_CYAN,
CONS_COLOR_GREEN, CONS_COLOR_WHITE, CONS_COLOR_BLACK,
CONS_COLOR_BLACK | (1 << 3) },
{ CONS_COLOR_GREEN | (1 << 3), CONS_COLOR_CYAN,
CONS_COLOR_YELLOW, CONS_COLOR_BLUE, CONS_COLOR_RED,
CONS_COLOR_MAGENTA, CONS_COLOR_WHITE, CONS_COLOR_BLACK,
CONS_COLOR_BLACK | (1 << 3) },
{ CONS_COLOR_YELLOW | (1 << 3), CONS_COLOR_RED,
CONS_COLOR_GREEN, CONS_COLOR_MAGENTA, CONS_COLOR_BLUE,
CONS_COLOR_CYAN, CONS_COLOR_WHITE, CONS_COLOR_BLACK,
CONS_COLOR_BLACK | (1 << 3) },
{ CONS_COLOR_BLUE | (1 << 3), CONS_COLOR_CYAN,
CONS_COLOR_MAGENTA, CONS_COLOR_RED, CONS_COLOR_GREEN,
CONS_COLOR_YELLOW, CONS_COLOR_WHITE, CONS_COLOR_BLACK,
CONS_COLOR_BLACK | (1 << 3) },
{ CONS_COLOR_MAGENTA | (1 << 3), CONS_COLOR_RED,
CONS_COLOR_BLUE, CONS_COLOR_YELLOW, CONS_COLOR_CYAN,
CONS_COLOR_BLUE, CONS_COLOR_WHITE, CONS_COLOR_BLACK,
CONS_COLOR_BLACK | (1 << 3) },
{ CONS_COLOR_CYAN | (1 << 3), CONS_COLOR_BLUE,
CONS_COLOR_MAGENTA, CONS_COLOR_GREEN, CONS_COLOR_RED,
CONS_COLOR_YELLOW, CONS_COLOR_WHITE, CONS_COLOR_BLACK,
CONS_COLOR_BLACK | (1 << 3) },
{ CONS_COLOR_WHITE | (1 << 3), CONS_COLOR_CYAN,
CONS_COLOR_GREEN, CONS_COLOR_YELLOW, CONS_COLOR_MAGENTA,
CONS_COLOR_RED, CONS_COLOR_BLUE, CONS_COLOR_CYAN | (1 << 3),
CONS_COLOR_BLACK | (1 << 3) },
{ CONS_COLOR_BLACK, CONS_COLOR_BLUE | (1 << 3),
CONS_COLOR_YELLOW | (1 << 3), CONS_COLOR_RED | (1 << 3),
CONS_COLOR_MAGENTA | (1 << 3), CONS_COLOR_GREEN | (1 << 3),
CONS_COLOR_CYAN | (1 << 3), CONS_COLOR_WHITE | (1 << 3),
CONS_COLOR_WHITE },
{ CONS_COLOR_RED, CONS_COLOR_YELLOW | (1 << 3),
CONS_COLOR_MAGENTA | (1 << 3), CONS_COLOR_BLUE | (1 << 3),
CONS_COLOR_CYAN | (1 << 3), CONS_COLOR_GREEN | (1 << 3),
CONS_COLOR_WHITE | (1 << 3), CONS_COLOR_WHITE,
CONS_COLOR_BLACK | (1 << 3) },
{ CONS_COLOR_GREEN, CONS_COLOR_CYAN | (1 << 3),
CONS_COLOR_YELLOW | (1 << 3), CONS_COLOR_BLUE | (1 << 3),
CONS_COLOR_RED | (1 << 3), CONS_COLOR_MAGENTA | (1 << 3),
CONS_COLOR_WHITE | (1 << 3), CONS_COLOR_WHITE,
CONS_COLOR_BLACK | (1 << 3) },
{ CONS_COLOR_YELLOW, CONS_COLOR_RED | (1 << 3),
CONS_COLOR_GREEN | (1 << 3), CONS_COLOR_MAGENTA | (1 << 3),
CONS_COLOR_BLUE | (1 << 3), CONS_COLOR_CYAN | (1 << 3),
CONS_COLOR_WHITE | (1 << 3), CONS_COLOR_WHITE,
CONS_COLOR_BLACK | (1 << 3) },
{ CONS_COLOR_BLUE, CONS_COLOR_CYAN | (1 << 3),
CONS_COLOR_MAGENTA | (1 << 3), CONS_COLOR_RED | (1 << 3),
CONS_COLOR_GREEN | (1 << 3), CONS_COLOR_YELLOW | (1 << 3),
CONS_COLOR_WHITE | (1 << 3), CONS_COLOR_WHITE,
CONS_COLOR_BLACK | (1 << 3) },
{ CONS_COLOR_MAGENTA, CONS_COLOR_RED | (1 << 3),
CONS_COLOR_BLUE | (1 << 3), CONS_COLOR_YELLOW | (1 << 3),
CONS_COLOR_CYAN | (1 << 3), CONS_COLOR_GREEN | (1 << 3),
CONS_COLOR_WHITE | (1 << 3), CONS_COLOR_WHITE,
CONS_COLOR_BLACK | (1 << 3) },
{ CONS_COLOR_CYAN, CONS_COLOR_BLUE | (1 << 3),
CONS_COLOR_MAGENTA | (1 << 3), CONS_COLOR_GREEN | (1 << 3),
CONS_COLOR_RED | (1 << 3), CONS_COLOR_YELLOW | (1 << 3),
CONS_COLOR_WHITE | (1 << 3), CONS_COLOR_WHITE,
CONS_COLOR_BLACK | (1 << 3) },
{ CONS_COLOR_WHITE, CONS_COLOR_CYAN | (1 << 3),
CONS_COLOR_GREEN | (1 << 3), CONS_COLOR_YELLOW | (1 << 3),
CONS_COLOR_MAGENTA | (1 << 3), CONS_COLOR_RED | (1 << 3),
CONS_COLOR_BLUE | (1 << 3), CONS_COLOR_CYAN,
CONS_COLOR_BLACK | (1 << 3) },
};
signed char res_fgcol = *r_fgcol;
signed char res_bgcol = *r_bgcol;
signed char new_bgcol = bgcol;
int i;
if (res_bgcol == -1)
{
for (i = 0; i < 9; i++)
{
if (res_fgcol == -1 || pref[bgcol][i] != fgcol)
{
res_bgcol = dynacolor_lookup (*dc, pref[bgcol][i]);
if (res_bgcol >= 0)
break;
}
}
assert_backtrace (res_bgcol >= 0);
new_bgcol = pref[bgcol][i];
}
if (fgcol == bgcol)
{
assert_backtrace (res_fgcol == -1);
res_fgcol = dynacolor_lookup (*dc, new_bgcol);
}
else
assert_backtrace (res_fgcol != res_bgcol);
if (res_fgcol == -1)
{
for (i = 0; i < 9; i++)
{
if (pref[fgcol][i] != new_bgcol)
{
res_fgcol = dynacolor_lookup (*dc, pref[fgcol][i]);
if (res_fgcol >= 0)
break;
}
}
assert_backtrace (res_fgcol >= 0);
}
*r_fgcol = res_fgcol;
*r_bgcol = res_bgcol;
}