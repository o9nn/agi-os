#include <stddef.h>
#include <assert-backtrace.h>
#include <malloc.h>
#include <wchar.h>
#include <stdlib.h>
#include <hurd/ihash.h>
#include <string.h>
#include <hurd/console.h>
#include "vga-hw.h"
#include "vga-support.h"
#include "bdf.h"
#include "vga-dynafont.h"
#include "unicode.h"
static dynafont_t active_dynafont;
typedef unsigned char vga_font_glyph[VGA_FONT_HEIGHT];
struct mapped_character
{
int refs;
#define WCHAR_BOLD	((wchar_t) 0x20000000)
#define WCHAR_ITALIC	((wchar_t) 0x10000000)
#define WCHAR_MASK	CONS_WCHAR_MASK
wchar_t character;
hurd_ihash_locp_t locp;
};
struct dynafont
{
bdf_font_t font;
bdf_font_t font_italic;
bdf_font_t font_bold;
bdf_font_t font_bold_italic;
int size;
int width;
struct hurd_ihash charmap;
struct mapped_character *charmap_data;
int vga_font_last_free_index;
int vga_font_free_indices;
int use_lgc;
int vga_font_last_free_index_lgc;
int vga_font_free_indices_lgc;
vga_font_glyph *vga_font;
int cursor_standout;
};
static
wchar_t ibm437_to_unicode[VGA_FONT_SIZE] = {
0,
UNICODE_WHITE_SMILING_FACE,
UNICODE_BLACK_SMILING_FACE,
UNICODE_BLACK_HEART_SUIT,
UNICODE_BLACK_DIAMOND_SUIT,
UNICODE_BLACK_CLUB_SUIT,
UNICODE_BLACK_SPADE_SUIT,
UNICODE_BULLET,
UNICODE_INVERSE_BULLET,
UNICODE_WHITE_CIRCLE,
UNICODE_INVERSE_WHITE_CIRCLE,
UNICODE_MALE_SIGN,
UNICODE_FEMALE_SIGN,
UNICODE_EIGHTH_NOTE,
UNICODE_BEAMED_EIGHTH_NOTES,
UNICODE_WHITE_SUN_WITH_RAYS,
UNICODE_BLACK_RIGHT_POINTING_TRIANGLE,
UNICODE_BLACK_LEFT_POINTING_TRIANGLE,
UNICODE_UP_DOWN_ARROW,
UNICODE_DOUBLE_EXCLAMATION_MARK,
UNICODE_PILCROW_SIGN,
UNICODE_SECTION_SIGN,
UNICODE_BLACK_RECTANGLE,
UNICODE_UP_DOWN_ARROW_WITH_BASE,
UNICODE_UPWARDS_ARROW,
UNICODE_DOWNWARDS_ARROW,
UNICODE_RIGHTWARDS_ARROW,
UNICODE_LEFTWARDS_ARROW,
UNICODE_RIGHT_ANGLE,
UNICODE_LEFT_RIGHT_ARROW,
UNICODE_BLACK_UP_POINTING_TRIANGLE,
UNICODE_BLACK_DOWN_POINTING_TRIANGLE,
' ', '!', '"', '#', '$', '%', '&', '\'',
'(', ')', '*', '+', ',', '-', '.', '/',
'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
':', ';', '<', '=', '>', '?',
'@', 'A', 'B', 'C', 'D', 'E', 'F', 'G',
'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
'X', 'Y', 'Z', '[', '\\', ']', '^', '_',
'`', 'a', 'b', 'c', 'd', 'e', 'f', 'g',
'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
'p', 'q', 'r', 's', 't', 'u', 'v', 'w',
'x', 'y', 'z', '{', '|', '}', '~', UNICODE_HOUSE,
UNICODE_LATIN_CAPITAL_LETTER_C_WITH_CEDILLA,
UNICODE_LATIN_SMALL_LETTER_U_WITH_DIARESIS,
UNICODE_LATIN_SMALL_LETTER_E_WITH_ACUTE,
UNICODE_LATIN_SMALL_LETTER_A_WITH_CIRCUMFLEX,
UNICODE_LATIN_SMALL_LETTER_A_WITH_DIARESIS,
UNICODE_LATIN_SMALL_LETTER_A_WITH_GRAVE,
UNICODE_LATIN_SMALL_LETTER_A_WITH_RING_ABOVE,
UNICODE_LATIN_SMALL_LETTER_C_WITH_CEDILLA,
UNICODE_LATIN_SMALL_LETTER_E_WITH_CIRCUMFLEX,
UNICODE_LATIN_SMALL_LETTER_E_WITH_DIARESIS,
UNICODE_LATIN_SMALL_LETTER_E_WITH_GRAVE,
UNICODE_LATIN_SMALL_LETTER_I_WITH_DIARESIS,
UNICODE_LATIN_SMALL_LETTER_I_WITH_CIRCUMFLEX,
UNICODE_LATIN_SMALL_LETTER_I_WITH_GRAVE,
UNICODE_LATIN_CAPITAL_LETTER_A_WITH_DIARESIS,
UNICODE_LATIN_CAPITAL_LETTER_A_WITH_RING_ABOVE,
UNICODE_LATIN_CAPITAL_LETTER_E_WITH_ACUTE,
UNICODE_LATIN_SMALL_LETTER_AE,
UNICODE_LATIN_CAPITAL_LETTER_AE,
UNICODE_LATIN_SMALL_LETTER_O_WITH_CIRCUMFLEX,
UNICODE_LATIN_SMALL_LETTER_O_WITH_DIARESIS,
UNICODE_LATIN_SMALL_LETTER_O_WITH_GRAVE,
UNICODE_LATIN_SMALL_LETTER_U_WITH_CIRCUMFLEX,
UNICODE_LATIN_SMALL_LETTER_U_WITH_GRAVE,
UNICODE_LATIN_SMALL_LETTER_Y_WITH_DIARESIS,
UNICODE_LATIN_CAPITAL_LETTER_O_WITH_DIARESIS,
UNICODE_LATIN_CAPITAL_LETTER_U_WITH_DIARESIS,
UNICODE_CENT_SIGN,
UNICODE_POUND_SIGN,
UNICODE_YEN_SIGN,
UNICODE_PESETA_SIGN,
UNICODE_LATIN_SMALL_LETTER_F_WITH_HOOK,
UNICODE_LATIN_SMALL_LETTER_A_WITH_ACUTE,
UNICODE_LATIN_SMALL_LETTER_I_WITH_ACUTE,
UNICODE_LATIN_SMALL_LETTER_O_WITH_ACUTE,
UNICODE_LATIN_SMALL_LETTER_U_WITH_ACUTE,
UNICODE_LATIN_SMALL_LETTER_N_WITH_TILDE,
UNICODE_LATIN_CAPITAL_LETTER_N_WITH_TILDE,
UNICODE_FEMININE_ORDINAL_INDICATOR,
UNICODE_MASCULINE_ORDINAL_INDICATOR,
UNICODE_INVERTED_QUESTION_MARK,
UNICODE_REVERSED_NOT_SIGN,
UNICODE_NOT_SIGN,
UNICODE_VULGAR_FRACTION_ONE_HALF,
UNICODE_VULGAR_FRACTION_ONE_QUARTER,
UNICODE_INVERTED_EXCLAMATION_MARK,
UNICODE_LEFT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK,
UNICODE_RIGHT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK,
UNICODE_LIGHT_SHADE,
UNICODE_MEDIUM_SHADE,
UNICODE_DARK_SHADE,
UNICODE_BOX_DRAWINGS_LIGHT_VERTICAL,
UNICODE_BOX_DRAWINGS_LIGHT_VERTICAL_AND_LEFT,
UNICODE_BOX_DRAWINGS_VERTICAL_SINGLE_AND_LEFT_DOUBLE,
UNICODE_BOX_DRAWINGS_VERTICAL_DOUBLE_AND_LEFT_SINGLE,
UNICODE_BOX_DRAWINGS_DOWN_DOUBLE_AND_LEFT_SINGLE,
UNICODE_BOX_DRAWINGS_DOWN_SINGLE_AND_LEFT_DOUBLE,
UNICODE_BOX_DRAWINGS_DOUBLE_VERTICAL_AND_LEFT,
UNICODE_BOX_DRAWINGS_DOUBLE_VERTICAL,
UNICODE_BOX_DRAWINGS_DOUBLE_DOWN_AND_LEFT,
UNICODE_BOX_DRAWINGS_DOUBLE_UP_AND_LEFT,
UNICODE_BOX_DRAWINGS_UP_DOUBLE_AND_LEFT_SINGLE,
UNICODE_BOX_DRAWINGS_UP_SINGLE_AND_LEFT_DOUBLE,
UNICODE_BOX_DRAWINGS_LIGHT_DOWN_AND_LEFT,
UNICODE_BOX_DRAWINGS_LIGHT_UP_AND_RIGHT,
UNICODE_BOX_DRAWINGS_LIGHT_UP_AND_HORIZONTAL,
UNICODE_BOX_DRAWINGS_LIGHT_DOWN_AND_HORIZONTAL,
UNICODE_BOX_DRAWINGS_LIGHT_VERTICAL_AND_RIGHT,
UNICODE_BOX_DRAWINGS_LIGHT_HORIZONTAL,
UNICODE_BOX_DRAWINGS_LIGHT_VERTICAL_AND_HORIZONTAL,
UNICODE_BOX_DRAWINGS_VERTICAL_SINGLE_AND_RIGHT_DOUBLE,
UNICODE_BOX_DRAWINGS_VERTICAL_DOUBLE_AND_RIGHT_SINGLE,
UNICODE_BOX_DRAWINGS_DOUBLE_UP_AND_RIGHT,
UNICODE_BOX_DRAWINGS_DOUBLE_DOWN_AND_RIGHT,
UNICODE_BOX_DRAWINGS_DOUBLE_UP_AND_HORIZONTAL,
UNICODE_BOX_DRAWINGS_DOUBLE_DOWN_AND_HORIZONTAL,
UNICODE_BOX_DRAWINGS_DOUBLE_VERTICAL_AND_RIGHT,
UNICODE_BOX_DRAWINGS_DOUBLE_HORIZONTAL,
UNICODE_BOX_DRAWINGS_DOUBLE_VERTICAL_AND_HORIZONTAL,
UNICODE_BOX_DRAWINGS_UP_SINGLE_AND_HORIZONTAL_DOUBLE,
UNICODE_BOX_DRAWINGS_UP_DOUBLE_AND_HORIZONTAL_SINGLE,
UNICODE_BOX_DRAWINGS_DOWN_SINGLE_AND_HORIZONTAL_DOUBLE,
UNICODE_BOX_DRAWINGS_DOWN_DOUBLE_AND_HORIZONTAL_SINGLE,
UNICODE_BOX_DRAWINGS_UP_DOUBLE_AND_RIGHT_SINGLE,
UNICODE_BOX_DRAWINGS_UP_SINGLE_AND_RIGHT_DOUBLE,
UNICODE_BOX_DRAWINGS_DOWN_SINGLE_AND_RIGHT_DOUBLE,
UNICODE_BOX_DRAWINGS_DOWN_DOUBLE_AND_RIGHT_SINGLE,
UNICODE_BOX_DRAWINGS_VERTICAL_DOUBLE_AND_HORIZONTAL_SINGLE,
UNICODE_BOX_DRAWINGS_VERTICAL_SINGLE_AND_HORIZONTAL_DOUBLE,
UNICODE_BOX_DRAWINGS_LIGHT_UP_AND_LEFT,
UNICODE_BOX_DRAWINGS_LIGHT_DOWN_AND_RIGHT,
UNICODE_FULL_BLOCK,
UNICODE_LOWER_HALF_BLOCK,
UNICODE_LEFT_HALF_BLOCK,
UNICODE_RIGHT_HALF_BLOCK,
UNICODE_UPPER_HALF_BLOCK,
UNICODE_GREEK_SMALL_LETTER_ALPHA,
UNICODE_GREEK_SMALL_LETTER_BETA,
UNICODE_GREEK_CAPITAL_LETTER_GAMMA,
UNICODE_GREEK_SMALL_LETTER_PI,
UNICODE_GREEK_CAPITAL_LETTER_SIGMA,
UNICODE_GREEK_SMALL_LETTER_SIGMA,
UNICODE_GREEK_SMALL_LETTER_MU,
UNICODE_GREEK_SMALL_LETTER_TAU,
UNICODE_GREEK_CAPITAL_LETTER_PHI,
UNICODE_GREEK_CAPITAL_LETTER_OMICRON,
UNICODE_GREEK_CAPITAL_LETTER_OMEGA,
UNICODE_GREEK_SMALL_LETTER_DELTA,
UNICODE_INFINITY,
UNICODE_GREEK_SMALL_LETTER_PHI,
UNICODE_GREEK_SMALL_LETTER_EPSILON,
UNICODE_INTERSECTION,
UNICODE_IDENTICAL_TO,
UNICODE_PLUS_MINUS_SIGN,
UNICODE_GREATER_THAN_OR_EQUAL_TO,
UNICODE_LESS_THAN_OR_EQUAL_TO,
UNICODE_TOP_HALF_INTEGRAL,
UNICODE_BOTTOM_HALF_INTEGRAL,
UNICODE_DIVISION_SIGN,
UNICODE_ALMOST_EQUAL_TO,
UNICODE_DEGREE_SIGN,
UNICODE_BULLET_OPERATOR,
UNICODE_MIDDLE_DOT,
UNICODE_SQUARE_ROOT,
UNICODE_SUPERSCRIPT_LATIN_SMALL_LETTER,
UNICODE_SUPERSCRIPT_TWO,
UNICODE_BLACK_SQUARE,
UNICODE_NO_BREAK_SPACE
};
static bdf_font_t
create_system_font (void)
{
bdf_error_t bdferr;
bdf_font_t font;
unsigned char bitmap[VGA_FONT_SIZE][VGA_FONT_HEIGHT];
int width = vga_get_font_width ();
int i;
void vga_add_glyph (int pos, int encoding)
{
char name[16];
snprintf (name, sizeof (name), "VGA %i", pos);
if (width == 8)
bdferr = bdf_add_glyph (font, name, encoding,
0, 8, 16, 0, 0, bitmap[pos]);
else
{
int i;
unsigned char glyph_bitmap[32];
for (i = 0; i < 16; i++)
{
glyph_bitmap[i * 2] = bitmap[pos][i];
if (pos >= VGA_FONT_LGC_BEGIN
&& pos < VGA_FONT_LGC_BEGIN + VGA_FONT_LGC_COUNT)
glyph_bitmap[i * 2 + 1]
= (bitmap[pos][i] & 1) ? 0x80 : 0;
else
glyph_bitmap[i * 2 + 1] = 0;
}
bdferr = bdf_add_glyph (font, name, encoding,
0, 9, 16, 0, 0, glyph_bitmap);
}
}
bdferr = bdf_new (&font, 2, 2, "vga-system", 10, 100, 100,
width, 16, 0, 0, 0);
if (bdferr)
{
if (bdferr != BDF_SYSTEM_ERROR)
errno = EGRATUITOUS;
return NULL;
}
vga_read_font_buffer (0, 0, (unsigned char *) bitmap,
VGA_FONT_SIZE * VGA_FONT_HEIGHT);
for (i = 0; i < VGA_FONT_SIZE; i++)
if (ibm437_to_unicode[i])
{
vga_add_glyph (i, ibm437_to_unicode[i]);
if (bdferr)
break;
if (ibm437_to_unicode[i] == UNICODE_GREEK_SMALL_LETTER_BETA)
vga_add_glyph (i, UNICODE_LATIN_SMALL_LETTER_SHARP_S);
else if (ibm437_to_unicode[i] == UNICODE_GREEK_SMALL_LETTER_MU)
vga_add_glyph (i, UNICODE_MICRO_SIGN);
if (bdferr)
break;
}
if (bdferr)
{
bdf_destroy (font);
if (bdferr != BDF_SYSTEM_ERROR)
errno = EGRATUITOUS;
return NULL;
}
return font;
}
#if QUAERENDO_INVENIETIS
#define GNU_HEAD_BEGIN (UNICODE_PRIVATE_USE_AREA + 0x0f00)
static void
add_gnu_head (bdf_font_t font)
{
#define GNU_HEAD_WIDTH 6
static unsigned char gnu_head[][GNU_HEAD_WIDTH] =
{
{ 255, 255, 255, 255, 255, 255 }, { 255,   0, 127, 255, 252,  31 },
{ 252,   0,  31, 255, 224,   7 }, { 248,   0,   7, 255,   0,   3 },
{ 240,   0,  15, 255, 128,   3 }, { 240,  31, 255, 255, 252,   1 },
{ 224,  63, 241, 255, 255,   1 }, { 192, 127, 128,  96, 255, 129 },
{ 192, 255,   0,   0,  63, 193 }, { 192, 254,   0,   0,  31, 193 },
{ 192, 252,   0,   0,  15, 193 }, { 192, 248,   0,   0,  15, 193 },
{ 192, 248,   0,   0,   7, 129 }, { 192,  96,  63, 131, 192,   1 },
{ 192,   1, 227,  98, 112,   1 }, { 192,   3, 195, 244, 176,   3 },
{ 224,   7, 221, 125, 248,   3 }, { 240,  15, 184, 124, 120,   7 },
{ 240,  15, 248, 124,  60,  15 }, { 248,  15, 220, 254, 124, 127 },
{ 252,  31, 223, 255, 254, 127 }, { 255, 159, 159, 255,  31, 191 },
{ 255, 223, 159, 255,  35,  63 }, { 255, 191, 127, 195, 152, 127 },
{ 255, 188, 255, 156, 199, 255 }, { 255,  96, 253, 134, 115, 255 },
{ 254, 134, 251, 227, 251, 255 }, { 254,  46, 254, 249, 251, 255 },
{ 255, 238, 126, 127, 231, 255 }, { 255, 239, 127, 127, 207, 255 },
{ 255, 239,  63,  63, 231, 255 }, { 255, 247, 159, 158,  15, 255 },
{ 255, 247, 207, 193, 159, 255 }, { 255, 247, 223, 255, 223, 255 },
{ 255, 243, 199, 252,  31, 255 }, { 255, 251, 227, 224,  63, 255 },
{ 255, 253, 241, 240, 255, 255 }, { 255, 252, 244, 126, 255, 255 },
{ 255, 254, 121, 122, 255, 255 }, { 255, 255, 252,  48, 255, 255 },
{ 255, 255, 252,  35, 255, 255 }, { 255, 255, 249,   1, 127, 255 },
{ 255, 255, 251,   0, 127, 255 }, { 255, 255, 255, 128, 255, 255 },
{ 255, 255, 255, 255, 255, 255 }
};
int height = (font->bbox.height > 32) ? 32 : font->bbox.height;
int width = (font->bbox.width + 7) / 8;
int rows = sizeof (gnu_head) / sizeof (gnu_head[0]);
int nr = (rows + height - 1) / height;
int row, col;
if (nr * GNU_HEAD_WIDTH > GNU_HEAD_BEGIN - UNICODE_PRIVATE_USE_AREA + 1)
return;
for (int i = 0; i < nr * GNU_HEAD_WIDTH; i++)
if (bdf_find_glyph (font, (int) GNU_HEAD_BEGIN + i, 0)
|| bdf_find_glyph (font, -1, (int) GNU_HEAD_BEGIN + i))
return;
for (row = 0; row < nr; row++)
for (col = 0; col < GNU_HEAD_WIDTH; col++)
{
char bitmap[font->bbox.height][width];
char name[] = "GNU Head ..........";
sprintf (&name[9], "%i", row * GNU_HEAD_WIDTH + col);
memset (bitmap, 0, sizeof (bitmap));
for (int j = 0; j < height && row * height + j < rows; j++)
bitmap[j][0] = gnu_head[row * height + j][col];
if (bdf_add_glyph (font, name,
GNU_HEAD_BEGIN + row * GNU_HEAD_WIDTH + col,
0, font->bbox.width, font->bbox.height,
0, 0, (unsigned char *) bitmap))
return;
}
}
#endif
error_t
dynafont_new (bdf_font_t font, bdf_font_t font_italic, bdf_font_t font_bold,
bdf_font_t font_bold_italic, int size, int width,
dynafont_t *dynafont)
{
dynafont_t df;
struct bdf_glyph *glyph = NULL;
if (!font)
font = create_system_font ();
if (!font || !font->bbox.height)
return errno;
if (!width)
width = font->bbox.width;
if ((width % 8) == 0)
width = 8;
if (width != 8 && width != 9)
return EINVAL;
df = malloc (sizeof *df);
if (!df)
return ENOMEM;
#if QUAERENDO_INVENIETIS
add_gnu_head (font);
#endif
bdf_sort_glyphs (font);
df->font = font;
df->font_italic = font_italic;
df->font_bold = font_bold;
df->font_bold_italic = font_bold_italic;
df->size = size;
df->width = width;
df->cursor_standout = 0;
df->charmap_data = calloc (size, sizeof (struct mapped_character));
if (!df->charmap_data)
{
free (df);
return ENOMEM;
}
df->vga_font = malloc (sizeof (vga_font_glyph) * size);
if (!df->vga_font)
{
free (df->charmap_data);
free (df);
return ENOMEM;
}
hurd_ihash_init (&df->charmap, offsetof (struct mapped_character, locp));
if (width == 9)
{
df->use_lgc = 1;
df->vga_font_free_indices = df->size
- (df->size / 256) * VGA_FONT_LGC_COUNT;
df->vga_font_last_free_index = 0;
df->vga_font_free_indices_lgc = (df->size / 256) * VGA_FONT_LGC_COUNT;
df->vga_font_last_free_index_lgc = VGA_FONT_LGC_BEGIN;
}
else
{
df->use_lgc = 0;
df->vga_font_free_indices = df->size;
df->vga_font_last_free_index = 0;
df->vga_font_free_indices_lgc = 0;
df->vga_font_last_free_index_lgc = 0;
}
for (int c = ' '; c <= '~'; c++)
{
glyph = bdf_find_glyph (df->font, c, 0);
if (!glyph)
glyph = bdf_find_glyph (df->font, -1, c);
if (glyph)
{
struct mapped_character *chr = &df->charmap_data[c];
df->vga_font_free_indices--;
chr->refs = 1;
for (int i = 0; i < ((glyph->bbox.height > 32)
? 32 : glyph->bbox.height); i++)
df->vga_font[c][i]
= glyph->bitmap[i * ((glyph->bbox.width + 7) / 8)];
if (glyph->bbox.height < 32)
memset (((char *) df->vga_font[c])
+ glyph->bbox.height, 0, 32 - glyph->bbox.height);
hurd_ihash_add (&df->charmap, c, chr);
}
}
{
struct mapped_character *chr = &df->charmap_data[FONT_INDEX_UNKNOWN];
df->vga_font_free_indices--;
chr->refs = 1;
glyph = bdf_find_glyph (df->font, UNICODE_REPLACEMENT_CHARACTER, 0);
if (!glyph)
glyph = bdf_find_glyph (df->font, -1, UNICODE_REPLACEMENT_CHARACTER);
if (glyph)
{
for (int i = 0; i < ((glyph->bbox.height > 32)
? 32 : glyph->bbox.height); i++)
df->vga_font[FONT_INDEX_UNKNOWN][i]
= glyph->bitmap[i * ((glyph->bbox.width + 7) / 8)];
if (glyph->bbox.height < 32)
memset (((char *) df->vga_font[FONT_INDEX_UNKNOWN])
+ glyph->bbox.height, 0, 32 - glyph->bbox.height);
hurd_ihash_add (&df->charmap, UNICODE_REPLACEMENT_CHARACTER, chr);
}
else
{
int i;
unsigned char *gl = df->vga_font[FONT_INDEX_UNKNOWN];
gl[0] = 0x7E;
gl[1] = 0xC3;
gl[2] = 0x99;
gl[3] = 0x99;
gl[4] = 0xF9;
gl[5] = 0xF3;
gl[6] = 0xF3;
gl[7] = 0xE7;
gl[8] = 0xFF;
gl[9] = 0xE7;
gl[10] = 0xE7;
gl[11] = 0x7E;
for (i = 12; i < 32; i++)
gl[i] = 0;
}
}
*dynafont = df;
return 0;
}
void
dynafont_free (dynafont_t df)
{
if (active_dynafont == df)
active_dynafont = NULL;
bdf_destroy (df->font);
if (df->font_italic)
bdf_destroy (df->font_italic);
if (df->font_bold)
bdf_destroy (df->font_bold);
if (df->font_bold_italic)
bdf_destroy (df->font_bold_italic);
hurd_ihash_destroy (&df->charmap);
free (df->charmap_data);
free (df->vga_font);
free (df);
}
static inline int
is_lgc (wchar_t chr)
{
static wchar_t horiz_glyphs[] =
{
UNICODE_BOX_DRAWINGS_LIGHT_HORIZONTAL,
UNICODE_BOX_DRAWINGS_HEAVY_HORIZONTAL,
UNICODE_BOX_DRAWINGS_LIGHT_DOWN_AND_RIGHT,
UNICODE_BOX_DRAWINGS_DOWN_LIGHT_AND_RIGHT_HEAVY,
UNICODE_BOX_DRAWINGS_DOWN_HEAVY_AND_RIGHT_LIGHT,
UNICODE_BOX_DRAWINGS_HEAVY_DOWN_AND_RIGHT,
UNICODE_BOX_DRAWINGS_LIGHT_UP_AND_RIGHT,
UNICODE_BOX_DRAWINGS_UP_LIGHT_AND_RIGHT_HEAVY,
UNICODE_BOX_DRAWINGS_UP_HEAVY_AND_RIGHT_LIGHT,
UNICODE_BOX_DRAWINGS_HEAVY_UP_AND_RIGHT,
UNICODE_BOX_DRAWINGS_LIGHT_VERTICAL_AND_RIGHT,
UNICODE_BOX_DRAWINGS_VERTICAL_LIGHT_AND_RIGHT_HEAVY,
UNICODE_BOX_DRAWINGS_UP_HEAVY_AND_RIGHT_UP_LIGHT,
UNICODE_BOX_DRAWINGS_DOWN_HEAVY_AND_RIGHT_UP_LIGHT,
UNICODE_BOX_DRAWINGS_VERTICAL_HEAVY_AND_RIGHT_LIGHT,
UNICODE_BOX_DRAWINGS_DOWN_LIGHT_AND_RIGHT_UP_HEAVY,
UNICODE_BOX_DRAWINGS_UP_LIGHT_AND_RIGHT_DOWN_HEAVY,
UNICODE_BOX_DRAWINGS_HEAVY_VERTICAL_AND_RIGHT,
UNICODE_BOX_DRAWINGS_LIGHT_DOWN_AND_HORIZONTAL,
UNICODE_BOX_DRAWINGS_LEFT_HEAVY_AND_RIGHT_DOWN_LIGHT,
UNICODE_BOX_DRAWINGS_RIGHT_HEAVY_AND_LEFT_DOWN_LIGHT,
UNICODE_BOX_DRAWINGS_DOWN_LIGHT_AND_HORIZONTAL_HEAVY,
UNICODE_BOX_DRAWINGS_DOWN_HEAVY_AND_HORIZONTAL_LIGHT,
UNICODE_BOX_DRAWINGS_RIGHT_LIGHT_AND_LEFT_DOWN_HEAVY,
UNICODE_BOX_DRAWINGS_LEFT_LIGHT_AND_RIGHT_DOWN_HEAVY,
UNICODE_BOX_DRAWINGS_HEAVY_DOWN_AND_HORIZONTAL,
UNICODE_BOX_DRAWINGS_LIGHT_UP_AND_HORIZONTAL,
UNICODE_BOX_DRAWINGS_LEFT_HEAVY_AND_RIGHT_UP_LIGHT,
UNICODE_BOX_DRAWINGS_RIGHT_HEAVY_AND_LEFT_UP_LIGHT,
UNICODE_BOX_DRAWINGS_UP_LIGHT_AND_HORIZONTAL_HEAVY,
UNICODE_BOX_DRAWINGS_UP_HEAVY_AND_HORIZONTAL_LIGHT,
UNICODE_BOX_DRAWINGS_RIGHT_LIGHT_AND_LEFT_UP_HEAVY,
UNICODE_BOX_DRAWINGS_LEFT_LIGHT_AND_RIGHT_UP_HEAVY,
UNICODE_BOX_DRAWINGS_HEAVY_UP_AND_HORIZONTAL,
UNICODE_BOX_DRAWINGS_LIGHT_VERTICAL_AND_HORIZONTAL,
UNICODE_BOX_DRAWINGS_LEFT_HEAVY_AND_RIGHT_VERTICAL_LIGHT,
UNICODE_BOX_DRAWINGS_RIGHT_HEAVY_AND_LEFT_VERTICAL_LIGHT,
UNICODE_BOX_DRAWINGS_VERTICAL_LIGHT_AND_HORIZONTAL_HEAVY,
UNICODE_BOX_DRAWINGS_UP_HEAVY_AND_DOWN_HORIZONTAL_LIGHT,
UNICODE_BOX_DRAWINGS_DOWN_HEAVY_AND_UP_HORIZONTAL_LIGHT,
UNICODE_BOX_DRAWINGS_VERTICAL_HEAVY_AND_HORIZONTAL_LIGHT,
UNICODE_BOX_DRAWINGS_LEFT_UP_HEAVY_AND_RIGHT_DOWN_LIGHT,
UNICODE_BOX_DRAWINGS_RIGHT_UP_HEAVY_AND_LEFT_DOWN_LIGHT,
UNICODE_BOX_DRAWINGS_LEFT_DOWN_HEAVY_AND_RIGHT_UP_LIGHT,
UNICODE_BOX_DRAWINGS_RIGHT_DOWN_HEAVY_AND_LEFT_UP_LIGHT,
UNICODE_BOX_DRAWINGS_DOWN_LIGHT_AND_UP_HORIZONTAL_HEAVY,
UNICODE_BOX_DRAWINGS_UP_LIGHT_AND_DOWN_HORIZONTAL_HEAVY,
UNICODE_BOX_DRAWINGS_RIGHT_LIGHT_AND_LEFT_VERTICAL_HEAVY,
UNICODE_BOX_DRAWINGS_LEFT_LIGHT_AND_RIGHT_VERTICAL_HEAVY,
UNICODE_BOX_DRAWINGS_HEAVY_VERTICAL_AND_HORIZONTAL,
UNICODE_BOX_DRAWINGS_DOUBLE_HORIZONTAL,
UNICODE_BOX_DRAWINGS_DOWN_SINGLE_AND_RIGHT_DOUBLE,
UNICODE_BOX_DRAWINGS_DOWN_DOUBLE_AND_RIGHT_SINGLE,
UNICODE_BOX_DRAWINGS_DOUBLE_DOWN_AND_RIGHT,
UNICODE_BOX_DRAWINGS_UP_SINGLE_AND_RIGHT_DOUBLE,
UNICODE_BOX_DRAWINGS_UP_DOUBLE_AND_RIGHT_SINGLE,
UNICODE_BOX_DRAWINGS_DOUBLE_UP_AND_RIGHT,
UNICODE_BOX_DRAWINGS_VERTICAL_SINGLE_AND_RIGHT_DOUBLE,
UNICODE_BOX_DRAWINGS_VERTICAL_DOUBLE_AND_RIGHT_SINGLE,
UNICODE_BOX_DRAWINGS_DOUBLE_VERTICAL_AND_RIGHT,
UNICODE_BOX_DRAWINGS_DOWN_SINGLE_AND_HORIZONTAL_DOUBLE,
UNICODE_BOX_DRAWINGS_DOWN_DOUBLE_AND_HORIZONTAL_SINGLE,
UNICODE_BOX_DRAWINGS_DOUBLE_DOWN_AND_HORIZONTAL,
UNICODE_BOX_DRAWINGS_UP_SINGLE_AND_HORIZONTAL_DOUBLE,
UNICODE_BOX_DRAWINGS_UP_DOUBLE_AND_HORIZONTAL_SINGLE,
UNICODE_BOX_DRAWINGS_DOUBLE_UP_AND_HORIZONTAL,
UNICODE_BOX_DRAWINGS_VERTICAL_SINGLE_AND_HORIZONTAL_DOUBLE,
UNICODE_BOX_DRAWINGS_VERTICAL_DOUBLE_AND_HORIZONTAL_SINGLE,
UNICODE_BOX_DRAWINGS_DOUBLE_VERTICAL_AND_HORIZONTAL,
UNICODE_BOX_DRAWINGS_LIGHT_ARC_DOWN_AND_RIGHT,
UNICODE_BOX_DRAWINGS_LIGHT_ARC_UP_AND_RIGHT,
#if 0
UNICODE_BOX_DRAWINGS_LIGHT_DIAGONAL_UPPER_RIGHT_TO_LOWER_LEFT,
UNICODE_BOX_DRAWINGS_LIGHT_DIAGONAL_UPPER_LEFT_TO_LOWER_RIGHT,
UNICODE_BOX_DRAWINGS_LIGHT_DIAGONAL_CROSS,
#endif
UNICODE_BOX_DRAWINGS_LIGHT_RIGHT,
UNICODE_BOX_DRAWINGS_HEAVY_RIGHT,
UNICODE_BOX_DRAWINGS_LIGHT_LEFT_AND_HEAVY_RIGHT,
UNICODE_BOX_DRAWINGS_HEAVY_LEFT_AND_LIGHT_RIGHT,
UNICODE_UPPER_HALF_BLOCK,
UNICODE_LOWER_ONE_EIGHTH_BLOCK,
UNICODE_LOWER_ONE_QUARTER_BLOCK,
UNICODE_LOWER_THREE_EIGHTHS_BLOCK,
UNICODE_LOWER_HALF_BLOCK,
UNICODE_LOWER_FIVE_EIGHTHS_BLOCK,
UNICODE_LOWER_THREE_QUARTERS_BLOCK,
UNICODE_LOWER_SEVEN_EIGHTHS_BLOCK,
UNICODE_FULL_BLOCK,
UNICODE_RIGHT_HALF_BLOCK,
#if 0
UNICODE_LIGHT_SHADE,
UNICODE_MEDIUM_SHADE,
UNICODE_DARK_SHADE,
#endif
UNICODE_BLACK_SQUARE,
UNICODE_UPPER_ONE_EIGHTH_BLOCK,
UNICODE_RIGHT_ONE_EIGHTH_BLOCK,
UNICODE_QUADRANT_LOWER_RIGHT,
UNICODE_QUADRANT_UPPER_LEFT_AND_LOWER_LEFT_AND_LOWER_RIGHT,
UNICODE_QUADRANT_UPPER_LEFT_AND_LOWER_RIGHT,
UNICODE_QUADRANT_UPPER_LEFT_AND_UPPER_RIGHT_AND_LOWER_LEFT,
UNICODE_QUADRANT_UPPER_LEFT_AND_UPPER_RIGHT_AND_LOWER_RIGHT,
UNICODE_QUADRANT_UPPER_RIGHT,
UNICODE_QUADRANT_UPPER_RIGHT_AND_LOWER_LEFT,
UNICODE_QUADRANT_UPPER_RIGHT_AND_LOWER_LEFT_AND_LOWER_RIGHT,
};
int cmp_wchar (const void *a, const void *b)
{
const wchar_t *wa = (const wchar_t *) a;
const wchar_t *wb = (const wchar_t *) b;
return (*wa > *wb) - (*wa < *wb);
}
#if QUAERENDO_INVENIETIS
if (chr >= GNU_HEAD_BEGIN && chr <= GNU_HEAD_BEGIN + 50)
return 1;
#endif
return bsearch (&chr, horiz_glyphs,
sizeof (horiz_glyphs) / sizeof (horiz_glyphs[0]),
sizeof (horiz_glyphs[0]), cmp_wchar) ? 1 : 0;
}
static int
dynafont_lookup_internal (dynafont_t df, bdf_font_t font,
wchar_t wide_chr, wchar_t attr, int *rpos)
{
struct mapped_character *chr = hurd_ihash_find (&df->charmap,
(int) (wide_chr | attr));
int lgc;
struct bdf_glyph *glyph;
int pos;
int found = 0;
lgc = df->use_lgc ? is_lgc (wide_chr) : 0;
if (chr)
{
if (!chr->refs++)
{
if (lgc)
df->vga_font_free_indices_lgc--;
else
df->vga_font_free_indices--;
}
*rpos = chr - df->charmap_data;
return 1;
}
if ((lgc && !df->vga_font_free_indices_lgc)
|| (!lgc && !df->vga_font_free_indices))
return 0;
glyph = bdf_find_glyph (font, (int) (wide_chr & ~CONS_WCHAR_CONTINUED), 0);
if (!glyph)
glyph = bdf_find_glyph (font, -1, (int) (wide_chr & ~CONS_WCHAR_CONTINUED));
if (!glyph)
return 0;
if (lgc)
{
int start_pos = df->vga_font_last_free_index_lgc + 1;
if ((start_pos % VGA_FONT_SIZE)
== VGA_FONT_LGC_BEGIN + VGA_FONT_LGC_COUNT)
{
start_pos += VGA_FONT_SIZE - VGA_FONT_LGC_COUNT;
start_pos %= df->size;
}
pos = start_pos;
do
{
if (df->charmap_data[pos].refs == 0)
{
found = 1;
break;
}
pos++;
if ((pos % VGA_FONT_SIZE) == VGA_FONT_LGC_BEGIN + VGA_FONT_LGC_COUNT)
{
pos += VGA_FONT_SIZE - VGA_FONT_LGC_COUNT;
pos %= df->size;
}
}
while (pos != start_pos);
assert_backtrace (found);
df->vga_font_free_indices_lgc--;
df->vga_font_last_free_index_lgc = pos;
}
else
{
int start_pos = (df->vga_font_last_free_index + 1) % df->size;
if (df->use_lgc && (start_pos % VGA_FONT_SIZE) == VGA_FONT_LGC_BEGIN)
start_pos += VGA_FONT_LGC_COUNT;
pos = start_pos;
do
{
if (df->charmap_data[pos].refs == 0)
{
found = 1;
break;
}
pos = (pos + 1) % df->size;
if (df->use_lgc && (pos % VGA_FONT_SIZE) == VGA_FONT_LGC_BEGIN)
pos += VGA_FONT_LGC_COUNT;
}
while (pos != start_pos);
assert_backtrace (found);
df->vga_font_free_indices--;
df->vga_font_last_free_index = pos;
}
chr = &df->charmap_data[pos];
chr->refs = 1;
chr->character = (wide_chr | attr);
{
int height = (glyph->bbox.height > 32) ? 32 : glyph->bbox.height;
int bwidth = (glyph->bbox.width + 7) / 8;
int ofs = (bwidth >= 2) && (wide_chr & CONS_WCHAR_CONTINUED);
for (int i = 0; i < height; i++)
df->vga_font[pos][i] = glyph->bitmap[i * bwidth + ofs];
if (height < 32)
memset (&df->vga_font[pos][height], 0, 32 - height);
}
if (active_dynafont == df)
vga_write_font_buffer (0, pos, df->vga_font[pos],
VGA_FONT_HEIGHT);
if (chr->locp)
hurd_ihash_locp_remove (&df->charmap, chr->locp);
hurd_ihash_add (&df->charmap, (int) (wide_chr | attr), chr);
*rpos = pos;
return 1;
}
int
dynafont_lookup (dynafont_t df, conchar_t *conchr)
{
wchar_t attr = (conchr->attr.italic ? WCHAR_ITALIC : 0)
| (conchr->attr.bold ? WCHAR_BOLD : 0);
int found = 0;
int pos = FONT_INDEX_UNKNOWN;
if (attr == (WCHAR_BOLD | WCHAR_ITALIC) && df->font_bold_italic)
found = dynafont_lookup_internal (df, df->font_bold_italic,
conchr->chr, WCHAR_BOLD | WCHAR_ITALIC,
&pos);
if (!found && (attr & WCHAR_BOLD) && df->font_bold)
found = dynafont_lookup_internal (df, df->font_bold,
conchr->chr, WCHAR_BOLD, &pos);
if (!found && (attr & WCHAR_ITALIC) && df->font_italic)
found = dynafont_lookup_internal (df, df->font_italic,
conchr->chr, WCHAR_ITALIC, &pos);
if (!found)
found = dynafont_lookup_internal (df, df->font, conchr->chr, 0, &pos);
if (!found)
{
df->charmap_data[FONT_INDEX_UNKNOWN].refs++;
pos = FONT_INDEX_UNKNOWN;
}
return pos;
}
void
dynafont_release (dynafont_t df, int vga_font_index)
{
if (! --df->charmap_data[vga_font_index].refs)
{
if (df->use_lgc
&& is_lgc (df->charmap_data[vga_font_index].character & WCHAR_MASK))
df->vga_font_free_indices_lgc++;
else
df->vga_font_free_indices++;
}
}
void
dynafont_set_cursor (dynafont_t df, int standout)
{
int height = (df->font->bbox.height > 32) ? 32 : df->font->bbox.height;
df->cursor_standout = standout;
if (df == active_dynafont)
{
if (standout)
vga_set_cursor_size (1, height - 1);
else
vga_set_cursor_size ((height >= 2) ? height - 2 : 0, height - 1);
}
}
void
dynafont_activate (dynafont_t df)
{
int height = (df->font->bbox.height > 32) ? 32 : df->font->bbox.height;
vga_write_font_buffer (0, 0, (unsigned char *) df->vga_font,
df->size * VGA_FONT_HEIGHT);
vga_select_font_buffer (0, (df->size == 512) ? 1 : 0);
vga_set_font_height (height);
vga_set_font_width (df->width);
active_dynafont = df;
dynafont_set_cursor (df, df->cursor_standout);
}
#if 0
void
dynafont_change_font (dynafont_t df, bdf_font_t font)
{
int i;
df->font = font;
for (i = 0; i < df->size; i++)
{
#ifndef ENCODING_UNKNOWN
if (i == FONT_INDEX_UNKNOWN)
continue;
#endif
if (! df->charmap_data[i].refs)
{
if (df->charmap_data[i].locp)
{
hurd_ihash_locp_remove (&df->charmap, df->charmap_data[i].locp);
df->charmap_data[i].locp = NULL;
}
}
else
{
struct bdf_glyph *glyph;
glyph = bdf_find_glyph (df->font,
(int) df->charmap_data[i].character, 0);
if (!glyph)
glyph = bdf_find_glyph (df->font, -1,
(int) df->charmap_data[i].character);
if (!glyph)
{
#ifdef ENCODING_UNKNOWN
if (i == FONT_INDEX_UNKNOWN)
continue;
#endif
memcpy (df->vga_font[i], df->vga_font[FONT_INDEX_UNKNOWN], 32);
}
else
{
for (int j = 0; j < ((glyph->bbox.height > 32)
? 32 : glyph->bbox.height); j++)
df->vga_font[i][j]
= glyph->bitmap[j * ((glyph->bbox.width + 7) / 8)];
if (glyph->bbox.height < 32)
memset (((char *) df->vga_font[i]) + glyph->bbox.height,
0, 32 - glyph->bbox.height);
}
}
}
if (active_dynafont == df)
dynafont_activate (df);
}
#endif