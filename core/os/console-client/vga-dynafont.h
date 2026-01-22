#ifndef _VGA_DYNAFONT_H_
#define _VGA_DYNAFONT_H_ 1
#include <wchar.h>
#include "bdf.h"
typedef struct dynafont *dynafont_t;
#define FONT_INDEX_UNKNOWN 0
error_t dynafont_new (bdf_font_t font, bdf_font_t font_italic,
bdf_font_t font_bold, bdf_font_t font_bold_italic,
int size, int width, dynafont_t *dynafont);
void dynafont_free (dynafont_t df);
int dynafont_lookup (dynafont_t df, conchar_t *chr);
void dynafont_release (dynafont_t df, int vga_font_index);
void dynafont_activate (dynafont_t df);
void dynafont_set_cursor (dynafont_t df, int standout);
void dynafont_change_font (dynafont_t df, bdf_font_t font);
#endif