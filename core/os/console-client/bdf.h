#ifndef _BDF_H_
#define _BDF_H_
#include <stdio.h>
typedef enum
{
BDF_NO_ERROR = 0,
BDF_SYSTEM_ERROR,
BDF_SYNTAX_ERROR,
BDF_INVALID_ARGUMENT,
BDF_COUNT_MISMATCH
} bdf_error_t;
const char *bdf_strerror (bdf_error_t err);
struct bdf_property
{
char *name;
enum { BDF_PROPERTY_NUMBER, BDF_PROPERTY_STRING } type;
union
{
int number;
char *string;
} value;
};
typedef struct bdf_property *bdf_property_t;
struct bdf_bbox
{
int width;
int height;
int offx;
int offy;
};
struct bdf_vector
{
int x;
int y;
};
struct bdf_glyph
{
char *name;
int encoding;
int internal_encoding;
struct bdf_bbox bbox;
unsigned char *bitmap;
int has_swidth : 1;
int has_dwidth : 1;
int has_swidth1 : 1;
int has_dwidth1 : 1;
int has_vvector : 1;
struct bdf_vector swidth;
struct bdf_vector dwidth;
struct bdf_vector swidth1;
struct bdf_vector dwidth1;
struct bdf_vector vvector;
};
struct bdf_font
{
int version_maj;
int version_min;
char *name;
int has_content_version : 1;
int content_version;
int point_size;
int res_x;
int res_y;
struct bdf_bbox bbox;
int __properties_allocated;
int properties_count;
struct bdf_property *properties;
int __glyphs_allocated;
int glyphs_count;
struct bdf_glyph *glyphs;
int metricsset;
int has_swidth : 1;
int has_dwidth : 1;
int has_swidth1 : 1;
int has_dwidth1 : 1;
int has_vvector : 1;
struct bdf_vector swidth;
struct bdf_vector dwidth;
struct bdf_vector swidth1;
struct bdf_vector dwidth1;
struct bdf_vector vvector;
};
typedef struct bdf_font *bdf_font_t;
bdf_error_t bdf_read (FILE *file, bdf_font_t *font, int *linecount);
void bdf_destroy (bdf_font_t font);
bdf_error_t bdf_new (bdf_font_t *font, int version_maj, int version_min,
const char *name, int point_size, int res_x, int res_y,
int bbox_width, int bbox_height, int bbox_offx,
int bbox_offy, int metricsset);
bdf_error_t bdf_set_swidth (bdf_font_t font, int glyph, int x, int y);
bdf_error_t bdf_set_dwidth (bdf_font_t font, int glyph, int x, int y);
bdf_error_t bdf_set_swidth1 (bdf_font_t font, int glyph, int x, int y);
bdf_error_t bdf_set_dwidth1 (bdf_font_t font, int glyph, int x, int y);
bdf_error_t bdf_set_vvector (bdf_font_t font, int glyph, int x, int y);
bdf_error_t bdf_add_string_property (bdf_font_t font, const char *name,
const char *value);
bdf_error_t bdf_add_number_property (bdf_font_t font, const char *name,
int value);
bdf_error_t bdf_add_glyph (bdf_font_t font, const char *name, int encoding,
int internal_encoding, int bbox_width,
int bbox_height, int bbox_offx, int bbox_offy,
const unsigned char *bitmap);
bdf_error_t bdf_write (FILE *filep, bdf_font_t font);
int bdf_compare_glyphs (const void *a, const void *b);
void bdf_sort_glyphs (bdf_font_t font);
struct bdf_glyph *bdf_find_glyph (bdf_font_t font, int enc, int internal_enc);
#endif