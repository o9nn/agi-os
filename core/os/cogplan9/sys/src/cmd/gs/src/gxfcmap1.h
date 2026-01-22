#ifndef gxfcmap1_INCLUDED
#  define gxfcmap1_INCLUDED
#include "gxfcmap.h"
typedef struct gs_cmap_adobe1_s gs_cmap_adobe1_t;
typedef struct gx_cmap_lookup_range_s {
gs_cmap_adobe1_t *cmap;
int num_entries;
byte key_prefix[MAX_CMAP_CODE_SIZE];
int key_prefix_size;
int key_size;
bool key_is_range;
gs_string keys;
gx_cmap_code_value_type_t value_type;
int value_size;
gs_string values;
int font_index;
} gx_cmap_lookup_range_t;
extern_st(st_cmap_lookup_range_element);
#define public_st_cmap_lookup_range() \
gs_public_st_composite(st_cmap_lookup_range, gx_cmap_lookup_range_t,\
"gx_cmap_lookup_range_t", cmap_lookup_range_enum_ptrs,\
cmap_lookup_range_reloc_ptrs)
#define public_st_cmap_lookup_range_element() \
gs_public_st_element(st_cmap_lookup_range_element, gx_cmap_lookup_range_t,\
"gx_cmap_lookup_range_t[]", cmap_lookup_range_elt_enum_ptrs,\
cmap_lookup_range_elt_reloc_ptrs, st_cmap_lookup_range)
typedef struct gx_code_space_s {
gx_code_space_range_t *ranges;
int num_ranges;
} gx_code_space_t;
typedef struct gx_code_map_s {
gx_cmap_lookup_range_t *lookup;
int num_lookup;
} gx_code_map_t;
struct gs_cmap_adobe1_s {
GS_CMAP_COMMON;
gx_code_space_t code_space;
gx_code_map_t def;
gx_code_map_t notdef;
gs_glyph_mark_proc_t mark_glyph;
void *mark_glyph_data;
};
extern_st(st_cmap_adobe1);
#define public_st_cmap_adobe1()	\
gs_public_st_suffix_add4(st_cmap_adobe1, gs_cmap_adobe1_t,\
"gs_cmap_adobe1_t", cmap_adobe1_enum_ptrs, cmap_adobe1_reloc_ptrs,\
st_cmap,\
code_space.ranges, def.lookup, notdef.lookup, mark_glyph_data)
int gs_cmap_adobe1_alloc(gs_cmap_adobe1_t **ppcmap, int wmode,
const byte *map_name, uint name_size,
uint num_fonts, uint num_ranges, uint num_lookups,
uint keys_size, uint values_size,
const gs_cid_system_info_t *pcidsi, gs_memory_t *mem);
#endif