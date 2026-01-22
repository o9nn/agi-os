#ifndef gxfcmap_INCLUDED
# define gxfcmap_INCLUDED
#include "gsfcmap.h"
#include "gsuid.h"
#include "gxcid.h"
#define MAX_CMAP_CODE_SIZE 4
typedef struct gx_code_space_range_s {
byte first[MAX_CMAP_CODE_SIZE];
byte last[MAX_CMAP_CODE_SIZE];
int size;
} gx_code_space_range_t;
typedef enum {
CODE_VALUE_CID,
CODE_VALUE_GLYPH,
CODE_VALUE_CHARS,
CODE_VALUE_NOTDEF
#define CODE_VALUE_MAX CODE_VALUE_NOTDEF
} gx_cmap_code_value_type_t;
typedef struct gx_cmap_lookup_entry_s {
byte key[2][MAX_CMAP_CODE_SIZE];
int key_size;
bool key_is_range;
gx_cmap_code_value_type_t value_type;
gs_const_string value;
int font_index;
} gx_cmap_lookup_entry_t;
#ifndef gs_cmap_DEFINED
# define gs_cmap_DEFINED
typedef struct gs_cmap_s gs_cmap_t;
#endif
#define GS_CMAP_COMMON\
int CMapType; \
gs_id id; \
\
gs_const_string CMapName;\
gs_cid_system_info_t *CIDSystemInfo; \
int num_fonts;\
float CMapVersion;\
gs_uid uid; \
long UIDOffset;\
int WMode;\
bool from_Unicode; \
bool ToUnicode; \
gs_glyph_name_proc_t glyph_name; \
void *glyph_name_data; \
const gs_cmap_procs_t *procs
extern_st(st_cmap);
#define public_st_cmap() \
BASIC_PTRS(cmap_ptrs) {\
GC_CONST_STRING_ELT(gs_cmap_t, CMapName),\
GC_OBJ_ELT3(gs_cmap_t, CIDSystemInfo, uid.xvalues, glyph_name_data)\
};\
gs_public_st_basic(st_cmap, gs_cmap_t, "gs_cmap_t", cmap_ptrs, cmap_data)
typedef struct gs_cmap_ranges_enum_s gs_cmap_ranges_enum_t;
typedef struct gs_cmap_lookups_enum_s gs_cmap_lookups_enum_t;
typedef struct gs_cmap_procs_s {
int (*decode_next)(const gs_cmap_t *pcmap, const gs_const_string *str,
uint *pindex, uint *pfidx,
gs_char *pchr, gs_glyph *pglyph);
void (*enum_ranges)(const gs_cmap_t *pcmap,
gs_cmap_ranges_enum_t *penum);
void (*enum_lookups)(const gs_cmap_t *pcmap, int which,
gs_cmap_lookups_enum_t *penum);
bool (*is_identity)(const gs_cmap_t *pcmap, int font_index_only);
} gs_cmap_procs_t;
struct gs_cmap_s {
GS_CMAP_COMMON;
};
typedef struct gs_cmap_ranges_enum_procs_s {
int (*next_range)(gs_cmap_ranges_enum_t *penum);
} gs_cmap_ranges_enum_procs_t;
struct gs_cmap_ranges_enum_s {
gx_code_space_range_t range;
const gs_cmap_t *cmap;
const gs_cmap_ranges_enum_procs_t *procs;
uint index;
};
typedef struct gs_cmap_lookups_enum_procs_s {
int (*next_lookup)(gs_cmap_lookups_enum_t *penum);
int (*next_entry)(gs_cmap_lookups_enum_t *penum);
} gs_cmap_lookups_enum_procs_t;
struct gs_cmap_lookups_enum_s {
gx_cmap_lookup_entry_t entry;
const gs_cmap_t *cmap;
const gs_cmap_lookups_enum_procs_t *procs;
uint index[2];
byte temp_value[max(sizeof(gs_glyph), sizeof(gs_char))];
};
extern const gs_cmap_lookups_enum_procs_t gs_cmap_no_lookups_procs;
void gs_cmap_ranges_enum_init(const gs_cmap_t *pcmap,
gs_cmap_ranges_enum_t *penum);
int gs_cmap_enum_next_range(gs_cmap_ranges_enum_t *penum);
void gs_cmap_lookups_enum_init(const gs_cmap_t *pcmap, int which,
gs_cmap_lookups_enum_t *penum);
int gs_cmap_enum_next_lookup(gs_cmap_lookups_enum_t *penum);
int gs_cmap_enum_next_entry(gs_cmap_lookups_enum_t *penum);
void gs_cmap_init(const gs_memory_t *mem, gs_cmap_t *pcmap, int num_fonts);
int gs_cmap_alloc(gs_cmap_t **ppcmap, const gs_memory_struct_type_t *pstype,
int wmode, const byte *map_name, uint name_size,
const gs_cid_system_info_t *pcidsi, int num_fonts,
const gs_cmap_procs_t *procs, gs_memory_t *mem);
void gs_cmap_ranges_enum_setup(gs_cmap_ranges_enum_t *penum,
const gs_cmap_t *pcmap,
const gs_cmap_ranges_enum_procs_t *procs);
void gs_cmap_lookups_enum_setup(gs_cmap_lookups_enum_t *penum,
const gs_cmap_t *pcmap,
const gs_cmap_lookups_enum_procs_t *procs);
bool gs_cmap_is_identity(const gs_cmap_t *pcmap, int font_index_only);
bool gs_cmap_compute_identity(const gs_cmap_t *pcmap, int font_index_only);
#endif