#ifndef gsgdata_INCLUDED
# define gsgdata_INCLUDED
#include "gsstype.h"
typedef struct gs_glyph_data_procs_s gs_glyph_data_procs_t;
#ifndef gs_glyph_data_DEFINED
# define gs_glyph_data_DEFINED
typedef struct gs_glyph_data_s gs_glyph_data_t;
#endif
struct gs_glyph_data_s {
gs_const_bytestring bits;
const gs_glyph_data_procs_t *procs;
void *proc_data;
gs_memory_t *memory;
};
extern_st(st_glyph_data);
#define ST_GLYPH_DATA_NUM_PTRS 2
struct gs_glyph_data_procs_s {
#define GS_PROC_GLYPH_DATA_FREE(proc)\
void proc(gs_glyph_data_t *pgd, client_name_t cname)
GS_PROC_GLYPH_DATA_FREE((*free));
#define GS_PROC_GLYPH_DATA_SUBSTRING(proc)\
int proc(gs_glyph_data_t *pgd, uint offset, uint size)
GS_PROC_GLYPH_DATA_SUBSTRING((*substring));
};
int gs_glyph_data_substring(gs_glyph_data_t *pgd, uint offset, uint size);
void gs_glyph_data_free(gs_glyph_data_t *pgd, client_name_t cname);
#ifndef gs_font_DEFINED
# define gs_font_DEFINED
typedef struct gs_font_s gs_font;
#endif
void gs_glyph_data_from_string(gs_glyph_data_t *pgd, const byte *data,
uint size, gs_font *font);
void gs_glyph_data_from_bytes(gs_glyph_data_t *pgd, const byte *bytes,
uint offset, uint size, gs_font *font);
void gs_glyph_data_from_null(gs_glyph_data_t *pgd);
#endif