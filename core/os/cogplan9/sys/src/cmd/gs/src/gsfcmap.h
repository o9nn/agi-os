#ifndef gsfcmap_INCLUDED
# define gsfcmap_INCLUDED
#include "gsccode.h"
#ifndef gs_cmap_DEFINED
# define gs_cmap_DEFINED
typedef struct gs_cmap_s gs_cmap_t;
#endif
int gs_cmap_create_identity(gs_cmap_t **ppcmap, int num_bytes, int wmode,
gs_memory_t *mem);
int gs_cmap_create_char_identity(gs_cmap_t **ppcmap, int num_bytes,
int wmode, gs_memory_t *mem);
int gs_cmap_decode_next(const gs_cmap_t *pcmap, const gs_const_string *str,
uint *pindex, uint *pfidx,
gs_char *pchr, gs_glyph *pglyph);
int gs_cmap_ToUnicode_alloc(gs_memory_t *mem, int id, int num_codes, int key_size,
gs_cmap_t **ppcmap);
void gs_cmap_ToUnicode_add_pair(gs_cmap_t *pcmap, int code0, int code2);
#endif