#include "gx.h"
#include "gserrors.h"
#include "memory_.h"
#include "gsstruct.h"
#include "gsgdata.h"
#include "gsgcache.h"
#include "gxfont.h"
#include "gxfont42.h"
typedef struct gs_glyph_cache_elem_s gs_glyph_cache_elem;
struct gs_glyph_cache_elem_s {
gs_glyph_data_t gd;
uint glyph_index;
uint lock_count;
gs_glyph_cache_elem *next;
};
gs_public_st_composite(st_glyph_cache_elem, gs_glyph_cache_elem, "gs_glyph_cache_elem",
gs_glyph_cache_elem_enum_ptrs, gs_glyph_cache_elem_reloc_ptrs);
private
ENUM_PTRS_WITH(gs_glyph_cache_elem_enum_ptrs, gs_glyph_cache_elem *e)
{
index --;
if (index < ST_GLYPH_DATA_NUM_PTRS)
return ENUM_USING(st_glyph_data, &e->gd, sizeof(e->gd), index);
return 0;
}
ENUM_PTR(0, gs_glyph_cache_elem, next);
ENUM_PTRS_END
private RELOC_PTRS_WITH(gs_glyph_cache_elem_reloc_ptrs, gs_glyph_cache_elem *e)
{
RELOC_PTR(gs_glyph_cache_elem, next);
RELOC_USING(st_glyph_data, &e->gd, sizeof(e->gd));
} RELOC_PTRS_END
struct gs_glyph_cache_s {
int total_size;
gs_glyph_cache_elem *list;
gs_memory_t *memory;
gs_font_type42 *pfont;
stream *s;
get_glyph_data_from_file read_data;
};
gs_private_st_ptrs4(st_glyph_cache, gs_glyph_cache, "gs_glyph_cache",
gs_glyph_cache_enum_ptrs, gs_glyph_cache_reloc_ptrs, list, memory, pfont, s);
GS_NOTIFY_PROC(gs_glpyh_cache__release);
gs_glyph_cache *
gs_glyph_cache__alloc(gs_font_type42 *pfont, stream *s,
get_glyph_data_from_file read_data)
{
gs_memory_t *mem = pfont->memory->stable_memory;
gs_glyph_cache *gdcache = (gs_glyph_cache *)gs_alloc_struct(mem,
gs_glyph_cache, &st_glyph_cache, "gs_glyph_cache");
if (gdcache == 0)
return 0;
gdcache->total_size = 0;
gdcache->list = NULL;
gdcache->pfont = pfont;
gdcache->s = s;
gdcache->memory = mem;
gdcache->read_data = read_data;
gs_font_notify_register((gs_font *)pfont, gs_glyph_cache__release, (void *)gdcache);
return gdcache;
}
int
gs_glyph_cache__release(void *data, void *event)
{
gs_glyph_cache *this = (gs_glyph_cache *)data;
gs_glyph_cache_elem *e = this->list;
gs_font_type42 *pfont = this->pfont;
while (e != NULL) {
gs_glyph_cache_elem *next_e;
next_e = e->next;
e->gd.procs->free(&e->gd, "gs_glyph_cache__release");
gs_free_object(this->memory, e, "gs_glyph_cache_elem__release");
e = next_e;
}
this->list = NULL;
gs_font_notify_unregister((gs_font *)pfont, gs_glyph_cache__release, (void *)this);
gs_free_object(this->memory, this, "gs_glyph_cache__release");
return 0;
}
private gs_glyph_cache_elem **
gs_glyph_cache_elem__locate(gs_glyph_cache *this, uint glyph_index)
{
gs_glyph_cache_elem **e = &this->list, **p_unlocked = NULL;
int count = 0;
for (; *e != 0; e = &(*e)->next, count++) {
if ((*e)->glyph_index == glyph_index) {
return e;
}
if ((*e)->lock_count == 0)
p_unlocked = e;
}
return p_unlocked;
}
private inline void
gs_glyph_cache_elem__move_to_head(gs_glyph_cache *this, gs_glyph_cache_elem **pe)
{ gs_glyph_cache_elem *e = *pe;
*pe = e->next;
e->next = this->list;
this->list = e;
}
private void
gs_glyph_cache_elem__free_data(gs_glyph_data_t *pgd, client_name_t cname)
{ gs_glyph_cache_elem *e = (gs_glyph_cache_elem *)pgd->proc_data;
e->lock_count--;
}
private int
gs_glyph_cache_elem__substring(gs_glyph_data_t *pgd, uint offset, uint size)
{ gs_glyph_cache_elem *e = (gs_glyph_cache_elem *)pgd->proc_data;
e->lock_count++;
return_error(gs_error_unregistered);
}
private const gs_glyph_data_procs_t gs_glyph_cache_elem_procs = {
gs_glyph_cache_elem__free_data, gs_glyph_cache_elem__substring
};
int
gs_get_glyph_data_cached(gs_font_type42 *pfont, uint glyph_index, gs_glyph_data_t *pgd)
{ gs_glyph_cache *gdcache = pfont->data.gdcache;
gs_glyph_cache_elem **pe = gs_glyph_cache_elem__locate(gdcache, glyph_index);
gs_glyph_cache_elem *e = NULL;
if (pe == NULL || (*pe)->glyph_index != glyph_index) {
int code;
if (pe != NULL && gdcache->total_size > 32767 &&
(*pe)->lock_count <= 0) {
e = *pe;
gdcache->total_size -= e->gd.bits.size + sizeof(*e);
e->gd.procs->free(&e->gd, "gs_get_glyph_data_cached");
gs_glyph_cache_elem__move_to_head(gdcache, pe);
} else {
e = (gs_glyph_cache_elem *)gs_alloc_struct(gdcache->memory,
gs_glyph_cache_elem, &st_glyph_cache_elem, "gs_glyph_cache_elem");
if (e == NULL)
return_error(gs_error_VMerror);
memset(e, 0, sizeof(*e));
e->next = gdcache->list;
gdcache->list = e;
e->gd.memory = gdcache->memory;
}
code = (*gdcache->read_data)(pfont, gdcache->s, glyph_index, &e->gd);
if (code < 0)
return code;
gdcache->total_size += e->gd.bits.size + sizeof(*e);
e->glyph_index = glyph_index;
} else {
e = *pe;
gs_glyph_cache_elem__move_to_head(gdcache, pe);
}
pgd->bits = e->gd.bits;
pgd->proc_data = e;
pgd->procs = &gs_glyph_cache_elem_procs;
e->lock_count++;
return 0;
}