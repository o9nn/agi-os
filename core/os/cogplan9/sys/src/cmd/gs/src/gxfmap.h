#ifndef gxfmap_INCLUDED
# define gxfmap_INCLUDED
#include "gsrefct.h"
#include "gsstype.h"
#include "gxfrac.h"
#include "gxtmap.h"
#define log2_transfer_map_size 8
#define transfer_map_size (1 << log2_transfer_map_size)
struct gx_transfer_map_s {
rc_header rc;
gs_mapping_proc proc;
gs_mapping_closure_t closure;
gs_id id;
frac values[transfer_map_size];
};
extern_st(st_transfer_map);
#define public_st_transfer_map() \
gs_public_st_composite(st_transfer_map, gx_transfer_map, "gx_transfer_map",\
transfer_map_enum_ptrs, transfer_map_reloc_ptrs)
void gx_set_identity_transfer(gx_transfer_map *);
#define FRAC_MAP_INTERPOLATE (log2_transfer_map_size <= 8)
#if FRAC_MAP_INTERPOLATE
frac gx_color_frac_map(frac, const frac *);
# define gx_map_color_frac(pgs,cf,m)\
(pgs->m->proc == gs_identity_transfer ? cf :\
gx_color_frac_map(cf, &pgs->m->values[0]))
#else
# define gx_map_color_frac(pgs,cf,m)\
(pgs->m->values[frac2bits(cf, log2_transfer_map_size)])
#endif
#define gx_map_color_float(pmap,v)\
((pmap)->values[(int)((v) * transfer_map_size + 0.5)] / frac_1_float)
float gs_mapped_transfer(floatp, const gx_transfer_map *);
float gs_identity_transfer(floatp, const gx_transfer_map *);
#endif