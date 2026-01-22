#ifndef gsovrc_INCLUDED
# define gsovrc_INCLUDED
#include "gsstype.h"
#include "gxcomp.h"
#ifndef gs_overprint_params_t_DEFINED
# define gs_overprint_params_t_DEFINED
typedef struct gs_overprint_params_s gs_overprint_params_t;
#endif
struct gs_overprint_params_s {
bool retain_any_comps;
bool retain_spot_comps;
gx_color_index drawn_comps;
};
typedef struct gs_overprint_s {
gs_composite_common;
gs_overprint_params_t params;
} gs_overprint_t;
#define private_st_gs_overprint_t() \
gs_private_st_simple(st_overprint, gs_overprint_t, "gs_overprint_t");
#define gs_overprint_set_drawn_comp(drawn_comps, i) \
((drawn_comps) |= (gx_color_index)1 << (i))
#define gs_overprint_clear_drawn_comp(drawn_comps, i) \
((drawn_comps) &= ~((gx_color_index)1 << 1))
#define gs_overprint_clear_all_drawn_comps(drawn_comps) \
((drawn_comps) = 0)
#define gs_overprint_get_drawn_comp(drawn_comps, i) \
(((drawn_comps) & ((gx_color_index)1 << (i))) != 0)
extern_st(st_overprint_params);
#define public_st_overprint_params_t \
gs_public_st_simple( st_overprint_params, \
gs_overprint_params_t, \
"gs_overprint_params_t" )
extern int gs_create_overprint(
gs_composite_t ** ppct,
const gs_overprint_params_t * pparams,
gs_memory_t * mem );
extern bool gs_is_overprint_compositor(const gs_composite_t * pct);
#endif