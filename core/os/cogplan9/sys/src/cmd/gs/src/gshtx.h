#ifndef gshtx_INCLUDED
#  define gshtx_INCLUDED
#include "gsmemory.h"
#include "gsht1.h"
#include "gxtmap.h"
#include "gscspace.h"
#define gs_ht gs_halftone
#define gs_spot_ht gs_spot_halftone
#define gs_threshold_ht gs_threshold_halftone
#define gs_ht_component gs_halftone_component
#define gs_multiple_ht gs_multiple_halftone
#define st_gs_ht st_halftone
#define st_ht_comp_element st_ht_component_element
#define ht_spot spot
#define ht_threshold threshold
#define ht_multiple multiple
#ifndef gs_state_DEFINED
#  define gs_state_DEFINED
typedef struct gs_state_s gs_state;
#endif
typedef gs_mapping_closure_proc_t gs_ht_transfer_proc;
extern int gs_ht_build(gs_ht ** ppht, uint num_comps, gs_memory_t * pmem);
extern int gs_ht_set_spot_comp(
gs_ht * pht,
int component_index,
floatp freq,
floatp angle,
float (*spot_func) (floatp, floatp),
bool accurate,
gs_ht_transfer_proc transfer,
const void *client_data
);
extern int gs_ht_set_threshold_comp(
gs_ht * pht,
int component_index,
int width,
int height,
const gs_const_string * thresholds,
gs_ht_transfer_proc transfer,
const void *client_data
);
extern int gs_ht_set_mask_comp(
gs_ht * pht,
int component_index,
int width,
int height,
int num_levels,
const byte * masks,
gs_ht_transfer_proc transfer,
const void *client_data
);
extern void gs_ht_reference(gs_ht * pht);
extern void gs_ht_release(gs_ht * pht);
#define gs_ht_assign(pto, pfrom)    \
BEGIN                           \
gs_ht_reference(pfrom);     \
if (pto != 0)               \
gs_ht_release(pto);     \
pto = pfrom;                \
END
#define gs_ht_init_ptr(pto, pfrom)          \
BEGIN gs_ht_reference(pfrom); pto = pfrom; END
extern int gs_ht_install(gs_state * pgs, gs_ht * pht);
#endif