#ifndef gxht_INCLUDED
#  define gxht_INCLUDED
#include "gsht1.h"
#include "gsrefct.h"
#include "gxhttype.h"
#include "gxtmap.h"
#include "gscspace.h"
typedef struct gs_spot_halftone_s {
gs_screen_halftone screen;
bool accurate_screens;
gs_mapping_proc transfer;
gs_mapping_closure_t transfer_closure;
} gs_spot_halftone;
#define st_spot_halftone_max_ptrs st_screen_halftone_max_ptrs + 1
#define GS_THRESHOLD_HALFTONE_COMMON\
int width;\
int height;\
gs_mapping_closure_t transfer_closure
typedef struct gs_threshold_halftone_common_s {
GS_THRESHOLD_HALFTONE_COMMON;
} gs_threshold_halftone_common;
typedef struct gs_threshold_halftone_s {
GS_THRESHOLD_HALFTONE_COMMON;
gs_const_string thresholds;
gs_mapping_proc transfer;
} gs_threshold_halftone;
#define st_threshold_halftone_max_ptrs 2
typedef struct gs_threshold2_halftone_s {
GS_THRESHOLD_HALFTONE_COMMON;
int width2;
int height2;
int bytes_per_sample;
gs_const_bytestring thresholds;
} gs_threshold2_halftone;
typedef struct gs_client_order_halftone_s gs_client_order_halftone;
#ifndef gx_ht_order_DEFINED
#  define gx_ht_order_DEFINED
typedef struct gx_ht_order_s gx_ht_order;
#endif
typedef struct gs_client_order_ht_procs_s {
int (*create_order) (gx_ht_order * porder,
gs_state * pgs,
const gs_client_order_halftone * phcop,
gs_memory_t * mem);
} gs_client_order_ht_procs_t;
struct gs_client_order_halftone_s {
int width;
int height;
int num_levels;
const gs_client_order_ht_procs_t *procs;
const void *client_data;
gs_mapping_closure_t transfer_closure;
};
#define st_client_order_halftone_max_ptrs 2
typedef struct gs_halftone_component_s {
int comp_number;
int cname;
gs_halftone_type type;
union {
gs_spot_halftone spot;
gs_threshold_halftone threshold;
gs_threshold2_halftone threshold2;
gs_client_order_halftone client_order;
} params;
} gs_halftone_component;
extern_st(st_halftone_component);
#define public_st_halftone_component()	\
gs_public_st_composite(st_halftone_component, gs_halftone_component,\
"gs_halftone_component", halftone_component_enum_ptrs,\
halftone_component_reloc_ptrs)
extern_st(st_ht_component_element);
#define public_st_ht_component_element() \
gs_public_st_element(st_ht_component_element, gs_halftone_component,\
"gs_halftone_component[]", ht_comp_elt_enum_ptrs, ht_comp_elt_reloc_ptrs,\
st_halftone_component)
#define st_halftone_component_max_ptrs\
max(max(st_spot_halftone_max_ptrs, st_threshold_halftone_max_ptrs),\
st_client_order_halftone_max_ptrs)
typedef struct gs_multiple_halftone_s {
gs_halftone_component *components;
uint num_comp;
int (*get_colorname_string)(const gs_memory_t *mem, gs_separation_name colorname_index,
unsigned char **ppstr, unsigned int *pname_size);
} gs_multiple_halftone;
#define st_multiple_halftone_max_ptrs 1
struct gs_halftone_s {
gs_halftone_type type;
rc_header rc;
union {
gs_screen_halftone screen;
gs_colorscreen_halftone colorscreen;
gs_spot_halftone spot;
gs_threshold_halftone threshold;
gs_threshold2_halftone threshold2;
gs_client_order_halftone client_order;
gs_multiple_halftone multiple;
} params;
};
extern_st(st_halftone);
#define public_st_halftone()	\
gs_public_st_composite(st_halftone, gs_halftone, "gs_halftone",\
halftone_enum_ptrs, halftone_reloc_ptrs)
#define st_halftone_max_ptrs\
max(max(st_screen_halftone_max_ptrs, st_colorscreen_halftone_max_ptrs),\
max(max(st_spot_halftone_max_ptrs, st_threshold_halftone_max_ptrs),\
max(st_client_order_halftone_max_ptrs,\
st_multiple_halftone_max_ptrs)))
void gs_setaccuratescreens(bool);
bool gs_currentaccuratescreens(void);
void gs_setusewts(bool);
bool gs_currentusewts(void);
int gs_screen_init_memory(gs_screen_enum *, gs_state *,
gs_screen_halftone *, bool, gs_memory_t *);
#define gs_screen_init_accurate(penum, pgs, phsp, accurate)\
gs_screen_init_memory(penum, pgs, phsp, accurate, pgs->memory)
void gs_setminscreenlevels(uint);
uint gs_currentminscreenlevels(void);
#endif