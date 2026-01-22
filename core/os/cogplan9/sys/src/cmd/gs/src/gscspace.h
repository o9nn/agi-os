#ifndef gscspace_INCLUDED
#  define gscspace_INCLUDED
#include "gsmemory.h"
#include "gsiparam.h"
typedef enum {
gs_color_space_index_DeviceGray = 0,
gs_color_space_index_DeviceRGB,
gs_color_space_index_DeviceCMYK,
gs_color_space_index_DevicePixel,
gs_color_space_index_DeviceN,
gs_color_space_index_CIEDEFG,
gs_color_space_index_CIEDEF,
gs_color_space_index_CIEABC,
gs_color_space_index_CIEA,
gs_color_space_index_Separation,
gs_color_space_index_Indexed,
gs_color_space_index_Pattern,
gs_color_space_index_CIEICC
} gs_color_space_index;
#define GS_COLOR_SPACE_TYPE_NAMES\
"DeviceGray", "DeviceRGB", "DeviceCMYK", "DevicePixel", "DeviceN",\
"ICCBased", "CIEBasedDEFG", "CIEBasedDEF", "CIEBasedABC", "CIEBasedA",\
"Separation", "Indexed", "Pattern"
typedef struct gs_color_space_type_s gs_color_space_type;
#define gs_cspace_common(param_union)   \
const gs_color_space_type * type;   \
gs_memory_t *               pmem;   \
gs_id                       id;     \
union {                             \
param_union;                    \
}                           params
typedef struct gs_device_pixel_params_s {
int depth;
} gs_device_pixel_params;
typedef struct gs_cie_a_s gs_cie_a;
typedef struct gs_cie_abc_s gs_cie_abc;
typedef struct gs_cie_def_s gs_cie_def;
typedef struct gs_cie_defg_s gs_cie_defg;
#define gs_small_base_cspace_params     \
gs_device_pixel_params   pixel;     \
gs_cie_defg *            defg;      \
gs_cie_def *             def;       \
gs_cie_abc *             abc;       \
gs_cie_a *               a
typedef struct gs_small_base_color_space_s {
gs_cspace_common(gs_small_base_cspace_params);
} gs_small_base_color_space;
#define gs_small_base_color_space_size sizeof(gs_small_base_color_space)
typedef struct gs_cie_icc_s gs_cie_icc;
typedef struct gs_cieicc_params_s {
gs_cie_icc *                picc_info;
gs_small_base_color_space   alt_space;
} gs_icc_params;
#define gs_base_cspace_params   \
gs_small_base_cspace_params;\
gs_icc_params   icc
typedef struct gs_base_color_space_s {
gs_cspace_common(gs_base_cspace_params);
} gs_base_color_space;
#define gs_base_color_space_size sizeof(gs_base_color_space)
#ifndef gs_device_n_map_DEFINED
#  define gs_device_n_map_DEFINED
typedef struct gs_device_n_map_s gs_device_n_map;
#endif
typedef ulong gs_separation_name;
typedef int (gs_callback_func_get_colorname_string)
(const gs_memory_t *mem, gs_separation_name colorname, unsigned char **ppstr, unsigned int *plen);
typedef enum { SEP_NONE, SEP_ALL, SEP_OTHER } separation_type;
typedef struct gs_separation_params_s {
gs_separation_name sep_name;
gs_base_color_space alt_space;
gs_device_n_map *map;
separation_type sep_type;
bool use_alt_cspace;
gs_callback_func_get_colorname_string *get_colorname_string;
} gs_separation_params;
typedef struct gs_device_n_params_s {
gs_separation_name *names;
uint num_components;
gs_base_color_space alt_space;
gs_device_n_map *map;
bool use_alt_cspace;
gs_callback_func_get_colorname_string *get_colorname_string;
} gs_device_n_params;
#define gs_direct_cspace_params         \
gs_base_cspace_params;              \
gs_separation_params separation;    \
gs_device_n_params device_n
typedef struct gs_direct_color_space_s {
gs_cspace_common(gs_direct_cspace_params);
} gs_direct_color_space;
#define gs_direct_color_space_size sizeof(gs_direct_color_space)
typedef struct gs_indexed_map_s gs_indexed_map;
typedef struct gs_indexed_params_s {
gs_direct_color_space base_space;
int hival;
union {
gs_const_string table;
gs_indexed_map *map;
} lookup;
bool use_proc;
} gs_indexed_params;
#define gs_paint_cspace_params          \
gs_direct_cspace_params;            \
gs_indexed_params indexed
typedef struct gs_paint_color_space_s {
gs_cspace_common(gs_paint_cspace_params);
} gs_paint_color_space;
#define gs_paint_color_space_size sizeof(gs_paint_color_space)
typedef struct gs_pattern_params_s {
bool has_base_space;
gs_paint_color_space base_space;
} gs_pattern_params;
struct gs_color_space_s {
gs_cspace_common(
gs_paint_cspace_params;
gs_pattern_params pattern
);
};
#define gs_pattern_color_space_size sizeof(gs_color_space)
#ifndef gs_color_space_DEFINED
#  define gs_color_space_DEFINED
typedef struct gs_color_space_s gs_color_space;
#endif
#define public_st_color_space()	  \
gs_public_st_composite( st_color_space,         \
gs_color_space,         \
"gs_color_space",       \
color_space_enum_ptrs,  \
color_space_reloc_ptrs  \
)
#define st_color_space_max_ptrs 2
extern int
gs_cspace_init_DeviceGray(const gs_memory_t *mem, gs_color_space *pcs),
gs_cspace_build_DeviceGray(gs_color_space ** ppcspace,
gs_memory_t * pmem),
gs_cspace_init_DeviceRGB(const gs_memory_t *mem, gs_color_space *pcs),
gs_cspace_build_DeviceRGB(gs_color_space ** ppcspace,
gs_memory_t * pmem),
gs_cspace_init_DeviceCMYK(const gs_memory_t *mem, gs_color_space *pcs),
gs_cspace_build_DeviceCMYK(gs_color_space ** ppcspace,
gs_memory_t * pmem);
void gs_cspace_init_from(gs_color_space * pcsto,
const gs_color_space * pcsfrom);
void gs_cspace_assign(gs_color_space * pdest, const gs_color_space * psrc);
void gs_cspace_release(gs_color_space * pcs);
gs_color_space_index gs_color_space_get_index(const gs_color_space *);
int gs_color_space_num_components(const gs_color_space *);
bool gs_color_space_equal(const gs_color_space *pcs1,
const gs_color_space *pcs2);
#ifndef gs_client_color_DEFINED
#  define gs_client_color_DEFINED
typedef struct gs_client_color_s gs_client_color;
#endif
void gs_color_space_restrict_color(gs_client_color *, const gs_color_space *);
const gs_color_space *gs_cspace_base_space(const gs_color_space * pcspace);
#define gs_color_space_indexed_base_space(pcspace)\
gs_cspace_base_space(pcspace)
#endif