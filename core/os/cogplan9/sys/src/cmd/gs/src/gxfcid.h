#ifndef gxfcid_INCLUDED
# define gxfcid_INCLUDED
#include "gxcid.h"
#include "gxfont.h"
#include "gxfont42.h"
#define MAX_GDBytes 4
typedef struct gs_font_cid_data_s {
gs_cid_system_info_t CIDSystemInfo;
int CIDCount;
int GDBytes;
} gs_font_cid_data;
extern_st(st_gs_font_cid_data);
#define public_st_gs_font_cid_data() \
gs_public_st_suffix_add0_final(st_gs_font_cid_data,\
gs_font_cid_data, "gs_font_cid_data",\
font_cid_data_enum_ptrs, font_cid_data_reloc_ptrs,\
gs_font_finalize, st_cid_system_info)
#define st_gs_font_cid_data_num_ptrs\
st_cid_system_info_num_ptrs
#ifndef gs_font_type1_DEFINED
# define gs_font_type1_DEFINED
typedef struct gs_font_type1_s gs_font_type1;
#endif
#ifndef gs_font_cid0_DEFINED
# define gs_font_cid0_DEFINED
typedef struct gs_font_cid0_s gs_font_cid0;
#endif
#define MAX_FDBytes 4
typedef struct gs_font_cid0_data_s {
gs_font_cid_data common;
ulong CIDMapOffset;
gs_font_type1 **FDArray;
uint FDArray_size;
int FDBytes;
int (*glyph_data)(gs_font_base *, gs_glyph, gs_glyph_data_t *, int *);
void *proc_data;
} gs_font_cid0_data;
struct gs_font_cid0_s {
gs_font_base_common;
gs_font_cid0_data cidata;
};
extern_st(st_gs_font_cid0);
#define public_st_gs_font_cid0() \
gs_public_st_composite_use_final(st_gs_font_cid0,\
gs_font_cid0, "gs_font_cid0",\
font_cid0_enum_ptrs, font_cid0_reloc_ptrs, gs_font_finalize)
#define st_gs_font_cid0_max_ptrs\
(st_gs_font_max_ptrs + st_gs_font_cid_data_num_ptrs + 2)
extern_st(st_gs_font_type1_ptr_element);
typedef struct gs_font_cid1_data_s {
gs_cid_system_info_t CIDSystemInfo;
} gs_font_cid1_data;
typedef struct gs_font_cid1_s {
gs_font_base_common;
gs_font_cid1_data cidata;
} gs_font_cid1;
extern_st(st_gs_font_cid1);
#define public_st_gs_font_cid1() \
gs_public_st_composite_use_final(st_gs_font_cid1,\
gs_font_cid1, "gs_font_cid1",\
font_cid1_enum_ptrs, font_cid1_reloc_ptrs, gs_font_finalize)
#define st_gs_font_cid1_max_ptrs\
(st_gs_font_max_ptrs + st_cid_system_info_num_ptrs)
#ifndef gs_font_cid2_DEFINED
# define gs_font_cid2_DEFINED
typedef struct gs_font_cid2_s gs_font_cid2;
#endif
typedef struct gs_font_cid2_data_s {
gs_font_cid_data common;
int MetricsCount;
int (*CIDMap_proc)(gs_font_cid2 *, gs_glyph);
struct o_ {
int (*get_outline)(gs_font_type42 *, uint, gs_glyph_data_t *);
int (*get_metrics)(gs_font_type42 *, uint, int, float [4]);
} orig_procs;
} gs_font_cid2_data;
struct gs_font_cid2_s {
gs_font_type42_common;
gs_font_cid2_data cidata;
};
extern_st(st_gs_font_cid2);
#define public_st_gs_font_cid2() \
gs_public_st_composite_use_final(st_gs_font_cid2,\
gs_font_cid2, "gs_font_cid2",\
font_cid2_enum_ptrs, font_cid2_reloc_ptrs, gs_font_finalize)
#define st_gs_font_cid2_max_ptrs\
(st_gs_font_type42_max_ptrs + st_gs_font_cid_data_num_ptrs)
const gs_cid_system_info_t *gs_font_cid_system_info(const gs_font *);
font_proc_enumerate_glyph(gs_font_cid0_enumerate_glyph);
bool gs_is_CIDSystemInfo_compatible(const gs_cid_system_info_t *info0,
const gs_cid_system_info_t *info1);
const gs_font *gs_cid0_indexed_font(const gs_font *, int);
bool gs_cid0_has_type2(const gs_font *font);
#endif