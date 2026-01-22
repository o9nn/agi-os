#ifndef gxfont1_INCLUDED
# define gxfont1_INCLUDED
#include "gstype1.h"
#include "gxfixed.h"
#ifndef gs_font_type1_DEFINED
# define gs_font_type1_DEFINED
typedef struct gs_font_type1_s gs_font_type1;
#endif
#define zone_table(size)\
struct {\
int count;\
float values[(size)*2];\
}
#define float_array(size)\
struct {\
int count;\
float values[size];\
}
#define stem_table(size)\
float_array(size)
#ifndef gs_type1_data_DEFINED
#define gs_type1_data_DEFINED
typedef struct gs_type1_data_s gs_type1_data;
#endif
typedef struct gs_type1_data_procs_s {
int (*glyph_data)(gs_font_type1 * pfont, gs_glyph glyph,
gs_glyph_data_t *pgd);
int (*subr_data)(gs_font_type1 * pfont, int subr_num, bool global,
gs_glyph_data_t *pgd);
int (*seac_data)(gs_font_type1 * pfont, int ccode,
gs_glyph * pglyph, gs_const_string *gstr, gs_glyph_data_t *pgd);
int (*push_values)(void *callback_data, const fixed *values,
int count);
int (*pop_value)(void *callback_data, fixed *value);
} gs_type1_data_procs_t;
struct gs_type1_data_s {
gs_type1_data_procs_t procs;
charstring_interpret_proc((*interpret));
void *proc_data;
gs_font_base *parent;
int lenIV;
uint subroutineNumberBias;
uint gsubrNumberBias;
long initialRandomSeed;
fixed defaultWidthX;
fixed nominalWidthX;
int BlueFuzz;
float BlueScale;
float BlueShift;
#define max_BlueValues 7
zone_table(max_BlueValues) BlueValues;
float ExpansionFactor;
bool ForceBold;
#define max_FamilyBlues 7
zone_table(max_FamilyBlues) FamilyBlues;
#define max_FamilyOtherBlues 5
zone_table(max_FamilyOtherBlues) FamilyOtherBlues;
int LanguageGroup;
#define max_OtherBlues 5
zone_table(max_OtherBlues) OtherBlues;
bool RndStemUp;
stem_table(1) StdHW;
stem_table(1) StdVW;
#define max_StemSnap 12
stem_table(max_StemSnap) StemSnapH;
stem_table(max_StemSnap) StemSnapV;
#define max_WeightVector 16
float_array(max_WeightVector) WeightVector;
};
#define gs_type1_data_s_DEFINED
struct gs_font_type1_s {
gs_font_base_common;
gs_type1_data data;
};
extern_st(st_gs_font_type1);
#define public_st_gs_font_type1() \
gs_public_st_suffix_add2_final(st_gs_font_type1, gs_font_type1,\
"gs_font_type1", font_type1_enum_ptrs, font_type1_reloc_ptrs,\
gs_font_finalize, st_gs_font_base, data.parent, data.proc_data)
font_proc_glyph_info(gs_type1_glyph_info);
int gs_type1_piece_codes( gs_font_type1 *pfont,
const gs_glyph_data_t *pgd, gs_char *chars);
#endif