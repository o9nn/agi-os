#ifndef gspcolor_INCLUDED
# define gspcolor_INCLUDED
#include "gsccolor.h"
#include "gsrefct.h"
#include "gsuid.h"
#ifndef gs_pattern_type_DEFINED
# define gs_pattern_type_DEFINED
typedef struct gs_pattern_type_s gs_pattern_type_t;
#endif
#define gs_pattern_template_common\
const gs_pattern_type_t *type;\
int PatternType; \
gs_uid uid;\
void *client_data
typedef struct gs_pattern_template_s {
gs_pattern_template_common;
} gs_pattern_template_t;
extern_st(st_pattern_template);
#define public_st_pattern_template() \
gs_public_st_ptrs2(st_pattern_template, gs_pattern_template_t,\
"gs_pattern_template_t", pattern_template_enum_ptrs,\
pattern_template_reloc_ptrs, uid.xvalues, client_data)
#define st_pattern_template_max_ptrs 2
#ifndef gs_pattern_instance_DEFINED
# define gs_pattern_instance_DEFINED
typedef struct gs_pattern_instance_s gs_pattern_instance_t;
#endif
#define gs_pattern_instance_common\
rc_header rc;\
\
const gs_pattern_type_t *type; \
gs_state *saved;\
gs_id pattern_id
struct gs_pattern_instance_s {
gs_pattern_instance_common;
};
extern_st(st_pattern_instance);
#define public_st_pattern_instance() \
gs_public_st_ptrs1(st_pattern_instance, gs_pattern_instance_t,\
"gs_pattern_instance_t", pattern_instance_enum_ptrs,\
pattern_instance_reloc_ptrs, saved)
#define st_pattern_instance_max_ptrs 1
int gs_setpattern(gs_state *, const gs_client_color *);
int gs_setpatternspace(gs_state *);
int gs_make_pattern(gs_client_color *, const gs_pattern_template_t *,
const gs_matrix *, gs_state *, gs_memory_t *);
const gs_pattern_template_t *gs_get_pattern(const gs_client_color *);
void gs_pattern_reference(gs_client_color * pcc, int delta);
#endif