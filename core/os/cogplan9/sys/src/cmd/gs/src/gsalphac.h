#ifndef gsalphac_INCLUDED
# define gsalphac_INCLUDED
#include "gscompt.h"
typedef enum {
composite_Clear = 0,
composite_Copy,
composite_Sover,
composite_Sin,
composite_Sout,
composite_Satop,
composite_Dover,
composite_Din,
composite_Dout,
composite_Datop,
composite_Xor,
composite_PlusD,
composite_PlusL,
#define composite_last composite_PlusL
composite_Highlight,
#define compositerect_last composite_Highlight
composite_Dissolve
#define composite_op_last composite_Dissolve
} gs_composite_op_t;
typedef struct gs_composite_alpha_params_s {
gs_composite_op_t op;
float delta;
} gs_composite_alpha_params_t;
int gs_create_composite_alpha(gs_composite_t ** ppcte,
const gs_composite_alpha_params_t * params,
gs_memory_t * mem);
#endif