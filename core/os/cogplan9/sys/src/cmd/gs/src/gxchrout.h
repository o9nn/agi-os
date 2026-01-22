#ifndef gxchrout_INCLUDED
#  define gxchrout_INCLUDED
#ifndef gs_imager_state_DEFINED
#  define gs_imager_state_DEFINED
typedef struct gs_imager_state_s gs_imager_state;
#endif
double gs_char_flatness(const gs_imager_state *pis, floatp default_scale);
#endif