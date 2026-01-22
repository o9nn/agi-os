#ifndef gstrap_INCLUDED
# define gstrap_INCLUDED
#include "gsparam.h"
#ifndef gx_path_DEFINED
# define gx_path_DEFINED
typedef struct gx_path_s gx_path;
#endif
typedef enum {
tp_Center,
tp_Choke,
tp_Spread,
tp_Normal
} gs_trap_placement_t;
#define gs_trap_placement_names\
"Center", "Choke", "Spread", "Normal"
typedef struct gs_trap_params_s {
float BlackColorLimit;
float BlackDensityLimit;
float BlackWidth;
bool Enabled;
bool ImageInternalTrapping;
bool ImagemaskTrapping;
int ImageResolution;
bool ImageToObjectTrapping;
gs_trap_placement_t ImageTrapPlacement;
float SlidingTrapLimit;
float StepLimit;
float TrapColorScaling;
float TrapWidth;
} gs_trap_params_t;
typedef struct gs_trap_zone_s {
gs_trap_params_t params;
gx_path *zone;
} gs_trap_zone_t;
int gs_settrapparams(gs_trap_params_t * params, gs_param_list * list);
#endif