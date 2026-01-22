#ifndef gdevdevn_INCLUDED
# define gdevdevn_INCLUDED
#define GX_DEVICE_MAX_SEPARATIONS 16
#define MAX_DEVICE_PROCESS_COLORS 6
typedef const char * fixed_colorant_name;
typedef fixed_colorant_name * fixed_colorant_names_list;
typedef struct devn_separation_name_s {
int size;
byte * data;
} devn_separation_name;
typedef struct gs_separations_s {
int num_separations;
devn_separation_name names[GX_DEVICE_MAX_SEPARATIONS];
} gs_separations;
typedef int gs_separation_map[GX_DEVICE_MAX_SEPARATIONS];
typedef struct gs_devn_params_s {
int bitspercomponent;
fixed_colorant_names_list std_colorant_names;
int num_std_colorant_names;
int max_separations;
gs_separations separations;
int num_separation_order_names;
gs_separation_map separation_order_map;
} gs_devn_params_t;
typedef gs_devn_params_t gs_devn_params;
extern fixed_colorant_name DeviceCMYKComponents[];
#include "gsequivc.h"
void gray_cs_to_devn_cm(gx_device * dev, int * map, frac gray, frac out[]);
void rgb_cs_to_devn_cm(gx_device * dev, int * map,
const gs_imager_state *pis, frac r, frac g, frac b, frac out[]);
void cmyk_cs_to_devn_cm(gx_device * dev, int * map,
frac c, frac m, frac y, frac k, frac out[]);
#define NO_AUTO_SPOT_COLORS 0
#define ENABLE_AUTO_SPOT_COLORS	1
#define ALLOW_EXTRA_SPOT_COLORS 2
int devn_get_color_comp_index(const gx_device * dev,
gs_devn_params * pdevn_params, equivalent_cmyk_color_params * pequiv_colors,
const char * pname, int name_size, int component_type,
int auto_spot_colors);
int devn_get_params(gx_device * pdev, gs_param_list * plist,
gs_devn_params * pdevn_params,
equivalent_cmyk_color_params * pequiv_colors);
int devn_printer_put_params(gx_device * pdev, gs_param_list * plist,
gs_devn_params * pdevn_params,
equivalent_cmyk_color_params * pequiv_colors);
int devn_put_params(gx_device * pdev, gs_param_list * plist,
gs_devn_params * pdevn_params,
equivalent_cmyk_color_params * pequiv_colors);
int check_pcm_and_separation_names(const gx_device * dev,
const gs_devn_params * pparams, const char * pname,
int name_size, int component_type);
int repack_data(byte * source, byte * dest, int depth, int first_bit,
int bit_width, int npixel);
int bpc_to_depth(int ncomp, int bpc);
#endif