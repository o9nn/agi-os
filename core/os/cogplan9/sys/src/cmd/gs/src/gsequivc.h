#ifndef gsequivc_INCLUDED
# define gsequivc_INCLUDED
typedef struct cmyk_color_s {
bool color_info_valid;
frac c;
frac m;
frac y;
frac k;
} cmyk_color;
typedef struct equivalent_cmyk_color_params_s {
bool all_color_info_valid;
cmyk_color color[GX_DEVICE_MAX_SEPARATIONS];
} equivalent_cmyk_color_params;
void update_spot_equivalent_cmyk_colors(gx_device * pdev,
const gs_state * pgs, gs_devn_params * pdevn_params,
equivalent_cmyk_color_params * pparams);
#endif