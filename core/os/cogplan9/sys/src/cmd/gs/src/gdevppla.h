#ifndef gdevppla_INCLUDED
#  define gdevppla_INCLUDED
int gdev_prn_set_procs_planar(gx_device *pdev);
int gdev_prn_open_planar(gx_device *pdev, bool upb);
int gdev_prn_get_params_planar(gx_device * pdev, gs_param_list * plist,
bool *pupb);
int gdev_prn_put_params_planar(gx_device * pdev, gs_param_list * plist,
bool *pupb);
int gdev_prn_create_buf_planar(gx_device **pbdev, gx_device *target,
const gx_render_plane_t *render_plane,
gs_memory_t *mem, bool for_band);
int gdev_prn_size_buf_planar(gx_device_buf_space_t *space,
gx_device *target,
const gx_render_plane_t *render_plane,
int height, bool for_band);
#endif