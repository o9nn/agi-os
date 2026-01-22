#ifndef gdevddrw_INCLUDED
# define gdevddrw_INCLUDED
enum fill_trap_flags {
ftf_peak0 = 1,
ftf_peak1 = 2,
ftf_pseudo_rasterization = 4
};
int gx_fill_trapezoid_cf_fd(gx_device * dev, const gs_fixed_edge * left,
const gs_fixed_edge * right, fixed ybot, fixed ytop, int flags,
const gx_device_color * pdevc, gs_logical_operation_t lop);
int gx_fill_trapezoid_cf_nd(gx_device * dev, const gs_fixed_edge * left,
const gs_fixed_edge * right, fixed ybot, fixed ytop, int flags,
const gx_device_color * pdevc, gs_logical_operation_t lop);
#endif