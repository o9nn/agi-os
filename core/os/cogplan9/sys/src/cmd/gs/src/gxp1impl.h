#ifndef gxp1impl_INCLUDED
#  define gxp1impl_INCLUDED
dev_color_proc_fill_rectangle(gx_dc_pattern_fill_rectangle);
dev_color_proc_fill_rectangle(gx_dc_pure_masked_fill_rect);
dev_color_proc_fill_rectangle(gx_dc_binary_masked_fill_rect);
dev_color_proc_fill_rectangle(gx_dc_colored_masked_fill_rect);
int gx_pattern_load(gx_device_color *, const gs_imager_state *,
gx_device *, gs_color_select_t);
pattern_proc_remap_color(gs_pattern1_remap_color);
#endif