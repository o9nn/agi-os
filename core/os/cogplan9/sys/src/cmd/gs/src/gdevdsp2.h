#ifndef gdevdsp2_INCLUDED
#  define gdevdsp2_INCLUDED
typedef struct gx_device_display_s gx_device_display;
#define gx_device_display_common\
gx_device_memory *mdev;\
display_callback *callback;\
void *pHandle;\
int nFormat;\
void *pBitmap;\
unsigned long ulBitmapSize;\
int HWResolution_set;\
gs_devn_params devn_params;\
equivalent_cmyk_color_params equiv_cmyk_colors
struct gx_device_display_s {
gx_device_common;
gx_device_display_common;
};
extern_st(st_device_display);
#define public_st_device_display()	\
gs_public_st_composite_use_final(st_device_display, gx_device_display,\
"gx_device_display", display_enum_ptrs, display_reloc_ptrs,\
gx_device_finalize)
#endif