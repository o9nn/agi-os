#ifndef gdevbmp_INCLUDED
#  define gdevbmp_INCLUDED
#define X_DPI 72
#define Y_DPI 72
int write_bmp_header(gx_device_printer *pdev, FILE *file);
int write_bmp_separated_header(gx_device_printer *pdev, FILE *file);
dev_proc_map_rgb_color(bmp_map_16m_rgb_color);
dev_proc_map_color_rgb(bmp_map_16m_color_rgb);
#endif