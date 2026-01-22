#ifndef gdevsvga_INCLUDED
#  define gdevsvga_INCLUDED
dev_proc_close_device(svga_close);
dev_proc_map_rgb_color(svga_map_rgb_color);
dev_proc_map_color_rgb(svga_map_color_rgb);
dev_proc_fill_rectangle(svga_fill_rectangle);
dev_proc_copy_mono(svga_copy_mono);
dev_proc_copy_color(svga_copy_color);
dev_proc_get_params(svga_get_params);
dev_proc_put_params(svga_put_params);
dev_proc_get_bits(svga_get_bits);
dev_proc_copy_alpha(svga_copy_alpha);
typedef struct {
int width, height;
int mode;
} mode_info;
typedef struct gx_device_svga_s gx_device_svga;
struct gx_device_svga_s {
gx_device_common;
int (*get_mode) (void);
void (*set_mode) (int);
void (*set_page) (gx_device_svga * fbdev, int pnum, int wnum);
bool fixed_colors;
const mode_info *mode;
uint raster;
int current_page;
int wnum_read, wnum_write;
union {
struct {
void (*bios_set_page) (int, int);
int pn_shift;
} vesa;
struct {
int select_reg;
} atiw;
struct {
int et_model;
} tseng;
} info;
};
#define svga_color_device(procs, name, depth, maxv, dither, get_mode, set_mode, set_page) {\
std_device_color_body(gx_device_svga, &procs, name,\
640, 480,\
480 / PAGE_HEIGHT_INCHES, 480 / PAGE_HEIGHT_INCHES,\
depth, maxv, dither),\
{ 0 },		\
get_mode, set_mode, set_page,\
0 \
}
#define svga_device(procs, name, get_mode, set_mode, set_page)\
svga_color_device(procs, name, 8, 31, 4, get_mode, set_mode, set_page)
void svga_init_colors(gx_device *);
int svga_find_mode(gx_device *, const mode_info *);
int svga_open(gx_device *);
#endif