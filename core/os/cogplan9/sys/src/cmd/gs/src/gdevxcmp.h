#ifndef gdevxcmp_INCLUDED
#  define gdevxcmp_INCLUDED
typedef struct x11_rgb_s {
gx_color_value rgb[3];
bool defined;
} x11_rgb_t;
typedef struct x11_color_s x11_color_t;
struct x11_color_s {
XColor color;
x11_color_t *next;
};
typedef ushort X_color_value;
#define X_max_color_value 0xffff
#if HaveStdCMap
typedef struct x11_cmap_values_s {
int cv_shift;
X_color_value nearest[64];
int pixel_shift;
} x11_cmap_values_t;
#endif
typedef struct x11_cman_s {
int num_rgb;
struct cmm_ {
X_color_value red, green, blue;
} color_mask, match_mask;
#if HaveStdCMap
struct {
XStandardColormap *map;
bool fast;
x11_cmap_values_t red, green, blue;
bool free_map;
} std_cmap;
#endif
struct cmc_ {
int size;
x11_rgb_t *values;
} color_to_rgb;
#define CUBE_INDEX(r,g,b) (((r) * xdev->color_info.dither_colors + (g)) * \
xdev->color_info.dither_colors + (b))
x_pixel *dither_ramp;
struct cmd_ {
int size;
x11_color_t **colors;
int shift;
int used;
int max_used;
} dynamic;
} x11_cman_t;
#endif