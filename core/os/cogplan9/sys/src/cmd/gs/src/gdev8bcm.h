#ifndef gdev8bcm_INCLUDED
#  define gdev8bcm_INCLUDED
#define gx_8bit_map_size 323
#define gx_8bit_map_spreader 123
typedef struct gx_8bit_map_entry_s {
ushort rgb;
#define gx_8bit_no_rgb ((ushort)0xffff)
#define gx_8bit_rgb_key(r, g, b)\
(((r >> (gx_color_value_bits - 5)) << 10) +\
((g >> (gx_color_value_bits - 5)) << 5) +\
(b >> (gx_color_value_bits - 5)))
short index;
} gx_8bit_map_entry;
typedef struct gx_8bit_color_map_s {
int count;
int max_count;
gx_8bit_map_entry map[gx_8bit_map_size + 1];
} gx_8bit_color_map;
void gx_8bit_map_init(gx_8bit_color_map *, int);
int gx_8bit_map_rgb_color(const gx_8bit_color_map *, gx_color_value,
gx_color_value, gx_color_value);
#define gx_8bit_map_is_full(pcm)\
((pcm)->count == (pcm)->max_count)
int gx_8bit_add_rgb_color(gx_8bit_color_map *, gx_color_value,
gx_color_value, gx_color_value);
#endif