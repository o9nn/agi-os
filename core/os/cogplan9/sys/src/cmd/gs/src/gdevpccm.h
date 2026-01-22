#ifndef gdevpccm_INCLUDED
#  define gdevpccm_INCLUDED
dev_proc_map_rgb_color(pc_4bit_map_rgb_color);
dev_proc_map_color_rgb(pc_4bit_map_color_rgb);
#define dci_pc_4bit dci_values(3, 4, 1, 1, 2, 2)
dev_proc_map_rgb_color(pc_8bit_map_rgb_color);
dev_proc_map_color_rgb(pc_8bit_map_color_rgb);
#define dci_pc_8bit dci_values(3, 8, 5, 5, 6, 6)
int pc_write_palette(gx_device *, uint, FILE *);
#endif