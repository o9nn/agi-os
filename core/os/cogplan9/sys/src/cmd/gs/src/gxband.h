#ifndef gxband_INCLUDED
#  define gxband_INCLUDED
#include "gxclio.h"
typedef struct gx_band_params_s {
bool page_uses_transparency;
int BandWidth;
int BandHeight;
long BandBufferSpace;
} gx_band_params_t;
#define BAND_PARAMS_INITIAL_VALUES 0, 0, 0
typedef struct gx_colors_used_s {
gx_color_index or;
bool slow_rop;
} gx_colors_used_t;
#define PAGE_INFO_NUM_COLORS_USED 50
typedef struct gx_band_page_info_s {
char cfname[gp_file_name_sizeof];
clist_file_ptr cfile;
char bfname[gp_file_name_sizeof];
clist_file_ptr bfile;
uint tile_cache_size;
long bfile_end_pos;
gx_band_params_t band_params;
int scan_lines_per_colors_used;
gx_colors_used_t band_colors_used[PAGE_INFO_NUM_COLORS_USED];
} gx_band_page_info_t;
#define PAGE_INFO_NULL_VALUES\
{ 0 }, 0, { 0 }, 0, 0, 0, { BAND_PARAMS_INITIAL_VALUES },\
0x3fffffff, { { 0 } }
#define page_cfile page_info.cfile
#define page_cfname page_info.cfname
#define page_bfile page_info.bfile
#define page_bfname page_info.bfname
#define page_tile_cache_size page_info.tile_cache_size
#define page_bfile_end_pos page_info.bfile_end_pos
#define page_band_height page_info.band_params.BandHeight
#endif