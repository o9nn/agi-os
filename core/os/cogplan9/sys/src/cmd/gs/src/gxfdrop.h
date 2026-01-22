#ifndef gxfdrop_INCLUDED
#  define gxfdrop_INCLUDED
#define ADJUST_SERIF 1
#define CHECK_SPOT_CONTIGUITY 1
#ifndef active_line_DEFINED
#  define active_line_DEFINED
typedef struct active_line_s active_line;
#endif
#ifndef line_list_DEFINED
#  define line_list_DEFINED
typedef struct line_list_s line_list;
#endif
typedef struct margin_s
{   int ibeg, iend;
struct margin_s *prev, *next;
} margin;
typedef struct section_s
{   short y0, y1;
#if ADJUST_SERIF && CHECK_SPOT_CONTIGUITY
short x0, x1;
#endif
} section;
typedef struct margin_set_s
{   fixed y;
margin *margin_list, *margin_touched;
section *sect;
} margin_set;
extern_st(st_section);
#define VD_SCALE 0.03
#define VD_RECT(x, y, w, h, c) vd_rect(int2fixed(x), int2fixed(y), int2fixed(x + w), int2fixed(y + h), 1, c)
#define VD_TRAP_COLOR RGB(0, 255, 255)
#define VD_MARG_COLOR RGB(255, 0, 0)
void init_section(section *sect, int i0, int i1);
void free_all_margins(line_list * ll);
int close_margins(gx_device * dev, line_list * ll, margin_set *ms);
int process_h_lists(line_list * ll, active_line * plp, active_line * flp, active_line * alp, fixed y0, fixed y1);
int margin_interior(line_list * ll, active_line * flp, active_line * alp, fixed y0, fixed y1);
int start_margin_set(gx_device * dev, line_list * ll, fixed y0);
int continue_margin_common(line_list * ll, margin_set * set, active_line * flp, active_line * alp, fixed y0, fixed y1);
#endif