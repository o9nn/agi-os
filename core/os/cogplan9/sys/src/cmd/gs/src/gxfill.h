#ifndef gxfill_INCLUDED
# define gxfill_INCLUDED
#ifndef active_line_DEFINED
# define active_line_DEFINED
typedef struct active_line_s active_line;
#endif
struct active_line_s {
gs_fixed_point start;
gs_fixed_point end;
gs_fixed_point diff;
fixed y_fast_max;
fixed num_adjust;
#if ARCH_DIV_NEG_POS_TRUNCATES
# define SET_NUM_ADJUST(alp) \
(alp)->num_adjust =\
((alp)->diff.x >= 0 ? 0 : -(alp)->diff.y + fixed_epsilon)
# define ADD_NUM_ADJUST(num, alp) ((num) + (alp)->num_adjust)
# define MAX_MINUS_NUM_ADJUST(alp) ADD_NUM_ADJUST(max_fixed, alp)
#else
# define SET_NUM_ADJUST(alp) DO_NOTHING
# define ADD_NUM_ADJUST(num, alp) (num)
# define MAX_MINUS_NUM_ADJUST(alp) max_fixed
#endif
#define SET_AL_POINTS(alp, startp, endp)\
BEGIN\
(alp)->diff.y = (endp).y - (startp).y;\
(alp)->diff.x = (endp).x - (startp).x;\
SET_NUM_ADJUST(alp);\
(alp)->y_fast_max = MAX_MINUS_NUM_ADJUST(alp) /\
(((alp)->diff.x >= 0 ? (alp)->diff.x : -(alp)->diff.x) | 1) +\
(startp).y;\
(alp)->start = startp, (alp)->end = endp;\
END
#define AL_X_AT_Y(alp, yv)\
((yv) == (alp)->end.y ? (alp)->end.x :\
((yv) <= (alp)->y_fast_max ?\
ADD_NUM_ADJUST(((yv) - (alp)->start.y) * (alp)->diff.x, alp) / (alp)->diff.y :\
(INCR_EXPR(slow_x),\
fixed_mult_quo((alp)->diff.x, (yv) - (alp)->start.y, (alp)->diff.y))) +\
(alp)->start.x)
fixed x_current;
fixed x_next;
const segment *pseg;
int direction;
#define DIR_UP 1
#define DIR_HORIZONTAL 0
#define DIR_DOWN (-1)
bool monotonic_x;
bool monotonic_y;
gx_flattened_iterator fi;
bool more_flattened;
active_line *prev, *next;
active_line *alloc_next;
};
typedef struct fill_options_s {
bool pseudo_rasterization;
fixed ymin, ymax;
const gx_device_color * pdevc;
gs_logical_operation_t lop;
bool fill_direct;
fixed fixed_flat;
bool fill_by_trapezoids;
fixed adjust_left, adjust_right;
fixed adjust_below, adjust_above;
gx_device *dev;
const gs_fixed_rect * pbox;
bool is_spotan;
int rule;
dev_proc_fill_rectangle((*fill_rect));
dev_proc_fill_trapezoid((*fill_trap));
} fill_options;
#ifndef line_list_DEFINED
# define line_list_DEFINED
typedef struct line_list_s line_list;
#endif
struct line_list_s {
gs_memory_t *memory;
active_line *active_area;
active_line *next_active;
active_line *limit;
int close_count;
active_line *y_list;
active_line *y_line;
active_line x_head;
#define x_list x_head.next
active_line *h_list0, *h_list1;
margin_set margin_set0, margin_set1;
margin *free_margin_list;
int local_margin_alloc_count;
int bbox_left, bbox_width;
int main_dir;
fixed y_break;
const fill_options * const fo;
#if arch_small_memory
# define MAX_LOCAL_ACTIVE 6
# define MAX_LOCAL_SECTION 50
#else
# define MAX_LOCAL_ACTIVE 20
# define MAX_LOCAL_SECTION 100
#endif
active_line local_active[MAX_LOCAL_ACTIVE];
margin local_margins[MAX_LOCAL_ACTIVE];
section local_section0[MAX_LOCAL_SECTION];
section local_section1[MAX_LOCAL_SECTION];
};
#define LOOP_FILL_RECTANGLE_DIRECT(fo, x, y, w, h)\
(FILL_DIRECT ?\
(fo)->fill_rect((fo)->dev, x, y, w, h, (fo)->pdevc->colors.pure) :\
gx_fill_rectangle_device_rop(x, y, w, h, (fo)->pdevc, (fo)->dev, (fo)->lop))
#ifdef DEBUG
struct stats_fill_s {
long
fill, fill_alloc, y_up, y_down, horiz, x_step, slow_x, iter, find_y,
band, band_step, band_fill, afill, slant, slant_shallow, sfill,
mq_cross, cross_slow, cross_low, order, slow_order;
};
typedef struct stats_fill_s stats_fill_t;
extern stats_fill_t stats_fill;
# define INCR(x) (++(stats_fill.x))
# define INCR_EXPR(x) INCR(x)
# define INCR_BY(x,n) (stats_fill.x += (n))
#else
# define INCR(x) DO_NOTHING
# define INCR_EXPR(x) discard(0)
# define INCR_BY(x,n) DO_NOTHING
#endif
#endif