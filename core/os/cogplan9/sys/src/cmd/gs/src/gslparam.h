#ifndef gslparam_INCLUDED
# define gslparam_INCLUDED
typedef enum {
gs_cap_butt = 0,
gs_cap_round = 1,
gs_cap_square = 2,
gs_cap_triangle = 3,
gs_cap_unknown = 4
} gs_line_cap;
#define gs_line_cap_max 3
typedef enum {
gs_join_miter = 0,
gs_join_round = 1,
gs_join_bevel = 2,
gs_join_none = 3,
gs_join_triangle = 4,
gs_join_unknown = 5
} gs_line_join;
#define gs_line_join_max 4
#endif