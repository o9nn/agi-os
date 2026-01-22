#ifndef gswts_INCLUDED
# define gswts_INCLUDED
#ifndef gs_wts_screen_enum_t_DEFINED
# define gs_wts_screen_enum_t_DEFINED
typedef struct gs_wts_screen_enum_s gs_wts_screen_enum_t;
#endif
typedef struct gx_wts_cell_params_s gx_wts_cell_params_t;
struct gx_wts_cell_params_s {
wts_screen_type t;
int width;
int height;
double ufast;
double vfast;
double uslow;
double vslow;
};
gx_wts_cell_params_t *
wts_pick_cell_size(gs_screen_halftone *ph, const gs_matrix *pmat);
gs_wts_screen_enum_t *
gs_wts_screen_enum_new(gx_wts_cell_params_t *wcp);
int
gs_wts_screen_enum_currentpoint(gs_wts_screen_enum_t *wse, gs_point *ppt);
int
gs_wts_screen_enum_next(gs_wts_screen_enum_t *wse, floatp value);
int
wts_sort_blue(gs_wts_screen_enum_t *wse);
int
wts_sort_cell(gs_wts_screen_enum_t *wse);
wts_screen_t *
wts_screen_from_enum(const gs_wts_screen_enum_t *wse);
void
gs_wts_free_enum(gs_wts_screen_enum_t *wse);
void
gs_wts_free_screen(wts_screen_t *wts);
#endif