#ifndef gsht_INCLUDED
#  define gsht_INCLUDED
typedef struct gs_screen_halftone_s {
float frequency;
float angle;
float (*spot_function) (floatp, floatp);
float actual_frequency;
float actual_angle;
} gs_screen_halftone;
#define st_screen_halftone_max_ptrs 0
typedef struct gs_colorscreen_halftone_s {
union _css {
gs_screen_halftone indexed[4];
struct _csc {
gs_screen_halftone red, green, blue, gray;
} colored;
} screens;
} gs_colorscreen_halftone;
#define st_colorscreen_halftone_max_ptrs 0
int gs_setscreen(gs_state *, gs_screen_halftone *);
int gs_currentscreen(const gs_state *, gs_screen_halftone *);
int gs_currentscreenlevels(const gs_state *);
typedef struct gs_screen_enum_s gs_screen_enum;
gs_screen_enum *gs_screen_enum_alloc(gs_memory_t *, client_name_t);
int gs_screen_init(gs_screen_enum *, gs_state *, gs_screen_halftone *);
int gs_screen_currentpoint(gs_screen_enum *, gs_point *);
int gs_screen_next(gs_screen_enum *, floatp);
int gs_screen_install(gs_screen_enum *);
#endif