#ifndef gxwts_INCLUDED
# define gxwts_INCLUDED
typedef bits16 wts_screen_sample_t;
#ifndef wts_screen_t_DEFINED
# define wts_screen_t_DEFINED
typedef struct wts_screen_s wts_screen_t;
#endif
typedef enum {
WTS_SCREEN_RAT,
WTS_SCREEN_J,
WTS_SCREEN_H
} wts_screen_type;
struct wts_screen_s {
wts_screen_type type;
int cell_width;
int cell_height;
int cell_shift;
wts_screen_sample_t *samples;
};
typedef struct {
wts_screen_t base;
int pa;
int pb;
int pc;
int pd;
int XA;
int YA;
int XB;
int YB;
int XC;
int YC;
int XD;
int YD;
} wts_screen_j_t;
typedef struct {
wts_screen_t base;
double px;
double py;
int x1;
int y1;
} wts_screen_h_t;
int
wts_get_samples(const wts_screen_t *ws, int x, int y,
wts_screen_sample_t **samples, int *p_nsamples);
#endif