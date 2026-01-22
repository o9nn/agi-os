#ifndef gsdfilt_INCLUDED
# define gsdfilt_INCLUDED
#ifndef gs_device_filter_stack_DEFINED
# define gs_device_filter_stack_DEFINED
typedef struct gs_device_filter_stack_s gs_device_filter_stack_t;
#endif
#ifndef gs_device_filter_DEFINED
# define gs_device_filter_DEFINED
typedef struct gs_device_filter_s gs_device_filter_t;
#endif
struct gs_device_filter_s {
int (*push)(gs_device_filter_t *self, gs_memory_t *mem, gs_state *pgs,
gx_device **pdev, gx_device *target);
int (*prepop)(gs_device_filter_t *self, gs_memory_t *mem, gs_state *pgs,
gx_device *dev);
int (*postpop)(gs_device_filter_t *self, gs_memory_t *mem, gs_state *pgs,
gx_device *dev);
};
extern_st(st_gs_device_filter);
int gs_push_device_filter(gs_memory_t *mem, gs_state *pgs, gs_device_filter_t *df);
int gs_pop_device_filter(gs_memory_t *mem, gs_state *pgs);
int gs_clear_device_filters(gs_memory_t *mem, gs_state *pgs);
#endif