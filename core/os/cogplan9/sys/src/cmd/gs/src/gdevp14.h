#ifndef gdevp14_INCLUDED
# define gdevp14_INCLUDED
typedef enum {
DeviceGray = 0,
DeviceRGB = 1,
DeviceCMYK = 2
} pdf14_default_colorspace_t;
typedef struct pdf14_buf_s pdf14_buf;
typedef struct pdf14_ctx_s pdf14_ctx;
struct pdf14_buf_s {
pdf14_buf *saved;
bool isolated;
bool knockout;
byte alpha;
byte shape;
gs_blend_mode_t blend_mode;
bool has_alpha_g;
bool has_shape;
gs_int_rect rect;
int rowstride;
int planestride;
int n_chan;
int n_planes;
byte *data;
byte *transfer_fn;
gs_int_rect bbox;
};
struct pdf14_ctx_s {
pdf14_buf *stack;
pdf14_buf *maskbuf;
gs_memory_t *memory;
gs_int_rect rect;
bool additive;
int n_chan;
};
typedef struct pdf14_device_s {
gx_device_forward_common;
pdf14_ctx *ctx;
float opacity;
float shape;
float alpha;
gs_blend_mode_t blend_mode;
const gx_color_map_procs *(*save_get_cmap_procs)(const gs_imager_state *,
const gx_device *);
gx_device_color_info saved_clist_color_info;
} pdf14_device;
int gs_pdf14_device_push(gs_memory_t *mem, gs_imager_state * pis,
gx_device * * pdev, gx_device * target);
int send_pdf14trans(gs_imager_state * pis, gx_device * dev,
gx_device * * pcdev, gs_pdf14trans_params_t * pparams, gs_memory_t * mem);
#endif