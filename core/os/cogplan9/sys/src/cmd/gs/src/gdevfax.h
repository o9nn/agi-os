#ifndef gdevfax_INCLUDED
#  define gdevfax_INCLUDED
#define X_DPI 204
#define Y_DPI 196
#define gx_fax_device_common\
int AdjustWidth
typedef struct gx_device_fax_s {
gx_device_common;
gx_prn_device_common;
gx_fax_device_common;
} gx_device_fax;
#define FAX_DEVICE_BODY(dtype, procs, dname, print_page)\
prn_device_std_body(dtype, procs, dname,\
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,\
X_DPI, Y_DPI,\
0, 0, 0, 0,	\
1, print_page),\
1
dev_proc_open_device(gdev_fax_open);
dev_proc_get_params(gdev_fax_get_params);
dev_proc_put_params(gdev_fax_put_params);
extern const gx_device_procs gdev_fax_std_procs;
void gdev_fax_init_state(stream_CFE_state *ss, const gx_device_fax *fdev);
void gdev_fax_init_fax_state(stream_CFE_state *ss,
const gx_device_fax *fdev);
int gdev_fax_print_strip(gx_device_printer * pdev, FILE * prn_stream,
const stream_template * temp, stream_state * ss,
int width, int row_first,
int row_end );
int gdev_fax_print_page(gx_device_printer *pdev, FILE *prn_stream,
stream_CFE_state *ss);
#endif