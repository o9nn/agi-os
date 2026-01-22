#include "unistd_.h"
#include <stdlib.h>
#include "gdevprn.h"
#include "gp.h"
#include "ijs.h"
#include "ijs_client.h"
int gdev_prn_maybe_realloc_memory(gx_device_printer *pdev,
gdev_prn_space_params *old_space,
int old_width, int old_height,
bool old_page_uses_transparency);
private dev_proc_open_device(gsijs_open);
private dev_proc_close_device(gsijs_close);
private dev_proc_output_page(gsijs_output_page);
private dev_proc_get_params(gsijs_get_params);
private dev_proc_put_params(gsijs_put_params);
private dev_proc_finish_copydevice(gsijs_finish_copydevice);
private const gx_device_procs gsijs_procs = {
gsijs_open,
NULL,
NULL,
gsijs_output_page,
gsijs_close,
gx_default_rgb_map_rgb_color,
gx_default_rgb_map_color_rgb,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
gsijs_get_params,
gsijs_put_params,
NULL,
NULL,
NULL,
NULL,
gx_page_device_get_page_device,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
gsijs_finish_copydevice
};
typedef struct gx_device_ijs_s gx_device_ijs;
struct gx_device_ijs_s {
gx_device_common;
gx_prn_device_common;
bool IjsUseOutputFD;
char IjsServer[gp_file_name_sizeof];
char *ColorSpace;
int ColorSpace_size;
int BitsPerSample;
char *DeviceManufacturer;
int DeviceManufacturer_size;
char *DeviceModel;
int DeviceModel_size;
char *IjsParams;
int IjsParams_size;
bool IjsTumble;
bool IjsTumble_set;
IjsClientCtx *ctx;
int ijs_version;
};
#define DEFAULT_DPI 74
gx_device_ijs gs_ijs_device =
{
prn_device_std_body(gx_device_ijs, gsijs_procs, "ijs",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
DEFAULT_DPI, DEFAULT_DPI,
0, 0, 0, 0,
24 , NULL ),
FALSE,
"",
NULL,
0,
8,
NULL,
0,
NULL,
0,
NULL,
0,
FALSE,
FALSE,
NULL,
0
};
private int gsijs_client_set_param(gx_device_ijs *ijsdev, const char *key,
const char *value);
private int gsijs_set_color_format(gx_device_ijs *ijsdev);
private int gsijs_read_int(gs_param_list *plist, gs_param_name pname,
int *pval, int min_value, int max_value, bool only_when_closed);
private int gsijs_read_bool(gs_param_list *plist, gs_param_name pname,
bool *pval, bool only_when_closed);
private int gsijs_read_string(gs_param_list * plist, gs_param_name pname,
char * str, uint size, bool safety, bool only_when_closed);
#define HPIJS_1_0_VERSION 29
private int
gsijs_parse_wxh (const char *val, int size, double *pw, double *ph)
{
char buf[256];
char *tail;
int i;
for (i = 0; i < size; i++)
if (val[i] == 'x')
break;
if (i + 1 >= size)
return IJS_ESYNTAX;
if (i >= sizeof(buf))
return IJS_EBUF;
memcpy (buf, val, i);
buf[i] = 0;
*pw = strtod (buf, &tail);
if (tail == buf)
return IJS_ESYNTAX;
if (size - i > sizeof(buf))
return IJS_EBUF;
memcpy (buf, val + i + 1, size - i - 1);
buf[size - i - 1] = 0;
*ph = strtod (buf, &tail);
if (tail == buf)
return IJS_ESYNTAX;
return 0;
}
private int
gsijs_set_generic_params_hpijs(gx_device_ijs *ijsdev)
{
char buf[256];
int code = 0;
if (ijsdev->IjsParams) {
code = gsijs_client_set_param(ijsdev, "IjsParams", ijsdev->IjsParams);
}
if (code == 0 && ijsdev->Duplex_set) {
int duplex_val;
duplex_val = ijsdev->Duplex ? (ijsdev->IjsTumble ? 1 : 2) : 0;
sprintf (buf, "%d", duplex_val);
code = gsijs_client_set_param(ijsdev, "Duplex", buf);
}
return code;
}
private int
gsijs_set_generic_params(gx_device_ijs *ijsdev)
{
char buf[256];
int code = 0;
int i, j;
char *value;
if (ijsdev->ijs_version == HPIJS_1_0_VERSION)
return gsijs_set_generic_params_hpijs(ijsdev);
value = NULL;
for (i=0, j=0; (j < ijsdev->IjsParams_size) && (i < sizeof(buf)-1); j++) {
char ch = ijsdev->IjsParams[j];
if (ch == '\\') {
j++;
buf[i++] = ijsdev->IjsParams[j];
}
else {
if (ch == '=') {
buf[i++] = '\0';
value = &buf[i];
}
else
buf[i++] = ch;
if (ch == ',') {
buf[i-1] = '\0';
if (value)
gsijs_client_set_param(ijsdev, buf, value);
i = 0;
value = NULL;
}
}
}
if (value)
code = gsijs_client_set_param(ijsdev, buf, value);
if (code == 0 && ijsdev->Duplex_set) {
code = gsijs_client_set_param(ijsdev, "PS:Duplex",
ijsdev->Duplex ? "true" : "false");
}
if (code == 0 && ijsdev->IjsTumble_set) {
code = gsijs_client_set_param(ijsdev, "PS:Tumble",
ijsdev->IjsTumble ? "true" :
"false");
}
return code;
}
private int
gsijs_set_margin_params_hpijs(gx_device_ijs *ijsdev)
{
char buf[256];
int code = 0;
if (code == 0) {
sprintf(buf, "%d", ijsdev->width);
code = gsijs_client_set_param(ijsdev, "Width", buf);
}
if (code == 0) {
sprintf(buf, "%d", ijsdev->height);
code = gsijs_client_set_param(ijsdev, "Height", buf);
}
if (code == 0) {
double printable_width, printable_height;
double printable_left, printable_top;
float m[4];
code = ijs_client_get_param(ijsdev->ctx, 0, "PrintableArea",
buf, sizeof(buf));
if (code == IJS_EUNKPARAM)
return 0;
else if (code >= 0) {
code = gsijs_parse_wxh(buf, code,
&printable_width, &printable_height);
}
if (code == 0) {
code = ijs_client_get_param(ijsdev->ctx, 0, "PrintableTopLeft",
buf, sizeof(buf));
if (code == IJS_EUNKPARAM)
return 0;
else if (code >= 0) {
code = gsijs_parse_wxh(buf, code,
&printable_left, &printable_top);
}
}
if (code == 0) {
m[0] = printable_left;
m[1] = ijsdev->MediaSize[1] * (1.0 / 72) -
printable_top - printable_height;
m[2] = ijsdev->MediaSize[0] * (1.0 / 72) -
printable_left - printable_width;
m[3] = printable_top;
gx_device_set_margins((gx_device *)ijsdev, m, true);
}
}
return code;
}
private int
gsijs_set_margin_params(gx_device_ijs *ijsdev)
{
char buf[256];
int code = 0;
int i, j;
char *value;
if (ijsdev->ijs_version == HPIJS_1_0_VERSION)
return gsijs_set_margin_params_hpijs(ijsdev);
value = NULL;
for (i=0, j=0; (j < ijsdev->IjsParams_size) && (i < sizeof(buf)-1); j++) {
char ch = ijsdev->IjsParams[j];
if (ch == '\\') {
j++;
buf[i++] = ijsdev->IjsParams[j];
}
else {
if (ch == '=') {
buf[i++] = '\0';
value = &buf[i];
}
else
buf[i++] = ch;
if (ch == ',') {
buf[i-1] = '\0';
if (value)
gsijs_client_set_param(ijsdev, buf, value);
i = 0;
value = NULL;
}
}
}
if (value)
code = gsijs_client_set_param(ijsdev, buf, value);
if (code == 0 && ijsdev->Duplex_set) {
code = gsijs_client_set_param(ijsdev, "Duplex",
ijsdev->Duplex ? "true" : "false");
}
if (code == 0 && ijsdev->IjsTumble_set) {
code = gsijs_client_set_param(ijsdev, "Tumble",
ijsdev->IjsTumble ? "true" :
"false");
}
if (code == 0) {
sprintf (buf, "%gx%g", ijsdev->MediaSize[0] * (1.0 / 72),
ijsdev->MediaSize[1] * (1.0 / 72));
code = ijs_client_set_param(ijsdev->ctx, 0, "PaperSize",
buf, strlen(buf));
}
if (code == 0) {
double printable_width, printable_height;
double printable_left, printable_top;
float m[4];
code = ijs_client_get_param(ijsdev->ctx, 0, "PrintableArea",
buf, sizeof(buf));
if (code == IJS_EUNKPARAM)
return 0;
else if (code >= 0) {
code = gsijs_parse_wxh (buf, code,
&printable_width, &printable_height);
}
if (code == 0) {
code = ijs_client_get_param(ijsdev->ctx, 0, "PrintableTopLeft",
buf, sizeof(buf));
if (code == IJS_EUNKPARAM)
return 0;
else if (code >= 0) {
code = gsijs_parse_wxh(buf, code,
&printable_left, &printable_top);
}
}
if (code == 0) {
m[0] = printable_left;
m[3] = printable_top;
m[2] = ijsdev->MediaSize[0] * (1.0 / 72) -
printable_left - printable_width;
m[1] = ijsdev->MediaSize[1] * (1.0 / 72) -
printable_top - printable_height;
gx_device_set_margins((gx_device *)ijsdev, m, true);
sprintf (buf, "%gx%g", printable_left, printable_top);
code = ijs_client_set_param(ijsdev->ctx, 0, "TopLeft",
buf, strlen(buf));
}
}
return code;
}
private int
gsijs_set_resolution(gx_device_ijs *ijsdev)
{
char buf[256];
int code;
floatp x_dpi, y_dpi;
int width = ijsdev->width;
int height = ijsdev->height;
bool save_is_open = ijsdev->is_open;
if (ijsdev->HWResolution[0] != DEFAULT_DPI ||
ijsdev->HWResolution[1] != DEFAULT_DPI) {
return 0;
}
code = ijs_client_get_param(ijsdev->ctx, 0, "Dpi",
buf, sizeof(buf));
if (code >= 0) {
int i;
for (i = 0; i < code; i++)
if (buf[i] == 'x')
break;
if (i == code) {
char *tail;
if (i == sizeof(buf))
code = IJS_EBUF;
buf[i] = 0;
x_dpi = y_dpi = strtod (buf, &tail);
if (tail == buf)
code = IJS_ESYNTAX;
} else {
double x, y;
code = gsijs_parse_wxh(buf, code, &x, &y);
x_dpi = x;
y_dpi = y;
}
}
if (code < 0) {
x_dpi = 72.0;
y_dpi = 72.0;
}
gx_device_set_resolution((gx_device *)ijsdev, x_dpi, y_dpi);
ijsdev->is_open = true;
code = gdev_prn_maybe_realloc_memory((gx_device_printer *)ijsdev,
&ijsdev->space_params, width, height,
ijsdev->page_uses_transparency);
ijsdev->is_open = save_is_open;
return code;
}
private int
gsijs_open(gx_device *dev)
{
gx_device_ijs *ijsdev = (gx_device_ijs *)dev;
int code;
char buf[256];
bool use_outputfd;
int fd = -1;
if (strlen(ijsdev->IjsServer) == 0) {
eprintf("ijs server not specified\n");
return gs_note_error(gs_error_ioerror);
}
use_outputfd = ijsdev->IjsUseOutputFD;
ijsdev->OpenOutputFile = use_outputfd;
code = gdev_prn_open(dev);
if (code < 0)
return code;
if (use_outputfd) {
fd = dup(fileno(ijsdev->file));
}
ijsdev->ctx = ijs_invoke_server(ijsdev->IjsServer);
if (ijsdev->ctx == (IjsClientCtx *)NULL) {
eprintf1("Can't start ijs server \042%s\042\n", ijsdev->IjsServer);
return gs_note_error(gs_error_ioerror);
}
ijsdev->ijs_version = ijs_client_get_version (ijsdev->ctx);
if (ijs_client_open(ijsdev->ctx) < 0) {
eprintf("Can't open ijs\n");
return gs_note_error(gs_error_ioerror);
}
if (ijs_client_begin_job(ijsdev->ctx, 0) < 0) {
eprintf("Can't begin ijs job 0\n");
ijs_client_close(ijsdev->ctx);
return gs_note_error(gs_error_ioerror);
}
if (use_outputfd) {
sprintf(buf, "%d", fd);
ijs_client_set_param(ijsdev->ctx, 0, "OutputFD", buf, strlen(buf));
close(fd);
} else {
ijs_client_set_param(ijsdev->ctx, 0, "OutputFile",
ijsdev->fname, strlen(ijsdev->fname));
}
if (code >= 0 && ijsdev->DeviceManufacturer)
code = ijs_client_set_param(ijsdev->ctx, 0, "DeviceManufacturer",
ijsdev->DeviceManufacturer,
strlen(ijsdev->DeviceManufacturer));
if (code >= 0 && ijsdev->DeviceModel)
code = ijs_client_set_param(ijsdev->ctx, 0, "DeviceModel",
ijsdev->DeviceModel,
strlen(ijsdev->DeviceModel));
if (code >= 0)
code = gsijs_set_generic_params(ijsdev);
if (code >= 0)
code = gsijs_set_resolution(ijsdev);
if (code >= 0)
code = gsijs_set_margin_params(ijsdev);
return code;
}
private int
gsijs_finish_copydevice(gx_device *dev, const gx_device *from_dev)
{
int code;
static const char rgb[] = "DeviceRGB";
gx_device_ijs *ijsdev = (gx_device_ijs *)dev;
code = gx_default_finish_copydevice(dev, from_dev);
if(code < 0)
return code;
if (!ijsdev->ColorSpace) {
ijsdev->ColorSpace = gs_malloc(ijsdev->memory, sizeof(rgb), 1,
"gsijs_finish_copydevice");
if (!ijsdev->ColorSpace)
return gs_note_error(gs_error_VMerror);
ijsdev->ColorSpace_size = sizeof(rgb);
memcpy(ijsdev->ColorSpace, rgb, sizeof(rgb));
}
return code;
}
private int
gsijs_close(gx_device *dev)
{
gx_device_ijs *ijsdev = (gx_device_ijs *)dev;
int code;
ijs_client_end_job(ijsdev->ctx, 0);
ijs_client_close(ijsdev->ctx);
ijs_client_begin_cmd(ijsdev->ctx, IJS_CMD_EXIT);
ijs_client_send_cmd_wait(ijsdev->ctx);
code = gdev_prn_close(dev);
if (ijsdev->IjsParams)
gs_free(dev->memory, ijsdev->IjsParams,
ijsdev->IjsParams_size, 1, "gsijs_read_string_malloc");
if (ijsdev->ColorSpace)
gs_free(dev->memory, ijsdev->ColorSpace,
ijsdev->ColorSpace_size, 1, "gsijs_read_string_malloc");
if (ijsdev->DeviceManufacturer)
gs_free(dev->memory, ijsdev->DeviceManufacturer,
ijsdev->DeviceManufacturer_size, 1, "gsijs_read_string_malloc");
if (ijsdev->DeviceModel)
gs_free(dev->memory, ijsdev->DeviceModel,
ijsdev->DeviceModel_size, 1, "gsijs_read_string_malloc");
ijsdev->IjsParams = NULL;
ijsdev->IjsParams_size = 0;
ijsdev->DeviceManufacturer = NULL;
ijsdev->DeviceManufacturer_size = 0;
ijsdev->DeviceModel = NULL;
ijsdev->DeviceModel_size = 0;
return code;
}
private int
gsijs_raster_width(gx_device *pdev)
{
int width = pdev->width;
gs_matrix imat;
float xscale;
int right, offset, end;
(*dev_proc(pdev, get_initial_matrix)) (pdev, &imat);
xscale = imat.xx * 72.0;
right = (int)(dev_r_margin(pdev) * xscale);
offset = (int)(dev_x_offset(pdev) * xscale);
end = offset + width - right;
return min(width, end);
}
private int ijs_all_white(unsigned char *data, int size)
{
int clean = 1;
int i;
for (i = 0; i < size; i++)
{
if (data[i] != 0xFF)
{
clean = 0;
break;
}
}
return clean;
}
private int
gsijs_output_page(gx_device *dev, int num_copies, int flush)
{
gx_device_ijs *ijsdev = (gx_device_ijs *)dev;
gx_device_printer *pdev = (gx_device_printer *)dev;
int raster = gdev_prn_raster(pdev);
int ijs_width, ijs_height;
int row_bytes;
int n_chan = pdev->color_info.num_components;
unsigned char *data;
char buf[256];
double xres = pdev->HWResolution[0];
double yres = pdev->HWResolution[1];
int code = 0;
int endcode = 0;
int status = 0;
int i, y;
if ((data = gs_alloc_bytes(pdev->memory, raster, "gsijs_output_page"))
== (unsigned char *)NULL)
return gs_note_error(gs_error_VMerror);
ijs_height = gdev_prn_print_scan_lines(dev);
if (ijsdev->ijs_version == HPIJS_1_0_VERSION) {
ijs_width = pdev->width;
} else {
ijs_width = gsijs_raster_width(dev);
}
row_bytes = (ijs_width * pdev->color_info.depth + 7) >> 3;
sprintf(buf, "%d", n_chan);
gsijs_client_set_param(ijsdev, "NumChan", buf);
sprintf(buf, "%d", ijsdev->BitsPerSample);
gsijs_client_set_param(ijsdev, "BitsPerSample", buf);
strcpy(buf, (n_chan == 4) ? "DeviceCMYK" :
((n_chan == 3) ? "DeviceRGB" : "DeviceGray"));
gsijs_client_set_param(ijsdev, "ColorSpace", buf);
if (ijsdev->ijs_version != HPIJS_1_0_VERSION) {
sprintf(buf, "%d", ijs_width);
gsijs_client_set_param(ijsdev, "Width", buf);
sprintf(buf, "%d", ijs_height);
gsijs_client_set_param(ijsdev, "Height", buf);
}
sprintf(buf, "%gx%g", xres, yres);
gsijs_client_set_param(ijsdev, "Dpi", buf);
for (i=0; i<num_copies; i++) {
unsigned char *actual_data;
ijs_client_begin_cmd (ijsdev->ctx, IJS_CMD_BEGIN_PAGE);
status = ijs_client_send_cmd_wait(ijsdev->ctx);
for (y = 0; y < ijs_height; y++) {
code = gdev_prn_get_bits(pdev, y, data, &actual_data);
if (code < 0)
break;
if (ijsdev->ijs_version == HPIJS_1_0_VERSION &&
ijs_all_white(actual_data, row_bytes))
status = ijs_client_send_data_wait(ijsdev->ctx, 0, NULL, 0);
else
status = ijs_client_send_data_wait(ijsdev->ctx, 0,
(char *)actual_data, row_bytes);
if (status)
break;
}
ijs_client_begin_cmd(ijsdev->ctx, IJS_CMD_END_PAGE);
status = ijs_client_send_cmd_wait(ijsdev->ctx);
}
gs_free_object(pdev->memory, data, "gsijs_output_page");
endcode = (pdev->buffer_space && !pdev->is_async_renderer ?
clist_finish_page(dev, flush) : 0);
if (endcode < 0)
return endcode;
if (code < 0)
return endcode;
if (status < 0)
return gs_note_error(gs_error_ioerror);
code = gx_finish_output_page(dev, num_copies, flush);
return code;
}
private int
gsijs_get_params(gx_device *dev, gs_param_list *plist)
{
gx_device_ijs *ijsdev = (gx_device_ijs *)dev;
gs_param_string gps;
int code = gdev_prn_get_params(dev, plist);
if (code >= 0) {
param_string_from_transient_string(gps, ijsdev->IjsServer);
code = param_write_string(plist, "IjsServer", &gps);
}
if (code >= 0) {
if (ijsdev->DeviceManufacturer) {
param_string_from_transient_string(gps,
ijsdev->DeviceManufacturer);
code = param_write_string(plist, "DeviceManufacturer", &gps);
} else {
code = param_write_null(plist, "DeviceManufacturer");
}
}
if (code >= 0) {
if (ijsdev->DeviceModel) {
param_string_from_transient_string(gps, ijsdev->DeviceModel);
code = param_write_string(plist, "DeviceModel", &gps);
} else {
code = param_write_null(plist, "DeviceModel");
}
}
if (code >= 0) {
if (ijsdev->IjsParams) {
param_string_from_transient_string(gps, ijsdev->IjsParams);
code = param_write_string(plist, "IjsParams", &gps);
} else {
code = param_write_null(plist, "IjsParams");
}
}
if (code >= 0)
code = param_write_int(plist, "BitsPerSample", &ijsdev->BitsPerSample);
if (code >= 0)
code = param_write_bool(plist, "IjsUseOutputFD",
&ijsdev->IjsUseOutputFD);
if (code >= 0) {
if (ijsdev->IjsTumble_set) {
code = param_write_bool(plist, "Tumble", &ijsdev->IjsTumble);
} else {
code = param_write_null(plist, "Tumble");
}
}
return code;
}
private int
gsijs_read_int(gs_param_list *plist, gs_param_name pname, int *pval,
int min_value, int max_value, bool only_when_closed)
{
int code = 0;
int new_value;
switch (code = param_read_int(plist, pname, &new_value)) {
case 0:
if (only_when_closed && (new_value != *pval)) {
code = gs_error_rangecheck;
goto e;
}
if ((new_value >= min_value) && (new_value <= max_value)) {
*pval = new_value;
break;
}
code = gs_note_error(gs_error_rangecheck);
goto e;
default:
if (param_read_null(plist, pname) == 0)
return 1;
e:param_signal_error(plist, pname, code);
case 1:
;
}
return code;
}
private int
gsijs_read_bool(gs_param_list *plist, gs_param_name pname, bool *pval,
bool only_when_closed)
{
int code = 0;
bool new_value;
switch (code = param_read_bool(plist, pname, &new_value)) {
case 0:
if (only_when_closed && (new_value != *pval)) {
code = gs_error_rangecheck;
goto e;
}
*pval = new_value;
break;
default:
if (param_read_null(plist, pname) == 0) {
return 1;
}
e:param_signal_error(plist, pname, code);
case 1:
;
}
return code;
}
private int
gsijs_read_string(gs_param_list *plist, gs_param_name pname, char *str,
uint size, bool safety, bool only_when_closed)
{
int code;
gs_param_string new_value;
int differs;
switch (code = param_read_string(plist, pname, &new_value)) {
case 0:
differs = bytes_compare(new_value.data, new_value.size,
(const byte *)str, strlen(str));
if (safety && differs) {
code = gs_error_invalidaccess;
goto e;
}
if (only_when_closed && differs) {
code = gs_error_rangecheck;
goto e;
}
if (new_value.size < size) {
strncpy(str, (const char *)new_value.data, new_value.size);
str[new_value.size+1] = '\0';
break;
}
code = gs_note_error(gs_error_rangecheck);
goto e;
default:
if (param_read_null(plist, pname) == 0)
return 1;
e:param_signal_error(plist, pname, code);
case 1:
;
}
return code;
}
private int
gsijs_read_string_malloc(gs_param_list *plist, gs_param_name pname, char **str,
int *size, bool only_when_closed)
{
int code;
gs_param_string new_value;
int differs;
switch (code = param_read_string(plist, pname, &new_value)) {
case 0:
differs = bytes_compare(new_value.data, new_value.size,
(const byte *)(*str ? *str : ""),
*str ? strlen(*str) : 0);
if (only_when_closed && differs) {
code = gs_error_rangecheck;
goto e;
}
if (new_value.size + 1 != *size) {
if (*str)
gs_free(plist->memory, *str, *size, 1,
"gsijs_read_string_malloc");
*str = NULL;
*size = 0;
}
if (*str == NULL)
*str = gs_malloc(plist->memory, new_value.size + 1, 1,
"gsijs_read_string_malloc");
if (*str == NULL) {
code = gs_note_error(gs_error_VMerror);
goto e;
}
*size = new_value.size + 1;
strncpy(*str, (const char *)new_value.data, new_value.size);
(*str)[new_value.size] = '\0';
break;
default:
if (param_read_null(plist, pname) == 0)
return 1;
e:param_signal_error(plist, pname, code);
case 1:
;
}
return code;
}
private int
gsijs_put_params(gx_device *dev, gs_param_list *plist)
{
gx_device_ijs *ijsdev = (gx_device_ijs *)dev;
int code = 0;
bool is_open = dev->is_open;
if (ijsdev->Duplex_set < 0) {
ijsdev->Duplex = 1;
ijsdev->Duplex_set = 0;
}
if (code >= 0)
code = gsijs_read_string(plist, "IjsServer",
ijsdev->IjsServer, sizeof(ijsdev->IjsServer),
dev->LockSafetyParams, is_open);
if (code >= 0)
code = gsijs_read_string_malloc(plist, "DeviceManufacturer",
&ijsdev->DeviceManufacturer, &ijsdev->DeviceManufacturer_size,
is_open);
if (code >= 0)
code = gsijs_read_string_malloc(plist, "DeviceModel",
&ijsdev->DeviceModel, &ijsdev->DeviceModel_size,
is_open);
if (code >= 0)
code = gsijs_read_string_malloc(plist, "IjsParams",
&(ijsdev->IjsParams), &(ijsdev->IjsParams_size), is_open);
if (code >= 0)
code = gsijs_read_int(plist, "BitsPerSample", &ijsdev->BitsPerSample,
1, 16, is_open);
if (code >= 0)
code = gsijs_read_bool(plist, "IjsUseOutputFD",
&ijsdev->IjsUseOutputFD, is_open);
if (code >= 0) {
code = gsijs_read_string_malloc(plist, "ProcessColorModel",
&ijsdev->ColorSpace, &ijsdev->ColorSpace_size, is_open);
}
if (code >= 0) {
code = gsijs_read_bool(plist, "Tumble", &ijsdev->IjsTumble, false);
if (code == 0)
ijsdev->IjsTumble_set = true;
}
if (code >= 0)
code = gsijs_set_color_format(ijsdev);
if (code >= 0)
code = gdev_prn_put_params(dev, plist);
if (code >= 0 && is_open) {
code = gsijs_set_generic_params(ijsdev);
if (code >= 0)
code = gsijs_set_margin_params(ijsdev);
if (code < 0)
return gs_note_error(gs_error_ioerror);
}
return code;
}
private int
gsijs_client_set_param(gx_device_ijs *ijsdev, const char *key,
const char *value)
{
int code = ijs_client_set_param(ijsdev->ctx, 0 ,
key, value, strlen(value));
if (code < 0)
dprintf2("ijs: Can't set parameter %s=%s\n", key, value);
return code;
}
private int
gsijs_set_color_format(gx_device_ijs *ijsdev)
{
gx_device_color_info dci = ijsdev->color_info;
int components;
int bpc = ijsdev->BitsPerSample;
int maxvalue;
const char *ColorSpace = ijsdev->ColorSpace;
if (ColorSpace == NULL)
ColorSpace = "DeviceRGB";
if (!strcmp (ColorSpace, "DeviceGray")) {
components = 1;
if (bpc == 1) {
ijsdev->procs.map_rgb_color = gx_default_w_b_map_rgb_color;
ijsdev->procs.map_color_rgb = gx_default_w_b_map_color_rgb;
} else {
ijsdev->procs.map_rgb_color = gx_default_gray_map_rgb_color;
ijsdev->procs.map_color_rgb = gx_default_gray_map_color_rgb;
}
ijsdev->procs.encode_color = gx_default_gray_fast_encode;
ijsdev->procs.decode_color = gx_default_decode_color;
dci.polarity = GX_CINFO_POLARITY_ADDITIVE;
dci.gray_index = 0;
} else if (!strcmp (ColorSpace, "DeviceRGB")) {
components = 3;
ijsdev->procs.map_rgb_color = gx_default_rgb_map_rgb_color;
ijsdev->procs.map_color_rgb = gx_default_rgb_map_color_rgb;
ijsdev->procs.encode_color = gx_default_rgb_map_rgb_color;
ijsdev->procs.decode_color = gx_default_rgb_map_color_rgb;
dci.polarity = GX_CINFO_POLARITY_ADDITIVE;
dci.gray_index = GX_CINFO_COMP_NO_INDEX;
} else if (!strcmp (ColorSpace, "DeviceCMYK")) {
components = 4;
ijsdev->procs.map_cmyk_color = cmyk_8bit_map_cmyk_color;
ijsdev->procs.map_color_rgb = cmyk_8bit_map_color_rgb;
ijsdev->procs.encode_color = cmyk_8bit_map_cmyk_color;
ijsdev->procs.decode_color = gx_default_decode_color;
dci.polarity = GX_CINFO_POLARITY_SUBTRACTIVE;
dci.gray_index = 3;
} else {
return -1;
}
maxvalue = (1 << bpc) - 1;
dci.max_components = components;
dci.num_components = components;
dci.depth = bpc * components;
dci.max_gray = maxvalue;
dci.max_color = components > 1 ? maxvalue : 0;
dci.dither_grays = maxvalue+1;
dci.dither_colors = components > 1 ? maxvalue+1 : 0;
dci.separable_and_linear = GX_CINFO_SEP_LIN;
dci.cm_name = ColorSpace;
ijsdev->color_info = dci;
set_linear_color_bits_mask_shift((gx_device *)ijsdev);
return 0;
}