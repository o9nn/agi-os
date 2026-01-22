#include "math_.h"
#include "gdevprn.h"
#include "gsparam.h"
#include "gstypes.h"
#include "gxdcconv.h"
#include "gdevdevn.h"
#include "gsequivc.h"
#include "gzstate.h"
#include "gsstate.h"
#include "gscspace.h"
#include "gxcspace.h"
private void capture_spot_equivalent_cmyk_colors(gx_device * pdev,
const gs_state * pgs, const gs_client_color * pcc,
const gs_color_space * pcs, int sep_num,
equivalent_cmyk_color_params * pparams);
#define compare_color_names(name, name_size, str, str_size) \
(name_size == str_size && \
(strncmp((const char *)name, (const char *)str, name_size) == 0))
private void
update_Separation_spot_equivalent_cmyk_colors(gx_device * pdev,
const gs_state * pgs, const gs_color_space * pcs,
gs_devn_params * pdevn_params,
equivalent_cmyk_color_params * pparams)
{
int i;
for (i = 0; i < pdevn_params->separations.num_separations; i++) {
if (pparams->color[i].color_info_valid == false) {
const devn_separation_name * dev_sep_name =
&(pdevn_params->separations.names[i]);
unsigned int cs_sep_name_size;
unsigned char * pcs_sep_name;
pcs->params.separation.get_colorname_string
(pdev->memory, pcs->params.separation.sep_name, &pcs_sep_name,
&cs_sep_name_size);
if (compare_color_names(dev_sep_name->data, dev_sep_name->size,
pcs_sep_name, cs_sep_name_size)) {
gs_color_space temp_cs = *pcs;
gs_client_color client_color;
temp_cs.params.separation.use_alt_cspace = true;
client_color.paint.values[0] = 1.0;
capture_spot_equivalent_cmyk_colors(pdev, pgs, &client_color,
&temp_cs, i, pparams);
break;
}
}
}
}
private void
update_DeviceN_spot_equivalent_cmyk_colors(gx_device * pdev,
const gs_state * pgs, const gs_color_space * pcs,
gs_devn_params * pdevn_params,
equivalent_cmyk_color_params * pparams)
{
int i;
unsigned int j;
unsigned int cs_sep_name_size;
unsigned char * pcs_sep_name;
for (j = 0; j < pcs->params.device_n.num_components; j++) {
pcs->params.device_n.get_colorname_string
(pdev->memory, pcs->params.device_n.names[j],
&pcs_sep_name, &cs_sep_name_size);
if (compare_color_names("None", 4, pcs_sep_name, cs_sep_name_size))
return;
}
for (i = 0; i < pdevn_params->separations.num_separations; i++) {
if (pparams->color[i].color_info_valid == false) {
const devn_separation_name * dev_sep_name =
&(pdevn_params->separations.names[i]);
for (j = 0; j < pcs->params.device_n.num_components; j++) {
pcs->params.device_n.get_colorname_string
(pdev->memory, pcs->params.device_n.names[j], &pcs_sep_name,
&cs_sep_name_size);
if (compare_color_names(dev_sep_name->data, dev_sep_name->size,
pcs_sep_name, cs_sep_name_size)) {
gs_color_space temp_cs = *pcs;
gs_client_color client_color;
memset(&client_color, 0, sizeof(client_color));
temp_cs.params.device_n.use_alt_cspace = true;
client_color.paint.values[j] = 1.0;
capture_spot_equivalent_cmyk_colors(pdev, pgs, &client_color,
&temp_cs, i, pparams);
break;
}
}
}
}
}
private bool check_all_colors_known(int num_spot,
equivalent_cmyk_color_params * pparams)
{
for (num_spot--; num_spot >= 0; num_spot--)
if (pparams->color[num_spot].color_info_valid == false)
return false;
return true;
}
void
update_spot_equivalent_cmyk_colors(gx_device * pdev, const gs_state * pgs,
gs_devn_params * pdevn_params, equivalent_cmyk_color_params * pparams)
{
const gs_color_space * pcs;
if (pparams->all_color_info_valid)
return;
if (pdevn_params->separations.num_separations == 0) {
pparams->all_color_info_valid = true;
return;
}
pcs = pgs->color_space;
if (pcs != NULL) {
if (pcs->type->index == gs_color_space_index_Separation) {
update_Separation_spot_equivalent_cmyk_colors(pdev, pgs, pcs,
pdevn_params, pparams);
pparams->all_color_info_valid = check_all_colors_known
(pdevn_params->separations.num_separations, pparams);
}
else if (pcs->type->index == gs_color_space_index_DeviceN) {
update_DeviceN_spot_equivalent_cmyk_colors(pdev, pgs, pcs,
pdevn_params, pparams);
pparams->all_color_info_valid = check_all_colors_known
(pdevn_params->separations.num_separations, pparams);
}
}
}
private void
save_spot_equivalent_cmyk_color(int sep_num,
equivalent_cmyk_color_params * pparams, frac cmyk[4])
{
pparams->color[sep_num].c = cmyk[0];
pparams->color[sep_num].m = cmyk[1];
pparams->color[sep_num].y = cmyk[2];
pparams->color[sep_num].k = cmyk[3];
pparams->color[sep_num].color_info_valid = true;
}
typedef struct color_capture_device_s {
gx_device_common;
gx_prn_device_common;
int sep_num;
equivalent_cmyk_color_params * pequiv_cmyk_colors;
} color_capture_device;
private cmap_proc_gray(cmap_gray_capture_cmyk_color);
private cmap_proc_rgb(cmap_rgb_capture_cmyk_color);
private cmap_proc_cmyk(cmap_cmyk_capture_cmyk_color);
private cmap_proc_rgb_alpha(cmap_rgb_alpha_capture_cmyk_color);
private cmap_proc_separation(cmap_separation_capture_cmyk_color);
private cmap_proc_devicen(cmap_devicen_capture_cmyk_color);
private const gx_color_map_procs cmap_capture_cmyk_color = {
cmap_gray_capture_cmyk_color,
cmap_rgb_capture_cmyk_color,
cmap_cmyk_capture_cmyk_color,
cmap_rgb_alpha_capture_cmyk_color,
cmap_separation_capture_cmyk_color,
cmap_devicen_capture_cmyk_color
};
private void
cmap_gray_capture_cmyk_color(frac gray, gx_device_color * pdc,
const gs_imager_state * pis, gx_device * dev, gs_color_select_t select)
{
equivalent_cmyk_color_params * pparams =
((color_capture_device *)dev)->pequiv_cmyk_colors;
int sep_num = ((color_capture_device *)dev)->sep_num;
frac cmyk[4];
cmyk[0] = cmyk[1] = cmyk[2] = frac_0;
cmyk[3] = frac_1 - gray;
save_spot_equivalent_cmyk_color(sep_num, pparams, cmyk);
}
private void
cmap_rgb_capture_cmyk_color(frac r, frac g, frac b, gx_device_color * pdc,
const gs_imager_state * pis, gx_device * dev, gs_color_select_t select)
{
equivalent_cmyk_color_params * pparams =
((color_capture_device *)dev)->pequiv_cmyk_colors;
int sep_num = ((color_capture_device *)dev)->sep_num;
frac cmyk[4];
color_rgb_to_cmyk(r, g, b, pis, cmyk);
save_spot_equivalent_cmyk_color(sep_num, pparams, cmyk);
}
private void
cmap_cmyk_capture_cmyk_color(frac c, frac m, frac y, frac k, gx_device_color * pdc,
const gs_imager_state * pis, gx_device * dev, gs_color_select_t select)
{
equivalent_cmyk_color_params * pparams =
((color_capture_device *)dev)->pequiv_cmyk_colors;
int sep_num = ((color_capture_device *)dev)->sep_num;
frac cmyk[4];
cmyk[0] = c;
cmyk[1] = m;
cmyk[2] = y;
cmyk[3] = k;
save_spot_equivalent_cmyk_color(sep_num, pparams, cmyk);
}
private void
cmap_rgb_alpha_capture_cmyk_color(frac r, frac g, frac b, frac alpha,
gx_device_color * pdc, const gs_imager_state * pis, gx_device * dev,
gs_color_select_t select)
{
cmap_rgb_capture_cmyk_color(r, g, b, pdc, pis, dev, select);
}
private void
cmap_separation_capture_cmyk_color(frac all, gx_device_color * pdc,
const gs_imager_state * pis, gx_device * dev, gs_color_select_t select)
{
dprintf("cmap_separation_capture_cmyk_color - this routine should not be executed\n");
}
private void
cmap_devicen_capture_cmyk_color(const frac * pcc, gx_device_color * pdc,
const gs_imager_state * pis, gx_device * dev, gs_color_select_t select)
{
dprintf("cmap_devicen_capture_cmyk_color - this routine should not be executed\n");
}
private void
capture_spot_equivalent_cmyk_colors(gx_device * pdev, const gs_state * pgs,
const gs_client_color * pcc, const gs_color_space * pcs,
int sep_num, equivalent_cmyk_color_params * pparams)
{
gs_imager_state temp_state = *((const gs_imager_state *)pgs);
color_capture_device temp_device = { 0 };
gx_device_color dev_color;
temp_device.color_info = pdev->color_info;
temp_device.sep_num = sep_num;
temp_device.pequiv_cmyk_colors = pparams;
temp_state.cmap_procs = &cmap_capture_cmyk_color;
temp_state.color_component_map.use_alt_cspace = true;
pcs->type->remap_color (pcc, pcs, &dev_color, &temp_state,
(gx_device *)&temp_device, gs_color_select_texture);
}