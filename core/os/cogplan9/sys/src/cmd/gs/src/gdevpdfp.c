#include "memory_.h"
#include "string_.h"
#include "gx.h"
#include "gserrors.h"
#include "gdevpdfx.h"
#include "gdevpdfo.h"
#include "gdevpdfg.h"
#include "gsparamx.h"
private int pdf_dsc_process(gx_device_pdf * pdev,
const gs_param_string_array * pma);
private const int CoreDistVersion = 5000;
private const gs_param_item_t pdf_param_items[] = {
#define pi(key, type, memb) { key, type, offset_of(gx_device_pdf, memb) }
pi("PDFEndPage", gs_param_type_int, EndPage),
pi("PDFStartPage", gs_param_type_int, StartPage),
pi("Optimize", gs_param_type_bool, Optimize),
pi("ParseDSCCommentsForDocInfo", gs_param_type_bool,
ParseDSCCommentsForDocInfo),
pi("ParseDSCComments", gs_param_type_bool, ParseDSCComments),
pi("EmitDSCWarnings", gs_param_type_bool, EmitDSCWarnings),
pi("CreateJobTicket", gs_param_type_bool, CreateJobTicket),
pi("PreserveEPSInfo", gs_param_type_bool, PreserveEPSInfo),
pi("AutoPositionEPSFiles", gs_param_type_bool, AutoPositionEPSFiles),
pi("PreserveCopyPage", gs_param_type_bool, PreserveCopyPage),
pi("UsePrologue", gs_param_type_bool, UsePrologue),
pi("OffOptimizations", gs_param_type_int, OffOptimizations),
pi("ReAssignCharacters", gs_param_type_bool, ReAssignCharacters),
pi("ReEncodeCharacters", gs_param_type_bool, ReEncodeCharacters),
pi("FirstObjectNumber", gs_param_type_long, FirstObjectNumber),
pi("CompressFonts", gs_param_type_bool, CompressFonts),
pi("PrintStatistics", gs_param_type_bool, PrintStatistics),
pi("MaxInlineImageSize", gs_param_type_long, MaxInlineImageSize),
pi("OwnerPassword", gs_param_type_string, OwnerPassword),
pi("UserPassword", gs_param_type_string, UserPassword),
pi("KeyLength", gs_param_type_int, KeyLength),
pi("Permissions", gs_param_type_int, Permissions),
pi("EncryptionR", gs_param_type_int, EncryptionR),
pi("NoEncrypt", gs_param_type_string, NoEncrypt),
pi("ForOPDFRead", gs_param_type_bool, ForOPDFRead),
pi("PatternImagemask", gs_param_type_bool, PatternImagemask),
pi("MaxClipPathSize", gs_param_type_int, MaxClipPathSize),
pi("MaxShadingBitmapSize", gs_param_type_int, MaxShadingBitmapSize),
pi("MaxViewerMemorySize", gs_param_type_int, MaxViewerMemorySize),
pi("HaveTrueTypes", gs_param_type_bool, HaveTrueTypes),
pi("HaveCIDSystem", gs_param_type_bool, HaveCIDSystem),
pi("HaveTransparency", gs_param_type_bool, HaveTransparency),
pi("OPDFReadProcsetPath", gs_param_type_string, OPDFReadProcsetPath),
pi("CompressEntireFile", gs_param_type_bool, CompressEntireFile),
pi("PDFX", gs_param_type_bool, PDFX),
#undef pi
gs_param_item_end
};
int
gdev_pdf_get_params(gx_device * dev, gs_param_list * plist)
{
gx_device_pdf *pdev = (gx_device_pdf *) dev;
float cl = (float)pdev->CompatibilityLevel;
int code = gdev_psdf_get_params(dev, plist);
int cdv = CoreDistVersion;
int EmbedFontObjects = 1;
if (code < 0 ||
(code = param_write_int(plist, ".EmbedFontObjects", &EmbedFontObjects)) < 0 ||
(code = param_write_int(plist, "CoreDistVersion", &cdv)) < 0 ||
(code = param_write_float(plist, "CompatibilityLevel", &cl)) < 0 ||
(param_requested(plist, "pdfmark") > 0 &&
(code = param_write_null(plist, "pdfmark")) < 0) ||
(param_requested(plist, "DSC") > 0 &&
(code = param_write_null(plist, "DSC")) < 0) ||
(code = gs_param_write_items(plist, pdev, NULL, pdf_param_items)) < 0
);
return code;
}
int
gdev_pdf_put_params(gx_device * dev, gs_param_list * plist)
{
gx_device_pdf *pdev = (gx_device_pdf *) dev;
int ecode, code;
gx_device_pdf save_dev;
float cl = (float)pdev->CompatibilityLevel;
bool locked = pdev->params.LockDistillerParams;
gs_param_name param_name;
{
gs_param_string_array ppa;
code = param_read_string_array(plist, (param_name = "pdfmark"), &ppa);
switch (code) {
case 0:
code = pdf_open_document(pdev);
if (code < 0)
return code;
code = pdfmark_process(pdev, &ppa);
if (code >= 0)
return code;
default:
param_signal_error(plist, param_name, code);
return code;
case 1:
break;
}
code = param_read_string_array(plist, (param_name = "DSC"), &ppa);
switch (code) {
case 0:
code = pdf_open_document(pdev);
if (code < 0)
return code;
code = pdf_dsc_process(pdev, &ppa);
if (code >= 0)
return code;
default:
param_signal_error(plist, param_name, code);
return code;
case 1:
break;
}
}
ecode = code = param_read_bool(plist, "LockDistillerParams", &locked);
if (!(locked && pdev->params.LockDistillerParams)) {
{
int efo = 1;
ecode = param_put_int(plist, (param_name = ".EmbedFontObjects"), &efo, ecode);
if (efo != 1)
param_signal_error(plist, param_name, ecode = gs_error_rangecheck);
}
{
int cdv = CoreDistVersion;
ecode = param_put_int(plist, (param_name = "CoreDistVersion"), &cdv, ecode);
if (cdv != CoreDistVersion)
param_signal_error(plist, param_name, ecode = gs_error_rangecheck);
}
save_dev = *pdev;
switch (code = param_read_float(plist, (param_name = "CompatibilityLevel"), &cl)) {
default:
ecode = code;
param_signal_error(plist, param_name, ecode);
case 0:
if (cl < (float)1.15)
cl = (float)1.1;
else if (cl < (float)1.25)
cl = (float)1.2;
else if (cl >= (float)1.35)
cl = (float)1.4;
else
cl = (float)1.3;
case 1:
break;
}
code = gs_param_read_items(plist, pdev, pdf_param_items);
if (code < 0)
ecode = code;
{
long fon = pdev->FirstObjectNumber;
if (fon != save_dev.FirstObjectNumber) {
if (fon <= 0 || fon > 0x7fff0000 ||
(pdev->next_id != 0 &&
pdev->next_id !=
save_dev.FirstObjectNumber + pdf_num_initial_ids)
) {
ecode = gs_error_rangecheck;
param_signal_error(plist, "FirstObjectNumber", ecode);
}
}
}
{
static const char *const pcm_names[] = {
"DeviceGray", "DeviceRGB", "DeviceCMYK", "DeviceN", 0
};
int pcm = -1;
ecode = param_put_enum(plist, "ProcessColorModel", &pcm,
pcm_names, ecode);
if (pcm >= 0) {
pdf_set_process_color_model(pdev, pcm);
pdf_set_initial_color(pdev, &pdev->saved_fill_color, &pdev->saved_stroke_color,
&pdev->fill_used_process_color, &pdev->stroke_used_process_color);
}
}
}
if (ecode < 0)
goto fail;
if (pdev->PDFX)
cl = (float)1.3;
pdev->version = (cl < 1.2 ? psdf_version_level2 : psdf_version_ll3);
if (pdev->ForOPDFRead) {
pdev->ResourcesBeforeUsage = true;
pdev->HaveCFF = false;
pdev->HavePDFWidths = false;
pdev->HaveStrokeColor = false;
cl = (float)1.2;
pdev->MaxInlineImageSize = max_long;
pdev->version = psdf_version_level2;
} else {
pdev->ResourcesBeforeUsage = false;
pdev->HaveCFF = true;
pdev->HavePDFWidths = true;
pdev->HaveStrokeColor = true;
}
ecode = gdev_psdf_put_params(dev, plist);
if (ecode < 0)
goto fail;
if (pdev->HaveTrueTypes && pdev->version == psdf_version_level2) {
pdev->version = psdf_version_level2_with_TT ;
}
#define MAX_EXTENT ((int)(MAX_USER_COORD * 0.9))
if (dev->height > MAX_EXTENT || dev->width > MAX_EXTENT) {
double factor =
max(dev->height / (double)MAX_EXTENT,
dev->width / (double)MAX_EXTENT);
gx_device_set_resolution(dev, dev->HWResolution[0] / factor,
dev->HWResolution[1] / factor);
}
#undef MAX_EXTENT
if (pdev->FirstObjectNumber != save_dev.FirstObjectNumber) {
if (pdev->xref.file != 0) {
fseek(pdev->xref.file, 0L, SEEK_SET);
pdf_initialize_ids(pdev);
}
}
pdev->CompatibilityLevel = (int)(cl * 10 + 0.5) / 10.0;
return 0;
fail:
pdev->version = save_dev.version;
pdf_set_process_color_model(pdev, save_dev.pcm_color_info_index);
pdev->saved_fill_color = save_dev.saved_fill_color;
pdev->saved_stroke_color = save_dev.saved_fill_color;
{
const gs_param_item_t *ppi = pdf_param_items;
for (; ppi->key; ++ppi)
memcpy((char *)pdev + ppi->offset,
(char *)&save_dev + ppi->offset,
gs_param_type_sizes[ppi->type]);
}
return ecode;
}
private int
pdf_dsc_process(gx_device_pdf * pdev, const gs_param_string_array * pma)
{
int code = 0;
int i;
if (!pdev->ParseDSCComments)
return 0;
for (i = 0; i + 1 < pma->size && code >= 0; i += 2) {
const gs_param_string *pkey = &pma->data[i];
const gs_param_string *pvalue = &pma->data[i + 1];
const char *key;
int code;
if (pdf_key_eq(pkey, "Creator"))
key = "/Creator";
else if (pdf_key_eq(pkey, "Title"))
key = "/Title";
else if (pdf_key_eq(pkey, "For"))
key = "/Author";
else {
pdf_page_dsc_info_t *ppdi;
if ((ppdi = &pdev->doc_dsc_info,
pdf_key_eq(pkey, "Orientation")) ||
(ppdi = &pdev->page_dsc_info,
pdf_key_eq(pkey, "PageOrientation"))
) {
if (pvalue->size == 1 && pvalue->data[0] >= '0' &&
pvalue->data[0] <= '3'
)
ppdi->orientation = pvalue->data[0] - '0';
else
ppdi->orientation = -1;
} else if ((ppdi = &pdev->doc_dsc_info,
pdf_key_eq(pkey, "ViewingOrientation")) ||
(ppdi = &pdev->page_dsc_info,
pdf_key_eq(pkey, "PageViewingOrientation"))
) {
gs_matrix mat;
int orient;
if (sscanf((const char *)pvalue->data, "[%g %g %g %g]",
&mat.xx, &mat.xy, &mat.yx, &mat.yy) != 4
)
continue;
for (orient = 0; orient < 4; ++orient) {
if (mat.xx == 1 && mat.xy == 0 && mat.yx == 0 && mat.yy == 1)
break;
gs_matrix_rotate(&mat, -90.0, &mat);
}
if (orient == 4)
orient = -1;
ppdi->viewing_orientation = orient;
} else {
gs_rect box;
if (pdf_key_eq(pkey, "EPSF")) {
pdev->is_EPS = (pvalue->size >= 1 && pvalue->data[0] != '0');
continue;
}
if (pdf_key_eq(pkey, "BoundingBox"))
ppdi = &pdev->doc_dsc_info;
else if (pdf_key_eq(pkey, "PageBoundingBox"))
ppdi = &pdev->page_dsc_info;
else
continue;
if (sscanf((const char *)pvalue->data, "[%lg %lg %lg %lg]",
&box.p.x, &box.p.y, &box.q.x, &box.q.y) != 4
)
continue;
ppdi->bounding_box = box;
}
continue;
}
if (pdev->ParseDSCCommentsForDocInfo || pdev->PreserveEPSInfo)
code = cos_dict_put_c_key_string(pdev->Info, key,
pvalue->data, pvalue->size);
}
return code;
}