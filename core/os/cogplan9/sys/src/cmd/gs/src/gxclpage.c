#include "gdevprn.h"
#include "gxcldev.h"
#include "gxclpage.h"
int
gdev_prn_save_page(gx_device_printer * pdev, gx_saved_page * page,
int num_copies)
{
if (!pdev->buffer_space)
return_error(gs_error_rangecheck);
if (strlen(pdev->dname) >= sizeof(page->dname))
return_error(gs_error_limitcheck);
{
gx_device_clist_writer * const pcldev =
(gx_device_clist_writer *)pdev;
int code;
if ((code = clist_end_page(pcldev)) < 0 ||
(code = clist_fclose(pcldev->page_cfile, pcldev->page_cfname, false)) < 0 ||
(code = clist_fclose(pcldev->page_bfile, pcldev->page_bfname, false)) < 0
)
return code;
memcpy(&page->device, pdev, sizeof(gx_device));
strcpy(page->dname, pdev->dname);
page->info = pcldev->page_info;
page->info.cfile = 0;
page->info.bfile = 0;
}
page->num_copies = num_copies;
return (*gs_clist_device_procs.open_device) ((gx_device *) pdev);
}
int
gdev_prn_render_pages(gx_device_printer * pdev,
const gx_placed_page * ppages, int count)
{
gx_device_clist_reader * const pcldev =
(gx_device_clist_reader *)pdev;
{
int i;
gx_band_params_t params;
for (i = 0; i < count; ++i) {
const gx_saved_page *page = ppages[i].page;
if (strcmp(page->dname, pdev->dname) != 0 ||
memcmp(&page->device.color_info, &pdev->color_info,
sizeof(pdev->color_info)) != 0
)
return_error(gs_error_rangecheck);
if (ppages[i].offset.y != 0)
return_error(gs_error_rangecheck);
if (page->info.band_params.BandBufferSpace !=
pdev->buffer_space ||
page->info.band_params.BandWidth !=
pdev->width
)
return_error(gs_error_rangecheck);
if (i == 0)
params = page->info.band_params;
else if (page->info.band_params.BandHeight !=
params.BandHeight
)
return_error(gs_error_rangecheck);
}
}
pcldev->ymin = pcldev->ymax = 0;
pcldev->pages = ppages;
pcldev->num_pages = count;
{
int code = (*dev_proc(pdev, output_page))
((gx_device *) pdev, ppages[0].page->num_copies, true);
int i;
for (i = 0; i < count; ++i) {
const gx_saved_page *page = ppages[i].page;
clist_unlink(page->info.cfname);
clist_unlink(page->info.bfname);
}
return code;
}
}