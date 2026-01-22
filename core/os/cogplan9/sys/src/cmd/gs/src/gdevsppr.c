#include "gdevprn.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/ioccom.h>
#include <unbdev/lpviio.h>
private dev_proc_open_device(sparc_open);
private dev_proc_print_page(sparc_print_page);
#define SPARC_MARGINS_A4	0.15, 0.12, 0.12, 0.15
#define SPARC_MARGINS_LETTER	0.15, 0.12, 0.12, 0.15
gx_device_procs prn_sparc_procs =
prn_procs(sparc_open, gdev_prn_output_page, gdev_prn_close);
const gx_device_printer far_data gs_sparc_device =
prn_device(prn_sparc_procs,
"sparc",
DEFAULT_WIDTH_10THS,DEFAULT_HEIGHT_10THS,
400,400,
0,0,0,0,
1,
sparc_print_page);
private int
sparc_open(gx_device *pdev)
{
const float *m;
static const float m_a4[4] = { SPARC_MARGINS_A4 };
static const float m_letter[4] = { SPARC_MARGINS_LETTER };
m = (pdev->height / pdev->y_pixels_per_inch >= 11.1 ? m_a4 : m_letter);
gx_device_set_margins(pdev, m, true);
return gdev_prn_open(pdev);
}
char *errmsg[]={
"EMOTOR",
"EROS",
"EFUSER",
"XEROFAIL",
"ILCKOPEN",
"NOTRAY",
"NOPAPR",
"XITJAM",
"MISFEED",
"WDRUMX",
"WDEVEX",
"NODRUM",
"NODEVE",
"EDRUMX",
"EDEVEX",
"ENGCOLD",
"TIMEOUT",
"EDMA",
"ESERIAL"
};
static char err_buffer[80];
private char *
err_code_string(int err_code)
{
if ((err_code<EMOTOR)||(err_code>ESERIAL))
{
sprintf(err_buffer,"err_code out of range: %d",err_code);
return err_buffer;
}
return errmsg[err_code];
}
int warning=0;
private int
sparc_print_page(gx_device_printer *pdev, FILE *prn)
{
struct lpvi_page lpvipage;
struct lpvi_err lpvierr;
char *out_buf;
int out_size;
if (ioctl(fileno(prn),LPVIIOC_GETPAGE,&lpvipage)!=0)
{
errprintf("sparc_print_page: LPVIIOC_GETPAGE failed\n");
return -1;
}
lpvipage.bitmap_width=gdev_mem_bytes_per_scan_line((gx_device *)pdev);
lpvipage.page_width=lpvipage.bitmap_width*8;
lpvipage.page_length=pdev->height;
lpvipage.resolution = (pdev->x_pixels_per_inch == 300 ? DPI300 : DPI400);
if (ioctl(fileno(prn),LPVIIOC_SETPAGE,&lpvipage)!=0)
{
errprintf("sparc_print_page: LPVIIOC_SETPAGE failed\n");
return -1;
}
out_size=lpvipage.bitmap_width*lpvipage.page_length;
out_buf=gs_malloc(pdev->memory, out_size,1,"sparc_print_page: out_buf");
gdev_prn_copy_scan_lines(pdev,0,out_buf,out_size);
while (write(fileno(prn),out_buf,out_size)!=out_size)
{
if (ioctl(fileno(prn),LPVIIOC_GETERR,&lpvierr)!=0)
{
errprintf("sparc_print_page: LPVIIOC_GETERR failed\n");
return -1;
}
switch (lpvierr.err_type)
{
case 0:
if (warning==0)
{
errprintf(
"sparc_print_page: Printer Problem with unknown reason...");
dflush();
warning=1;
}
sleep(5);
break;
case ENGWARN:
errprintf(
"sparc_print_page: Printer-Warning: %s...",
err_code_string(lpvierr.err_code));
dflush();
warning=1;
sleep(5);
break;
case ENGFATL:
errprintf(
"sparc_print_page: Printer-Fatal: %s\n",
err_code_string(lpvierr.err_code));
return -1;
case EDRVR:
errprintf(
"sparc_print_page: Interface/driver error: %s\n",
err_code_string(lpvierr.err_code));
return -1;
default:
errprintf(
"sparc_print_page: Unknown err_type=%d(err_code=%d)\n",
lpvierr.err_type,lpvierr.err_code);
return -1;
}
}
if (warning==1)
{
errprintf("OK.\n");
warning=0;
}
gs_free(pdev->memory, out_buf,out_size,1,"sparc_print_page: out_buf");
return 0;
}