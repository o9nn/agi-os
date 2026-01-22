#include "math_.h"
#include "time_.h"
#include "stat_.h"
#include "unistd_.h"
#include "gx.h"
#include "gscdefs.h"
#include "gxdevice.h"
#include "gdevpsu.h"
#include "spprint.h"
#include "stream.h"
#include "gserrors.h"
int
psw_print_lines(FILE *f, const char *const lines[])
{
int i;
for (i = 0; lines[i] != 0; ++i) {
if (fprintf(f, "%s\n", lines[i]) < 0)
return_error(gs_error_ioerror);
}
return 0;
}
private void
psw_put_procset_name(stream *s, const gx_device *dev,
const gx_device_pswrite_common_t *pdpc)
{
pprints1(s, "GS_%s", dev->dname);
pprintd3(s, "_%d_%d_%d",
(int)pdpc->LanguageLevel,
(int)(pdpc->LanguageLevel * 10 + 0.5) % 10,
pdpc->ProcSet_version);
}
private void
psw_print_procset_name(FILE *f, const gx_device *dev,
const gx_device_pswrite_common_t *pdpc)
{
byte buf[100];
stream s;
s_init(&s, dev->memory);
swrite_file(&s, f, buf, sizeof(buf));
psw_put_procset_name(&s, dev, pdpc);
sflush(&s);
}
private void
psw_print_bbox(FILE *f, const gs_rect *pbbox)
{
fprintf(f, "%%%%BoundingBox: %d %d %d %d\n",
(int)floor(pbbox->p.x), (int)floor(pbbox->p.y),
(int)ceil(pbbox->q.x), (int)ceil(pbbox->q.y));
fprintf(f, "%%%%HiResBoundingBox: %f %f %f %f\n",
pbbox->p.x, pbbox->p.y, pbbox->q.x, pbbox->q.y);
}
private const char *const psw_ps_header[] = {
"%!PS-Adobe-3.0",
"%%Pages: (atend)",
0
};
private const char *const psw_eps_header[] = {
"%!PS-Adobe-3.0 EPSF-3.0",
0
};
private const char *const psw_begin_prolog[] = {
"%%EndComments",
"%%BeginProlog",
"% This copyright applies to everything between here and the %%EndProlog:",
0
};
private const char *const psw_ps_procset[] = {
"/PageSize 2 array def"
"/setpagesize"
"{ PageSize aload pop "
"3 index eq exch",
"4 index eq and"
"{ pop pop pop"
"}"
"{ PageSize dup  1",
"5 -1 roll put 0 "
"4 -1 roll put "
"dup null eq {false} {dup where} ifelse"
"{ exch get exec"
"}",
"{ pop"
"/setpagedevice where",
"{ pop 1 dict dup /PageSize PageSize put setpagedevice"
"}",
"{ /setpage where"
"{ pop PageSize aload pop pageparams 3 {exch pop} repeat",
"setpage"
"}"
"if"
"}"
"ifelse"
"}"
"ifelse"
"}"
"ifelse"
"} bind def",
0
};
private const char *const psw_end_prolog[] = {
"end readonly def",
"%%EndResource",
"/pagesave null def",
"%%EndProlog",
0
};
private bool
is_seekable(FILE *f)
{
struct stat buf;
if(fstat(fileno(f), &buf))
return 0;
return S_ISREG(buf.st_mode);
}
int
psw_begin_file_header(FILE *f, const gx_device *dev, const gs_rect *pbbox,
gx_device_pswrite_common_t *pdpc, bool ascii)
{
psw_print_lines(f, (pdpc->ProduceEPS ? psw_eps_header : psw_ps_header));
if (pbbox) {
psw_print_bbox(f, pbbox);
pdpc->bbox_position = 0;
} else if (!is_seekable(f)) {
pdpc->bbox_position = -1;
fputs("%%BoundingBox: (atend)\n", f);
fputs("%%HiResBoundingBox: (atend)\n", f);
} else {
pdpc->bbox_position = ftell(f);
fputs("%...............................................................\n", f);
fputs("%...............................................................\n", f);
}
fprintf(f, "%%%%Creator: %s %ld (%s)\n", gs_product, (long)gs_revision,
dev->dname);
{
time_t t;
struct tm tms;
time(&t);
tms = *localtime(&t);
fprintf(f, "%%%%CreationDate: %d/%02d/%02d %02d:%02d:%02d\n",
tms.tm_year + 1900, tms.tm_mon + 1, tms.tm_mday,
tms.tm_hour, tms.tm_min, tms.tm_sec);
}
if (ascii)
fputs("%%DocumentData: Clean7Bit\n", f);
if (pdpc->LanguageLevel >= 2.0)
fprintf(f, "%%%%LanguageLevel: %d\n", (int)pdpc->LanguageLevel);
else if (pdpc->LanguageLevel == 1.5)
fputs("%%Extensions: CMYK\n", f);
psw_print_lines(f, psw_begin_prolog);
fprintf(f, "%% %s\n", gs_copyright);
fputs("%%BeginResource: procset ", f);
fflush(f);
psw_print_procset_name(f, dev, pdpc);
fputs("\n/", f);
fflush(f);
psw_print_procset_name(f, dev, pdpc);
fputs(" 80 dict dup begin\n", f);
psw_print_lines(f, psw_ps_procset);
fflush(f);
if (ferror(f))
return_error(gs_error_ioerror);
return 0;
}
int
psw_end_file_header(FILE *f)
{
return psw_print_lines(f, psw_end_prolog);
}
int
psw_end_file(FILE *f, const gx_device *dev,
const gx_device_pswrite_common_t *pdpc, const gs_rect *pbbox,
int  page_count)
{
if (f == NULL)
return 0;
fprintf(f, "%%%%Trailer\n%%%%Pages: %ld\n", (long)page_count);
if (ferror(f))
return_error(gs_error_ioerror);
if (dev->PageCount > 0 && pdpc->bbox_position != 0) {
if (pdpc->bbox_position >= 0) {
long save_pos = ftell(f);
fseek(f, pdpc->bbox_position, SEEK_SET);
psw_print_bbox(f, pbbox);
fputc('%', f);
if (ferror(f))
return_error(gs_error_ioerror);
fseek(f, save_pos, SEEK_SET);
} else
psw_print_bbox(f, pbbox);
}
if (!pdpc->ProduceEPS)
fputs("%%EOF\n", f);
if (ferror(f))
return_error(gs_error_ioerror);
return 0;
}
int
psw_write_page_header(stream *s, const gx_device *dev,
const gx_device_pswrite_common_t *pdpc,
bool do_scale, long page_ord, int dictsize)
{
long page = dev->PageCount + 1;
pprintld2(s, "%%%%Page: %ld %ld\n%%%%BeginPageSetup\n", page, page_ord);
psw_put_procset_name(s, dev, pdpc);
stream_puts(s, " begin\n");
if (!pdpc->ProduceEPS) {
int width = (int)(dev->width * 72.0 / dev->HWResolution[0] + 0.5);
int height = (int)(dev->height * 72.0 / dev->HWResolution[1] + 0.5);
typedef struct ps_ {
const char *size_name;
int width, height;
} page_size;
static const page_size sizes[] = {
{"/11x17", 792, 1224},
{"/a3", 842, 1190},
{"/a4", 595, 842},
{"/b5", 501, 709},
{"/ledger", 1224, 792},
{"/legal", 612, 1008},
{"/letter", 612, 792},
{"null", 0, 0}
};
const page_size *p = sizes;
while (p->size_name[0] == '/' &&
(p->width != width || p->height != height))
++p;
pprintd2(s, "%d %d ", width, height);
pprints1(s, "%s setpagesize\n", p->size_name);
}
pprintd1(s, "/pagesave save store %d dict begin\n", dictsize);
if (do_scale)
pprintg2(s, "%g %g scale\n",
72.0 / dev->HWResolution[0], 72.0 / dev->HWResolution[1]);
stream_puts(s, "%%EndPageSetup\ngsave mark\n");
if (s->end_status == ERRC)
return_error(gs_error_ioerror);
return 0;
}
int
psw_write_page_trailer(FILE *f, int num_copies, int flush)
{
fprintf(f, "cleartomark end end pagesave restore\n");
if (num_copies != 1)
fprintf(f, "userdict /#copies %d put\n", num_copies);
fprintf(f, " %s\n%%%%PageTrailer\n", (flush ? "showpage" : "copypage"));
fflush(f);
if (ferror(f))
return_error(gs_error_ioerror);
return 0;
}