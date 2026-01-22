#include "gx.h"
#include "memory_.h"
#include "gdevpdfx.h"
#include "gdevpdtx.h"
#include "gdevpdtf.h"
#include "gdevpdti.h"
#include "gdevpdts.h"
private_st_pdf_text_data();
pdf_text_data_t *
pdf_text_data_alloc(gs_memory_t *mem)
{
pdf_text_data_t *ptd =
gs_alloc_struct(mem, pdf_text_data_t, &st_pdf_text_data,
"pdf_text_data_alloc");
pdf_outline_fonts_t *pofs = pdf_outline_fonts_alloc(mem);
pdf_bitmap_fonts_t *pbfs = pdf_bitmap_fonts_alloc(mem);
pdf_text_state_t *pts = pdf_text_state_alloc(mem);
if (pts == 0 || pbfs == 0 || pofs == 0 || ptd == 0) {
gs_free_object(mem, pts, "pdf_text_data_alloc");
gs_free_object(mem, pbfs, "pdf_text_data_alloc");
gs_free_object(mem, pofs, "pdf_text_data_alloc");
gs_free_object(mem, ptd, "pdf_text_data_alloc");
return 0;
}
memset(ptd, 0, sizeof(*ptd));
ptd->outline_fonts = pofs;
ptd->bitmap_fonts = pbfs;
ptd->text_state = pts;
return ptd;
}