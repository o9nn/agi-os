#include "antiword.h"
#define HALF_INCH	36000L
static document_block_type *pAnchor = NULL;
static document_block_type tInfo;
void
vDestroyDocumentInfoList(void)
{
DBG_MSG("vDestroyDocumentInfoList");
pAnchor = NULL;
}
void
vCreateDocumentInfoList(const document_block_type *pDocument)
{
fail(pDocument == NULL);
fail(pAnchor != NULL);
tInfo = *pDocument;
pAnchor = &tInfo;
}
long
lGetDefaultTabWidth(void)
{
long	lDefaultTabWidth;
USHORT	usTmp;
if (pAnchor == NULL) {
DBG_FIXME();
return HALF_INCH;
}
usTmp = pAnchor->usDefaultTabWidth;
lDefaultTabWidth = usTmp == 0 ? HALF_INCH : lTwips2MilliPoints(usTmp);
NO_DBG_DEC(lDefaultTabWidth);
return lDefaultTabWidth;
}
UCHAR
ucGetDopHdrFtrSpecification(void)
{
if (pAnchor == NULL) {
DBG_FIXME();
return 0x00;
}
return pAnchor->ucHdrFtrSpecification;
}