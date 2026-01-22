#include <stdlib.h>
#include "antiword.h"
typedef struct picture_mem_tag {
picture_block_type tInfo;
struct picture_mem_tag *pNext;
} picture_mem_type;
static picture_mem_type *pAnchor = NULL;
static picture_mem_type *pPictureLast = NULL;
void
vDestroyPictInfoList(void)
{
picture_mem_type *pCurr, *pNext;
DBG_MSG("vDestroyPictInfoList");
pCurr = pAnchor;
while (pCurr != NULL) {
pNext = pCurr->pNext;
pCurr = xfree(pCurr);
pCurr = pNext;
}
pAnchor = NULL;
pPictureLast = NULL;
}
void
vAdd2PictInfoList(const picture_block_type *pPictureBlock)
{
picture_mem_type *pListMember;
fail(pPictureBlock == NULL);
NO_DBG_MSG("bAdd2PictInfoList");
if (pPictureBlock->ulFileOffset == FC_INVALID) {
return;
}
if (pPictureBlock->ulFileOffsetPicture == FC_INVALID) {
return;
}
NO_DBG_HEX(pPictureBlock->ulFileOffset);
NO_DBG_HEX(pPictureBlock->ulFileOffsetPicture);
NO_DBG_HEX(pPictureBlock->ulPictureOffset);
pListMember = xmalloc(sizeof(picture_mem_type));
pListMember->tInfo = *pPictureBlock;
pListMember->pNext = NULL;
if (pAnchor == NULL) {
pAnchor = pListMember;
} else {
fail(pPictureLast == NULL);
pPictureLast->pNext = pListMember;
}
pPictureLast = pListMember;
}
ULONG
ulGetPictInfoListItem(ULONG ulFileOffset)
{
picture_mem_type *pCurr;
for (pCurr = pAnchor; pCurr != NULL; pCurr = pCurr->pNext) {
if (pCurr->tInfo.ulFileOffset == ulFileOffset) {
return pCurr->tInfo.ulFileOffsetPicture;
}
}
return FC_INVALID;
}