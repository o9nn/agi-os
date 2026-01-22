#include <stddef.h>
#include <string.h>
#include "antiword.h"
typedef struct section_mem_tag {
section_block_type tInfo;
ULONG ulCharPos;
struct section_mem_tag *pNext;
} section_mem_type;
static section_mem_type *pAnchor = NULL;
static section_mem_type *pSectionLast = NULL;
void
vDestroySectionInfoList(void)
{
section_mem_type *pCurr, *pNext;
DBG_MSG("vDestroySectionInfoList");
pCurr = pAnchor;
while (pCurr != NULL) {
pNext = pCurr->pNext;
pCurr = xfree(pCurr);
pCurr = pNext;
}
pAnchor = NULL;
pSectionLast = NULL;
}
void
vAdd2SectionInfoList(const section_block_type *pSection, ULONG ulCharPos)
{
section_mem_type *pListMember;
fail(pSection == NULL);
pListMember = xmalloc(sizeof(section_mem_type));
pListMember->tInfo = *pSection;
pListMember->ulCharPos = ulCharPos;
pListMember->pNext = NULL;
if (pAnchor == NULL) {
pAnchor = pListMember;
} else {
fail(pSectionLast == NULL);
pSectionLast->pNext = pListMember;
}
pSectionLast = pListMember;
}
void
vGetDefaultSection(section_block_type *pSection)
{
(void)memset(pSection, 0, sizeof(*pSection));
pSection->bNewPage = TRUE;
}
void
vDefault2SectionInfoList(ULONG ulCharPos)
{
section_block_type tSection;
vGetDefaultSection(&tSection);
vAdd2SectionInfoList(&tSection, ulCharPos);
}
const section_block_type *
pGetSectionInfo(const section_block_type *pOld, ULONG ulCharPos)
{
const section_mem_type *pCurr;
if (pOld == NULL || ulCharPos == 0) {
if (pAnchor == NULL) {
vDefault2SectionInfoList(0);
fail(pAnchor == NULL);
}
NO_DBG_MSG("First record");
return &pAnchor->tInfo;
}
NO_DBG_HEX(ulCharPos);
for (pCurr = pAnchor; pCurr != NULL; pCurr = pCurr->pNext) {
NO_DBG_HEX(pCurr->ulCharPos);
if (ulCharPos == pCurr->ulCharPos ||
ulCharPos + 1 == pCurr->ulCharPos) {
NO_DBG_HEX(pCurr->ulCharPos);
return &pCurr->tInfo;
}
}
return pOld;
}
size_t
tGetNumberOfSections(void)
{
const section_mem_type *pCurr;
size_t tCounter;
for (tCounter = 0, pCurr = pAnchor;
pCurr != NULL;
tCounter++, pCurr = pCurr->pNext)
;
return tCounter;
}
UCHAR
ucGetSepHdrFtrSpecification(size_t tSectionNumber)
{
const section_mem_type *pCurr;
size_t tIndex;
for (tIndex = 0, pCurr = pAnchor;
tIndex < tSectionNumber && pCurr != NULL;
tIndex++, pCurr = pCurr->pNext)
;
if (pCurr == NULL) {
DBG_DEC(tSectionNumber);
DBG_FIXME();
return 0x00;
}
return pCurr->tInfo.ucHdrFtrSpecification;
}