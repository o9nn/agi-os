#include "gx.h"
#include "cgconfig.h"
#include "port.h"
#include "shareinc.h"
#include "gxfapi.h"
struct IF_STATE;
private LPUB8 stub_PCLEO_charptr(LPUB8 pfont_hdr, UW16 sym_code)
{ return NULL;
}
private LPUB8 stub_PCLchId2ptr(IF_STATE *pIFS, UW16 chId)
{ return NULL;
}
private LPUB8 stub_PCLglyphID2Ptr(IF_STATE *pIFS, UW16 glyphID)
{ return NULL;
}
private LPUB8 (*m_PCLEO_charptr)(LPUB8 pfont_hdr, UW16 sym_code) = stub_PCLEO_charptr;
private LPUB8 (*m_PCLchId2ptr)(IF_STATE *pIFS, UW16 chId) = stub_PCLchId2ptr;
private LPUB8 (*m_PCLglyphID2Ptr)(IF_STATE *pIFS, UW16 glyphID) = stub_PCLglyphID2Ptr;
LPUB8 PCLEO_charptr(LPUB8 pfont_hdr, UW16 sym_code)
{ return m_PCLEO_charptr(pfont_hdr, sym_code);
}
LPUB8 PCLchId2ptr(IF_STATE *pIFS, UW16 chId)
{ return m_PCLchId2ptr(FSA chId);
}
LPUB8 PCLglyphID2Ptr(IF_STATE *pIFS, UW16 glyphID)
{ return m_PCLglyphID2Ptr(FSA glyphID);
}
void gx_set_UFST_Callbacks(LPUB8 (*p_PCLEO_charptr)(LPUB8 pfont_hdr, UW16 sym_code),
LPUB8 (*p_PCLchId2ptr)(IF_STATE *pIFS, UW16 chId),
LPUB8 (*p_PCLglyphID2Ptr)(IF_STATE *pIFS, UW16 glyphID))
{ m_PCLEO_charptr = p_PCLEO_charptr;
m_PCLchId2ptr = p_PCLchId2ptr;
m_PCLglyphID2Ptr = p_PCLglyphID2Ptr;
}
void gx_reset_UFST_Callbacks()
{ m_PCLEO_charptr = stub_PCLEO_charptr;
m_PCLchId2ptr = stub_PCLchId2ptr;
m_PCLglyphID2Ptr = stub_PCLglyphID2Ptr;
}