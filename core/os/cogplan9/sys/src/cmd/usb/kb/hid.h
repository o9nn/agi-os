typedef struct Chain Chain;
typedef struct HidInterface HidInterface;
typedef struct HidRepTempl HidRepTempl;
enum {
Stack = 32 * 1024,
PtrCSP = 0x020103,
KbdCSP = 0x010103,
Getproto = 0x03,
Setidle = 0x0a,
Setproto = 0x0b,
Bootproto = 0,
Reportproto = 1,
};
enum {
Mlctrl = 0,
Mlshift = 1,
Mlalt = 2,
Mlgui = 3,
Mrctrl = 4,
Mrshift = 5,
Mralt = 6,
Mrgui = 7,
Mctrl = 1<<Mlctrl | 1<<Mrctrl,
Mshift = 1<<Mlshift | 1<<Mrshift,
Malt = 1<<Mlalt | 1<<Mralt,
Mcompose = 1<<Mlalt,
Maltgr = 1<<Mralt,
Mgui = 1<<Mlgui | 1<<Mrgui,
MaxAcc = 3,
PtrMask= 0xf,
};
enum {
SCesc1 = 0xe0,
SCesc2 = 0xe1,
SClshift = 0x2a,
SCrshift = 0x36,
SCctrl = 0x1d,
SCcompose = 0x38,
Keyup = 0x80,
Keymask = 0x7f,
};
int kbmain(Dev *d, int argc, char*argv[]);
enum{
MaxChLen = 64,
};
struct Chain {
int b;
int e;
uchar buf[MaxChLen];
};
#define MSK(nbits) ((1UL << (nbits)) - 1)
#define IsCut(bbits, ebits) (((ebits)/8 - (bbits)/8) > 0)
enum {
KindPad = 0,
KindButtons,
KindX,
KindY,
KindWheel,
MaxVals = 16,
MaxIfc = 8,
};
struct HidInterface {
ulong v[MaxVals];
uchar kind[MaxVals];
int nbits;
int count;
};
struct HidRepTempl{
int id;
uint sz;
int nifcs;
HidInterface ifcs[MaxIfc];
};
enum {
HidReportApp = 0x01,
HidTypeUsgPg = 0x05,
HidPgButts = 0x09,
HidTypeRepSz = 0x75,
HidTypeCnt = 0x95,
HidCollection = 0xa1,
HidTypeUsg = 0x09,
HidPtr = 0x01,
HidX = 0x30,
HidY = 0x31,
HidZ = 0x32,
HidWheel = 0x38,
HidInput = 0x81,
HidReportId = 0x85,
HidReportIdPtr = 0x01,
HidEnd = 0xc0,
};
void dumpreport(HidRepTempl *templ);
int hidifcval(HidRepTempl *templ, int kind, int n);
int parsereport(HidRepTempl *templ, Chain *rep);
int parsereportdesc(HidRepTempl *temp, uchar *repdesc, int repsz);