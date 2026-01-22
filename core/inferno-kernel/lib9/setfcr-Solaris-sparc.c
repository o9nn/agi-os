#include "lib9.h"
ulong
getfcr(void)
{
ulong v;
asm("	st	%fsr, [%fp-8]");
return v;
}
void
setfcr(ulong v)
{
ulong vv;
vv = (getfcr() & ~FPFCR) | (v & FPFCR);
asm("	ld	[%fp-4], %fsr");
}
ulong
getfsr(void)
{
ulong v;
asm("	st	%fsr, [%fp-8]");
return v;
}
void
setfsr(ulong v)
{
ulong vv;
vv = (getfsr() & ~FPFSR) | (v & FPFSR);
asm("	ld	[%fp-4], %fsr");
}