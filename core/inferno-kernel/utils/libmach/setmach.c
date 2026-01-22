#include <lib9.h>
#include <bio.h>
#include "mach.h"
typedef struct machtab Machtab;
struct machtab
{
char *name;
short type;
short boottype;
int asstype;
Mach *mach;
Machdata *machdata;
};
extern Mach mmips, msparc, mi386, mamd64,
marm, mmips2be, mmips2le, mpower, mpower64, mriscv, mriscv64;
extern Machdata mipsmach, sparcmach, i386mach,
armmach, mipsmach2le, powermach, riscvmach, riscv64mach;
Machtab machines[] =
{
{ "mips2LE",
FMIPS2LE,
0,
AMIPS,
&mmips2le,
&mipsmach2le, },
{ "mips",
FMIPS,
FMIPSB,
AMIPS,
&mmips,
&mipsmach, },
{ "mips2",
FMIPS2BE,
FMIPSB,
AMIPS,
&mmips2be,
&mipsmach, },
{ "mipsco",
FMIPS,
FMIPSB,
AMIPSCO,
&mmips,
&mipsmach, },
{ "sparc",
FSPARC,
FSPARCB,
ASPARC,
&msparc,
&sparcmach, },
{ "sunsparc",
FSPARC,
FSPARCB,
ASUNSPARC,
&msparc,
&sparcmach, },
{ "386",
FI386,
FI386B,
AI386,
&mi386,
&i386mach, },
{ "86",
FI386,
FI386B,
AI8086,
&mi386,
&i386mach, },
{ "amd64",
FAMD64,
FAMD64B,
AAMD64,
&mamd64,
&i386mach, },
{ "arm",
FARM,
FARMB,
AARM,
&marm,
&armmach, },
{ "power",
FPOWER,
FPOWERB,
APOWER,
&mpower,
&powermach, },
{ "power64",
FPOWER64,
FPOWER64B,
APOWER64,
&mpower64,
&powermach, },
{ "riscv",
FRISCV,
FRISCVB,
ARISCV,
&mriscv,
&riscvmach, },
{ "riscv64",
FRISCV64,
FRISCV64B,
ARISCV64,
&mriscv64,
&riscv64mach, },
{ 0 },
};
void
machbytype(int type)
{
Machtab *mp;
for (mp = machines; mp->name; mp++){
if (mp->type == type || mp->boottype == type) {
asstype = mp->asstype;
machdata = mp->machdata;
break;
}
}
}
int
machbyname(char *name)
{
Machtab *mp;
if (!name) {
asstype = AMIPS;
machdata = &mipsmach;
mach = &mmips;
return 1;
}
for (mp = machines; mp->name; mp++){
if (strcmp(mp->name, name) == 0) {
asstype = mp->asstype;
machdata = mp->machdata;
mach = mp->mach;
return 1;
}
}
return 0;
}