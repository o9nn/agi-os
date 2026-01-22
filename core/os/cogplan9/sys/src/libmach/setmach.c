#include	<u.h>
#include	<libc.h>
#include	<bio.h>
#include	<mach.h>
typedef	struct machtab Machtab;
struct machtab
{
char		*name;
short		type;
short		boottype;
int		asstype;
Mach		*mach;
Machdata	*machdata;
};
extern	Mach		mmips, msparc, m68020, mi386, mamd64,
marm, mmips2be, mmips2le, mpower, mpower64, malpha, msparc64;
extern	Machdata	mipsmach, mipsmachle, sparcmach, m68020mach, i386mach,
armmach, mipsmach2le, powermach, alphamach, sparc64mach;
Machtab	machines[] =
{
{	"68020",
F68020,
F68020B,
A68020,
&m68020,
&m68020mach,	},
{	"68020",
F68020,
FNEXTB,
A68020,
&m68020,
&m68020mach,	},
{	"mips2LE",
FMIPS2LE,
0,
AMIPS,
&mmips2le,
&mipsmach2le, 	},
{	"mipsLE",
FMIPSLE,
0,
AMIPS,
&mmips,
&mipsmachle, 	},
{	"mips",
FMIPS,
FMIPSB,
AMIPS,
&mmips,
&mipsmach, 	},
{	"mips2",
FMIPS2BE,
FMIPSB,
AMIPS,
&mmips2be,
&mipsmach, 	},
{	"mipsco",
FMIPS,
FMIPSB,
AMIPSCO,
&mmips,
&mipsmach,	},
{	"sparc",
FSPARC,
FSPARCB,
ASPARC,
&msparc,
&sparcmach,	},
{	"sunsparc",
FSPARC,
FSPARCB,
ASUNSPARC,
&msparc,
&sparcmach,	},
{	"386",
FI386,
FI386B,
AI386,
&mi386,
&i386mach,	},
{	"86",
FI386,
FI386B,
AI8086,
&mi386,
&i386mach,	},
{	"amd64",
FAMD64,
FAMD64B,
AAMD64,
&mamd64,
&i386mach,	},
{	"arm",
FARM,
FARMB,
AARM,
&marm,
&armmach,	},
{	"power",
FPOWER,
FPOWERB,
APOWER,
&mpower,
&powermach,	},
{	"power64",
FPOWER64,
FPOWER64B,
APOWER64,
&mpower64,
&powermach,	},
{	"alpha",
FALPHA,
FALPHAB,
AALPHA,
&malpha,
&alphamach,	},
{	"sparc64",
FSPARC64,
FSPARCB,
ASPARC64,
&msparc64,
&sparc64mach,	},
{	0		},
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