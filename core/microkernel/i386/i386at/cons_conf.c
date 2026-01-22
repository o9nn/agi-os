#include <sys/types.h>
#include <device/cons.h>
#ifdef	MACH_HYP
#include <xen/console.h>
#else
#include "kd.h"
#if NCOM > 0
#include "com.h"
#endif
#endif
#if	ENABLE_IMMEDIATE_CONSOLE
#include "immc.h"
#endif
struct	consdev constab[] = {
#ifdef	MACH_HYP
{"hyp",	hypcnprobe,	hypcninit,	hypcngetc,	hypcnputc},
#else
#if	ENABLE_IMMEDIATE_CONSOLE
{"immc", immc_cnprobe,	immc_cninit,	immc_cngetc,	immc_cnputc},
#endif
{"kd",	kdcnprobe,	kdcninit,	kdcngetc,	kdcnputc},
#if NCOM > 0
{"com",	comcnprobe,	comcninit,	comcngetc,	comcnputc},
#endif
#endif
{0}
};