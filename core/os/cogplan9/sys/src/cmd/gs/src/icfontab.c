#include "ccfont.h"
#undef font_
#define font_(fname, fproc, zfproc) extern ccfont_proc(fproc);
#ifndef GCONFIGF_H
# include "gconfigf.h"
#else
# include GCONFIGF_H
#endif
private const ccfont_fproc fprocs[] = {
#undef font_
#define font_(fname, fproc, zfproc) fproc,
#ifndef GCONFIGF_H
# include "gconfigf.h"
#else
# include GCONFIGF_H
#endif
0
};
int
ccfont_fprocs(int *pnum_fprocs, const ccfont_fproc ** pfprocs)
{
*pnum_fprocs = countof(fprocs) - 1;
*pfprocs = &fprocs[0];
return ccfont_version;
}