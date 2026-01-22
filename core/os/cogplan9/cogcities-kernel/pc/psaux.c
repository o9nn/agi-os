#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "../port/error.h"
#include "io.h"
#define Image	IMAGE
#include <draw.h>
#include <memdraw.h>
#include <cursor.h>
#include "screen.h"
Queue *psauxq;
static void
psauxputc(int c, int)
{
uchar uc;
uc = c;
qproduce(psauxq, &uc, 1);
}
static long
psauxread(Chan*, void *a, long n, vlong)
{
return qread(psauxq, a, n);
}
static long
psauxwrite(Chan*, void *a, long n, vlong)
{
return i8042auxcmds(a, n);
}
void
psauxlink(void)
{
psauxq = qopen(1024, 0, 0, 0);
if(psauxq == nil)
panic("psauxlink");
qnoblock(psauxq, 1);
i8042auxenable(psauxputc);
addarchfile("psaux", DMEXCL|0660, psauxread, psauxwrite);
}