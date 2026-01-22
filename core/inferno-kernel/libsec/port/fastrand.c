#include	"os.h"
#include	<libsec.h>
ulong
fastrand(void)
{
ulong x;
genrandom((uchar*)&x, sizeof x);
return x;
}