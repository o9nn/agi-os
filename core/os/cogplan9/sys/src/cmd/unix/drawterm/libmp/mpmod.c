#include "os.h"
#include <mp.h>
#include "dat.h"
void
mpmod(mpint *b, mpint *m, mpint *remainder)
{
mpdiv(b, m, nil, remainder);
if(remainder->sign < 0)
mpadd(m, remainder, remainder);
}