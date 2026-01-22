#include "os.h"
#include <mp.h>
#include "dat.h"
int
mpmagcmp(mpint *b1, mpint *b2)
{
int i;
i = b1->top - b2->top;
if(i)
return i;
return mpveccmp(b1->p, b1->top, b2->p, b2->top);
}
int
mpcmp(mpint *b1, mpint *b2)
{
if(b1->sign != b2->sign)
return b1->sign - b2->sign;
if(b1->sign < 0)
return mpmagcmp(b2, b1);
else
return mpmagcmp(b1, b2);
}