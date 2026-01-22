#include "cc.h"
double
convvtof(vlong v)
{
double d;
d = v;
return d;
}
vlong
convftov(double d)
{
vlong v;
v = d;
return v;
}
double
convftox(double d, int et)
{
if(!typefd[et])
diag(Z, "bad type in castftox %s", tnames[et]);
return d;
}
vlong
convvtox(vlong c, int et)
{
int n;
n = 8 * ewidth[et];
c &= MASK(n);
if(!typeu[et])
if(c & SIGN(n))
c |= ~MASK(n);
return c;
}