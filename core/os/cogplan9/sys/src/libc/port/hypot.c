#include <u.h>
#include <libc.h>
double
hypot(double p, double q)
{
double r, s, pfac;
if(p < 0)
p = -p;
if(q < 0)
q = -q;
if(p < q) {
r = p;
p = q;
q = r;
}
if(p == 0)
return 0;
pfac = p;
r = q = q/p;
p = 1;
for(;;) {
r *= r;
s = r+4;
if(s == 4)
return p*pfac;
r /= s;
p += 2*r*p;
q *= r;
r = q/p;
}
}