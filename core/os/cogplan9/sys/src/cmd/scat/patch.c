#include <u.h>
#include <libc.h>
#include <bio.h>
#include "sky.h"
void
radec(int p, int *rah, int *ram, int *deg)
{
*deg = (p&255)-90;
p >>= 8;
*rah = p/15;
*ram = (p%15)*4;
if(*deg<0)
(*deg)++;
}
long
patcha(Angle ra, Angle dec)
{
ra = DEG(ra);
dec = DEG(dec);
if(dec >= 0)
return patch(floor(ra/15), ((int)floor(ra*4))%60, floor(dec));
dec = -dec;
return patch(floor(ra/15), ((int)floor(ra*4))%60, -floor(dec));
}
char round[91]={
0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
6, 6, 6, 6, 6, 12, 12, 15, 30, -1,
};
long
patch(int rah, int ram, int deg)
{
int ra, dec;
if(rah<0 || rah>=24 || ram<0 || abs(deg)>=90){
fprint(2, "scat: patch: bad ra or dec %dh%dm %d\n", rah, ram, deg);
abort();
}
if(deg < 0)
deg--;
else if(deg < 90)
deg++;
dec = deg+90;
deg = abs(deg);
if(deg<1 || deg>90){
fprint(2, "scat: patch: panic %dh%dm %d\n", rah, ram, deg);
abort();
}
if(deg == 90)
ra = 180;
else{
ra = 15*rah+ram/4;
ra -= ra%round[deg];
}
if(dec > 90)
--dec;
if(ra >= 360)
ra -= 360;
return (ra<<8)|dec;
}