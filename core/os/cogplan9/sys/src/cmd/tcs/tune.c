#include <u.h>
#include <libc.h>
#include <bio.h>
#include "hdr.h"
#include "conv.h"
typedef struct Tmap Tmap;
struct Tmap
{
Rune u;
Rune t;
};
static Tmap t1[] =
{
{0x0b85, 0xe201},
{0x0b86, 0xe202},
{0x0b87, 0xe203},
{0x0b88, 0xe204},
{0x0b89, 0xe205},
{0x0b8a, 0xe206},
{0x0b8e, 0xe207},
{0x0b8f, 0xe208},
{0x0b90, 0xe209},
{0x0b92, 0xe20a},
{0x0b93, 0xe20b},
{0x0b94, 0xe20c},
{0x0b83, 0xe20d}
};
static Rune t2[] =
{
0x0bcd,
0x0bcd,
0x0bbe,
0x0bbf,
0x0bc0,
0x0bc1,
0x0bc2,
0x0bc6,
0x0bc7,
0x0bc8,
0x0bca,
0x0bcb,
0x0bcc
};
static Tmap t3[] =
{
{0x0b95, 0xe211},
{0x0b99, 0xe221},
{0x0b9a, 0xe231},
{0x0b9c, 0xe331},
{0x0b9e, 0xe241},
{0x0b9f, 0xe251},
{0x0ba3, 0xe261},
{0x0ba4, 0xe271},
{0x0ba8, 0xe281},
{0x0ba9, 0xe321},
{0x0baa, 0xe291},
{0x0bae, 0xe2a1},
{0x0baf, 0xe2b1},
{0x0bb0, 0xe2c1},
{0x0bb1, 0xe311},
{0x0bb2, 0xe2d1},
{0x0bb3, 0xe301},
{0x0bb4, 0xe2f1},
{0x0bb5, 0xe2e1},
{0x0bb6, 0xe341},
{0x0bb7, 0xe351},
{0x0bb8, 0xe361},
{0x0bb9, 0xe371}
};
static Rune
findbytune(Tmap *tab, int size, Rune t)
{
int i;
for(i = 0; i < size; i++)
if(tab[i].t == t)
return tab[i].u;
return Runeerror;
}
static Rune
findbyuni(Tmap *tab, int size, Rune u)
{
int i;
for(i = 0; i < size; i++)
if(tab[i].u == u)
return tab[i].t;
return Runeerror;
}
static int
findindex(Rune *rstr, int size, Rune r)
{
int i;
for(i = 0; i < size; i++)
if(rstr[i] == r)
return i;
return -1;
}
void
tune_in(int fd, long *x, struct convert *out)
{
Biobuf b;
Rune rbuf[N];
Rune *r, *er, tr;
int c, i;
USED(x);
r = rbuf;
er = rbuf+N-3;
Binit(&b, fd, OREAD);
while((c = Bgetrune(&b)) != Beof){
ninput += b.runesize;
if(r >= er){
OUT(out, rbuf, r-rbuf);
r = rbuf;
}
if(c>=0xe210 && c <= 0xe38c && (i = c%16) < nelem(t2)){
if(c >= 0xe380){
*r++ = 0x0b95;
*r++ = 0x0bcd;
*r++ = 0x0bb7;
}else
*r++ = findbytune(t3, nelem(t3), c-i+1);
if(i != 1)
*r++ = t2[i];
}else if((tr = findbytune(t1, nelem(t1), c)) != Runeerror)
*r++ = tr;
else switch(c){
case 0xe3d0:
*r++ = 0x0ba3; *r++ = 0x0bbe;
break;
case 0xe3d1:
*r++ = 0x0bb1; *r++ = 0x0bbe;
break;
case 0xe3d2:
*r++ = 0x0ba9; *r++ = 0x0bbe;
break;
case 0xe3d4:
*r++ = 0x0ba3; *r++ = 0x0bc8;
break;
case 0xe3d5:
*r++ = 0x0bb2; *r++ = 0x0bc8;
break;
case 0xe3d6:
*r++ = 0x0bb3; *r++ = 0x0bc8;
break;
case 0xe3d7:
*r++ = 0x0ba9; *r++ = 0x0bc8;
break;
case 0xe38d:
*r++ = 0x0bb6; *r++ = 0x0bcd; *r++ = 0x0bb0; *r++ = 0x0bc0;
break;
default:
if(c >= 0xe200 && c <= 0xe3ff){
if(squawk)
EPR "%s: rune 0x%x not in output cs\n", argv0, c);
nerrors++;
if(clean)
break;
c = BADMAP;
}
*r++ = c;
break;
}
}
if(r > rbuf)
OUT(out, rbuf, r-rbuf);
OUT(out, rbuf, 0);
}
void
tune_out(Rune *r, int n, long *x)
{
static int state = 0;
static Rune lastr;
Rune *er, tr, rr;
char *p;
int i;
USED(x);
nrunes += n;
er = r+n;
for(p = obuf; r < er; r++){
switch(state){
case 0:
case0:
if((tr = findbyuni(t3, nelem(t3), *r)) != Runeerror){
lastr = tr;
state = 1;
}else if(*r == 0x0b92){
lastr = 0xe20a;
state = 3;
}else if((tr = findbyuni(t1, nelem(t1), *r)) != Runeerror)
p += runetochar(p, &tr);
else
p += runetochar(p, r);
break;
case 1:
case1:
if((i = findindex(t2, nelem(t2), *r)) != -1){
if(lastr && lastr != Runeerror)
lastr += i-1;
if(*r ==0x0bc6)
state = 5;
else if(*r ==0x0bc7)
state = 4;
else if(lastr == 0xe210)
state = 2;
else if(lastr == 0xe340)
state = 6;
else{
if(lastr)
p += runetochar(p, &lastr);
state = 0;
}
}else if(lastr && lastr != Runeerror && (*r == 0x00b2 || *r == 0x00b3 || *r == 0x2074)){
if(squawk)
EPR "%s: character <U+%.4X, U+%.4X> not in output cs\n", argv0, lastr, *r);
lastr = clean ? 0 : Runeerror;
nerrors++;
}else{
if(lastr)
p += runetochar(p, &lastr);
state = 0;
goto case0;
}
break;
case 2:
if(*r == 0x0bb7){
lastr = 0xe381;
state = 1;
break;
}
p += runetochar(p, &lastr);
state = 0;
goto case0;
case 3:
state = 0;
if(*r == 0x0bd7){
rr = 0xe20c;
p += runetochar(p, &rr);
break;
}
p += runetochar(p, &lastr);
goto case0;
case 4:
state = 0;
if(*r == 0x0bbe){
if(lastr){
if(lastr != Runeerror)
lastr += 3;
p += runetochar(p, &lastr);
}
break;
}
if(lastr)
p += runetochar(p, &lastr);
goto case0;
case 5:
state = 0;
if(*r == 0x0bbe || *r == 0x0bd7){
if(lastr){
if(lastr != Runeerror)
lastr += *r == 0x0bbe ? 3 : 5;
p += runetochar(p, &lastr);
}
break;
}
if(lastr)
p += runetochar(p, &lastr);
goto case0;
case 6:
if(*r == 0x0bb0){
state = 7;
break;
}
p += runetochar(p, &lastr);
state = 0;
goto case0;
case 7:
if(*r == 0x0bc0){
rr = 0xe38d;
p += runetochar(p, &rr);
state = 0;
break;
}
p += runetochar(p, &lastr);
lastr = 0xe2c1;
state = 1;
goto case1;
}
}
if(n == 0 && state != 0){
if(lastr)
p += runetochar(p, &lastr);
state = 0;
}
noutput += p-obuf;
write(1, obuf, p-obuf);
}