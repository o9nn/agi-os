#include	<plan9.h>
char *argv0;
enum
{
Bit1	= 7,
Bitx	= 6,
Bit2	= 5,
Bit3	= 4,
Bit4	= 3,
T1	= ((1<<(Bit1+1))-1) ^ 0xFF,
Tx	= ((1<<(Bitx+1))-1) ^ 0xFF,
T2	= ((1<<(Bit2+1))-1) ^ 0xFF,
T3	= ((1<<(Bit3+1))-1) ^ 0xFF,
T4	= ((1<<(Bit4+1))-1) ^ 0xFF,
T5	= ((1<<(Bit5+1))-1) ^ 0xFF,
Rune1	= (1<<(Bit1+0*Bitx))-1,
Rune2	= (1<<(Bit2+1*Bitx))-1,
Rune3	= (1<<(Bit3+2*Bitx))-1,
Rune4	= (1<<(Bit4+3*Bitx))-1,
Maskx	= (1<<Bitx)-1,
Testx	= Maskx ^ 0xFF,
SurrogateMin	= 0xD800,
SurrogateMax	= 0xDFFF,
Bad	= Runeerror
};
int
chartorune(Rune *rune, char *str)
{
int c, c1, c2, c3;
long l;
c = *(uchar*)str;
if(c < Tx) {
*rune = c;
return 1;
}
c1 = *(uchar*)(str+1) ^ Tx;
if(c1 & Testx)
goto bad;
if(c < T3) {
if(c < T2)
goto bad;
l = ((c << Bitx) | c1) & Rune2;
if(l <= Rune1)
goto bad;
*rune = l;
return 2;
}
c2 = *(uchar*)(str+2) ^ Tx;
if(c2 & Testx)
goto bad;
if(c < T4) {
l = ((((c << Bitx) | c1) << Bitx) | c2) & Rune3;
if(l <= Rune2)
goto bad;
if (SurrogateMin <= l && l <= SurrogateMax)
goto bad;
*rune = l;
return 3;
}
if(UTFmax >= 4) {
c3 = *(uchar*)(str+3) ^ Tx;
if(c3 & Testx)
goto bad;
if(c < T5) {
l = ((((((c << Bitx) | c1) << Bitx) | c2) << Bitx) | c3) & Rune4;
if(l <= Rune3)
goto bad;
if(l > Runemax)
goto bad;
*rune = l;
return 4;
}
}
bad:
*rune = Bad;
return 1;
}
int
runetochar(char *str, Rune *rune)
{
long c;
c = *rune;
if(c <= Rune1) {
str[0] = c;
return 1;
}
if(c <= Rune2) {
str[0] = T2 | (c >> 1*Bitx);
str[1] = Tx | (c & Maskx);
return 2;
}
if (c > Runemax)
c = Runeerror;
if (SurrogateMin <= c && c <= SurrogateMax)
c = Runeerror;
if (c <= Rune3) {
str[0] = T3 |  (c >> 2*Bitx);
str[1] = Tx | ((c >> 1*Bitx) & Maskx);
str[2] = Tx |  (c & Maskx);
return 3;
}
str[0] = T4 | (c >> 3*Bitx);
str[1] = Tx | ((c >> 2*Bitx) & Maskx);
str[2] = Tx | ((c >> 1*Bitx) & Maskx);
str[3] = Tx | (c & Maskx);
return 4;
}
int
runelen(long c)
{
Rune rune;
char str[10];
rune = c;
return runetochar(str, &rune);
}
int
utflen(char *s)
{
int c;
long n;
Rune rune;
n = 0;
for(;;) {
c = *(uchar*)s;
if(c < Runeself) {
if(c == 0)
return n;
s++;
} else
s += chartorune(&rune, s);
n++;
}
return 0;
}