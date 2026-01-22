#include	"u.h"
#include	"../port/lib.h"
struct cvlist
{
char	*ld;
char	*si;
Rune	*so;
} latintab[] = {
#include "../port/latin1.h"
0,	0,		0
};
long
unicode(Rune *k, int n)
{
long c;
Rune *r;
c = 0;
for(r = &k[1]; r<&k[n]; r++){
c <<= 4;
if('0'<=*r && *r<='9')
c += *r-'0';
else if('a'<=*r && *r<='f')
c += 10 + *r-'a';
else if('A'<=*r && *r<='F')
c += 10 + *r-'A';
else
return -1;
}
return c;
}
long
latin1(Rune *k, int n)
{
struct cvlist *l;
int c;
char* p;
if(k[0] == 'X')
if(n>=5)
return unicode(k, 5);
else
return -5;
if(k[0] == 'x')
if(n>=UTFmax*2+1)
return unicode(k, UTFmax*2+1);
else
return -(UTFmax+1);
for(l=latintab; l->ld!=0; l++)
if(k[0] == l->ld[0]){
if(n == 1)
return -2;
if(l->ld[1] == 0)
c = k[1];
else if(l->ld[1] != k[1])
continue;
else if(n == 2)
return -3;
else
c = k[2];
for(p=l->si; *p!=0; p++)
if(*p == c)
return l->so[p - l->si];
return -1;
}
return -1;
}