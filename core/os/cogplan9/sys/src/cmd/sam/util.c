#include "sam.h"
void
cvttorunes(char *p, int n, Rune *r, int *nb, int *nr, int *nulls)
{
uchar *q;
Rune *s;
int j, w;
q = (uchar*)p;
s = r;
for(j=0; j<n; j+=w){
if(*q < Runeself){
w = 1;
*s = *q++;
}else{
w = chartorune(s, (char*)q);
q += w;
}
if(*s)
s++;
else if(nulls)
*nulls = TRUE;
}
*nb = (char*)q-p;
*nr = s-r;
}
void*
fbufalloc(void)
{
return emalloc(BUFSIZE);
}
void
fbuffree(void *f)
{
free(f);
}
uint
min(uint a, uint b)
{
if(a < b)
return a;
return b;
}