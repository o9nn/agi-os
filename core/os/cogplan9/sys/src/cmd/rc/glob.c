#include "rc.h"
#include "exec.h"
#include "fns.h"
char *globname;
struct word *globv;
void
deglob(void *as)
{
char *s = as;
char *t = s;
do{
if(*t==GLOB)
t++;
*s++=*t;
}while(*t++);
}
int
globcmp(const void *s, const void *t)
{
return strcmp(*(char**)s, *(char**)t);
}
void
globsort(word *left, word *right)
{
char **list;
word *a;
int n = 0;
for(a = left;a!=right;a = a->next) n++;
list = (char **)emalloc(n*sizeof(char *));
for(a = left,n = 0;a!=right;a = a->next,n++) list[n] = a->word;
qsort((void *)list, n, sizeof(void *), globcmp);
for(a = left,n = 0;a!=right;a = a->next,n++) a->word = list[n];
efree((char *)list);
}
void
globdir(uchar *p, uchar *namep)
{
uchar *t, *newp;
int f;
if(*p=='\0'){
globv = newword(globname, globv);
return;
}
t = namep;
newp = p;
while(*newp){
if(*newp==GLOB)
break;
*t=*newp++;
if(*t++=='/'){
namep = t;
p = newp;
}
}
if(*newp=='\0'){
*t='\0';
if(access(globname, 0)==0)
globv = newword(globname, globv);
return;
}
*namep='\0';
if((f = Opendir(globname[0]?globname:"."))<0) return;
while(*newp!='/' && *newp!='\0') newp++;
while(Readdir(f, namep, *newp=='/')){
if(matchfn(namep, p)){
for(t = namep;*t;t++);
globdir(newp, t);
}
}
Closedir(f);
}
void
glob(void *ap)
{
uchar *p = ap;
word *svglobv = globv;
int globlen = Globsize(ap);
if(!globlen){
deglob(p);
globv = newword((char *)p, globv);
return;
}
globname = emalloc(globlen);
globname[0]='\0';
globdir(p, (uchar *)globname);
efree(globname);
if(svglobv==globv){
deglob(p);
globv = newword((char *)p, globv);
}
else
globsort(globv, svglobv);
}
int
equtf(uchar *p, uchar *q)
{
Rune pr, qr;
if(*p!=*q)
return 0;
chartorune(&pr, (char*)p);
chartorune(&qr, (char*)q);
return pr == qr;
}
uchar*
nextutf(uchar *p)
{
Rune dummy;
return p + chartorune(&dummy, (char*)p);
}
int
unicode(uchar *p)
{
Rune r;
chartorune(&r, (char*)p);
return r;
}
int
matchfn(void *as, void *ap)
{
uchar *s = as, *p = ap;
if(s[0]=='.' && (s[1]=='\0' || s[1]=='.' && s[2]=='\0') && p[0]!='.')
return 0;
return match(s, p, '/');
}
int
match(void *as, void *ap, int stop)
{
int compl, hit, lo, hi, t, c;
uchar *s = as, *p = ap;
for(; *p!=stop && *p!='\0'; s = nextutf(s), p = nextutf(p)){
if(*p!=GLOB){
if(!equtf(p, s)) return 0;
}
else switch(*++p){
case GLOB:
if(*s!=GLOB)
return 0;
break;
case '*':
for(;;){
if(match(s, nextutf(p), stop)) return 1;
if(!*s)
break;
s = nextutf(s);
}
return 0;
case '?':
if(*s=='\0')
return 0;
break;
case '[':
if(*s=='\0')
return 0;
c = unicode(s);
p++;
compl=*p=='~';
if(compl)
p++;
hit = 0;
while(*p!=']'){
if(*p=='\0')
return 0;
lo = unicode(p);
p = nextutf(p);
if(*p!='-')
hi = lo;
else{
p++;
if(*p=='\0')
return 0;
hi = unicode(p);
p = nextutf(p);
if(hi<lo){ t = lo; lo = hi; hi = t; }
}
if(lo<=c && c<=hi)
hit = 1;
}
if(compl)
hit=!hit;
if(!hit)
return 0;
break;
}
}
return *s=='\0';
}
void
globlist1(word *gl)
{
if(gl){
globlist1(gl->next);
glob(gl->word);
}
}
void
globlist(void)
{
word *a;
globv = 0;
globlist1(runq->argv->words);
poplist();
pushlist();
if(globv){
for(a = globv;a->next;a = a->next);
a->next = runq->argv->words;
runq->argv->words = globv;
}
}