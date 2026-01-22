#include <lib9.h>
#include "regexp.h"
#include "regcomp.h"
extern void
_renewmatch(Resub *mp, int ms, Resublist *sp)
{
int i;
if(mp==0 || ms<=0)
return;
if(mp[0].s.sp==0 || sp->m[0].s.sp<mp[0].s.sp ||
(sp->m[0].s.sp==mp[0].s.sp && sp->m[0].e.ep>mp[0].e.ep)){
for(i=0; i<ms && i<NSUBEXP; i++)
mp[i] = sp->m[i];
for(; i<ms; i++)
mp[i].s.sp = mp[i].e.ep = 0;
}
}
extern Relist*
_renewthread(Relist *lp,
Reinst *ip,
Resublist *sep)
{
Relist *p;
for(p=lp; p->inst; p++){
if(p->inst == ip){
if((sep)->m[0].s.sp < p->se.m[0].s.sp)
p->se = *sep;
return 0;
}
}
p->inst = ip;
p->se = *sep;
(++p)->inst = 0;
return p;
}
extern Relist*
_renewemptythread(Relist *lp,
Reinst *ip,
char *sp)
{
Relist *p;
for(p=lp; p->inst; p++){
if(p->inst == ip){
if(sp < p->se.m[0].s.sp) {
memset((void *)&p->se, 0, sizeof(p->se));
p->se.m[0].s.sp = sp;
}
return 0;
}
}
p->inst = ip;
memset((void *)&p->se, 0, sizeof(p->se));
p->se.m[0].s.sp = sp;
(++p)->inst = 0;
return p;
}
extern Relist*
_rrenewemptythread(Relist *lp,
Reinst *ip,
Rune *rsp)
{
Relist *p;
for(p=lp; p->inst; p++){
if(p->inst == ip){
if(rsp < p->se.m[0].s.rsp) {
memset((void *)&p->se, 0, sizeof(p->se));
p->se.m[0].s.rsp = rsp;
}
return 0;
}
}
p->inst = ip;
memset((void *)&p->se, 0, sizeof(p->se));
p->se.m[0].s.rsp = rsp;
(++p)->inst = 0;
return p;
}