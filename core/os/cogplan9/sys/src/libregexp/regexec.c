#include <u.h>
#include <libc.h>
#include "regexp.h"
#include "regcomp.h"
static int
regexec1(Reprog *progp,
char *bol,
Resub *mp,
int ms,
Reljunk *j
)
{
int flag=0;
Reinst *inst;
Relist *tlp;
char *s;
int i, checkstart;
Rune r, *rp, *ep;
int n;
Relist* tl;
Relist* nl;
Relist* tle;
Relist* nle;
int match;
char *p;
match = 0;
checkstart = j->starttype;
if(mp)
for(i=0; i<ms; i++) {
mp[i].sp = 0;
mp[i].ep = 0;
}
j->relist[0][0].inst = 0;
j->relist[1][0].inst = 0;
s = j->starts;
do{
if(checkstart) {
switch(j->starttype) {
case RUNE:
p = utfrune(s, j->startchar);
if(p == 0 || s == j->eol)
return match;
s = p;
break;
case BOL:
if(s == bol)
break;
p = utfrune(s, '\n');
if(p == 0 || s == j->eol)
return match;
s = p+1;
break;
}
}
r = *(uchar*)s;
if(r < Runeself)
n = 1;
else
n = chartorune(&r, s);
tl = j->relist[flag];
tle = j->reliste[flag];
nl = j->relist[flag^=1];
nle = j->reliste[flag];
nl->inst = 0;
if(match == 0)
_renewemptythread(tl, progp->startinst, ms, s);
for(tlp=tl; tlp->inst; tlp++){
for(inst = tlp->inst; ; inst = inst->next){
switch(inst->type){
case RUNE:
if(inst->r == r){
if(_renewthread(nl, inst->next, ms, &tlp->se)==nle)
return -1;
}
break;
case LBRA:
tlp->se.m[inst->subid].sp = s;
continue;
case RBRA:
tlp->se.m[inst->subid].ep = s;
continue;
case ANY:
if(r != '\n')
if(_renewthread(nl, inst->next, ms, &tlp->se)==nle)
return -1;
break;
case ANYNL:
if(_renewthread(nl, inst->next, ms, &tlp->se)==nle)
return -1;
break;
case BOL:
if(s == bol || *(s-1) == '\n')
continue;
break;
case EOL:
if(s == j->eol || r == 0 || r == '\n')
continue;
break;
case CCLASS:
ep = inst->cp->end;
for(rp = inst->cp->spans; rp < ep; rp += 2)
if(r >= rp[0] && r <= rp[1]){
if(_renewthread(nl, inst->next, ms, &tlp->se)==nle)
return -1;
break;
}
break;
case NCCLASS:
ep = inst->cp->end;
for(rp = inst->cp->spans; rp < ep; rp += 2)
if(r >= rp[0] && r <= rp[1])
break;
if(rp == ep)
if(_renewthread(nl, inst->next, ms, &tlp->se)==nle)
return -1;
break;
case OR:
if(_renewthread(tlp, inst->right, ms, &tlp->se) == tle)
return -1;
continue;
case END:
match = 1;
tlp->se.m[0].ep = s;
if(mp != 0)
_renewmatch(mp, ms, &tlp->se);
break;
}
break;
}
}
if(s == j->eol)
break;
checkstart = j->starttype && nl->inst==0;
s += n;
}while(r);
return match;
}
static int
regexec2(Reprog *progp,
char *bol,
Resub *mp,
int ms,
Reljunk *j
)
{
int rv;
Relist *relist0, *relist1;
relist0 = malloc(BIGLISTSIZE*sizeof(Relist));
if(relist0 == nil)
return -1;
relist1 = malloc(BIGLISTSIZE*sizeof(Relist));
if(relist1 == nil){
free(relist1);
return -1;
}
j->relist[0] = relist0;
j->relist[1] = relist1;
j->reliste[0] = relist0 + BIGLISTSIZE - 2;
j->reliste[1] = relist1 + BIGLISTSIZE - 2;
rv = regexec1(progp, bol, mp, ms, j);
free(relist0);
free(relist1);
return rv;
}
extern int
regexec(Reprog *progp,
char *bol,
Resub *mp,
int ms)
{
Reljunk j;
Relist relist0[LISTSIZE], relist1[LISTSIZE];
int rv;
j.starts = bol;
j.eol = 0;
if(mp && ms>0){
if(mp->sp)
j.starts = mp->sp;
if(mp->ep)
j.eol = mp->ep;
}
j.starttype = 0;
j.startchar = 0;
if(progp->startinst->type == RUNE && progp->startinst->r < Runeself) {
j.starttype = RUNE;
j.startchar = progp->startinst->r;
}
if(progp->startinst->type == BOL)
j.starttype = BOL;
j.relist[0] = relist0;
j.relist[1] = relist1;
j.reliste[0] = relist0 + nelem(relist0) - 2;
j.reliste[1] = relist1 + nelem(relist1) - 2;
rv = regexec1(progp, bol, mp, ms, &j);
if(rv >= 0)
return rv;
rv = regexec2(progp, bol, mp, ms, &j);
if(rv >= 0)
return rv;
return -1;
}