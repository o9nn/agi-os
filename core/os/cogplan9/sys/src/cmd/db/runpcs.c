#include "defs.h"
#include "fns.h"
BKPT *bkpthead;
BOOL bpin;
int pid;
int nnote;
int ending;
char note[NNOTE][ERRMAX];
runpcs(int runmode, int keepnote)
{
int rc;
BKPT *bkpt;
rc = 0;
if (adrflg)
rput(cormap, mach->pc, dot);
dot = rget(cormap, mach->pc);
flush();
while (loopcnt-- > 0) {
if(loopcnt != 0)
printpc();
if (runmode == SINGLE) {
bkpt = scanbkpt(dot);
if (bkpt) {
switch(bkpt->flag){
case BKPTTMP:
bkpt->flag = BKPTCLR;
break;
case BKPTSKIP:
bkpt->flag = BKPTSET;
break;
}
}
runstep(dot, keepnote);
} else {
if ((bkpt = scanbkpt(rget(cormap, mach->pc))) != 0) {
execbkpt(bkpt, keepnote);
keepnote = 0;
}
setbp();
runrun(keepnote);
}
keepnote = 0;
delbp();
dot = rget(cormap, mach->pc);
if (nnote > 0) {
keepnote = 1;
rc = 0;
continue;
}
bkpt = scanbkpt(dot);
if(bkpt == 0){
keepnote = 0;
rc = 0;
continue;
}
if (bkpt->flag == BKPTTMP)
bkpt->flag = BKPTCLR;
else if (bkpt->flag == BKPTSKIP) {
execbkpt(bkpt, keepnote);
keepnote = 0;
loopcnt++;
continue;
}
else {
bkpt->flag = BKPTSKIP;
--bkpt->count;
if ((bkpt->comm[0] == EOR || command(bkpt->comm, ':') != 0)
&&  bkpt->count != 0) {
execbkpt(bkpt, keepnote);
keepnote = 0;
loopcnt++;
continue;
}
bkpt->count = bkpt->initcnt;
}
rc = 1;
}
return(rc);
}
void
endpcs(void)
{
BKPT *bk;
if(ending)
return;
ending = 1;
if (pid) {
if(pcsactive){
killpcs();
pcsactive = 0;
}
pid=0;
nnote=0;
for (bk=bkpthead; bk; bk = bk->nxtbkpt)
if (bk->flag == BKPTTMP)
bk->flag = BKPTCLR;
else if (bk->flag != BKPTCLR)
bk->flag = BKPTSET;
}
bpin = FALSE;
ending = 0;
}
void
setup(void)
{
nnote = 0;
startpcs();
bpin = FALSE;
pcsactive = 1;
}
void
execbkpt(BKPT *bk, int keepnote)
{
runstep(bk->loc, keepnote);
bk->flag = BKPTSET;
}
BKPT *
scanbkpt(ADDR adr)
{
BKPT *bk;
for (bk = bkpthead; bk; bk = bk->nxtbkpt)
if (bk->flag != BKPTCLR && bk->loc == adr)
break;
return(bk);
}
void
delbp(void)
{
BKPT *bk;
if (bpin == FALSE || pid == 0)
return;
for (bk = bkpthead; bk; bk = bk->nxtbkpt)
if (bk->flag != BKPTCLR)
bkput(bk, 0);
bpin = FALSE;
}
void
setbp(void)
{
BKPT *bk;
if (bpin == TRUE || pid == 0)
return;
for (bk = bkpthead; bk; bk = bk->nxtbkpt)
if (bk->flag != BKPTCLR)
bkput(bk, 1);
bpin = TRUE;
}