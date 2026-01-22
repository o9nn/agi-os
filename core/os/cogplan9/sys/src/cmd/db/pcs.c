#include "defs.h"
#include "fns.h"
char	NOPCS[] = "no process";
void
subpcs(int modif)
{
int	check;
int	runmode;
int	keepnote;
int	n, r;
long line, curr;
BKPT *bk;
char *comptr;
runmode=SINGLE;
r = 0;
keepnote=0;
loopcnt=cntval;
switch (modif) {
case 'd':
case 'D':
if ((bk=scanbkpt(dot)) == 0)
error("no breakpoint set");
bk->flag=BKPTCLR;
return;
case 'b':
case 'B':
if (bk=scanbkpt(dot))
bk->flag=BKPTCLR;
for (bk=bkpthead; bk; bk=bk->nxtbkpt)
if (bk->flag == BKPTCLR)
break;
if (bk==0) {
bk = (BKPT *)malloc(sizeof(*bk));
if (bk == 0)
error("too many breakpoints");
bk->nxtbkpt=bkpthead;
bkpthead=bk;
}
bk->loc = dot;
bk->initcnt = bk->count = cntval;
bk->flag = modif == 'b' ? BKPTSET : BKPTTMP;
check=MAXCOM-1;
comptr=bk->comm;
rdc();
reread();
do {
*comptr++ = readchar();
} while (check-- && lastc!=EOR);
*comptr=0;
if(bk->comm[0] != EOR && cntflg == FALSE)
bk->initcnt = bk->count = HUGEINT;
reread();
if (check)
return;
error("bkpt command too long");
case 'k' :
case 'K':
if (pid == 0)
error(NOPCS);
dprint("%d: killed", pid);
pcsactive = 1;
endpcs();
return;
case 'r':
case 'R':
endpcs();
setup();
runmode = CONTIN;
break;
case 's':
if (pid == 0) {
setup();
loopcnt--;
}
runmode=SINGLE;
keepnote=defval(1);
break;
case 'S':
if (pid == 0) {
setup();
loopcnt--;
}
keepnote=defval(1);
line = pc2line(rget(cormap, mach->pc));
n = loopcnt;
dprint("%s: running\n", symfil);
flush();
for (loopcnt = 1; n > 0; loopcnt = 1) {
r = runpcs(SINGLE, keepnote);
curr = pc2line(dot);
if (line != curr) {
line = curr;
n--;
}
}
loopcnt = 0;
break;
case 'c':
case 'C':
if (pid==0)
error(NOPCS);
runmode=CONTIN;
keepnote=defval(1);
break;
case 'n':
if (pid==0)
error(NOPCS);
n=defval(-1);
if(n>=0 && n<nnote){
nnote--;
memmove(note[n], note[n+1], (nnote-n)*sizeof(note[0]));
}
notes();
return;
case 'h':
if (adrflg && adrval == 0) {
if (pid == 0)
error(NOPCS);
ungrab();
}
else {
grab();
dprint("stopped at%16t");
goto Return;
}
return;
case 'x':
if (pid == 0)
error(NOPCS);
ungrab();
return;
default:
error("bad `:' command");
}
if (loopcnt>0) {
dprint("%s: running\n", symfil);
flush();
r = runpcs(runmode,keepnote);
}
if (r)
dprint("breakpoint%16t");
else
dprint("stopped at%16t");
Return:
delbp();
printpc();
notes();
}