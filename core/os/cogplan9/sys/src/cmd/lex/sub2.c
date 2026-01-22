# include "ldefs.h"
void
cfoll(int v)
{
int i,j,k;
uchar *p;
i = name[v];
if(i < NCH) i = 1;
switch(i){
case 1: case RSTR: case RCCL: case RNCCL: case RNULLS:
for(j=0;j<tptr;j++)
tmpstat[j] = FALSE;
count = 0;
follow(v);
# ifdef PP
padd(foll,v);
# endif
# ifndef PP
add(foll,v);
# endif
if(i == RSTR) cfoll(left[v]);
else if(i == RCCL || i == RNCCL){
for(j=1; j<NCH;j++)
symbol[j] = (i==RNCCL);
p = ptr[v];
while(*p)
symbol[*p++] = (i == RCCL);
p = pcptr;
for(j=1;j<NCH;j++)
if(symbol[j]){
for(k=0;p+k < pcptr; k++)
if(cindex[j] == *(p+k))
break;
if(p+k >= pcptr)*pcptr++ = cindex[j];
}
*pcptr++ = 0;
if(pcptr > pchar + pchlen)
error("Too many packed character classes");
ptr[v] = p;
name[v] = RCCL;
# ifdef DEBUG
if(debug && *p){
print("ccl %d: %d",v,*p++);
while(*p)
print(", %d",*p++);
print("\n");
}
# endif
}
break;
case CARAT:
cfoll(left[v]);
break;
case STAR: case PLUS: case QUEST: case RSCON:
cfoll(left[v]);
break;
case BAR: case RCAT: case DIV: case RNEWE:
cfoll(left[v]);
cfoll(right[v]);
break;
# ifdef DEBUG
case FINAL:
case S1FINAL:
case S2FINAL:
break;
default:
warning("bad switch cfoll %d",v);
# endif
}
}
# ifdef DEBUG
void
pfoll(void)
{
int i,k,*p;
int j;
print("pos\tchars\n");
for(i=0;i<tptr;i++)
if(p=foll[i]){
j = *p++;
if(j >= 1){
print("%d:\t%d",i,*p++);
for(k=2;k<=j;k++)
print(", %d",*p++);
print("\n");
}
}
}
# endif
void
add(int **array, int n)
{
int i, *temp;
uchar *ctemp;
temp = nxtpos;
ctemp = tmpstat;
array[n] = nxtpos;
*temp++ = count;
for(i=0;i<tptr;i++)
if(ctemp[i] == TRUE)
*temp++ = i;
nxtpos = temp;
if(nxtpos >= positions+maxpos)
error("Too many positions %s",(maxpos== MAXPOS?"\nTry using %p num":""));
}
void
follow(int v)
{
int p;
if(v >= tptr-1)return;
p = parent[v];
if(p == 0) return;
switch(name[p]){
case RSTR:
if(tmpstat[p] == FALSE){
count++;
tmpstat[p] = TRUE;
}
break;
case STAR: case PLUS:
first(v);
follow(p);
break;
case BAR: case QUEST: case RNEWE:
follow(p);
break;
case RCAT: case DIV:
if(v == left[p]){
if(nullstr[right[p]])
follow(p);
first(right[p]);
}
else follow(p);
break;
case RSCON: case CARAT:
follow(p);
break;
# ifdef DEBUG
default:
warning("bad switch follow %d",p);
# endif
}
}
void
first(int v)
{
int i;
uchar *p;
i = name[v];
if(i < NCH)i = 1;
switch(i){
case 1: case RCCL: case RNCCL: case RNULLS: case FINAL: case S1FINAL: case S2FINAL:
if(tmpstat[v] == FALSE){
count++;
tmpstat[v] = TRUE;
}
break;
case BAR: case RNEWE:
first(left[v]);
first(right[v]);
break;
case CARAT:
if(stnum % 2 == 1)
first(left[v]);
break;
case RSCON:
i = stnum/2 +1;
p = (uchar *)right[v];
while(*p)
if(*p++ == i){
first(left[v]);
break;
}
break;
case STAR: case QUEST: case PLUS:  case RSTR:
first(left[v]);
break;
case RCAT: case DIV:
first(left[v]);
if(nullstr[left[v]])
first(right[v]);
break;
# ifdef DEBUG
default:
warning("bad switch first %d",v);
# endif
}
}
void
cgoto(void)
{
int i, j, s;
int npos, curpos, n;
int tryit;
uchar tch[NCH];
int tst[NCH];
uchar *q;
Bprint(&fout,"int yyvstop[] = {\n0,\n");
while(stnum < 2 || stnum/2 < sptr){
for(i = 0; i<tptr; i++) tmpstat[i] = 0;
count = 0;
if(tptr > 0)first(tptr-1);
add(state,stnum);
# ifdef DEBUG
if(debug){
if(stnum > 1)
print("%s:\n",sname[stnum/2]);
pstate(stnum);
}
# endif
stnum++;
}
stnum--;
for(s = 0; s <= stnum; s++){
tryit = FALSE;
cpackflg[s] = FALSE;
sfall[s] = -1;
acompute(s);
for(i=0;i<NCH;i++) symbol[i] = 0;
npos = *state[s];
for(i = 1; i<=npos; i++){
curpos = *(state[s]+i);
if(name[curpos] < NCH) symbol[name[curpos]] = TRUE;
else switch(name[curpos]){
case RCCL:
tryit = TRUE;
q = ptr[curpos];
while(*q){
for(j=1;j<NCH;j++)
if(cindex[j] == *q)
symbol[j] = TRUE;
q++;
}
break;
case RSTR:
symbol[right[curpos]] = TRUE;
break;
# ifdef DEBUG
case RNULLS:
case FINAL:
case S1FINAL:
case S2FINAL:
break;
default:
warning("bad switch cgoto %d state %d",curpos,s);
break;
# endif
}
}
# ifdef DEBUG
if(debug){
print("State %d transitions on:\n\t",s);
charc = 0;
for(i = 1; i<NCH; i++){
if(symbol[i]) allprint(i);
if(charc > LINESIZE){
charc = 0;
print("\n\t");
}
}
print("\n");
}
# endif
n = 0;
for(i = 1; i<NCH; i++){
if(symbol[i]){
nextstate(s,i);
xstate = notin(stnum);
if(xstate == -2) warning("bad state  %d %o",s,i);
else if(xstate == -1){
if(stnum >= nstates)
error("Too many states %s",(nstates == NSTATES ? "\nTry using %n num":""));
add(state,++stnum);
# ifdef DEBUG
if(debug)pstate(stnum);
# endif
tch[n] = i;
tst[n++] = stnum;
} else {
tch[n] = i;
tst[n++] = xstate;
}
}
}
tch[n] = 0;
tst[n] = -1;
if(n > 0) packtrans(s,tch,tst,n,tryit);
else gotof[s] = -1;
}
Bprint(&fout,"0};\n");
}
void
nextstate(int s, int c)
{
int j, *newpos;
uchar *temp, *tz;
int *pos, i, *f, num, curpos, number;
num = *state[s];
temp = tmpstat;
pos = state[s] + 1;
for(i = 0; i<num; i++){
curpos = *pos++;
j = name[curpos];
if(j < NCH && j == c
|| j == RSTR && c == right[curpos]
|| j == RCCL && member(c, ptr[curpos])){
f = foll[curpos];
number = *f;
newpos = f+1;
for(j=0;j<number;j++)
temp[*newpos++] = 2;
}
}
j = 0;
tz = temp + tptr;
while(temp < tz){
if(*temp == 2){
j++;
*temp++ = 1;
}
else *temp++ = 0;
}
count = j;
}
int
notin(int n)
{
int *j,k;
uchar *temp;
int i;
if(count == 0)
return(-2);
temp = tmpstat;
for(i=n;i>=0;i--){
j = state[i];
if(count == *j++){
for(k=0;k<count;k++)
if(!temp[*j++])break;
if(k >= count)
return(i);
}
}
return(-1);
}
void
packtrans(int st, uchar *tch, int *tst, int cnt, int tryit)
{
int i,j,k;
int cmin, cval, tcnt, diff, p, *ast;
uchar *ach;
int go[NCH], temp[NCH], c;
int swork[NCH];
uchar cwork[NCH];
int upper;
rcount += cnt;
cmin = -1;
cval = NCH;
ast = tst;
ach = tch;
if(tryit){
for(i=1;i<NCH;i++){
go[i] = temp[i] = -1;
symbol[i] = 1;
}
for(i=0;i<cnt;i++){
go[tch[i]] = tst[i];
symbol[tch[i]] = 0;
}
for(i=0; i<cnt;i++){
c = match[tch[i]];
if(go[c] != tst[i] || c == tch[i])
temp[tch[i]] = tst[i];
}
for(i=1;i<NCH;i++)
if(symbol[i]) temp[i] = -2;
k = 0;
for(i=1;i<NCH;i++)
if(temp[i] != -1)k++;
if(k <cnt){
# ifdef DEBUG
if(debug) print("use compression  %d,  %d vs %d\n",st,k,cnt);
# endif
k = 0;
for(i=1;i<NCH;i++)
if(temp[i] != -1){
cwork[k] = i;
swork[k++] = (temp[i] == -2 ? -1 : temp[i]);
}
cwork[k] = 0;
# ifdef PC
ach = cwork;
ast = swork;
cnt = k;
cpackflg[st] = TRUE;
# endif
}
}
for(i=0; i<st; i++){
if(sfall[i] != -1) continue;
if(cpackflg[st] == 1) if(!(cpackflg[i] == 1)) continue;
p = gotof[i];
if(p == -1)  continue;
tcnt = nexts[p];
if(tcnt > cnt) continue;
diff = 0;
j = 0;
upper = p + tcnt;
while(ach[j] && p < upper){
while(ach[j] < nchar[p] && ach[j]){diff++; j++; }
if(ach[j] == 0)break;
if(ach[j] > nchar[p]){diff=NCH;break;}
if(ast[j] != nexts[++p] || ast[j] == -1 || (cpackflg[st] && ach[j] != match[ach[j]]))diff++;
j++;
}
while(ach[j]){
diff++;
j++;
}
if(p < upper)diff = NCH;
if(diff < cval && diff < tcnt){
cval = diff;
cmin = i;
if(cval == 0)break;
}
}
# ifdef DEBUG
if(debug)print("select st %d for st %d diff %d\n",cmin,st,cval);
# endif
# ifdef PS
if(cmin != -1){
gotof[st] = nptr;
k = 0;
sfall[st] = cmin;
p = gotof[cmin]+1;
j = 0;
while(ach[j]){
while(ach[j] < nchar[p-1] && ach[j]){
k++;
nchar[nptr] = ach[j];
nexts[++nptr] = ast[j];
j++;
}
if(nchar[p-1] == 0)break;
if(ach[j] > nchar[p-1]){
warning("bad transition %d %d",st,cmin);
goto nopack;
}
if(ast[j] != nexts[p] || ast[j] == -1 || (cpackflg[st] && ach[j] != match[ach[j]])){
k++;
nchar[nptr] = ach[j];
nexts[++nptr] = ast[j];
}
p++;
j++;
}
while(ach[j]){
nchar[nptr] = ach[j];
nexts[++nptr] = ast[j++];
k++;
}
nexts[gotof[st]] = cnt = k;
nchar[nptr++] = 0;
} else {
# endif
nopack:
gotof[st] = nptr;
nexts[nptr] = cnt;
for(i=0;i<cnt;i++){
nchar[nptr] = ach[i];
nexts[++nptr] = ast[i];
}
nchar[nptr++] = 0;
# ifdef PS
}
# endif
if(cnt < 1){
gotof[st] = -1;
nptr--;
} else
if(nptr > ntrans)
error("Too many transitions %s",(ntrans==NTRANS?"\nTry using %a num":""));
}
# ifdef DEBUG
void
pstate(int s)
{
int *p,i,j;
print("State %d:\n",s);
p = state[s];
i = *p++;
if(i == 0) return;
print("%4d",*p++);
for(j = 1; j<i; j++){
print(", %4d",*p++);
if(j%30 == 0)print("\n");
}
print("\n");
}
# endif
int
member(int d, uchar *t)
{
int c;
uchar *s;
c = d;
s = t;
c = cindex[c];
while(*s)
if(*s++ == c) return(1);
return(0);
}
# ifdef DEBUG
void
stprt(int i)
{
int p, t;
print("State %d:",i);
t = atable[i];
if(t != -1)print(" final");
print("\n");
if(cpackflg[i] == TRUE)print("backup char in use\n");
if(sfall[i] != -1)print("fall back state %d\n",sfall[i]);
p = gotof[i];
if(p == -1) return;
print("(%d transitions)\n",nexts[p]);
while(nchar[p]){
charc = 0;
if(nexts[p+1] >= 0)
print("%d\t",nexts[p+1]);
else print("err\t");
allprint(nchar[p++]);
while(nexts[p] == nexts[p+1] && nchar[p]){
if(charc > LINESIZE){
charc = 0;
print("\n\t");
}
allprint(nchar[p++]);
}
print("\n");
}
print("\n");
}
# endif
void
acompute(int s)
{
int *p, i, j;
int cnt, m;
int temp[300], k, neg[300], n;
k = 0;
n = 0;
p = state[s];
cnt = *p++;
if(cnt > 300)
error("Too many positions for one state - acompute");
for(i=0;i<cnt;i++){
if(name[*p] == FINAL)temp[k++] = left[*p];
else if(name[*p] == S1FINAL){temp[k++] = left[*p];
if (left[*p] >= NACTIONS) error("Too many right contexts");
extra[left[*p]] = 1;
} else if(name[*p] == S2FINAL)neg[n++] = left[*p];
p++;
}
atable[s] = -1;
if(k < 1 && n < 1) return;
# ifdef DEBUG
if(debug) print("final %d actions:",s);
# endif
for(i=0; i<k; i++)
for(j=i+1;j<k;j++)
if(temp[j] < temp[i]){
m = temp[j];
temp[j] = temp[i];
temp[i] = m;
}
for(i=0;i<k-1;i++)
if(temp[i] == temp[i+1]) temp[i] = 0;
atable[s] = aptr;
# ifdef DEBUG
Bprint(&fout,"",s);
# endif
Bputc(&fout, '\n');
for(i=0;i<k;i++)
if(temp[i] != 0){
Bprint(&fout,"%d,\n",temp[i]);
# ifdef DEBUG
if(debug)
print("%d ",temp[i]);
# endif
aptr++;
}
for(i=0;i<n;i++){
Bprint(&fout,"%d,\n",neg[i]);
aptr++;
# ifdef DEBUG
if(debug)print("%d ",neg[i]);
# endif
}
# ifdef DEBUG
if(debug)print("\n");
# endif
Bprint(&fout,"0,\n");
aptr++;
}
# ifdef DEBUG
void
pccl(void) {
int i, j;
print("char class intersection\n");
for(i=0; i< ccount; i++){
charc = 0;
print("class %d:\n\t",i);
for(j=1;j<NCH;j++)
if(cindex[j] == i){
allprint(j);
if(charc > LINESIZE){
print("\n\t");
charc = 0;
}
}
print("\n");
}
charc = 0;
print("match:\n");
for(i=0;i<NCH;i++){
allprint(match[i]);
if(charc > LINESIZE){
print("\n");
charc = 0;
}
}
print("\n");
}
# endif
void
mkmatch(void)
{
int i;
uchar tab[NCH];
for(i=0; i<ccount; i++)
tab[i] = 0;
for(i=1;i<NCH;i++)
if(tab[cindex[i]] == 0)
tab[cindex[i]] = i;
for(i = 1; i<NCH; i++)
match[i] = tab[cindex[i]];
}
void
layout(void)
{
int i, j, k;
int  top, bot, startup, omin;
for(i=0; i<outsize;i++)
verify[i] = advance[i] = 0;
omin = 0;
yytop = 0;
for(i=0; i<= stnum; i++){
j = gotof[i];
if(j == -1){
stoff[i] = 0;
continue;
}
bot = j;
while(nchar[j])j++;
top = j - 1;
# ifdef DEBUG
if (debug) {
print("State %d: (layout)\n", i);
for(j=bot; j<=top;j++) {
print("  %o", nchar[j]);
if (j%10==0) print("\n");
}
print("\n");
}
# endif
while(verify[omin+NCH]) omin++;
startup = omin;
# ifdef DEBUG
if (debug) print("bot,top %d, %d startup begins %d\n",bot,top,startup);
# endif
do {
startup += 1;
if(startup > outsize - NCH)
error("output table overflow");
for(j = bot; j<= top; j++){
k = startup + nchar[j];
if(verify[k])break;
}
} while (j <= top);
# ifdef DEBUG
if (debug) print(" startup going to be %d\n", startup);
# endif
for(j = bot; j<= top; j++){
k = startup + nchar[j];
verify[k] = i+1;
advance[k] = nexts[j+1]+1;
if(yytop < k) yytop = k;
}
stoff[i] = startup;
}
Bprint(&fout,"# define YYTYPE %s\n",stnum+1 >= NCH ? "int" : "Uchar");
Bprint(&fout,"struct yywork { YYTYPE verify, advance; } yycrank[] = {\n");
for(i=0;i<=yytop;i+=4){
for(j=0;j<4;j++){
k = i+j;
if(verify[k])
Bprint(&fout,"%d,%d,\t",verify[k],advance[k]);
else
Bprint(&fout,"0,0,\t");
}
Bputc(&fout, '\n');
}
Bprint(&fout,"0,0};\n");
Bprint(&fout,"struct yysvf yysvec[] = {\n");
Bprint(&fout,"0,\t0,\t0,\n");
for(i=0;i<=stnum;i++){
if(cpackflg[i])stoff[i] = -stoff[i];
Bprint(&fout,"yycrank+%d,\t",stoff[i]);
if(sfall[i] != -1)
Bprint(&fout,"yysvec+%d,\t", sfall[i]+1);
else Bprint(&fout,"0,\t\t");
if(atable[i] != -1)
Bprint(&fout,"yyvstop+%d,",atable[i]);
else Bprint(&fout,"0,\t");
# ifdef DEBUG
Bprint(&fout,"\t\t",i);
# endif
Bputc(&fout, '\n');
}
Bprint(&fout,"0,\t0,\t0};\n");
Bprint(&fout,"struct yywork *yytop = yycrank+%d;\n",yytop);
Bprint(&fout,"struct yysvf *yybgin = yysvec+1;\n");
Bprint(&fout,"Uchar yymatch[] = {\n");
for(i=0; i<NCH; i+=8){
for(j=0; j<8; j++){
int fbch;
fbch = match[i+j];
if(isprint(fbch) && fbch != '\'' && fbch != '\\')
Bprint(&fout,"'%c' ,",fbch);
else Bprint(&fout,"0%-3o,",fbch);
}
Bputc(&fout, '\n');
}
Bprint(&fout,"0};\n");
Bprint(&fout,"Uchar yyextra[] = {\n");
for(i=0;i<casecount;i+=8){
for(j=0;j<8;j++)
Bprint(&fout, "%d,", i+j<NACTIONS ?
extra[i+j] : 0);
Bputc(&fout, '\n');
}
Bprint(&fout,"0};\n");
}
# ifdef PP
void
padd(int **array, int n)
{
int i, *j, k;
array[n] = nxtpos;
if(count == 0){
*nxtpos++ = 0;
return;
}
for(i=tptr-1;i>=0;i--){
j = array[i];
if(j && *j++ == count){
for(k=0;k<count;k++)
if(!tmpstat[*j++])break;
if(k >= count){
array[n] = array[i];
return;
}
}
}
add(array,n);
}
# endif