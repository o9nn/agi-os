#include	"u.h"
#include	"lib.h"
#include	"dat.h"
#include	"fns.h"
#include	"error.h"
static int
ncmdfield(char *p, int n)
{
int white, nwhite;
char *ep;
int nf;
if(p == nil)
return 1;
nf = 0;
ep = p+n;
white = 1;
while(p < ep){
nwhite = (strchr(" \t\r\n", *p++ & 0xFF) != 0);
if(white && !nwhite)
nf++;
white = nwhite;
}
return nf+1;
}
Cmdbuf*
parsecmd(char *p, int n)
{
Cmdbuf *volatile cb;
int nf;
char *sp;
nf = ncmdfield(p, n);
sp = smalloc(sizeof(*cb) + nf * sizeof(char*) + n + 1);
cb = (Cmdbuf*)sp;
cb->f = (char**)(&cb[1]);
cb->buf = (char*)(&cb->f[nf]);
if(up!=nil && waserror()){
free(cb);
nexterror();
}
memmove(cb->buf, p, n);
if(up != nil)
poperror();
if(n > 0 && cb->buf[n-1] == '\n')
n--;
cb->buf[n] = '\0';
cb->nf = tokenize(cb->buf, cb->f, nf-1);
cb->f[cb->nf] = nil;
return cb;
}
void
cmderror(Cmdbuf *cb, char *s)
{
int i;
char *p, *e;
p = up->genbuf;
e = p+ERRMAX-10;
p = seprint(p, e, "%s \"", s);
for(i=0; i<cb->nf; i++){
if(i > 0)
p = seprint(p, e, " ");
p = seprint(p, e, "%q", cb->f[i]);
}
strcpy(p, "\"");
error(up->genbuf);
}
Cmdtab*
lookupcmd(Cmdbuf *cb, Cmdtab *ctab, int nctab)
{
int i;
Cmdtab *ct;
if(cb->nf == 0)
error("empty control message");
for(ct = ctab, i=0; i<nctab; i++, ct++){
if(strcmp(ct->cmd, "*") !=0)
if(strcmp(ct->cmd, cb->f[0]) != 0)
continue;
if(ct->narg != 0 && ct->narg != cb->nf)
cmderror(cb, Ecmdargs);
return ct;
}
cmderror(cb, "unknown control message");
return nil;
}