#include <u.h>
#include <libc.h>
#include <bio.h>
#include "modem.h"
static char buf[102400];
static int
page(Modem *m, char *spool)
{
int count, r;
char c;
m->valid &= ~(Vfhng|Vfet|Vfpts);
if(command(m, "AT+FDR") != Eok)
return Esys;
switch(response(m, 30)){
case Rconnect:
m->phase = 'C';
if((r = createfaxfile(m, spool)) != Eok)
return r;
if((r = putmchar(m, "\022")) != Eok)
return r;
break;
case Rhangup:
return Eok;
default:
return seterror(m, Eattn);
}
verbose("starting page %d", m->pageno);
count = 0;
while((r = getmchar(m, &c, 6)) == Eok){
if(c == '\020'){
if((r = getmchar(m, &c, 3)) != Eok)
break;
if(c == '\003')
break;
if(c != '\020'){
verbose("B%2.2ux", c);
continue;
}
}
buf[count++] = c;
if(count >= sizeof(buf)){
if(write(m->pagefd, buf, count) < 0){
close(m->pagefd);
return seterror(m, Esys);
}
count = 0;
}
}
verbose("page %d done, count %d", m->pageno, count);
if(count && write(m->pagefd, buf, count) < 0){
close(m->pagefd);
return seterror(m, Esys);
}
if(r != Eok)
return r;
switch(r = response(m, 20)){
case Rok:
case Rrerror:
return Eok;
default:
verbose("page: response %d", r);
return Eproto;
}
}
static int
receive(Modem *m, char *spool)
{
int r;
loop:
switch(r = page(m, spool)){
case Eok:
if((m->valid & Vfhng) == 0 && (m->valid & (Vfet|Vfpts)) != (Vfet|Vfpts)){
verbose("receive: invalid page reponse: #%4.4ux", m->valid);
return seterror(m, Eproto);
}
if((m->valid & Vfpts) && m->fpts[0] != 1)
goto loop;
if(m->valid & Vfet){
switch(m->fet){
case 0:
case 2:
m->pageno++;
goto loop;
case 1:
faxrlog(m, Eok);
m->pageno = 1;
m->time = time(0);
m->pid = getpid();
goto loop;
}
verbose("receive: invalid FET: %d", m->fet);
return seterror(m, Eproto);
}
if(m->valid & Vfhng){
if(m->fhng == 0)
return Eok;
verbose("receive: FHNG: %d", m->fhng);
return seterror(m, Eattn);
}
default:
return r;
}
}
int
faxreceive(Modem *m, char *spool)
{
int r;
verbose("faxdaemon");
if((r = initfaxmodem(m)) != Eok)
return r;
m->pageno = 1;
m->time = time(0);
m->pid = getpid();
fcon(m);
return receive(m, spool);
}