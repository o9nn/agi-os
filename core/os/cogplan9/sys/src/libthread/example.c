#include <u.h>
#include <libc.h>
#include <thread.h>
enum
{
STACK = 2048,
};
void
mouseproc(void *arg)
{
char m[48];
int mfd;
Channel *mc;
mc = arg;
if((mfd = open("/dev/mouse", OREAD)) < 0)
sysfatal("open /dev/mouse: %r");
for(;;){
if(read(mfd, m, sizeof m) != sizeof m)
sysfatal("eof");
if(atoi(m+1+2*12)&4)
sysfatal("button 3");
send(mc, m);
}
}
void
clockproc(void *arg)
{
int t;
Channel *c;
c = arg;
for(t=0;; t++){
sleep(1000);
sendul(c, t);
}
}
void
threadmain(int argc, char *argv[])
{
char m[48];
int t;
Alt a[] = {
{nil,	m,	CHANRCV},
{nil,	&t,	CHANRCV},
{nil,	nil,	CHANEND},
};
a[0].c = chancreate(sizeof m, 0);
proccreate(mouseproc, a[0].c, STACK);
a[1].c = chancreate(sizeof(ulong), 0);
proccreate(clockproc, a[1].c, STACK);
for(;;){
switch(alt(a)){
case 0:
fprint(2, "click ");
break;
case 1:
fprint(2, "tic ");
break;
default:
sysfatal("can't happen");
}
}
}