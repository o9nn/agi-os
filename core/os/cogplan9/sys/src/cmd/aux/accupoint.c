#include <u.h>
#include <libc.h>
#include <draw.h>
#include <event.h>
typedef struct M M;
struct M
{
Mouse;
int	byte;
};
int	button2;
int	interrupted;
int
readmouse(M *m)
{
char buf[1+4*12];
int n;
n = read(0, buf, sizeof buf);
if(n < 0)
return n;
if(n != sizeof buf)
return 0;
m->byte = buf[0];
m->xy.x =  atoi(buf+1+0*12);
m->xy.y =  atoi(buf+1+1*12);
m->buttons =  atoi(buf+1+2*12);
m->msec =  atoi(buf+1+3*12);
return 1;
}
void
writemouse(M *m)
{
print("%c%11d %11d %11d %11ld ",
m->byte,
m->xy.x,
m->xy.y,
m->buttons&7,
m->msec);
}
void
notifyf(void*, char *s)
{
if(strcmp(s, "alarm") == 0)
interrupted = 1;
noted(NCONT);
}
void
main(void)
{
M m, om;
int n;
notify(notifyf);
memset(&m, 0, sizeof m);
om = m;
for(;;){
interrupted = 0;
if(button2)
alarm(550);
n = readmouse(&m);
if(button2)
alarm(0);
if(interrupted){
om.buttons &= ~2;
button2 = 0;
writemouse(&om);
continue;
}
if(n <= 0)
break;
if((om.buttons&16) && (m.buttons&16)){
om.buttons &= ~16;
continue;
}
if(m.buttons & 2)
button2 = 0;
else{
if(m.buttons & 16){
button2 = 0;
m.buttons |= 2;
writemouse(&m);
m.buttons &= ~2;
}else if(m.buttons & 8){
button2 = 1;
}
}
if(button2)
m.buttons |= 2;
if(m.byte!=om.byte || m.buttons!=om.buttons || !eqpt(m.xy, om.xy))
writemouse(&m);
om = m;
}
}