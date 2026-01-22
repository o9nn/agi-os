#include "iolib.h"
FILE *freopen(const char *name, const char *mode, FILE *f){
int m;
if(f->state!=CLOSED){
fclose(f);
}
m = *mode++;
if(m == 0)
return NULL;
if(*mode == 'b')
mode++;
switch(m){
default:
return NULL;
case 'r':
f->fd=open(name, (*mode == '+'? ORDWR: OREAD));
break;
case 'w':
f->fd=create(name, (*mode == '+'? ORDWR: OWRITE), 0666);
break;
case 'a':
m = (*mode == '+'? ORDWR: OWRITE);
f->fd=open(name, m);
if(f->fd<0)
f->fd=create(name, m, 0666);
seek(f->fd, 0LL, 2);
break;
}
if(f->fd==-1)
return NULL;
f->flags=(mode[0]=='a')? APPEND : 0;
f->state=OPEN;
f->buf=0;
f->rp=0;
f->wp=0;
f->lp=0;
return f;
}