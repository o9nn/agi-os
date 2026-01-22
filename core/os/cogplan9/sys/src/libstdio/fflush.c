#include "iolib.h"
FILE _IO_stream[]={
0,	0,	OPEN,	0,	0,	0,	0,	0,	0,
1,	0,	OPEN,	0,	0,	0,	0,	0,	0,
2,	0,	OPEN,	0,	0,	0,	0,	0,	0,
};
int _fflush(FILE *f){
int error, cnt;
if(f==NULL){
error=0;
for(f=_IO_stream;f!=&_IO_stream[FOPEN_MAX];f++)
if(f->state==WR && _fflush(f)==EOF)
error=EOF;
return error;
}
if(f->flags&STRING) return EOF;
switch(f->state){
default:
return 0;
case CLOSED:
case ERR:
return EOF;
case WR:
cnt=(f->flags&LINEBUF?f->lp:f->wp)-f->buf;
if(cnt && write(f->fd, f->buf, cnt)!=cnt){
f->state=ERR;
return EOF;
}
f->rp=f->wp=f->buf;
f->state=RDWR;
return 0;
}
}
int fflush(FILE *f)
{
int r;
qlock(&_stdiolk);
r = _fflush(f);
qunlock(&_stdiolk);
return r;
}