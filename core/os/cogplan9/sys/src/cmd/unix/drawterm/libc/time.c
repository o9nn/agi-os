#include <u.h>
#include <libc.h>
static long
oldtime(long *tp)
{
char b[20];
static int f = -1;
int i, retries;
long t;
memset(b, 0, sizeof(b));
for(retries = 0; retries < 100; retries++){
if(f < 0)
f = open("/dev/time", OREAD|OCEXEC);
if(f < 0)
break;
if(seek(f, 0, 0) < 0 || (i = read(f, b, sizeof(b))) < 0){
close(f);
f = -1;
} else {
if(i != 0)
break;
}
}
t = atol(b);
if(tp)
*tp = t;
return t;
}
long
time(long *tp)
{
vlong t;
t = nsec()/((vlong)1000000000);
if(t == 0)
t = oldtime(0);
if(tp != nil)
*tp = t;
return t;
}