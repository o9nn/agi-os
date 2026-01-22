#include <u.h>
#include <libc.h>
void
main(int argc, char *argv[])
{
long n;
char *p, *q;
if(argc>1){
for(n = strtol(argv[1], &p, 0); n > 0; n--)
sleep(1000);
if(*p++ == '.' && (n = strtol(p, &q, 10)) > 0){
switch(q - p){
case 0:
break;
case 1:
n *= 100;
break;
case 2:
n *= 10;
break;
default:
p[3] = 0;
n = strtol(p, 0, 10);
break;
}
sleep(n);
}
}
exits(0);
}