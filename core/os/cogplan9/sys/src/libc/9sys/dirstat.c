#include <u.h>
#include <libc.h>
#include <fcall.h>
enum
{
DIRSIZE	= STATFIXLEN + 16 * 4
};
Dir*
dirstat(char *name)
{
Dir *d;
uchar *buf;
int n, nd, i;
nd = DIRSIZE;
for(i=0; i<2; i++){
d = malloc(sizeof(Dir) + BIT16SZ + nd);
if(d == nil)
return nil;
buf = (uchar*)&d[1];
n = stat(name, buf, BIT16SZ+nd);
if(n < BIT16SZ){
free(d);
return nil;
}
nd = GBIT16((uchar*)buf);
if(nd <= n){
convM2D(buf, n, d, (char*)&d[1]);
return d;
}
free(d);
}
return nil;
}