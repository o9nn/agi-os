#include <u.h>
#include <libc.h>
#include <fcall.h>
enum
{
DIRSIZE	= STATFIXLEN + 16 * 4
};
Dir*
dirfstat(int fd)
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
n = fstat(fd, buf, BIT16SZ+nd);
if(n < BIT16SZ){
free(d);
return nil;
}
nd = GBIT16(buf);
if(nd <= n){
convM2D(buf, n, d, (char*)&d[1]);
return d;
}
free(d);
}
return nil;
}