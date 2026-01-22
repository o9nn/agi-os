#include <u.h>
#include <libc.h>
#include <draw.h>
Subfont*
_getsubfont(Display *d, char *name)
{
int fd;
Subfont *f;
fd = open(name, OREAD);
if(fd < 0){
fprint(2, "getsubfont: can't open %s: %r\n", name);
return 0;
}
if(d && d->locking == 0)
unlockdisplay(d);
f = readsubfont(d, name, fd, d && d->locking==0);
if(d && d->locking == 0)
lockdisplay(d);
if(f == 0)
fprint(2, "getsubfont: can't read %s: %r\n", name);
close(fd);
setmalloctag(f, getcallerpc(&d));
return f;
}