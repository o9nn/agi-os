#include "all.h"
Xfid *
xfauth(Xfile *, String *)
{
return 0;
}
long
xfauthread(Xfid *xf, long, uchar *, long)
{
chat("xfauthread %s...", xf->uid);
return 0;
}
long
xfauthwrite(Xfid *xf, long, uchar *, long)
{
chat("xfauthwrite %s...", xf->uid);
return 0;
}
int
xfauthremove(Xfid *, char *)
{
chat("authremove...");
return -1;
}