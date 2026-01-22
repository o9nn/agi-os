#include <u.h>
#include <libc.h>
#include <bio.h>
#include <ndb.h>
Ndbtuple*
ndbdiscard(Ndbtuple *t, Ndbtuple *a)
{
Ndbtuple *nt;
for(nt = t; nt != nil; nt = nt->entry){
if(nt->line == a)
nt->line = a->line;
if(nt->entry == a)
nt->entry = a->entry;
}
if(t == a)
t = a->entry;
a->entry = nil;
ndbfree(a);
ndbsetmalloctag(t, getcallerpc(&t));
return t;
}