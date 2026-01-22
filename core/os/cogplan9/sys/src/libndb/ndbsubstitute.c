#include <u.h>
#include <libc.h>
#include <bio.h>
#include <ndb.h>
Ndbtuple*
ndbsubstitute(Ndbtuple *t, Ndbtuple *a, Ndbtuple *b)
{
Ndbtuple *nt;
if(a == b){
ndbsetmalloctag(t, getcallerpc(&t));
return t;
}
if(b == nil){
t = ndbdiscard(t, a);
ndbsetmalloctag(t, getcallerpc(&t));
return t;
}
for(nt = t; nt != nil; nt = nt->entry){
if(nt->line == a)
nt->line = b;
if(nt->entry == a)
nt->entry = b;
}
for(nt = b; nt->entry; nt = nt->entry)
nt->line = nt->entry;
nt->line = a->line;
nt->entry = a->entry;
a->entry = nil;
ndbfree(a);
if(a == t){
ndbsetmalloctag(b, getcallerpc(&t));
return b;
}else{
ndbsetmalloctag(t, getcallerpc(&t));
return t;
}
}