#include <u.h>
#include <libc.h>
#include <bio.h>
#include <ndb.h>
Ndbtuple*
ndbreorder(Ndbtuple *t, Ndbtuple *x)
{
Ndbtuple *nt;
Ndbtuple *last, *prev;
if(x == t)
return t;
for(last = x; last->line == last->entry; last = last->line)
;
if(last->line != t){
for(nt = t; nt->entry != last->line; nt = nt->entry)
;
nt->entry = nil;
for(nt = last; nt->entry != nil; nt = nt->entry)
;
nt->entry = t;
}
if(x != last->line){
for(prev = last; prev->line != x; prev = prev->line);
;
nt = last->entry;
last->entry = last->line;
prev->entry = nt;
}
return x;
}