#include <u.h>
#include <libc.h>
#include <bio.h>
#include <ip.h>
#include <ndb.h>
Ndbtuple*
ndbfindattr(Ndbtuple *entry, Ndbtuple *line, char *attr)
{
Ndbtuple *nt;
for(nt = line; nt;){
if(strcmp(attr, nt->attr) == 0)
return nt;
nt = nt->line;
if(nt == line)
break;
}
for(nt = entry; nt; nt = nt->entry)
if(strcmp(attr, nt->attr) == 0)
return nt;
return nil;
}
Ndbtuple*
ndblookval(Ndbtuple *entry, Ndbtuple *line, char *attr, char *to)
{
Ndbtuple *t;
t = ndbfindattr(entry, line, attr);
if(t != nil){
strncpy(to, t->val, Ndbvlen-1);
to[Ndbvlen-1] = 0;
}
return t;
}