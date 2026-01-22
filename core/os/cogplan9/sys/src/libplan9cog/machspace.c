#include <u.h>
#include <libc.h>
#include <plan9cog.h>
MachSpace*
machspaceinit(AtomSpace *local)
{
MachSpace *ms;
ms = mallocz(sizeof(MachSpace), 1);
if(ms == nil)
return nil;
ms->local = local;
ms->remote = nil;
ms->nremote = 0;
ms->hosts = nil;
return ms;
}
void
machspacefree(MachSpace *ms)
{
int i;
if(ms == nil)
return;
lock(ms);
for(i = 0; i < ms->nremote; i++){
free(ms->hosts[i]);
}
free(ms->hosts);
free(ms->remote);
unlock(ms);
free(ms);
}
int
machspaceconnect(MachSpace *ms, char *host)
{
char **newhosts;
AtomSpace **newremote;
if(ms == nil || host == nil)
return -1;
lock(ms);
newhosts = realloc(ms->hosts, sizeof(char*) * (ms->nremote + 1));
newremote = realloc(ms->remote, sizeof(AtomSpace*) * (ms->nremote + 1));
if(newhosts == nil || newremote == nil){
unlock(ms);
return -1;
}
ms->hosts = newhosts;
ms->remote = newremote;
ms->hosts[ms->nremote] = strdup(host);
ms->remote[ms->nremote] = nil;
ms->nremote++;
unlock(ms);
return 0;
}
Atom*
machspacefind(MachSpace *ms, ulong id)
{
Atom *a;
int i;
if(ms == nil)
return nil;
a = atomfind(ms->local, id);
if(a != nil)
return a;
lock(ms);
for(i = 0; i < ms->nremote; i++){
if(ms->remote[i]){
a = atomfind(ms->remote[i], id);
if(a != nil){
unlock(ms);
return a;
}
}
}
unlock(ms);
return nil;
}
int
machspacesync(MachSpace *ms)
{
return 0;
}