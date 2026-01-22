#include <u.h>
#include <libc.h>
#include <bio.h>
#include <ndb.h>
#include <ip.h>
#include "dns.h"
Area *owned, *delegated;
Area*
inmyarea(char *name)
{
int len;
Area *s, *d;
len = strlen(name);
for(s = owned; s; s = s->next){
if(s->len > len)
continue;
if(cistrcmp(s->soarr->owner->name, name + len - s->len) == 0)
if(len == s->len || name[len - s->len - 1] == '.')
break;
}
if(s == nil)
return nil;
for(d = delegated; d; d = d->next){
if(d->len > len)
continue;
if(cistrcmp(d->soarr->owner->name, name + len - d->len) == 0)
if(len == d->len || name[len - d->len - 1] == '.')
return nil;
}
return s;
}
void
addarea(DN *dp, RR *rp, Ndbtuple *t)
{
Area *s;
Area **l;
lock(&dnlock);
if(t->val[0])
l = &delegated;
else
l = &owned;
for (s = *l; s != nil; s = s->next)
if (strcmp(dp->name, s->soarr->owner->name) == 0) {
unlock(&dnlock);
return;
}
s = emalloc(sizeof(*s));
s->len = strlen(dp->name);
rrcopy(rp, &s->soarr);
s->soarr->owner = dp;
s->soarr->db = 1;
s->soarr->ttl = Hour;
s->neednotify = 1;
s->needrefresh = 0;
if (debug)
dnslog("new area %s %s", dp->name,
l == &delegated? "delegated": "owned");
s->next = *l;
*l = s;
unlock(&dnlock);
}
void
freearea(Area **l)
{
Area *s;
while(s = *l){
*l = s->next;
lock(&dnlock);
rrfree(s->soarr);
memset(s, 0, sizeof *s);
unlock(&dnlock);
free(s);
}
}
void
refresh_areas(Area *s)
{
int pid;
Waitmsg *w;
for(; s != nil; s = s->next){
if(!s->needrefresh)
continue;
if(zonerefreshprogram == nil){
s->needrefresh = 0;
continue;
}
pid = fork();
if (pid == -1) {
sleep(1000);
continue;
}
if (pid == 0){
execl(zonerefreshprogram, "zonerefresh",
s->soarr->owner->name, nil);
exits("exec zonerefresh failed");
}
while ((w = wait()) != nil && w->pid != pid)
free(w);
if (w && w->pid == pid)
if(w->msg == nil || *w->msg == '\0')
s->needrefresh = 0;
free(w);
}
}