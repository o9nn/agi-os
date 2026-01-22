#include <u.h>
#include <libc.h>
#include <fcall.h>
#include <thread.h>
#include <libsec.h>
#include <ctype.h>
#include <9p.h>
#include "cifs.h"
enum {
Nomatch,
Exactmatch,
Badmatch
};
#define SINT_MAX	0x7fffffff
typedef struct Dfscache Dfscache;
struct Dfscache {
Dfscache*next;
char	*src;
char	*host;
char	*share;
char	*path;
long	expiry;
long	rtt;
int	prox;
};
Dfscache *Cache;
int
dfscacheinfo(Fmt *f)
{
long ex;
Dfscache *cp;
for(cp = Cache; cp; cp = cp->next){
ex = cp->expiry - time(nil);
if(ex < 0)
ex = -1;
fmtprint(f, "%-42s %6ld %8.1f %4d %-16s %-24s %s\n",
cp->src, ex, (double)cp->rtt/1000.0L, cp->prox,
cp->host, cp->share, cp->path);
}
return 0;
}
char *
trimshare(char *s)
{
char *p;
static char name[128];
strncpy(name, s, sizeof(name));
name[sizeof(name)-1] = 0;
if((p = strrchr(name, '$')) != nil && p[1] == 0)
*p = 0;
return name;
}
static Dfscache *
lookup(char *path, int *match)
{
int len, n, m;
Dfscache *cp, *best;
if(match)
*match = Nomatch;
len = 0;
best = nil;
m = strlen(path);
for(cp = Cache; cp; cp = cp->next){
n = strlen(cp->src);
if(n < len)
continue;
if(strncmp(path, cp->src, n) != 0)
continue;
if(path[n] != 0 && path[n] != '/')
continue;
best = cp;
len = n;
if(n == m){
if(match)
*match = Exactmatch;
break;
}
}
return best;
}
char *
mapfile(char *opath)
{
int exact;
Dfscache *cp;
char *p, *path;
static char npath[MAX_DFS_PATH];
path = opath;
if((cp = lookup(path, &exact)) != nil){
snprint(npath, sizeof npath, "/%s%s%s%s", cp->share,
*cp->path? "/": "", cp->path, path + strlen(cp->src));
path = npath;
}
if((p = strchr(path+1, '/')) == nil)
p = "/";
if(Debug && strstr(Debug, "dfs") != nil)
print("mapfile src=%q => dst=%q\n", opath, p);
return p;
}
int
mapshare(char *path, Share **osp)
{
int i;
Share *sp;
Dfscache *cp;
char *s, *try;
char *tail[] = { "", "$" };
if((cp = lookup(path, nil)) == nil)
return 0;
for(sp = Shares; sp < Shares+Nshares; sp++){
s = trimshare(sp->name);
if(cistrcmp(cp->share, s) != 0)
continue;
if(Checkcase && strcmp(cp->share, s) != 0)
continue;
if(Debug && strstr(Debug, "dfs") != nil)
print("mapshare, already connected, src=%q => dst=%q\n", path, sp->name);
*osp = sp;
return 0;
}
sp = Shares + Nshares;
for(i = 0; i < 2; i++){
try = smprint("%s%s", cp->share, tail[i]);
if(CIFStreeconnect(Sess, Sess->cname, try, sp) == 0){
sp->name = try;
*osp = sp;
Nshares++;
if(Debug && strstr(Debug, "dfs") != nil)
print("mapshare connected, src=%q dst=%q\n",
path, cp->share);
return 0;
}
free(try);
}
if(Debug && strstr(Debug, "dfs") != nil)
print("mapshare failed src=%s\n", path);
werrstr("not found");
return -1;
}
static int
remap(Dfscache *cp, Refer *re)
{
int n;
long rtt;
char *p, *a[4];
enum {
Hostname = 1,
Sharename = 2,
Pathname = 3,
Rtt_tol = 10
};
if(Debug && strstr(Debug, "dfs") != nil)
print("	remap %s\n", re->addr);
for(p = re->addr; *p; p++)
if(*p == '\\')
*p = '/';
if(cp->prox < re->prox){
if(Debug && strstr(Debug, "dfs") != nil)
print("	remap %d < %d\n", cp->prox, re->prox);
return -1;
}
if((n = getfields(re->addr, a, sizeof(a), 0, "/")) < 3){
if(Debug && strstr(Debug, "dfs") != nil)
print("	remap nfields=%d\n", n);
return -1;
}
if((rtt = ping(a[Hostname], Dfstout)) == -1){
if(Debug && strstr(Debug, "dfs") != nil)
print("	remap ping failed\n");
return -1;
}
if(cp->rtt < rtt && (rtt/labs(rtt-cp->rtt)) < Rtt_tol){
if(Debug && strstr(Debug, "dfs") != nil)
print("	remap bad ping %ld < %ld && %ld < %d\n",
cp->rtt, rtt, (rtt/labs(rtt-cp->rtt)), Rtt_tol);
return -1;
}
if(n < 4)
a[Pathname] = "";
if(re->ttl == 0)
re->ttl = 60*5;
free(cp->host);
free(cp->share);
free(cp->path);
cp->rtt = rtt;
cp->prox = re->prox;
cp->expiry = time(nil)+re->ttl;
cp->host = estrdup9p(a[Hostname]);
cp->share = estrdup9p(trimshare(a[Sharename]));
cp->path = estrdup9p(a[Pathname]);
if(Debug && strstr(Debug, "dfs") != nil)
print("	remap ping OK prox=%d host=%s share=%s path=%s\n",
cp->prox, cp->host, cp->share, cp->path);
return 0;
}
static int
redir1(Session *s, char *path, Dfscache *cp, int level)
{
Refer retab[16], *re;
int n, gflags, used, found;
if(level > 8)
return -1;
if((n = T2getdfsreferral(s, &Ipc, path, &gflags, &used, retab,
nelem(retab))) == -1)
return -1;
if(! (gflags & DFS_HEADER_ROOT))
used = SINT_MAX;
found = 0;
for(re = retab; re < retab+n; re++){
if(Debug && strstr(Debug, "dfs") != nil)
print("referal level=%d prox=%d path=%q addr=%q\n",
level, re->prox, re->path, re->addr);
if(gflags & DFS_HEADER_STORAGE){
if(remap(cp, re) == 0)
found = 1;
} else{
if(redir1(s, re->addr, cp, level+1) != -1)
found = 1;
}
free(re->addr);
free(re->path);
}
if(Debug && strstr(Debug, "dfs") != nil)
print("referal level=%d path=%q found=%d used=%d\n",
level, path, found, used);
if(!found)
return -1;
return used;
}
int
redirect(Session *s, Share *sp, char *path)
{
int match;
char *unc;
Dfscache *cp;
if(Debug && strstr(Debug, "dfs") != nil)
print("redirect name=%q path=%q\n", sp->name, path);
cp = lookup(path, &match);
if(match == Badmatch)
return -1;
if(cp && match == Exactmatch){
if(cp->expiry >= time(nil)){
if(Debug && strstr(Debug, "dfs") != nil)
print("redirect cache=hit src=%q => share=%q path=%q\n",
cp->src, cp->share, cp->path);
return 0;
} else{
cp->rtt = SINT_MAX;
cp->prox = SINT_MAX;
unc = smprint("
cp->share, cp->path, *cp->path? "/": "",
path + strlen(cp->src) + 1);
if(unc == nil)
sysfatal("no memory: %r");
if(redir1(s, unc, cp, 1) == -1){
if(Debug && strstr(Debug, "dfs") != nil)
print("redirect refresh failed unc=%q\n",
unc);
free(unc);
return -1;
}
free(unc);
if(Debug && strstr(Debug, "dfs") != nil)
print("redirect refresh cache=stale src=%q => share=%q path=%q\n",
cp->src, cp->share, cp->path);
return 0;
}
}
if(cp)
unc = smprint("
cp->path, *cp->path? "/": "", path + strlen(cp->src) + 1);
else
unc = smprint("
if(unc == nil)
sysfatal("no memory: %r");
cp = emalloc9p(sizeof(Dfscache));
memset(cp, 0, sizeof(Dfscache));
cp->rtt = SINT_MAX;
cp->prox = SINT_MAX;
if(redir1(s, unc, cp, 1) == -1){
if(Debug && strstr(Debug, "dfs") != nil)
print("redirect new failed unc=%q\n", unc);
free(unc);
free(cp);
return -1;
}
free(unc);
cp->src = estrdup9p(path);
cp->next = Cache;
Cache = cp;
if(Debug && strstr(Debug, "dfs") != nil)
print("redirect cache=miss src=%q => share=%q path=%q\n",
cp->src, cp->share, cp->path);
return 0;
}