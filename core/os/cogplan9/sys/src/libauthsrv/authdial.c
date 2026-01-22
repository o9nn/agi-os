#include <u.h>
#include <libc.h>
#include <authsrv.h>
#include <bio.h>
#include <ndb.h>
int
authdial(char *netroot, char *dom)
{
char *p;
int rv;
if(dom == nil)
return dial(netmkaddr("$auth", netroot, "ticket"), 0, 0, 0);
p = csgetvalue(netroot, "authdom", dom, "auth", nil);
if(p == nil)
p = csgetvalue(netroot, "dom", dom, "auth", nil);
if(p == nil)
p = smprint("p9auth.%s", dom);
if(p == nil){
werrstr("no auth server found for %s", dom);
return -1;
}
rv = dial(netmkaddr(p, netroot, "ticket"), 0, 0, 0);
free(p);
return rv;
}