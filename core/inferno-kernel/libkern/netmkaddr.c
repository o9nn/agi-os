#include <lib9.h>
char *
netmkaddr(char *linear, char *defnet, char *defsrv)
{
static char addr[4*(NAMELEN+1)];
char *cp;
cp = strchr(linear, '!');
if(cp == 0){
if(defnet==0){
if(defsrv)
sprint(addr, "net!%.*s!%.*s", 2*NAMELEN, linear,
NAMELEN, defsrv);
else
sprint(addr, "net!%.*s", 2*NAMELEN, linear);
} else {
if(defsrv)
sprint(addr, "%.*s!%.*s!%.*s", NAMELEN, defnet,
2*NAMELEN, linear, NAMELEN, defsrv);
else
sprint(addr, "%.*s!%.*s", NAMELEN, defnet,
2*NAMELEN, linear);
}
return addr;
}
cp = strchr(cp+1, '!');
if(cp)
return linear;
if(defsrv == 0)
return linear;
sprint(addr, "%.*s!%.*s", 3*NAMELEN, linear,
NAMELEN, defsrv);
return addr;
}