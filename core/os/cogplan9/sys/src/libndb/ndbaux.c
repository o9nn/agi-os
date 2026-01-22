#include <u.h>
#include <libc.h>
#include <bio.h>
#include <ctype.h>
#include <ndb.h>
#include "ndbhf.h"
char*
_ndbparsetuple(char *cp, Ndbtuple **tp)
{
char *p;
int len;
Ndbtuple *t;
EATWHITE(cp);
if(*cp == '#' || *cp == '\n')
return 0;
t = ndbnew(nil, nil);
setmalloctag(t, getcallerpc(&cp));
*tp = t;
p = cp;
while(*cp != '=' && !ISWHITE(*cp) && *cp != '\n')
cp++;
len = cp - p;
if(len >= Ndbalen)
len = Ndbalen-1;
strncpy(t->attr, p, len);
EATWHITE(cp);
if(*cp == '='){
cp++;
if(*cp == '"'){
p = ++cp;
while(*cp != '\n' && *cp != '"')
cp++;
len = cp - p;
if(*cp == '"')
cp++;
} else if(*cp == '#'){
len = 0;
} else {
p = cp;
while(!ISWHITE(*cp) && *cp != '\n')
cp++;
len = cp - p;
}
ndbsetval(t, p, len);
}
return cp;
}
Ndbtuple*
_ndbparseline(char *cp)
{
Ndbtuple *t;
Ndbtuple *first, *last;
first = last = 0;
while(*cp != '#' && *cp != '\n'){
t = 0;
cp = _ndbparsetuple(cp, &t);
if(cp == 0)
break;
if(first){
last->line = t;
last->entry = t;
} else
first = t;
last = t;
t->line = 0;
t->entry = 0;
setmalloctag(t, getcallerpc(&cp));
}
if(first)
last->line = first;
ndbsetmalloctag(first, getcallerpc(&cp));
return first;
}