#include <u.h>
#include <libc.h>
#include <bio.h>
#include <thread.h>
#include "object.h"
#include "parse.h"
#include "search.h"
Object *
search(Object *rt, Object *parent, Reprog *preg) {
Object *o, *nr;
char *s;
int i;
int yes = 0;
nr = newobject(rt->type, parent);
nr->orig = rt->orig?rt->orig:rt;
nr->value = rt->value;
strncpy(nr->key, rt->key, KEYLEN);
if((((s = nr->value)) && regexec(preg, s, nil, 0) == 1)
|| (((s = nr->value)) && regexec(preg, s, nil, 0) == 1))
yes = 1;
for(i = 0; i < rt->nchildren; i++)
if((o = search((Object*)rt->children[i], nr, preg))){
yes = 1;
addchild(nr, o, "search");
}
if(yes == 0){
freeobject(nr, "s");
return nil;
}
return nr;
}