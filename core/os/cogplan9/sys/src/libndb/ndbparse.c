#include <u.h>
#include <libc.h>
#include <bio.h>
#include <ctype.h>
#include <ndb.h>
#include "ndbhf.h"
Ndbtuple*
ndbparse(Ndb *db)
{
char *line;
Ndbtuple *t;
Ndbtuple *first, *last;
int len;
first = last = 0;
for(;;){
if((line = Brdline(&db->b, '\n')) == 0)
break;
len = Blinelen(&db->b);
if(line[len-1] != '\n')
break;
if(first && !ISWHITE(*line) && *line != '#'){
Bseek(&db->b, -len, 1);
break;
}
t = _ndbparseline(line);
if(t == 0)
continue;
setmalloctag(t, getcallerpc(&db));
if(first)
last->entry = t;
else
first = t;
last = t;
while(last->entry)
last = last->entry;
}
ndbsetmalloctag(first, getcallerpc(&db));
return first;
}