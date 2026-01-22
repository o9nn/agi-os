#include "common.h"
#include "send.h"
static int forward_loop(char *, char *);
extern dest *
up_bind(dest *destp, message *mp, int checkforward)
{
dest *list[2];
int li;
dest *bound=0;
dest *dp;
int i;
list[0] = destp;
list[1] = 0;
for (dp = d_rm(&list[0]); dp != 0; dp = d_rm(&list[0])) {
if (!checkforward)
dp->authorized = 1;
dp->addr = escapespecial(dp->addr);
if (forward_loop(s_to_c(dp->addr), thissys)) {
dp->status = d_eloop;
d_same_insert(&bound, dp);
} else if(forward_loop(s_to_c(mp->sender), thissys)) {
dp->status = d_eloop;
d_same_insert(&bound, dp);
} else if(shellchars(s_to_c(dp->addr))) {
dp->status = d_syntax;
d_same_insert(&bound, dp);
} else
d_insert(&list[1], dp);
}
li = 1;
for (i=0; list[li]!=0 && i<32; ++i, li ^= 1) {
for (dp = d_rm(&list[li]); dp != 0; dp = d_rm(&list[li])){
dest *newlist;
rewrite(dp, mp);
if(debug)
fprint(2, "%s -> %s\n", s_to_c(dp->addr),
dp->repl1 ? s_to_c(dp->repl1):"");
switch (dp->status) {
case d_auth:
if(!dp->authorized){
authorize(dp);
if(dp->status==d_auth)
d_insert(&list[li^1], dp);
else
d_insert(&bound, dp);
}
break;
case d_cat:
newlist = expand_local(dp);
if (newlist == 0) {
d_same_insert(&bound, dp);
} else if (newlist->status == d_undefined) {
d_insert(&list[li^1], newlist);
} else {
d_same_insert(&bound, newlist);
}
break;
case d_pipe:
d_same_insert(&bound, dp);
break;
case d_alias:
newlist = s_to_dest(dp->repl1, dp);
if(newlist != 0)
d_insert(&list[li^1], newlist);
else
d_same_insert(&bound, dp);
break;
case d_translate:
newlist = translate(dp);
if (newlist != 0)
d_insert(&list[li^1], newlist);
else
d_same_insert(&bound, dp);
break;
default:
d_same_insert(&bound, dp);
break;
}
}
}
for (dp = d_rm(&list[li]); dp != 0; dp = d_rm(&list[li])) {
dp->status = d_loop;
d_same_insert(&bound, dp);
}
return bound;
}
static int
forward_loop(char *addr, char *system)
{
int len = strlen(system), found = 0;
while (addr = strchr(addr, '!'))
if (!strncmp(++addr, system, len)
&& addr[len] == '!' && ++found == 4)
return 1;
return 0;
}