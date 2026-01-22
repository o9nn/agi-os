#include "common.h"
#include "send.h"
#define isspace(c) ((c)==' ' || (c)=='\t' || (c)=='\n')
extern char*
skipequiv(char *base)
{
char *sp;
static Biobuf *fp;
while(*base){
sp = strchr(base, '!');
if(sp==0)
break;
*sp = '\0';
if(lookup(base, "equivlist", &fp, 0, 0)==1){
*sp='!';
base=sp+1;
} else {
*sp='!';
break;
}
}
return base;
}
static int
okfile(char *cp, Biobuf *fp)
{
char *buf;
int len;
char *bp, *ep;
int c;
len = strlen(cp);
Bseek(fp, 0, 0);
while(buf = Brdline(fp, '\n')) {
ep = &buf[Blinelen(fp)];
for(bp=buf; bp < ep;){
while(isspace(*bp) || *bp==',')
bp++;
if(strncmp(bp, cp, len) == 0) {
c = *(bp+len);
if(isspace(c) || c==',')
return 1;
}
while(bp < ep && (!isspace(*bp)) && *bp!=',')
bp++;
}
}
return 0;
}
extern int
lookup(char *cp, char *local, Biobuf **lfpp, char *global, Biobuf **gfpp)
{
static String *file = 0;
if (local) {
if (file == 0)
file = s_new();
abspath(local, UPASLIB, s_restart(file));
if (*lfpp != 0 || (*lfpp = sysopen(s_to_c(file), "r", 0)) != 0) {
if (okfile(cp, *lfpp))
return 1;
} else
local = 0;
}
if (global) {
abspath(global, UPASLIB, s_restart(file));
if (*gfpp != 0 || (*gfpp = sysopen(s_to_c(file), "r", 0)) != 0) {
if (okfile(cp, *gfpp))
return 1;
} else
global = 0;
}
return (local || global)? 0 : -1;
}