#include "iolib.h"
static char tmpsmade[FOPEN_MAX][L_tmpnam+1];
static int ntmps = 0;
static void rmtmps(void);
FILE *tmpfile(void){
FILE *f;
static char name[]="/tmp/tf0000000000000";
char *p;
while(access(name, 0)==0){
p=name+7;
while(*p=='9') *p++='0';
if(*p=='\0') return NULL;
++*p;
}
f=fopen(name, "wb+");
if(f && ntmps<FOPEN_MAX){
if(ntmps==0)
atexit(rmtmps);
strcpy(tmpsmade[ntmps++], name);
}
return f;
}
static void
rmtmps(void)
{
int i;
for(i=0; i<ntmps; i++)
remove(tmpsmade[i]);
}