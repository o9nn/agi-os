#include <u.h>
#include <libc.h>
static char qsep[] = " \t\r\n";
static char*
qtoken(char *s, char *sep)
{
int quoting;
char *t;
quoting = 0;
t = s;
while(*t!='\0' && (quoting || utfrune(sep, *t)==nil)){
if(*t != '\''){
*s++ = *t++;
continue;
}
if(!quoting){
quoting = 1;
t++;
continue;
}
if(t[1] != '\''){
t++;
quoting = 0;
continue;
}
t++;
*s++ = *t++;
}
if(*s != '\0'){
*s = '\0';
if(t == s)
t++;
}
return t;
}
static char*
etoken(char *t, char *sep)
{
int quoting;
quoting = 0;
while(*t!='\0' && (quoting || utfrune(sep, *t)==nil)){
if(*t != '\''){
t++;
continue;
}
if(!quoting){
quoting = 1;
t++;
continue;
}
if(t[1] != '\''){
t++;
quoting = 0;
continue;
}
t += 2;
}
return t;
}
int
gettokens(char *s, char **args, int maxargs, char *sep)
{
int nargs;
for(nargs=0; nargs<maxargs; nargs++){
while(*s!='\0' && utfrune(sep, *s)!=nil)
*s++ = '\0';
if(*s == '\0')
break;
args[nargs] = s;
s = etoken(s, sep);
}
return nargs;
}
int
tokenize(char *s, char **args, int maxargs)
{
int nargs;
for(nargs=0; nargs<maxargs; nargs++){
while(*s!='\0' && utfrune(qsep, *s)!=nil)
s++;
if(*s == '\0')
break;
args[nargs] = s;
s = qtoken(s, qsep);
}
return nargs;
}