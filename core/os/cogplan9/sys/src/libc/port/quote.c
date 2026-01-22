#include <u.h>
#include <libc.h>
int	(*doquote)(int);
extern int _needsquotes(char*, int*);
extern int _runeneedsquotes(Rune*, int*);
char*
unquotestrdup(char *s)
{
char *t, *ret;
int quoting;
ret = s = strdup(s);
if(ret == nil)
return ret;
quoting = 0;
t = s;
while(*t!='\0' && (quoting || (*t!=' ' && *t!='\t'))){
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
if(t != s)
memmove(s, t, strlen(t)+1);
return ret;
}
Rune*
unquoterunestrdup(Rune *s)
{
Rune *t, *ret;
int quoting;
ret = s = runestrdup(s);
if(ret == nil)
return ret;
quoting = 0;
t = s;
while(*t!='\0' && (quoting || (*t!=' ' && *t!='\t'))){
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
if(t != s)
memmove(s, t, (runestrlen(t)+1)*sizeof(Rune));
return ret;
}
char*
quotestrdup(char *s)
{
char *t, *u, *ret;
int quotelen;
Rune r;
if(_needsquotes(s, &quotelen) == 0)
return strdup(s);
ret = malloc(quotelen+1);
if(ret == nil)
return nil;
u = ret;
*u++ = '\'';
for(t=s; *t; t++){
r = *t;
if(r == L'\'')
*u++ = r;
*u++ = r;
}
*u++ = '\'';
*u = '\0';
return ret;
}
Rune*
quoterunestrdup(Rune *s)
{
Rune *t, *u, *ret;
int quotelen;
Rune r;
if(_runeneedsquotes(s, &quotelen) == 0)
return runestrdup(s);
ret = malloc((quotelen+1)*sizeof(Rune));
if(ret == nil)
return nil;
u = ret;
*u++ = '\'';
for(t=s; *t; t++){
r = *t;
if(r == L'\'')
*u++ = r;
*u++ = r;
}
*u++ = '\'';
*u = '\0';
return ret;
}