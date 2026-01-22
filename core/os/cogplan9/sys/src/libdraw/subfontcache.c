#include <u.h>
#include <libc.h>
#include <draw.h>
static char	*lastname;
Subfont	*lastsubfont;
Subfont*
lookupsubfont(Display *d, char *name)
{
if(d && strcmp(name, "*default*") == 0)
return d->defaultsubfont;
if(lastname && strcmp(name, lastname)==0)
if(d==lastsubfont->bits->display){
lastsubfont->ref++;
return lastsubfont;
}
return 0;
}
void
installsubfont(char *name, Subfont *subfont)
{
free(lastname);
lastname = strdup(name);
lastsubfont = subfont;
}
void
uninstallsubfont(Subfont *subfont)
{
if(subfont == lastsubfont){
lastname = 0;
lastsubfont = 0;
}
}