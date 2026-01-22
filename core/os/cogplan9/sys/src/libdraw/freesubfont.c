#include <u.h>
#include <libc.h>
#include <draw.h>
void
freesubfont(Subfont *f)
{
if(f == 0)
return;
f->ref--;
if(f->ref > 0)
return;
uninstallsubfont(f);
free(f->info);
freeimage(f->bits);
free(f);
}