#include "store.h"
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <error.h>
static error_t
open_class (int need_open,
const char *name, const char *clname_end,
const struct store_class **classp)
{
char *modname, *clsym;
void *mod;
if (asprintf (&modname,
"libstore_%.*s%s", (int) (clname_end - name), name,
STORE_SONAME_SUFFIX) < 0)
return ENOMEM;
errno = 0;
mod = dlopen (modname, RTLD_LAZY);
if (mod == NULL)
{
const char *errstring = dlerror ();
if (errno != ENOENT)
error (0, 0, "cannot load %s: %s", modname, errstring);
}
free (modname);
if (mod == NULL)
return errno ?: ENOENT;
if (asprintf (&clsym, "store_%.*s_class",
(int) (clname_end - name), name) < 0)
{
dlclose (mod);
return ENOMEM;
}
*classp = dlsym (mod, clsym);
free (clsym);
if (*classp == NULL)
{
error (0, 0, "invalid store module %.*s: %s",
(int) (clname_end - name), name, dlerror ());
dlclose (mod);
return EGRATUITOUS;
}
if (need_open && ! (*classp)->open)
{
dlclose (mod);
return EOPNOTSUPP;
}
return 0;
}
error_t
store_module_find_class (const char *name, const char *clname_end,
const struct store_class **classp)
{
return open_class (0, name, clname_end, classp);
}
error_t
store_module_open (const char *name, int flags,
const struct store_class *const *classes,
struct store **store)
{
const struct store_class *cl;
const char *clname_end = strchrnul (name, ':');
error_t err;
err = open_class (1, name, clname_end, &cl);
if (err)
return err;
if (*clname_end)
clname_end++;
if (! *clname_end)
clname_end = 0;
return (*cl->open) (clname_end, flags, classes, store);
}
const struct store_class store_module_open_class =
{ -1, "module", open: store_module_open };
STORE_STD_CLASS (module_open);
error_t
store_module_decode (struct store_enc *enc,
const struct store_class *const *classes,
struct store **store)
{
char *modname;
void *mod;
const struct store_class *const *cl, *const *clend;
enum file_storage_class id;
if (enc->cur_int >= enc->num_ints)
return EINVAL;
id = enc->ints[enc->cur_int];
if (asprintf (&modname, "libstore_type-%d%s", id, STORE_SONAME_SUFFIX) < 0)
return ENOMEM;
mod = dlopen (modname, RTLD_LAZY);
free (modname);
if (mod == NULL)
{
(void) dlerror ();
return ENOENT;
}
cl = dlsym (mod, "__start_store_std_classes");
if (cl == NULL)
{
error (0, 0, "invalid store module type-%d: %s", id, dlerror ());
dlclose (mod);
return EGRATUITOUS;
}
clend = dlsym (mod, "__stop_store_std_classes");
if (clend == NULL)
{
error (0, 0, "invalid store module type-%d: %s", id, dlerror ());
dlclose (mod);
return EGRATUITOUS;
}
while (cl < clend)
if ((*cl)->decode && (*cl)->id == id)
return (*(*cl)->decode) (enc, classes, store);
else
++cl;
dlclose (mod);
return EOPNOTSUPP;
}