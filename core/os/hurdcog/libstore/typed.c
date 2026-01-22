#include "store.h"
#include <string.h>
#include <dlfcn.h>
#include <link.h>
const struct store_class *
store_find_class (const char *name, const char *clname_end,
const struct store_class *const *classes)
{
const struct store_class *const *cl;
if (! clname_end)
clname_end = strchr (name, '\0');
if (classes != 0)
{
for (cl = classes; *cl != 0; ++cl)
if (strlen ((*cl)->name) == (clname_end - name)
&& !memcmp (name, (*cl)->name, (clname_end - name)))
break;
return *cl;
}
for (cl = __start_store_std_classes; cl < __stop_store_std_classes; ++cl)
if (strlen ((*cl)->name) == (clname_end - name)
&& strncmp (name, (*cl)->name, (clname_end - name)) == 0)
return *cl;
# pragma weak _r_debug
# pragma weak dlsym
# pragma weak dlopen
# pragma weak dlclose
# pragma weak dlerror
if (dlsym)
{
struct link_map *map;
for (map = _r_debug.r_map; map != 0; map = map->l_next)
{
const struct store_class *const *start, *const *stop;
void *module = dlopen (map->l_name, RTLD_NOLOAD);
if (module == 0)
{
(void) dlerror ();
continue;
}
start = dlsym (map, "__start_store_std_classes");
if (start == 0)
(void) dlerror ();
else if (start != __start_store_std_classes)
{
stop = dlsym (map, "__stop_store_std_classes");
if (stop == 0)
(void) dlerror ();
else
for (cl = start; cl < stop; ++cl)
if (strlen ((*cl)->name) == (clname_end - name)
&& strncmp (name, (*cl)->name, (clname_end - name)) == 0)
{
dlclose (module);
return *cl;
}
}
dlclose (module);
}
}
return 0;
}
error_t
store_typed_open (const char *name, int flags,
const struct store_class *const *classes,
struct store **store)
{
const struct store_class *cl;
const char *clname_end = strchrnul (name, ':');
if (clname_end == name && *clname_end)
return store_open (name + 1, flags, classes, store);
cl = store_find_class (name, clname_end, classes);
if (cl != 0)
{
if (! cl->open)
return EOPNOTSUPP;
if (*clname_end)
clname_end++;
if (! *clname_end)
clname_end = 0;
return (*cl->open) (clname_end, flags, classes, store);
}
# pragma weak store_module_open
if (store_module_open)
{
error_t err = store_module_open (name, flags, classes, store);
if (err != ENOENT)
return err;
}
if (*clname_end)
return EINVAL;
else
return store_open (name, flags, classes, store);
}
const struct store_class
store_typed_open_class = { -1, "typed", open: store_typed_open };
STORE_STD_CLASS (typed_open);