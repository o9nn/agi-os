#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <dlfcn.h>
#include <pthread.h>
#include "driver.h"
#define LIST_GROW 4
char *driver_path;
pthread_mutex_t driver_list_lock;
driver_t driver_list;
size_t driver_list_len;
size_t driver_list_alloc;
error_t
driver_init (void)
{
pthread_mutex_init (&driver_list_lock, NULL);
pthread_mutex_init (&display_list_lock, NULL);
pthread_mutex_init (&input_list_lock, NULL);
pthread_mutex_init (&bell_list_lock, NULL);
return 0;
}
error_t
driver_fini (void)
{
unsigned int i;
pthread_mutex_lock (&driver_list_lock);
for (i = 0; i < driver_list_len; i++)
{
driver_list[i].ops->fini (driver_list[i].handle, 1);
dlclose (driver_list[i].module);
free (driver_list[i].name);
free (driver_list[i].driver);
}
driver_list_len = 0;
pthread_mutex_unlock (&driver_list_lock);
return 0;
}
error_t driver_add (const char *const name, const char *const driver,
int argc, char *argv[], int *next, int start)
{
error_t err;
static char cons_defpath[] = CONSOLE_DEFPATH;
driver_ops_t ops;
char *filename = NULL;
char *modname;
void *shobj = NULL;
driver_t drv;
unsigned int i;
char *dir = driver_path;
int defpath = 0;
char *opt_backup;
pthread_mutex_lock (&driver_list_lock);
for (i = 0; i < driver_list_len; i++)
if (driver_list[i].name && !strcmp (driver_list[i].name, name))
{
pthread_mutex_unlock (&driver_list_lock);
return EEXIST;
}
if (!dir || !*dir)
{
dir = cons_defpath;
defpath = 1;
}
while (dir)
{
free (filename);
if (asprintf (&filename,
"%s/%s%s", dir, driver, CONSOLE_SONAME_SUFFIX) < 0)
{
pthread_mutex_unlock (&driver_list_lock);
return ENOMEM;
}
errno = 0;
shobj = dlopen (filename, RTLD_LAZY);
if (!shobj)
{
(void) dlerror ();
if (errno != ENOENT)
{
free (filename);
pthread_mutex_unlock (&driver_list_lock);
return errno ?: EGRATUITOUS;
}
}
else
break;
dir += strlen (dir) + 1;
if (!*dir)
{
if (defpath)
break;
else
{
dir = cons_defpath;
defpath = 1;
}
}
}
if (!shobj)
{
free (filename);
pthread_mutex_unlock (&driver_list_lock);
return ENOENT;
}
if (asprintf (&modname, "driver_%s_ops", driver) < 0)
{
dlclose (shobj);
free (filename);
pthread_mutex_unlock (&driver_list_lock);
return ENOMEM;
}
ops = dlsym (shobj, modname);
free (modname);
if (!ops || !ops->init)
{
dlclose (shobj);
free (filename);
pthread_mutex_unlock (&driver_list_lock);
return EGRATUITOUS;
}
if (driver_list_len == driver_list_alloc)
{
size_t new_alloc = driver_list_alloc + LIST_GROW;
driver_t new_list = realloc (driver_list,
new_alloc * sizeof (*driver_list));
if (!new_list)
{
dlclose (shobj);
free (filename);
pthread_mutex_unlock (&driver_list_lock);
return errno;
}
driver_list = new_list;
driver_list_alloc = new_alloc;
}
drv = &driver_list[driver_list_len];
drv->name = strdup (name);
drv->driver = strdup (driver);
drv->filename = filename;
drv->ops = ops;
drv->module = shobj;
if (!drv->name || !drv->driver)
{
if (drv->name)
free (drv->name);
if (drv->driver)
free (drv->driver);
dlclose (shobj);
free (filename);
pthread_mutex_unlock (&driver_list_lock);
return ENOMEM;
}
opt_backup = argv[*next - 1];
argv[*next - 1] = (char *) name;
err = (*drv->ops->init) (&drv->handle, start, argc - (*next - 1),
argv + *next - 1, next);
argv[*next - 1] = opt_backup;
if (!err && start && drv->ops->start)
err = (*drv->ops->start) (drv->handle);
if (err)
{
free (drv->name);
free (drv->driver);
dlclose (shobj);
free (filename);
pthread_mutex_unlock (&driver_list_lock);
return err;
}
driver_list_len++;
pthread_mutex_unlock (&driver_list_lock);
return 0;
}
error_t
driver_start (char **name)
{
error_t err = 0;
int i;
pthread_mutex_lock (&driver_list_lock);
for (i = 0; i < driver_list_len; i++)
{
if (driver_list[i].ops->start)
err = (*driver_list[i].ops->start) (driver_list[i].handle);
if (err)
{
*name = driver_list[i].name;
while (i > 0)
{
i--;
(*driver_list[i].ops->fini) (driver_list[i].handle, 1);
}
break;
}
}
pthread_mutex_unlock (&driver_list_lock);
return err;
}
error_t driver_remove (const char *const name)
{
error_t err;
unsigned int i;
pthread_mutex_lock (&driver_list_lock);
for (i = 0; i < driver_list_len; i++)
if (driver_list[i].name && !strcmp (driver_list[i].name, name))
{
err = driver_list[i].ops->fini (driver_list[i].handle, 0);
if (!err)
{
dlclose (driver_list[i].module);
free (driver_list[i].name);
free (driver_list[i].driver);
free (driver_list[i].filename);
while (i + 1 < driver_list_len)
{
driver_list[i] = driver_list[i + 1];
i++;
}
driver_list_len--;
}
pthread_mutex_unlock (&driver_list_lock);
return err;
}
pthread_mutex_unlock (&driver_list_lock);
return ESRCH;
}
#define ADD_REMOVE_COMPONENT(component) \
pthread_mutex_t component##_list_lock; \
component##_t component##_list; \
size_t component##_list_len; \
size_t component##_list_alloc; \
\
error_t \
driver_add_##component (component##_ops_t ops, void *handle) \
{ \
pthread_mutex_lock (&component##_list_lock); \
if (component##_list_len == component##_list_alloc) \
{ \
size_t new_alloc = component##_list_alloc + LIST_GROW; \
component##_t new_list = realloc (component##_list, \
new_alloc \
* sizeof (*component##_list)); \
if (!new_list) \
{ \
pthread_mutex_unlock (&component##_list_lock); \
return errno; \
} \
component##_list = new_list; \
component##_list_alloc = new_alloc; \
} \
component##_list[component##_list_len].ops = ops; \
component##_list[component##_list_len].handle = handle; \
component##_list_len++; \
pthread_mutex_unlock (&component##_list_lock); \
return 0; \
} \
\
error_t \
driver_remove_##component (component##_ops_t ops, void *handle) \
{ \
unsigned int i; \
\
pthread_mutex_lock (&component##_list_lock); \
for (i = 0; i < component##_list_len; i++) \
if (component##_list[i].ops == ops \
&& component##_list[i].handle == handle) \
{ \
while (i + 1 < component##_list_len) \
{ \
component##_list[i] = component##_list[i + 1]; \
i++; \
} \
component##_list_len--; \
} \
pthread_mutex_unlock (&component##_list_lock); \
return 0; \
}
ADD_REMOVE_COMPONENT (display)
ADD_REMOVE_COMPONENT (input)
ADD_REMOVE_COMPONENT (bell)