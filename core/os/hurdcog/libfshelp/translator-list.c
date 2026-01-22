#include <argz.h>
#include <assert-backtrace.h>
#include <hurd/fsys.h>
#include <hurd/ihash.h>
#include <hurd/ports.h>
#include <mach.h>
#include <mach/notify.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <libgen.h>
#include "fshelp.h"
struct translator
{
hurd_ihash_locp_t locp;
struct port_info *pi;
char *name;
mach_port_t active;
};
static void
cleanup (void *value, void *arg)
{
struct translator *translator = value;
if (translator->pi)
ports_port_deref (translator->pi);
mach_port_deallocate (mach_task_self (), translator->active);
free (translator->name);
free (translator);
}
static hurd_ihash_key_t
hash (const void *key)
{
return (hurd_ihash_key_t) hurd_ihash_hash32 (key, sizeof (void *), 0);
}
static int
compare (const void *a, const void *b)
{
return *(void **) a == *(void **) b;
}
static struct hurd_ihash translator_ihash
= HURD_IHASH_INITIALIZER_GKI (offsetof (struct translator, locp),
cleanup, NULL, hash, compare);
static pthread_mutex_t translator_ihash_lock = PTHREAD_MUTEX_INITIALIZER;
error_t
fshelp_set_active_translator (struct port_info *pi,
const char *name,
const struct transbox *transbox)
{
error_t err = 0;
struct translator *t;
hurd_ihash_locp_t slot;
pthread_mutex_lock (&translator_ihash_lock);
t = hurd_ihash_locp_find (&translator_ihash, (hurd_ihash_key_t) transbox,
&slot);
if (t)
goto update;
if (! MACH_PORT_VALID (transbox->active))
goto out;
t = malloc (sizeof *t);
if (! t)
{
err = errno;
goto out;
}
t->active = MACH_PORT_NULL;
t->pi = NULL;
t->name = strdup (name);
if (! t->name)
{
err = errno;
free (t);
goto out;
}
err = hurd_ihash_locp_add (&translator_ihash, slot,
(hurd_ihash_key_t) transbox, t);
if (err)
{
free (t->name);
free (t);
goto out;
}
update:
if (MACH_PORT_VALID (transbox->active) && transbox->active != t->active)
{
mach_port_t old;
err = mach_port_request_notification (mach_task_self (), transbox->active,
MACH_NOTIFY_DEAD_NAME, 0,
pi->port_right,
MACH_MSG_TYPE_MAKE_SEND_ONCE,
&old);
if (err)
goto out;
if (MACH_PORT_VALID (old))
mach_port_deallocate (mach_task_self (), old);
if (t->pi)
ports_port_deref (t->pi);
ports_port_ref (pi);
t->pi = pi;
if (MACH_PORT_VALID (t->active))
mach_port_deallocate (mach_task_self (), t->active);
mach_port_mod_refs (mach_task_self (), transbox->active,
MACH_PORT_RIGHT_SEND, +1);
t->active = transbox->active;
}
else if (! MACH_PORT_VALID (transbox->active))
{
int ok;
ok = hurd_ihash_remove (&translator_ihash, (hurd_ihash_key_t) transbox);
assert_backtrace (ok);
}
out:
pthread_mutex_unlock (&translator_ihash_lock);
return err;
}
error_t
fshelp_remove_active_translator (mach_port_t active)
{
error_t err = 0;
pthread_mutex_lock (&translator_ihash_lock);
struct translator *t = NULL;
HURD_IHASH_ITERATE (&translator_ihash, value)
{
struct translator *v = value;
if (active == v->active)
{
t = v;
break;
}
}
if (t)
hurd_ihash_locp_remove (&translator_ihash, t->locp);
pthread_mutex_unlock (&translator_ihash_lock);
return err;
}
error_t
fshelp_get_active_translators (char **translators,
size_t *translators_len,
mach_port_t **controls,
size_t *controls_count)
{
error_t err = 0;
pthread_mutex_lock (&translator_ihash_lock);
*controls = calloc (translator_ihash.nr_items, sizeof **controls);
if (*controls == NULL)
{
pthread_mutex_unlock (&translator_ihash_lock);
return ENOMEM;
}
*controls_count = 0;
HURD_IHASH_ITERATE (&translator_ihash, value)
{
struct translator *t = value;
err = mach_port_mod_refs (mach_task_self (), t->active,
MACH_PORT_RIGHT_SEND, +1);
if (err)
{
err = 0;
continue;
}
(*controls)[*controls_count] = t->active;
(*controls_count)++;
err = argz_add (translators, translators_len, t->name);
if (err)
break;
}
pthread_mutex_unlock (&translator_ihash_lock);
return err;
}
error_t
fshelp_map_active_translators (error_t (*fun)(void *cookie,
const char *name,
mach_port_t control),
void *cookie)
{
error_t err = 0;
pthread_mutex_lock (&translator_ihash_lock);
HURD_IHASH_ITERATE (&translator_ihash, value)
{
struct translator *t = value;
err = fun (cookie, t->name, t->active);
if (err)
break;
}
pthread_mutex_unlock (&translator_ihash_lock);
return err;
}