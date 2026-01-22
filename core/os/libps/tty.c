#include <hurd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert-backtrace.h>
#include <hurd/term.h>
#include "ps.h"
#include "common.h"
#include "ps_term.h"
error_t
ps_tty_create (file_t port, struct ps_tty **tty)
{
*tty = NEW (struct ps_tty);
if (*tty == NULL)
return ENOMEM;
(*tty)->port = port;
(*tty)->name_state = PS_TTY_NAME_PENDING;
(*tty)->short_name = NULL;
(*tty)->short_name_alloced = FALSE;
return 0;
}
void
ps_tty_free (struct ps_tty *tty)
{
mach_port_deallocate(mach_task_self (), tty->port);
if (tty->name_state == PS_TTY_NAME_OK && tty->name != NULL)
free ((char *)tty->name);
if (tty->short_name_alloced)
free ((char *)tty->short_name);
free (tty);
}
const char *
ps_tty_name (struct ps_tty *tty)
{
if (tty->name_state == PS_TTY_NAME_PENDING)
{
string_t buf;
if (ps_term_get_nodename (tty->port, buf) != 0)
tty->name_state = PS_TTY_NAME_ERROR;
else
{
tty->name = strdup (buf);
tty->name_state = (tty->name ? PS_TTY_NAME_OK : PS_TTY_NAME_ERROR);
}
}
if (tty->name_state == PS_TTY_NAME_OK)
return tty->name;
else
return NULL;
}
struct ps_tty_abbrev
{
const char *pfx;
const char *subst;
};
const struct ps_tty_abbrev ps_tty_abbrevs[] =
{
{ "/tmp/console", "oc" },
{ "/dev/console", "co" },
{ "/dev/tty", "" },
{ "/dev/pty", "" },
{ "/dev/com", "c" },
{ "/dev/", "" },
{ 0 }
};
const char *
ps_tty_short_name (struct ps_tty *tty)
{
if (tty->short_name != NULL)
return tty->short_name;
else
{
const struct ps_tty_abbrev *abbrev;
const char *name = ps_tty_name (tty);
if (name)
for (abbrev = ps_tty_abbrevs; abbrev->pfx != NULL; abbrev++)
{
const char *subst = abbrev->subst;
size_t pfx_len = strlen (abbrev->pfx);
if (strncmp (name, abbrev->pfx, pfx_len) == 0)
{
if (name[pfx_len] == '\0')
tty->short_name = abbrev->subst;
else if (!subst || subst[0] == '\0')
tty->short_name = name + pfx_len;
else
{
size_t slen = strlen (subst);
size_t nlen = strlen (name + pfx_len) + 1;
char *n = malloc (slen + nlen);
if (n)
{
memcpy (n, subst, slen);
memcpy (&n[slen], &name[pfx_len], nlen);
tty->short_name = n;
tty->short_name_alloced = TRUE;
}
}
break;
}
}
if (tty->short_name == NULL)
tty->short_name = name;
return tty->short_name;
}
}