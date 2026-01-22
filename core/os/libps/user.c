#include <hurd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert-backtrace.h>
#include "ps.h"
#include "common.h"
static error_t
install_passwd (struct ps_user *u, struct passwd *pw)
{
int needed = 0;
#define COUNT(field) if (pw->field != NULL) (needed += strlen(pw->field) + 1)
COUNT (pw_name);
COUNT (pw_passwd);
COUNT (pw_gecos);
COUNT (pw_dir);
COUNT (pw_shell);
u->storage = malloc (needed);
if (u->storage != NULL)
{
char *p = u->storage;
#define COPY(field) \
if (pw->field != NULL) \
strcpy(p, pw->field), (pw->field = p), (p += strlen (p) + 1)
COPY (pw_name);
COPY (pw_passwd);
COPY (pw_gecos);
COPY (pw_dir);
COPY (pw_shell);
u->passwd = *pw;
return 0;
}
else
return ENOMEM;
}
error_t
ps_user_create (uid_t uid, struct ps_user **u)
{
*u = NEW (struct ps_user);
if (*u == NULL)
return ENOMEM;
(*u)->uid = uid;
(*u)->passwd_state = PS_USER_PASSWD_PENDING;
return 0;
}
error_t
ps_user_uname_create (char *uname, struct ps_user **u)
{
struct passwd *pw = getpwnam (uname);
if (pw)
return ps_user_passwd_create (pw, u);
else
return EINVAL;
}
error_t
ps_user_passwd_create (struct passwd *pw, struct ps_user **u)
{
error_t err = 0;
*u = NEW (struct ps_user);
if (*u == NULL)
err = ENOMEM;
else
{
err = install_passwd (*u, pw);
if (err)
FREE (*u);
else
{
(*u)->passwd_state = PS_USER_PASSWD_OK;
(*u)->uid = pw->pw_uid;
}
}
return err;
}
void
ps_user_free (struct ps_user *u)
{
if (u->passwd_state == PS_USER_PASSWD_OK)
free (u->storage);
free (u);
}
struct passwd *ps_user_passwd (struct ps_user *u)
{
if (u->passwd_state == PS_USER_PASSWD_OK)
return &u->passwd;
else if (u->passwd_state == PS_USER_PASSWD_ERROR)
return NULL;
else
{
struct passwd *pw = getpwuid (u->uid);
if (pw != NULL && install_passwd (u, pw) == 0)
{
u->passwd_state = PS_USER_PASSWD_OK;
return &u->passwd;
}
else
{
u->passwd_state = PS_USER_PASSWD_ERROR;
return NULL;
}
}
}
char *ps_user_name (struct ps_user *u)
{
struct passwd *pw = ps_user_passwd (u);
if (pw)
return pw->pw_name;
else
return NULL;
}