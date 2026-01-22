#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include "assert-backtrace.h"
#include <idvec.h>
#include <grp.h>
#include <pwd.h>
#include <shadow.h>
#ifdef HAVE_LIBCRYPT
#include <crypt.h>
#else
#warning "No crypt on this system!  Using plain-text passwords."
#define crypt(password, encrypted) password
#endif
#define SHADOW_PASSWORD_STRING "x"
static error_t
verify_id (uid_t id, int is_group, int multiple,
char *(*getpass_fn) (const char *prompt,
uid_t id, int is_group,
void *pwd_or_grp, void *hook),
void *getpass_hook,
error_t (*verify_fn) (const char *password,
uid_t id, int is_group,
void *pwd_or_grp, void *hook),
void *verify_hook);
static char *
get_passwd (const char *prompt,
uid_t id, int is_group,
void *pwd_or_grp, void *hook)
{
char *st = getpass (prompt);
if (st)
st = strdup (st);
return st;
}
static error_t
verify_passwd (const char *password,
uid_t id, int is_group,
void *pwd_or_grp, void *hook)
{
const char *encrypted;
int wheel_uid = (intptr_t)hook;
const char *sys_encrypted;
if (! pwd_or_grp)
return (id == 0 ? 0 : EACCES);
sys_encrypted =
(is_group
? ((struct passwd *)pwd_or_grp)->pw_passwd
: ((struct group *)pwd_or_grp)->gr_passwd);
if (sys_encrypted[0] == '\0')
return 0;
encrypted = crypt (password, sys_encrypted);
if (! encrypted)
return errno;
if (strcmp (encrypted, sys_encrypted) == 0)
return 0;
else if (id == 0 && !is_group && wheel_uid)
{
struct passwd _pw, *pw;
char lookup_buf[1024];
char sp_lookup_buf[1024];
const char *check_shadow (struct passwd *pw)
{
if (strcmp (pw->pw_passwd, SHADOW_PASSWORD_STRING) == 0)
{
struct spwd _sp, *sp;
if (getspnam_r (pw->pw_name, &_sp, sp_lookup_buf,
sizeof sp_lookup_buf, &sp) == 0)
return sp->sp_pwdp;
}
return pw->pw_passwd;
}
if (getpwuid_r (wheel_uid, &_pw, lookup_buf, sizeof lookup_buf, &pw)
|| ! pw)
return errno ?: EINVAL;
sys_encrypted = check_shadow (pw);
encrypted = crypt (password, sys_encrypted);
if (! encrypted)
return errno;
if (strcmp (encrypted, sys_encrypted) == 0)
return 0;
}
return EACCES;
}
error_t
idvec_verify (const struct idvec *uids, const struct idvec *gids,
const struct idvec *have_uids, const struct idvec *have_gids,
char *(*getpass_fn) (const char *prompt,
uid_t id, int is_group,
void *pwd_or_grp, void *hook),
void *getpass_hook,
error_t (*verify_fn) (const char *password,
uid_t id, int is_group,
void *pwd_or_grp, void *hook),
void *verify_hook)
{
if (have_uids && idvec_contains (have_uids, 0))
return 0;
else
{
unsigned int i;
int multiple = 0;
error_t err = 0;
struct idvec implied_gids = IDVEC_INIT;
int wheel_uid =
((have_uids && have_gids
&& (idvec_contains (have_gids, 0) && have_uids->num > 0))
? have_uids->ids[0]
: 0);
if (! verify_fn)
{
verify_fn = verify_passwd;
verify_hook = (void *)(intptr_t)wheel_uid;
}
if (uids && gids)
{
int num_non_implied_gids = 0;
idvec_merge_implied_gids (&implied_gids, uids);
for (i = 0; i < gids->num; i++)
if (! idvec_contains (&implied_gids, gids->ids[i]))
num_non_implied_gids++;
multiple = (uids->num + num_non_implied_gids) > 1;
}
else if (uids)
multiple = uids->num > 1;
else if (gids)
multiple = gids->num > 1;
if (uids && idvec_contains (uids, 0))
err = verify_id (0, 0, multiple,
getpass_fn, getpass_hook, verify_fn, verify_hook);
else
{
if (uids)
for (i = 0; i < uids->num && !err; i++)
{
uid_t uid = uids->ids[i];
if (!have_uids || !idvec_contains (have_uids, uid))
err = verify_id (uid, 0, multiple,
getpass_fn, getpass_hook, verify_fn, verify_hook);
}
if (gids)
for (i = 0; i < gids->num && !err; i++)
{
gid_t gid = gids->ids[i];
if ((!have_gids || !idvec_contains (have_gids, gid))
&& !idvec_contains (&implied_gids, gid))
err = verify_id (gid, 1, multiple,
getpass_fn, getpass_hook, verify_fn, verify_hook);
}
}
idvec_fini (&implied_gids);
return err;
}
}
static error_t
verify_id (uid_t id, int is_group, int multiple,
char *(*getpass_fn) (const char *prompt,
uid_t id, int is_group,
void *pwd_or_grp, void *hook),
void *getpass_hook,
error_t (*verify_fn) (const char *password,
uid_t id, int is_group,
void *pwd_or_grp, void *hook),
void *verify_hook)
{
int err;
void *pwd_or_grp = 0;
char *name = 0;
char *prompt = 0, *password;
char id_lookup_buf[1024];
char sp_lookup_buf[1024];
assert_backtrace (verify_fn);
if (id != (uid_t) -1)
do
{
if (is_group)
{
struct group _gr, *gr;
if (getgrgid_r (id, &_gr, id_lookup_buf, sizeof id_lookup_buf, &gr)
== 0 && gr)
{
if (!gr->gr_passwd || !*gr->gr_passwd)
return (*verify_fn) ("", id, 1, gr, verify_hook);
name = gr->gr_name;
pwd_or_grp = gr;
}
}
else
{
struct passwd _pw, *pw;
if (getpwuid_r (id, &_pw, id_lookup_buf, sizeof id_lookup_buf, &pw)
== 0 && pw)
{
if (strcmp (pw->pw_passwd, SHADOW_PASSWORD_STRING) == 0)
{
struct spwd _sp, *sp;
if (getspnam_r (pw->pw_name, &_sp, sp_lookup_buf,
sizeof sp_lookup_buf, &sp) == 0)
pw->pw_passwd = sp->sp_pwdp;
}
if (pw->pw_passwd[0] == '\0')
return (*verify_fn) ("", id, 0, pw, verify_hook);
name = pw->pw_name;
pwd_or_grp = pw;
}
}
if (! name)
{
if (id != 0 || is_group)
{
id = 0;
is_group = 0;
multiple = 1;
}
else
name = "root";
}
}
while (! name);
if (! getpass_fn)
getpass_fn = get_passwd;
if (multiple)
{
if (name)
asprintf (&prompt, "Password for %s%s:",
is_group ? "group " : "", name);
else
asprintf (&prompt, "Password for %s %d:",
is_group ? "group" : "user", id);
}
if (prompt)
{
password =
(*getpass_fn) (prompt, id, is_group, pwd_or_grp, getpass_hook);
free (prompt);
}
else
password =
(*getpass_fn) ("Password:", id, is_group, pwd_or_grp, getpass_hook);
if (password)
{
err = (*verify_fn) (password, id, is_group, pwd_or_grp, verify_hook);
memset (password, 0, strlen (password));
free (password);
}
else
err = EACCES;
return err;
}