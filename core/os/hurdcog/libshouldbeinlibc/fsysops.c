#include <errno.h>
#include <argz.h>
#include <mach.h>
#include <sys/mman.h>
#include <hurd/fsys.h>
#include <string.h>
error_t
fsys_set_readonly (fsys_t fsys, int readonly)
{
error_t err;
char *opts = readonly ? "--readonly" : "--writable";
size_t opts_len = strlen (opts) + 1;
err = fsys_set_options (fsys, opts, opts_len, 0);
if (err == EINVAL)
err = EOPNOTSUPP;
return err;
}
error_t
fsys_get_readonly (fsys_t fsys, int *readonly)
{
error_t err;
char _opts[200], *opts = _opts;
mach_msg_type_number_t opts_len = sizeof opts;
err = fsys_get_options (fsys, &opts, &opts_len);
if (! err)
{
char *opt;
int ok = 0;
for (opt = opts
; !ok && opt && opt < opts + opts_len
; opt = argz_next (opts, opts_len, opt))
if (strcasecmp (opt, "--readonly") == 0)
{
*readonly = 1;
ok = 1;
}
else if (strcasecmp (opt, "--writable") == 0)
{
*readonly = 0;
ok = 1;
}
if (! ok)
err = EOPNOTSUPP;
if (opts != _opts)
munmap (opts, opts_len);
}
return err;
}
error_t
fsys_update (fsys_t fsys, int readonly)
{
error_t err;
char *opts = "--update";
size_t opts_len = strlen (opts) + 1;
err = fsys_set_options (fsys, opts, opts_len, 0);
if (err == EINVAL)
err = EOPNOTSUPP;
return err;
}