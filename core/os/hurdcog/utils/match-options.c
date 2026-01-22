#include <argz.h>
#include <error.h>
#include <stdlib.h>
#include <string.h>
#include "match-options.h"
char *test_opts;
size_t test_opts_len;
int
match_options (struct mntent *mntent)
{
char *opts;
size_t opts_len;
error_t err = argz_create_sep (mntent->mnt_opts, ',', &opts, &opts_len);
if (err)
error (3, err, "parsing mount options failed");
for (char *test = test_opts;
test; test = argz_next (test_opts, test_opts_len, test))
{
char *needle = test;
int inverse = strncmp("no", needle, 2) == 0;
if (inverse)
needle += 2;
int match = 0;
for (char *opt = opts; opt; opt = argz_next (opts, opts_len, opt))
{
if (strcmp (opt, needle) == 0) {
if (inverse)
return 0;
match = 1;
}
}
if (! inverse && ! match)
return 0;
}
return 1;
}