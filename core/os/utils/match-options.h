#include <mntent.h>
extern char *test_opts;
extern size_t test_opts_len;
int
match_options (struct mntent *mntent);