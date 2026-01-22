#ifndef LPDIR_H
# include "LPdir.h"
#endif
struct LP_dir_context_st {
void *dummy;
};
const char *LP_find_file(LP_DIR_CTX **ctx, const char *directory)
{
errno = EINVAL;
return 0;
}
int LP_find_file_end(LP_DIR_CTX **ctx)
{
errno = EINVAL;
return 0;
}