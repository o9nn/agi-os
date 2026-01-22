#include <stddef.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <sys/types.h>
#include <dirent.h>
#include <errno.h>
#ifndef LPDIR_H
# include "LPdir.h"
#endif
#if defined(PATH_MAX)
# define LP_ENTRY_SIZE PATH_MAX
#elif defined(NAME_MAX)
# define LP_ENTRY_SIZE NAME_MAX
#endif
#if !defined(LP_ENTRY_SIZE) || LP_ENTRY_SIZE<255
# undef LP_ENTRY_SIZE
# define LP_ENTRY_SIZE 255
#endif
struct LP_dir_context_st {
DIR *dir;
char entry_name[LP_ENTRY_SIZE + 1];
};
const char *LP_find_file(LP_DIR_CTX **ctx, const char *directory)
{
struct dirent *direntry = NULL;
if (ctx == NULL || directory == NULL) {
errno = EINVAL;
return 0;
}
errno = 0;
if (*ctx == NULL) {
*ctx = (LP_DIR_CTX *)malloc(sizeof(LP_DIR_CTX));
if (*ctx == NULL) {
errno = ENOMEM;
return 0;
}
memset(*ctx, '\0', sizeof(LP_DIR_CTX));
(*ctx)->dir = opendir(directory);
if ((*ctx)->dir == NULL) {
int save_errno = errno;
free(*ctx);
*ctx = NULL;
errno = save_errno;
return 0;
}
}
direntry = readdir((*ctx)->dir);
if (direntry == NULL) {
return 0;
}
strncpy((*ctx)->entry_name, direntry->d_name,
sizeof((*ctx)->entry_name) - 1);
(*ctx)->entry_name[sizeof((*ctx)->entry_name) - 1] = '\0';
return (*ctx)->entry_name;
}
int LP_find_file_end(LP_DIR_CTX **ctx)
{
if (ctx != NULL && *ctx != NULL) {
int ret = closedir((*ctx)->dir);
free(*ctx);
switch (ret) {
case 0:
return 1;
case -1:
return 0;
default:
break;
}
}
errno = EINVAL;
return 0;
}