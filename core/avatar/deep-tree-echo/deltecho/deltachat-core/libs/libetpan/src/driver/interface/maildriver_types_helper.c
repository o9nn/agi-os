#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "maildriver_types_helper.h"
#include "mail.h"
#include "clist.h"
#include <string.h>
#include <stdlib.h>
int mail_flags_add_extension(struct mail_flags * flags,
char * ext_flag)
{
char * str;
int r;
if (mail_flags_has_extension(flags, ext_flag))
return MAIL_NO_ERROR;
str = strdup(ext_flag);
if (str == NULL)
return MAIL_ERROR_MEMORY;
r = clist_append(flags->fl_extension, str);
if (r < 0) {
free(str);
return MAIL_ERROR_MEMORY;
}
return MAIL_NO_ERROR;
}
int mail_flags_remove_extension(struct mail_flags * flags,
char * ext_flag)
{
clistiter * cur;
cur = clist_begin(flags->fl_extension);
while (cur != NULL) {
char * flag_name;
flag_name = clist_content(cur);
if (strcasecmp(flag_name, ext_flag) == 0) {
free(flag_name);
cur = clist_delete(flags->fl_extension, cur);
}
else
cur = clist_next(cur);
}
return MAIL_NO_ERROR;
}
int mail_flags_has_extension(struct mail_flags * flags,
char * ext_flag)
{
clistiter * cur;
for(cur = clist_begin(flags->fl_extension) ; cur != NULL ;
cur = clist_next(cur)) {
char * flag_name;
flag_name = clist_content(cur);
if (strcasecmp(flag_name, ext_flag) == 0)
return TRUE;
}
return FALSE;
}