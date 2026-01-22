#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "mailmessage_types.h"
#include "mail.h"
#include <stdlib.h>
#include <string.h>
LIBETPAN_EXPORT
mailmessage * mailmessage_new(void)
{
mailmessage * msg_info;
msg_info = malloc(sizeof(* msg_info));
if (msg_info == NULL)
goto err;
msg_info->msg_driver = NULL;
msg_info->msg_session = NULL;
msg_info->msg_index = 0;
msg_info->msg_uid = NULL;
msg_info->msg_cached = FALSE;
msg_info->msg_size = 0;
msg_info->msg_fields = NULL;
memset(&msg_info->msg_single_fields,
0, sizeof(struct mailimf_single_fields));
msg_info->msg_resolved = FALSE;
msg_info->msg_flags = NULL;
msg_info->msg_mime = NULL;
msg_info->msg_data = NULL;
msg_info->msg_folder = NULL;
msg_info->msg_user_data = NULL;
return msg_info;
err:
return NULL;
}
LIBETPAN_EXPORT
void mailmessage_free(mailmessage * msg_info)
{
if (msg_info->msg_driver != NULL) {
if (msg_info->msg_driver->msg_uninitialize != NULL)
msg_info->msg_driver->msg_uninitialize(msg_info);
}
if (msg_info->msg_fields != NULL)
mailimf_fields_free(msg_info->msg_fields);
if (msg_info->msg_mime != NULL)
mailmime_free(msg_info->msg_mime);
if (msg_info->msg_flags != NULL)
mail_flags_free(msg_info->msg_flags);
if (msg_info->msg_uid != NULL)
free(msg_info->msg_uid);
free(msg_info);
}