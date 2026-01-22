#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "data_message_driver.h"
#include "mailmessage.h"
#include "mailmessage_tools.h"
#ifdef HAVE_UNISTD_H
#	include <unistd.h>
#endif
#ifdef HAVE_SYS_MMAN_H
#	include <sys/mman.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
static int fetch_size(mailmessage * msg, size_t * result)
{
struct generic_message_t * msg_data;
msg_data = msg->msg_data;
* result = msg_data->msg_length;
return MAIL_NO_ERROR;
}
static mailmessage_driver local_data_message_driver = {
"data",
mailmessage_generic_initialize,
mailmessage_generic_uninitialize,
mailmessage_generic_flush,
NULL,
mailmessage_generic_fetch_result_free,
mailmessage_generic_fetch,
mailmessage_generic_fetch_header,
mailmessage_generic_fetch_body,
fetch_size,
mailmessage_generic_get_bodystructure,
mailmessage_generic_fetch_section,
mailmessage_generic_fetch_section_header,
mailmessage_generic_fetch_section_mime,
mailmessage_generic_fetch_section_body,
mailmessage_generic_fetch_envelope,
NULL
};
mailmessage_driver * data_message_driver = &local_data_message_driver;
mailmessage * data_message_init(char * data, size_t len)
{
struct generic_message_t * msg_data;
mailmessage * msg;
int r;
struct mailimf_fields * fields;
msg = mailmessage_new();
if (msg == NULL)
goto err;
r = mailmessage_init(msg, NULL, data_message_driver, 0, len);
if (r < 0)
goto free;
msg_data = msg->msg_data;
msg_data->msg_fetched = 1;
msg_data->msg_message = data;
msg_data->msg_length = len;
r = mailmessage_generic_fetch_envelope(msg, &fields);
if (r != MAIL_NO_ERROR)
goto free;
msg->msg_fields = fields;
return msg;
free:
mailmessage_free(msg);
err:
return NULL;
}
void data_message_detach_mime(mailmessage * msg)
{
msg->msg_mime = NULL;
}