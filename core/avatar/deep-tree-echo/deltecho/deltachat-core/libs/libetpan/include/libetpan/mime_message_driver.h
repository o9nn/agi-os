#ifndef MIME_MESSAGE_DRIVER_H
#define MIME_MESSAGE_DRIVER_H
#include <libetpan/mailmessage.h>
#define LIBETPAN_MIME_MESSAGE
extern mailmessage_driver * mime_message_driver;
mailmessage * mime_message_init(struct mailmime * mime);
void mime_message_detach_mime(mailmessage * msg);
int mime_message_set_tmpdir(mailmessage * msg, char * tmpdir);
#endif