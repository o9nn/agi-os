#ifndef DATA_MESSAGE_DRIVER_H
#define DATA_MESSAGE_DRIVER_H
#include <libetpan/mailmessage.h>
#define LIBETPAN_DATA_MESSAGE
#ifdef __cplusplus
extern "C" {
#endif
extern mailmessage_driver * data_message_driver;
LIBETPAN_EXPORT
mailmessage * data_message_init(char * data, size_t len);
LIBETPAN_EXPORT
void data_message_detach_mime(mailmessage * msg);
#ifdef __cplusplus
}
#endif
#endif