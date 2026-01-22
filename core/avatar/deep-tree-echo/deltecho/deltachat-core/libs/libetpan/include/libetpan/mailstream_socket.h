#ifndef MAILSTREAM_SOCKET_H
#define MAILSTREAM_SOCKET_H
#include <libetpan/mailstream.h>
#ifdef __cplusplus
extern "C" {
#endif
extern mailstream_low_driver * mailstream_socket_driver;
mailstream_low * mailstream_low_socket_open(int fd);
void mailstream_socket_set_use_read(mailstream * stream, int use_read);
mailstream * mailstream_socket_open(int fd);
mailstream * mailstream_socket_open_timeout(int fd, time_t timeout);
#ifdef __cplusplus
}
#endif
#endif