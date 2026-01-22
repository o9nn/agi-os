#ifndef LOGIN_INTERFACE_H
#define LOGIN_INTERFACE_H
#include "net.h"
#define LOGIN_REQUEST_COOKIE_SIZE (128/8)
#define LOGIN_REQUEST_MAX_DATA_SIZE (1024 + 128 + 64 + 2)
#define LOGIN_REQUEST_ERRMSG_INTERNAL_FAILURE \
"Internal error occurred. Refer to server log for more information."
enum login_request_flags {
LOGIN_REQUEST_FLAG_TLS_COMPRESSION	= BIT(0),
LOGIN_REQUEST_FLAG_END_CLIENT_SECURED_TLS = BIT(2),
LOGIN_REQUEST_FLAG_IMPLICIT		= BIT(3),
};
struct login_request {
unsigned int tag;
pid_t auth_pid;
unsigned int auth_id;
unsigned int client_pid;
uint8_t cookie[LOGIN_REQUEST_COOKIE_SIZE];
struct ip_addr local_ip, remote_ip;
in_port_t local_port, remote_port;
uint32_t flags;
uint32_t data_size;
ino_t ino;
};
enum login_reply_status {
LOGIN_REPLY_STATUS_OK,
LOGIN_REPLY_STATUS_INTERNAL_ERROR
};
struct login_reply {
unsigned int tag;
enum login_reply_status status;
pid_t mail_pid;
};
#endif