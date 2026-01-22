#ifndef CONNECT_H
#define CONNECT_H
#ifdef HAVE_INTTYPES_H
#	include <inttypes.h>
#endif
#include <time.h>
#ifdef __cplusplus
extern "C" {
#endif
uint16_t mail_get_service_port(const char * name, char * protocol);
int mail_tcp_connect(const char * server, uint16_t port);
int mail_tcp_connect_timeout(const char * server, uint16_t port, time_t timeout);
int mail_tcp_connect_with_local_address(const char * server, uint16_t port,
const char * local_address, uint16_t local_port);
int mail_tcp_connect_with_local_address_timeout(const char * server, uint16_t port,
const char * local_address, uint16_t local_port, time_t timeout);
#ifdef __cplusplus
}
#endif
#endif