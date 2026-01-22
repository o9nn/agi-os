#ifndef MAILSMTP_HELPER_H
#define MAILSMTP_HELPER_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/libetpan-config.h>
#include "mailsmtp_types.h"
#include "clist.h"
LIBETPAN_EXPORT
int mailsmtp_init(mailsmtp * session);
LIBETPAN_EXPORT
int mailsmtp_init_with_ip(mailsmtp * session, int useip);
LIBETPAN_EXPORT
int mailesmtp_send(mailsmtp * session,
const char * from,
int return_full,
const char * envid,
clist * addresses,
const char * message, size_t size);
LIBETPAN_EXPORT
int mailesmtp_send_quit(mailsmtp * session,
const char * from,
int return_full,
const char * envid,
clist * addresses,
const char * message, size_t size);
LIBETPAN_EXPORT
int mailesmtp_send_quit_no_disconnect(mailsmtp * session,
const char * from,
int return_full,
const char * envid,
clist * addresses,
const char * message, size_t size);
LIBETPAN_EXPORT
int mailsmtp_send(mailsmtp * session,
const char * from,
clist * addresses,
const char * message, size_t size);
LIBETPAN_EXPORT
clist * esmtp_address_list_new(void);
LIBETPAN_EXPORT
int esmtp_address_list_add(clist * list, char * address,
int notify, char * orcpt);
LIBETPAN_EXPORT
void esmtp_address_list_free(clist * l);
LIBETPAN_EXPORT
clist * smtp_address_list_new(void);
LIBETPAN_EXPORT
int smtp_address_list_add(clist * list, char * address);
LIBETPAN_EXPORT
void smtp_address_list_free(clist * l);
#ifdef __cplusplus
}
#endif
#endif