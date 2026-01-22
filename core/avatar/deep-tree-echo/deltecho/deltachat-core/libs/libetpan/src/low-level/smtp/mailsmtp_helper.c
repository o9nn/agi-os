#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "mailsmtp.h"
#include <string.h>
#include <stdlib.h>
#include "mail.h"
LIBETPAN_EXPORT
int mailsmtp_init(mailsmtp * session)
{
return mailsmtp_init_with_ip(session, 0);
}
LIBETPAN_EXPORT
int mailsmtp_init_with_ip(mailsmtp * session, int useip)
{
int r;
r = mailesmtp_ehlo_with_ip(session, useip);
if (r == MAILSMTP_NO_ERROR)
return MAILSMTP_NO_ERROR;
r = mailsmtp_helo_with_ip(session, useip);
if (r == MAILSMTP_NO_ERROR)
return MAILSMTP_NO_ERROR;
return r;
}
static int mailesmtp_send_mail(mailsmtp * session,
const char * from,
int return_full,
const char * envid,
clist * addresses,
const char * message, size_t size)
{
int r;
clistiter * l;
if (!session->esmtp)
return mailsmtp_send(session, from, addresses, message, size);
if ((session->esmtp & MAILSMTP_ESMTP_SIZE) != 0) {
if (session->smtp_max_msg_size != 0) {
if (size > session->smtp_max_msg_size) {
return MAILSMTP_ERROR_EXCEED_STORAGE_ALLOCATION;
}
}
}
r = mailesmtp_mail_size(session, from, return_full, envid, size);
if (r != MAILSMTP_NO_ERROR)
return r;
for(l = clist_begin(addresses) ; l != NULL; l = clist_next(l)) {
struct esmtp_address * addr;
addr = clist_content(l);
r = mailesmtp_rcpt(session, addr->address, addr->notify, addr->orcpt);
if (r != MAILSMTP_NO_ERROR)
return r;
}
r = mailsmtp_data(session);
if (r != MAILSMTP_NO_ERROR)
return r;
return MAILSMTP_NO_ERROR;
}
LIBETPAN_EXPORT
int mailesmtp_send(mailsmtp * session,
const char * from,
int return_full,
const char * envid,
clist * addresses,
const char * message, size_t size)
{
int r = mailesmtp_send_mail(session, from, return_full, envid, addresses, message, size);
if (r != MAILSMTP_NO_ERROR)
return r;
r = mailsmtp_data_message(session, message, size);
if (r != MAILSMTP_NO_ERROR)
return r;
return MAILSMTP_NO_ERROR;
}
LIBETPAN_EXPORT
int mailesmtp_send_quit(mailsmtp * session,
const char * from,
int return_full,
const char * envid,
clist * addresses,
const char * message, size_t size)
{
int r = mailesmtp_send_mail(session, from, return_full, envid, addresses, message, size);
if (r != MAILSMTP_NO_ERROR)
return r;
r = mailsmtp_data_message_quit(session, message, size);
if (r != MAILSMTP_NO_ERROR)
return r;
return MAILSMTP_NO_ERROR;
}
LIBETPAN_EXPORT
int mailesmtp_send_quit_no_disconnect(mailsmtp * session,
const char * from,
int return_full,
const char * envid,
clist * addresses,
const char * message, size_t size)
{
int r = mailesmtp_send_mail(session, from, return_full, envid, addresses, message, size);
if (r != MAILSMTP_NO_ERROR)
return r;
r = mailsmtp_data_message_quit_no_disconnect(session, message, size);
if (r != MAILSMTP_NO_ERROR)
return r;
return MAILSMTP_NO_ERROR;
}
LIBETPAN_EXPORT
int mailsmtp_send(mailsmtp * session,
const char * from,
clist * addresses,
const char * message, size_t size)
{
int r;
clistiter * l;
r = mailsmtp_mail(session, from);
if (r != MAILSMTP_NO_ERROR)
return r;
for(l = clist_begin(addresses) ; l != NULL; l = clist_next(l)) {
struct esmtp_address * addr;
addr = clist_content(l);
r = mailsmtp_rcpt(session, addr->address);
if (r != MAILSMTP_NO_ERROR)
return r;
}
r = mailsmtp_data(session);
if (r != MAILSMTP_NO_ERROR)
return r;
r = mailsmtp_data_message(session, message, size);
if (r != MAILSMTP_NO_ERROR)
return r;
return MAILSMTP_NO_ERROR;
}
static struct esmtp_address * esmtp_address_new(char * addr,
int notify, char * orcpt)
{
struct esmtp_address * esmtpa;
esmtpa = malloc(sizeof(* esmtpa));
if (esmtpa == NULL)
return NULL;
esmtpa->address = strdup(addr);
if (esmtpa->address == NULL) {
free(esmtpa);
return NULL;
}
if (orcpt != NULL) {
esmtpa->orcpt = strdup(orcpt);
if (esmtpa->orcpt == NULL) {
free(esmtpa->address);
free(esmtpa);
return NULL;
}
}
else
esmtpa->orcpt = NULL;
esmtpa->notify = notify;
return esmtpa;
}
static void esmtp_address_free(struct esmtp_address * addr)
{
if (addr->orcpt)
free(addr->orcpt);
if (addr->address)
free(addr->address);
free(addr);
}
LIBETPAN_EXPORT
clist * esmtp_address_list_new(void)
{
return clist_new();
}
LIBETPAN_EXPORT
void esmtp_address_list_free(clist * l)
{
clist_foreach(l, (clist_func) esmtp_address_free, NULL);
clist_free(l);
}
LIBETPAN_EXPORT
int esmtp_address_list_add(clist * list, char * address,
int notify, char * orcpt)
{
struct esmtp_address * esmtpa;
int r;
esmtpa = esmtp_address_new(address, notify, orcpt);
if (esmtpa == NULL)
return -1;
r = clist_append(list, esmtpa);
if (r < 0) {
esmtp_address_free(esmtpa);
return -1;
}
return 0;
}
LIBETPAN_EXPORT
clist * smtp_address_list_new(void)
{
return esmtp_address_list_new();
}
LIBETPAN_EXPORT
int smtp_address_list_add(clist * list, char * address)
{
return esmtp_address_list_add(list, address, 0, NULL);
}
LIBETPAN_EXPORT
void smtp_address_list_free(clist * l)
{
esmtp_address_list_free(l);
}