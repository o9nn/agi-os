#ifndef IMAP_MSGPART_URL_H
#define IMAP_MSGPART_URL_H
#include "imap-msgpart.h"
struct imap_url;
struct imap_msgpart;
struct imap_msgpart_url;
int imap_msgpart_url_create(struct mail_user *user, const struct imap_url *url,
struct imap_msgpart_url **url_r,
const char **client_error_r);
int imap_msgpart_url_parse(struct mail_user *user, struct mailbox *selected_box,
const char *urlstr, struct imap_msgpart_url **url_r,
const char **client_error_r);
int imap_msgpart_url_open_mailbox(struct imap_msgpart_url *mpurl,
struct mailbox **box_r, enum mail_error *error_code_r,
const char **client_error_r);
struct mailbox *imap_msgpart_url_get_mailbox(struct imap_msgpart_url *mpurl);
int imap_msgpart_url_open_mail(struct imap_msgpart_url *mpurl,
struct mail **mail_r,
const char **client_error_r);
struct imap_msgpart *
imap_msgpart_url_get_part(struct imap_msgpart_url *mpurl);
void imap_msgpart_url_set_decode_to_binary(struct imap_msgpart_url *mpurl);
int imap_msgpart_url_read_part(struct imap_msgpart_url *mpurl,
struct imap_msgpart_open_result *result_r,
const char **client_error_r);
int imap_msgpart_url_get_bodypartstructure(struct imap_msgpart_url *mpurl,
const char **bpstruct_r,
const char **client_error_r);
int imap_msgpart_url_verify(struct imap_msgpart_url *mpurl,
const char **client_error_r);
void imap_msgpart_url_free(struct imap_msgpart_url **mpurl);
#endif