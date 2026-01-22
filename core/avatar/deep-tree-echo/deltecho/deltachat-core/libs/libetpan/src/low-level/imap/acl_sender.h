#ifndef ACL_SENDER_H
#define ACL_SENDER_H
#include "mailimap_sender.h"
#include "acl_types.h"
#ifdef __cplusplus
extern "C" {
#endif
int mailimap_acl_setacl_send(mailstream * fd,
const char * mailbox,
const char * identifier,
const char * mod_rights);
int mailimap_acl_deleteacl_send(mailstream * fd,
const char * mailbox,
const char * identifier);
int mailimap_acl_getacl_send(mailstream * fd,
const char * mailbox);
int mailimap_acl_listrights_send(mailstream * fd,
const char * mailbox,
const char * identifier);
int mailimap_acl_myrights_send(mailstream * fd,
const char * mailbox);
#ifdef __cplusplus
}
#endif
#endif