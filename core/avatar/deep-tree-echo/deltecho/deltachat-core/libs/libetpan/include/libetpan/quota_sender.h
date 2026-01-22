#ifndef QUOTA_SENDER_H
#define QUOTA_SENDER_H
#include "mailimap_sender.h"
#ifdef __cplusplus
extern "C" {
#endif
int mailimap_quota_getquota_send(mailstream * fd,
const char * quotaroot);
int mailimap_quota_getquotaroot_send(mailstream * fd,
const char * list_mb);
#ifdef __cplusplus
}
#endif
#endif