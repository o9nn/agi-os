#ifndef NAMESPACE_SENDER_H
#define NAMESPACE_SENDER_H
#include "mailimap_sender.h"
#ifdef __cplusplus
extern "C" {
#endif
int mailimap_namespace_send(mailstream * fd);
#ifdef __cplusplus
}
#endif
#endif