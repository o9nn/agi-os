#ifndef MAILMBOX_PARSE_H
#define MAILMBOX_PARSE_H
#ifdef __cplusplus
extern "C" {
#endif
#include "mailmbox_types.h"
int mailmbox_parse(struct mailmbox_folder * folder);
int
mailmbox_parse_additionnal(struct mailmbox_folder * folder,
size_t * indx);
#ifdef __cplusplus
}
#endif
#endif