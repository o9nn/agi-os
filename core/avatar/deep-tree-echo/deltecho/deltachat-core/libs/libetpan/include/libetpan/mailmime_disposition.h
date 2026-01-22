#ifndef MAILMIME_DISPOSITION_H
#define MAILMIME_DISPOSITION_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/mailmime_types.h>
int mailmime_disposition_parse(const char * message, size_t length,
size_t * indx,
struct mailmime_disposition ** result);
int
mailmime_disposition_type_parse(const char * message, size_t length,
size_t * indx,
struct mailmime_disposition_type ** result);
int mailmime_disposition_guess_type(const char * message, size_t length,
size_t indx);
#ifdef __cplusplus
}
#endif
#endif