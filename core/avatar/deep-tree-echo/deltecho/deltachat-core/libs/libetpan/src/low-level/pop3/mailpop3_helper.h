#ifndef MAILPOP3_HELPER_H
#define MAILPOP3_HELPER_H
#ifdef __cplusplus
extern "C" {
#endif
#include "mailpop3.h"
int mailpop3_login_apop(mailpop3 * f,
const char * user,
const char * password);
int mailpop3_login(mailpop3 * f,
const char * user,
const char * password);
int mailpop3_header(mailpop3 * f, uint32_t indx, char ** result,
size_t * result_len);
void mailpop3_header_free(char * str);
#ifdef __cplusplus
}
#endif
#endif