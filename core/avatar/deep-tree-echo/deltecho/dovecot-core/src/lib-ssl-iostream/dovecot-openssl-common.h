#ifndef DOVECOT_OPENSSL_COMMON_H
#define DOVECOT_OPENSSL_COMMON_H
void dovecot_openssl_common_global_ref(void);
bool dovecot_openssl_common_global_unref(void);
int dovecot_openssl_common_global_set_engine(const char *engine,
const char **error_r);
#endif