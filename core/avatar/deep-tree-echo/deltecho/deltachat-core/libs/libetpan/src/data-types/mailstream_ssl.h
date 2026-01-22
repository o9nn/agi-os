#ifndef MAILSTREAM_SSL_H
#define MAILSTREAM_SSL_H
#include <libetpan/mailstream.h>
#ifdef __cplusplus
extern "C" {
#endif
#if LIBETPAN_IOS_DISABLE_SSL
#undef USE_SSL
#endif
#ifdef USE_SSL
extern mailstream_low_driver * mailstream_ssl_driver;
#endif
struct mailstream_ssl_context;
LIBETPAN_EXPORT
mailstream_low * mailstream_low_ssl_open(int fd);
LIBETPAN_EXPORT
mailstream_low * mailstream_low_ssl_open_timeout(int fd, time_t timeout);
LIBETPAN_EXPORT
mailstream_low * mailstream_low_tls_open(int fd);
LIBETPAN_EXPORT
mailstream_low * mailstream_low_tls_open_timeout(int fd, time_t timeout);
LIBETPAN_EXPORT
mailstream * mailstream_ssl_open(int fd);
LIBETPAN_EXPORT
mailstream * mailstream_ssl_open_timeout(int fd, time_t timeout);
LIBETPAN_EXPORT
mailstream * mailstream_ssl_open_with_callback(int fd,
void (* callback)(struct mailstream_ssl_context * ssl_context, void * data), void * data);
LIBETPAN_EXPORT
mailstream * mailstream_ssl_open_with_callback_timeout(int fd, time_t timeout,
void (* callback)(struct mailstream_ssl_context * ssl_context, void * data), void * data);
LIBETPAN_EXPORT
void mailstream_gnutls_init_not_required(void);
LIBETPAN_EXPORT
void mailstream_openssl_init_not_required(void);
LIBETPAN_EXPORT
void mailstream_ssl_init_not_required(void);
LIBETPAN_EXPORT
ssize_t mailstream_ssl_get_certificate(mailstream *stream, unsigned char **cert_DER);
LIBETPAN_EXPORT
mailstream_low * mailstream_low_ssl_open_with_callback(int fd,
void (* callback)(struct mailstream_ssl_context * ssl_context, void * data), void * data);
LIBETPAN_EXPORT
mailstream_low * mailstream_low_ssl_open_with_callback_timeout(int fd, time_t timeout,
void (* callback)(struct mailstream_ssl_context * ssl_context, void * data), void * data);
LIBETPAN_EXPORT
mailstream_low * mailstream_low_tls_open_with_callback(int fd,
void (* callback)(struct mailstream_ssl_context * ssl_context, void * data), void * data);
LIBETPAN_EXPORT
mailstream_low * mailstream_low_tls_open_with_callback_timeout(int fd, time_t timeout,
void (* callback)(struct mailstream_ssl_context * ssl_context, void * data), void * data);
LIBETPAN_EXPORT
int mailstream_ssl_set_client_certicate(struct mailstream_ssl_context * ssl_context,
char * file_name);
LIBETPAN_EXPORT
int mailstream_ssl_set_client_certificate_data(struct mailstream_ssl_context * ssl_context,
unsigned char *x509_der, size_t len);
int mailstream_ssl_set_client_private_key_data(struct mailstream_ssl_context * ssl_context,
unsigned char *pkey_der, size_t len);
LIBETPAN_EXPORT
int mailstream_ssl_set_server_certicate(struct mailstream_ssl_context * ssl_context,
char * CAfile, char * CApath);
LIBETPAN_EXPORT
void * mailstream_ssl_get_openssl_ssl_ctx(struct mailstream_ssl_context * ssl_context);
LIBETPAN_EXPORT
int mailstream_ssl_get_fd(struct mailstream_ssl_context * ssl_context);
#ifdef __cplusplus
}
#endif
#endif