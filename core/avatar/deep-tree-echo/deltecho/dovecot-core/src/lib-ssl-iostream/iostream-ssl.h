#ifndef IOSTREAM_SSL_H
#define IOSTREAM_SSL_H
struct ssl_iostream;
struct ssl_iostream_context;
struct ssl_iostream_cert {
const char *cert;
const char *key;
const char *key_password;
};
struct ssl_iostream_settings {
const char *min_protocol;
const char *cipher_list;
const char *ciphersuites;
const char *curve_list;
const char *ca, *ca_file, *ca_dir;
struct ssl_iostream_cert cert;
struct ssl_iostream_cert alt_cert;
const char *dh;
const char *cert_username_field;
const char *crypto_device;
bool verbose, verbose_invalid_cert;
bool skip_crl_check;
bool verify_remote_cert;
bool allow_invalid_cert;
bool prefer_server_ciphers;
bool compression;
bool tickets;
};
int ssl_module_load(const char **error_r);
typedef int
ssl_iostream_handshake_callback_t(const char **error_r, void *context);
typedef int ssl_iostream_sni_callback_t(const char *name, const char **error_r,
void *context);
int io_stream_ssl_global_init(const struct ssl_iostream_settings *set,
const char **error_r);
int io_stream_create_ssl_client(struct ssl_iostream_context *ctx, const char *host,
const struct ssl_iostream_settings *set,
struct event *event_parent,
struct istream **input, struct ostream **output,
struct ssl_iostream **iostream_r,
const char **error_r);
int io_stream_create_ssl_server(struct ssl_iostream_context *ctx,
const struct ssl_iostream_settings *set,
struct event *event_parent,
struct istream **input, struct ostream **output,
struct ssl_iostream **iostream_r,
const char **error_r);
void ssl_iostream_destroy(struct ssl_iostream **ssl_io);
void ssl_iostream_set_log_prefix(struct ssl_iostream *ssl_io,
const char *prefix);
int ssl_iostream_handshake(struct ssl_iostream *ssl_io);
void ssl_iostream_set_handshake_callback(struct ssl_iostream *ssl_io,
ssl_iostream_handshake_callback_t *callback,
void *context);
void ssl_iostream_set_sni_callback(struct ssl_iostream *ssl_io,
ssl_iostream_sni_callback_t *callback,
void *context);
void ssl_iostream_change_context(struct ssl_iostream *ssl_io,
struct ssl_iostream_context *ctx);
bool ssl_iostream_is_handshaked(const struct ssl_iostream *ssl_io);
bool ssl_iostream_has_handshake_failed(const struct ssl_iostream *ssl_io);
bool ssl_iostream_has_valid_client_cert(const struct ssl_iostream *ssl_io);
bool ssl_iostream_has_broken_client_cert(struct ssl_iostream *ssl_io);
int ssl_iostream_check_cert_validity(struct ssl_iostream *ssl_io,
const char *host, const char **error_r);
bool ssl_iostream_cert_match_name(struct ssl_iostream *ssl_io, const char *name,
const char **reason_r);
const char *ssl_iostream_get_peer_name(struct ssl_iostream *ssl_io);
const char *ssl_iostream_get_compression(struct ssl_iostream *ssl_io);
const char *ssl_iostream_get_server_name(struct ssl_iostream *ssl_io);
const char *ssl_iostream_get_security_string(struct ssl_iostream *ssl_io);
const char *ssl_iostream_get_ja3(struct ssl_iostream *ssl_io);
const char *ssl_iostream_get_cipher(struct ssl_iostream *ssl_io,
unsigned int *bits_r);
const char *ssl_iostream_get_pfs(struct ssl_iostream *ssl_io);
const char *ssl_iostream_get_protocol_name(struct ssl_iostream *ssl_io);
const char *ssl_iostream_get_last_error(struct ssl_iostream *ssl_io);
int ssl_iostream_context_init_client(const struct ssl_iostream_settings *set,
struct ssl_iostream_context **ctx_r,
const char **error_r);
int ssl_iostream_context_init_server(const struct ssl_iostream_settings *set,
struct ssl_iostream_context **ctx_r,
const char **error_r);
void ssl_iostream_context_ref(struct ssl_iostream_context *ctx);
void ssl_iostream_context_unref(struct ssl_iostream_context **ctx);
struct ssl_iostream_settings *ssl_iostream_settings_dup(pool_t pool,
const struct ssl_iostream_settings *old_set);
void ssl_iostream_settings_init_from(pool_t pool,
struct ssl_iostream_settings *dest,
const struct ssl_iostream_settings *src);
int ssl_iostream_client_context_cache_get(const struct ssl_iostream_settings *set,
struct ssl_iostream_context **ctx_r,
const char **error_r);
int ssl_iostream_server_context_cache_get(const struct ssl_iostream_settings *set,
struct ssl_iostream_context **ctx_r,
const char **error_r);
void ssl_iostream_context_cache_free(void);
#endif