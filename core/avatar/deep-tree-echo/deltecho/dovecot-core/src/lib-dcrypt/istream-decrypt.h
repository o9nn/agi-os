#ifndef ISTREAM_DECRYPT_H
#define ISTREAM_DECRYPT_H
struct dcrypt_private_key;
struct dcrypt_context_symmetric;
enum decrypt_istream_format {
DECRYPT_FORMAT_V1,
DECRYPT_FORMAT_V2
};
typedef int
i_stream_decrypt_get_key_callback_t(const char *pubkey_digest,
struct dcrypt_private_key **priv_key_r,
const char **error_r, void *context);
struct istream *
i_stream_create_decrypt(struct istream *input,
struct dcrypt_private_key *priv_key);
struct istream *
i_stream_create_sym_decrypt(struct istream *input,
struct dcrypt_context_symmetric *ctx);
struct istream *
i_stream_create_decrypt_callback(struct istream *input,
i_stream_decrypt_get_key_callback_t *callback,
void *context);
enum decrypt_istream_format
i_stream_encrypt_get_format(const struct istream *input);
enum io_stream_encrypt_flags
i_stream_encrypt_get_flags(const struct istream *input);
#endif