#ifndef OSTREAM_ENCRYPT_H
#define OSTREAM_ENCRYPT_H
struct dcrypt_public_key;
struct dcrypt_context_symmetric;
struct ostream *
o_stream_create_encrypt(struct ostream *output, const char *algorithm,
struct dcrypt_public_key *box_pub,
enum io_stream_encrypt_flags flags);
struct ostream *
o_stream_create_sym_encrypt(struct ostream *output,
struct dcrypt_context_symmetric *ctx);
#endif