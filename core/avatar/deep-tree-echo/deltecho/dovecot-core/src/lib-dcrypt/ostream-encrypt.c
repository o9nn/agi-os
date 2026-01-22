#include "lib.h"
#include "buffer.h"
#include "randgen.h"
#include "dcrypt-iostream.h"
#include "ostream-encrypt.h"
#include "ostream-private.h"
#include "hash-method.h"
#include "sha2.h"
#include "safe-memset.h"
#include "dcrypt.h"
#include <arpa/inet.h>
#define IO_STREAM_ENCRYPT_SEED_SIZE 32
#define IO_STREAM_ENCRYPT_ROUNDS 2048
struct encrypt_ostream {
struct ostream_private ostream;
struct dcrypt_context_symmetric *ctx_sym;
struct dcrypt_context_hmac *ctx_mac;
enum io_stream_encrypt_flags flags;
struct dcrypt_public_key *pub;
unsigned char *key_data;
size_t key_data_len;
buffer_t *cipher_oid;
buffer_t *mac_oid;
size_t block_size;
bool finalized;
bool failed;
bool prefix_written;
};
static int
o_stream_encrypt_send(struct encrypt_ostream *stream,
const unsigned char *data, size_t size)
{
ssize_t ec;
ec = o_stream_send(stream->ostream.parent, data, size);
if (ec == (ssize_t)size)
return 0;
else if (ec < 0) {
o_stream_copy_error_from_parent(&stream->ostream);
return -1;
} else {
io_stream_set_error(&stream->ostream.iostream,
"ostream-encrypt: "
"Unexpectedly short write to parent stream");
stream->ostream.ostream.stream_errno = EINVAL;
return -1;
}
}
static int
o_stream_encrypt_send_header_v1(struct encrypt_ostream *stream)
{
unsigned char c;
unsigned short s;
i_assert(!stream->prefix_written);
stream->prefix_written = TRUE;
buffer_t *values = t_buffer_create(256);
buffer_append(values, IOSTREAM_CRYPT_MAGIC,
sizeof(IOSTREAM_CRYPT_MAGIC));
c = 1;
buffer_append(values, &c, 1);
s = htons(stream->key_data_len);
buffer_append(values, &s, 2);
buffer_append(values, stream->key_data, stream->key_data_len);
i_free_and_null(stream->key_data);
return o_stream_encrypt_send(stream, values->data, values->used);
}
static int
o_stream_encrypt_send_header_v2(struct encrypt_ostream *stream)
{
unsigned char c;
unsigned int i;
i_assert(!stream->prefix_written);
stream->prefix_written = TRUE;
buffer_t *values = t_buffer_create(256);
buffer_append(values, IOSTREAM_CRYPT_MAGIC,
sizeof(IOSTREAM_CRYPT_MAGIC));
c = 2;
buffer_append(values, &c, 1);
i = cpu32_to_be(stream->flags);
buffer_append(values, &i, 4);
i = cpu32_to_be(sizeof(IOSTREAM_CRYPT_MAGIC) + 9 +
stream->cipher_oid->used + stream->mac_oid->used +
8 + stream->key_data_len);
buffer_append(values, &i, 4);
buffer_append_buf(values, stream->cipher_oid, 0, SIZE_MAX);
buffer_append_buf(values, stream->mac_oid, 0, SIZE_MAX);
i = cpu32_to_be(IO_STREAM_ENCRYPT_ROUNDS);
buffer_append(values, &i, 4);
i = cpu32_to_be(stream->key_data_len);
buffer_append(values, &i, 4);
buffer_append(values, stream->key_data, stream->key_data_len);
i_free_and_null(stream->key_data);
return o_stream_encrypt_send(stream, values->data, values->used);
}
static int
o_stream_encrypt_keydata_create_v1(struct encrypt_ostream *stream)
{
buffer_t *encrypted_key, *ephemeral_key, *secret, *res, buf;
const char *error = NULL;
const struct hash_method *hash = &hash_method_sha256;
unsigned char seed[IO_STREAM_ENCRYPT_SEED_SIZE];
unsigned char pkhash[hash->digest_size];
unsigned char ekhash[hash->digest_size];
unsigned char hres[hash->digest_size];
unsigned char hctx[hash->context_size];
buffer_create_from_data(&buf, pkhash, sizeof(pkhash));
if (!dcrypt_key_id_public_old(stream->pub, &buf, &error)) {
io_stream_set_error(&stream->ostream.iostream,
"Key hash failed: %s", error);
return -1;
}
hash->init(hctx);
hash->loop(hctx, seed, sizeof(seed));
hash->result(hctx, ekhash);
ephemeral_key = t_buffer_create(256);
encrypted_key = t_buffer_create(256);
secret = t_buffer_create(256);
if (!dcrypt_ecdh_derive_secret_peer(stream->pub, ephemeral_key,
secret, &error)) {
io_stream_set_error(&stream->ostream.iostream,
"Cannot perform ECDH: %s", error);
return -1;
}
hash->init(hctx);
hash->loop(hctx, secret->data, secret->used);
hash->result(hctx, hres);
safe_memset(buffer_get_modifiable_data(secret, 0), 0, secret->used);
struct dcrypt_context_symmetric *dctx;
if (!dcrypt_ctx_sym_create("aes-256-ctr", DCRYPT_MODE_ENCRYPT,
&dctx, &error)) {
io_stream_set_error(&stream->ostream.iostream,
"Key encryption error: %s", error);
return -1;
}
random_fill(seed, sizeof(seed));
hash->init(hctx);
hash->loop(hctx, seed, sizeof(seed));
hash->result(hctx, ekhash);
int ec = 0;
dcrypt_ctx_sym_set_iv(dctx, (const unsigned char*)
"\x00\x00\x00\x00\x00\x00\x00\x00"
"\x00\x00\x00\x00\x00\x00\x00\x00", 16);
dcrypt_ctx_sym_set_key(dctx, hres, sizeof(hres));
if (!dcrypt_ctx_sym_init(dctx, &error) ||
!dcrypt_ctx_sym_update(dctx, seed, sizeof(seed),
encrypted_key, &error) ||
!dcrypt_ctx_sym_final(dctx, encrypted_key, &error)) {
ec = -1;
}
dcrypt_ctx_sym_destroy(&dctx);
if (ec != 0) {
safe_memset(seed, 0, sizeof(seed));
io_stream_set_error(&stream->ostream.iostream,
"Key encryption error: %s", error);
return -1;
}
dcrypt_ctx_sym_set_iv(stream->ctx_sym, (const unsigned char*)
"\x00\x00\x00\x00\x00\x00\x00\x00"
"\x00\x00\x00\x00\x00\x00\x00\x00", 16);
dcrypt_ctx_sym_set_key(stream->ctx_sym, seed, sizeof(seed));
safe_memset(seed, 0, sizeof(seed));
if (!dcrypt_ctx_sym_init(stream->ctx_sym, &error)) {
io_stream_set_error(&stream->ostream.iostream,
"Encryption init error: %s", error);
return -1;
}
res = buffer_create_dynamic(default_pool, 256);
unsigned short s;
s = htons(ephemeral_key->used);
buffer_append(res, &s, 2);
buffer_append(res, ephemeral_key->data, ephemeral_key->used);
s = htons(sizeof(pkhash));
buffer_append(res, &s, 2);
buffer_append(res, pkhash, sizeof(pkhash));
s = htons(sizeof(ekhash));
buffer_append(res, &s, 2);
buffer_append(res, ekhash, sizeof(ekhash));
s = htons(encrypted_key->used);
buffer_append(res, &s, 2);
buffer_append(res, encrypted_key->data, encrypted_key->used);
stream->key_data_len = res->used;
stream->key_data = buffer_free_without_data(&res);
return 0;
}
static int
o_stream_encrypt_key_for_pubkey_v2(struct encrypt_ostream *stream,
const char *malg, const unsigned char *key,
size_t key_len,
struct dcrypt_public_key *pubkey,
buffer_t *res)
{
enum dcrypt_key_type ktype;
const char *error;
buffer_t *encrypted_key, *ephemeral_key, *temp_key;
ephemeral_key = t_buffer_create(256);
encrypted_key = t_buffer_create(256);
temp_key = t_buffer_create(48);
ktype = dcrypt_key_type_public(pubkey);
if (ktype == DCRYPT_KEY_RSA) {
if (!dcrypt_rsa_encrypt(pubkey, key, key_len, encrypted_key,
DCRYPT_PADDING_RSA_PKCS1_OAEP,
&error)) {
io_stream_set_error(&stream->ostream.iostream,
"Cannot encrypt key data: %s",
error);
return -1;
}
} else if (ktype == DCRYPT_KEY_EC) {
buffer_t *secret = t_buffer_create(256);
if (!dcrypt_ecdh_derive_secret_peer(pubkey, ephemeral_key,
secret, &error)) {
io_stream_set_error(&stream->ostream.iostream,
"Cannot perform ECDH: %s", error);
return -1;
}
if (!dcrypt_pbkdf2(secret->data, secret->used,
ephemeral_key->data, ephemeral_key->used,
malg, IO_STREAM_ENCRYPT_ROUNDS, temp_key,
48, &error)) {
safe_memset(buffer_get_modifiable_data(secret, 0),
0, secret->used);
io_stream_set_error(&stream->ostream.iostream,
"Cannot perform key encryption: %s",
error);
}
safe_memset(buffer_get_modifiable_data(secret, 0),
0, secret->used);
struct dcrypt_context_symmetric *dctx;
if (!dcrypt_ctx_sym_create("AES-256-CBC", DCRYPT_MODE_ENCRYPT,
&dctx, &error)) {
safe_memset(buffer_get_modifiable_data(temp_key, 0),
0, temp_key->used);
io_stream_set_error(&stream->ostream.iostream,
"Cannot perform key encryption: %s",
error);
return -1;
}
const unsigned char *ptr = temp_key->data;
i_assert(temp_key->used == 48);
dcrypt_ctx_sym_set_key(dctx, ptr, 32);
dcrypt_ctx_sym_set_iv(dctx, ptr+32, 16);
safe_memset(buffer_get_modifiable_data(temp_key, 0),
0, temp_key->used);
int ec = 0;
if (!dcrypt_ctx_sym_init(dctx, &error) ||
!dcrypt_ctx_sym_update(dctx, key, key_len,
encrypted_key, &error) ||
!dcrypt_ctx_sym_final(dctx, encrypted_key, &error)) {
io_stream_set_error(&stream->ostream.iostream,
"Cannot perform key encryption: %s",
error);
ec = -1;
}
dcrypt_ctx_sym_destroy(&dctx);
if (ec != 0) return ec;
} else {
io_stream_set_error(&stream->ostream.iostream,
"Unsupported key type");
return -1;
}
char kt = ktype;
buffer_append(res, &kt, 1);
if (!dcrypt_key_id_public(stream->pub, "sha256", res, &error)) {
io_stream_set_error(&stream->ostream.iostream,
"Cannot hash public key: %s", error);
return -1;
}
unsigned int val = cpu32_to_be(ephemeral_key->used);
buffer_append(res, &val, 4);
buffer_append_buf(res, ephemeral_key, 0, SIZE_MAX);
val = cpu32_to_be(encrypted_key->used);
buffer_append(res, &val, 4);
buffer_append_buf(res, encrypted_key, 0, SIZE_MAX);
return 0;
}
static int
o_stream_encrypt_keydata_create_v2(struct encrypt_ostream *stream,
const char *malg)
{
const struct hash_method *hash = hash_method_lookup(malg);
const char *error;
size_t tagsize;
const unsigned char *ptr;
size_t kl;
unsigned int val;
buffer_t *keydata, *res;
if (hash == NULL) {
io_stream_set_error(&stream->ostream.iostream,
"Encryption init error: "
"Hash algorithm '%s' not supported", malg);
return -1;
}
if ((stream->flags & IO_STREAM_ENC_INTEGRITY_HMAC) ==
IO_STREAM_ENC_INTEGRITY_HMAC) {
tagsize = IOSTREAM_TAG_SIZE;
} else if ((stream->flags & IO_STREAM_ENC_INTEGRITY_AEAD) ==
IO_STREAM_ENC_INTEGRITY_AEAD) {
tagsize = IOSTREAM_TAG_SIZE;
} else {
tagsize = 0;
}
kl = dcrypt_ctx_sym_get_key_length(stream->ctx_sym) +
dcrypt_ctx_sym_get_iv_length(stream->ctx_sym) + tagsize;
keydata = t_buffer_create(kl);
random_fill(buffer_append_space_unsafe(keydata, kl), kl);
buffer_set_used_size(keydata, kl);
ptr = keydata->data;
res = buffer_create_dynamic(default_pool, 256);
buffer_append(res, "\1", 1);
if (o_stream_encrypt_key_for_pubkey_v2(stream, malg, ptr, kl,
stream->pub, res) != 0) {
buffer_free(&res);
return -1;
}
unsigned char hctx[hash->context_size];
unsigned char hres[hash->digest_size];
hash->init(hctx);
hash->loop(hctx, ptr, kl);
hash->result(hctx, hres);
for(int i = 1; i < 2049; i++) {
uint32_t i_msb = cpu32_to_be(i);
hash->init(hctx);
hash->loop(hctx, hres, sizeof(hres));
hash->loop(hctx, &i_msb, sizeof(i_msb));
hash->result(hctx, hres);
}
val = cpu32_to_be(sizeof(hres));
buffer_append(res, &val, 4);
buffer_append(res, hres, sizeof(hres));
stream->key_data_len = res->used;
stream->key_data = buffer_free_without_data(&res);
dcrypt_ctx_sym_set_key(stream->ctx_sym, ptr,
dcrypt_ctx_sym_get_key_length(stream->ctx_sym));
ptr += dcrypt_ctx_sym_get_key_length(stream->ctx_sym);
dcrypt_ctx_sym_set_iv(stream->ctx_sym, ptr,
dcrypt_ctx_sym_get_iv_length(stream->ctx_sym));
ptr += dcrypt_ctx_sym_get_iv_length(stream->ctx_sym);
if ((stream->flags & IO_STREAM_ENC_INTEGRITY_HMAC) ==
IO_STREAM_ENC_INTEGRITY_HMAC) {
dcrypt_ctx_hmac_set_key(stream->ctx_mac, ptr, tagsize);
dcrypt_ctx_hmac_init(stream->ctx_mac, &error);
} else if ((stream->flags & IO_STREAM_ENC_INTEGRITY_AEAD) ==
IO_STREAM_ENC_INTEGRITY_AEAD) {
dcrypt_ctx_sym_set_aad(stream->ctx_sym, ptr, tagsize);
}
safe_memset(buffer_get_modifiable_data(keydata, 0), 0, keydata->used);
if (!dcrypt_ctx_sym_init(stream->ctx_sym, &error)) {
io_stream_set_error(&stream->ostream.iostream,
"Encryption init error: %s", error);
return -1;
}
return 0;
}
static ssize_t
o_stream_encrypt_sendv(struct ostream_private *stream,
const struct const_iovec *iov, unsigned int iov_count)
{
struct encrypt_ostream *estream = (struct encrypt_ostream *)stream;
const char *error;
ssize_t ec,total = 0;
i_assert(!estream->finalized);
if (!estream->prefix_written) {
T_BEGIN {
if ((estream->flags & IO_STREAM_ENC_VERSION_1) ==
IO_STREAM_ENC_VERSION_1)
ec = o_stream_encrypt_send_header_v1(estream);
else
ec = o_stream_encrypt_send_header_v2(estream);
} T_END;
if (ec < 0) {
return -1;
}
}
unsigned char ciphertext[IO_BLOCK_SIZE];
buffer_t buf;
buffer_create_from_data(&buf, ciphertext, sizeof(ciphertext));
for(unsigned int i = 0; i < iov_count; i++) {
size_t bl, off = 0, len = iov[i].iov_len;
const unsigned char *ptr = iov[i].iov_base;
while(len > 0) {
buffer_set_used_size(&buf, 0);
bl = I_MIN(sizeof(ciphertext)/2, len);
if (!dcrypt_ctx_sym_update(estream->ctx_sym, ptr + off,
bl, &buf, &error)) {
io_stream_set_error(&stream->iostream,
"Encryption failure: %s",
error);
return -1;
}
if ((estream->flags & IO_STREAM_ENC_INTEGRITY_HMAC) ==
IO_STREAM_ENC_INTEGRITY_HMAC) {
if (!dcrypt_ctx_hmac_update(estream->ctx_mac,
buf.data, buf.used, &error)) {
io_stream_set_error(&stream->iostream,
"MAC failure: %s", error);
return -1;
}
}
if (o_stream_encrypt_send(estream, buf.data, buf.used) < 0) {
return -1;
}
len -= bl;
off += bl;
total += bl;
}
}
stream->ostream.offset += total;
return total;
}
static int
o_stream_encrypt_finalize(struct ostream_private *stream)
{
const char *error;
struct encrypt_ostream *estream = (struct encrypt_ostream *)stream;
if (estream->finalized) {
return 0;
}
estream->finalized = TRUE;
if (!estream->prefix_written) return 0;
buffer_t *buf = t_buffer_create(
dcrypt_ctx_sym_get_block_size(estream->ctx_sym));
if (!dcrypt_ctx_sym_final(estream->ctx_sym, buf, &error)) {
io_stream_set_error(&estream->ostream.iostream,
"Encryption failure: %s", error);
return -1;
}
if (buf->used > 0) {
if (((estream->flags & IO_STREAM_ENC_INTEGRITY_HMAC) ==
IO_STREAM_ENC_INTEGRITY_HMAC)) {
if (!dcrypt_ctx_hmac_update(estream->ctx_mac, buf->data,
buf->used, &error)) {
io_stream_set_error(&estream->ostream.iostream,
"MAC failure: %s", error);
return -1;
}
}
if (o_stream_encrypt_send(estream, buf->data, buf->used) < 0) {
return -1;
}
}
buffer_set_used_size(buf, 0);
if ((estream->flags & IO_STREAM_ENC_INTEGRITY_HMAC) ==
IO_STREAM_ENC_INTEGRITY_HMAC) {
if (!dcrypt_ctx_hmac_final(estream->ctx_mac, buf, &error)) {
io_stream_set_error(&estream->ostream.iostream,
"MAC failure: %s", error);
return -1;
}
} else if ((estream->flags & IO_STREAM_ENC_INTEGRITY_AEAD) ==
IO_STREAM_ENC_INTEGRITY_AEAD) {
dcrypt_ctx_sym_get_tag(estream->ctx_sym, buf);
i_assert(buf->used > 0);
}
if (buf->used > 0 &&
o_stream_encrypt_send(estream, buf->data, buf->used) < 0) {
return -1;
}
return 0;
}
static int
o_stream_encrypt_flush(struct ostream_private *stream)
{
struct encrypt_ostream *estream = (struct encrypt_ostream *)stream;
if (stream->finished && estream->ctx_sym != NULL &&
!estream->finalized) {
if (o_stream_encrypt_finalize(&estream->ostream) < 0)
return -1;
}
return o_stream_flush_parent(stream);
}
static void
o_stream_encrypt_close(struct iostream_private *stream,
bool close_parent)
{
struct encrypt_ostream *estream = (struct encrypt_ostream *)stream;
i_assert(estream->finalized || estream->ctx_sym == NULL ||
estream->ostream.ostream.stream_errno != 0);
if (close_parent)
o_stream_close(estream->ostream.parent);
}
static void
o_stream_encrypt_destroy(struct iostream_private *stream)
{
struct encrypt_ostream *estream = (struct encrypt_ostream *)stream;
if (estream->ctx_sym != NULL)
dcrypt_ctx_sym_destroy(&estream->ctx_sym);
if (estream->ctx_mac != NULL)
dcrypt_ctx_hmac_destroy(&estream->ctx_mac);
if (estream->key_data != NULL)
i_free(estream->key_data);
if (estream->cipher_oid != NULL)
buffer_free(&estream->cipher_oid);
if (estream->mac_oid != NULL)
buffer_free(&estream->mac_oid);
if (estream->pub != NULL)
dcrypt_key_unref_public(&estream->pub);
o_stream_unref(&estream->ostream.parent);
}
static int
o_stream_encrypt_init(struct encrypt_ostream *estream, const char *algorithm)
{
const char *error;
char *calg, *malg;
if ((estream->flags & IO_STREAM_ENC_VERSION_1) ==
IO_STREAM_ENC_VERSION_1) {
if (!dcrypt_ctx_sym_create("AES-256-CTR", DCRYPT_MODE_ENCRYPT,
&estream->ctx_sym, &error)) {
io_stream_set_error(&estream->ostream.iostream,
"Cannot create ostream-encrypt: %s",
error);
return -1;
}
estream->flags |= IO_STREAM_ENC_INTEGRITY_NONE;
return o_stream_encrypt_keydata_create_v1(estream);
} else {
calg = t_strdup_noconst(algorithm);
malg = strrchr(calg, '-');
if (malg == NULL) {
io_stream_set_error(&estream->ostream.iostream,
"Invalid algorithm "
"(must be cipher-mac)");
return -1;
}
(*malg++) = '\0';
if (!dcrypt_ctx_sym_create(calg, DCRYPT_MODE_ENCRYPT,
&estream->ctx_sym, &error)) {
io_stream_set_error(&estream->ostream.iostream,
"Cannot create ostream-encrypt: %s",
error);
return -1;
}
estream->cipher_oid = buffer_create_dynamic(default_pool, 12);
estream->block_size =
dcrypt_ctx_sym_get_block_size(estream->ctx_sym);
if (!dcrypt_name2oid(calg, estream->cipher_oid, &error)) {
io_stream_set_error(&estream->ostream.iostream,
"Cannot create ostream-encrypt: %s",
error);
return -1;
}
if ((estream->flags & IO_STREAM_ENC_INTEGRITY_HMAC) ==
IO_STREAM_ENC_INTEGRITY_HMAC) {
if (!dcrypt_ctx_hmac_create(malg, &estream->ctx_mac,
&error)) {
io_stream_set_error(&estream->ostream.iostream,
"Cannot create ostream-encrypt: %s",
error);
return -1;
}
}
estream->mac_oid = buffer_create_dynamic(default_pool, 12);
if (!dcrypt_name2oid(malg, estream->mac_oid, &error)) {
io_stream_set_error(&estream->ostream.iostream,
"Cannot create ostream-encrypt: %s", error);
return -1;
}
return o_stream_encrypt_keydata_create_v2(estream, malg);
}
}
static struct encrypt_ostream *
o_stream_create_encrypt_common(enum io_stream_encrypt_flags flags)
{
struct encrypt_ostream *estream;
estream = i_new(struct encrypt_ostream, 1);
estream->ostream.sendv = o_stream_encrypt_sendv;
estream->ostream.flush = o_stream_encrypt_flush;
estream->ostream.iostream.close = o_stream_encrypt_close;
estream->ostream.iostream.destroy = o_stream_encrypt_destroy;
estream->flags = flags;
return estream;
}
struct ostream *
o_stream_create_encrypt(struct ostream *output, const char *algorithm,
struct dcrypt_public_key *box_pub, enum io_stream_encrypt_flags flags)
{
struct encrypt_ostream *estream = o_stream_create_encrypt_common(flags);
int ec;
dcrypt_key_ref_public(box_pub);
estream->pub = box_pub;
T_BEGIN {
ec = o_stream_encrypt_init(estream, algorithm);
} T_END;
struct ostream *os = o_stream_create(&estream->ostream, output,
o_stream_get_fd(output));
if (ec != 0) {
os->stream_errno = EINVAL;
}
return os;
}
struct ostream *
o_stream_create_sym_encrypt(struct ostream *output,
struct dcrypt_context_symmetric *ctx)
{
struct encrypt_ostream *estream =
o_stream_create_encrypt_common(IO_STREAM_ENC_INTEGRITY_NONE);
const char *error;
int ec;
estream->prefix_written = TRUE;
if (!dcrypt_ctx_sym_init(estream->ctx_sym, &error))
ec = -1;
else
ec = 0;
estream->ctx_sym = ctx;
struct ostream *os = o_stream_create(&estream->ostream, output,
o_stream_get_fd(output));
if (ec != 0) {
io_stream_set_error(&estream->ostream.iostream,
"Could not initialize stream: %s",
error);
os->stream_errno = EINVAL;
}
return os;
}