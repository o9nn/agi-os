#ifndef CRYPTO_H_
#define CRYPTO_H_
#include "keyring.h"
#include "packet.h"
#include "memory.h"
#include "packet-parse.h"
#include <openssl/dsa.h>
#define PGP_MIN_HASH_SIZE 16
struct pgp_hash_t {
pgp_hash_alg_t alg;
size_t size;
const char *name;
int (*init)(pgp_hash_t *);
void (*add)(pgp_hash_t *, const uint8_t *, unsigned);
unsigned (*finish)(pgp_hash_t *, uint8_t *);
void *data;
};
struct pgp_crypt_t {
pgp_symm_alg_t alg;
size_t blocksize;
size_t keysize;
void (*set_iv)(pgp_crypt_t *, const uint8_t *);
void (*set_crypt_key)(pgp_crypt_t *, const uint8_t *);
int (*base_init)(pgp_crypt_t *);
void (*decrypt_resync)(pgp_crypt_t *);
void (*block_encrypt)(pgp_crypt_t *, void *, const void *);
void (*block_decrypt)(pgp_crypt_t *, void *, const void *);
void (*cfb_encrypt)(pgp_crypt_t *, void *, const void *, size_t);
void (*cfb_decrypt)(pgp_crypt_t *, void *, const void *, size_t);
void (*decrypt_finish)(pgp_crypt_t *);
uint8_t iv[PGP_MAX_BLOCK_SIZE];
uint8_t civ[PGP_MAX_BLOCK_SIZE];
uint8_t siv[PGP_MAX_BLOCK_SIZE];
uint8_t key[PGP_MAX_KEY_SIZE];
int num;
void *encrypt_key;
void *decrypt_key;
};
typedef struct pgp_validation_t {
unsigned validc;
pgp_sig_info_t *valid_sigs;
unsigned invalidc;
pgp_sig_info_t *invalid_sigs;
unsigned unknownc;
pgp_sig_info_t *unknown_sigs;
time_t birthtime;
time_t duration;
} pgp_validation_t;
void pgp_crypto_finish(void);
void pgp_hash_md5(pgp_hash_t *);
void pgp_hash_sha1(pgp_hash_t *);
void pgp_hash_sha256(pgp_hash_t *);
void pgp_hash_sha512(pgp_hash_t *);
void pgp_hash_sha384(pgp_hash_t *);
void pgp_hash_sha224(pgp_hash_t *);
unsigned pgp_hash_any(pgp_hash_t *, pgp_hash_alg_t);
pgp_hash_alg_t pgp_str_to_hash_alg(const char *);
const char *pgp_text_from_hash(pgp_hash_t *);
unsigned pgp_hash_size(pgp_hash_alg_t);
unsigned pgp_hash(uint8_t *, pgp_hash_alg_t, const void *, size_t);
void pgp_hash_add_int(pgp_hash_t *, unsigned, unsigned);
unsigned pgp_dsa_verify(const uint8_t *, size_t,
const pgp_dsa_sig_t *,
const pgp_dsa_pubkey_t *);
int pgp_rsa_public_decrypt(uint8_t *, const uint8_t *, size_t,
const pgp_rsa_pubkey_t *);
int pgp_rsa_public_encrypt(uint8_t *, const uint8_t *, size_t,
const pgp_rsa_pubkey_t *);
int pgp_rsa_private_encrypt(uint8_t *, const uint8_t *, size_t,
const pgp_rsa_seckey_t *, const pgp_rsa_pubkey_t *);
int pgp_rsa_private_decrypt(uint8_t *, const uint8_t *, size_t,
const pgp_rsa_seckey_t *, const pgp_rsa_pubkey_t *);
int pgp_rsa_private_check(const pgp_seckey_t *seckey);
int pgp_dsa_private_check(const pgp_dsa_seckey_t *seckey);
int pgp_elgamal_private_check(const pgp_elgamal_seckey_t *seckey);
int pgp_elgamal_public_encrypt(uint8_t *, uint8_t *, const uint8_t *, size_t,
const pgp_elgamal_pubkey_t *);
int pgp_elgamal_private_decrypt(uint8_t *, const uint8_t *, const uint8_t *, size_t,
const pgp_elgamal_seckey_t *, const pgp_elgamal_pubkey_t *);
pgp_symm_alg_t pgp_str_to_cipher(const char *);
unsigned pgp_block_size(pgp_symm_alg_t);
unsigned pgp_key_size(pgp_symm_alg_t);
int pgp_decrypt_data(pgp_content_enum, pgp_region_t *,
pgp_stream_t *);
int pgp_crypt_any(pgp_crypt_t *, pgp_symm_alg_t);
void pgp_decrypt_init(pgp_crypt_t *);
void pgp_encrypt_init(pgp_crypt_t *);
size_t pgp_decrypt_se(pgp_crypt_t *, void *, const void *, size_t);
size_t pgp_encrypt_se(pgp_crypt_t *, void *, const void *, size_t);
size_t pgp_decrypt_se_ip(pgp_crypt_t *, void *, const void *, size_t);
size_t pgp_encrypt_se_ip(pgp_crypt_t *, void *, const void *, size_t);
unsigned pgp_is_sa_supported(pgp_symm_alg_t);
void pgp_reader_push_decrypt(pgp_stream_t *, pgp_crypt_t *,
pgp_region_t *);
void pgp_reader_pop_decrypt(pgp_stream_t *);
void pgp_reader_push_hash(pgp_stream_t *, pgp_hash_t *);
void pgp_reader_pop_hash(pgp_stream_t *);
int pgp_decrypt_decode_mpi(uint8_t *, unsigned, const BIGNUM *,
const BIGNUM *, const pgp_seckey_t *);
unsigned pgp_rsa_encrypt_mpi(const uint8_t *, const size_t,
const pgp_pubkey_t *,
pgp_pk_sesskey_params_t *);
unsigned pgp_elgamal_encrypt_mpi(const uint8_t *, const size_t,
const pgp_pubkey_t *,
pgp_pk_sesskey_params_t *);
struct pgp_key_data;
void pgp_writer_push_encrypt(pgp_output_t *,
const struct pgp_key_data *);
unsigned pgp_encrypt_file(pgp_io_t *, const char *, const char *,
const pgp_key_t *,
const unsigned, const unsigned, const char *);
unsigned pgp_decrypt_file(pgp_io_t *,
const char *,
const char *,
pgp_keyring_t *,
pgp_keyring_t *,
const unsigned,
const unsigned,
const unsigned,
void *,
int,
pgp_cbfunc_t *);
pgp_memory_t *
pgp_encrypt_buf(pgp_io_t *, const void *, const size_t,
const pgp_keyring_t *,
const unsigned, const char *, unsigned);
pgp_memory_t *
pgp_decrypt_buf(pgp_io_t *,
const void *,
const size_t,
pgp_keyring_t *,
pgp_keyring_t *,
const unsigned,
const unsigned,
const char* symm_passphrase);
pgp_memory_t *
pgp_decrypt_and_validate_buf(pgp_io_t *io,
pgp_validation_t *result,
const void *input,
const size_t insize,
pgp_keyring_t *secring,
pgp_keyring_t *pubring,
const unsigned use_armour,
key_id_t **recipients_key_ids,
unsigned *recipients_count);
#if 0
pgp_key_t *pgp_rsa_new_selfsign_key(const int,
const unsigned long, const uint8_t *, const char *,
const char *);
#endif
unsigned pgp_rsa_generate_keypair(pgp_key_t *,
const int,
const unsigned long,
const char *,
const char *,
const uint8_t *,
const size_t);
int pgp_dsa_size(const pgp_dsa_pubkey_t *);
pgp_dsa_sig_t *pgp_dsa_sign(uint8_t *, unsigned,
const pgp_dsa_seckey_t *,
const pgp_dsa_pubkey_t *);
struct pgp_reader_t {
pgp_reader_func_t *reader;
pgp_reader_destroyer_t *destroyer;
void *arg;
unsigned accumulate:1;
uint8_t *accumulated;
unsigned asize;
unsigned alength;
unsigned position;
pgp_reader_t *next;
pgp_stream_t *parent;
unsigned partial_read:1;
unsigned coalescing:1;
unsigned virtualc;
unsigned virtualoff;
uint8_t *virtualpkt;
};
struct pgp_cryptinfo_t {
const char *symm_passphrase;
pgp_keyring_t *secring;
pgp_key_t *keydata;
pgp_keyring_t *pubring;
DYNARRAY(key_id_t, recipients_key_ids);
};
struct pgp_cbdata_t {
pgp_cbfunc_t *cbfunc;
void *arg;
pgp_error_t **errors;
pgp_cbdata_t *next;
pgp_output_t *output;
pgp_io_t *io;
pgp_cryptinfo_t cryptinfo;
pgp_printstate_t printstate;
pgp_seckey_t *sshseckey;
int gotpass;
};
typedef struct {
pgp_hash_t hash;
uint8_t keyid[PGP_KEY_ID_SIZE];
} pgp_hashtype_t;
#define NTAGS 0x100
struct pgp_stream_t {
uint8_t ss_raw[NTAGS / 8];
uint8_t ss_parsed[NTAGS / 8];
pgp_reader_t readinfo;
pgp_cbdata_t cbinfo;
pgp_error_t *errors;
void *io;
pgp_crypt_t decrypt;
pgp_cryptinfo_t cryptinfo;
size_t hashc;
pgp_hashtype_t *hashes;
};
uint8_t* pgp_s2k_do(const char* passphrase,
int wanted_key_len,
pgp_s2k_specifier_t s2k_spec, pgp_hash_alg_t s2k_hash_algo, const uint8_t* s2k_salt, int s2k_iter_id);
#endif