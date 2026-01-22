#ifndef SIGNATURE_H_
#define SIGNATURE_H_
#include <sys/types.h>
#include <inttypes.h>
#include "packet.h"
#include "create.h"
#include "memory.h"
typedef struct pgp_create_sig_t pgp_create_sig_t;
pgp_create_sig_t *pgp_create_sig_new(void);
void pgp_create_sig_delete(pgp_create_sig_t *);
unsigned pgp_check_useridcert_sig(const pgp_pubkey_t *,
const uint8_t *,
const pgp_sig_t *,
const pgp_pubkey_t *);
unsigned pgp_check_userattrcert_sig(const pgp_pubkey_t *,
const pgp_data_t *,
const pgp_sig_t *,
const pgp_pubkey_t *);
unsigned pgp_check_subkey_sig(const pgp_pubkey_t *,
const pgp_pubkey_t *,
const pgp_sig_t *,
const pgp_pubkey_t *);
unsigned pgp_check_direct_sig(const pgp_pubkey_t *,
const pgp_sig_t *,
const pgp_pubkey_t *);
unsigned pgp_check_hash_sig(pgp_hash_t *,
const pgp_sig_t *,
const pgp_pubkey_t *);
void pgp_sig_start_key_sig(pgp_create_sig_t *,
const pgp_pubkey_t *,
const pgp_pubkey_t * subkey,
const uint8_t *,
pgp_sig_type_t);
void pgp_start_sig(pgp_create_sig_t *,
const pgp_seckey_t *,
const pgp_hash_alg_t,
const pgp_sig_type_t);
void pgp_sig_add_data(pgp_create_sig_t *, const void *, size_t);
pgp_hash_t *pgp_sig_get_hash(pgp_create_sig_t *);
unsigned pgp_end_hashed_subpkts(pgp_create_sig_t *);
unsigned pgp_write_sig(pgp_output_t *, pgp_create_sig_t *,
const pgp_pubkey_t *, const pgp_seckey_t *);
unsigned pgp_add_issuer_keyid(pgp_create_sig_t *,
const uint8_t *);
void pgp_add_primary_userid(pgp_create_sig_t *, unsigned);
unsigned
pgp_add_creation_time(pgp_create_sig_t *sig, time_t when);
unsigned
pgp_add_sig_expiration_time(pgp_create_sig_t *sig, time_t duration);
unsigned
pgp_add_key_expiration_time(pgp_create_sig_t *sig, time_t duration);
unsigned
pgp_add_key_flags(pgp_create_sig_t *sig, uint8_t flags);
unsigned
pgp_add_key_prefs(pgp_create_sig_t *sig);
unsigned
pgp_add_key_features(pgp_create_sig_t *sig);
unsigned pgp_sign_file(pgp_io_t *,
const char *,
const char *,
const pgp_seckey_t *,
const char *,
const time_t,
const time_t,
const unsigned,
const unsigned,
const unsigned);
int pgp_sign_detached(pgp_io_t *,
const char *,
char *,
const pgp_seckey_t *,
const char *,
const time_t,
const time_t,
const unsigned,
const unsigned);
unsigned pgp_crc24(unsigned, uint8_t);
void pgp_reader_push_dearmour(pgp_stream_t *);
void pgp_reader_pop_dearmour(pgp_stream_t *);
unsigned pgp_writer_push_clearsigned(pgp_output_t *, pgp_create_sig_t *);
void pgp_writer_push_armor_msg(pgp_output_t *);
typedef enum {
PGP_PGP_MESSAGE = 1,
PGP_PGP_PUBLIC_KEY_BLOCK,
PGP_PGP_PRIVATE_KEY_BLOCK,
PGP_PGP_MULTIPART_MESSAGE_PART_X_OF_Y,
PGP_PGP_MULTIPART_MESSAGE_PART_X,
PGP_PGP_SIGNATURE
} pgp_armor_type_t;
#define CRC24_INIT 0xb704ceL
unsigned pgp_writer_use_armored_sig(pgp_output_t *);
void pgp_writer_push_armoured(pgp_output_t *, pgp_armor_type_t);
pgp_memory_t *pgp_sign_buf(pgp_io_t *,
const void *,
const size_t,
const pgp_seckey_t *,
const time_t,
const time_t,
const char *,
const unsigned,
const unsigned);
struct pgp_create_sig_t {
pgp_hash_t hash;
pgp_sig_t sig;
pgp_memory_t *mem;
pgp_output_t *output;
unsigned hashoff;
unsigned hashlen;
unsigned unhashoff;
};
void
pgp_sig_start_key_rev(pgp_create_sig_t *sig,
const pgp_pubkey_t *key,
pgp_sig_type_t type);
unsigned
pgp_add_revocation_reason(
pgp_create_sig_t *sig,
uint8_t code, const char *reason);
#endif