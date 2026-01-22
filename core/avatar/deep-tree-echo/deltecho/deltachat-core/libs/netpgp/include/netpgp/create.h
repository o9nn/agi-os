#ifndef CREATE_H_
#define CREATE_H_
#include "types.h"
#include "packet.h"
#include "crypto.h"
#include "errors.h"
#include "keyring.h"
#include "writer.h"
#include "memory.h"
struct pgp_output_t {
pgp_writer_t writer;
pgp_error_t *errors;
};
pgp_output_t *pgp_output_new(void);
void pgp_output_delete(pgp_output_t *);
int pgp_filewrite(const char *, const char *, const size_t, const unsigned);
void pgp_build_pubkey(pgp_memory_t *, const pgp_pubkey_t *, unsigned);
unsigned pgp_calc_sesskey_checksum(pgp_pk_sesskey_t *, uint8_t *);
unsigned pgp_write_struct_userid(pgp_output_t *, const uint8_t *);
unsigned pgp_write_ss_header(pgp_output_t *, size_t, pgp_content_enum);
unsigned pgp_write_struct_seckey_ptag(const pgp_seckey_t *key,
const uint8_t *passphrase,
const size_t pplen,
pgp_output_t *output,
pgp_content_enum ptag);
unsigned pgp_write_struct_seckey(const pgp_seckey_t *,
const uint8_t *,
const size_t,
pgp_output_t *);
unsigned pgp_write_struct_pubkey(pgp_output_t *, const pgp_pubkey_t *);
unsigned pgp_write_one_pass_sig(pgp_output_t *,
const pgp_seckey_t *,
const pgp_hash_alg_t,
const pgp_sig_type_t);
unsigned pgp_write_litdata(pgp_output_t *,
const uint8_t *,
const int,
const pgp_litdata_enum);
pgp_pk_sesskey_t *pgp_create_pk_sesskey(pgp_key_t *, const char *, const pgp_pk_sesskey_t *);
unsigned pgp_write_pk_sesskey(pgp_output_t *, pgp_pk_sesskey_t *);
unsigned pgp_write_xfer_key(pgp_output_t *output,
const pgp_key_t *key,
const unsigned armoured);
void pgp_fast_create_userid(uint8_t **, uint8_t *);
unsigned pgp_write_userid(const uint8_t *, pgp_output_t *);
void pgp_fast_create_rsa_pubkey(pgp_pubkey_t *, time_t, BIGNUM *, BIGNUM *);
unsigned pgp_write_rsa_pubkey(time_t, const BIGNUM *, const BIGNUM *,
pgp_output_t *);
void pgp_fast_create_rsa_seckey(pgp_seckey_t *, time_t, BIGNUM *,
BIGNUM *, BIGNUM *, BIGNUM *,
BIGNUM *, BIGNUM *);
unsigned encode_m_buf(const uint8_t *, size_t, const pgp_pubkey_t *,
uint8_t *);
#if 0
unsigned pgp_fileread_litdata(const char *, const pgp_litdata_enum,
pgp_output_t *);
unsigned pgp_write_symm_enc_data(const uint8_t *, const int, pgp_symm_alg_t, const uint8_t* key,
pgp_output_t *);
#endif
#endif