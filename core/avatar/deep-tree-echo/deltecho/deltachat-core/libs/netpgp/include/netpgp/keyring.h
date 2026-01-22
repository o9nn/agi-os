#ifndef KEYRING_H_
#define KEYRING_H_
#include "packet.h"
#include "packet-parse.h"
#include "memory.h"
enum {
MAX_ID_LENGTH		= 128,
MAX_PASSPHRASE_LENGTH	= 256
};
typedef struct pgp_key_t	pgp_key_t;
typedef struct pgp_keyring_t {
DYNARRAY(pgp_key_t,	key);
pgp_hash_alg_t	hashtype;
} pgp_keyring_t;
pgp_key_t *pgp_getkeybyid(pgp_io_t *,
const pgp_keyring_t *,
const uint8_t *,
unsigned *,
pgp_pubkey_t **,
pgp_seckey_t **,
unsigned checkrevoke,
unsigned checkexpiry);
unsigned pgp_deletekeybyid(pgp_io_t *,
pgp_keyring_t *,
const uint8_t *);
pgp_key_t *pgp_getkeybyfpr(pgp_io_t *,
const pgp_keyring_t *,
const uint8_t *fpr,
size_t length,
unsigned *from,
pgp_pubkey_t **,
unsigned checkrevoke,
unsigned checkexpiry);
unsigned pgp_deletekeybyfpr(pgp_io_t *,
pgp_keyring_t *,
const uint8_t *fpr,
size_t length);
const pgp_key_t *pgp_getkeybyname(pgp_io_t *,
const pgp_keyring_t *,
const char *);
const pgp_key_t *pgp_getnextkeybyname(pgp_io_t *,
const pgp_keyring_t *,
const char *,
unsigned *);
void pgp_key_free(pgp_key_t *);
void pgp_keydata_free(pgp_key_t *);
void pgp_keyring_free(pgp_keyring_t *);
void pgp_keyring_purge(pgp_keyring_t *);
void pgp_dump_keyring(const pgp_keyring_t *);
pgp_pubkey_t *pgp_key_get_pubkey(pgp_key_t *);
unsigned   pgp_is_key_secret(pgp_key_t *);
pgp_seckey_t *pgp_get_seckey(pgp_key_t *);
pgp_seckey_t *pgp_get_writable_seckey(pgp_key_t *);
unsigned
pgp_keyring_fileread(pgp_io_t *io,
pgp_keyring_t *pubring,
pgp_keyring_t *secring,
const unsigned armour,
const char *filename);
#if 0
unsigned
pgp_keyring_read_from_mem(pgp_io_t *io,
pgp_keyring_t *pubring,
pgp_keyring_t *secring,
const unsigned armour,
pgp_memory_t *mem);
#endif
int pgp_keyring_list(pgp_io_t *, const pgp_keyring_t *, const int);
void pgp_forget(void *, unsigned);
unsigned pgp_update_userid(
pgp_key_t *key,
const uint8_t *userid,
const pgp_subpacket_t *sigpkt,
const pgp_sig_info_t *siginfo);
unsigned pgp_add_selfsigned_userid(pgp_key_t *skey, pgp_key_t *pkey, const uint8_t *userid, time_t duration);
pgp_key_t  *pgp_keydata_new(void);
void pgp_keydata_init(pgp_key_t *, const pgp_content_enum);
char *pgp_export_key(pgp_io_t *, const pgp_key_t *, uint8_t *);
int pgp_keyring_add(pgp_keyring_t *, const pgp_key_t *);
pgp_key_t *pgp_ensure_pubkey(
pgp_keyring_t *,
pgp_pubkey_t *,
uint8_t *);
pgp_key_t *pgp_ensure_seckey(
pgp_keyring_t *keyring,
pgp_seckey_t *seckey,
uint8_t *pubkeyid);
unsigned pgp_add_directsig(
pgp_key_t *key,
const pgp_subpacket_t *sigpkt,
pgp_sig_info_t *siginfo);
unsigned pgp_update_subkey(
pgp_key_t *key,
pgp_content_enum subkeytype,
pgp_keydata_key_t *subkey,
const pgp_subpacket_t *sigpkt,
pgp_sig_info_t *siginfo);
int pgp_append_keyring(pgp_keyring_t *, pgp_keyring_t *);
pgp_subpacket_t * pgp_copy_packet(pgp_subpacket_t *, const pgp_subpacket_t *);
uint8_t * pgp_copy_userid(uint8_t **dst, const uint8_t *src);
const int32_t pgp_key_get_uid0(pgp_key_t *keydata);
const uint8_t *pgp_key_get_primary_userid(pgp_key_t *key);
pgp_pubkey_t * pgp_key_get_sigkey(pgp_key_t *key);
#if 0
pgp_seckey_t * pgp_key_get_certkey(pgp_key_t *key);
#endif
pgp_pubkey_t * pgp_key_get_enckey(pgp_key_t *key, const uint8_t **id);
pgp_seckey_t * pgp_key_get_deckey(pgp_key_t *key, const uint8_t **id);
const int32_t
pgp_key_find_uid_cond(
const pgp_key_t *key,
unsigned(*uidcond) ( uint8_t *, void *),
void *uidcondarg,
unsigned(*sigcond) ( const pgp_sig_info_t *, void *),
void *sigcondarg,
time_t *youngest,
unsigned checkrevoke,
unsigned checkexpiry);
const pgp_key_rating_t pgp_key_get_rating(pgp_key_t *key);
#if 0
unsigned
pgp_key_revoke(pgp_key_t *skey, pgp_key_t *pkey, uint8_t code, const char *reason);
#endif
#endif