#ifndef VALIDATE_H_
#define VALIDATE_H_	1
typedef struct validate_key_cb_t{
pgp_content_enum	type;
pgp_keydata_key_t	key;
pgp_keydata_key_t	subkey;
uint8_t			     pubkeyid[PGP_KEY_ID_SIZE];
enum {
LS_UNKNOWN = 0,
LS_ATTRIBUTE,
LS_ID,
LS_SUBKEY,
LS_PRIMARY,
}               	 last_seen;
uint8_t			*userid;
pgp_data_t		 userattr;
uint8_t			 hash[PGP_MAX_HASH_SIZE];
const pgp_keyring_t	*keyring;
pgp_validation_t	*result;
pgp_cb_ret_t(*getpassphrase) (const pgp_packet_t *,
pgp_cbdata_t *);
unsigned not_commited;
pgp_sig_info_t valid_sig_info;
unsigned sig_is_valid;
pgp_cb_ret_t(*on_valid) (
struct validate_key_cb_t *,
const pgp_subpacket_t *);
void *on_valid_args;
} validate_key_cb_t;
typedef struct {
enum {
LITDATA,
SIGNED_CLEARTEXT
} type;
union {
pgp_litdata_body_t	 litdata_body;
pgp_fixed_body_t	 cleartext_body;
} data;
uint8_t			 	 hash[PGP_MAX_HASH_SIZE];
pgp_memory_t			*mem;
const pgp_keyring_t		*keyring;
pgp_validation_t		*result;
char				*detachname;
} validate_data_cb_t;
#if 0
pgp_cb_ret_t pgp_validate_key_cb(const pgp_packet_t *, pgp_cbdata_t *);
#endif
#if 0
unsigned check_binary_sig(const uint8_t *,
const unsigned,
const pgp_sig_t *,
const pgp_pubkey_t *);
#endif
unsigned   pgp_validate_file(pgp_io_t *,
pgp_validation_t *,
const char *,
const char *,
const int,
const pgp_keyring_t *);
unsigned   pgp_validate_mem(pgp_io_t *,
pgp_validation_t *,
pgp_memory_t *,
pgp_memory_t **,
const int,
const pgp_keyring_t *);
unsigned   pgp_validate_mem_detached(pgp_io_t *,
pgp_validation_t *,
pgp_memory_t *,
pgp_memory_t **,
const int,
const pgp_keyring_t *,
pgp_memory_t *);
pgp_cb_ret_t validate_data_cb(const pgp_packet_t *, pgp_cbdata_t *);
void pgp_free_sig_info(pgp_sig_info_t *);
#if 0
unsigned
pgp_filter_keys_fileread(pgp_io_t *io,
pgp_keyring_t *destpubring,
pgp_keyring_t *destsecring,
pgp_keyring_t *certring,
const unsigned armour,
const char *filename);
#endif
unsigned
pgp_filter_keys_from_mem(pgp_io_t *io,
pgp_keyring_t *destpubring,
pgp_keyring_t *destsecring,
pgp_keyring_t *certring,
const unsigned armour,
const pgp_memory_t *mem);
#endif