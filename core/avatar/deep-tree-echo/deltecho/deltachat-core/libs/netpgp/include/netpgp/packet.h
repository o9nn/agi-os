#ifndef PACKET_H_
#define PACKET_H_
#include <time.h>
#ifdef HAVE_OPENSSL_BN_H
#include <openssl/bn.h>
#endif
#include <openssl/ossl_typ.h>
#include "types.h"
#include "errors.h"
typedef struct pgp_printstate_t {
unsigned	unarmoured;
unsigned	skipping;
int		indent;
} pgp_printstate_t;
typedef struct {
size_t           len;
uint8_t		*contents;
uint8_t		 mmapped;
} pgp_data_t;
#define PGP_PTAG_ALWAYS_SET		0x80
#define PGP_PTAG_NEW_FORMAT		0x40
#define PGP_PTAG_OF_CONTENT_TAG_MASK	0x3c
#define PGP_PTAG_OF_CONTENT_TAG_SHIFT	2
#define PGP_PTAG_OF_LENGTH_TYPE_MASK	0x03
typedef enum {
PGP_PTAG_OLD_LEN_1 = 0x00,
PGP_PTAG_OLD_LEN_2 = 0x01,
PGP_PTAG_OLD_LEN_4 = 0x02,
PGP_PTAG_OLD_LEN_INDETERMINATE = 0x03
} pgp_ptag_of_lt_t;
#define PGP_PTAG_NF_CONTENT_TAG_MASK	0x3f
#define PGP_PTAG_NF_CONTENT_TAG_SHIFT	0
typedef enum {
PGP_PTAG_CT_RESERVED = 0,
PGP_PTAG_CT_PK_SESSION_KEY = 1,
PGP_PTAG_CT_SIGNATURE = 2,
PGP_PTAG_CT_SK_SESSION_KEY = 3,
PGP_PTAG_CT_1_PASS_SIG = 4,
PGP_PTAG_CT_SECRET_KEY = 5,
PGP_PTAG_CT_PUBLIC_KEY = 6,
PGP_PTAG_CT_SECRET_SUBKEY = 7,
PGP_PTAG_CT_COMPRESSED = 8,
PGP_PTAG_CT_SE_DATA = 9,
PGP_PTAG_CT_MARKER = 10,
PGP_PTAG_CT_LITDATA = 11,
PGP_PTAG_CT_TRUST = 12,
PGP_PTAG_CT_USER_ID = 13,
PGP_PTAG_CT_PUBLIC_SUBKEY = 14,
PGP_PTAG_CT_RESERVED2 = 15,
PGP_PTAG_CT_RESERVED3 = 16,
PGP_PTAG_CT_USER_ATTR = 17,
PGP_PTAG_CT_SE_IP_DATA = 18,
PGP_PTAG_CT_MDC = 19,
PGP_PARSER_PTAG = 0x100,
PGP_PTAG_RAW_SS = 0x101,
PGP_PTAG_SS_ALL = 0x102,
PGP_PARSER_PACKET_END = 0x103,
PGP_PTAG_SIG_SUBPKT_BASE = 0x200,
PGP_PTAG_SS_CREATION_TIME = 0x200 + 2,
PGP_PTAG_SS_EXPIRATION_TIME = 0x200 + 3,
PGP_PTAG_SS_EXPORT_CERT = 0x200 + 4,
PGP_PTAG_SS_TRUST = 0x200 + 5,
PGP_PTAG_SS_REGEXP = 0x200 + 6,
PGP_PTAG_SS_REVOCABLE = 0x200 + 7,
PGP_PTAG_SS_KEY_EXPIRY = 0x200 + 9,
PGP_PTAG_SS_RESERVED = 0x200 + 10,
PGP_PTAG_SS_PREFERRED_SKA = 0x200 + 11,
PGP_PTAG_SS_REVOCATION_KEY = 0x200 + 12,
PGP_PTAG_SS_ISSUER_KEY_ID = 0x200 + 16,
PGP_PTAG_SS_NOTATION_DATA = 0x200 + 20,
PGP_PTAG_SS_PREFERRED_HASH = 0x200 + 21,
PGP_PTAG_SS_PREF_COMPRESS = 0x200 + 22,
PGP_PTAG_SS_KEYSERV_PREFS = 0x200 + 23,
PGP_PTAG_SS_PREF_KEYSERV = 0x200 + 24,
PGP_PTAG_SS_PRIMARY_USER_ID = 0x200 + 25,
PGP_PTAG_SS_POLICY_URI = 0x200 + 26,
PGP_PTAG_SS_KEY_FLAGS = 0x200 + 27,
PGP_PTAG_SS_SIGNERS_USER_ID = 0x200 + 28,
PGP_PTAG_SS_REVOCATION_REASON = 0x200 + 29,
PGP_PTAG_SS_FEATURES = 0x200 + 30,
PGP_PTAG_SS_SIGNATURE_TARGET = 0x200 + 31,
PGP_PTAG_SS_EMBEDDED_SIGNATURE = 0x200 + 32,
PGP_PTAG_SS_USERDEFINED00 = 0x200 + 100,
PGP_PTAG_SS_USERDEFINED01 = 0x200 + 101,
PGP_PTAG_SS_USERDEFINED02 = 0x200 + 102,
PGP_PTAG_SS_USERDEFINED03 = 0x200 + 103,
PGP_PTAG_SS_USERDEFINED04 = 0x200 + 104,
PGP_PTAG_SS_USERDEFINED05 = 0x200 + 105,
PGP_PTAG_SS_USERDEFINED06 = 0x200 + 106,
PGP_PTAG_SS_USERDEFINED07 = 0x200 + 107,
PGP_PTAG_SS_USERDEFINED08 = 0x200 + 108,
PGP_PTAG_SS_USERDEFINED09 = 0x200 + 109,
PGP_PTAG_SS_USERDEFINED10 = 0x200 + 110,
PGP_PTAG_CT_LITDATA_HEADER = 0x300,
PGP_PTAG_CT_LITDATA_BODY = 0x300 + 1,
PGP_PTAG_CT_SIGNATURE_HEADER = 0x300 + 2,
PGP_PTAG_CT_SIGNATURE_FOOTER = 0x300 + 3,
PGP_PTAG_CT_ARMOUR_HEADER = 0x300 + 4,
PGP_PTAG_CT_ARMOUR_TRAILER = 0x300 + 5,
PGP_PTAG_CT_SIGNED_CLEARTEXT_HEADER = 0x300 + 6,
PGP_PTAG_CT_SIGNED_CLEARTEXT_BODY = 0x300 + 7,
PGP_PTAG_CT_SIGNED_CLEARTEXT_TRAILER = 0x300 + 8,
PGP_PTAG_CT_UNARMOURED_TEXT = 0x300 + 9,
PGP_PTAG_CT_ENCRYPTED_SECRET_KEY = 0x300 + 10,
PGP_PTAG_CT_SE_DATA_HEADER = 0x300 + 11,
PGP_PTAG_CT_SE_DATA_BODY = 0x300 + 12,
PGP_PTAG_CT_SE_IP_DATA_HEADER = 0x300 + 13,
PGP_PTAG_CT_SE_IP_DATA_BODY = 0x300 + 14,
PGP_PTAG_CT_ENCRYPTED_PK_SESSION_KEY = 0x300 + 15,
PGP_GET_PASSPHRASE = 0x400,
PGP_GET_SECKEY = 0x400 + 1,
PGP_PARSER_ERROR = 0x500,
PGP_PARSER_ERRCODE = 0x500 + 1
} pgp_content_enum;
enum {
PGP_REVOCATION_NO_REASON	= 0,
PGP_REVOCATION_SUPERSEDED	= 1,
PGP_REVOCATION_COMPROMISED	= 2,
PGP_REVOCATION_RETIRED		= 3,
PGP_REVOCATION_NO_LONGER_VALID	= 0x20
};
typedef struct {
pgp_errcode_t   errcode;
} pgp_parser_errcode_t;
typedef struct {
unsigned        new_format;
unsigned        type;
pgp_ptag_of_lt_t length_type;
unsigned        length;
unsigned        position;
unsigned	size;
} pgp_ptag_t;
typedef enum {
PGP_PKA_NOTHING	= 0,
PGP_PKA_RSA = 1,
PGP_PKA_RSA_ENCRYPT_ONLY = 2,
PGP_PKA_RSA_SIGN_ONLY = 3,
PGP_PKA_ELGAMAL = 16,
PGP_PKA_DSA = 17,
PGP_PKA_RESERVED_ELLIPTIC_CURVE = 18,
PGP_PKA_RESERVED_ECDSA = 19,
PGP_PKA_ELGAMAL_ENCRYPT_OR_SIGN = 20,
PGP_PKA_RESERVED_DH = 21,
PGP_PKA_PRIVATE00 = 100,
PGP_PKA_PRIVATE01 = 101,
PGP_PKA_PRIVATE02 = 102,
PGP_PKA_PRIVATE03 = 103,
PGP_PKA_PRIVATE04 = 104,
PGP_PKA_PRIVATE05 = 105,
PGP_PKA_PRIVATE06 = 106,
PGP_PKA_PRIVATE07 = 107,
PGP_PKA_PRIVATE08 = 108,
PGP_PKA_PRIVATE09 = 109,
PGP_PKA_PRIVATE10 = 110
} pgp_pubkey_alg_t;
typedef struct {
BIGNUM         *p;
BIGNUM         *q;
BIGNUM         *g;
BIGNUM         *y;
} pgp_dsa_pubkey_t;
typedef struct {
BIGNUM         *n;
BIGNUM         *e;
} pgp_rsa_pubkey_t;
typedef struct {
BIGNUM         *p;
BIGNUM         *g;
BIGNUM         *y;
} pgp_elgamal_pubkey_t;
typedef enum {
PGP_V2 = 2,
PGP_V3 = 3,
PGP_V4 = 4
} pgp_version_t;
typedef struct {
pgp_version_t		version;
time_t			birthtime;
time_t			duration;
unsigned		days_valid;
pgp_pubkey_alg_t	alg;
union {
pgp_dsa_pubkey_t dsa;
pgp_rsa_pubkey_t rsa;
pgp_elgamal_pubkey_t elgamal;
}			key;
} pgp_pubkey_t;
typedef struct {
BIGNUM         *d;
BIGNUM         *p;
BIGNUM         *q;
BIGNUM         *u;
} pgp_rsa_seckey_t;
typedef struct {
BIGNUM         *x;
} pgp_dsa_seckey_t;
typedef struct {
BIGNUM         *x;
} pgp_elgamal_seckey_t;
typedef enum {
PGP_S2KU_NONE = 0,
PGP_S2KU_ENCRYPTED_AND_HASHED = 254,
PGP_S2KU_ENCRYPTED = 255
} pgp_s2k_usage_t;
typedef enum {
PGP_S2KS_SIMPLE = 0,
PGP_S2KS_SALTED = 1,
PGP_S2KS_ITERATED_AND_SALTED = 3
} pgp_s2k_specifier_t;
typedef enum {
PGP_SA_PLAINTEXT = 0,
PGP_SA_IDEA = 1,
PGP_SA_TRIPLEDES = 2,
PGP_SA_CAST5 = 3,
PGP_SA_BLOWFISH = 4,
PGP_SA_AES_128 = 7,
PGP_SA_AES_192 = 8,
PGP_SA_AES_256 = 9,
PGP_SA_TWOFISH = 10,
PGP_SA_CAMELLIA_128 = 100,
PGP_SA_CAMELLIA_192 = 101,
PGP_SA_CAMELLIA_256 = 102
} pgp_symm_alg_t;
#define PGP_SA_DEFAULT_CIPHER	PGP_SA_CAST5
typedef enum {
PGP_HASH_UNKNOWN = -1,
PGP_HASH_MD5 = 1,
PGP_HASH_SHA1 = 2,
PGP_HASH_RIPEMD = 3,
PGP_HASH_SHA256 = 8,
PGP_HASH_SHA384 = 9,
PGP_HASH_SHA512 = 10,
PGP_HASH_SHA224 = 11
} pgp_hash_alg_t;
#define	PGP_DEFAULT_HASH_ALGORITHM	PGP_HASH_SHA256
void   pgp_calc_mdc_hash(const uint8_t *,
const size_t,
const uint8_t *,
const unsigned,
uint8_t *);
unsigned   pgp_is_hash_alg_supported(const pgp_hash_alg_t *);
#define PGP_MAX_BLOCK_SIZE	16
#define PGP_MAX_KEY_SIZE	32
#define PGP_SALT_SIZE		8
#define PGP_MAX_HASH_SIZE	64
typedef struct pgp_seckey_t {
pgp_pubkey_t			pubkey;
pgp_s2k_usage_t		s2k_usage;
pgp_s2k_specifier_t		s2k_specifier;
pgp_symm_alg_t		alg;
pgp_hash_alg_t		hash_alg;
uint8_t				salt[PGP_SALT_SIZE];
unsigned			octetc;
uint8_t				iv[PGP_MAX_BLOCK_SIZE];
union {
pgp_rsa_seckey_t		rsa;
pgp_dsa_seckey_t		dsa;
pgp_elgamal_seckey_t		elgamal;
}				key;
unsigned			checksum;
uint8_t			       *checkhash;
} pgp_seckey_t;
typedef enum {
PGP_SIG_BINARY = 0x00,
PGP_SIG_TEXT = 0x01,
PGP_SIG_STANDALONE = 0x02,
PGP_CERT_GENERIC = 0x10,
PGP_CERT_PERSONA = 0x11,
PGP_CERT_CASUAL = 0x12,
PGP_CERT_POSITIVE = 0x13,
PGP_SIG_SUBKEY = 0x18,
PGP_SIG_PRIMARY = 0x19,
PGP_SIG_DIRECT = 0x1f,
PGP_SIG_REV_KEY = 0x20,
PGP_SIG_REV_SUBKEY = 0x28,
PGP_SIG_REV_CERT = 0x30,
PGP_SIG_TIMESTAMP = 0x40,
PGP_SIG_3RD_PARTY = 0x50
} pgp_sig_type_t;
typedef struct pgp_rsa_sig_t {
BIGNUM         *sig;
} pgp_rsa_sig_t;
typedef struct pgp_dsa_sig_t {
BIGNUM         *r;
BIGNUM         *s;
} pgp_dsa_sig_t;
typedef struct pgp_elgamal_sig_t {
BIGNUM         *r;
BIGNUM         *s;
} pgp_elgamal_sig_t;
#define PGP_KEY_ID_SIZE		8
#define PGP_FINGERPRINT_SIZE	20
typedef struct pgp_sig_info_t {
pgp_version_t   version;
pgp_sig_type_t  type;
time_t          birthtime;
time_t          duration;
time_t          key_expiry;
uint8_t         key_flags;
uint8_t		signer_id[PGP_KEY_ID_SIZE];
pgp_pubkey_alg_t key_alg;
pgp_hash_alg_t hash_alg;
union {
pgp_rsa_sig_t	rsa;
pgp_dsa_sig_t	dsa;
pgp_elgamal_sig_t	elgamal;
pgp_data_t	unknown;
}			sig;
size_t          v4_hashlen;
uint8_t		*v4_hashed;
unsigned	 birthtime_set:1;
unsigned	 signer_id_set:1;
unsigned	 duration_set:1;
unsigned	 key_expiry_set:1;
unsigned	 key_flags_set:1;
unsigned	 primary_userid:1;
} pgp_sig_info_t;
typedef enum {
PGP_KEYFLAG_CERT_KEYS = 0x01,
PGP_KEYFLAG_SIGN_DATA = 0x02,
PGP_KEYFLAG_ENC_COMM = 0x04,
PGP_KEYFLAG_ENC_STORAGE = 0x08,
PGP_KEYFLAG_SPLIT = 0x10,
PGP_KEYFLAG_AUTH = 0x20,
PGP_KEYFLAG_GROUP = 0x80
} pgp_key_flags_t;
typedef struct pgp_sig_t {
pgp_sig_info_t info;
uint8_t		 hash2[2];
size_t		 v4_hashstart;
#if 0
pgp_hash_t     *hash;
#endif
} pgp_sig_t;
typedef struct pgp_ss_raw_t {
pgp_content_enum	 tag;
size_t          	 length;
uint8_t			*raw;
} pgp_ss_raw_t;
typedef struct pgp_ss_trust_t {
uint8_t			 level;
uint8_t			 amount;
} pgp_ss_trust_t;
typedef struct pgp_ss_notation_t {
pgp_data_t		flags;
pgp_data_t		name;
pgp_data_t		value;
} pgp_ss_notation_t;
typedef struct pgp_ss_sig_target_t {
pgp_pubkey_alg_t	pka_alg;
pgp_hash_alg_t		hash_alg;
pgp_data_t		hash;
} pgp_ss_sig_target_t;
typedef struct pgp_subpacket_t {
size_t          	 length;
uint8_t			*raw;
} pgp_subpacket_t;
typedef enum {
PGP_C_NONE = 0,
PGP_C_ZIP = 1,
PGP_C_ZLIB = 2,
PGP_C_BZIP2 = 3
} pgp_compression_type_t;
typedef struct {
uint8_t			version;
pgp_sig_type_t		sig_type;
pgp_hash_alg_t		hash_alg;
pgp_pubkey_alg_t	key_alg;
uint8_t			keyid[PGP_KEY_ID_SIZE];
unsigned		nested;
} pgp_one_pass_sig_t;
typedef struct {
uint8_t   		class;
uint8_t   		algid;
uint8_t   		fingerprint[PGP_FINGERPRINT_SIZE];
} pgp_ss_revocation_key_t;
typedef struct {
uint8_t   		 code;
char			*reason;
} pgp_ss_revocation_t;
typedef enum {
PGP_LDT_BINARY = 'b',
PGP_LDT_TEXT = 't',
PGP_LDT_UTF8 = 'u',
PGP_LDT_LOCAL = 'l',
PGP_LDT_LOCAL2 = '1'
} pgp_litdata_enum;
typedef struct {
pgp_litdata_enum	format;
char			filename[256];
time_t			mtime;
} pgp_litdata_header_t;
typedef struct {
unsigned         length;
uint8_t		*data;
void		*mem;
} pgp_litdata_body_t;
typedef struct {
char           *key;
char           *value;
} pgp_header_var_t;
typedef struct {
pgp_header_var_t	*headers;
unsigned	         headerc;
} pgp_headers_t;
typedef struct {
const char	*type;
pgp_headers_t	 headers;
} pgp_armour_header_t;
typedef struct pgp_fixed_body_t {
unsigned        length;
uint8_t		data[8192];
} pgp_fixed_body_t;
typedef struct pgp_dyn_body_t {
unsigned         length;
uint8_t		*data;
} pgp_dyn_body_t;
enum {
PGP_SE_IP_DATA_VERSION = 1,
PGP_PKSK_V3 = 3
};
typedef struct {
BIGNUM         *encrypted_m;
BIGNUM         *m;
} pgp_pk_sesskey_params_rsa_t;
typedef struct {
BIGNUM         *g_to_k;
BIGNUM         *encrypted_m;
} pgp_pk_sesskey_params_elgamal_t;
typedef union {
pgp_pk_sesskey_params_rsa_t rsa;
pgp_pk_sesskey_params_elgamal_t elgamal;
} pgp_pk_sesskey_params_t;
typedef uint8_t key_id_t[PGP_KEY_ID_SIZE];
typedef struct {
unsigned			version;
key_id_t				key_id;
pgp_pubkey_alg_t		alg;
pgp_pk_sesskey_params_t	params;
pgp_symm_alg_t		symm_alg;
uint8_t				key[PGP_MAX_KEY_SIZE];
uint16_t			checksum;
} pgp_pk_sesskey_t;
typedef struct {
const pgp_seckey_t *seckey;
char          **passphrase;
} pgp_seckey_passphrase_t;
typedef struct {
const pgp_seckey_t **seckey;
const pgp_pk_sesskey_t *pk_sesskey;
} pgp_get_seckey_t;
typedef union {
const char 			*error;
pgp_parser_errcode_t		errcode;
pgp_ptag_t			ptag;
pgp_pubkey_t			pubkey;
pgp_data_t			trust;
uint8_t				*userid;
pgp_data_t			userattr;
pgp_sig_t			sig;
pgp_ss_raw_t			ss_raw;
pgp_ss_trust_t		ss_trust;
unsigned			ss_revocable;
time_t				ss_time;
uint8_t				ss_issuer[PGP_KEY_ID_SIZE];
pgp_ss_notation_t		ss_notation;
pgp_subpacket_t		packet;
pgp_compression_type_t	compressed;
pgp_one_pass_sig_t		one_pass_sig;
pgp_data_t			ss_skapref;
pgp_data_t			ss_hashpref;
pgp_data_t			ss_zpref;
pgp_data_t			ss_key_flags;
pgp_data_t			ss_key_server_prefs;
unsigned			ss_primary_userid;
char				*ss_regexp;
char				*ss_policy;
char				*ss_keyserv;
pgp_ss_revocation_key_t	ss_revocation_key;
pgp_data_t			ss_userdef;
pgp_data_t			ss_unknown;
pgp_litdata_header_t		litdata_header;
pgp_litdata_body_t		litdata_body;
pgp_dyn_body_t		mdc;
pgp_data_t			ss_features;
pgp_ss_sig_target_t		ss_sig_target;
pgp_data_t			ss_embedded_sig;
pgp_ss_revocation_t		ss_revocation;
pgp_seckey_t			seckey;
uint8_t				*ss_signer;
pgp_armour_header_t		armour_header;
const char 			*armour_trailer;
pgp_headers_t			cleartext_head;
pgp_fixed_body_t		cleartext_body;
struct pgp_hash_t		*cleartext_trailer;
pgp_dyn_body_t		unarmoured_text;
pgp_pk_sesskey_t		pk_sesskey;
pgp_seckey_passphrase_t	skey_passphrase;
unsigned			se_ip_data_header;
pgp_dyn_body_t		se_ip_data_body;
pgp_fixed_body_t		se_data_body;
pgp_get_seckey_t		get_seckey;
} pgp_contents_t;
struct pgp_packet_t {
pgp_content_enum	tag;
uint8_t			critical;
pgp_contents_t	u;
};
typedef struct {
uint8_t			fingerprint[PGP_FINGERPRINT_SIZE];
unsigned        	length;
pgp_hash_alg_t	hashtype;
} pgp_fingerprint_t;
int pgp_keyid(uint8_t *, const size_t, const pgp_pubkey_t *, pgp_hash_alg_t);
int pgp_fingerprint(pgp_fingerprint_t *, const pgp_pubkey_t *, pgp_hash_alg_t);
void pgp_finish(void);
void pgp_pubkey_free(pgp_pubkey_t *);
int pgp_pubkey_dup(pgp_pubkey_t *,pgp_pubkey_t *);
void pgp_userid_free(uint8_t **);
void pgp_data_free(pgp_data_t *);
void pgp_sig_free(pgp_sig_t *);
void pgp_ss_notation_free(pgp_ss_notation_t *);
void pgp_ss_revocation_free(pgp_ss_revocation_t *);
void pgp_ss_sig_target_free(pgp_ss_sig_target_t *);
void pgp_subpacket_free(pgp_subpacket_t *);
void pgp_parser_content_free(pgp_packet_t *);
void pgp_seckey_free(pgp_seckey_t *);
int pgp_seckey_dup(pgp_seckey_t *,pgp_seckey_t *);
void pgp_pk_sesskey_free(pgp_pk_sesskey_t *);
#define DYNARRAY(type, arr)	\
unsigned arr##c; unsigned arr##vsize; type *arr##s
#define EXPAND_ARRAY(str, arr) do {					\
if (str->arr##c == str->arr##vsize) {				\
void	*__newarr;					\
char	*__newarrc;					\
unsigned	__newsize;				\
__newsize = (str->arr##vsize * 2) + 10; 		\
if ((__newarrc = __newarr = realloc(str->arr##s,	\
__newsize * sizeof(*str->arr##s))) == NULL) {	\
(void) fprintf(stderr, "EXPAND_ARRAY - bad realloc\n"); \
} else {						\
(void) memset(&__newarrc[str->arr##vsize * sizeof(*str->arr##s)], \
0x0, (__newsize - str->arr##vsize) * sizeof(*str->arr##s)); \
str->arr##s = __newarr;				\
str->arr##vsize = __newsize;			\
}							\
}								\
} while(0)
#define FREE_ARRAY(str, arr) do {					\
if (str->arr##s) {				\
free(str->arr##s);				\
str->arr##s = NULL;				\
str->arr##vsize = 0;			\
str->arr##c = 0;				\
}								\
} while(0)
#define INIT_ARRAY(str, arr) do {					\
str->arr##s = NULL;				\
str->arr##vsize = 0;			\
str->arr##c = 0;				\
} while(0)
typedef union {
pgp_pubkey_t pubkey;
pgp_seckey_t seckey;
} pgp_keydata_key_t;
typedef struct pgp_uidsig_t {
uint32_t		uid;
pgp_sig_info_t		siginfo;
uint8_t			trustlevel;
uint8_t			trustamount;
pgp_subpacket_t	packet;
} pgp_uidsig_t;
typedef struct pgp_subkeysig_t {
uint32_t		subkey;
pgp_sig_info_t		siginfo;
pgp_subpacket_t	packet;
} pgp_subkeysig_t;
typedef struct pgp_subkey_t {
pgp_keydata_key_t	key;
uint8_t id[PGP_KEY_ID_SIZE];
} pgp_subkey_t;
typedef struct pgp_directsig_t {
pgp_sig_info_t siginfo;
pgp_subpacket_t  packet;
} pgp_directsig_t;
struct pgp_key_t {
pgp_content_enum	type;
pgp_keydata_key_t	key;
DYNARRAY(pgp_directsig_t, directsig);
DYNARRAY(uint8_t *, uid);
DYNARRAY(pgp_uidsig_t, uidsig);
DYNARRAY(pgp_subkey_t,	subkey);
DYNARRAY(pgp_subkeysig_t, subkeysig);
uint8_t			pubkeyid[PGP_KEY_ID_SIZE];
pgp_fingerprint_t	pubkeyfpr;
};
typedef enum {
PGP_VALID,
PGP_WEAK,
PGP_TOOSHORT,
PGP_INVALID,
PGP_EXPIRED,
PGP_REVOKED
} pgp_key_rating_t;
#define MDC_PKT_TAG	0xd3
#endif