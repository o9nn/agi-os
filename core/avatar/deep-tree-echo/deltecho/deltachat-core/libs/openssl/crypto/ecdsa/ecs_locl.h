#ifndef HEADER_ECS_LOCL_H
# define HEADER_ECS_LOCL_H
# include <openssl/ecdsa.h>
#ifdef __cplusplus
extern "C" {
#endif
struct ecdsa_method {
const char *name;
ECDSA_SIG *(*ecdsa_do_sign) (const unsigned char *dgst, int dgst_len,
const BIGNUM *inv, const BIGNUM *rp,
EC_KEY *eckey);
int (*ecdsa_sign_setup) (EC_KEY *eckey, BN_CTX *ctx, BIGNUM **kinv,
BIGNUM **r);
int (*ecdsa_do_verify) (const unsigned char *dgst, int dgst_len,
const ECDSA_SIG *sig, EC_KEY *eckey);
# if 0
int (*init) (EC_KEY *eckey);
int (*finish) (EC_KEY *eckey);
# endif
int flags;
char *app_data;
};
# define ECDSA_FLAG_FIPS_METHOD 0x1
typedef struct ecdsa_data_st {
int (*init) (EC_KEY *);
ENGINE *engine;
int flags;
const ECDSA_METHOD *meth;
CRYPTO_EX_DATA ex_data;
} ECDSA_DATA;
ECDSA_DATA *ecdsa_check(EC_KEY *eckey);
#ifdef __cplusplus
}
#endif
#endif