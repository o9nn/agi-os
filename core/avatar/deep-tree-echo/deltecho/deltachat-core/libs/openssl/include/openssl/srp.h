#ifndef __SRP_H__
# define __SRP_H__
# ifndef OPENSSL_NO_SRP
# include <stdio.h>
# include <string.h>
#ifdef __cplusplus
extern "C" {
#endif
# include <openssl/safestack.h>
# include <openssl/bn.h>
# include <openssl/crypto.h>
typedef struct SRP_gN_cache_st {
char *b64_bn;
BIGNUM *bn;
} SRP_gN_cache;
DECLARE_STACK_OF(SRP_gN_cache)
typedef struct SRP_user_pwd_st {
char *id;
BIGNUM *s;
BIGNUM *v;
const BIGNUM *g;
const BIGNUM *N;
char *info;
} SRP_user_pwd;
DECLARE_STACK_OF(SRP_user_pwd)
void SRP_user_pwd_free(SRP_user_pwd *user_pwd);
typedef struct SRP_VBASE_st {
STACK_OF(SRP_user_pwd) *users_pwd;
STACK_OF(SRP_gN_cache) *gN_cache;
char *seed_key;
BIGNUM *default_g;
BIGNUM *default_N;
} SRP_VBASE;
typedef struct SRP_gN_st {
char *id;
BIGNUM *g;
BIGNUM *N;
} SRP_gN;
DECLARE_STACK_OF(SRP_gN)
SRP_VBASE *SRP_VBASE_new(char *seed_key);
int SRP_VBASE_free(SRP_VBASE *vb);
int SRP_VBASE_init(SRP_VBASE *vb, char *verifier_file);
SRP_user_pwd *SRP_VBASE_get_by_user(SRP_VBASE *vb, char *username);
SRP_user_pwd *SRP_VBASE_get1_by_user(SRP_VBASE *vb, char *username);
char *SRP_create_verifier(const char *user, const char *pass, char **salt,
char **verifier, const char *N, const char *g);
int SRP_create_verifier_BN(const char *user, const char *pass, BIGNUM **salt,
BIGNUM **verifier, BIGNUM *N, BIGNUM *g);
# define SRP_NO_ERROR 0
# define SRP_ERR_VBASE_INCOMPLETE_FILE 1
# define SRP_ERR_VBASE_BN_LIB 2
# define SRP_ERR_OPEN_FILE 3
# define SRP_ERR_MEMORY 4
# define DB_srptype 0
# define DB_srpverifier 1
# define DB_srpsalt 2
# define DB_srpid 3
# define DB_srpgN 4
# define DB_srpinfo 5
# undef DB_NUMBER
# define DB_NUMBER 6
# define DB_SRP_INDEX 'I'
# define DB_SRP_VALID 'V'
# define DB_SRP_REVOKED 'R'
# define DB_SRP_MODIF 'v'
char *SRP_check_known_gN_param(BIGNUM *g, BIGNUM *N);
SRP_gN *SRP_get_default_gN(const char *id);
BIGNUM *SRP_Calc_server_key(BIGNUM *A, BIGNUM *v, BIGNUM *u, BIGNUM *b,
BIGNUM *N);
BIGNUM *SRP_Calc_B(BIGNUM *b, BIGNUM *N, BIGNUM *g, BIGNUM *v);
int SRP_Verify_A_mod_N(BIGNUM *A, BIGNUM *N);
BIGNUM *SRP_Calc_u(BIGNUM *A, BIGNUM *B, BIGNUM *N);
BIGNUM *SRP_Calc_x(BIGNUM *s, const char *user, const char *pass);
BIGNUM *SRP_Calc_A(BIGNUM *a, BIGNUM *N, BIGNUM *g);
BIGNUM *SRP_Calc_client_key(BIGNUM *N, BIGNUM *B, BIGNUM *g, BIGNUM *x,
BIGNUM *a, BIGNUM *u);
int SRP_Verify_B_mod_N(BIGNUM *B, BIGNUM *N);
# define SRP_MINIMAL_N 1024
#ifdef __cplusplus
}
#endif
# endif
#endif