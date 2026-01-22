#ifndef HEADER_JPAKE_H
# define HEADER_JPAKE_H
# include <openssl/opensslconf.h>
# ifdef OPENSSL_NO_JPAKE
# error JPAKE is disabled.
# endif
#ifdef __cplusplus
extern "C" {
#endif
# include <openssl/bn.h>
# include <openssl/sha.h>
typedef struct JPAKE_CTX JPAKE_CTX;
typedef struct {
BIGNUM *gr;
BIGNUM *b;
} JPAKE_ZKP;
typedef struct {
BIGNUM *gx;
JPAKE_ZKP zkpx;
} JPAKE_STEP_PART;
typedef struct {
JPAKE_STEP_PART p1;
JPAKE_STEP_PART p2;
} JPAKE_STEP1;
typedef JPAKE_STEP_PART JPAKE_STEP2;
typedef struct {
unsigned char hhk[SHA_DIGEST_LENGTH];
} JPAKE_STEP3A;
typedef struct {
unsigned char hk[SHA_DIGEST_LENGTH];
} JPAKE_STEP3B;
JPAKE_CTX *JPAKE_CTX_new(const char *name, const char *peer_name,
const BIGNUM *p, const BIGNUM *g, const BIGNUM *q,
const BIGNUM *secret);
void JPAKE_CTX_free(JPAKE_CTX *ctx);
void JPAKE_STEP1_init(JPAKE_STEP1 *s1);
int JPAKE_STEP1_generate(JPAKE_STEP1 *send, JPAKE_CTX *ctx);
int JPAKE_STEP1_process(JPAKE_CTX *ctx, const JPAKE_STEP1 *received);
void JPAKE_STEP1_release(JPAKE_STEP1 *s1);
void JPAKE_STEP2_init(JPAKE_STEP2 *s2);
int JPAKE_STEP2_generate(JPAKE_STEP2 *send, JPAKE_CTX *ctx);
int JPAKE_STEP2_process(JPAKE_CTX *ctx, const JPAKE_STEP2 *received);
void JPAKE_STEP2_release(JPAKE_STEP2 *s2);
void JPAKE_STEP3A_init(JPAKE_STEP3A *s3a);
int JPAKE_STEP3A_generate(JPAKE_STEP3A *send, JPAKE_CTX *ctx);
int JPAKE_STEP3A_process(JPAKE_CTX *ctx, const JPAKE_STEP3A *received);
void JPAKE_STEP3A_release(JPAKE_STEP3A *s3a);
void JPAKE_STEP3B_init(JPAKE_STEP3B *s3b);
int JPAKE_STEP3B_generate(JPAKE_STEP3B *send, JPAKE_CTX *ctx);
int JPAKE_STEP3B_process(JPAKE_CTX *ctx, const JPAKE_STEP3B *received);
void JPAKE_STEP3B_release(JPAKE_STEP3B *s3b);
const BIGNUM *JPAKE_get_shared_key(JPAKE_CTX *ctx);
void ERR_load_JPAKE_strings(void);
# define JPAKE_F_JPAKE_STEP1_PROCESS 101
# define JPAKE_F_JPAKE_STEP2_PROCESS 102
# define JPAKE_F_JPAKE_STEP3A_PROCESS 103
# define JPAKE_F_JPAKE_STEP3B_PROCESS 104
# define JPAKE_F_VERIFY_ZKP 100
# define JPAKE_R_G_TO_THE_X3_IS_NOT_LEGAL 108
# define JPAKE_R_G_TO_THE_X4_IS_NOT_LEGAL 109
# define JPAKE_R_G_TO_THE_X4_IS_ONE 105
# define JPAKE_R_HASH_OF_HASH_OF_KEY_MISMATCH 106
# define JPAKE_R_HASH_OF_KEY_MISMATCH 107
# define JPAKE_R_VERIFY_B_FAILED 102
# define JPAKE_R_VERIFY_X3_FAILED 103
# define JPAKE_R_VERIFY_X4_FAILED 104
# define JPAKE_R_ZKP_VERIFY_FAILED 100
#ifdef __cplusplus
}
#endif
#endif