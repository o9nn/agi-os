#ifndef HMAC_MD5_H
#define HMAC_MD5_H 1
#ifdef __cplusplus
extern "C" {
#endif
#define HMAC_MD5_SIZE 16
typedef struct HMAC_MD5_CTX_s {
MD5_CTX ictx, octx;
} HMAC_MD5_CTX;
typedef struct HMAC_MD5_STATE_s {
UINT4 istate[4];
UINT4 ostate[4];
} HMAC_MD5_STATE;
void hmac_md5(const unsigned char *text, int text_len,
const unsigned char *key, int key_len,
unsigned char digest[HMAC_MD5_SIZE]);
void hmac_md5_init(HMAC_MD5_CTX *hmac,
const unsigned char *key, int key_len);
void hmac_md5_precalc(HMAC_MD5_STATE *hmac,
const unsigned char *key, int key_len);
void hmac_md5_import(HMAC_MD5_CTX *hmac, HMAC_MD5_STATE *state);
#define hmac_md5_update(hmac, text, text_len) MD5Update(&(hmac)->ictx, (text), (text_len))
void hmac_md5_final(unsigned char digest[HMAC_MD5_SIZE],
HMAC_MD5_CTX *hmac);
#ifdef __cplusplus
}
#endif
#endif