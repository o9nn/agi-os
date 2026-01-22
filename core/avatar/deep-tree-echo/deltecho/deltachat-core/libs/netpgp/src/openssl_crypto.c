#include "netpgp/config-netpgp.h"
#ifdef HAVE_SYS_CDEFS_H
#include <sys/cdefs.h>
#endif
#if defined(__NetBSD__)
__COPYRIGHT("@(#) Copyright (c) 2009 The NetBSD Foundation, Inc. All rights reserved.");
__RCSID("$NetBSD$");
#endif
#ifdef HAVE_OPENSSL_DSA_H
#include <openssl/dsa.h>
#endif
#ifdef HAVE_OPENSSL_RSA_H
#include <openssl/rsa.h>
#endif
#ifdef HAVE_OPENSSL_ERR_H
#include <openssl/err.h>
#endif
#include <openssl/pem.h>
#include <openssl/evp.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include "netpgp/crypto.h"
#include "netpgp/keyring.h"
#include "netpgp/readerwriter.h"
#include "netpgp/netpgpdefs.h"
#include "netpgp/netpgpdigest.h"
#include "netpgp/packet.h"
#include "netpgp/openssl11stub.h"
static int
md5_init(pgp_hash_t *hash)
{
if (hash->data) {
(void) fprintf(stderr, "md5_init: hash data non-null\n");
}
if ((hash->data = calloc(1, sizeof(MD5_CTX))) == NULL) {
(void) fprintf(stderr, "md5_init: bad alloc\n");
return 0;
}
MD5_Init(hash->data);
return 1;
}
static void
md5_add(pgp_hash_t *hash, const uint8_t *data, unsigned length)
{
MD5_Update(hash->data, data, length);
}
static unsigned
md5_finish(pgp_hash_t *hash, uint8_t *out)
{
MD5_Final(out, hash->data);
free(hash->data);
hash->data = NULL;
return 16;
}
static const pgp_hash_t md5 = {
PGP_HASH_MD5,
MD5_DIGEST_LENGTH,
"MD5",
md5_init,
md5_add,
md5_finish,
NULL
};
void
pgp_hash_md5(pgp_hash_t *hash)
{
*hash = md5;
}
static int
sha1_init(pgp_hash_t *hash)
{
if (hash->data) {
(void) fprintf(stderr, "sha1_init: hash data non-null\n");
}
if ((hash->data = calloc(1, sizeof(SHA_CTX))) == NULL) {
(void) fprintf(stderr, "sha1_init: bad alloc\n");
return 0;
}
SHA1_Init(hash->data);
return 1;
}
static void
sha1_add(pgp_hash_t *hash, const uint8_t *data, unsigned length)
{
if (pgp_get_debug_level(__FILE__)) {
hexdump(stderr, "sha1_add", data, length);
}
SHA1_Update(hash->data, data, length);
}
static unsigned
sha1_finish(pgp_hash_t *hash, uint8_t *out)
{
SHA1_Final(out, hash->data);
if (pgp_get_debug_level(__FILE__)) {
hexdump(stderr, "sha1_finish", out, PGP_SHA1_HASH_SIZE);
}
free(hash->data);
hash->data = NULL;
return PGP_SHA1_HASH_SIZE;
}
static const pgp_hash_t sha1 = {
PGP_HASH_SHA1,
PGP_SHA1_HASH_SIZE,
"SHA1",
sha1_init,
sha1_add,
sha1_finish,
NULL
};
void
pgp_hash_sha1(pgp_hash_t *hash)
{
*hash = sha1;
}
static int
sha256_init(pgp_hash_t *hash)
{
if (hash->data) {
(void) fprintf(stderr, "sha256_init: hash data non-null\n");
}
if ((hash->data = calloc(1, sizeof(SHA256_CTX))) == NULL) {
(void) fprintf(stderr, "sha256_init: bad alloc\n");
return 0;
}
SHA256_Init(hash->data);
return 1;
}
static void
sha256_add(pgp_hash_t *hash, const uint8_t *data, unsigned length)
{
if (pgp_get_debug_level(__FILE__)) {
hexdump(stderr, "sha256_add", data, length);
}
SHA256_Update(hash->data, data, length);
}
static unsigned
sha256_finish(pgp_hash_t *hash, uint8_t *out)
{
SHA256_Final(out, hash->data);
if (pgp_get_debug_level(__FILE__)) {
hexdump(stderr, "sha1_finish", out, SHA256_DIGEST_LENGTH);
}
free(hash->data);
hash->data = NULL;
return SHA256_DIGEST_LENGTH;
}
static const pgp_hash_t sha256 = {
PGP_HASH_SHA256,
SHA256_DIGEST_LENGTH,
"SHA256",
sha256_init,
sha256_add,
sha256_finish,
NULL
};
void
pgp_hash_sha256(pgp_hash_t *hash)
{
*hash = sha256;
}
static int
sha384_init(pgp_hash_t *hash)
{
if (hash->data) {
(void) fprintf(stderr, "sha384_init: hash data non-null\n");
}
if ((hash->data = calloc(1, sizeof(SHA512_CTX))) == NULL) {
(void) fprintf(stderr, "sha384_init: bad alloc\n");
return 0;
}
SHA384_Init(hash->data);
return 1;
}
static void
sha384_add(pgp_hash_t *hash, const uint8_t *data, unsigned length)
{
if (pgp_get_debug_level(__FILE__)) {
hexdump(stderr, "sha384_add", data, length);
}
SHA384_Update(hash->data, data, length);
}
static unsigned
sha384_finish(pgp_hash_t *hash, uint8_t *out)
{
SHA384_Final(out, hash->data);
if (pgp_get_debug_level(__FILE__)) {
hexdump(stderr, "sha384_finish", out, SHA384_DIGEST_LENGTH);
}
free(hash->data);
hash->data = NULL;
return SHA384_DIGEST_LENGTH;
}
static const pgp_hash_t sha384 = {
PGP_HASH_SHA384,
SHA384_DIGEST_LENGTH,
"SHA384",
sha384_init,
sha384_add,
sha384_finish,
NULL
};
void
pgp_hash_sha384(pgp_hash_t *hash)
{
*hash = sha384;
}
static int
sha512_init(pgp_hash_t *hash)
{
if (hash->data) {
(void) fprintf(stderr, "sha512_init: hash data non-null\n");
}
if ((hash->data = calloc(1, sizeof(SHA512_CTX))) == NULL) {
(void) fprintf(stderr, "sha512_init: bad alloc\n");
return 0;
}
SHA512_Init(hash->data);
return 1;
}
static void
sha512_add(pgp_hash_t *hash, const uint8_t *data, unsigned length)
{
if (pgp_get_debug_level(__FILE__)) {
hexdump(stderr, "sha512_add", data, length);
}
SHA512_Update(hash->data, data, length);
}
static unsigned
sha512_finish(pgp_hash_t *hash, uint8_t *out)
{
SHA512_Final(out, hash->data);
if (pgp_get_debug_level(__FILE__)) {
hexdump(stderr, "sha512_finish", out, SHA512_DIGEST_LENGTH);
}
free(hash->data);
hash->data = NULL;
return SHA512_DIGEST_LENGTH;
}
static const pgp_hash_t sha512 = {
PGP_HASH_SHA512,
SHA512_DIGEST_LENGTH,
"SHA512",
sha512_init,
sha512_add,
sha512_finish,
NULL
};
void
pgp_hash_sha512(pgp_hash_t *hash)
{
*hash = sha512;
}
static int
sha224_init(pgp_hash_t *hash)
{
if (hash->data) {
(void) fprintf(stderr, "sha224_init: hash data non-null\n");
}
if ((hash->data = calloc(1, sizeof(SHA256_CTX))) == NULL) {
(void) fprintf(stderr, "sha256_init: bad alloc\n");
return 0;
}
SHA224_Init(hash->data);
return 1;
}
static void
sha224_add(pgp_hash_t *hash, const uint8_t *data, unsigned length)
{
if (pgp_get_debug_level(__FILE__)) {
hexdump(stderr, "sha224_add", data, length);
}
SHA224_Update(hash->data, data, length);
}
static unsigned
sha224_finish(pgp_hash_t *hash, uint8_t *out)
{
SHA224_Final(out, hash->data);
if (pgp_get_debug_level(__FILE__)) {
hexdump(stderr, "sha224_finish", out, SHA224_DIGEST_LENGTH);
}
free(hash->data);
hash->data = NULL;
return SHA224_DIGEST_LENGTH;
}
static const pgp_hash_t sha224 = {
PGP_HASH_SHA224,
SHA224_DIGEST_LENGTH,
"SHA224",
sha224_init,
sha224_add,
sha224_finish,
NULL
};
void
pgp_hash_sha224(pgp_hash_t *hash)
{
*hash = sha224;
}
unsigned
pgp_dsa_verify(const uint8_t *hash, size_t hash_length,
const pgp_dsa_sig_t *sig,
const pgp_dsa_pubkey_t *dsa)
{
unsigned	qlen;
DSA_SIG        *osig;
DSA            *odsa;
int             ret;
osig = DSA_SIG_new();
DSA_SIG_set0(osig,
BN_dup(sig->r),
BN_dup(sig->s));
odsa = DSA_new();
DSA_set0_pqg(odsa,
BN_dup(dsa->p),
BN_dup(dsa->q),
BN_dup(dsa->g));
DSA_set0_key(odsa,
BN_dup(dsa->y),
NULL);
if (pgp_get_debug_level(__FILE__)) {
hexdump(stderr, "input hash", hash, hash_length);
(void) fprintf(stderr, "Q=%d\n", BN_num_bytes(dsa->q));
}
if ((qlen = (unsigned)BN_num_bytes(dsa->q)) < hash_length) {
hash_length = qlen;
}
ret = DSA_do_verify(hash, (int)hash_length, osig, odsa);
if (pgp_get_debug_level(__FILE__)) {
(void) fprintf(stderr, "ret=%d\n", ret);
}
if (ret < 0) {
(void) fprintf(stderr, "pgp_dsa_verify: DSA verification\n");
return 0;
}
DSA_free(odsa);
DSA_SIG_free(osig);
return (unsigned)ret;
}
int
pgp_rsa_public_decrypt(uint8_t *out,
const uint8_t *in,
size_t length,
const pgp_rsa_pubkey_t *pubkey)
{
RSA            *orsa;
int             n;
orsa = RSA_new();
RSA_set0_key(orsa,
BN_dup(pubkey->n),
BN_dup(pubkey->e),
NULL);
n = RSA_public_decrypt((int)length, in, out, orsa, RSA_NO_PADDING);
RSA_free(orsa);
return n;
}
int
pgp_rsa_private_encrypt(uint8_t *out,
const uint8_t *in,
size_t length,
const pgp_rsa_seckey_t *seckey,
const pgp_rsa_pubkey_t *pubkey)
{
RSA            *orsa;
int             n;
orsa = RSA_new();
RSA_set0_key(orsa,
BN_dup(pubkey->n),
BN_dup(pubkey->e),
BN_dup(seckey->d));
RSA_set0_factors(orsa,
BN_dup(seckey->p),
BN_dup(seckey->q));
n = RSA_private_encrypt((int)length, in, out, orsa, RSA_NO_PADDING);
RSA_free(orsa);
return n;
}
int
pgp_rsa_private_check(const pgp_seckey_t *seckey)
{
RSA *test = RSA_new();
int res = 1;
RSA_set0_key(test,
BN_dup(seckey->pubkey.key.rsa.n),
BN_dup(seckey->pubkey.key.rsa.e),
BN_dup(seckey->key.rsa.d));
RSA_set0_factors(test,
BN_dup(seckey->key.rsa.p),
BN_dup(seckey->key.rsa.q));
if (RSA_check_key(test) != 1) {
res = 0;
(void) fprintf(stderr,
"test_seckey: RSA_check_key failed\n");
}
RSA_free(test);
return res;
}
int pgp_dsa_private_check(const pgp_dsa_seckey_t *secdsa)
{
return 0;
}
int pgp_elgamal_private_check(const pgp_elgamal_seckey_t *seckey)
{
return 0;
}
int
pgp_rsa_private_decrypt(uint8_t *out,
const uint8_t *in,
size_t length,
const pgp_rsa_seckey_t *seckey,
const pgp_rsa_pubkey_t *pubkey)
{
RSA            *keypair;
int             n;
char            errbuf[1024];
keypair = RSA_new();
RSA_set0_key(keypair,
BN_dup(pubkey->n),
BN_dup(pubkey->e),
BN_dup(seckey->d));
RSA_set0_factors(keypair,
BN_dup(seckey->p),
BN_dup(seckey->q));
if (RSA_check_key(keypair) != 1) {
(void) fprintf(stderr, "RSA_check_key is not set\n");
return 0;
}
n = RSA_private_decrypt((int)length, in, out, keypair, RSA_NO_PADDING);
if (pgp_get_debug_level(__FILE__)) {
printf("pgp_rsa_private_decrypt: n=%d\n",n);
}
errbuf[0] = '\0';
if (n == -1) {
unsigned long   err = ERR_get_error();
ERR_error_string(err, &errbuf[0]);
(void) fprintf(stderr, "openssl error : %s\n", errbuf);
}
RSA_free(keypair);
return n;
}
int
pgp_rsa_public_encrypt(uint8_t *out,
const uint8_t *in,
size_t length,
const pgp_rsa_pubkey_t *pubkey)
{
RSA            *orsa;
int             n;
orsa = RSA_new();
RSA_set0_key(orsa,
BN_dup(pubkey->n),
BN_dup(pubkey->e),
NULL);
n = RSA_public_encrypt((int)length, in, out, orsa, RSA_NO_PADDING);
if (n == -1) {
BIO            *fd_out;
fd_out = BIO_new_fd(fileno(stderr), BIO_NOCLOSE);
ERR_print_errors(fd_out);
}
RSA_free(orsa);
return n;
}
void
pgp_crypto_finish(void)
{
}
const char     *
pgp_text_from_hash(pgp_hash_t *hash)
{
return hash->name;
}
unsigned
pgp_rsa_generate_keypair(pgp_key_t *keydata,
const int numbits,
const unsigned long e__,
const char *hashalg,
const char *cipher,
const uint8_t *passphrase,
const size_t pplen)
{
pgp_seckey_t *seckey;
RSA            *rsa;
BN_CTX         *ctx;
pgp_output_t *output;
pgp_memory_t   *mem;
int res;
const BIGNUM *_n = NULL;
const BIGNUM *_e = NULL;
const BIGNUM *_d = NULL;
const BIGNUM *_p = NULL;
const BIGNUM *_q = NULL;
ctx = BN_CTX_new();
pgp_keydata_init(keydata, PGP_PTAG_CT_SECRET_KEY);
seckey = pgp_get_writable_seckey(keydata);
BIGNUM *exp = BN_new();
BN_set_word(exp, e__);
rsa = RSA_new();
res = RSA_generate_key_ex(rsa, numbits, exp, NULL);
BN_free(exp);
if (!res){
RSA_free(rsa);
return 0;
};
seckey->pubkey.version = PGP_V4;
seckey->pubkey.birthtime = time(NULL);
seckey->pubkey.days_valid = 0;
seckey->pubkey.alg = PGP_PKA_RSA;
RSA_get0_key(rsa, &_n, &_e, &_d);
seckey->pubkey.key.rsa.n = BN_dup(_n);
seckey->pubkey.key.rsa.e = BN_dup(_e);
seckey->s2k_usage = PGP_S2KU_NONE;
if ((seckey->hash_alg = pgp_str_to_hash_alg(hashalg)) == PGP_HASH_UNKNOWN) {
seckey->hash_alg = PGP_HASH_SHA1;
}
seckey->alg = pgp_str_to_cipher(cipher);
seckey->octetc = 0;
seckey->checksum = 0;
RSA_get0_factors(rsa, &_p, &_q);
seckey->key.rsa.d = BN_dup(_d);
seckey->key.rsa.p = BN_dup(_p);
seckey->key.rsa.q = BN_dup(_q);
seckey->key.rsa.u = BN_mod_inverse(NULL, _p, _q, ctx);
if (seckey->key.rsa.u == NULL) {
RSA_free(rsa);
(void) fprintf(stderr, "seckey->key.rsa.u is NULL\n");
return 0;
}
BN_CTX_free(ctx);
RSA_free(rsa);
pgp_keyid(keydata->pubkeyid, PGP_KEY_ID_SIZE, &keydata->key.seckey.pubkey, seckey->hash_alg);
pgp_fingerprint(&keydata->pubkeyfpr, &keydata->key.seckey.pubkey, seckey->hash_alg);
output = NULL;
mem = NULL;
pgp_setup_memory_write(&output, &mem, 128);
pgp_push_checksum_writer(output, seckey);
switch (seckey->pubkey.alg) {
case PGP_PKA_DSA:
return pgp_write_mpi(output, seckey->key.dsa.x);
case PGP_PKA_RSA:
case PGP_PKA_RSA_ENCRYPT_ONLY:
case PGP_PKA_RSA_SIGN_ONLY:
if (!pgp_write_mpi(output, seckey->key.rsa.d) ||
!pgp_write_mpi(output, seckey->key.rsa.p) ||
!pgp_write_mpi(output, seckey->key.rsa.q) ||
!pgp_write_mpi(output, seckey->key.rsa.u)) {
return 0;
}
break;
case PGP_PKA_ELGAMAL:
return pgp_write_mpi(output, seckey->key.elgamal.x);
default:
(void) fprintf(stderr, "Bad seckey->pubkey.alg\n");
return 0;
}
pgp_writer_close(output);
pgp_teardown_memory_write(output, mem);
if (pgp_get_debug_level(__FILE__)) {
pgp_rsa_private_check(seckey);
}
return 1;
}
#if 0
pgp_key_t  *
pgp_rsa_new_selfsign_key(const int numbits,
const unsigned long e,
const uint8_t *userid,
const char *hashalg,
const char *cipher)
{
pgp_key_t  *keydata;
keydata = pgp_keydata_new();
if (!pgp_rsa_generate_keypair(keydata, numbits, e, hashalg, cipher,
(const uint8_t *) "", (const size_t) 0) ||
!pgp_add_selfsigned_userid(keydata, NULL, userid, 0 )) {
pgp_keydata_free(keydata);
return NULL;
}
return keydata;
}
#endif
pgp_dsa_sig_t        *
pgp_dsa_sign(uint8_t *hashbuf,
unsigned hashsize,
const pgp_dsa_seckey_t *secdsa,
const pgp_dsa_pubkey_t *pubdsa)
{
DSA_SIG        *dsasig;
DSA            *odsa;
pgp_dsa_sig_t  *pgpdsasig;
const BIGNUM *pr = NULL;
const BIGNUM *ps = NULL;
odsa = DSA_new();
DSA_set0_pqg(odsa,
BN_dup(pubdsa->p),
BN_dup(pubdsa->q),
BN_dup(pubdsa->g));
DSA_set0_key(odsa,
BN_dup(pubdsa->y),
BN_dup(secdsa->x));
dsasig = DSA_do_sign(hashbuf, (int)hashsize, odsa);
DSA_free(odsa);
DSA_SIG_get0(dsasig, &pr, &ps);
pgpdsasig = calloc(1,sizeof(pgp_dsa_sig_t));
if(pgpdsasig != NULL){
pgpdsasig->r = BN_dup(pr);
pgpdsasig->s = BN_dup(ps);
}
DSA_SIG_free(dsasig);
return pgpdsasig;
}
static int
decide_k_bits(int p_bits)
{
return (p_bits <= 5120) ? p_bits / 10 + 160 : (p_bits / 8 + 200) * 3 / 2;
}
int
pgp_elgamal_public_encrypt(uint8_t *g_to_k, uint8_t *encm,
const uint8_t *in,
size_t size,
const pgp_elgamal_pubkey_t *pubkey)
{
int	ret = 0;
int	k_bits;
BIGNUM	   *m;
BIGNUM	   *p;
BIGNUM	   *g;
BIGNUM	   *y;
BIGNUM	   *k;
BIGNUM	   *yk;
BIGNUM	   *c1;
BIGNUM	   *c2;
BN_CTX	   *tmp;
m = BN_bin2bn(in, (int)size, NULL);
p = pubkey->p;
g = pubkey->g;
y = pubkey->y;
k = BN_new();
yk = BN_new();
c1 = BN_new();
c2 = BN_new();
tmp = BN_CTX_new();
if (!m || !p || !g || !y || !k || !yk || !c1 || !c2 || !tmp) {
goto done;
}
k_bits = decide_k_bits(BN_num_bits(p));
if (!BN_rand(k, k_bits, 0, 0)) {
goto done;
}
if (!BN_mod_exp(c1, g, k, p, tmp)) {
goto done;
}
if (!BN_mod_exp(yk, y, k, p, tmp)) {
goto done;
}
if (!BN_mod_mul(c2, m, yk, p, tmp)) {
goto done;
}
BN_bn2bin(c1, g_to_k);
ret = BN_num_bytes(c1);
BN_bn2bin(c2, encm);
ret += BN_num_bytes(c2);
done:
if (tmp) {
BN_CTX_free(tmp);
}
if (c2) {
BN_clear_free(c2);
}
if (c1) {
BN_clear_free(c1);
}
if (yk) {
BN_clear_free(yk);
}
if (k) {
BN_clear_free(k);
}
if (g) {
BN_clear_free(g);
}
return ret;
}
int
pgp_elgamal_private_decrypt(uint8_t *out,
const uint8_t *g_to_k,
const uint8_t *in,
size_t length,
const pgp_elgamal_seckey_t *seckey,
const pgp_elgamal_pubkey_t *pubkey)
{
BIGNUM	*bndiv;
BIGNUM	*c1x;
BN_CTX	*tmp;
BIGNUM	*c1;
BIGNUM	*c2;
BIGNUM	*p;
BIGNUM	*x;
BIGNUM	*m;
int	 ret;
ret = 0;
c1 = BN_bin2bn(g_to_k, (int)length, NULL);
c2 = BN_bin2bn(in, (int)length, NULL);
p = pubkey->p;
x = seckey->x;
c1x = BN_new();
bndiv = BN_new();
m = BN_new();
tmp = BN_CTX_new();
if (!c1 || !c2 || !p || !x || !c1x || !bndiv || !m || !tmp) {
goto done;
}
if (!BN_mod_exp(c1x, c1, x, p, tmp)) {
goto done;
}
if (!BN_mod_inverse(bndiv, c1x, p, tmp)) {
goto done;
}
if (!BN_mod_mul(m, c2, bndiv, p, tmp)) {
goto done;
}
ret = BN_bn2bin(m, out);
done:
if (tmp) {
BN_CTX_free(tmp);
}
if (m) {
BN_clear_free(m);
}
if (bndiv) {
BN_clear_free(bndiv);
}
if (c1x) {
BN_clear_free(c1x);
}
if (x) {
BN_clear_free(x);
}
if (p) {
BN_clear_free(p);
}
if (c1) {
BN_clear_free(c1);
}
if (c2) {
BN_clear_free(c2);
}
return ret;
}