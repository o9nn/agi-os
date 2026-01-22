#include "netpgp/config-netpgp.h"
#ifdef HAVE_SYS_CDEFS_H
#include <sys/cdefs.h>
#endif
#if defined(__NetBSD__)
__COPYRIGHT("@(#) Copyright (c) 2009 The NetBSD Foundation, Inc. All rights reserved.");
__RCSID("$NetBSD$");
#endif
#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_OPENSSL_CAST_H
#include <openssl/cast.h>
#endif
#include "netpgp/create.h"
#include "netpgp/keyring.h"
#include "netpgp/packet.h"
#include "netpgp/signature.h"
#include "netpgp/writer.h"
#include "netpgp/readerwriter.h"
#include "netpgp/memory.h"
#include "netpgp/netpgpdefs.h"
#include "netpgp/netpgpdigest.h"
unsigned
pgp_write_ss_header(pgp_output_t *output,
size_t length,
pgp_content_enum type)
{
return pgp_write_length(output, (unsigned int)length) &&
pgp_write_scalar(output, (unsigned)(type -
(unsigned)PGP_PTAG_SIG_SUBPKT_BASE), 1);
}
void
pgp_fast_create_userid(uint8_t **id, uint8_t *userid)
{
*id = userid;
}
unsigned
pgp_write_struct_userid(pgp_output_t *output, const uint8_t *id)
{
return pgp_write_ptag(output, PGP_PTAG_CT_USER_ID) &&
pgp_write_length(output, (unsigned)strlen((const char *) id)) &&
pgp_write(output, id, (unsigned)strlen((const char *) id));
}
unsigned
pgp_write_userid(const uint8_t *userid, pgp_output_t *output)
{
return pgp_write_struct_userid(output, userid);
}
static unsigned
mpi_length(const BIGNUM *bn)
{
return (unsigned)(2 + (BN_num_bits(bn) + 7) / 8);
}
static unsigned
pubkey_length(const pgp_pubkey_t *key)
{
switch (key->alg) {
case PGP_PKA_DSA:
return mpi_length(key->key.dsa.p) + mpi_length(key->key.dsa.q) +
mpi_length(key->key.dsa.g) + mpi_length(key->key.dsa.y);
case PGP_PKA_RSA:
return mpi_length(key->key.rsa.n) + mpi_length(key->key.rsa.e);
case PGP_PKA_ELGAMAL:
return mpi_length(key->key.elgamal.p) +
mpi_length(key->key.elgamal.g) +
mpi_length(key->key.elgamal.y);
default:
(void) fprintf(stderr,
"pubkey_length: unknown key algorithm\n");
}
return 0;
}
static unsigned
seckey_length(const pgp_seckey_t *key)
{
int             len;
switch (key->pubkey.alg) {
case PGP_PKA_DSA:
return (unsigned)(mpi_length(key->key.dsa.x) + pubkey_length(&key->pubkey));
case PGP_PKA_RSA:
len = mpi_length(key->key.rsa.d) + mpi_length(key->key.rsa.p) +
mpi_length(key->key.rsa.q) + mpi_length(key->key.rsa.u);
return (unsigned)(len + pubkey_length(&key->pubkey));
case PGP_PKA_ELGAMAL:
return (unsigned)(
mpi_length(key->key.dsa.x) + pubkey_length(&key->pubkey));
default:
(void) fprintf(stderr,
"seckey_length: unknown key algorithm\n");
}
return 0;
}
void
pgp_fast_create_rsa_pubkey(pgp_pubkey_t *key, time_t t,
BIGNUM *n, BIGNUM *e)
{
key->version = PGP_V4;
key->birthtime = t;
key->alg = PGP_PKA_RSA;
key->key.rsa.n = n;
key->key.rsa.e = e;
}
static unsigned
write_pubkey_body(const pgp_pubkey_t *key, pgp_output_t *output)
{
if (!(pgp_write_scalar(output, (unsigned)key->version, 1) &&
pgp_write_scalar(output, (unsigned)key->birthtime, 4))) {
return 0;
}
if (key->version != 4 &&
!pgp_write_scalar(output, key->days_valid, 2)) {
return 0;
}
if (!pgp_write_scalar(output, (unsigned)key->alg, 1)) {
return 0;
}
switch (key->alg) {
case PGP_PKA_DSA:
return pgp_write_mpi(output, key->key.dsa.p) &&
pgp_write_mpi(output, key->key.dsa.q) &&
pgp_write_mpi(output, key->key.dsa.g) &&
pgp_write_mpi(output, key->key.dsa.y);
case PGP_PKA_RSA:
case PGP_PKA_RSA_ENCRYPT_ONLY:
case PGP_PKA_RSA_SIGN_ONLY:
return pgp_write_mpi(output, key->key.rsa.n) &&
pgp_write_mpi(output, key->key.rsa.e);
case PGP_PKA_ELGAMAL:
return pgp_write_mpi(output, key->key.elgamal.p) &&
pgp_write_mpi(output, key->key.elgamal.g) &&
pgp_write_mpi(output, key->key.elgamal.y);
default:
(void) fprintf(stderr,
"write_pubkey_body: bad algorithm\n");
break;
}
return 0;
}
static unsigned
write_seckey_body(const pgp_seckey_t *key,
const uint8_t *passphrase,
const size_t pplen,
pgp_output_t *output)
{
pgp_crypt_t   crypted;
pgp_hash_t    hash;
unsigned	done = 0;
unsigned	i = 0;
uint8_t		sesskey[CAST_KEY_LENGTH];
if (!write_pubkey_body(&key->pubkey, output)) {
return 0;
}
if (!pgp_write_scalar(output, (unsigned)key->s2k_usage, 1)) {
return 0;
}
if (key->s2k_usage != PGP_S2KU_NONE) {
if (key->s2k_usage != PGP_S2KU_ENCRYPTED_AND_HASHED) {
(void) fprintf(stderr, "write_seckey_body: s2k usage\n");
return 0;
}
if (key->alg != PGP_SA_CAST5) {
(void) fprintf(stderr, "write_seckey_body: algorithm\n");
return 0;
}
if (!pgp_write_scalar(output, (unsigned)key->alg, 1)) {
return 0;
}
if (key->s2k_specifier != PGP_S2KS_SIMPLE &&
key->s2k_specifier != PGP_S2KS_SALTED) {
(void) fprintf(stderr, "write_seckey_body: s2k spec\n");
return 0;
}
if (!pgp_write_scalar(output, (unsigned)key->s2k_specifier, 1)) {
return 0;
}
if (!pgp_write_scalar(output, (unsigned)key->hash_alg, 1)) {
return 0;
}
switch (key->s2k_specifier) {
case PGP_S2KS_SIMPLE:
break;
case PGP_S2KS_SALTED:
pgp_random(__UNCONST(&key->salt[0]), PGP_SALT_SIZE);
if (!pgp_write(output, key->salt, PGP_SALT_SIZE)) {
return 0;
}
break;
default:
(void) fprintf(stderr,
"invalid/unsupported s2k specifier %d\n",
key->s2k_specifier);
return 0;
}
if (!pgp_write(output, &key->iv[0], pgp_block_size(key->alg))) {
return 0;
}
switch (key->s2k_specifier) {
case PGP_S2KS_SIMPLE:
case PGP_S2KS_SALTED:
for (done = 0, i = 0; done < CAST_KEY_LENGTH; i++) {
unsigned 	hashsize;
unsigned 	j;
unsigned	needed;
unsigned	size;
uint8_t		zero = 0;
uint8_t		*hashed;
pgp_hash_any(&hash, PGP_HASH_SHA1);
hashsize = pgp_hash_size(key->hash_alg);
needed = CAST_KEY_LENGTH - done;
size = MIN(needed, hashsize);
if ((hashed = calloc(1, hashsize)) == NULL) {
(void) fprintf(stderr, "write_seckey_body: bad alloc\n");
return 0;
}
if (!hash.init(&hash)) {
(void) fprintf(stderr, "write_seckey_body: bad alloc\n");
free(hashed);
return 0;
}
for (j = 0; j < i; j++) {
hash.add(&hash, &zero, 1);
}
if (key->s2k_specifier == PGP_S2KS_SALTED) {
hash.add(&hash, key->salt, PGP_SALT_SIZE);
}
hash.add(&hash, passphrase, (unsigned)pplen);
hash.finish(&hash, hashed);
(void) memcpy(&sesskey[i * hashsize],
hashed, (unsigned)size);
done += (unsigned)size;
free(hashed);
if (done > CAST_KEY_LENGTH) {
(void) fprintf(stderr,
"write_seckey_body: short add\n");
return 0;
}
}
break;
default:
(void) fprintf(stderr,
"invalid/unsupported s2k specifier %d\n",
key->s2k_specifier);
return 0;
}
if( !pgp_crypt_any(&crypted, key->alg) ) {
return 0;
}
crypted.set_iv(&crypted, key->iv);
crypted.set_crypt_key(&crypted, sesskey);
pgp_encrypt_init(&crypted);
if (pgp_get_debug_level(__FILE__)) {
hexdump(stderr, "writing: iv=", key->iv, pgp_block_size(key->alg));
hexdump(stderr, "key= ", sesskey, CAST_KEY_LENGTH);
(void) fprintf(stderr, "\nturning encryption on...\n");
}
pgp_push_enc_crypt(output, &crypted);
}else{
pgp_push_sum16_writer(output);
}
switch (key->pubkey.alg) {
case PGP_PKA_RSA:
case PGP_PKA_RSA_ENCRYPT_ONLY:
case PGP_PKA_RSA_SIGN_ONLY:
if (!pgp_write_mpi(output, key->key.rsa.d) ||
!pgp_write_mpi(output, key->key.rsa.p) ||
!pgp_write_mpi(output, key->key.rsa.q) ||
!pgp_write_mpi(output, key->key.rsa.u)) {
if (pgp_get_debug_level(__FILE__)) {
(void) fprintf(stderr,
"4 x mpi not written - problem\n");
}
return 0;
}
break;
case PGP_PKA_DSA:
return pgp_write_mpi(output, key->key.dsa.x);
case PGP_PKA_ELGAMAL:
return pgp_write_mpi(output, key->key.elgamal.x);
default:
return 0;
}
if (key->s2k_usage != PGP_S2KU_NONE) {
if (!pgp_write(output, key->checkhash, PGP_CHECKHASH_SIZE)) {
return 0;
}
pgp_writer_pop(output);
}else{
uint16_t checksum = pgp_pop_sum16_writer(output);
if (!pgp_write_scalar(output, checksum, 2)) {
return 0;
}
}
return 1;
}
unsigned
pgp_write_struct_pubkey_ptag(
pgp_output_t *output,
const pgp_pubkey_t *key,
pgp_content_enum ptag)
{
return pgp_write_ptag(output, ptag) &&
pgp_write_length(output, 1 + 4 + 1 + pubkey_length(key)) &&
write_pubkey_body(key, output);
}
unsigned
pgp_write_struct_pubkey(pgp_output_t *output, const pgp_pubkey_t *key)
{
return pgp_write_struct_pubkey_ptag(output, key, PGP_PTAG_CT_PUBLIC_KEY);
}
unsigned
pgp_write_xfer_key(pgp_output_t *output,
const pgp_key_t *key,
const unsigned armoured)
{
unsigned          directsigidx = 0;
pgp_directsig_t  *directsigp;
unsigned          uididx = 0;
unsigned          uidsigidx = 0;
uint8_t         **uidp;
pgp_uidsig_t     *uidsigp;
pgp_subkey_t     *subkeyp;
unsigned          subkeyidx = 0;
unsigned          subkeysigidx = 0;
pgp_subkeysig_t  *subkeysigp;
#if 0
if (armoured) {
pgp_writer_push_armoured(output, PGP_PGP_PUBLIC_KEY_BLOCK);
}
#endif
if (key->type == PGP_PTAG_CT_PUBLIC_KEY) {
if (!pgp_write_struct_pubkey(output, &key->key.pubkey)) {
return 0;
}
}else{
if (!pgp_write_struct_seckey(&key->key.seckey, (const uint8_t *)"", 0, output)) {
return 0;
}
}
directsigp = key->directsigs;
for (directsigidx = 0 ; directsigidx < key->directsigc;
directsigidx++, directsigp++)
{
if (!pgp_write(output, directsigp->packet.raw,
(unsigned)directsigp->packet.length)) {
return 0;
}
}
uidp = key->uids;
for (uididx = 0 ; uididx < key->uidc; uididx++, uidp++)
{
if (!pgp_write_struct_userid(output, *uidp)) {
return 0;
}
uidsigp = key->uidsigs;
for (uidsigidx = 0 ; uidsigidx < key->uidsigc; uidsigidx++, uidsigp++)
{
if(uidsigp->uid == uididx)
{
if (!pgp_write(output, uidsigp->packet.raw,
(unsigned)uidsigp->packet.length)) {
return 0;
}
}
}
}
subkeyp = key->subkeys;
for (subkeyidx = 0 ; subkeyidx < key->subkeyc; subkeyidx++, subkeyp++)
{
if (key->type == PGP_PTAG_CT_PUBLIC_KEY) {
if (!pgp_write_struct_pubkey_ptag(
output, &subkeyp->key.pubkey,
PGP_PTAG_CT_PUBLIC_SUBKEY)) {
return 0;
}
}else{
if (!pgp_write_struct_seckey_ptag(
&subkeyp->key.seckey, (const uint8_t *)"", 0, output,
PGP_PTAG_CT_SECRET_SUBKEY)) {
return 0;
}
}
subkeysigp = key->subkeysigs;
for (subkeysigidx = 0 ; subkeysigidx < key->subkeysigc;
subkeysigidx++, subkeysigp++)
{
if(subkeysigp->subkey == subkeyidx)
{
if (!pgp_write(output, subkeysigp->packet.raw,
(unsigned)subkeysigp->packet.length)) {
return 0;
}
}
}
}
if (armoured) {
pgp_writer_info_finalise(&output->errors, &output->writer);
pgp_writer_pop(output);
}
return 1;
}
unsigned
pgp_write_rsa_pubkey(time_t t, const BIGNUM *n,
const BIGNUM *e,
pgp_output_t *output)
{
pgp_pubkey_t key;
pgp_fast_create_rsa_pubkey(&key, t, __UNCONST(n), __UNCONST(e));
return pgp_write_struct_pubkey(output, &key);
}
void
pgp_build_pubkey(pgp_memory_t *out, const pgp_pubkey_t *key,
unsigned make_packet)
{
pgp_output_t *output;
output = pgp_output_new();
pgp_memory_init(out, 128);
pgp_writer_set_memory(output, out);
write_pubkey_body(key, output);
if (make_packet) {
pgp_memory_make_packet(out, PGP_PTAG_CT_PUBLIC_KEY);
}
pgp_output_delete(output);
}
void
pgp_fast_create_rsa_seckey(pgp_seckey_t *key, time_t t,
BIGNUM *d, BIGNUM *p, BIGNUM *q, BIGNUM *u,
BIGNUM *n, BIGNUM *e)
{
pgp_fast_create_rsa_pubkey(&key->pubkey, t, n, e);
key->key.rsa.d = d;
key->key.rsa.p = p;
key->key.rsa.q = q;
key->key.rsa.u = u;
key->s2k_usage = PGP_S2KU_NONE;
}
unsigned
pgp_write_struct_seckey_ptag(const pgp_seckey_t *key,
const uint8_t *passphrase,
const size_t pplen,
pgp_output_t *output,
pgp_content_enum ptag)
{
int             length = 0;
if (key->pubkey.version != 4) {
(void) fprintf(stderr,
"pgp_write_struct_seckey: public key version\n");
return 0;
}
length += 1 + 4 + 1;
length += 1;
switch (key->s2k_usage) {
case PGP_S2KU_NONE:
break;
case PGP_S2KU_ENCRYPTED_AND_HASHED:
case PGP_S2KU_ENCRYPTED:
length += 1;
switch (key->s2k_specifier) {
case PGP_S2KS_SIMPLE:
length += 1;
break;
case PGP_S2KS_SALTED:
length += 1 + 8;
break;
case PGP_S2KS_ITERATED_AND_SALTED:
length += 1 + 8 + 1;
break;
default:
(void) fprintf(stderr,
"pgp_write_struct_seckey: s2k spec\n");
return 0;
}
break;
default:
(void) fprintf(stderr,
"pgp_write_struct_seckey: s2k usage\n");
return 0;
}
if (key->s2k_usage) {
length += pgp_block_size(key->alg);
}
switch (key->s2k_usage) {
case PGP_S2KU_NONE:
case PGP_S2KU_ENCRYPTED:
length += 2;
break;
case PGP_S2KU_ENCRYPTED_AND_HASHED:
length += PGP_CHECKHASH_SIZE;
break;
default:
(void) fprintf(stderr,
"pgp_write_struct_seckey: s2k cksum usage\n");
return 0;
}
length += (unsigned)seckey_length(key);
return pgp_write_ptag(output, ptag) &&
pgp_write_length(output, (unsigned)length) &&
write_seckey_body(key, passphrase, pplen, output);
}
unsigned
pgp_write_struct_seckey(const pgp_seckey_t *key,
const uint8_t *passphrase,
const size_t pplen,
pgp_output_t *output)
{
return  pgp_write_struct_seckey_ptag(
key, passphrase, pplen, output, PGP_PTAG_CT_SECRET_KEY);
}
pgp_output_t *
pgp_output_new(void)
{
return calloc(1, sizeof(pgp_output_t));
}
void
pgp_output_delete(pgp_output_t *output)
{
pgp_writer_info_delete(&output->writer);
free(output);
}
unsigned
pgp_calc_sesskey_checksum(pgp_pk_sesskey_t *sesskey, uint8_t cs[2])
{
uint32_t   checksum = 0;
unsigned    i;
if (!pgp_is_sa_supported(sesskey->symm_alg)) {
return 0;
}
for (i = 0; i < pgp_key_size(sesskey->symm_alg); i++) {
checksum += sesskey->key[i];
}
checksum = checksum % 65536;
cs[0] = (uint8_t)((checksum >> 8) & 0xff);
cs[1] = (uint8_t)(checksum & 0xff);
if (pgp_get_debug_level(__FILE__)) {
hexdump(stderr, "nm buf checksum:", cs, 2);
}
return 1;
}
static unsigned
create_unencoded_m_buf(pgp_pk_sesskey_t *sesskey, pgp_crypt_t *cipherinfo, uint8_t *m_buf)
{
unsigned	i;
m_buf[0] = sesskey->symm_alg;
for (i = 0; i < cipherinfo->keysize ; i++) {
m_buf[1 + i] = sesskey->key[i];
}
return pgp_calc_sesskey_checksum(sesskey,
m_buf + 1 + cipherinfo->keysize);
}
unsigned
encode_m_buf(const uint8_t *M, size_t mLen, const pgp_pubkey_t * pubkey,
uint8_t *EM)
{
unsigned    k;
unsigned        i;
switch (pubkey->alg) {
case PGP_PKA_RSA:
k = (unsigned)BN_num_bytes(pubkey->key.rsa.n);
if (mLen > k - 11) {
(void) fprintf(stderr, "encode_m_buf: message too long\n");
return 0;
}
break;
case PGP_PKA_DSA:
case PGP_PKA_ELGAMAL:
k = (unsigned)BN_num_bytes(pubkey->key.elgamal.p);
if (mLen > k - 11) {
(void) fprintf(stderr, "encode_m_buf: message too long\n");
return 0;
}
break;
default:
(void) fprintf(stderr, "encode_m_buf: pubkey algorithm\n");
return 0;
}
EM[0] = 0x00;
EM[1] = 0x02;
for (i = 2; i < (k - mLen) - 1; ++i) {
do {
pgp_random(EM + i, 1);
} while (EM[i] == 0);
}
if (i < 8 + 2) {
(void) fprintf(stderr, "encode_m_buf: bad i len\n");
return 0;
}
EM[i++] = 0;
(void) memcpy(EM + i, M, mLen);
if (pgp_get_debug_level(__FILE__)) {
hexdump(stderr, "Encoded Message:", EM, mLen);
}
return 1;
}
pgp_pk_sesskey_t *
pgp_create_pk_sesskey(pgp_key_t *key, const char *ciphername, const pgp_pk_sesskey_t *initial_sesskey)
{
pgp_pubkey_t	*pubkey;
pgp_pk_sesskey_t	*sesskey;
pgp_symm_alg_t	 cipher;
const uint8_t		*id;
pgp_crypt_t		 cipherinfo;
uint8_t			*unencoded_m_buf;
uint8_t			*encoded_m_buf;
size_t			 sz_encoded_m_buf;
pubkey = pgp_key_get_enckey(key, &id);
if( pubkey == NULL ) {
return NULL;
}
(void) memset(&cipherinfo, 0x0, sizeof(cipherinfo));
if( !pgp_crypt_any(&cipherinfo, cipher = pgp_str_to_cipher((ciphername) ? ciphername : "cast5")) ) {
return NULL;
}
unencoded_m_buf = calloc(1, cipherinfo.keysize + 1 + 2);
if (unencoded_m_buf == NULL) {
(void) fprintf(stderr,
"pgp_create_pk_sesskey: can't allocate\n");
return NULL;
}
switch(pubkey->alg) {
case PGP_PKA_RSA:
sz_encoded_m_buf = BN_num_bytes(pubkey->key.rsa.n);
break;
case PGP_PKA_DSA:
case PGP_PKA_ELGAMAL:
sz_encoded_m_buf = BN_num_bytes(pubkey->key.elgamal.p);
break;
default:
sz_encoded_m_buf = 0;
break;
}
if ((encoded_m_buf = calloc(1, sz_encoded_m_buf)) == NULL) {
(void) fprintf(stderr,
"pgp_create_pk_sesskey: can't allocate\n");
free(unencoded_m_buf);
return NULL;
}
if ((sesskey = calloc(1, sizeof(*sesskey))) == NULL) {
(void) fprintf(stderr,
"pgp_create_pk_sesskey: can't allocate\n");
free(unencoded_m_buf);
free(encoded_m_buf);
return NULL;
}
if (key->type != PGP_PTAG_CT_PUBLIC_KEY) {
(void) fprintf(stderr,
"pgp_create_pk_sesskey: bad type\n");
free(unencoded_m_buf);
free(encoded_m_buf);
free(sesskey);
return NULL;
}
sesskey->version = PGP_PKSK_V3;
(void) memcpy(sesskey->key_id, id, sizeof(sesskey->key_id));
if (pgp_get_debug_level(__FILE__)) {
hexdump(stderr, "Encrypting for keyid", id, sizeof(sesskey->key_id));
}
switch (pubkey->alg) {
case PGP_PKA_RSA:
case PGP_PKA_DSA:
case PGP_PKA_ELGAMAL:
break;
default:
(void) fprintf(stderr,
"pgp_create_pk_sesskey: bad pubkey algorithm\n");
free(unencoded_m_buf);
free(encoded_m_buf);
free(sesskey);
return NULL;
}
sesskey->alg = pubkey->alg;
sesskey->symm_alg = cipher;
if(initial_sesskey){
if(initial_sesskey->symm_alg != cipher){
free(unencoded_m_buf);
free(encoded_m_buf);
free(sesskey);
return NULL;
}
memcpy(sesskey->key, initial_sesskey->key, cipherinfo.keysize);
}else{
pgp_random(sesskey->key, cipherinfo.keysize);
}
if (pgp_get_debug_level(__FILE__)) {
hexdump(stderr, "sesskey created", sesskey->key,
cipherinfo.keysize + 1 + 2);
}
if (create_unencoded_m_buf(sesskey, &cipherinfo, &unencoded_m_buf[0]) == 0) {
free(unencoded_m_buf);
free(encoded_m_buf);
free(sesskey);
return NULL;
}
if (pgp_get_debug_level(__FILE__)) {
hexdump(stderr, "uuencoded m buf", unencoded_m_buf, cipherinfo.keysize + 1 + 2);
}
encode_m_buf(unencoded_m_buf, cipherinfo.keysize + 1 + 2, pubkey, encoded_m_buf);
switch (key->key.pubkey.alg) {
case PGP_PKA_RSA:
if (!pgp_rsa_encrypt_mpi(encoded_m_buf, sz_encoded_m_buf, pubkey,
&sesskey->params)) {
free(unencoded_m_buf);
free(encoded_m_buf);
free(sesskey);
return NULL;
}
break;
case PGP_PKA_DSA:
case PGP_PKA_ELGAMAL:
if (!pgp_elgamal_encrypt_mpi(encoded_m_buf, sz_encoded_m_buf, pubkey,
&sesskey->params)) {
free(unencoded_m_buf);
free(encoded_m_buf);
free(sesskey);
return NULL;
}
break;
default:
break;
}
free(unencoded_m_buf);
free(encoded_m_buf);
return sesskey;
}
unsigned
pgp_write_pk_sesskey(pgp_output_t *output, pgp_pk_sesskey_t *pksk)
{
if (pksk == NULL) {
(void) fprintf(stderr,
"pgp_write_pk_sesskey: NULL pksk\n");
return 0;
}
switch (pksk->alg) {
case PGP_PKA_RSA:
return pgp_write_ptag(output, PGP_PTAG_CT_PK_SESSION_KEY) &&
pgp_write_length(output, (unsigned)(1 + 8 + 1 +
BN_num_bytes(pksk->params.rsa.encrypted_m) + 2)) &&
pgp_write_scalar(output, (unsigned)pksk->version, 1) &&
pgp_write(output, pksk->key_id, 8) &&
pgp_write_scalar(output, (unsigned)pksk->alg, 1) &&
pgp_write_mpi(output, pksk->params.rsa.encrypted_m)
;
case PGP_PKA_DSA:
case PGP_PKA_ELGAMAL:
return pgp_write_ptag(output, PGP_PTAG_CT_PK_SESSION_KEY) &&
pgp_write_length(output, (unsigned)(1 + 8 + 1 +
BN_num_bytes(pksk->params.elgamal.g_to_k) + 2 +
BN_num_bytes(pksk->params.elgamal.encrypted_m) + 2)) &&
pgp_write_scalar(output, (unsigned)pksk->version, 1) &&
pgp_write(output, pksk->key_id, 8) &&
pgp_write_scalar(output, (unsigned)pksk->alg, 1) &&
pgp_write_mpi(output, pksk->params.elgamal.g_to_k) &&
pgp_write_mpi(output, pksk->params.elgamal.encrypted_m)
;
default:
(void) fprintf(stderr,
"pgp_write_pk_sesskey: bad algorithm\n");
return 0;
}
}
unsigned
pgp_write_mdc(pgp_output_t *output, const uint8_t *hashed)
{
return pgp_write_ptag(output, PGP_PTAG_CT_MDC) &&
pgp_write_length(output, PGP_SHA1_HASH_SIZE) &&
pgp_write(output, hashed, PGP_SHA1_HASH_SIZE);
}
unsigned
pgp_write_litdata(pgp_output_t *output,
const uint8_t *data,
const int maxlen,
const pgp_litdata_enum type)
{
return pgp_write_ptag(output, PGP_PTAG_CT_LITDATA) &&
pgp_write_length(output, (unsigned)(1 + 1 + 4 + maxlen)) &&
pgp_write_scalar(output, (unsigned)type, 1) &&
pgp_write_scalar(output, 0, 1) &&
pgp_write_scalar(output, 0, 4) &&
pgp_write(output, data, (unsigned)maxlen);
}
#if 0
unsigned
pgp_fileread_litdata(const char *filename,
const pgp_litdata_enum type,
pgp_output_t *output)
{
pgp_memory_t	*mem;
unsigned   	 ret;
int		 len;
mem = pgp_memory_new();
if (!pgp_mem_readfile(mem, filename)) {
(void) fprintf(stderr, "pgp_mem_readfile of '%s' failed\n", filename);
return 0;
}
len = (int)pgp_mem_len(mem);
ret = pgp_write_litdata(output, pgp_mem_data(mem), len, type);
pgp_memory_free(mem);
return ret;
}
#endif
int
pgp_filewrite(const char *filename, const char *buf,
const size_t len, const unsigned overwrite)
{
int		flags;
int		fd;
flags = O_WRONLY | O_CREAT;
if (overwrite) {
flags |= O_TRUNC;
} else {
flags |= O_EXCL;
}
#ifdef O_BINARY
flags |= O_BINARY;
#endif
fd = open(filename, flags, 0600);
if (fd < 0) {
(void) fprintf(stderr, "can't open '%s'\n", filename);
return 0;
}
if (write(fd, buf, len) != (int)len) {
(void) close(fd);
return 0;
}
return (close(fd) == 0);
}
#if 0
unsigned
pgp_write_symm_enc_data(const uint8_t *data,
const int len,
pgp_symm_alg_t alg,
const uint8_t* key,
pgp_output_t * output)
{
pgp_crypt_t	crypt_info;
uint8_t		*encrypted = (uint8_t *) NULL;
size_t		encrypted_sz;
int             done = 0;
if( !pgp_crypt_any(&crypt_info, alg) ) {
return 0;
}
crypt_info.set_crypt_key(&crypt_info, key);
pgp_encrypt_init(&crypt_info);
encrypted_sz = (size_t)(len + crypt_info.blocksize + 2);
if ((encrypted = calloc(1, encrypted_sz)) == NULL) {
(void) fprintf(stderr, "can't allocate %" PRIsize "d\n",
encrypted_sz);
return 0;
}
done = (int)pgp_encrypt_se(&crypt_info, encrypted, data, (unsigned)len);
if (done != len) {
(void) fprintf(stderr,
"pgp_write_symm_enc_data: done != len\n");
return 0;
}
return pgp_write_ptag(output, PGP_PTAG_CT_SE_DATA) &&
pgp_write_length(output, (unsigned)(encrypted_sz)) &&
pgp_write(output, encrypted, (unsigned)encrypted_sz);
}
#endif
unsigned
pgp_write_one_pass_sig(pgp_output_t *output,
const pgp_seckey_t *seckey,
const pgp_hash_alg_t hash_alg,
const pgp_sig_type_t sig_type)
{
uint8_t   keyid[PGP_KEY_ID_SIZE];
pgp_keyid(keyid, PGP_KEY_ID_SIZE, &seckey->pubkey, PGP_HASH_SHA1);
return pgp_write_ptag(output, PGP_PTAG_CT_1_PASS_SIG) &&
pgp_write_length(output, 1 + 1 + 1 + 1 + 8 + 1) &&
pgp_write_scalar(output, 3, 1)	 &&
pgp_write_scalar(output, (unsigned)sig_type, 1) &&
pgp_write_scalar(output, (unsigned)hash_alg, 1) &&
pgp_write_scalar(output, (unsigned)seckey->pubkey.alg, 1) &&
pgp_write(output, keyid, 8) &&
pgp_write_scalar(output, 1, 1);
}