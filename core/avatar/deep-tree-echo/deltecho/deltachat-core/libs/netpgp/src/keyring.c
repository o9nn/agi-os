#include "netpgp/config-netpgp.h"
#ifdef HAVE_SYS_CDEFS_H
#include <sys/cdefs.h>
#endif
#if defined(__NetBSD__)
__COPYRIGHT("@(#) Copyright (c) 2009 The NetBSD Foundation, Inc. All rights reserved.");
__RCSID("$NetBSD$");
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#include <regex.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_TERMIOS_H
#include <termios.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include "netpgp/types.h"
#include "netpgp/keyring.h"
#include "netpgp/packet-parse.h"
#include "netpgp/signature.h"
#include "netpgp/netpgpsdk.h"
#include "netpgp/readerwriter.h"
#include "netpgp/netpgpdefs.h"
#include "netpgp/packet.h"
#include "netpgp/crypto.h"
#include "netpgp/validate.h"
#include "netpgp/netpgpdefs.h"
#include "netpgp/netpgpdigest.h"
pgp_key_t *
pgp_keydata_new(void)
{
return calloc(1, sizeof(pgp_key_t));
}
void
pgp_key_free(pgp_key_t *key)
{
unsigned n;
if (key==NULL) {
return;
}
if (key->type == PGP_PTAG_CT_PUBLIC_KEY) {
pgp_pubkey_free(&key->key.pubkey);
} else {
pgp_seckey_free(&key->key.seckey);
}
for (n = 0; n < key->directsigc; ++n) {
pgp_free_sig_info(&key->directsigs[n].siginfo);
pgp_subpacket_free(&key->directsigs[n].packet);
}
FREE_ARRAY(key, directsig);
for (n = 0; n < key->uidc; ++n) {
pgp_userid_free(&key->uids[n]);
}
FREE_ARRAY(key, uid);
for (n = 0; n < key->uidsigc; ++n) {
pgp_free_sig_info(&key->uidsigs[n].siginfo);
pgp_subpacket_free(&key->uidsigs[n].packet);
}
FREE_ARRAY(key, uidsig);
for (n = 0; n < key->subkeyc; ++n) {
if (key->type == PGP_PTAG_CT_PUBLIC_KEY) {
pgp_pubkey_free(&key->subkeys[n].key.pubkey);
} else {
pgp_seckey_free(&key->subkeys[n].key.seckey);
}
}
FREE_ARRAY(key, subkey);
for (n = 0; n < key->subkeysigc; ++n) {
pgp_free_sig_info(&key->subkeysigs[n].siginfo);
pgp_subpacket_free(&key->subkeysigs[n].packet);
}
FREE_ARRAY(key, subkeysig);
}
void
pgp_keydata_free(pgp_key_t *keydata)
{
pgp_key_free(keydata);
free(keydata);
}
static unsigned siginfo_in_time(pgp_sig_info_t *siginfo){
time_t now;
now = time(NULL);
return now >= siginfo->birthtime && (
siginfo->key_expiry == 0 ||
now < siginfo->birthtime +
siginfo->key_expiry);
}
const int32_t
pgp_key_find_uid_cond(
const pgp_key_t *key,
unsigned(*uidcond) ( uint8_t *, void *),
void *uidcondarg,
unsigned(*sigcond) ( const pgp_sig_info_t *, void *),
void *sigcondarg,
time_t *youngest,
unsigned checkrevoke,
unsigned checkexpiry)
{
unsigned uididx = 0;
unsigned uidsigidx = 0;
int32_t res = -1;
int32_t lastgood;
uint8_t **uidp;
pgp_uidsig_t *uidsigp;
time_t yngst = 0;
if(!youngest)
youngest = &yngst;
uidp = key->uids;
for (uididx = 0 ; uididx < key->uidc; uididx++, uidp++)
{
if(uidcond && !uidcond(*uidp, uidcondarg)) continue;
lastgood = res;
uidsigp = key->uidsigs;
for (uidsigidx = 0 ; uidsigidx < key->uidsigc; uidsigidx++, uidsigp++)
{
if(uidsigp->uid == uididx)
{
if(uidsigp->siginfo.type == PGP_SIG_REV_CERT)
{
if(!checkrevoke)
continue;
res = lastgood;
break;
}
if(!checkexpiry || siginfo_in_time(&uidsigp->siginfo))
{
if(!sigcond || sigcond(&uidsigp->siginfo, sigcondarg))
{
if(uidsigp->siginfo.birthtime > *youngest)
{
*youngest = uidsigp->siginfo.birthtime;
res = uididx;
}
}
}
}
}
}
return res;
}
const int32_t
pgp_key_find_key_conds(
pgp_key_t *key,
unsigned(*keycond) ( const pgp_pubkey_t *, const uint8_t *, void*),
void *keycondarg,
unsigned(*sigcond) ( const pgp_sig_info_t *, void*),
void *sigcondarg,
unsigned checkrevoke,
unsigned checkexpiry)
{
unsigned subkeyidx = 0;
unsigned subkeysigidx = 0;
unsigned directsigidx = 0;
int32_t res = -2;
int32_t lastgood;
pgp_subkey_t *subkeyp;
pgp_subkeysig_t *subkeysigp;
pgp_directsig_t *directsigp;
time_t youngest;
youngest = 0;
if(!keycond || keycond(pgp_key_get_pubkey(key),
key->pubkeyid, keycondarg)){
int32_t uidres;
directsigp = key->directsigs;
for (directsigidx = 0 ; directsigidx < key->directsigc;
directsigidx++, directsigp++)
{
if(directsigp->siginfo.type == PGP_SIG_REV_KEY)
{
if(!checkrevoke)
continue;
return -2;
}
if(!checkexpiry || siginfo_in_time(&directsigp->siginfo))
{
if(!sigcond || sigcond(&directsigp->siginfo, sigcondarg))
{
if(directsigp->siginfo.birthtime > youngest)
{
youngest = directsigp->siginfo.birthtime;
res = -1;
}
}
}
}
uidres = pgp_key_find_uid_cond(
key, NULL, NULL, sigcond, sigcondarg, &youngest,
checkrevoke, checkexpiry);
if(uidres != -1){
res = -1;
}
}
subkeyp = key->subkeys;
for (subkeyidx = 0 ; subkeyidx < key->subkeyc; subkeyidx++, subkeyp++)
{
lastgood = res;
subkeysigp = key->subkeysigs;
if(keycond && !keycond(&subkeyp->key.pubkey, subkeyp->id, keycondarg))
continue;
for (subkeysigidx = 0 ; subkeysigidx < key->subkeysigc;
subkeysigidx++, subkeysigp++)
{
if(subkeysigp->subkey == subkeyidx)
{
if(subkeysigp->siginfo.type == PGP_SIG_REV_SUBKEY)
{
if(!checkrevoke)
continue;
res = lastgood;
break;
}
if(!checkexpiry || siginfo_in_time(&subkeysigp->siginfo))
{
if(!sigcond || sigcond(&subkeysigp->siginfo, sigcondarg))
{
if(subkeysigp->siginfo.birthtime > youngest)
{
youngest = subkeysigp->siginfo.birthtime;
res = subkeyidx;
}
}
}
}
}
}
return res;
}
pgp_pubkey_t *
pgp_key_get_pubkey(pgp_key_t *keydata)
{
return (keydata->type == PGP_PTAG_CT_PUBLIC_KEY) ?
&keydata->key.pubkey :
&keydata->key.seckey.pubkey;
}
pgp_pubkey_t *
pgp_key_get_subpubkey(pgp_key_t *key, int32_t subkeyidx)
{
return (key->type == PGP_PTAG_CT_PUBLIC_KEY) ?
&key->subkeys[subkeyidx].key.pubkey :
&key->subkeys[subkeyidx].key.seckey.pubkey;
}
pgp_seckey_t *
pgp_key_get_subseckey(pgp_key_t *key, int32_t subkeyidx)
{
return (key->type == PGP_PTAG_CT_SECRET_KEY) ?
&key->subkeys[subkeyidx].key.seckey :
NULL;
}
static pgp_pubkey_t *
key_get_pubkey_from_subidx(
pgp_key_t *key,
const uint8_t **id,
int32_t subkeyidx)
{
if(subkeyidx == -2){
return NULL;
}
if(subkeyidx != -1)
{
if(id)
*id = key->subkeys[subkeyidx].id;
return pgp_key_get_subpubkey(key, subkeyidx);
}
if(id)
*id = key->pubkeyid;
return pgp_key_get_pubkey(key);
}
static pgp_seckey_t *
key_get_seckey_from_subidx(
pgp_key_t *key,
const uint8_t **id,
int32_t subkeyidx)
{
if(subkeyidx == -2){
return NULL;
}
if(subkeyidx != -1)
{
if(id)
*id = key->subkeys[subkeyidx].id;
return pgp_key_get_subseckey(key, subkeyidx);
}
if(id)
*id = key->pubkeyid;
return pgp_get_seckey(key);
}
static unsigned is_signing_role(const pgp_sig_info_t *siginfo, void *arg)
{
return siginfo->key_flags & PGP_KEYFLAG_SIGN_DATA;
}
pgp_pubkey_t *
pgp_key_get_sigkey(pgp_key_t *key)
{
int32_t subkeyidx =
pgp_key_find_key_conds(key, NULL, NULL, &is_signing_role, NULL, 0, 0);
return key_get_pubkey_from_subidx(key, NULL, subkeyidx);
}
#if 0
pgp_seckey_t *
pgp_key_get_certkey(pgp_key_t *key)
{
int32_t subkeyidx =
pgp_key_find_key_conds(key, NULL, NULL, &is_signing_role, NULL, 1, 0);
return key_get_seckey_from_subidx(key, NULL, subkeyidx);
}
#endif
static unsigned is_encryption_role(const pgp_sig_info_t *siginfo, void *arg)
{
return siginfo->key_flags & PGP_KEYFLAG_ENC_COMM;
}
pgp_pubkey_t *
pgp_key_get_enckey(pgp_key_t *key, const uint8_t **id)
{
int32_t subkeyidx =
pgp_key_find_key_conds(key, NULL, NULL, &is_encryption_role, NULL, 1, 0);
return key_get_pubkey_from_subidx(key, id, subkeyidx);
}
pgp_seckey_t *
pgp_key_get_deckey(pgp_key_t *key, const uint8_t **id)
{
int32_t subkeyidx =
pgp_key_find_key_conds(key, NULL, NULL, &is_encryption_role, NULL, 0, 0);
return key_get_seckey_from_subidx(key, id, subkeyidx);
}
static unsigned primary_uid_sigcond(const pgp_sig_info_t *siginfo, void *arg)
{
return siginfo->primary_userid;
}
const int32_t pgp_key_get_uid0(pgp_key_t *key)
{
int32_t res =
pgp_key_find_uid_cond(key, NULL, NULL, &primary_uid_sigcond, NULL, NULL, 1, 0);
return res == -1 ?
pgp_key_find_uid_cond(key, NULL, NULL, NULL, NULL, NULL, 1, 0):
res;
}
const uint8_t *pgp_key_get_primary_userid(pgp_key_t *key)
{
const int32_t uid0 = pgp_key_get_uid0(key);
if( uid0 >= 0 && key->uids && key->uidc > uid0)
{
return key->uids[uid0];
}
return NULL;
}
unsigned key_bit_len(const pgp_pubkey_t *key)
{
switch (key->alg) {
case PGP_PKA_DSA:
return BN_num_bits(key->key.dsa.p);
case PGP_PKA_RSA:
return BN_num_bits(key->key.rsa.n);
case PGP_PKA_ELGAMAL:
return BN_num_bits(key->key.elgamal.p);
default:
return 0;
}
}
unsigned key_is_weak(
const pgp_pubkey_t *key,
const uint8_t *keyid,
void *arg)
{
unsigned kbl;
pgp_key_rating_t *res;
res = (pgp_key_rating_t*)arg;
kbl = key_bit_len(key);
if(kbl == 0)
{
*res = PGP_INVALID;
}
else if(kbl < 1024)
{
*res = PGP_TOOSHORT;
}
else if(kbl == 1024 && key->alg == PGP_PKA_RSA)
{
*res = PGP_WEAK;
}
return 0;
}
const pgp_key_rating_t pgp_key_get_rating(pgp_key_t *key)
{
pgp_key_rating_t res = PGP_VALID;
pgp_key_find_key_conds(key, &key_is_weak, (void*)&res, NULL, NULL, 0, 0);
if(res == PGP_VALID)
{
if(pgp_key_find_key_conds(
key, NULL, NULL, NULL, NULL, 1, 0) == -2)
{
return PGP_REVOKED;
}
if(pgp_key_find_key_conds(
key, NULL, NULL, NULL, NULL, 0, 1) == -2)
{
return PGP_EXPIRED;
}
}
return res;
}
unsigned
pgp_is_key_secret(pgp_key_t *data)
{
return data->type != PGP_PTAG_CT_PUBLIC_KEY;
}
pgp_seckey_t *
pgp_get_seckey(pgp_key_t *data)
{
return (data->type == PGP_PTAG_CT_SECRET_KEY) ?
&data->key.seckey : NULL;
}
pgp_seckey_t *
pgp_get_writable_seckey(pgp_key_t *data)
{
return (data->type == PGP_PTAG_CT_SECRET_KEY) ?
&data->key.seckey : NULL;
}
void
pgp_forget(void *vp, unsigned size)
{
(void) memset(vp, 0x0, size);
}
typedef struct {
FILE *passfp;
const pgp_key_t *key;
char *passphrase;
pgp_seckey_t *seckey;
} decrypt_t;
uint8_t *
pgp_copy_userid(uint8_t **dst, const uint8_t *src)
{
size_t len;
len = strlen((const char *) src);
if (*dst) {
free(*dst);
}
if ((*dst = calloc(1, len + 1)) == NULL) {
(void) fprintf(stderr, "pgp_copy_userid: bad alloc\n");
} else {
(void) memcpy(*dst, src, len);
}
return *dst;
}
pgp_subpacket_t *
pgp_copy_packet(pgp_subpacket_t *dst, const pgp_subpacket_t *src)
{
if (dst->raw) {
free(dst->raw);
}
if ((dst->raw = calloc(1, src->length)) == NULL) {
(void) fprintf(stderr, "pgp_copy_packet: bad alloc\n");
} else {
dst->length = src->length;
(void) memcpy(dst->raw, src->raw, src->length);
}
return dst;
}
#if 0
uint8_t *
pgp_add_userid(pgp_key_t *key, const uint8_t *userid)
{
uint8_t **uidp;
EXPAND_ARRAY(key, uid);
uidp = &key->uids[key->uidc++];
*uidp = NULL;
return pgp_copy_userid(uidp, userid);
}
#endif
void print_packet_hex(const pgp_subpacket_t *pkt);
#if 0
unsigned
pgp_add_selfsigned_userid(pgp_key_t *skey, pgp_key_t *pkey, const uint8_t *userid, time_t key_expiry)
{
pgp_create_sig_t *sig;
pgp_subpacket_t sigpacket;
pgp_memory_t *mem_sig = NULL;
pgp_output_t *sigoutput = NULL;
sig = pgp_create_sig_new();
pgp_sig_start_key_sig(sig, &skey->key.seckey.pubkey, userid, PGP_CERT_POSITIVE);
pgp_add_creation_time(sig, time(NULL));
pgp_add_key_expiration_time(sig, key_expiry);
pgp_add_issuer_keyid(sig, skey->pubkeyid);
pgp_add_primary_userid(sig, 1);
pgp_add_key_flags(sig, PGP_KEYFLAG_SIGN_DATA|PGP_KEYFLAG_ENC_COMM);
pgp_add_key_prefs(sig);
pgp_add_key_features(sig);
pgp_end_hashed_subpkts(sig);
pgp_setup_memory_write(&sigoutput, &mem_sig, 128);
pgp_write_sig(sigoutput, sig, &skey->key.seckey.pubkey, &skey->key.seckey);
sigpacket.length = pgp_mem_len(mem_sig);
sigpacket.raw = pgp_mem_data(mem_sig);
pgp_update_userid(skey, userid, &sigpacket, &sig->sig.info);
if(pkey)
pgp_update_userid(pkey, userid, &sigpacket, &sig->sig.info);
pgp_create_sig_delete(sig);
pgp_output_delete(sigoutput);
pgp_memory_free(mem_sig);
return 1;
}
#endif
#if 0
unsigned
pgp_key_revoke(pgp_key_t *skey, pgp_key_t *pkey, uint8_t code, const char *reason)
{
pgp_create_sig_t *sig;
pgp_subpacket_t sigpacket;
pgp_memory_t *mem_sig = NULL;
pgp_output_t *sigoutput = NULL;
sig = pgp_create_sig_new();
pgp_sig_start_key_rev(
sig, &skey->key.seckey.pubkey,
PGP_SIG_REV_KEY);
pgp_add_creation_time(sig, time(NULL));
pgp_add_issuer_keyid(sig, skey->pubkeyid);
pgp_add_revocation_reason(sig, code, reason);
pgp_end_hashed_subpkts(sig);
pgp_setup_memory_write(&sigoutput, &mem_sig, 128);
pgp_write_sig(sigoutput, sig, &skey->key.seckey.pubkey, &skey->key.seckey);
sigpacket.length = pgp_mem_len(mem_sig);
sigpacket.raw = pgp_mem_data(mem_sig);
pgp_add_directsig(skey, &sigpacket, &sig->sig.info);
pgp_add_directsig(pkey, &sigpacket, &sig->sig.info);
pgp_create_sig_delete(sig);
pgp_output_delete(sigoutput);
pgp_memory_free(mem_sig);
return 1;
}
#endif
void
pgp_keydata_init(pgp_key_t *keydata, const pgp_content_enum type)
{
if (keydata->type != PGP_PTAG_CT_RESERVED) {
(void) fprintf(stderr,
"pgp_keydata_init: wrong keydata type\n");
} else if (type != PGP_PTAG_CT_PUBLIC_KEY &&
type != PGP_PTAG_CT_SECRET_KEY) {
(void) fprintf(stderr, "pgp_keydata_init: wrong type\n");
} else {
keydata->type = type;
}
}
#if 0
unsigned
pgp_keyring_fileread(pgp_io_t *io,
pgp_keyring_t *pubring,
pgp_keyring_t *secring,
const unsigned armour,
const char *filename)
{
return pgp_filter_keys_fileread(
io,
pubring,
secring,
NULL ,
armour,
filename);
}
#endif
#if 0
unsigned
pgp_keyring_read_from_mem(pgp_io_t *io,
pgp_keyring_t *pubring,
pgp_keyring_t *secring,
const unsigned armour,
pgp_memory_t *mem)
{
return pgp_filter_keys_from_mem(io,
pubring,
secring,
NULL ,
armour,
mem);
}
#endif
void
pgp_keyring_free(pgp_keyring_t *keyring)
{
(void)free(keyring->keys);
keyring->keys = NULL;
keyring->keyc = keyring->keyvsize = 0;
}
void
pgp_keyring_purge(pgp_keyring_t *keyring)
{
pgp_key_t *keyp;
unsigned c = 0;
for (keyp = keyring->keys; c < keyring->keyc; c++, keyp++) {
pgp_key_free(keyp);
}
pgp_keyring_free(keyring);
}
static unsigned
deletekey( pgp_keyring_t *keyring, pgp_key_t *key, unsigned from)
{
pgp_key_free(key);
keyring->keyc--;
for ( ; keyring && from < keyring->keyc; from += 1) {
memcpy(&keyring->keys[from], &keyring->keys[from+1],
sizeof(pgp_key_t));
}
return 1;
}
unsigned key_id_match(const pgp_pubkey_t *key, const uint8_t *keyid, void *refidarg)
{
uint8_t *refid = refidarg;
return (memcmp(keyid, refid, PGP_KEY_ID_SIZE) == 0);
}
pgp_key_t *
pgp_getkeybyid(pgp_io_t *io, const pgp_keyring_t *keyring,
const uint8_t *keyid, unsigned *from,
pgp_pubkey_t **pubkey,
pgp_seckey_t **seckey,
unsigned checkrevoke,
unsigned checkexpiry)
{
uint8_t nullid[PGP_KEY_ID_SIZE];
(void) memset(nullid, 0x0, sizeof(nullid));
for ( ; keyring && *from < keyring->keyc; *from += 1) {
pgp_key_t *key = &keyring->keys[*from];
int32_t subkeyidx;
if (pgp_get_debug_level(__FILE__)) {
hexdump(io->errs, "keyring keyid", key->pubkeyid, PGP_KEY_ID_SIZE);
hexdump(io->errs, "keyid", keyid, PGP_KEY_ID_SIZE);
}
subkeyidx = pgp_key_find_key_conds(key, &key_id_match,
(void*)keyid, NULL, NULL,
checkrevoke, checkexpiry);
if (subkeyidx != -2) {
if (pubkey) {
*pubkey = key_get_pubkey_from_subidx(key, NULL, subkeyidx);
}
if (seckey) {
*seckey = key_get_seckey_from_subidx(key, NULL, subkeyidx);
}
return key;
}
}
return NULL;
}
unsigned
pgp_deletekeybyid(pgp_io_t *io, pgp_keyring_t *keyring,
const uint8_t *keyid)
{
unsigned from = 0;
pgp_key_t *key;
if ((key = (pgp_key_t *)pgp_getkeybyid(io, keyring, keyid,
&from, NULL, NULL, 0, 0)) == NULL) {
return 0;
}
deletekey(keyring, key, from);
return 1;
}
pgp_key_t *
pgp_getkeybyfpr(pgp_io_t *io, const pgp_keyring_t *keyring,
const uint8_t *fpr, size_t length,
unsigned *from,
pgp_pubkey_t **pubkey,
unsigned checkrevoke,
unsigned checkexpiry)
{
for ( ; keyring && *from < keyring->keyc; *from += 1) {
pgp_key_t *key = &keyring->keys[*from];
pgp_fingerprint_t *kfp = &key->pubkeyfpr;
if (kfp->length == length &&
memcmp(kfp->fingerprint, fpr, length) == 0) {
if(checkrevoke || checkexpiry){
int32_t subkeyidx;
subkeyidx = pgp_key_find_key_conds(key,
NULL, NULL,
NULL, NULL,
checkrevoke, checkexpiry);
if (subkeyidx == -2) return NULL;
}
if (pubkey) {
*pubkey = &key->key.pubkey;
}
return key;
}
}
return NULL;
}
unsigned
pgp_deletekeybyfpr(pgp_io_t *io, pgp_keyring_t *keyring,
const uint8_t *fpr, size_t length)
{
unsigned from = 0;
pgp_key_t *key;
if ((key = (pgp_key_t *)pgp_getkeybyfpr(io, keyring, fpr, length,
&from, NULL,0,0)) == NULL) {
return 0;
}
deletekey(keyring, key, from);
return 1;
}
#if 0
static void
str2keyid(const char *userid, uint8_t *keyid, size_t len)
{
static const char *uppers = "0123456789ABCDEF";
static const char *lowers = "0123456789abcdef";
const char *hi;
const char *lo;
uint8_t hichar;
uint8_t lochar;
size_t j;
int i;
for (i = 0, j = 0 ; j < len && userid[i] && userid[i + 1] ; i += 2, j++) {
if ((hi = strchr(uppers, userid[i])) == NULL) {
if ((hi = strchr(lowers, userid[i])) == NULL) {
break;
}
hichar = (uint8_t)(hi - lowers);
} else {
hichar = (uint8_t)(hi - uppers);
}
if ((lo = strchr(uppers, userid[i + 1])) == NULL) {
if ((lo = strchr(lowers, userid[i + 1])) == NULL) {
break;
}
lochar = (uint8_t)(lo - lowers);
} else {
lochar = (uint8_t)(lo - uppers);
}
keyid[j] = (hichar << 4) | (lochar);
}
keyid[j] = 0x0;
}
#endif
#if 0
static const pgp_key_t *
getkeybyname(pgp_io_t *io,
const pgp_keyring_t *keyring,
const char *name,
unsigned *from)
{
uint8_t **uidp;
unsigned i = 0;
pgp_key_t *keyp;
regex_t r;
size_t len;
if (!keyring || !name || !from) {
return NULL;
}
len = strlen(name);
if (pgp_get_debug_level(__FILE__)) {
(void) fprintf(io->outs, "[%u] name '%s', len %zu\n",
*from, name, len);
}
if (pgp_get_debug_level(__FILE__) && name != NULL) {
(void) fprintf(io->outs, "regex match '%s' from %u\n",
name, *from);
}
if (name != NULL) {
(void) regcomp(&r, name, REG_NOSUB | REG_LITERAL | REG_ICASE);
}
if(keyring->keys != NULL)
for (keyp = &keyring->keys[*from]; *from < keyring->keyc; *from += 1, keyp++) {
uidp = keyp->uids;
if (name == NULL) {
return keyp;
} else {
for (i = 0 ; i < keyp->uidc; i++, uidp++) {
if (regexec(&r, (char *)*uidp, 0, NULL, 0) == 0) {
if (pgp_get_debug_level(__FILE__)) {
(void) fprintf(io->outs,
"MATCHED keyid \"%s\" len %" PRIsize "u\n",
(char *) *uidp, len);
}
regfree(&r);
return keyp;
}
}
}
}
regfree(&r);
return NULL;
}
#endif
#if 0
const pgp_key_t *
pgp_getkeybyname(pgp_io_t *io,
const pgp_keyring_t *keyring,
const char *name)
{
unsigned from;
from = 0;
return getkeybyname(io, keyring, name, &from);
}
#endif
#if 0
const pgp_key_t *
pgp_getnextkeybyname(pgp_io_t *io,
const pgp_keyring_t *keyring,
const char *name,
unsigned *n)
{
return getkeybyname(io, keyring, name, n);
}
#endif
#if 0
char *
pgp_export_key(pgp_io_t *io, const pgp_key_t *keydata, uint8_t *passphrase)
{
pgp_output_t *output;
pgp_memory_t *mem;
char *cp;
__PGP_USED(io);
pgp_setup_memory_write(&output, &mem, 128);
pgp_write_xfer_key(output, keydata, 1);
cp = netpgp_strdup(pgp_mem_data(mem));
pgp_teardown_memory_write(output, mem);
return cp;
}
#endif
int
pgp_keyring_add(pgp_keyring_t *dst, const pgp_key_t *src)
{
pgp_key_t *key;
EXPAND_ARRAY(dst, key);
key = &dst->keys[dst->keyc++];
memcpy(key, src, sizeof(*key));
return 1;
}
pgp_key_t *pgp_ensure_pubkey(
pgp_keyring_t *keyring,
pgp_pubkey_t *pubkey,
uint8_t *pubkeyid)
{
pgp_key_t *key;
unsigned c;
if(keyring == NULL) return NULL;
for (c = 0; c < keyring->keyc; c += 1) {
if (memcmp(keyring->keys[c].pubkeyid,
pubkeyid, PGP_KEY_ID_SIZE) == 0) {
return &keyring->keys[c];
}
}
EXPAND_ARRAY(keyring, key);
key = &keyring->keys[keyring->keyc++];
(void) memset(key, 0x0, sizeof(*key));
key->type = PGP_PTAG_CT_PUBLIC_KEY;
pgp_pubkey_dup(&key->key.pubkey, pubkey);
(void) memcpy(&key->pubkeyid, pubkeyid, PGP_KEY_ID_SIZE);
pgp_fingerprint(&key->pubkeyfpr, pubkey, keyring->hashtype);
return key;
}
pgp_key_t *pgp_ensure_seckey(
pgp_keyring_t *keyring,
pgp_seckey_t *seckey,
uint8_t *pubkeyid)
{
pgp_key_t *key;
unsigned c;
if (keyring == NULL) return NULL;
for (c = 0; c < keyring->keyc; c += 1) {
if (memcmp(keyring->keys[c].pubkeyid,
pubkeyid, PGP_KEY_ID_SIZE) == 0) {
return &keyring->keys[c];
}
}
EXPAND_ARRAY(keyring, key);
key = &keyring->keys[keyring->keyc++];
(void) memset(key, 0x0, sizeof(*key));
key->type = PGP_PTAG_CT_SECRET_KEY;
pgp_seckey_dup(&key->key.seckey, seckey);
(void) memcpy(&key->pubkeyid, pubkeyid, PGP_KEY_ID_SIZE);
pgp_fingerprint(&key->pubkeyfpr, &seckey->pubkey, keyring->hashtype);
return key;
}
unsigned pgp_add_directsig(
pgp_key_t *key,
const pgp_subpacket_t *sigpkt,
pgp_sig_info_t *siginfo)
{
pgp_directsig_t *directsigp;
unsigned directsigidx;
directsigp = key->directsigs;
for (directsigidx = 0 ; directsigidx < key->directsigc;
directsigidx++, directsigp++)
{
if( directsigp->packet.length == sigpkt->length &&
memcmp(directsigp->packet.raw, sigpkt->raw, sigpkt->length) == 0)
{
return 1;
}
}
EXPAND_ARRAY(key, directsig);
directsigp = &key->directsigs[key->directsigc++];
copy_sig_info(&directsigp->siginfo,
siginfo);
pgp_copy_packet(&directsigp->packet, sigpkt);
return 0;
}
unsigned pgp_update_userid(
pgp_key_t *key,
const uint8_t *userid,
const pgp_subpacket_t *sigpkt,
const pgp_sig_info_t *siginfo)
{
unsigned uididx = 0;
unsigned uidsigidx = 0;
uint8_t **uidp;
pgp_uidsig_t *uidsigp;
uidp = key->uids;
for (uididx = 0 ; uididx < key->uidc; uididx++, uidp++)
{
if (strcmp((char *)*uidp, (char *)userid) == 0)
{
uidsigp = key->uidsigs;
for (uidsigidx = 0 ; uidsigidx < key->uidsigc;
uidsigidx++, uidsigp++)
{
if(uidsigp->uid == uididx &&
uidsigp->packet.length == sigpkt->length &&
memcmp(uidsigp->packet.raw, sigpkt->raw,
sigpkt->length) == 0)
{
return 1;
}
}
break;
}
}
if(uididx==key->uidc){
EXPAND_ARRAY(key, uid);
uidp = &key->uids[key->uidc++];
*uidp = NULL;
pgp_copy_userid(uidp, userid);
}
EXPAND_ARRAY(key, uidsig);
uidsigp = &key->uidsigs[key->uidsigc++];
uidsigp->uid = uididx;
copy_sig_info(&uidsigp->siginfo , siginfo );
pgp_copy_packet(&uidsigp->packet, sigpkt);
return 0;
}
unsigned pgp_update_subkey(
pgp_key_t *key,
pgp_content_enum subkeytype,
pgp_keydata_key_t *subkey,
const pgp_subpacket_t *sigpkt,
pgp_sig_info_t *siginfo)
{
unsigned subkeyidx = 0;
unsigned subkeysigidx = 0;
pgp_subkey_t *subkeyp;
pgp_subkeysig_t *subkeysigp;
uint8_t subkeyid[PGP_KEY_ID_SIZE];
pgp_keyid(subkeyid, PGP_KEY_ID_SIZE,
(subkeytype == PGP_PTAG_CT_PUBLIC_KEY) ?
&subkey->pubkey:
&subkey->seckey.pubkey, PGP_HASH_SHA1);
subkeyp = key->subkeys;
for (subkeyidx = 0 ; subkeyidx < key->subkeyc; subkeyidx++, subkeyp++)
{
if(memcmp(subkeyid, subkeyp->id, PGP_KEY_ID_SIZE) == 0 )
{
subkeysigp = key->subkeysigs;
for (subkeysigidx = 0 ; subkeysigidx < key->subkeysigc;
subkeysigidx++, subkeysigp++)
{
if(subkeysigp->subkey == subkeyidx &&
subkeysigp->packet.length == sigpkt->length &&
memcmp(subkeysigp->packet.raw, sigpkt->raw,
sigpkt->length) == 0)
{
return 1;
}
}
break;
}
}
if(subkeyidx==key->subkeyc){
if(subkeytype == PGP_PTAG_CT_PUBLIC_KEY &&
key->type != PGP_PTAG_CT_PUBLIC_KEY){
return 1;
}
EXPAND_ARRAY(key, subkey);
subkeyp = &key->subkeys[key->subkeyc++];
if(key->type == PGP_PTAG_CT_PUBLIC_KEY) {
pgp_pubkey_dup(&subkeyp->key.pubkey,
(subkeytype == PGP_PTAG_CT_PUBLIC_KEY) ?
&subkey->pubkey:
&subkey->seckey.pubkey);
} else {
pgp_seckey_dup(&subkeyp->key.seckey, &subkey->seckey);
}
memcpy(subkeyp->id, subkeyid, PGP_KEY_ID_SIZE);
}
EXPAND_ARRAY(key, subkeysig);
subkeysigp = &key->subkeysigs[key->subkeysigc++];
subkeysigp->subkey = subkeyidx;
copy_sig_info(&subkeysigp->siginfo,
siginfo);
pgp_copy_packet(&subkeysigp->packet, sigpkt);
return 0;
}
int
pgp_append_keyring(pgp_keyring_t *keyring, pgp_keyring_t *newring)
{
unsigned i;
for (i = 0 ; i < newring->keyc ; i++) {
EXPAND_ARRAY(keyring, key);
(void) memcpy(&keyring->keys[keyring->keyc], &newring->keys[i],
sizeof(newring->keys[i]));
keyring->keyc += 1;
}
return 1;
}