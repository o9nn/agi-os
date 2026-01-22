#include "dc_context.h"
#ifdef DC_USE_RPGP
#include <librpgp.h>
#else
#include <netpgp-extra.h>
#include <openssl/rand.h>
#endif
#include "dc_key.h"
#include "dc_keyring.h"
#include "dc_pgp.h"
#include "dc_hash.h"
#ifdef DC_USE_RPGP
void dc_pgp_init(void)
{
}
#else
static int s_io_initialized = 0;
static pgp_io_t s_io;
void dc_pgp_init(void)
{
if (s_io_initialized) {
return;
}
memset(&s_io, 0, sizeof(pgp_io_t));
s_io.outs = stdout;
s_io.errs = stderr;
s_io.res = stderr;
s_io_initialized = 1;
}
#endif
void dc_pgp_exit(void)
{
}
#ifdef DC_USE_RPGP
void dc_pgp_rand_seed(dc_context_t* context, const void* buf, size_t bytes) {}
#else
void dc_pgp_rand_seed(dc_context_t* context, const void* buf, size_t bytes)
{
if (buf==NULL || bytes<=0) {
return;
}
RAND_seed(buf, bytes);
}
#endif
int dc_split_armored_data(char* buf, const char** ret_headerline, const char** ret_setupcodebegin, const char** ret_preferencrypt, const char** ret_base64)
{
int success = 0;
size_t line_chars = 0;
char* line = buf;
char* p1 = buf;
char* p2 = NULL;
char* headerline = NULL;
char* base64 = NULL;
#define PGP_WS "\t\r\n "
if (ret_headerline) { *ret_headerline = NULL; }
if (ret_setupcodebegin) { *ret_setupcodebegin = NULL; }
if (ret_preferencrypt) { *ret_preferencrypt = NULL; }
if (ret_base64) { *ret_base64 = NULL; }
if (buf==NULL || ret_headerline==NULL) {
goto cleanup;
}
dc_remove_cr_chars(buf);
while (*p1) {
if (*p1=='\n') {
line[line_chars] = 0;
if (headerline==NULL) {
dc_trim(line);
if (strncmp(line, "-----BEGIN ", 11)==0 && strncmp(&line[strlen(line)-5], "-----", 5)==0) {
headerline = line;
if (ret_headerline) {
*ret_headerline = headerline;
}
}
}
else if (strspn(line, PGP_WS)==strlen(line)) {
base64 = p1+1;
break;
}
else if ((p2=strchr(line, ':'))==NULL) {
line[line_chars] = '\n';
base64 = line;
break;
}
else {
*p2 = 0;
dc_trim(line);
if (strcasecmp(line, "Passphrase-Begin")==0) {
p2++;
dc_trim(p2);
if (ret_setupcodebegin) {
*ret_setupcodebegin = p2;
}
}
else if (strcasecmp(line, "Autocrypt-Prefer-Encrypt")==0) {
p2++;
dc_trim(p2);
if (ret_preferencrypt) {
*ret_preferencrypt = p2;
}
}
}
p1++;
line = p1;
line_chars = 0;
}
else {
p1++;
line_chars++;
}
}
if (headerline==NULL || base64==NULL) {
goto cleanup;
}
if ((p1=strstr(base64, "-----END "))==NULL
|| strncmp(p1+9, headerline+11, strlen(headerline+11))!=0) {
goto cleanup;
}
*p1 = 0;
dc_trim(base64);
if (ret_base64) {
*ret_base64 = base64;
}
success = 1;
cleanup:
return success;
}
#ifdef DC_USE_RPGP
int dc_pgp_handle_rpgp_error(dc_context_t* context) {
int success = 0;
int len = 0;
char* msg = NULL;
len = rpgp_last_error_length();
if (len==0) {
goto cleanup;
}
msg = rpgp_last_error_message();
if (context != NULL) {
dc_log_info(context, 0, "[rpgp][error] %s", msg);
}
success = 1;
cleanup:
if (msg) { rpgp_string_drop(msg); }
return success;
}
#endif
#ifdef DC_USE_RPGP
int dc_pgp_create_keypair(dc_context_t* context, const char* addr, dc_key_t* ret_public_key, dc_key_t* ret_private_key)
{
int success = 0;
rpgp_signed_secret_key* skey = NULL;
rpgp_signed_public_key* pkey = NULL;
rpgp_cvec* skey_bytes = NULL;
rpgp_cvec* pkey_bytes = NULL;
char* user_id = NULL;
user_id = dc_mprintf("<%s>", addr);
skey = rpgp_create_rsa_skey(DC_KEYGEN_BITS, user_id);
if (dc_pgp_handle_rpgp_error(context)) {
goto cleanup;
}
skey_bytes = rpgp_skey_to_bytes(skey);
if (dc_pgp_handle_rpgp_error(context)) {
goto cleanup;
}
pkey = rpgp_skey_public_key(skey);
if (dc_pgp_handle_rpgp_error(context)) {
goto cleanup;
}
pkey_bytes = rpgp_pkey_to_bytes(pkey);
if (dc_pgp_handle_rpgp_error(context)) {
goto cleanup;
}
dc_key_set_from_binary(ret_private_key, rpgp_cvec_data(skey_bytes), rpgp_cvec_len(skey_bytes), DC_KEY_PRIVATE);
if (dc_pgp_handle_rpgp_error(context)) {
goto cleanup;
}
dc_key_set_from_binary(ret_public_key, rpgp_cvec_data(pkey_bytes), rpgp_cvec_len(pkey_bytes), DC_KEY_PUBLIC);
if (dc_pgp_handle_rpgp_error(context)) {
goto cleanup;
}
success = 1;
cleanup:
if (skey) { rpgp_skey_drop(skey); }
if (skey_bytes) { rpgp_cvec_drop(skey_bytes); }
if (pkey) { rpgp_pkey_drop(pkey); }
if (pkey_bytes) { rpgp_cvec_drop(pkey_bytes); }
if (user_id) { free(user_id); }
return success;
}
#else
static unsigned add_key_prefs(pgp_create_sig_t *sig)
{
return
pgp_write_ss_header(sig->output, 6, PGP_PTAG_SS_PREFERRED_SKA) &&
pgp_write_scalar(sig->output, PGP_SA_AES_256, 1) &&
pgp_write_scalar(sig->output, PGP_SA_AES_128, 1) &&
pgp_write_scalar(sig->output, PGP_SA_CAST5, 1) &&
pgp_write_scalar(sig->output, PGP_SA_TRIPLEDES, 1) &&
pgp_write_scalar(sig->output, PGP_SA_IDEA, 1) &&
pgp_write_ss_header(sig->output, 6, PGP_PTAG_SS_PREFERRED_HASH) &&
pgp_write_scalar(sig->output, PGP_HASH_SHA256, 1) &&
pgp_write_scalar(sig->output, PGP_HASH_SHA384, 1) &&
pgp_write_scalar(sig->output, PGP_HASH_SHA512, 1) &&
pgp_write_scalar(sig->output, PGP_HASH_SHA224, 1) &&
pgp_write_scalar(sig->output, PGP_HASH_SHA1, 1) &&
pgp_write_ss_header(sig->output, 2, PGP_PTAG_SS_PREF_COMPRESS) &&
pgp_write_scalar(sig->output, PGP_C_ZLIB, 1) ;
}
static void add_selfsigned_userid(pgp_key_t *skey, pgp_key_t *pkey, const uint8_t *userid, time_t key_expiry)
{
pgp_create_sig_t* sig = NULL;
pgp_subpacket_t sigpacket;
pgp_memory_t* mem_sig = NULL;
pgp_output_t* sigoutput = NULL;
sig = pgp_create_sig_new();
pgp_sig_start_key_sig(sig, &skey->key.seckey.pubkey, NULL, userid, PGP_CERT_POSITIVE);
pgp_add_creation_time(sig, time(NULL));
pgp_add_key_expiration_time(sig, key_expiry);
pgp_add_primary_userid(sig, 1);
pgp_add_key_flags(sig, PGP_KEYFLAG_SIGN_DATA|PGP_KEYFLAG_CERT_KEYS);
add_key_prefs(sig);
pgp_add_key_features(sig);
pgp_end_hashed_subpkts(sig);
pgp_add_issuer_keyid(sig, skey->pubkeyid);
pgp_setup_memory_write(&sigoutput, &mem_sig, 128);
pgp_write_sig(sigoutput, sig, &skey->key.seckey.pubkey, &skey->key.seckey);
sigpacket.length = pgp_mem_len(mem_sig);
sigpacket.raw = pgp_mem_data(mem_sig);
pgp_update_userid(skey, userid, &sigpacket, &sig->sig.info);
if(pkey) {
pgp_update_userid(pkey, userid, &sigpacket, &sig->sig.info);
}
pgp_create_sig_delete(sig);
pgp_output_delete(sigoutput);
pgp_memory_free(mem_sig);
}
static void add_subkey_binding_signature(pgp_subkeysig_t* p, pgp_key_t* primarykey, pgp_key_t* subkey, pgp_key_t* seckey)
{
pgp_create_sig_t* sig = NULL;
pgp_output_t* sigoutput = NULL;
pgp_memory_t* mem_sig = NULL;
sig = pgp_create_sig_new();
pgp_sig_start_key_sig(sig, &primarykey->key.pubkey, &subkey->key.pubkey, NULL, PGP_SIG_SUBKEY);
pgp_add_creation_time(sig, time(NULL));
pgp_add_key_expiration_time(sig, 0);
pgp_add_key_flags(sig, PGP_KEYFLAG_ENC_STORAGE|PGP_KEYFLAG_ENC_COMM);
pgp_end_hashed_subpkts(sig);
pgp_add_issuer_keyid(sig, seckey->pubkeyid);
pgp_setup_memory_write(&sigoutput, &mem_sig, 128);
pgp_write_sig(sigoutput, sig, &seckey->key.seckey.pubkey, &seckey->key.seckey);
p->subkey = primarykey->subkeyc-1;
p->packet.length = mem_sig->length;
p->packet.raw = mem_sig->buf; mem_sig->buf = NULL;
copy_sig_info(&p->siginfo, &sig->sig.info);
pgp_create_sig_delete(sig);
pgp_output_delete(sigoutput);
free(mem_sig);
}
int dc_pgp_create_keypair(dc_context_t* context, const char* addr, dc_key_t* ret_public_key, dc_key_t* ret_private_key)
{
int success = 0;
pgp_key_t seckey;
pgp_key_t pubkey;
pgp_key_t subkey;
uint8_t subkeyid[PGP_KEY_ID_SIZE];
uint8_t* user_id = NULL;
pgp_memory_t* pubmem = pgp_memory_new();
pgp_memory_t* secmem = pgp_memory_new();
pgp_output_t* pubout = pgp_output_new();
pgp_output_t* secout = pgp_output_new();
memset(&seckey, 0, sizeof(pgp_key_t));
memset(&pubkey, 0, sizeof(pgp_key_t));
memset(&subkey, 0, sizeof(pgp_key_t));
if (context==NULL || addr==NULL || ret_public_key==NULL || ret_private_key==NULL
|| pubmem==NULL || secmem==NULL || pubout==NULL || secout==NULL) {
goto cleanup;
}
#if 0
user_id = (uint8_t*)dc_mprintf("<%08X@%08X.org>", (int)random(), (int)random());
#else
user_id = (uint8_t*)dc_mprintf("<%s>", addr);
#endif
if (!pgp_rsa_generate_keypair(&seckey, DC_KEYGEN_BITS, DC_KEYGEN_E, NULL, NULL, NULL, 0)
|| !pgp_rsa_generate_keypair(&subkey, DC_KEYGEN_BITS, DC_KEYGEN_E, NULL, NULL, NULL, 0)) {
goto cleanup;
}
pubkey.type = PGP_PTAG_CT_PUBLIC_KEY;
pgp_pubkey_dup(&pubkey.key.pubkey, &seckey.key.pubkey);
memcpy(pubkey.pubkeyid, seckey.pubkeyid, PGP_KEY_ID_SIZE);
pgp_fingerprint(&pubkey.pubkeyfpr, &seckey.key.pubkey, 0);
add_selfsigned_userid(&seckey, &pubkey, (const uint8_t*)user_id, 0);
EXPAND_ARRAY((&pubkey), subkey);
{
pgp_subkey_t* p = &pubkey.subkeys[pubkey.subkeyc++];
pgp_pubkey_dup(&p->key.pubkey, &subkey.key.pubkey);
pgp_keyid(subkeyid, PGP_KEY_ID_SIZE, &pubkey.key.pubkey, PGP_HASH_SHA1);
memcpy(p->id, subkeyid, PGP_KEY_ID_SIZE);
}
EXPAND_ARRAY((&pubkey), subkeysig);
add_subkey_binding_signature(&pubkey.subkeysigs[pubkey.subkeysigc++], &pubkey, &subkey, &seckey);
EXPAND_ARRAY((&seckey), subkey);
{
pgp_subkey_t* p = &seckey.subkeys[seckey.subkeyc++];
pgp_seckey_dup(&p->key.seckey, &subkey.key.seckey);
pgp_keyid(subkeyid, PGP_KEY_ID_SIZE, &seckey.key.pubkey, PGP_HASH_SHA1);
memcpy(p->id, subkeyid, PGP_KEY_ID_SIZE);
}
EXPAND_ARRAY((&seckey), subkeysig);
add_subkey_binding_signature(&seckey.subkeysigs[seckey.subkeysigc++], &seckey, &subkey, &seckey);
pgp_writer_set_memory(pubout, pubmem);
if (!pgp_write_xfer_key(pubout, &pubkey, 0)
|| pubmem->buf==NULL || pubmem->length <= 0) {
goto cleanup;
}
pgp_writer_set_memory(secout, secmem);
if (!pgp_write_xfer_key(secout, &seckey, 0)
|| secmem->buf==NULL || secmem->length <= 0) {
goto cleanup;
}
dc_key_set_from_binary(ret_public_key, pubmem->buf, pubmem->length, DC_KEY_PUBLIC);
dc_key_set_from_binary(ret_private_key, secmem->buf, secmem->length, DC_KEY_PRIVATE);
success = 1;
cleanup:
if (pubout) { pgp_output_delete(pubout); }
if (secout) { pgp_output_delete(secout); }
if (pubmem) { pgp_memory_free(pubmem); }
if (secmem) { pgp_memory_free(secmem); }
pgp_key_free(&seckey);
pgp_key_free(&pubkey);
pgp_key_free(&subkey);
free(user_id);
return success;
}
#endif
#ifdef DC_USE_RPGP
int dc_pgp_is_valid_key(dc_context_t* context, const dc_key_t* raw_key)
{
int key_is_valid = 0;
rpgp_public_or_secret_key* key = NULL;
if (context==NULL || raw_key==NULL || raw_key->binary==NULL || raw_key->bytes <= 0) {
goto cleanup;
}
key = rpgp_key_from_bytes(raw_key->binary, raw_key->bytes);
if (dc_pgp_handle_rpgp_error(context)) {
goto cleanup;
}
if (raw_key->type==DC_KEY_PUBLIC && rpgp_key_is_public(key)) {
key_is_valid = 1;
}
else if (raw_key->type==DC_KEY_PRIVATE && rpgp_key_is_secret(key)) {
key_is_valid = 1;
}
cleanup:
if (key) { rpgp_key_drop(key); }
return key_is_valid;
}
#else
int dc_pgp_is_valid_key(dc_context_t* context, const dc_key_t* raw_key)
{
int key_is_valid = 0;
pgp_keyring_t* public_keys = calloc(1, sizeof(pgp_keyring_t));
pgp_keyring_t* private_keys = calloc(1, sizeof(pgp_keyring_t));
pgp_memory_t* keysmem = pgp_memory_new();
if (context==NULL || raw_key==NULL
|| raw_key->binary==NULL || raw_key->bytes <= 0
|| public_keys==NULL || private_keys==NULL || keysmem==NULL) {
goto cleanup;
}
pgp_memory_add(keysmem, raw_key->binary, raw_key->bytes);
pgp_filter_keys_from_mem(&s_io, public_keys, private_keys, NULL, 0, keysmem);
if (raw_key->type==DC_KEY_PUBLIC && public_keys->keyc >= 1) {
key_is_valid = 1;
}
else if (raw_key->type==DC_KEY_PRIVATE && private_keys->keyc >= 1) {
key_is_valid = 1;
}
cleanup:
if (keysmem) { pgp_memory_free(keysmem); }
if (public_keys) { pgp_keyring_purge(public_keys); free(public_keys); }
if (private_keys) { pgp_keyring_purge(private_keys); free(private_keys); }
return key_is_valid;
}
#endif
#ifdef DC_USE_RPGP
int dc_pgp_calc_fingerprint(const dc_key_t* raw_key, uint8_t** ret_fingerprint, size_t* ret_fingerprint_bytes) {
int success = 0;
rpgp_public_or_secret_key* key = NULL;
rpgp_cvec* fingerprint = NULL;
if (raw_key==NULL || ret_fingerprint==NULL || *ret_fingerprint!=NULL || ret_fingerprint_bytes==NULL || *ret_fingerprint_bytes!=0
|| raw_key->binary==NULL || raw_key->bytes <= 0) {
goto cleanup;
}
key = rpgp_key_from_bytes(raw_key->binary, raw_key->bytes);
if (dc_pgp_handle_rpgp_error(NULL)) {
goto cleanup;
}
fingerprint = rpgp_key_fingerprint(key);
if (dc_pgp_handle_rpgp_error(NULL)) {
goto cleanup;
}
*ret_fingerprint_bytes = rpgp_cvec_len(fingerprint);
*ret_fingerprint = malloc(*ret_fingerprint_bytes);
memcpy(*ret_fingerprint, rpgp_cvec_data(fingerprint), *ret_fingerprint_bytes);
success = 1;
cleanup:
if (key) { rpgp_key_drop(key); }
if (fingerprint) { rpgp_cvec_drop(fingerprint); }
return success;
}
#else
int dc_pgp_calc_fingerprint(const dc_key_t* raw_key, uint8_t** ret_fingerprint, size_t* ret_fingerprint_bytes)
{
int success = 0;
pgp_keyring_t* public_keys = calloc(1, sizeof(pgp_keyring_t));
pgp_keyring_t* private_keys = calloc(1, sizeof(pgp_keyring_t));
pgp_memory_t* keysmem = pgp_memory_new();
if (raw_key==NULL || ret_fingerprint==NULL || *ret_fingerprint!=NULL || ret_fingerprint_bytes==NULL || *ret_fingerprint_bytes!=0
|| raw_key->binary==NULL || raw_key->bytes <= 0
|| public_keys==NULL || private_keys==NULL || keysmem==NULL) {
goto cleanup;
}
pgp_memory_add(keysmem, raw_key->binary, raw_key->bytes);
pgp_filter_keys_from_mem(&s_io, public_keys, private_keys, NULL, 0, keysmem);
if (raw_key->type != DC_KEY_PUBLIC || public_keys->keyc <= 0) {
goto cleanup;
}
pgp_key_t* key0 = &public_keys->keys[0];
pgp_pubkey_t* pubkey0 = &key0->key.pubkey;
if (!pgp_fingerprint(&key0->pubkeyfpr, pubkey0, 0)) {
goto cleanup;
}
*ret_fingerprint_bytes = key0->pubkeyfpr.length;
*ret_fingerprint = malloc(*ret_fingerprint_bytes);
memcpy(*ret_fingerprint, key0->pubkeyfpr.fingerprint, *ret_fingerprint_bytes);
success = 1;
cleanup:
if (keysmem) { pgp_memory_free(keysmem); }
if (public_keys) { pgp_keyring_purge(public_keys); free(public_keys); }
if (private_keys) { pgp_keyring_purge(private_keys); free(private_keys); }
return success;
}
#endif
#ifdef DC_USE_RPGP
int dc_pgp_split_key(dc_context_t* context, const dc_key_t* private_in, dc_key_t* ret_public_key)
{
int success = 0;
rpgp_signed_secret_key* key = NULL;
rpgp_signed_public_key* pub_key = NULL;
rpgp_cvec* buf = NULL;
if (context==NULL || private_in==NULL || ret_public_key==NULL) {
goto cleanup;
}
if (private_in->type!=DC_KEY_PRIVATE) {
dc_log_warning(context, 0, "Split key: Given key is no private key.");
goto cleanup;
}
key = rpgp_skey_from_bytes(private_in->binary, private_in->bytes);
if (dc_pgp_handle_rpgp_error(context)) {
goto cleanup;
}
pub_key = rpgp_skey_public_key(key);
if (dc_pgp_handle_rpgp_error(context)) {
goto cleanup;
}
buf = rpgp_pkey_to_bytes(pub_key);
if (dc_pgp_handle_rpgp_error(context)) {
goto cleanup;
}
dc_key_set_from_binary(ret_public_key, rpgp_cvec_data(buf), rpgp_cvec_len(buf), DC_KEY_PUBLIC);
success = 1;
cleanup:
if (key) { rpgp_skey_drop(key); }
if (pub_key) { rpgp_pkey_drop(pub_key); }
if (buf) { rpgp_cvec_drop(buf); }
return success;
}
#else
int dc_pgp_split_key(dc_context_t* context, const dc_key_t* private_in, dc_key_t* ret_public_key)
{
int success = 0;
pgp_keyring_t* public_keys = calloc(1, sizeof(pgp_keyring_t));
pgp_keyring_t* private_keys = calloc(1, sizeof(pgp_keyring_t));
pgp_memory_t* keysmem = pgp_memory_new();
pgp_memory_t* pubmem = pgp_memory_new();
pgp_output_t* pubout = pgp_output_new();
if (context==NULL || private_in==NULL || ret_public_key==NULL
|| public_keys==NULL || private_keys==NULL || keysmem==NULL || pubmem==NULL || pubout==NULL) {
goto cleanup;
}
pgp_memory_add(keysmem, private_in->binary, private_in->bytes);
pgp_filter_keys_from_mem(&s_io, public_keys, private_keys, NULL, 0, keysmem);
if (private_in->type!=DC_KEY_PRIVATE || private_keys->keyc <= 0) {
dc_log_warning(context, 0, "Split key: Given key is no private key.");
goto cleanup;
}
if (public_keys->keyc <= 0) {
dc_log_warning(context, 0, "Split key: Given key does not contain a public key.");
goto cleanup;
}
pgp_writer_set_memory(pubout, pubmem);
if (!pgp_write_xfer_key(pubout, &public_keys->keys[0], 0)
|| pubmem->buf==NULL || pubmem->length <= 0) {
goto cleanup;
}
dc_key_set_from_binary(ret_public_key, pubmem->buf, pubmem->length, DC_KEY_PUBLIC);
success = 1;
cleanup:
if (pubout) { pgp_output_delete(pubout); }
if (pubmem) { pgp_memory_free(pubmem); }
if (keysmem) { pgp_memory_free(keysmem); }
if (public_keys) { pgp_keyring_purge(public_keys); free(public_keys); }
if (private_keys) { pgp_keyring_purge(private_keys); free(private_keys); }
return success;
}
#endif
#ifdef DC_USE_RPGP
int dc_pgp_pk_encrypt( dc_context_t* context,
const void* plain_text,
size_t plain_bytes,
const dc_keyring_t* raw_public_keys_for_encryption,
const dc_key_t* raw_private_key_for_signing,
int use_armor,
void** ret_ctext,
size_t* ret_ctext_bytes)
{
int i = 0;
int success = 0;
int public_keys_len = 0;
rpgp_signed_public_key* *public_keys = NULL;
rpgp_signed_secret_key* private_key = NULL;
rpgp_message* encrypted = NULL;
if (context==NULL || plain_text==NULL || plain_bytes==0 || ret_ctext==NULL || ret_ctext_bytes==NULL
|| raw_public_keys_for_encryption==NULL || raw_public_keys_for_encryption->count<=0
|| use_armor==0 ) {
goto cleanup;
}
*ret_ctext = NULL;
*ret_ctext_bytes = 0;
public_keys_len = raw_public_keys_for_encryption->count;
public_keys = malloc(sizeof(rpgp_signed_public_key*) * public_keys_len);
if (raw_private_key_for_signing) {
private_key = rpgp_skey_from_bytes(raw_private_key_for_signing->binary,
raw_private_key_for_signing->bytes);
if (private_key == NULL || dc_pgp_handle_rpgp_error(context)) {
dc_log_warning(context, 0, "No key for signing found.");
goto cleanup;
}
}
for (i = 0; i < public_keys_len; i++) {
public_keys[i] = rpgp_pkey_from_bytes(raw_public_keys_for_encryption->keys[i]->binary,
raw_public_keys_for_encryption->keys[i]->bytes);
if (dc_pgp_handle_rpgp_error(context)) {
goto cleanup;
}
}
{
clock_t op_clocks = 0;
clock_t start = clock();
if (private_key==NULL) {
encrypted = rpgp_encrypt_bytes_to_keys(plain_text, plain_bytes,
(const rpgp_signed_public_key* const*)public_keys, public_keys_len);
if (dc_pgp_handle_rpgp_error(context)) {
dc_log_warning(context, 0, "Encryption failed.");
goto cleanup;
}
op_clocks = clock()-start;
dc_log_info(context, 0, "Message encrypted in %.3f ms.", (double)(op_clocks)*1000.0/CLOCKS_PER_SEC);
} else {
encrypted = rpgp_sign_encrypt_bytes_to_keys(plain_text, plain_bytes,
(const rpgp_signed_public_key* const*)public_keys, public_keys_len,
private_key);
if (dc_pgp_handle_rpgp_error(context)) {
dc_log_warning(context, 0, "Signing and encrypting failed.");
goto cleanup;
}
op_clocks = clock()-start;
dc_log_info(context, 0, "Message signed and encrypted in %.3f ms.", (double)(op_clocks)*1000.0/CLOCKS_PER_SEC);
}
rpgp_cvec* armored = rpgp_msg_to_armored(encrypted);
if (dc_pgp_handle_rpgp_error(context)) {
goto cleanup;
}
*ret_ctext = (void*)rpgp_cvec_data(armored);
*ret_ctext_bytes = rpgp_cvec_len(armored);
free(armored);
}
success = 1;
cleanup:
if (private_key) { rpgp_skey_drop(private_key); }
for (i = 0; i < public_keys_len; i++) {
rpgp_pkey_drop(public_keys[i]);
}
if (encrypted) { rpgp_msg_drop(encrypted); }
return success;
}
#else
int dc_pgp_pk_encrypt( dc_context_t* context,
const void* plain_text,
size_t plain_bytes,
const dc_keyring_t* raw_public_keys_for_encryption,
const dc_key_t* raw_private_key_for_signing,
int use_armor,
void** ret_ctext,
size_t* ret_ctext_bytes)
{
pgp_keyring_t* public_keys = calloc(1, sizeof(pgp_keyring_t));
pgp_keyring_t* private_keys = calloc(1, sizeof(pgp_keyring_t));
pgp_keyring_t* dummy_keys = calloc(1, sizeof(pgp_keyring_t));
pgp_memory_t* keysmem = pgp_memory_new();
pgp_memory_t* signedmem = NULL;
int i = 0;
int success = 0;
if (context==NULL || plain_text==NULL || plain_bytes==0 || ret_ctext==NULL || ret_ctext_bytes==NULL
|| raw_public_keys_for_encryption==NULL || raw_public_keys_for_encryption->count<=0
|| keysmem==NULL || public_keys==NULL || private_keys==NULL || dummy_keys==NULL) {
goto cleanup;
}
*ret_ctext = NULL;
*ret_ctext_bytes = 0;
for (i = 0; i < raw_public_keys_for_encryption->count; i++) {
pgp_memory_clear(keysmem);
pgp_memory_add(keysmem, raw_public_keys_for_encryption->keys[i]->binary, raw_public_keys_for_encryption->keys[i]->bytes);
pgp_filter_keys_from_mem(&s_io, public_keys, private_keys, NULL, 0, keysmem);
}
if (public_keys->keyc <=0 || private_keys->keyc!=0) {
dc_log_warning(context, 0, "Encryption-keyring contains unexpected data (%i/%i)", public_keys->keyc, private_keys->keyc);
goto cleanup;
}
{
const void* signed_text = NULL;
size_t signed_bytes = 0;
int encrypt_raw_packet = 0;
clock_t sign_clocks = 0;
clock_t encrypt_clocks = 0;
if (raw_private_key_for_signing) {
pgp_memory_clear(keysmem);
pgp_memory_add(keysmem, raw_private_key_for_signing->binary, raw_private_key_for_signing->bytes);
pgp_filter_keys_from_mem(&s_io, dummy_keys, private_keys, NULL, 0, keysmem);
if (private_keys->keyc <= 0) {
dc_log_warning(context, 0, "No key for signing found.");
goto cleanup;
}
clock_t start = clock();
pgp_key_t* sk0 = &private_keys->keys[0];
signedmem = pgp_sign_buf(&s_io, plain_text, plain_bytes, &sk0->key.seckey, time(NULL), 0,
NULL, 0, 0);
sign_clocks = clock()-start;
if (signedmem==NULL) {
dc_log_warning(context, 0, "Signing failed.");
goto cleanup;
}
signed_text = signedmem->buf;
signed_bytes = signedmem->length;
encrypt_raw_packet = 1;
}
else {
signed_text = plain_text;
signed_bytes = plain_bytes;
encrypt_raw_packet = 0;
}
clock_t start = clock();
pgp_memory_t* outmem = pgp_encrypt_buf(&s_io, signed_text, signed_bytes, public_keys, use_armor, NULL, encrypt_raw_packet);
encrypt_clocks = clock()-start;
dc_log_info(context, 0, "Message signed in %.3f ms and encrypted in %.3f ms.", (double)(sign_clocks)*1000.0/CLOCKS_PER_SEC, (double)(encrypt_clocks)*1000.0/CLOCKS_PER_SEC);
if (outmem==NULL) {
dc_log_warning(context, 0, "Encryption failed.");
goto cleanup;
}
*ret_ctext = outmem->buf;
*ret_ctext_bytes = outmem->length;
free(outmem);
}
success = 1;
cleanup:
if (keysmem) { pgp_memory_free(keysmem); }
if (signedmem) { pgp_memory_free(signedmem); }
if (public_keys) { pgp_keyring_purge(public_keys); free(public_keys); }
if (private_keys) { pgp_keyring_purge(private_keys); free(private_keys); }
if (dummy_keys) { pgp_keyring_purge(dummy_keys); free(dummy_keys); }
return success;
}
#endif
#ifdef DC_USE_RPGP
int dc_pgp_pk_decrypt( dc_context_t* context,
const void* ctext,
size_t ctext_bytes,
const dc_keyring_t* raw_private_keys_for_decryption,
const dc_keyring_t* raw_public_keys_for_validation,
int use_armor,
void** ret_plain,
size_t* ret_plain_bytes,
dc_hash_t* ret_signature_fingerprints)
{
int i = 0;
int success = 0;
rpgp_message* encrypted = NULL;
rpgp_message_decrypt_result* decrypted = NULL;
int private_keys_len = 0;
int public_keys_len = 0;
rpgp_signed_secret_key* *private_keys = NULL;
rpgp_signed_public_key* *public_keys = NULL;
if (context==NULL || ctext==NULL || ctext_bytes==0 || ret_plain==NULL
|| ret_plain_bytes==NULL || raw_private_keys_for_decryption==NULL
|| raw_private_keys_for_decryption->count<=0
|| use_armor==0 ) {
goto cleanup;
}
*ret_plain = NULL;
*ret_plain_bytes = 0;
private_keys_len = raw_private_keys_for_decryption->count;
private_keys = malloc(sizeof(rpgp_signed_secret_key*) * private_keys_len);
if (raw_public_keys_for_validation) {
public_keys_len = raw_public_keys_for_validation->count;
public_keys = malloc(sizeof(rpgp_signed_public_key*) * public_keys_len);
}
for (i = 0; i < raw_private_keys_for_decryption->count; i++) {
private_keys[i] = rpgp_skey_from_bytes(raw_private_keys_for_decryption->keys[i]->binary,
raw_private_keys_for_decryption->keys[i]->bytes);
if (dc_pgp_handle_rpgp_error(context)) {
goto cleanup;
}
}
if (raw_public_keys_for_validation) {
for (i = 0; i < raw_public_keys_for_validation->count; i++) {
public_keys[i] = rpgp_pkey_from_bytes(raw_public_keys_for_validation->keys[i]->binary,
raw_public_keys_for_validation->keys[i]->bytes);
if (dc_pgp_handle_rpgp_error(context)) {
goto cleanup;
}
}
}
{
encrypted = rpgp_msg_from_armor(ctext, ctext_bytes);
if (dc_pgp_handle_rpgp_error(context)) {
goto cleanup;
}
decrypted = rpgp_msg_decrypt_no_pw(encrypted,
(const rpgp_signed_secret_key* const*)private_keys, private_keys_len,
(const rpgp_signed_public_key* const*)public_keys, public_keys_len);
if (dc_pgp_handle_rpgp_error(context)) {
goto cleanup;
}
rpgp_cvec* decrypted_bytes = rpgp_msg_to_bytes(decrypted->message_ptr);
if (dc_pgp_handle_rpgp_error(context)) {
goto cleanup;
}
*ret_plain_bytes = rpgp_cvec_len(decrypted_bytes);
*ret_plain = (void*)rpgp_cvec_data(decrypted_bytes);
free(decrypted_bytes);
if (ret_signature_fingerprints) {
uint32_t j = 0;
uint32_t len = (uint32_t)decrypted->valid_ids_len;
for (; j < len; j++) {
char* fingerprint_hex = decrypted->valid_ids_ptr[j];
if (fingerprint_hex) {
dc_hash_insert(ret_signature_fingerprints, fingerprint_hex, strlen(fingerprint_hex), (void*)1);
free(fingerprint_hex);
}
}
}
}
success = 1;
cleanup:
for (i = 0; i < private_keys_len; i++) {
rpgp_skey_drop(private_keys[i]);
}
for (i = 0; i < public_keys_len; i++) {
rpgp_pkey_drop(public_keys[i]);
}
if (encrypted) { rpgp_msg_drop(encrypted); }
if (decrypted) { rpgp_message_decrypt_result_drop(decrypted); }
return success;
}
#else
int dc_pgp_pk_decrypt( dc_context_t* context,
const void* ctext,
size_t ctext_bytes,
const dc_keyring_t* raw_private_keys_for_decryption,
const dc_keyring_t* raw_public_keys_for_validation,
int use_armor,
void** ret_plain,
size_t* ret_plain_bytes,
dc_hash_t* ret_signature_fingerprints)
{
pgp_keyring_t* public_keys = calloc(1, sizeof(pgp_keyring_t));
pgp_keyring_t* private_keys = calloc(1, sizeof(pgp_keyring_t));
pgp_keyring_t* dummy_keys = calloc(1, sizeof(pgp_keyring_t));
pgp_validation_t* vresult = calloc(1, sizeof(pgp_validation_t));
key_id_t* recipients_key_ids = NULL;
unsigned recipients_cnt = 0;
pgp_memory_t* keysmem = pgp_memory_new();
int i = 0;
int success = 0;
if (context==NULL || ctext==NULL || ctext_bytes==0 || ret_plain==NULL || ret_plain_bytes==NULL
|| raw_private_keys_for_decryption==NULL || raw_private_keys_for_decryption->count<=0
|| vresult==NULL || keysmem==NULL || public_keys==NULL || private_keys==NULL) {
goto cleanup;
}
*ret_plain = NULL;
*ret_plain_bytes = 0;
for (i = 0; i < raw_private_keys_for_decryption->count; i++) {
pgp_memory_clear(keysmem);
pgp_memory_add(keysmem, raw_private_keys_for_decryption->keys[i]->binary, raw_private_keys_for_decryption->keys[i]->bytes);
pgp_filter_keys_from_mem(&s_io, dummy_keys, private_keys, NULL, 0, keysmem);
}
if (private_keys->keyc<=0) {
dc_log_warning(context, 0, "Decryption-keyring contains unexpected data (%i/%i)", public_keys->keyc, private_keys->keyc);
goto cleanup;
}
if (raw_public_keys_for_validation) {
for (i = 0; i < raw_public_keys_for_validation->count; i++) {
pgp_memory_clear(keysmem);
pgp_memory_add(keysmem, raw_public_keys_for_validation->keys[i]->binary, raw_public_keys_for_validation->keys[i]->bytes);
pgp_filter_keys_from_mem(&s_io, public_keys, dummy_keys, NULL, 0, keysmem);
}
}
{
pgp_memory_t* outmem = pgp_decrypt_and_validate_buf(&s_io, vresult, ctext, ctext_bytes, private_keys, public_keys,
use_armor, &recipients_key_ids, &recipients_cnt);
if (outmem==NULL) {
dc_log_warning(context, 0, "Decryption failed.");
goto cleanup;
}
*ret_plain = outmem->buf;
*ret_plain_bytes = outmem->length;
free(outmem);
if (ret_signature_fingerprints)
{
for (i = 0; i < vresult->validc; i++)
{
unsigned from = 0;
pgp_key_t* key0 = pgp_getkeybyid(&s_io, public_keys, vresult->valid_sigs[i].signer_id, &from, NULL, NULL, 0, 0);
if (key0) {
pgp_pubkey_t* pubkey0 = &key0->key.pubkey;
if (!pgp_fingerprint(&key0->pubkeyfpr, pubkey0, 0)) {
goto cleanup;
}
char* fingerprint_hex = dc_binary_to_uc_hex(key0->pubkeyfpr.fingerprint, key0->pubkeyfpr.length);
if (fingerprint_hex) {
dc_hash_insert(ret_signature_fingerprints, fingerprint_hex, strlen(fingerprint_hex), (void*)1);
}
free(fingerprint_hex);
}
}
}
}
success = 1;
cleanup:
if (keysmem) { pgp_memory_free(keysmem); }
if (public_keys) { pgp_keyring_purge(public_keys); free(public_keys); }
if (private_keys) { pgp_keyring_purge(private_keys); free(private_keys); }
if (dummy_keys) { pgp_keyring_purge(dummy_keys); free(dummy_keys); }
if (vresult) { pgp_validate_result_free(vresult); }
free(recipients_key_ids);
return success;
}
#endif
#ifdef DC_USE_RPGP
int dc_pgp_symm_encrypt(dc_context_t* context,
const char* passphrase,
const void* plain, size_t plain_bytes,
char** ret_ctext_armored)
{
int success = 0;
rpgp_message* decrypted = NULL;
if (context==NULL || passphrase==NULL || plain==NULL || plain_bytes==0
|| ret_ctext_armored==NULL ) {
goto cleanup;
}
decrypted = rpgp_encrypt_bytes_with_password(plain, plain_bytes, passphrase);
if (dc_pgp_handle_rpgp_error(context)) {
goto cleanup;
}
*ret_ctext_armored = rpgp_msg_to_armored_str(decrypted);
if (dc_pgp_handle_rpgp_error(context)) {
goto cleanup;
}
success = 1;
cleanup:
if (decrypted) { rpgp_msg_drop(decrypted); }
return success;
}
#else
int dc_pgp_symm_encrypt(dc_context_t* context,
const char* passphrase,
const void* plain, size_t plain_bytes,
char** ret_ctext_armored)
{
int success = 0;
uint8_t salt[PGP_SALT_SIZE];
pgp_crypt_t crypt_info;
uint8_t* key = NULL;
pgp_output_t* payload_output = NULL;
pgp_memory_t* payload_mem = NULL;
pgp_output_t* encr_output = NULL;
pgp_memory_t* encr_mem = NULL;
if (context==NULL || passphrase==NULL || plain==NULL || plain_bytes==0
|| ret_ctext_armored==NULL ) {
goto cleanup;
}
pgp_setup_memory_write(&payload_output, &payload_mem, 128);
pgp_write_litdata(payload_output, (const uint8_t*)plain, plain_bytes, PGP_LDT_BINARY);
pgp_random(salt, PGP_SALT_SIZE);
#define SYMM_ALGO PGP_SA_AES_128
if (!pgp_crypt_any(&crypt_info, SYMM_ALGO)) {
goto cleanup;
}
int s2k_spec = PGP_S2KS_ITERATED_AND_SALTED;
int s2k_iter_id = 96;
#define HASH_ALG PGP_HASH_SHA256
if ((key = pgp_s2k_do(passphrase, crypt_info.keysize, s2k_spec, HASH_ALG, salt, s2k_iter_id))==NULL) {
goto cleanup;
}
pgp_setup_memory_write(&encr_output, &encr_mem, 128);
pgp_writer_push_armor_msg(encr_output);
pgp_write_ptag (encr_output, PGP_PTAG_CT_SK_SESSION_KEY);
pgp_write_length (encr_output, 1
+ 1
+ 1
+ 1
+ ((s2k_spec==PGP_S2KS_SALTED || s2k_spec==PGP_S2KS_ITERATED_AND_SALTED)? PGP_SALT_SIZE : 0)
+ ((s2k_spec==PGP_S2KS_ITERATED_AND_SALTED)? 1 : 0));
pgp_write_scalar (encr_output, 4, 1);
pgp_write_scalar (encr_output, SYMM_ALGO, 1);
pgp_write_scalar (encr_output, s2k_spec, 1);
pgp_write_scalar (encr_output, HASH_ALG, 1);
if (s2k_spec==PGP_S2KS_SALTED || s2k_spec==PGP_S2KS_ITERATED_AND_SALTED) {
pgp_write (encr_output, salt, PGP_SALT_SIZE);
}
if (s2k_spec==PGP_S2KS_ITERATED_AND_SALTED) {
pgp_write_scalar (encr_output, s2k_iter_id, 1);
}
{
uint8_t* iv = calloc(1, crypt_info.blocksize); if (iv==NULL) { goto cleanup; }
crypt_info.set_iv(&crypt_info, iv);
free(iv);
crypt_info.set_crypt_key(&crypt_info, &key[0]);
pgp_encrypt_init(&crypt_info);
pgp_write_se_ip_pktset(encr_output, payload_mem->buf, payload_mem->length, &crypt_info);
crypt_info.decrypt_finish(&crypt_info);
}
pgp_writer_close(encr_output);
*ret_ctext_armored = dc_null_terminate((const char*)encr_mem->buf, encr_mem->length);
success = 1;
cleanup:
if (payload_output) { pgp_output_delete(payload_output); }
if (payload_mem) { pgp_memory_free(payload_mem); }
if (encr_output) { pgp_output_delete(encr_output); }
if (encr_mem) { pgp_memory_free(encr_mem); }
free(key);
return success;
}
#endif
#ifdef DC_USE_RPGP
int dc_pgp_symm_decrypt(dc_context_t* context,
const char* passphrase,
const void* ctext, size_t ctext_bytes,
void** ret_plain_text, size_t* ret_plain_bytes)
{
int success = 0;
rpgp_message* encrypted = NULL;
rpgp_message* decrypted = NULL;
encrypted = rpgp_msg_from_bytes(ctext, ctext_bytes);
if (dc_pgp_handle_rpgp_error(context)) {
goto cleanup;
}
decrypted = rpgp_msg_decrypt_with_password(encrypted, passphrase);
if (dc_pgp_handle_rpgp_error(context)) {
goto cleanup;
}
rpgp_cvec* decrypted_bytes = rpgp_msg_to_bytes(decrypted);
if (dc_pgp_handle_rpgp_error(context)) {
goto cleanup;
}
*ret_plain_text = (void*)rpgp_cvec_data(decrypted_bytes);
*ret_plain_bytes = rpgp_cvec_len(decrypted_bytes);
free(decrypted_bytes);
success = 1;
cleanup:
if (encrypted) { rpgp_msg_drop(encrypted); }
if (decrypted) { rpgp_msg_drop(decrypted); }
return success;
}
#else
int dc_pgp_symm_decrypt(dc_context_t* context,
const char* passphrase,
const void* ctext, size_t ctext_bytes,
void** ret_plain_text, size_t* ret_plain_bytes)
{
int success = 0;
pgp_io_t io;
pgp_memory_t* outmem = NULL;
memset(&io, 0, sizeof(pgp_io_t));
io.outs = stdout;
io.errs = stderr;
io.res = stderr;
if ((outmem=pgp_decrypt_buf(&io, ctext, ctext_bytes, NULL, NULL, 0, 0, passphrase))==NULL) {
goto cleanup;
}
*ret_plain_text = outmem->buf;
*ret_plain_bytes = outmem->length;
free(outmem);
outmem = NULL;
success = 1;
cleanup:
if (outmem) { pgp_memory_free(outmem); }
return success;
}
#endif