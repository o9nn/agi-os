#include "netpgp/config-netpgp.h"
#ifdef HAVE_SYS_CDEFS_H
#include <sys/cdefs.h>
#endif
#if defined(__NetBSD__)
__COPYRIGHT("@(#) Copyright (c) 2009 The NetBSD Foundation, Inc. All rights reserved.");
__RCSID("$NetBSD$");
#endif
#include <sys/types.h>
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <string.h>
#include "netpgp/types.h"
#include "netpgp/crypto.h"
#include "netpgp/readerwriter.h"
#include "netpgp/memory.h"
#include "netpgp/netpgpdefs.h"
#include "netpgp/signature.h"
#include "netpgp/netpgpsdk.h"
#include "netpgp/validate.h"
int
pgp_decrypt_decode_mpi(uint8_t *buf,
unsigned buflen,
const BIGNUM *g_to_k,
const BIGNUM *encmpi,
const pgp_seckey_t *seckey)
{
unsigned mpisize;
uint8_t encmpibuf[NETPGP_BUFSIZ];
uint8_t mpibuf[NETPGP_BUFSIZ];
uint8_t gkbuf[NETPGP_BUFSIZ];
int i;
int n;
mpisize = (unsigned)BN_num_bytes(encmpi);
if (mpisize > sizeof(encmpibuf)) {
(void) fprintf(stderr, "mpisize too big %u\n", mpisize);
return -1;
}
switch (seckey->pubkey.alg) {
case PGP_PKA_RSA:
BN_bn2bin(encmpi, encmpibuf);
if (pgp_get_debug_level(__FILE__)) {
hexdump(stderr, "encrypted", encmpibuf, 16);
}
n = pgp_rsa_private_decrypt(mpibuf, encmpibuf,
(unsigned)(BN_num_bits(encmpi) + 7) / 8,
&seckey->key.rsa, &seckey->pubkey.key.rsa);
if (n == -1) {
(void) fprintf(stderr, "ops_rsa_private_decrypt failure\n");
return -1;
}
if (pgp_get_debug_level(__FILE__)) {
hexdump(stderr, "decrypted", mpibuf, 16);
}
if (n <= 0) {
return -1;
}
if (mpibuf[0] != 0 || mpibuf[1] != 2) {
return -1;
}
for (i = 2; i < n && mpibuf[i]; ++i) {
}
if (i == n || i < 10) {
return -1;
}
i += 1;
if ((unsigned) (n - i) <= buflen) {
(void) memcpy(buf, mpibuf + i, (unsigned)(n - i));
}
if (pgp_get_debug_level(__FILE__)) {
hexdump(stderr, "decoded m", buf, (size_t)(n - i));
}
return n - i;
case PGP_PKA_DSA:
case PGP_PKA_ELGAMAL:
(void) BN_bn2bin(g_to_k, gkbuf);
(void) BN_bn2bin(encmpi, encmpibuf);
if (pgp_get_debug_level(__FILE__)) {
hexdump(stderr, "encrypted", encmpibuf, 16);
}
n = pgp_elgamal_private_decrypt(mpibuf, gkbuf, encmpibuf,
(unsigned)BN_num_bytes(encmpi),
&seckey->key.elgamal, &seckey->pubkey.key.elgamal);
if (n == -1) {
(void) fprintf(stderr, "ops_elgamal_private_decrypt failure\n");
return -1;
}
if (pgp_get_debug_level(__FILE__)) {
hexdump(stderr, "decrypted", mpibuf, 16);
}
if (n <= 0) {
return -1;
}
if (mpibuf[0] != 2) {
fprintf(stderr, "mpibuf mismatch\n");
return -1;
}
for (i = 1; i < n && mpibuf[i]; ++i) {
}
if (i == n || i < 10) {
fprintf(stderr, "175 n %d\n", n);
return -1;
}
i += 1;
if ((unsigned) (n - i) <= buflen) {
(void) memcpy(buf, mpibuf + i, (unsigned)(n - i));
}
if (pgp_get_debug_level(__FILE__)) {
hexdump(stderr, "decoded m", buf, (size_t)(n - i));
}
return n - i;
default:
(void) fprintf(stderr, "pubkey algorithm wrong\n");
return -1;
}
}
unsigned
pgp_rsa_encrypt_mpi(const uint8_t *encoded_m_buf,
const size_t sz_encoded_m_buf,
const pgp_pubkey_t * pubkey,
pgp_pk_sesskey_params_t * skp)
{
uint8_t encmpibuf[NETPGP_BUFSIZ];
int n;
if (sz_encoded_m_buf != (size_t)BN_num_bytes(pubkey->key.rsa.n)) {
(void) fprintf(stderr, "sz_encoded_m_buf wrong\n");
return 0;
}
n = pgp_rsa_public_encrypt(encmpibuf, encoded_m_buf,
sz_encoded_m_buf, &pubkey->key.rsa);
if (n == -1) {
(void) fprintf(stderr, "pgp_rsa_public_encrypt failure\n");
return 0;
}
if (n <= 0)
return 0;
skp->rsa.encrypted_m = BN_bin2bn(encmpibuf, n, NULL);
if (pgp_get_debug_level(__FILE__)) {
hexdump(stderr, "encrypted mpi", encmpibuf, 16);
}
return 1;
}
unsigned
pgp_elgamal_encrypt_mpi(const uint8_t *encoded_m_buf,
const size_t sz_encoded_m_buf,
const pgp_pubkey_t * pubkey,
pgp_pk_sesskey_params_t * skp)
{
uint8_t encmpibuf[NETPGP_BUFSIZ];
uint8_t g_to_k[NETPGP_BUFSIZ];
int n;
if (sz_encoded_m_buf != (size_t)BN_num_bytes(pubkey->key.elgamal.p)) {
(void) fprintf(stderr, "sz_encoded_m_buf wrong\n");
return 0;
}
n = pgp_elgamal_public_encrypt(g_to_k, encmpibuf, encoded_m_buf,
sz_encoded_m_buf, &pubkey->key.elgamal);
if (n == -1) {
(void) fprintf(stderr, "pgp_elgamal_public_encrypt failure\n");
return 0;
}
if (n <= 0)
return 0;
skp->elgamal.g_to_k = BN_bin2bn(g_to_k, n / 2, NULL);
skp->elgamal.encrypted_m = BN_bin2bn(encmpibuf, n / 2, NULL);
if (pgp_get_debug_level(__FILE__)) {
hexdump(stderr, "encrypted mpi", encmpibuf, 16);
}
return 1;
}
static pgp_cb_ret_t
write_parsed_cb(const pgp_packet_t *pkt, pgp_cbdata_t *cbinfo)
{
const pgp_contents_t *content = &pkt->u;
if (pgp_get_debug_level(__FILE__)) {
printf("write_parsed_cb: ");
}
if (pkt->tag != PGP_PTAG_CT_UNARMOURED_TEXT && cbinfo->printstate.skipping) {
puts("...end of skip");
cbinfo->printstate.skipping = 0;
}
switch (pkt->tag) {
case PGP_PTAG_CT_UNARMOURED_TEXT:
printf("PGP_PTAG_CT_UNARMOURED_TEXT\n");
if (!cbinfo->printstate.skipping) {
puts("Skipping...");
cbinfo->printstate.skipping = 1;
}
if (fwrite(content->unarmoured_text.data, 1,
content->unarmoured_text.length, stdout) != content->unarmoured_text.length) {
fprintf(stderr, "unable to write unarmoured text data\n");
cbinfo->printstate.skipping = 1;
}
break;
case PGP_PTAG_CT_PK_SESSION_KEY:
return pgp_pk_sesskey_cb(pkt, cbinfo);
case PGP_GET_SECKEY:
if (cbinfo->sshseckey) {
*content->get_seckey.seckey = cbinfo->sshseckey;
return PGP_KEEP_MEMORY;
}
return pgp_get_seckey_cb(pkt, cbinfo);
case PGP_GET_PASSPHRASE:
break;
case PGP_PTAG_CT_LITDATA_BODY:
return pgp_litdata_cb(pkt, cbinfo);
case PGP_PTAG_CT_ARMOUR_HEADER:
case PGP_PTAG_CT_ARMOUR_TRAILER:
case PGP_PTAG_CT_ENCRYPTED_PK_SESSION_KEY:
case PGP_PTAG_CT_COMPRESSED:
case PGP_PTAG_CT_LITDATA_HEADER:
case PGP_PTAG_CT_SE_IP_DATA_BODY:
case PGP_PTAG_CT_SE_IP_DATA_HEADER:
case PGP_PTAG_CT_SE_DATA_BODY:
case PGP_PTAG_CT_SE_DATA_HEADER:
break;
default:
if (pgp_get_debug_level(__FILE__)) {
fprintf(stderr, "Unexpected packet tag=%d (0x%x)\n",
pkt->tag,
pkt->tag);
}
break;
}
return PGP_RELEASE_MEMORY;
}
#if 0
unsigned
pgp_encrypt_file(pgp_io_t *io,
const char *infile,
const char *outfile,
const pgp_key_t *key,
const unsigned use_armour,
const unsigned allow_overwrite,
const char *cipher)
{
pgp_output_t *output;
pgp_memory_t *inmem;
pgp_keyring_t *rcpts;
int fd_out;
__PGP_USED(io);
inmem = pgp_memory_new();
if (!pgp_mem_readfile(inmem, infile)) {
return 0;
}
fd_out = pgp_setup_file_write(&output, outfile, allow_overwrite);
if (fd_out < 0) {
pgp_memory_free(inmem);
return 0;
}
if (use_armour) {
pgp_writer_push_armor_msg(output);
}
if ((rcpts = calloc(1, sizeof(*rcpts))) == NULL) {
(void) fprintf(io->errs,
"netpgp_encrypt_buf: out of memory to create recipients list\n");
return 0;
}
pgp_keyring_add(rcpts, key);
if(rcpts->keys == NULL){
(void) fprintf(io->errs,
"netpgp_encrypt_buf: out of memory to add recipient\n");
return 0;
}
if (!pgp_push_enc_se_ip(output, rcpts, cipher, 0)) {
pgp_memory_free(inmem);
return 0;
}
pgp_keyring_free(rcpts);
pgp_write(output, pgp_mem_data(inmem), (unsigned)pgp_mem_len(inmem));
pgp_memory_free(inmem);
pgp_teardown_file_write(output, fd_out);
return 1;
}
#endif
pgp_memory_t *
pgp_encrypt_buf(pgp_io_t *io,
const void *input,
const size_t insize,
const pgp_keyring_t *pubkeys,
const unsigned use_armour,
const char *cipher,
unsigned raw)
{
pgp_output_t *output;
pgp_memory_t *outmem;
__PGP_USED(io);
if (input == NULL) {
(void) fprintf(io->errs,
"pgp_encrypt_buf: null memory\n");
return 0;
}
pgp_setup_memory_write(&output, &outmem, insize);
if (use_armour) {
pgp_writer_push_armor_msg(output);
}
pgp_push_enc_se_ip(output, pubkeys, cipher, raw);
pgp_write(output, input, (unsigned)insize);
pgp_writer_close(output);
pgp_output_delete(output);
return outmem;
}
#if 0
unsigned
pgp_decrypt_file(pgp_io_t *io,
const char *infile,
const char *outfile,
pgp_keyring_t *secring,
pgp_keyring_t *pubring,
const unsigned use_armour,
const unsigned allow_overwrite,
const unsigned sshkeys,
void *passfp,
int numtries,
pgp_cbfunc_t *getpassfunc)
{
pgp_stream_t *parse = NULL;
const int printerrors = 1;
char *filename = NULL;
int fd_in;
int fd_out;
fd_in = pgp_setup_file_read(io, &parse, infile,
NULL,
write_parsed_cb,
0);
if (fd_in < 0) {
perror(infile);
return 0;
}
if (outfile) {
fd_out = pgp_setup_file_write(&parse->cbinfo.output, outfile,
allow_overwrite);
if (fd_out < 0) {
perror(outfile);
pgp_teardown_file_read(parse, fd_in);
return 0;
}
} else {
const int suffixlen = 4;
const char *suffix = infile + strlen(infile) - suffixlen;
unsigned filenamelen;
if (strcmp(suffix, ".gpg") == 0 ||
strcmp(suffix, ".asc") == 0) {
filenamelen = (unsigned)(strlen(infile) - strlen(suffix));
if ((filename = calloc(1, filenamelen + 1)) == NULL) {
(void) fprintf(stderr, "can't allocate %" PRIsize "d bytes\n",
(size_t)(filenamelen + 1));
return 0;
}
(void) strncpy(filename, infile, filenamelen);
filename[filenamelen] = 0x0;
}
fd_out = pgp_setup_file_write(&parse->cbinfo.output,
filename, allow_overwrite);
if (fd_out < 0) {
perror(filename);
free(filename);
pgp_teardown_file_read(parse, fd_in);
return 0;
}
}
parse->cbinfo.cryptinfo.secring = secring;
parse->cbinfo.passfp = passfp;
parse->cbinfo.cryptinfo.getpassphrase = getpassfunc;
parse->cbinfo.cryptinfo.pubring = pubring;
parse->cbinfo.sshseckey = (sshkeys) ? &secring->keys[0].key.seckey : NULL;
parse->cbinfo.numtries = numtries;
if (use_armour) {
pgp_reader_push_dearmour(parse);
}
pgp_parse(parse, printerrors);
if (use_armour) {
pgp_reader_pop_dearmour(parse);
}
if (!parse->cbinfo.gotpass) {
(void) unlink((filename) ? filename : outfile);
}
if (filename) {
pgp_teardown_file_write(parse->cbinfo.output, fd_out);
free(filename);
}
pgp_teardown_file_read(parse, fd_in);
return 1;
}
#endif
pgp_memory_t *
pgp_decrypt_buf(pgp_io_t *io,
const void *input,
const size_t insize,
pgp_keyring_t *secring,
pgp_keyring_t *pubring,
const unsigned use_armour,
const unsigned sshkeys,
const char* symm_passphrase)
{
pgp_stream_t *parse = NULL;
pgp_memory_t *outmem;
pgp_memory_t *inmem;
const int printerrors = 1;
if (input == NULL) {
(void) fprintf(io->errs,
"pgp_encrypt_buf: null memory\n");
return 0;
}
inmem = pgp_memory_new();
pgp_memory_add(inmem, input, insize);
pgp_setup_memory_read(io, &parse, inmem,
NULL,
write_parsed_cb,
0);
pgp_setup_memory_write(&parse->cbinfo.output, &outmem, insize);
parse->cbinfo.cryptinfo.secring = secring;
parse->cbinfo.cryptinfo.pubring = pubring;
parse->cbinfo.sshseckey = (sshkeys) ? &secring->keys[0].key.seckey : NULL;
parse->cbinfo.cryptinfo.symm_passphrase = symm_passphrase;
if (use_armour) {
pgp_reader_push_dearmour(parse);
}
pgp_parse(parse, printerrors);
if (use_armour) {
pgp_reader_pop_dearmour(parse);
}
pgp_writer_close(parse->cbinfo.output);
pgp_output_delete(parse->cbinfo.output);
if (!parse->cbinfo.gotpass) {
pgp_memory_free(outmem);
outmem = NULL;
}
pgp_teardown_memory_read(parse, inmem);
return outmem;
}
static pgp_cb_ret_t
pgp_decrypt_and_validate_cb(const pgp_packet_t *pkt, pgp_cbdata_t *cbinfo)
{
pgp_cb_ret_t ret_write_cb = PGP_RELEASE_MEMORY;
pgp_cb_ret_t ret_validate_cb = PGP_RELEASE_MEMORY;
ret_write_cb = write_parsed_cb(pkt, cbinfo);
switch (pkt->tag) {
case PGP_PTAG_CT_LITDATA_BODY:
case PGP_PTAG_CT_SIGNED_CLEARTEXT_BODY:
case PGP_PTAG_CT_SIGNATURE:
case PGP_PTAG_CT_SIGNATURE_FOOTER:
ret_validate_cb = validate_data_cb(pkt, cbinfo);
break;
default:
break;
}
return (ret_write_cb == PGP_KEEP_MEMORY ||
ret_validate_cb == PGP_KEEP_MEMORY) ?
PGP_KEEP_MEMORY : PGP_RELEASE_MEMORY;
}
pgp_memory_t *
pgp_decrypt_and_validate_buf(pgp_io_t *io,
pgp_validation_t *result,
const void *input,
const size_t insize,
pgp_keyring_t *secring,
pgp_keyring_t *pubring,
const unsigned use_armour,
key_id_t **recipients_key_ids,
unsigned *recipients_count)
{
const unsigned sshkeys = 0;
validate_data_cb_t validation;
pgp_stream_t *stream = NULL;
pgp_memory_t *outmem;
pgp_memory_t *inmem;
const int printerrors = 1;
if (input == NULL) {
(void) fprintf(io->errs,
"pgp_encrypt_buf: null memory\n");
return 0;
}
inmem = pgp_memory_new();
pgp_memory_add(inmem, input, insize);
pgp_setup_memory_read(io, &stream, inmem,
&validation,
pgp_decrypt_and_validate_cb,
1);
(void) memset(&validation, 0x0, sizeof(validation));
validation.result = result;
validation.keyring = pubring;
validation.mem = pgp_memory_new();
pgp_memory_init(validation.mem, 128);
pgp_setup_memory_write(&stream->cbinfo.output, &outmem, insize);
stream->cbinfo.cryptinfo.secring = secring;
stream->cbinfo.cryptinfo.pubring = pubring;
stream->cbinfo.sshseckey = (sshkeys) ? &secring->keys[0].key.seckey : NULL;
if (use_armour) {
pgp_reader_push_dearmour(stream);
}
pgp_parse(stream, printerrors);
if (use_armour) {
pgp_reader_pop_dearmour(stream);
}
*recipients_count = stream->cbinfo.cryptinfo.recipients_key_idsc;
if (*recipients_count == 0) {
*recipients_key_ids = NULL;
} else {
*recipients_key_ids = calloc(sizeof(key_id_t),*recipients_count);
if( *recipients_key_ids != NULL)
{
memcpy(*recipients_key_ids,
stream->cbinfo.cryptinfo.recipients_key_idss,
sizeof(key_id_t) * *recipients_count);
}
}
if( *recipients_key_ids == NULL)
{
pgp_memory_free(outmem);
*recipients_count = 0;
outmem = NULL;
}
pgp_writer_close(stream->cbinfo.output);
pgp_output_delete(stream->cbinfo.output);
pgp_teardown_memory_read(stream, inmem);
pgp_memory_free(validation.mem);
return outmem;
}