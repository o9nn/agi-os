#ifndef READERWRITER_H_
#define READERWRITER_H_
#include "create.h"
#include "memory.h"
#define USE_MMAP_FOR_FILES      1
void pgp_reader_set_fd(pgp_stream_t *, int);
void pgp_reader_set_mmap(pgp_stream_t *, int);
void pgp_reader_set_memory(pgp_stream_t *, const void *, size_t);
void pgp_reader_push_sum16(pgp_stream_t *);
uint16_t pgp_reader_pop_sum16(pgp_stream_t *);
void pgp_reader_push_se_ip_data(pgp_stream_t *, pgp_crypt_t *,
pgp_region_t *);
void pgp_reader_pop_se_ip_data(pgp_stream_t *);
unsigned pgp_write_mdc(pgp_output_t *, const uint8_t *);
unsigned pgp_write_se_ip_pktset(pgp_output_t *, const uint8_t *,
const unsigned,
pgp_crypt_t *);
void pgp_push_enc_crypt(pgp_output_t *, pgp_crypt_t *);
int pgp_push_enc_se_ip(pgp_output_t *, const pgp_keyring_t *, const char *, unsigned);
void pgp_push_checksum_writer(pgp_output_t *, pgp_seckey_t *);
unsigned pgp_pop_skey_checksum_writer(pgp_output_t *);
void pgp_setup_memory_write(pgp_output_t **, pgp_memory_t **, size_t);
void pgp_teardown_memory_write(pgp_output_t *, pgp_memory_t *);
void pgp_setup_memory_read(pgp_io_t *,
pgp_stream_t **,
const pgp_memory_t *,
void *,
pgp_cb_ret_t callback(const pgp_packet_t *,
pgp_cbdata_t *),
unsigned);
void pgp_teardown_memory_read(pgp_stream_t *, pgp_memory_t *);
int pgp_setup_file_write(pgp_output_t **, const char *, unsigned);
void pgp_teardown_file_write(pgp_output_t *, int);
int pgp_setup_file_append(pgp_output_t **, const char *);
void pgp_teardown_file_append(pgp_output_t *, int);
int pgp_setup_file_read(pgp_io_t *,
pgp_stream_t **,
const char *,
void *,
pgp_cb_ret_t callback(const pgp_packet_t *,
pgp_cbdata_t *),
unsigned);
void pgp_teardown_file_read(pgp_stream_t *, int);
unsigned pgp_reader_set_accumulate(pgp_stream_t *, unsigned);
pgp_cb_ret_t pgp_litdata_cb(const pgp_packet_t *, pgp_cbdata_t *);
pgp_cb_ret_t pgp_pk_sesskey_cb(const pgp_packet_t *, pgp_cbdata_t *);
pgp_cb_ret_t pgp_get_seckey_cb(const pgp_packet_t *, pgp_cbdata_t *);
int pgp_getpassphrase(void *, char *, size_t);
#endif