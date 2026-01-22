#ifndef PACKET_PARSE_H_
#define PACKET_PARSE_H_
#include "types.h"
#include "packet.h"
typedef struct pgp_region_t {
struct pgp_region_t	*parent;
unsigned		 length;
unsigned		 readc;
unsigned		 last_read;
unsigned		 indeterminate:1;
} pgp_region_t;
void pgp_init_subregion(pgp_region_t *, pgp_region_t *);
typedef enum {
PGP_RELEASE_MEMORY,
PGP_KEEP_MEMORY,
PGP_FINISHED
} pgp_cb_ret_t;
typedef struct pgp_cbdata_t	 pgp_cbdata_t;
typedef pgp_cb_ret_t pgp_cbfunc_t(const pgp_packet_t *,
pgp_cbdata_t *);
pgp_cb_ret_t
get_passphrase_cb(const pgp_packet_t *, pgp_cbdata_t *);
typedef struct pgp_stream_t	pgp_stream_t;
typedef struct pgp_reader_t		pgp_reader_t;
typedef struct pgp_cryptinfo_t	pgp_cryptinfo_t;
typedef int pgp_reader_func_t(pgp_stream_t *, void *, size_t, pgp_error_t **,
pgp_reader_t *, pgp_cbdata_t *);
typedef void pgp_reader_destroyer_t(pgp_reader_t *);
void pgp_stream_delete(pgp_stream_t *);
pgp_error_t *pgp_stream_get_errors(pgp_stream_t *);
pgp_crypt_t *pgp_get_decrypt(pgp_stream_t *);
void pgp_set_callback(pgp_stream_t *, pgp_cbfunc_t *, void *);
void pgp_callback_push(pgp_stream_t *, pgp_cbfunc_t *, void *);
void *pgp_callback_arg(pgp_cbdata_t *);
void *pgp_callback_errors(pgp_cbdata_t *);
void pgp_reader_set(pgp_stream_t *, pgp_reader_func_t *,
pgp_reader_destroyer_t *, void *);
void pgp_reader_push(pgp_stream_t *, pgp_reader_func_t *,
pgp_reader_destroyer_t *, void *);
void pgp_reader_pop(pgp_stream_t *);
void *pgp_reader_get_arg(pgp_reader_t *);
pgp_cb_ret_t pgp_callback(const pgp_packet_t *,
pgp_cbdata_t *);
pgp_cb_ret_t pgp_stacked_callback(const pgp_packet_t *,
pgp_cbdata_t *);
pgp_reader_t *pgp_readinfo(pgp_stream_t *);
int pgp_parse(pgp_stream_t *, const int);
typedef enum {
PGP_PARSE_RAW,
PGP_PARSE_PARSED,
PGP_PARSE_IGNORE
} pgp_parse_type_t;
void pgp_parse_options(pgp_stream_t *, pgp_content_enum,
pgp_parse_type_t);
unsigned pgp_limited_read(pgp_stream_t *, uint8_t *, size_t, pgp_region_t *,
pgp_error_t **, pgp_reader_t *,
pgp_cbdata_t *);
unsigned pgp_stacked_limited_read(pgp_stream_t *, uint8_t *, unsigned,
pgp_region_t *, pgp_error_t **,
pgp_reader_t *, pgp_cbdata_t *);
void pgp_parse_hash_init(pgp_stream_t *, pgp_hash_alg_t,
const uint8_t *);
void pgp_parse_hash_data(pgp_stream_t *, const void *, size_t);
void pgp_parse_hash_finish(pgp_stream_t *);
#if 0
pgp_hash_t *pgp_parse_hash_find(pgp_stream_t *, const uint8_t *);
#endif
pgp_reader_func_t    pgp_stacked_read;
int pgp_decompress(pgp_region_t *, pgp_stream_t *,
pgp_compression_type_t);
unsigned pgp_writez(pgp_output_t *, const uint8_t *,
const unsigned);
void
copy_sig_info(pgp_sig_info_t *dst, const pgp_sig_info_t *src);
#endif