#ifndef WRITER_H_
#define WRITER_H_
#include "types.h"
#include "packet.h"
#include "crypto.h"
#include "errors.h"
#include "keyring.h"
typedef struct pgp_writer_t	pgp_writer_t;
typedef unsigned pgp_writer_func_t(const uint8_t *,
unsigned,
pgp_error_t **,
pgp_writer_t *);
typedef unsigned
pgp_writer_finaliser_t(pgp_error_t **, pgp_writer_t *);
typedef void    pgp_writer_destroyer_t(pgp_writer_t *);
struct pgp_writer_t {
pgp_writer_func_t	 *writer;
pgp_writer_finaliser_t *finaliser;
pgp_writer_destroyer_t *destroyer;
void			 *arg;
pgp_writer_t	 	 *next;
pgp_io_t		 *io;
};
void *pgp_writer_get_arg(pgp_writer_t *);
void pgp_writer_set(pgp_output_t *,
pgp_writer_func_t *,
pgp_writer_finaliser_t *,
pgp_writer_destroyer_t *,
void *);
void pgp_writer_push(pgp_output_t *,
pgp_writer_func_t *,
pgp_writer_finaliser_t *,
pgp_writer_destroyer_t *,
void *);
void pgp_writer_pop(pgp_output_t *);
unsigned pgp_writer_passthrough(const uint8_t *,
unsigned,
pgp_error_t **,
pgp_writer_t *);
void pgp_writer_set_fd(pgp_output_t *, int);
unsigned pgp_writer_close(pgp_output_t *);
unsigned pgp_write(pgp_output_t *, const void *, unsigned);
unsigned pgp_write_length(pgp_output_t *, unsigned);
unsigned pgp_write_ptag(pgp_output_t *, pgp_content_enum);
unsigned pgp_write_scalar(pgp_output_t *, unsigned, unsigned);
unsigned pgp_write_mpi(pgp_output_t *, const BIGNUM *);
void pgp_writer_info_delete(pgp_writer_t *);
unsigned pgp_writer_info_finalise(pgp_error_t **, pgp_writer_t *);
#if 0
void pgp_push_stream_enc_se_ip(pgp_output_t *, pgp_key_t *, const char *);
#endif
void pgp_push_sum16_writer(pgp_output_t *output);
uint16_t pgp_pop_sum16_writer(pgp_output_t *output);
#endif