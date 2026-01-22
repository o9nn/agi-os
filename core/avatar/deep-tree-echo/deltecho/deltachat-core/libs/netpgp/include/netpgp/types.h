#ifndef TYPES_H_
#define TYPES_H_
#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif
typedef struct pgp_io_t {
void *outs;
void *errs;
void *res;
} pgp_io_t;
typedef struct {
int type;
const char *string;
} pgp_map_t;
typedef pgp_map_t pgp_errcode_name_map_t;
typedef struct pgp_crypt_t pgp_crypt_t;
typedef struct pgp_hash_t pgp_hash_t;
typedef uint8_t pgp_ss_rr_code_t;
typedef struct pgp_packet_t pgp_packet_t;
typedef enum {
PGP_WF_DUMMY
} pgp_writer_flags_t;
typedef struct pgp_output_t pgp_output_t;
#endif