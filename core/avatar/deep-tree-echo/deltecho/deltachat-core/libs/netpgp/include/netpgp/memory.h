#ifndef MEMORY_H_
#define MEMORY_H_
#include <sys/types.h>
#include "packet.h"
typedef struct pgp_memory_t {
uint8_t		*buf;
size_t          length;
size_t          allocated;
unsigned	mmapped;
} pgp_memory_t;
pgp_memory_t   *pgp_memory_new(void);
void pgp_memory_free(pgp_memory_t *);
void pgp_memory_init(pgp_memory_t *, size_t);
void pgp_memory_pad(pgp_memory_t *, size_t);
void pgp_memory_add(pgp_memory_t *, const uint8_t *, size_t);
void pgp_memory_place_int(pgp_memory_t *, unsigned, unsigned, size_t);
void pgp_memory_make_packet(pgp_memory_t *, pgp_content_enum);
void pgp_memory_clear(pgp_memory_t *);
void pgp_memory_release(pgp_memory_t *);
void pgp_writer_set_memory(pgp_output_t *, pgp_memory_t *);
size_t pgp_mem_len(const pgp_memory_t *);
void *pgp_mem_data(const pgp_memory_t *);
int pgp_mem_readfile(pgp_memory_t *, const char *);
void pgp_random(void *, size_t);
#endif