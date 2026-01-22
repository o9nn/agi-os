#ifndef NETPGPDEFS_H_
#define NETPGPDEFS_H_	1
#define PRItime		"ll"
#ifdef WIN32
#define PRIsize		"I"
#else
#define PRIsize		"z"
#endif
#define __PGP_USED(x)	(void)&(x)
#ifndef __UNCONST
#define __UNCONST(a)	((void *)(unsigned long)(const void *)(a))
#endif
#define PGP_ARRAY_SIZE(a)       (sizeof(a)/sizeof(*(a)))
void            hexdump(FILE *, const char *, const uint8_t *, size_t);
const char     *pgp_str_from_map(int, pgp_map_t *);
int             pgp_set_debug_level(const char *);
int             pgp_get_debug_level(const char *);
void		*pgp_new(size_t);
#define NETPGP_BUFSIZ	8192
#define CALLBACK(t, cbinfo, pkt)	do {				\
(pkt)->tag = (t);						\
if (pgp_callback(pkt, cbinfo) == PGP_RELEASE_MEMORY) {	\
pgp_parser_content_free(pkt);				\
}								\
} while(0)
#ifndef MIN
#define MIN(x, y) (((x) < (y)) ? (x) : (y))
#endif
#endif