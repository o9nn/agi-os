#ifndef PACKET_SHOW_H_
#define PACKET_SHOW_H_
#include "packet.h"
typedef struct {
unsigned    size;
unsigned    used;
char          **strings;
} pgp_list_t;
typedef struct {
pgp_list_t	known;
pgp_list_t   	unknown;
} pgp_text_t;
typedef struct {
uint8_t		mask;
const char     *string;
} pgp_bit_map_t;
void pgp_text_init(pgp_text_t *);
void pgp_text_free(pgp_text_t *);
const char *pgp_show_packet_tag(pgp_content_enum);
const char *pgp_show_ss_type(pgp_content_enum);
const char *pgp_show_sig_type(pgp_sig_type_t);
const char *pgp_show_pka(pgp_pubkey_alg_t);
const char *pgp_show_ss_zpref(uint8_t);
const char *pgp_show_hash_alg(uint8_t);
const char *pgp_show_symm_alg(uint8_t);
pgp_text_t *pgp_showall_ss_skapref(const pgp_data_t *);
const char *pgp_show_ss_skapref(uint8_t);
const char *pgp_show_ss_rr_code(pgp_ss_rr_code_t);
pgp_text_t *pgp_showall_ss_features(pgp_data_t);
const char *pgp_show_ss_key_flag(uint8_t, pgp_bit_map_t *);
const char *pgp_show_keyserv_pref(uint8_t, pgp_bit_map_t *);
pgp_text_t *pgp_showall_notation(pgp_ss_notation_t);
#endif