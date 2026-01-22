#ifndef MAILIMAP_TYPES_HELPER_H
#define MAILIMAP_TYPES_HELPER_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/mailimap_types.h>
LIBETPAN_EXPORT
struct mailimap_set_item * mailimap_set_item_new_single(uint32_t indx);
LIBETPAN_EXPORT
struct mailimap_set *
mailimap_set_new_single_item(struct mailimap_set_item * item);
LIBETPAN_EXPORT
struct mailimap_set * mailimap_set_new_interval(uint32_t first, uint32_t last);
LIBETPAN_EXPORT
struct mailimap_set * mailimap_set_new_single(uint32_t indx);
LIBETPAN_EXPORT
struct mailimap_set * mailimap_set_new_empty(void);
LIBETPAN_EXPORT
int mailimap_set_add(struct mailimap_set * set,
struct mailimap_set_item * set_item);
LIBETPAN_EXPORT
int mailimap_set_add_interval(struct mailimap_set * set,
uint32_t first, uint32_t last);
LIBETPAN_EXPORT
int mailimap_set_add_single(struct mailimap_set * set,
uint32_t indx);
LIBETPAN_EXPORT
struct mailimap_section * mailimap_section_new_header(void);
LIBETPAN_EXPORT
struct mailimap_section *
mailimap_section_new_header_fields(struct mailimap_header_list * header_list);
LIBETPAN_EXPORT
struct mailimap_section *
mailimap_section_new_header_fields_not(struct mailimap_header_list * header_list);
LIBETPAN_EXPORT
struct mailimap_section * mailimap_section_new_text(void);
LIBETPAN_EXPORT
struct mailimap_section *
mailimap_section_new_part(struct mailimap_section_part * part);
LIBETPAN_EXPORT
struct mailimap_section *
mailimap_section_new_part_mime(struct mailimap_section_part * part);
LIBETPAN_EXPORT
struct mailimap_section *
mailimap_section_new_part_header(struct mailimap_section_part * part);
LIBETPAN_EXPORT
struct mailimap_section *
mailimap_section_new_part_header_fields(struct mailimap_section_part *
part,
struct mailimap_header_list *
header_list);
LIBETPAN_EXPORT
struct mailimap_section *
mailimap_section_new_part_header_fields_not(struct mailimap_section_part
* part,
struct mailimap_header_list
* header_list);
LIBETPAN_EXPORT
struct mailimap_section *
mailimap_section_new_part_text(struct mailimap_section_part * part);
LIBETPAN_EXPORT
struct mailimap_fetch_att *
mailimap_fetch_att_new_envelope(void);
LIBETPAN_EXPORT
struct mailimap_fetch_att *
mailimap_fetch_att_new_flags(void);
LIBETPAN_EXPORT
struct mailimap_fetch_att *
mailimap_fetch_att_new_internaldate(void);
LIBETPAN_EXPORT
struct mailimap_fetch_att *
mailimap_fetch_att_new_rfc822(void);
LIBETPAN_EXPORT
struct mailimap_fetch_att *
mailimap_fetch_att_new_rfc822_header(void);
LIBETPAN_EXPORT
struct mailimap_fetch_att *
mailimap_fetch_att_new_rfc822_size(void);
LIBETPAN_EXPORT
struct mailimap_fetch_att *
mailimap_fetch_att_new_rfc822_text(void);
LIBETPAN_EXPORT
struct mailimap_fetch_att *
mailimap_fetch_att_new_body(void);
LIBETPAN_EXPORT
struct mailimap_fetch_att *
mailimap_fetch_att_new_bodystructure(void);
LIBETPAN_EXPORT
struct mailimap_fetch_att *
mailimap_fetch_att_new_uid(void);
LIBETPAN_EXPORT
struct mailimap_fetch_att *
mailimap_fetch_att_new_body_section(struct mailimap_section * section);
LIBETPAN_EXPORT
struct mailimap_fetch_att *
mailimap_fetch_att_new_body_peek_section(struct mailimap_section * section);
LIBETPAN_EXPORT
struct mailimap_fetch_att *
mailimap_fetch_att_new_body_section_partial(struct mailimap_section * section,
uint32_t offset, uint32_t size);
LIBETPAN_EXPORT
struct mailimap_fetch_att *
mailimap_fetch_att_new_body_peek_section_partial(struct mailimap_section * section,
uint32_t offset, uint32_t size);
LIBETPAN_EXPORT
struct mailimap_fetch_att *
mailimap_fetch_att_new_extension(char * ext_keyword);
LIBETPAN_EXPORT
struct mailimap_fetch_type *
mailimap_fetch_type_new_all(void);
LIBETPAN_EXPORT
struct mailimap_fetch_type *
mailimap_fetch_type_new_full(void);
LIBETPAN_EXPORT
struct mailimap_fetch_type *
mailimap_fetch_type_new_fast(void);
LIBETPAN_EXPORT
struct mailimap_fetch_type *
mailimap_fetch_type_new_fetch_att(struct mailimap_fetch_att * fetch_att);
LIBETPAN_EXPORT
struct mailimap_fetch_type *
mailimap_fetch_type_new_fetch_att_list(clist * fetch_att_list);
LIBETPAN_EXPORT
struct mailimap_fetch_type *
mailimap_fetch_type_new_fetch_att_list_empty(void);
LIBETPAN_EXPORT
int
mailimap_fetch_type_new_fetch_att_list_add(struct mailimap_fetch_type *
fetch_type,
struct mailimap_fetch_att *
fetch_att);
LIBETPAN_EXPORT
struct mailimap_store_att_flags *
mailimap_store_att_flags_new_set_flags(struct mailimap_flag_list * flags);
LIBETPAN_EXPORT
struct mailimap_store_att_flags *
mailimap_store_att_flags_new_set_flags_silent(struct mailimap_flag_list *
flags);
LIBETPAN_EXPORT
struct mailimap_store_att_flags *
mailimap_store_att_flags_new_add_flags(struct mailimap_flag_list * flags);
LIBETPAN_EXPORT
struct mailimap_store_att_flags *
mailimap_store_att_flags_new_add_flags_silent(struct mailimap_flag_list *
flags);
LIBETPAN_EXPORT
struct mailimap_store_att_flags *
mailimap_store_att_flags_new_remove_flags(struct mailimap_flag_list * flags);
LIBETPAN_EXPORT
struct mailimap_store_att_flags *
mailimap_store_att_flags_new_remove_flags_silent(struct mailimap_flag_list *
flags);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_all(void);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_bcc(char * sk_bcc);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_before(struct mailimap_date * sk_before);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_body(char * sk_body);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_cc(char * sk_cc);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_from(char * sk_from);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_keyword(char * sk_keyword);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_on(struct mailimap_date * sk_on);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_since(struct mailimap_date * sk_since);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_subject(char * sk_subject);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_text(char * sk_text);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_to(char * sk_to);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_unkeyword(char * sk_unkeyword);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_header(char * sk_header_name, char * sk_header_value);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_larger(uint32_t sk_larger);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_not(struct mailimap_search_key * sk_not);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_or(struct mailimap_search_key * sk_or1,
struct mailimap_search_key * sk_or2);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_sentbefore(struct mailimap_date * sk_sentbefore);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_senton(struct mailimap_date * sk_senton);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_sentsince(struct mailimap_date * sk_sentsince);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_smaller(uint32_t sk_smaller);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_uid(struct mailimap_set * sk_uid);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_set(struct mailimap_set * sk_set);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_multiple(clist * sk_multiple);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_multiple_empty(void);
LIBETPAN_EXPORT
int
mailimap_search_key_multiple_add(struct mailimap_search_key * keys,
struct mailimap_search_key * key_item);
LIBETPAN_EXPORT
struct mailimap_flag_list *
mailimap_flag_list_new_empty(void);
LIBETPAN_EXPORT
int mailimap_flag_list_add(struct mailimap_flag_list * flag_list,
struct mailimap_flag * f);
LIBETPAN_EXPORT
struct mailimap_flag * mailimap_flag_new_answered(void);
LIBETPAN_EXPORT
struct mailimap_flag * mailimap_flag_new_flagged(void);
LIBETPAN_EXPORT
struct mailimap_flag * mailimap_flag_new_deleted(void);
LIBETPAN_EXPORT
struct mailimap_flag * mailimap_flag_new_seen(void);
LIBETPAN_EXPORT
struct mailimap_flag * mailimap_flag_new_draft(void);
LIBETPAN_EXPORT
struct mailimap_flag * mailimap_flag_new_flag_keyword(char * flag_keyword);
LIBETPAN_EXPORT
struct mailimap_flag * mailimap_flag_new_flag_extension(char * flag_extension);
LIBETPAN_EXPORT
struct mailimap_status_att_list * mailimap_status_att_list_new_empty(void);
LIBETPAN_EXPORT
int
mailimap_status_att_list_add(struct mailimap_status_att_list * sa_list,
int status_att);
LIBETPAN_EXPORT
int mailimap_get_section_part_from_body(struct mailimap_body * root_part,
struct mailimap_body * part,
struct mailimap_section_part ** result);
#ifdef __cplusplus
}
#endif
#endif