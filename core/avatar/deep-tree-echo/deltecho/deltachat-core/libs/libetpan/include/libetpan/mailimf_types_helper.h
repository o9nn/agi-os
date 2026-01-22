#ifndef MAILIMF_TYPES_HELPER
#define MAILIMF_TYPES_HELPER
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/mailimf_types.h>
LIBETPAN_EXPORT
struct mailimf_mailbox_list *
mailimf_mailbox_list_new_empty(void);
LIBETPAN_EXPORT
int mailimf_mailbox_list_add(struct mailimf_mailbox_list * mailbox_list,
struct mailimf_mailbox * mb);
LIBETPAN_EXPORT
int mailimf_mailbox_list_add_parse(struct mailimf_mailbox_list * mailbox_list,
char * mb_str);
LIBETPAN_EXPORT
int mailimf_mailbox_list_add_mb(struct mailimf_mailbox_list * mailbox_list,
char * display_name, char * address);
LIBETPAN_EXPORT
struct mailimf_address_list *
mailimf_address_list_new_empty(void);
LIBETPAN_EXPORT
int mailimf_address_list_add(struct mailimf_address_list * address_list,
struct mailimf_address * addr);
LIBETPAN_EXPORT
int mailimf_address_list_add_parse(struct mailimf_address_list * address_list,
char * addr_str);
LIBETPAN_EXPORT
int mailimf_address_list_add_mb(struct mailimf_address_list * address_list,
char * display_name, char * address);
LIBETPAN_EXPORT
int
mailimf_resent_fields_add_data(struct mailimf_fields * fields,
struct mailimf_date_time * resent_date,
struct mailimf_mailbox_list * resent_from,
struct mailimf_mailbox * resent_sender,
struct mailimf_address_list * resent_to,
struct mailimf_address_list * resent_cc,
struct mailimf_address_list * resent_bcc,
char * resent_msg_id);
LIBETPAN_EXPORT
struct mailimf_fields *
mailimf_resent_fields_new_with_data_all(struct mailimf_date_time *
resent_date, struct mailimf_mailbox_list * resent_from,
struct mailimf_mailbox * resent_sender,
struct mailimf_address_list * resent_to,
struct mailimf_address_list * resent_cc,
struct mailimf_address_list * resent_bcc,
char * resent_msg_id);
LIBETPAN_EXPORT
struct mailimf_fields *
mailimf_resent_fields_new_with_data(struct mailimf_mailbox_list * from,
struct mailimf_mailbox * sender,
struct mailimf_address_list * to,
struct mailimf_address_list * cc,
struct mailimf_address_list * bcc);
LIBETPAN_EXPORT
struct mailimf_fields *
mailimf_fields_new_empty(void);
LIBETPAN_EXPORT
int mailimf_fields_add(struct mailimf_fields * fields,
struct mailimf_field * field);
LIBETPAN_EXPORT
int mailimf_fields_add_data(struct mailimf_fields * fields,
struct mailimf_date_time * date,
struct mailimf_mailbox_list * from,
struct mailimf_mailbox * sender,
struct mailimf_address_list * reply_to,
struct mailimf_address_list * to,
struct mailimf_address_list * cc,
struct mailimf_address_list * bcc,
char * msg_id,
clist * in_reply_to,
clist * references,
char * subject);
LIBETPAN_EXPORT
struct mailimf_fields *
mailimf_fields_new_with_data_all(struct mailimf_date_time * date,
struct mailimf_mailbox_list * from,
struct mailimf_mailbox * sender,
struct mailimf_address_list * reply_to,
struct mailimf_address_list * to,
struct mailimf_address_list * cc,
struct mailimf_address_list * bcc,
char * message_id,
clist * in_reply_to,
clist * references,
char * subject);
LIBETPAN_EXPORT
struct mailimf_fields *
mailimf_fields_new_with_data(struct mailimf_mailbox_list * from,
struct mailimf_mailbox * sender,
struct mailimf_address_list * reply_to,
struct mailimf_address_list * to,
struct mailimf_address_list * cc,
struct mailimf_address_list * bcc,
clist * in_reply_to,
clist * references,
char * subject);
LIBETPAN_EXPORT
char * mailimf_get_message_id(void);
LIBETPAN_EXPORT
struct mailimf_date_time * mailimf_get_current_date(void);
LIBETPAN_EXPORT
struct mailimf_date_time * mailimf_get_date(time_t time);
LIBETPAN_EXPORT
void mailimf_single_fields_init(struct mailimf_single_fields * single_fields,
struct mailimf_fields * fields);
LIBETPAN_EXPORT
struct mailimf_single_fields *
mailimf_single_fields_new(struct mailimf_fields * fields);
LIBETPAN_EXPORT
void mailimf_single_fields_free(struct mailimf_single_fields *
single_fields);
LIBETPAN_EXPORT
struct mailimf_field * mailimf_field_new_custom(char * name, char * value);
#ifdef __cplusplus
}
#endif
#endif