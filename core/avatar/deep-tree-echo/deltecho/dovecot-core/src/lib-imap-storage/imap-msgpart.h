#ifndef IMAP_MSGPART_H
#define IMAP_MSGPART_H
struct imap_msgpart;
struct imap_msgpart_open_result {
struct istream *input;
uoff_t size;
enum mail_fetch_field size_field;
bool binary_decoded_input_has_nuls;
};
struct imap_msgpart *imap_msgpart_full(void);
struct imap_msgpart *imap_msgpart_header(void);
struct imap_msgpart *imap_msgpart_body(void);
int imap_msgpart_parse(const char *section, struct imap_msgpart **msgpart_r);
void imap_msgpart_free(struct imap_msgpart **msgpart);
bool imap_msgpart_contains_body(const struct imap_msgpart *msgpart);
void imap_msgpart_set_decode_to_binary(struct imap_msgpart *msgpart);
void imap_msgpart_set_partial(struct imap_msgpart *msgpart,
uoff_t offset, uoff_t size);
uoff_t imap_msgpart_get_partial_offset(struct imap_msgpart *msgpart);
uoff_t imap_msgpart_get_partial_size(struct imap_msgpart *msgpart);
enum mail_fetch_field imap_msgpart_get_fetch_data(struct imap_msgpart *msgpart);
void imap_msgpart_get_wanted_headers(struct imap_msgpart *msgpart,
ARRAY_TYPE(const_string) *headers);
int imap_msgpart_open(struct mail *mail, struct imap_msgpart *msgpart,
struct imap_msgpart_open_result *result_r);
int imap_msgpart_size(struct mail *mail, struct imap_msgpart *msgpart,
uoff_t *size_r);
int imap_msgpart_bodypartstructure(struct mail *mail,
struct imap_msgpart *msgpart,
const char **bpstruct_r);
void imap_msgpart_close_mailbox(struct imap_msgpart *msgpart);
#endif