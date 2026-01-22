#ifndef MESSAGE_SIZE_H
#define MESSAGE_SIZE_H
struct message_size {
uoff_t physical_size;
uoff_t virtual_size;
unsigned int lines;
};
int message_get_header_size(struct istream *input, struct message_size *hdr,
bool *has_nuls_r);
int message_get_body_size(struct istream *input, struct message_size *body,
bool *has_nuls_r);
void message_size_add(struct message_size *dest,
const struct message_size *src);
int message_skip_virtual(struct istream *input, uoff_t virtual_skip,
bool *last_cr_r);
#endif