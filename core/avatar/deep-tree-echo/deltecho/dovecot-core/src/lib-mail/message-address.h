#ifndef MESSAGE_ADDRESS_H
#define MESSAGE_ADDRESS_H
struct smtp_address;
enum message_address_parse_flags {
MESSAGE_ADDRESS_PARSE_FLAG_FILL_MISSING = BIT(0),
MESSAGE_ADDRESS_PARSE_FLAG_STRICT_DOTS = BIT(1),
};
struct message_address {
struct message_address *next;
const char *name;
const char *route;
const char *mailbox;
const char *domain;
bool invalid_syntax;
};
struct message_address *
message_address_parse(pool_t pool, const unsigned char *data, size_t size,
unsigned int max_addresses,
enum message_address_parse_flags flags);
int message_address_parse_path(pool_t pool, const unsigned char *data,
size_t size, struct message_address **addr_r);
void message_address_init(struct message_address *addr,
const char *name, const char *mailbox, const char *domain)
ATTR_NULL(1);
void message_address_init_from_smtp(struct message_address *addr,
const char *name, const struct smtp_address *smtp_addr)
ATTR_NULL(1);
void message_address_write(string_t *str, const struct message_address *addr);
const char *message_address_to_string(const struct message_address *addr);
const char *message_address_first_to_string(const struct message_address *addr);
bool message_header_is_address(const char *hdr_name);
#endif