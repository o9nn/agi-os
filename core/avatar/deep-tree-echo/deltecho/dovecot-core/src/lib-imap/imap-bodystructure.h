#ifndef IMAP_BODYSTRUCTURE_H
#define IMAP_BODYSTRUCTURE_H
struct message_part;
struct message_header_line;
struct imap_arg;
int imap_bodystructure_write(const struct message_part *part,
string_t *dest, bool extended,
const char **error_r);
int imap_bodystructure_parse_full(const char *bodystructure, pool_t pool,
struct message_part **parts, const char **error_r);
int imap_bodystructure_parse_args(const struct imap_arg *args, pool_t pool,
struct message_part **parts, const char **error_r);
int imap_bodystructure_parse(const char *bodystructure, pool_t pool,
struct message_part *parts, const char **error_r);
int imap_body_parse_from_bodystructure(const char *bodystructure,
string_t *dest, const char **error_r);
#endif