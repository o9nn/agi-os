#ifndef IMAP_PARSER_H
#define IMAP_PARSER_H
#include "imap-arg.h"
enum imap_parser_flags {
IMAP_PARSE_FLAG_LITERAL_SIZE	= 0x01,
IMAP_PARSE_FLAG_NO_UNESCAPE	= 0x02,
IMAP_PARSE_FLAG_LITERAL_TYPE	= 0x04,
IMAP_PARSE_FLAG_ATOM_ALLCHARS	= 0x08,
IMAP_PARSE_FLAG_MULTILINE_STR	= 0x10,
IMAP_PARSE_FLAG_INSIDE_LIST	= 0x20,
IMAP_PARSE_FLAG_LITERAL8	= 0x40,
IMAP_PARSE_FLAG_SERVER_TEXT	= 0x80,
IMAP_PARSE_FLAG_STOP_AT_LIST	= 0x100
};
enum imap_parser_error {
IMAP_PARSE_ERROR_NONE = 0,
IMAP_PARSE_ERROR_BAD_SYNTAX,
IMAP_PARSE_ERROR_LINE_TOO_LONG,
IMAP_PARSE_ERROR_LITERAL_TOO_BIG
};
struct imap_parser;
struct imap_parser *
imap_parser_create(struct istream *input, struct ostream *output,
size_t max_line_size) ATTR_NULL(2);
void imap_parser_ref(struct imap_parser *parser);
void imap_parser_unref(struct imap_parser **parser);
void imap_parser_enable_literal_minus(struct imap_parser *parser);
void imap_parser_reset(struct imap_parser *parser);
void imap_parser_set_streams(struct imap_parser *parser, struct istream *input,
struct ostream *output) ATTR_NULL(3);
const char *imap_parser_get_error(struct imap_parser *parser,
enum imap_parser_error *error_r) ATTR_NULL(2);
int imap_parser_read_args(struct imap_parser *parser, unsigned int count,
enum imap_parser_flags flags,
const struct imap_arg **args_r);
bool imap_parser_get_literal_size(struct imap_parser *parser, uoff_t *size_r);
void imap_parser_read_last_literal(struct imap_parser *parser);
int imap_parser_finish_line(struct imap_parser *parser, unsigned int count,
enum imap_parser_flags flags,
const struct imap_arg **args_r);
const char *imap_parser_read_word(struct imap_parser *parser);
int imap_parser_read_tag(struct imap_parser *parser, const char **tag_r);
int imap_parser_read_command_name(struct imap_parser *parser,
const char **name_r);
int imap_parser_client_read_tag(struct imap_parser *parser,
const char **tag_r);
#endif