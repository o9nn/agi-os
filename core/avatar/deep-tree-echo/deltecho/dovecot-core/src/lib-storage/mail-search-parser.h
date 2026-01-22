#ifndef MAIL_SEARCH_PARSER_H
#define MAIL_SEARCH_PARSER_H
#define MAIL_SEARCH_PARSER_KEY_LIST "("
struct imap_arg;
struct mail_search_parser *
mail_search_parser_init_imap(const struct imap_arg *args);
struct mail_search_parser *
mail_search_parser_init_cmdline(const char *const args[]);
void mail_search_parser_deinit(struct mail_search_parser **parser);
int mail_search_parse_key(struct mail_search_parser *parser,
const char **key_r);
int mail_search_parse_string(struct mail_search_parser *parser,
const char **value_r);
bool mail_search_parse_skip_next(struct mail_search_parser *parser,
const char *str);
const char *mail_search_parser_get_error(struct mail_search_parser *parser);
#endif