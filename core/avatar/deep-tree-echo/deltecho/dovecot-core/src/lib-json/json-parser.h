#ifndef JSON_PARSER_H
#define JSON_PARSER_H
#include "json-types.h"
struct json_parser;
struct json_parser_state;
enum json_parser_flags {
JSON_PARSER_FLAG_STRICT = BIT(0),
JSON_PARSER_FLAG_STRINGS_ALLOW_NUL = BIT(1),
JSON_PARSER_FLAG_STRINGS_AS_DATA = BIT(2),
JSON_PARSER_FLAG_NUMBERS_AS_STRING = BIT(3),
JSON_PARSER_FLAG_ALLOW_BOM = BIT(4)
};
struct json_parser_callbacks {
void (*parse_list_open)(void *context, void *parent_context,
const char *name, bool object,
void **list_context_r);
void (*parse_list_close)(void *context, void *parent_context,
bool object);
void (*parse_object_member)(void *context, void *parent_context,
const char *name);
void (*parse_value)(void *context, void *parent_context,
const char *name, enum json_type type,
const struct json_value *value);
};
struct json_parser_location {
uoff_t offset;
uoff_t line;
uoff_t value_line;
uoff_t column;
};
struct json_parser *
json_parser_init(struct istream *input, const struct json_limits *limits,
enum json_parser_flags flags,
const struct json_parser_callbacks *callbacks,
void *context);
void json_parser_deinit(struct json_parser **_parser);
void ATTR_FORMAT(2, 3)
json_parser_error(struct json_parser *parser, const char *format, ...);
void json_parser_interrupt(struct json_parser *parser);
int json_parse_more(struct json_parser *parser, const char **error_r);
void json_parser_get_location(struct json_parser *parser,
struct json_parser_location *loc_r);
void json_parser_enable_string_stream(struct json_parser *parser,
size_t threshold, size_t max_buffer_size);
void json_parser_disable_string_stream(struct json_parser *parser);
#endif