#ifndef JSON_ISTREAM_H
#define JSON_ISTREAM_H
#include "json-tree.h"
#include "json-parser.h"
struct json_istream;
enum json_istream_type {
JSON_ISTREAM_TYPE_NORMAL = 0,
JSON_ISTREAM_TYPE_ARRAY,
JSON_ISTREAM_TYPE_OBJECT,
};
struct json_istream *
json_istream_create(struct istream *input, enum json_istream_type type,
const struct json_limits *limits,
enum json_parser_flags parser_flags);
void json_istream_ref(struct json_istream *stream);
void json_istream_unref(struct json_istream **_stream);
void json_istream_destroy(struct json_istream **_stream);
void json_istream_close(struct json_istream *stream);
bool json_istream_is_closed(struct json_istream *stream) ATTR_PURE;
static inline struct json_istream *
json_istream_create_array(struct istream *input,
const struct json_limits *limits,
enum json_parser_flags parser_flags)
{
return json_istream_create(input, JSON_ISTREAM_TYPE_ARRAY,
limits, parser_flags);
}
static inline struct json_istream *
json_istream_create_object(struct istream *input,
const struct json_limits *limits,
enum json_parser_flags parser_flags)
{
return json_istream_create(input, JSON_ISTREAM_TYPE_OBJECT,
limits, parser_flags);
}
unsigned int json_istream_get_node_level(struct json_istream *stream);
bool json_istream_is_at_end(struct json_istream *stream);
bool json_istream_failed(struct json_istream *stream);
const char *json_istream_get_error(struct json_istream *stream);
void json_istream_get_location(struct json_istream *stream,
struct json_parser_location *loc_r);
int json_istream_finish(struct json_istream **_stream,
const char **error_r);
int json_istream_read(struct json_istream *stream,
struct json_node *node_r);
int json_istream_read_next(struct json_istream *stream,
struct json_node *node_r);
void json_istream_skip(struct json_istream *stream);
void json_istream_ignore(struct json_istream *stream, unsigned int count);
int json_istream_read_object_member(struct json_istream *stream,
const char **name_r);
int json_istream_descend(struct json_istream *stream,
struct json_node *node_r);
void json_istream_ascend(struct json_istream *stream);
void json_istream_ascend_to(struct json_istream *stream,
unsigned int node_level);
int json_istream_walk(struct json_istream *stream,
struct json_node *node_r);
int json_istream_read_stream(struct json_istream *stream,
size_t threshold, size_t max_buffer_size,
const char *temp_path_prefix,
struct json_node *node_r);
int json_istream_read_next_stream(struct json_istream *stream,
size_t threshold, size_t max_buffer_size,
const char *temp_path_prefix,
struct json_node *node_r);
int json_istream_walk_stream(struct json_istream *stream,
size_t threshold, size_t max_buffer_size,
const char *temp_path_prefix,
struct json_node *node_r);
int json_istream_read_tree(struct json_istream *stream,
struct json_tree **tree_r);
int json_istream_read_into_tree_node(struct json_istream *stream,
struct json_tree_node *tree_node);
int json_istream_read_into_tree(struct json_istream *stream,
struct json_tree *tree);
#endif