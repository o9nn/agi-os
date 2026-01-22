#ifndef JSON_OSTREAM_H
#define JSON_OSTREAM_H
#include "lib.h"
#include "json-types.h"
#include "json-tree.h"
#include "json-generator.h"
struct json_ostream;
struct json_ostream *
json_ostream_create(struct ostream *output,
enum json_generator_flags gen_flags);
struct json_ostream *
json_ostream_create_str(string_t *buf,
enum json_generator_flags gen_flags);
void json_ostream_ref(struct json_ostream *stream);
void json_ostream_unref(struct json_ostream **_stream);
void json_ostream_destroy(struct json_ostream **_stream);
void json_ostream_close(struct json_ostream *stream);
bool json_ostream_is_closed(struct json_ostream *stream) ATTR_PURE;
void json_ostream_set_format(struct json_ostream *stream,
const struct json_format *format);
unsigned int json_ostream_get_write_node_level(struct json_ostream *stream);
void json_ostream_cork(struct json_ostream *stream);
void json_ostream_uncork(struct json_ostream *stream);
bool json_ostream_is_corked(struct json_ostream *stream);
int json_ostream_flush(struct json_ostream *stream);
void json_ostream_nflush(struct json_ostream *stream);
const char *json_ostream_get_error(struct json_ostream *stream);
int json_ostream_nfinish(struct json_ostream *stream);
void json_ostream_nfinish_destroy(struct json_ostream **_stream);
void json_ostream_ignore_last_errors(struct json_ostream *stream);
void json_ostream_set_no_error_handling(struct json_ostream *stream, bool set);
int json_ostream_write_object_member(struct json_ostream *stream,
const char *name);
void json_ostream_nwrite_object_member(struct json_ostream *stream,
const char *name);
int json_ostream_write_value(struct json_ostream *stream,
const char *name, enum json_type type,
const struct json_value *value);
void json_ostream_nwrite_value(struct json_ostream *stream,
const char *name, enum json_type type,
const struct json_value *value);
int json_ostream_write_node(struct json_ostream *stream,
const struct json_node *node, bool copy);
void json_ostream_nwrite_node(struct json_ostream *stream,
const struct json_node *node);
int json_ostream_write_number(struct json_ostream *stream,
const char *name, intmax_t number);
void json_ostream_nwrite_number(struct json_ostream *stream,
const char *name, intmax_t number);
int json_ostream_write_number_raw(struct json_ostream *stream,
const char *name, const char *number);
void json_ostream_nwrite_number_raw(struct json_ostream *stream,
const char *name, const char *number);
int json_ostream_write_string_data(struct json_ostream *stream,
const char *name,
const void *data, size_t size);
void json_ostream_nwrite_string_data(struct json_ostream *stream,
const char *name,
const void *data, size_t size);
static inline int
json_ostream_write_string_buffer(struct json_ostream *stream,
const char *name, const buffer_t *buf)
{
return json_ostream_write_string_data(stream, name,
buf->data, buf->used);
}
static inline void
json_ostream_nwrite_string_buffer(struct json_ostream *stream,
const char *name, const buffer_t *buf)
{
json_ostream_nwrite_string_data(stream, name, buf->data, buf->used);
}
int json_ostream_write_string(struct json_ostream *stream,
const char *name, const char *str);
void json_ostream_nwrite_string(struct json_ostream *stream,
const char *name, const char *str);
void json_ostream_nwritef_string(struct json_ostream *stream,
const char *name,
const char *format, ...) ATTR_FORMAT(3, 4);
int json_ostream_write_string_stream(struct json_ostream *stream,
const char *name, struct istream *input);
void json_ostream_nwrite_string_stream(struct json_ostream *stream,
const char *name, struct istream *input);
int json_ostream_open_string(struct json_ostream *stream, const char *name);
void json_ostream_nopen_string(struct json_ostream *stream, const char *name);
int json_ostream_close_string(struct json_ostream *stream);
void json_ostream_nclose_string(struct json_ostream *stream);
int json_ostream_write_null(struct json_ostream *stream, const char *name);
void json_ostream_nwrite_null(struct json_ostream *stream, const char *name);
int json_ostream_write_false(struct json_ostream *stream, const char *name);
void json_ostream_nwrite_false(struct json_ostream *stream, const char *name);
int json_ostream_write_true(struct json_ostream *stream, const char *name);
void json_ostream_nwrite_true(struct json_ostream *stream, const char *name);
int json_ostream_write_bool(struct json_ostream *stream,
const char *name, bool value);
void json_ostream_nwrite_bool(struct json_ostream *stream,
const char *name, bool value);
int json_ostream_descend_object(struct json_ostream *stream,
const char *name);
void json_ostream_ndescend_object(struct json_ostream *stream,
const char *name);
int json_ostream_ascend_object(struct json_ostream *stream);
void json_ostream_nascend_object(struct json_ostream *stream);
int json_ostream_descend_array(struct json_ostream *stream,
const char *name);
void json_ostream_ndescend_array(struct json_ostream *stream,
const char *name);
int json_ostream_ascend_array(struct json_ostream *stream);
void json_ostream_nascend_array(struct json_ostream *stream);
int json_ostream_write_text_data(struct json_ostream *stream,
const char *name,
const void *data, size_t size);
void json_ostream_nwrite_text_data(struct json_ostream *stream,
const char *name,
const void *data, size_t size);
int json_ostream_write_text(struct json_ostream *stream,
const char *name, const char *str);
void json_ostream_nwrite_text(struct json_ostream *stream,
const char *name, const char *str);
int json_ostream_write_text_stream(struct json_ostream *stream,
const char *name, struct istream *input);
void json_ostream_nwrite_text_stream(struct json_ostream *stream,
const char *name, struct istream *input);
int json_ostream_write_tree(struct json_ostream *stream, const char *name,
struct json_tree *jtree);
void json_ostream_nwrite_tree(struct json_ostream *stream, const char *name,
const struct json_tree *jtree);
int json_ostream_open_string_stream(struct json_ostream *stream,
const char *name,
struct ostream **ostream_r);
struct ostream *
json_ostream_nopen_string_stream(struct json_ostream *stream, const char *name);
int json_ostream_open_space(struct json_ostream *stream, const char *name);
void json_ostream_nopen_space(struct json_ostream *stream, const char *name);
void json_ostream_close_space(struct json_ostream *stream);
#endif