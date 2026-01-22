#ifndef INDEX_ATTACHMENT_H
#define INDEX_ATTACHMENT_H
#include "sha1.h"
struct fs;
struct mail_save_context;
struct mail_storage;
struct mail_attachment_extref {
const char *path;
uoff_t start_offset;
uoff_t size;
unsigned int base64_blocks_per_line;
bool base64_have_crlf;
};
ARRAY_DEFINE_TYPE(mail_attachment_extref, struct mail_attachment_extref);
void index_attachment_save_begin(struct mail_save_context *ctx,
struct fs *fs, struct istream *input);
int index_attachment_save_continue(struct mail_save_context *ctx);
int index_attachment_save_finish(struct mail_save_context *ctx);
void index_attachment_save_free(struct mail_save_context *ctx);
const ARRAY_TYPE(mail_attachment_extref) *
index_attachment_save_get_extrefs(struct mail_save_context *ctx);
int index_attachment_delete(struct mail_storage *storage,
struct fs *fs, const char *name);
void index_attachment_append_extrefs(string_t *str,
const ARRAY_TYPE(mail_attachment_extref) *extrefs);
bool index_attachment_parse_extrefs(const char *line, pool_t pool,
ARRAY_TYPE(mail_attachment_extref) *extrefs);
int index_attachment_stream_get(struct fs *fs, const char *attachment_dir,
const char *path_suffix,
struct istream **stream, uoff_t full_size,
const char *ext_refs, const char **error_r);
#endif