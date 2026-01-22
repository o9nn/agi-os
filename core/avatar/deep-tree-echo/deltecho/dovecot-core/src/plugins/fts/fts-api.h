#ifndef FTS_API_H
#define FTS_API_H
struct mail;
struct mailbox;
struct mail_namespace;
struct mail_search_arg;
struct fts_backend;
#include "seq-range-array.h"
enum fts_lookup_flags {
FTS_LOOKUP_FLAG_AND_ARGS	= 0x01,
FTS_LOOKUP_FLAG_NO_AUTO_FUZZY	= 0x02
};
enum fts_backend_build_key_type {
FTS_BACKEND_BUILD_KEY_HDR,
FTS_BACKEND_BUILD_KEY_MIME_HDR,
FTS_BACKEND_BUILD_KEY_BODY_PART,
FTS_BACKEND_BUILD_KEY_BODY_PART_BINARY
};
struct fts_backend_build_key {
uint32_t uid;
enum fts_backend_build_key_type type;
struct message_part *part;
const char *hdr_name;
const char *body_content_type;
const char *body_content_disposition;
};
struct fts_score_map {
uint32_t uid;
float score;
};
ARRAY_DEFINE_TYPE(fts_score_map, struct fts_score_map);
struct fts_search_state;
struct fts_result {
pool_t pool;
struct fts_search_state *search_state;
struct mailbox *box;
ARRAY_TYPE(seq_range) definite_uids;
ARRAY_TYPE(seq_range) maybe_uids;
ARRAY_TYPE(fts_score_map) scores;
bool scores_sorted;
};
struct fts_multi_result {
pool_t pool;
struct fts_search_state *search_state;
struct fts_result *box_results;
};
extern struct event_category event_category_fts;
int fts_backend_init(const char *backend_name, struct mail_namespace *ns,
const char **error_r, struct fts_backend **backend_r);
void fts_backend_deinit(struct fts_backend **backend);
int fts_backend_get_last_uid(struct fts_backend *backend, struct mailbox *box,
uint32_t *last_uid_r);
int fts_backend_is_uid_indexed(struct fts_backend *backend, struct mailbox *box,
uint32_t uid, uint32_t *last_indexed_uid_r);
bool fts_backend_is_updating(struct fts_backend *backend);
struct fts_backend_update_context *
fts_backend_update_init(struct fts_backend *backend);
int fts_backend_update_deinit(struct fts_backend_update_context **ctx);
void fts_backend_update_set_mailbox(struct fts_backend_update_context *ctx,
struct mailbox *box);
void fts_backend_update_expunge(struct fts_backend_update_context *ctx,
uint32_t uid);
bool fts_backend_update_set_build_key(struct fts_backend_update_context *ctx,
const struct fts_backend_build_key *key);
void fts_backend_update_unset_build_key(struct fts_backend_update_context *ctx);
int fts_backend_update_build_more(struct fts_backend_update_context *ctx,
const unsigned char *data, size_t size);
int fts_backend_refresh(struct fts_backend *backend, struct mailbox *box);
int fts_backend_rescan(struct fts_backend *backend);
int fts_backend_optimize(struct fts_backend *backend);
bool fts_backend_can_lookup(struct fts_backend *backend,
const struct mail_search_arg *args);
int fts_backend_lookup(struct fts_backend *backend, struct mailbox *box,
struct mail_search_arg *args,
enum fts_lookup_flags flags,
struct fts_result *result);
int fts_backend_lookup_multi(struct fts_backend *backend,
struct mailbox *const boxes[],
struct mail_search_arg *args,
enum fts_lookup_flags flags,
struct fts_multi_result *result);
void fts_backend_lookup_done(struct fts_backend *backend);
#endif