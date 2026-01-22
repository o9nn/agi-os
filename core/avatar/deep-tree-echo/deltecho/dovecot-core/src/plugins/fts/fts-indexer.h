#ifndef FTS_BUILD_H
#define FTS_BUILD_H
struct fts_backend;
struct fts_indexer_context;
int fts_indexer_init(struct fts_backend *backend, struct mailbox *box,
struct fts_indexer_context **ctx_r);
int fts_indexer_deinit(struct fts_indexer_context **ctx);
int fts_indexer_more(struct fts_indexer_context *ctx);
int fts_indexer_cmd(struct mail_user *user, const char *cmd,
struct event *event, const char **path_r);
#endif