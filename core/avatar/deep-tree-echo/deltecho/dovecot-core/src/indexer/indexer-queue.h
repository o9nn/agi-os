#ifndef INDEXER_QUEUE_H
#define INDEXER_QUEUE_H
#include "indexer.h"
typedef void
indexer_queue_callback_t(const struct indexer_status *status, void *context);
enum indexer_request_type {
INDEXER_REQUEST_TYPE_INDEX,
INDEXER_REQUEST_TYPE_OPTIMIZE,
};
struct indexer_request {
struct indexer_request *prev, *next;
struct indexer_request *user_prev, *user_next;
char *username;
char *mailbox;
char *session_id;
unsigned int max_recent_msgs;
enum indexer_request_type type;
bool working:1;
bool reindex_head:1;
bool reindex_tail:1;
unsigned int working_context_idx;
ARRAY(void *) contexts;
};
struct indexer_queue *indexer_queue_init(indexer_queue_callback_t *callback);
void indexer_queue_deinit(struct indexer_queue **queue);
void indexer_queue_set_listen_callback(struct indexer_queue *queue,
void (*callback)(struct indexer_queue *));
void indexer_queue_append(struct indexer_queue *queue, bool append,
const char *username, const char *mailbox,
const char *session_id, unsigned int max_recent_msgs,
void *context);
void indexer_queue_append_optimize(struct indexer_queue *queue,
const char *username, const char *mailbox,
void *context);
void indexer_queue_cancel(struct indexer_queue *queue,
const char *username, const char *mailbox_mask);
void indexer_queue_cancel_all(struct indexer_queue *queue);
bool indexer_queue_is_empty(struct indexer_queue *queue);
unsigned int indexer_queue_count(struct indexer_queue *queue);
struct indexer_request *indexer_queue_request_peek(struct indexer_queue *queue);
void indexer_queue_request_remove(struct indexer_queue *queue);
void indexer_queue_request_status(struct indexer_queue *queue,
struct indexer_request *request,
const struct indexer_status *status);
void indexer_queue_move_head_to_tail(struct indexer_queue *queue);
void indexer_queue_request_work(struct indexer_request *request);
void indexer_queue_request_finish(struct indexer_queue *queue,
struct indexer_request **request,
enum indexer_state state);
struct indexer_queue_iter *
indexer_queue_iter_init(struct indexer_queue *queue, bool only_working);
struct indexer_request *indexer_queue_iter_next(struct indexer_queue_iter *iter);
void indexer_queue_iter_deinit(struct indexer_queue_iter **iter);
#endif