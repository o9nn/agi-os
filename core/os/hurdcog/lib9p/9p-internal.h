#ifndef _9P_INTERNAL_H
#define _9P_INTERNAL_H
#include "9p.h"
#include <pthread.h>
#include <sys/socket.h>
#define P9_MAX_MSG_SIZE  8192
#define P9_MIN_MSG_SIZE  1024
#define P9_MAX_STRING    256
#define P9_NOFID         (~0U)
#define P9_MAX_FID       65536
struct p9_connection {
int fd;
uint32_t msize;
uint16_t tag;
pthread_mutex_t lock;
pthread_mutex_t tag_lock;
char version[32];
uint32_t fid_pool;
struct p9_fid **fid_table;
pthread_mutex_t fid_lock;
};
struct p9_fid {
uint32_t fid;
struct p9_connection *conn;
struct p9_qid qid;
uint8_t mode;
int open;
};
struct p9_server {
int fd;
pthread_t thread;
int running;
p9_handler_t handlers[256];
pthread_mutex_t lock;
};
struct p9_message {
uint32_t size;
uint8_t type;
uint16_t tag;
uint8_t data[];
};
struct p9_namespace {
struct {
char *path;
struct p9_connection *conn;
} *mounts;
int nmounts;
int capacity;
pthread_mutex_t lock;
};
struct p9_auth {
char method[64];
void *state;
};
int p9_send_message(struct p9_connection *conn, struct p9_message *msg);
struct p9_message *p9_receive_message(struct p9_connection *conn);
void p9_free_message(struct p9_message *msg);
uint16_t p9_alloc_tag(struct p9_connection *conn);
void p9_free_tag(struct p9_connection *conn, uint16_t tag);
uint32_t p9_alloc_fid(struct p9_connection *conn);
void p9_free_fid(struct p9_connection *conn, uint32_t fid);
int p9_encode_string(uint8_t **buf, size_t *len, const char *str);
int p9_decode_string(uint8_t **buf, size_t *len, char **str);
int p9_encode_qid(uint8_t **buf, size_t *len, const struct p9_qid *qid);
int p9_decode_qid(uint8_t **buf, size_t *len, struct p9_qid *qid);
int p9_encode_stat(uint8_t **buf, size_t *len, const struct p9_stat *stat);
int p9_decode_stat(uint8_t **buf, size_t *len, struct p9_stat *stat);
void p9_init_qid(struct p9_qid *qid, uint8_t type, uint32_t version, uint64_t path);
void p9_free_stat(struct p9_stat *stat);
int p9_copy_stat(struct p9_stat *dest, const struct p9_stat *src);
#define P9_EIO          1
#define P9_EPROTO       2
#define P9_ENOMEM       3
#define P9_EINVAL       4
#define P9_ENOENT       5
#define P9_EACCES       6
#define P9_EEXIST       7
#define P9_EISDIR       8
#define P9_ENOTDIR      9
#define P9_EMFILE       10
#endif