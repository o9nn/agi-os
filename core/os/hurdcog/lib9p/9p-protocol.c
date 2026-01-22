#include "9p-internal.h"
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <netdb.h>
int p9_errno = 0;
static const char *p9_error_messages[] = {
[P9_EIO] = "I/O error",
[P9_EPROTO] = "Protocol error",
[P9_ENOMEM] = "Out of memory",
[P9_EINVAL] = "Invalid argument",
[P9_ENOENT] = "No such file or directory",
[P9_EACCES] = "Permission denied",
[P9_EEXIST] = "File exists",
[P9_EISDIR] = "Is a directory",
[P9_ENOTDIR] = "Not a directory",
[P9_EMFILE] = "Too many open files",
};
const char *
p9_strerror(int error)
{
if (error >= 0 && error < sizeof(p9_error_messages)/sizeof(char*))
return p9_error_messages[error];
return "Unknown error";
}
struct p9_connection *
p9_connect(const char *address, uint16_t port)
{
struct p9_connection *conn;
struct sockaddr_in addr;
struct hostent *host;
conn = calloc(1, sizeof(struct p9_connection));
if (!conn) {
p9_errno = P9_ENOMEM;
return NULL;
}
conn->fd = -1;
conn->msize = P9_MIN_MSG_SIZE;
conn->tag = 1;
conn->fid_pool = 1;
pthread_mutex_init(&conn->lock, NULL);
pthread_mutex_init(&conn->tag_lock, NULL);
pthread_mutex_init(&conn->fid_lock, NULL);
conn->fid_table = calloc(P9_MAX_FID, sizeof(struct p9_fid *));
if (!conn->fid_table) {
p9_errno = P9_ENOMEM;
goto error;
}
conn->fd = socket(AF_INET, SOCK_STREAM, 0);
if (conn->fd < 0) {
p9_errno = P9_EIO;
goto error;
}
host = gethostbyname(address);
if (!host) {
p9_errno = P9_EIO;
goto error;
}
memset(&addr, 0, sizeof(addr));
addr.sin_family = AF_INET;
addr.sin_port = htons(port);
memcpy(&addr.sin_addr, host->h_addr, host->h_length);
if (connect(conn->fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
p9_errno = P9_EIO;
goto error;
}
return conn;
error:
if (conn) {
if (conn->fd >= 0)
close(conn->fd);
if (conn->fid_table)
free(conn->fid_table);
free(conn);
}
return NULL;
}
void
p9_disconnect(struct p9_connection *conn)
{
if (!conn)
return;
pthread_mutex_lock(&conn->lock);
if (conn->fd >= 0) {
close(conn->fd);
conn->fd = -1;
}
if (conn->fid_table) {
for (int i = 0; i < P9_MAX_FID; i++) {
if (conn->fid_table[i]) {
free(conn->fid_table[i]);
}
}
free(conn->fid_table);
}
pthread_mutex_unlock(&conn->lock);
pthread_mutex_destroy(&conn->lock);
pthread_mutex_destroy(&conn->tag_lock);
pthread_mutex_destroy(&conn->fid_lock);
free(conn);
}
uint16_t
p9_alloc_tag(struct p9_connection *conn)
{
uint16_t tag;
pthread_mutex_lock(&conn->tag_lock);
tag = conn->tag++;
if (conn->tag == 0)
conn->tag = 1;
pthread_mutex_unlock(&conn->tag_lock);
return tag;
}
void
p9_free_tag(struct p9_connection *conn, uint16_t tag)
{
(void)conn;
(void)tag;
}
uint32_t
p9_alloc_fid(struct p9_connection *conn)
{
uint32_t fid;
pthread_mutex_lock(&conn->fid_lock);
for (int i = 1; i < P9_MAX_FID; i++) {
if (!conn->fid_table[i]) {
fid = i;
conn->fid_table[i] = (struct p9_fid *)1;
goto found;
}
}
fid = P9_NOFID;
found:
pthread_mutex_unlock(&conn->fid_lock);
return fid;
}
void
p9_free_fid(struct p9_connection *conn, uint32_t fid)
{
if (fid >= P9_MAX_FID)
return;
pthread_mutex_lock(&conn->fid_lock);
if (conn->fid_table[fid]) {
if (conn->fid_table[fid] != (struct p9_fid *)1) {
free(conn->fid_table[fid]);
}
conn->fid_table[fid] = NULL;
}
pthread_mutex_unlock(&conn->fid_lock);
}
int
p9_encode_string(uint8_t **buf, size_t *len, const char *str)
{
size_t slen = str ? strlen(str) : 0;
if (*len < 2 + slen)
return -1;
*(uint16_t*)*buf = slen;
*buf += 2;
*len -= 2;
if (slen > 0) {
memcpy(*buf, str, slen);
*buf += slen;
*len -= slen;
}
return 0;
}
int
p9_decode_string(uint8_t **buf, size_t *len, char **str)
{
uint16_t slen;
if (*len < 2)
return -1;
slen = *(uint16_t*)*buf;
*buf += 2;
*len -= 2;
if (*len < slen)
return -1;
*str = malloc(slen + 1);
if (!*str)
return -1;
memcpy(*str, *buf, slen);
(*str)[slen] = '\0';
*buf += slen;
*len -= slen;
return 0;
}
int
p9_encode_qid(uint8_t **buf, size_t *len, const struct p9_qid *qid)
{
if (*len < 13)
return -1;
**buf = qid->type;
(*buf)++;
*(uint32_t*)*buf = qid->version;
*buf += 4;
*(uint64_t*)*buf = qid->path;
*buf += 8;
*len -= 13;
return 0;
}
int
p9_decode_qid(uint8_t **buf, size_t *len, struct p9_qid *qid)
{
if (*len < 13)
return -1;
qid->type = **buf;
(*buf)++;
qid->version = *(uint32_t*)*buf;
*buf += 4;
qid->path = *(uint64_t*)*buf;
*buf += 8;
*len -= 13;
return 0;
}
void
p9_init_qid(struct p9_qid *qid, uint8_t type, uint32_t version, uint64_t path)
{
qid->type = type;
qid->version = version;
qid->path = path;
}
void
p9_free_stat(struct p9_stat *stat)
{
if (stat) {
free(stat->name);
free(stat->uid);
free(stat->gid);
free(stat->muid);
memset(stat, 0, sizeof(*stat));
}
}
const char *
p9_get_version(struct p9_connection *conn)
{
return conn ? conn->version : "";
}
uint32_t
p9_get_msize(struct p9_connection *conn)
{
return conn ? conn->msize : 0;
}