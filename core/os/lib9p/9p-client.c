#include "9p-internal.h"
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
static struct p9_message *
p9_rpc(struct p9_connection *conn, struct p9_message *tmsg)
{
struct p9_message *rmsg;
pthread_mutex_lock(&conn->lock);
if (p9_send_message(conn, tmsg) < 0) {
pthread_mutex_unlock(&conn->lock);
return NULL;
}
rmsg = p9_receive_message(conn);
pthread_mutex_unlock(&conn->lock);
return rmsg;
}
int
p9_version(struct p9_connection *conn, const char *version, uint32_t msize)
{
struct p9_message *tmsg, *rmsg;
uint8_t *buf;
size_t len;
uint16_t tag;
uint32_t rmsize;
char *rversion;
int ret = -1;
if (!conn || !version) {
p9_errno = P9_EINVAL;
return -1;
}
len = 4 + 1 + 2 + 4 + 2 + strlen(version);
tmsg = malloc(sizeof(struct p9_message) + len);
if (!tmsg) {
p9_errno = P9_ENOMEM;
return -1;
}
tag = p9_alloc_tag(conn);
tmsg->size = len;
tmsg->type = P9_TVERSION;
tmsg->tag = tag;
buf = tmsg->data;
len = tmsg->size - 7;
*(uint32_t*)buf = msize;
buf += 4;
len -= 4;
if (p9_encode_string(&buf, &len, version) < 0) {
p9_errno = P9_EPROTO;
goto cleanup;
}
rmsg = p9_rpc(conn, tmsg);
if (!rmsg) {
p9_errno = P9_EIO;
goto cleanup;
}
if (rmsg->type != P9_RVERSION) {
p9_errno = P9_EPROTO;
goto cleanup_rmsg;
}
buf = rmsg->data;
len = rmsg->size - 7;
if (len < 4) {
p9_errno = P9_EPROTO;
goto cleanup_rmsg;
}
rmsize = *(uint32_t*)buf;
buf += 4;
len -= 4;
if (p9_decode_string(&buf, &len, &rversion) < 0) {
p9_errno = P9_EPROTO;
goto cleanup_rmsg;
}
conn->msize = rmsize < msize ? rmsize : msize;
strncpy(conn->version, rversion, sizeof(conn->version) - 1);
conn->version[sizeof(conn->version) - 1] = '\0';
free(rversion);
ret = 0;
cleanup_rmsg:
p9_free_message(rmsg);
cleanup:
p9_free_tag(conn, tag);
free(tmsg);
return ret;
}
struct p9_fid *
p9_attach(struct p9_connection *conn, const char *uname, const char *aname)
{
struct p9_message *tmsg, *rmsg;
struct p9_fid *fid;
uint8_t *buf;
size_t len;
uint16_t tag;
uint32_t fid_num;
int ret = -1;
if (!conn || !uname) {
p9_errno = P9_EINVAL;
return NULL;
}
fid_num = p9_alloc_fid(conn);
if (fid_num == P9_NOFID) {
p9_errno = P9_EMFILE;
return NULL;
}
fid = malloc(sizeof(struct p9_fid));
if (!fid) {
p9_errno = P9_ENOMEM;
p9_free_fid(conn, fid_num);
return NULL;
}
fid->fid = fid_num;
fid->conn = conn;
fid->open = 0;
len = 4 + 1 + 2 + 4 + 4 + 2 + strlen(uname) + 2 + (aname ? strlen(aname) : 0);
tmsg = malloc(sizeof(struct p9_message) + len);
if (!tmsg) {
p9_errno = P9_ENOMEM;
goto cleanup_fid;
}
tag = p9_alloc_tag(conn);
tmsg->size = len;
tmsg->type = P9_TATTACH;
tmsg->tag = tag;
buf = tmsg->data;
len = tmsg->size - 7;
*(uint32_t*)buf = fid_num;
buf += 4;
len -= 4;
*(uint32_t*)buf = P9_NOFID;
buf += 4;
len -= 4;
if (p9_encode_string(&buf, &len, uname) < 0) {
p9_errno = P9_EPROTO;
goto cleanup_msg;
}
if (p9_encode_string(&buf, &len, aname) < 0) {
p9_errno = P9_EPROTO;
goto cleanup_msg;
}
rmsg = p9_rpc(conn, tmsg);
if (!rmsg) {
p9_errno = P9_EIO;
goto cleanup_msg;
}
if (rmsg->type != P9_RATTACH) {
p9_errno = P9_EPROTO;
goto cleanup_rmsg;
}
buf = rmsg->data;
len = rmsg->size - 7;
if (p9_decode_qid(&buf, &len, &fid->qid) < 0) {
p9_errno = P9_EPROTO;
goto cleanup_rmsg;
}
pthread_mutex_lock(&conn->fid_lock);
conn->fid_table[fid_num] = fid;
pthread_mutex_unlock(&conn->fid_lock);
ret = 0;
cleanup_rmsg:
p9_free_message(rmsg);
cleanup_msg:
p9_free_tag(conn, tag);
free(tmsg);
if (ret < 0) {
cleanup_fid:
free(fid);
p9_free_fid(conn, fid_num);
return NULL;
}
return fid;
}
struct p9_fid *
p9_walk(struct p9_fid *fid, const char **names, int nnames)
{
struct p9_message *tmsg, *rmsg;
struct p9_fid *newfid;
uint8_t *buf;
size_t len, msglen;
uint16_t tag, nwqids;
uint32_t newfid_num;
int i, ret = -1;
if (!fid || !names || nnames < 0 || nnames > 16) {
p9_errno = P9_EINVAL;
return NULL;
}
newfid_num = p9_alloc_fid(fid->conn);
if (newfid_num == P9_NOFID) {
p9_errno = P9_EMFILE;
return NULL;
}
newfid = malloc(sizeof(struct p9_fid));
if (!newfid) {
p9_errno = P9_ENOMEM;
p9_free_fid(fid->conn, newfid_num);
return NULL;
}
newfid->fid = newfid_num;
newfid->conn = fid->conn;
newfid->open = 0;
msglen = 4 + 1 + 2 + 4 + 4 + 2;
for (i = 0; i < nnames; i++) {
msglen += 2 + strlen(names[i]);
}
tmsg = malloc(sizeof(struct p9_message) + msglen);
if (!tmsg) {
p9_errno = P9_ENOMEM;
goto cleanup_newfid;
}
tag = p9_alloc_tag(fid->conn);
tmsg->size = msglen;
tmsg->type = P9_TWALK;
tmsg->tag = tag;
buf = tmsg->data;
len = tmsg->size - 7;
*(uint32_t*)buf = fid->fid;
buf += 4;
len -= 4;
*(uint32_t*)buf = newfid_num;
buf += 4;
len -= 4;
*(uint16_t*)buf = nnames;
buf += 2;
len -= 2;
for (i = 0; i < nnames; i++) {
if (p9_encode_string(&buf, &len, names[i]) < 0) {
p9_errno = P9_EPROTO;
goto cleanup_msg;
}
}
rmsg = p9_rpc(fid->conn, tmsg);
if (!rmsg) {
p9_errno = P9_EIO;
goto cleanup_msg;
}
if (rmsg->type != P9_RWALK) {
p9_errno = P9_EPROTO;
goto cleanup_rmsg;
}
buf = rmsg->data;
len = rmsg->size - 7;
if (len < 2) {
p9_errno = P9_EPROTO;
goto cleanup_rmsg;
}
nwqids = *(uint16_t*)buf;
buf += 2;
len -= 2;
if (nwqids != nnames) {
p9_errno = P9_ENOENT;
goto cleanup_rmsg;
}
if (nwqids > 0) {
for (i = 0; i < nwqids - 1; i++) {
struct p9_qid dummy;
if (p9_decode_qid(&buf, &len, &dummy) < 0) {
p9_errno = P9_EPROTO;
goto cleanup_rmsg;
}
}
if (p9_decode_qid(&buf, &len, &newfid->qid) < 0) {
p9_errno = P9_EPROTO;
goto cleanup_rmsg;
}
} else {
newfid->qid = fid->qid;
}
pthread_mutex_lock(&fid->conn->fid_lock);
fid->conn->fid_table[newfid_num] = newfid;
pthread_mutex_unlock(&fid->conn->fid_lock);
ret = 0;
cleanup_rmsg:
p9_free_message(rmsg);
cleanup_msg:
p9_free_tag(fid->conn, tag);
free(tmsg);
if (ret < 0) {
cleanup_newfid:
free(newfid);
p9_free_fid(fid->conn, newfid_num);
return NULL;
}
return newfid;
}
int
p9_open(struct p9_fid *fid, uint8_t mode)
{
struct p9_message *tmsg, *rmsg;
uint8_t *buf;
size_t len;
uint16_t tag;
int ret = -1;
if (!fid) {
p9_errno = P9_EINVAL;
return -1;
}
if (fid->open) {
p9_errno = P9_EINVAL;
return -1;
}
len = 4 + 1 + 2 + 4 + 1;
tmsg = malloc(sizeof(struct p9_message) + len);
if (!tmsg) {
p9_errno = P9_ENOMEM;
return -1;
}
tag = p9_alloc_tag(fid->conn);
tmsg->size = len;
tmsg->type = P9_TOPEN;
tmsg->tag = tag;
buf = tmsg->data;
len = tmsg->size - 7;
*(uint32_t*)buf = fid->fid;
buf += 4;
len -= 4;
*buf = mode;
buf++;
len--;
rmsg = p9_rpc(fid->conn, tmsg);
if (!rmsg) {
p9_errno = P9_EIO;
goto cleanup;
}
if (rmsg->type != P9_ROPEN) {
p9_errno = P9_EPROTO;
goto cleanup_rmsg;
}
buf = rmsg->data;
len = rmsg->size - 7;
if (p9_decode_qid(&buf, &len, &fid->qid) < 0) {
p9_errno = P9_EPROTO;
goto cleanup_rmsg;
}
fid->open = 1;
fid->mode = mode;
ret = 0;
cleanup_rmsg:
p9_free_message(rmsg);
cleanup:
p9_free_tag(fid->conn, tag);
free(tmsg);
return ret;
}
ssize_t
p9_read(struct p9_fid *fid, void *buf, size_t count, uint64_t offset)
{
struct p9_message *tmsg, *rmsg;
uint8_t *msgbuf;
size_t len;
uint16_t tag;
uint32_t rcount;
ssize_t ret = -1;
if (!fid || !buf || !fid->open) {
p9_errno = P9_EINVAL;
return -1;
}
len = 4 + 1 + 2 + 4 + 8 + 4;
tmsg = malloc(sizeof(struct p9_message) + len);
if (!tmsg) {
p9_errno = P9_ENOMEM;
return -1;
}
tag = p9_alloc_tag(fid->conn);
tmsg->size = len;
tmsg->type = P9_TREAD;
tmsg->tag = tag;
msgbuf = tmsg->data;
len = tmsg->size - 7;
*(uint32_t*)msgbuf = fid->fid;
msgbuf += 4;
len -= 4;
*(uint64_t*)msgbuf = offset;
msgbuf += 8;
len -= 8;
*(uint32_t*)msgbuf = count;
msgbuf += 4;
len -= 4;
rmsg = p9_rpc(fid->conn, tmsg);
if (!rmsg) {
p9_errno = P9_EIO;
goto cleanup;
}
if (rmsg->type != P9_RREAD) {
p9_errno = P9_EPROTO;
goto cleanup_rmsg;
}
msgbuf = rmsg->data;
len = rmsg->size - 7;
if (len < 4) {
p9_errno = P9_EPROTO;
goto cleanup_rmsg;
}
rcount = *(uint32_t*)msgbuf;
msgbuf += 4;
len -= 4;
if (len < rcount) {
p9_errno = P9_EPROTO;
goto cleanup_rmsg;
}
if (rcount > count)
rcount = count;
memcpy(buf, msgbuf, rcount);
ret = rcount;
cleanup_rmsg:
p9_free_message(rmsg);
cleanup:
p9_free_tag(fid->conn, tag);
free(tmsg);
return ret;
}
ssize_t
p9_write(struct p9_fid *fid, const void *buf, size_t count, uint64_t offset)
{
struct p9_message *tmsg, *rmsg;
uint8_t *msgbuf;
size_t len;
uint16_t tag;
uint32_t wcount;
ssize_t ret = -1;
if (!fid || !buf || !fid->open) {
p9_errno = P9_EINVAL;
return -1;
}
len = 4 + 1 + 2 + 4 + 8 + 4 + count;
tmsg = malloc(sizeof(struct p9_message) + len);
if (!tmsg) {
p9_errno = P9_ENOMEM;
return -1;
}
tag = p9_alloc_tag(fid->conn);
tmsg->size = len;
tmsg->type = P9_TWRITE;
tmsg->tag = tag;
msgbuf = tmsg->data;
len = tmsg->size - 7;
*(uint32_t*)msgbuf = fid->fid;
msgbuf += 4;
len -= 4;
*(uint64_t*)msgbuf = offset;
msgbuf += 8;
len -= 8;
*(uint32_t*)msgbuf = count;
msgbuf += 4;
len -= 4;
memcpy(msgbuf, buf, count);
msgbuf += count;
len -= count;
rmsg = p9_rpc(fid->conn, tmsg);
if (!rmsg) {
p9_errno = P9_EIO;
goto cleanup;
}
if (rmsg->type != P9_RWRITE) {
p9_errno = P9_EPROTO;
goto cleanup_rmsg;
}
msgbuf = rmsg->data;
len = rmsg->size - 7;
if (len < 4) {
p9_errno = P9_EPROTO;
goto cleanup_rmsg;
}
wcount = *(uint32_t*)msgbuf;
ret = wcount;
cleanup_rmsg:
p9_free_message(rmsg);
cleanup:
p9_free_tag(fid->conn, tag);
free(tmsg);
return ret;
}
int
p9_clunk(struct p9_fid *fid)
{
struct p9_message *tmsg, *rmsg;
uint8_t *buf;
size_t len;
uint16_t tag;
int ret = -1;
if (!fid) {
p9_errno = P9_EINVAL;
return -1;
}
len = 4 + 1 + 2 + 4;
tmsg = malloc(sizeof(struct p9_message) + len);
if (!tmsg) {
p9_errno = P9_ENOMEM;
return -1;
}
tag = p9_alloc_tag(fid->conn);
tmsg->size = len;
tmsg->type = P9_TCLUNK;
tmsg->tag = tag;
buf = tmsg->data;
len = tmsg->size - 7;
*(uint32_t*)buf = fid->fid;
buf += 4;
len -= 4;
rmsg = p9_rpc(fid->conn, tmsg);
if (!rmsg) {
p9_errno = P9_EIO;
goto cleanup;
}
if (rmsg->type != P9_RCLUNK) {
p9_errno = P9_EPROTO;
goto cleanup_rmsg;
}
ret = 0;
cleanup_rmsg:
p9_free_message(rmsg);
cleanup:
p9_free_tag(fid->conn, tag);
free(tmsg);
p9_free_fid(fid->conn, fid->fid);
return ret;
}