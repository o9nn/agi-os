#ifndef IMAP_URLAUTH_FETCH_H
#define IMAP_URLAUTH_FETCH_H
struct imap_url;
struct imap_urlauth_context;
struct imap_urlauth_fetch;
enum imap_urlauth_fetch_flags {
IMAP_URLAUTH_FETCH_FLAG_EXTENDED = 0x01,
IMAP_URLAUTH_FETCH_FLAG_BODY = 0x02,
IMAP_URLAUTH_FETCH_FLAG_BINARY = 0x04,
IMAP_URLAUTH_FETCH_FLAG_BODYPARTSTRUCTURE = 0x08,
};
struct imap_urlauth_fetch_reply {
const char *url;
enum imap_urlauth_fetch_flags flags;
struct istream *input;
uoff_t size;
const char *bodypartstruct;
const char *error;
bool succeeded:1;
bool binary_has_nuls:1;
};
typedef int
imap_urlauth_fetch_callback_t(struct imap_urlauth_fetch_reply *reply,
bool last, void *context);
struct imap_urlauth_fetch *
imap_urlauth_fetch_init(struct imap_urlauth_context *uctx,
imap_urlauth_fetch_callback_t *callback, void *context);
void imap_urlauth_fetch_deinit(struct imap_urlauth_fetch **ufetch);
int imap_urlauth_fetch_url(struct imap_urlauth_fetch *ufetch, const char *url,
enum imap_urlauth_fetch_flags url_flags);
int imap_urlauth_fetch_url_parsed(struct imap_urlauth_fetch *ufetch,
const char *url, struct imap_url *imap_url,
enum imap_urlauth_fetch_flags url_flags);
bool imap_urlauth_fetch_continue(struct imap_urlauth_fetch *ufetch);
bool imap_urlauth_fetch_is_pending(struct imap_urlauth_fetch *ufetch);
#endif