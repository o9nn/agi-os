#ifndef MAILDIR_KEYWORDS_H
#define MAILDIR_KEYWORDS_H
#define MAILDIR_KEYWORDS_NAME "dovecot-keywords"
struct maildir_mailbox;
struct maildir_keywords;
struct maildir_keywords_sync_ctx;
struct maildir_keywords *maildir_keywords_init(struct maildir_mailbox *mbox);
void maildir_keywords_deinit(struct maildir_keywords **mk);
struct maildir_keywords *
maildir_keywords_init_readonly(struct mailbox *box);
struct maildir_keywords_sync_ctx *
maildir_keywords_sync_init(struct maildir_keywords *mk,
struct mail_index *index);
struct maildir_keywords_sync_ctx *
maildir_keywords_sync_init_readonly(struct maildir_keywords *mk,
struct mail_index *index);
void maildir_keywords_sync_deinit(struct maildir_keywords_sync_ctx **ctx);
unsigned int maildir_keywords_char_idx(struct maildir_keywords_sync_ctx *ctx,
char keyword);
char maildir_keywords_idx_char(struct maildir_keywords_sync_ctx *ctx,
unsigned int idx);
#endif