#ifndef MAIL_COPY_H
#define MAIL_COPY_H
struct mail;
struct mail_save_context;
struct mailbox;
int mail_storage_copy(struct mail_save_context *ctx, struct mail *mail);
int mail_save_copy_default_metadata(struct mail_save_context *ctx,
struct mail *mail);
bool mail_storage_copy_can_use_hardlink(struct mailbox *src,
struct mailbox *dest);
#endif