#ifndef SDBOX_FILE_H
#define SDBOX_FILE_H
#include "dbox-file.h"
struct sdbox_file {
struct dbox_file file;
struct sdbox_mailbox *mbox;
uint32_t uid;
pool_t attachment_pool;
ARRAY_TYPE(const_string) attachment_paths;
bool written_to_disk;
};
struct dbox_file *sdbox_file_init(struct sdbox_mailbox *mbox, uint32_t uid);
struct dbox_file *sdbox_file_create(struct sdbox_mailbox *mbox);
void sdbox_file_free(struct dbox_file *file);
int sdbox_file_get_attachments(struct dbox_file *file, const char **extrefs_r);
const char *
sdbox_file_attachment_relpath(struct sdbox_file *file, const char *srcpath);
int sdbox_file_assign_uid(struct sdbox_file *file, uint32_t uid);
int sdbox_file_create_fd(struct dbox_file *file, const char *path,
bool parents);
int sdbox_file_move(struct dbox_file *file, bool alt_path);
int sdbox_file_unlink_with_attachments(struct sdbox_file *sfile);
int sdbox_file_unlink_aborted_save(struct sdbox_file *file);
#endif