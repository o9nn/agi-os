#ifndef MDBOX_STORAGE_REBUILD_H
#define MDBOX_STORAGE_REBUILD_H
enum mdbox_rebuild_reason {
MDBOX_REBUILD_REASON_CORRUPTED = BIT(0),
MDBOX_REBUILD_REASON_MAILBOX_FSCKD = BIT(1),
MDBOX_REBUILD_REASON_MAP_FSCKD = BIT(2),
MDBOX_REBUILD_REASON_FORCED = BIT(3),
};
int mdbox_storage_rebuild(struct mdbox_storage *storage,
struct mailbox *fsckd_box,
enum mdbox_rebuild_reason reason);
#endif