#ifndef MBOX_LOCK_H
#define MBOX_LOCK_H
int mbox_lock(struct mbox_mailbox *mbox, int lock_type,
unsigned int *lock_id_r);
int ATTR_NOWARN_UNUSED_RESULT
mbox_unlock(struct mbox_mailbox *mbox, unsigned int lock_id);
unsigned int mbox_get_cur_lock_id(struct mbox_mailbox *mbox);
void mbox_dotlock_touch(struct mbox_mailbox *mbox);
#endif