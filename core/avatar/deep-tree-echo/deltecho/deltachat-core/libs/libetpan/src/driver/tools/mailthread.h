#ifndef MAILTHREAD_H
#define MAILTHREAD_H
#include <libetpan/mailthread_types.h>
#ifdef __cplusplus
extern "C" {
#endif
LIBETPAN_EXPORT
int mail_build_thread(int type, char * default_from,
struct mailmessage_list * env_list,
struct mailmessage_tree ** result,
int (* comp_func)(struct mailmessage_tree **,
struct mailmessage_tree **));
LIBETPAN_EXPORT
int mail_thread_sort(struct mailmessage_tree * tree,
int (* comp_func)(struct mailmessage_tree **,
struct mailmessage_tree **),
int sort_sub);
LIBETPAN_EXPORT
int mailthread_tree_timecomp(struct mailmessage_tree ** ptree1,
struct mailmessage_tree ** ptree2);
#ifdef __cplusplus
}
#endif
#endif