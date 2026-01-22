#ifndef MAILENGINE_H
#define MAILENGINE_H
#include <libetpan/mailmessage.h>
#include <libetpan/mailfolder.h>
#include <libetpan/mailprivacy_types.h>
#ifdef __cplusplus
extern "C" {
#endif
struct mailengine *
libetpan_engine_new(struct mailprivacy * privacy);
void libetpan_engine_free(struct mailengine * engine);
struct mailprivacy *
libetpan_engine_get_privacy(struct mailengine * engine);
int libetpan_message_ref(struct mailengine * engine,
mailmessage * msg);
int libetpan_message_unref(struct mailengine * engine,
mailmessage * msg);
int libetpan_message_mime_ref(struct mailengine * engine,
mailmessage * msg);
int libetpan_message_mime_unref(struct mailengine * engine,
mailmessage * msg);
int libetpan_folder_get_msg_list(struct mailengine * engine,
struct mailfolder * folder,
struct mailmessage_list ** p_new_msg_list,
struct mailmessage_list ** p_lost_msg_list);
int libetpan_folder_fetch_env_list(struct mailengine * engine,
struct mailfolder * folder,
struct mailmessage_list * msg_list);
void libetpan_folder_free_msg_list(struct mailengine * engine,
struct mailfolder * folder,
struct mailmessage_list * env_list);
int libetpan_storage_add(struct mailengine * engine,
struct mailstorage * storage);
void libetpan_storage_remove(struct mailengine * engine,
struct mailstorage * storage);
int libetpan_storage_connect(struct mailengine * engine,
struct mailstorage * storage);
void libetpan_storage_disconnect(struct mailengine * engine,
struct mailstorage * storage);
int libetpan_storage_used(struct mailengine * engine,
struct mailstorage * storage);
int libetpan_folder_connect(struct mailengine * engine,
struct mailfolder * folder);
void libetpan_folder_disconnect(struct mailengine * engine,
struct mailfolder * folder);
struct mailfolder *
libetpan_message_get_folder(struct mailengine * engine,
mailmessage * msg);
struct mailstorage *
libetpan_message_get_storage(struct mailengine * engine,
mailmessage * msg);
int libetpan_message_register(struct mailengine * engine,
struct mailfolder * folder,
mailmessage * msg);
void libetpan_engine_debug(struct mailengine * engine, FILE * f);
extern void * engine_app;
#ifdef __cplusplus
}
#endif
#endif