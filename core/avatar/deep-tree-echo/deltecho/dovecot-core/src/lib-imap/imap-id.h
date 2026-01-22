#ifndef IMAP_ID_H
#define IMAP_ID_H
struct imap_arg;
struct imap_id_log_entry {
struct event *event;
string_t *reply;
unsigned int invalid_key_id_counter;
};
#define IMAP_ID_KEY_MAX_LEN 30
#define IMAP_ID_PARAMS_LOG_MAX_LEN 1024
const char *imap_id_reply_generate(const char *settings);
void imap_id_add_log_entry(struct imap_id_log_entry *log_entry,
const char *key, const char *value);
#endif