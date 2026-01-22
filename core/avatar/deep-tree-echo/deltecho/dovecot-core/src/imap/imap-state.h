#ifndef IMAP_STATE_H
#define IMAP_STATE_H
int imap_state_export_internal(struct client *client, buffer_t *dest,
const char **error_r);
int imap_state_import_internal(struct client *client,
const unsigned char *data, size_t size,
const char **error_r);
int imap_state_import_external(struct client *client,
const unsigned char *data, size_t size,
const char **error_r);
int imap_state_export_base(struct client *client, bool internal,
buffer_t *dest, const char **error_r);
ssize_t imap_state_import_base(struct client *client, bool internal,
const unsigned char *data, size_t size,
const char **error_r);
void imap_state_import_idle_cmd_tag(struct client *client, const char *tag);
#endif