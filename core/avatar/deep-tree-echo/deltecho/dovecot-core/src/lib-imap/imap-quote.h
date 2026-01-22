#ifndef IMAP_QUOTE_H
#define IMAP_QUOTE_H
void imap_append_string(string_t *dest, const char *src);
void imap_append_astring(string_t *dest, const char *src);
void imap_append_nstring(string_t *dest, const char *src);
void imap_append_nstring_nolf(string_t *dest, const char *src);
void imap_append_quoted(string_t *dest, const char *src);
void imap_append_string_for_humans(string_t *dest,
const unsigned char *src, size_t size);
#endif