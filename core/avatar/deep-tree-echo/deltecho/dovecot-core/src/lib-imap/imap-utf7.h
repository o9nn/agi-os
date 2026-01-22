#ifndef IMAP_UTF7_H
#define IMAP_UTF7_H
int imap_utf8_to_utf7(const char *src, string_t *dest);
int t_imap_utf8_to_utf7(const char *src, const char **dest_r);
int imap_escaped_utf8_to_utf7(const char *src, char escape_char, string_t *dest);
int imap_escaped_utf8_hex_to_char(const char *str, unsigned char *chr_r);
int imap_utf7_to_utf8(const char *src, string_t *dest);
void imap_utf7_to_utf8_escaped(const char *src, const char *escape_chars,
string_t *dest);
bool imap_utf7_is_valid(const char *src);
#endif