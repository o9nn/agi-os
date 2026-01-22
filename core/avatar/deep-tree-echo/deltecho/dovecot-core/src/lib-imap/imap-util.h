#ifndef IMAP_UTIL_H
#define IMAP_UTIL_H
#include "seq-range-array.h"
#include "mail-types.h"
struct imap_arg;
void imap_write_flags(string_t *dest, enum mail_flags flags,
const char *const *keywords) ATTR_NULL(3);
enum mail_flags imap_parse_system_flag(const char *str);
void imap_write_seq_range(string_t *dest, const ARRAY_TYPE(seq_range) *array);
void imap_write_arg(string_t *dest, const struct imap_arg *arg);
void imap_write_args(string_t *dest, const struct imap_arg *args);
void imap_write_args_for_human(string_t *dest, const struct imap_arg *args);
const char *imap_args_to_str(const struct imap_arg *args);
#endif