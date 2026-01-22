#ifndef MAIL_ERROR_H
#define MAIL_ERROR_H
#define MAIL_ERRSTR_MAILBOX_NOT_FOUND "Mailbox doesn't exist: %s"
#define MAIL_ERRSTR_NO_PERMISSION "Permission denied"
#define MAIL_ERRSTR_INTERRUPTED "Operation interrupted"
#define MAIL_ERRSTR_NO_QUOTA "Not enough disk quota"
#define MAIL_ERRSTR_LOCK_TIMEOUT "Timeout while waiting for lock"
#define MAIL_ERRSTR_CRITICAL_MSG \
"Internal error occurred. Refer to server log for more information."
#define MAIL_ERRSTR_CRITICAL_MSG_STAMP \
MAIL_ERRSTR_CRITICAL_MSG " [%Y-%m-%d %H:%M:%S]"
#define T_MAIL_ERR_MAILBOX_NOT_FOUND(name) \
t_strdup_printf(MAIL_ERRSTR_MAILBOX_NOT_FOUND, name)
enum mail_error {
MAIL_ERROR_NONE = 0,
MAIL_ERROR_TEMP,
MAIL_ERROR_UNAVAILABLE,
MAIL_ERROR_NOTPOSSIBLE,
MAIL_ERROR_PARAMS,
MAIL_ERROR_PERM,
MAIL_ERROR_NOQUOTA,
MAIL_ERROR_NOTFOUND,
MAIL_ERROR_EXISTS,
MAIL_ERROR_EXPUNGED,
MAIL_ERROR_INUSE,
MAIL_ERROR_CONVERSION,
MAIL_ERROR_INVALIDDATA,
MAIL_ERROR_LIMIT,
MAIL_ERROR_LOOKUP_ABORTED,
MAIL_ERROR_INTERRUPTED,
};
bool mail_error_from_errno(enum mail_error *error_r,
const char **error_string_r);
const char *mail_error_eacces_msg(const char *func, const char *path);
const char *mail_error_create_eacces_msg(const char *func, const char *path);
#endif