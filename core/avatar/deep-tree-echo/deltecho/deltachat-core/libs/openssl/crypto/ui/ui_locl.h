#ifndef HEADER_UI_LOCL_H
# define HEADER_UI_LOCL_H
# include <openssl/ui.h>
# include <openssl/crypto.h>
# ifdef _
# undef _
# endif
struct ui_method_st {
char *name;
int (*ui_open_session) (UI *ui);
int (*ui_write_string) (UI *ui, UI_STRING *uis);
int (*ui_flush) (UI *ui);
int (*ui_read_string) (UI *ui, UI_STRING *uis);
int (*ui_close_session) (UI *ui);
char *(*ui_construct_prompt) (UI *ui, const char *object_desc,
const char *object_name);
};
struct ui_string_st {
enum UI_string_types type;
const char *out_string;
int input_flags;
char *result_buf;
union {
struct {
int result_minsize;
int result_maxsize;
const char *test_buf;
} string_data;
struct {
const char *action_desc;
const char *ok_chars;
const char *cancel_chars;
} boolean_data;
} _;
# define OUT_STRING_FREEABLE 0x01
int flags;
};
struct ui_st {
const UI_METHOD *meth;
STACK_OF(UI_STRING) *strings;
void *user_data;
CRYPTO_EX_DATA ex_data;
# define UI_FLAG_REDOABLE 0x0001
# define UI_FLAG_PRINT_ERRORS 0x0100
int flags;
};
#endif