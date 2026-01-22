#ifndef MAILDRIVER_TYPES_HELPER_H
#define MAILDRIVER_TYPES_HELPER_H
#include <libetpan/maildriver_types.h>
#ifdef __cplusplus
extern "C" {
#endif
int mail_flags_add_extension(struct mail_flags * flags,
char * ext_flag);
int mail_flags_remove_extension(struct mail_flags * flags,
char * ext_flag);
int mail_flags_has_extension(struct mail_flags * flags,
char * ext_flag);
#ifdef __cplusplus
}
#endif
#endif