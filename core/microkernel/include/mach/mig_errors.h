#ifndef _MACH_MIG_ERRORS_H_
#define _MACH_MIG_ERRORS_H_
#include <mach/kern_return.h>
#include <mach/message.h>
#define MIG_TYPE_ERROR -300
#define MIG_REPLY_MISMATCH -301
#define MIG_REMOTE_ERROR -302
#define MIG_BAD_ID -303
#define MIG_BAD_ARGUMENTS -304
#define MIG_NO_REPLY -305
#define MIG_EXCEPTION -306
#define MIG_ARRAY_TOO_LARGE -307
#define MIG_SERVER_DIED -308
#define MIG_DESTROY_REQUEST -309
typedef struct {
mach_msg_header_t Head;
mach_msg_type_t RetCodeType;
kern_return_t RetCode;
} mig_reply_header_t;
typedef struct mig_symtab {
char *ms_routine_name;
int ms_routine_number;
#if defined(__STDC__) || defined(c_plus_plus) || defined(hc)
void
#else
int
#endif
(*ms_routine)(void);
} mig_symtab_t;
#if defined(__STDC__) || defined(c_plus_plus)
typedef void (*mig_routine_t)(mach_msg_header_t *, mach_msg_header_t *);
#else
#if defined(hc)
typedef void (*mig_routine_t)();
#else
typedef int (*mig_routine_t)();
#endif
#endif
#endif