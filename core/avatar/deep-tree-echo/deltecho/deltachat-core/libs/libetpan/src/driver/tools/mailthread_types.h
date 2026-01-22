#ifndef MAILTHREAD_TYPES_H
#define MAILTHREAD_TYPES_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/maildriver_types.h>
#include <libetpan/mailmessage_types.h>
enum {
MAIL_THREAD_REFERENCES,
MAIL_THREAD_REFERENCES_NO_SUBJECT,
MAIL_THREAD_ORDEREDSUBJECT,
MAIL_THREAD_NONE
};
#ifdef __cplusplus
}
#endif
#endif