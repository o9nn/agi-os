#ifndef UIDPLUS_TYPES_H
#define UIDPLUS_TYPES_H
#ifdef __cplusplus
extern "C" {
#endif
#ifndef WIN32
#include <inttypes.h>
#endif
#include "mailimap_types.h"
enum {
MAILIMAP_UIDPLUS_RESP_CODE_APND,
MAILIMAP_UIDPLUS_RESP_CODE_COPY,
MAILIMAP_UIDPLUS_RESP_CODE_UIDNOTSTICKY
};
struct mailimap_uidplus_resp_code_apnd {
uint32_t uid_uidvalidity;
struct mailimap_set * uid_set;
};
struct mailimap_uidplus_resp_code_copy {
uint32_t uid_uidvalidity;
struct mailimap_set * uid_source_set;
struct mailimap_set * uid_dest_set;
};
LIBETPAN_EXPORT
struct mailimap_uidplus_resp_code_apnd *
mailimap_uidplus_resp_code_apnd_new(uint32_t uid_uidvalidity, struct mailimap_set * uid_set);
LIBETPAN_EXPORT
void mailimap_uidplus_resp_code_apnd_free(struct mailimap_uidplus_resp_code_apnd * resp_code_apnd);
LIBETPAN_EXPORT
struct mailimap_uidplus_resp_code_copy *
mailimap_uidplus_resp_code_copy_new(uint32_t uid_uidvalidity, struct mailimap_set * uid_source_set, struct mailimap_set * uid_dest_set);
LIBETPAN_EXPORT
void mailimap_uidplus_resp_code_copy_free(struct mailimap_uidplus_resp_code_copy * resp_code_copy);
LIBETPAN_EXPORT
void mailimap_uidplus_free(struct mailimap_extension_data * ext_data);
#ifdef __cplusplus
}
#endif
#endif