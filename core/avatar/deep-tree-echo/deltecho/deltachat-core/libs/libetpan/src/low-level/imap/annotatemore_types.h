#ifndef ANNOTATEMORE_TYPES_H
#define ANNOTATEMORE_TYPES_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/libetpan-config.h>
#include <libetpan/mailstream.h>
#include <libetpan/clist.h>
enum {
MAILIMAP_ANNOTATEMORE_TYPE_ANNOTATE_DATA,
MAILIMAP_ANNOTATEMORE_TYPE_RESP_TEXT_CODE
};
enum {
MAILIMAP_ANNOTATEMORE_RESP_TEXT_CODE_UNSPECIFIED,
MAILIMAP_ANNOTATEMORE_RESP_TEXT_CODE_TOOBIG,
MAILIMAP_ANNOTATEMORE_RESP_TEXT_CODE_TOOMANY
};
void mailimap_annotatemore_attrib_free(char * attrib);
void mailimap_annotatemore_value_free(char * value);
void mailimap_annotatemore_entry_free(char * entry);
struct mailimap_annotatemore_att_value  {
char * attrib;
char * value;
};
LIBETPAN_EXPORT
struct mailimap_annotatemore_att_value *
mailimap_annotatemore_att_value_new(char * attrib, char * value);
void mailimap_annotatemore_att_value_free(struct
mailimap_annotatemore_att_value * att_value);
struct mailimap_annotatemore_entry_att {
char * entry;
clist * att_value_list;
};
LIBETPAN_EXPORT
struct mailimap_annotatemore_entry_att *
mailimap_annotatemore_entry_att_new(char * entry, clist * list);
LIBETPAN_EXPORT
void mailimap_annotatemore_entry_att_free(struct
mailimap_annotatemore_entry_att * en_att);
LIBETPAN_EXPORT
struct mailimap_annotatemore_entry_att *
mailimap_annotatemore_entry_att_new_empty(char * entry);
LIBETPAN_EXPORT
int mailimap_annotatemore_entry_att_add(struct
mailimap_annotatemore_entry_att * en_att,
struct mailimap_annotatemore_att_value * at_value);
enum {
MAILIMAP_ANNOTATEMORE_ENTRY_LIST_TYPE_ERROR,
MAILIMAP_ANNOTATEMORE_ENTRY_LIST_TYPE_ENTRY_ATT_LIST,
MAILIMAP_ANNOTATEMORE_ENTRY_LIST_TYPE_ENTRY_LIST
};
struct mailimap_annotatemore_entry_list {
int en_list_type;
clist * en_list_data;
};
struct mailimap_annotatemore_entry_list *
mailimap_annotatemore_entry_list_new(int type, clist * en_att_list, clist * en_list);
void mailimap_annotatemore_entry_list_free(struct
mailimap_annotatemore_entry_list * en_list);
struct mailimap_annotatemore_annotate_data {
char * mailbox;
struct mailimap_annotatemore_entry_list * entry_list;
};
struct mailimap_annotatemore_annotate_data *
mailimap_annotatemore_annotate_data_new(char * mb, struct
mailimap_annotatemore_entry_list * en_list);
LIBETPAN_EXPORT
void mailimap_annotatemore_annotate_data_free(struct
mailimap_annotatemore_annotate_data * an_data);
struct mailimap_annotatemore_entry_match_list {
clist * entry_match_list;
};
LIBETPAN_EXPORT
struct mailimap_annotatemore_entry_match_list *
mailimap_annotatemore_entry_match_list_new(clist * en_list);
LIBETPAN_EXPORT
void mailimap_annotatemore_entry_match_list_free(
struct mailimap_annotatemore_entry_match_list * en_list);
struct mailimap_annotatemore_attrib_match_list {
clist * attrib_match_list;
};
LIBETPAN_EXPORT
struct mailimap_annotatemore_attrib_match_list *
mailimap_annotatemore_attrib_match_list_new(clist * at_list);
LIBETPAN_EXPORT
void mailimap_annotatemore_attrib_match_list_free(
struct mailimap_annotatemore_attrib_match_list * at_list);
LIBETPAN_EXPORT
struct mailimap_annotatemore_entry_match_list *
mailimap_annotatemore_entry_match_list_new_empty(void);
LIBETPAN_EXPORT
int mailimap_annotatemore_entry_match_list_add(
struct mailimap_annotatemore_entry_match_list * en_list,
char * entry);
LIBETPAN_EXPORT
struct mailimap_annotatemore_attrib_match_list *
mailimap_annotatemore_attrib_match_list_new_empty(void);
LIBETPAN_EXPORT
int mailimap_annotatemore_attrib_match_list_add(
struct mailimap_annotatemore_attrib_match_list * at_list,
char * attrib);
struct mailimap_annotatemore_entry_att_list {
clist * entry_att_list;
};
LIBETPAN_EXPORT
struct mailimap_annotatemore_entry_att_list *
mailimap_annotatemore_entry_att_list_new(clist * en_list);
LIBETPAN_EXPORT
void mailimap_annotatemore_entry_att_list_free(
struct mailimap_annotatemore_entry_att_list * en_list);
LIBETPAN_EXPORT
struct mailimap_annotatemore_entry_att_list *
mailimap_annotatemore_entry_att_list_new_empty(void);
LIBETPAN_EXPORT
int mailimap_annotatemore_entry_att_list_add(
struct mailimap_annotatemore_entry_att_list * en_list,
struct mailimap_annotatemore_entry_att * en_att);
void
mailimap_annotatemore_free(struct mailimap_extension_data * ext_data);
#ifdef __cplusplus
}
#endif
#endif