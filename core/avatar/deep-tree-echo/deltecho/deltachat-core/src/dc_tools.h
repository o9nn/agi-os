#ifndef __DC_TOOLS_H__
#define __DC_TOOLS_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/libetpan.h>
int dc_exactly_one_bit_set (int v);
#define DC_EDITORIAL_OPEN "["
#define DC_EDITORIAL_CLOSE "]"
#define DC_EDITORIAL_ELLIPSE DC_EDITORIAL_OPEN "..." DC_EDITORIAL_CLOSE
#define DC_NDASH "\xE2\x80\x93"
char* dc_strdup (const char*);
char* dc_strdup_keep_null (const char*);
int dc_atoi_null_is_0 (const char*);
double dc_atof (const char*);
char* dc_ftoa (double);
void dc_ltrim (char*);
void dc_rtrim (char*);
void dc_trim (char*);
char* dc_strlower (const char*);
void dc_strlower_in_place (char*);
int dc_str_replace (char** haystack, const char* needle, const char* replacement);
int dc_str_contains (const char* haystack, const char* needle);
char* dc_null_terminate (const char*, int bytes);
char* dc_mprintf (const char* format, ...);
char* dc_binary_to_uc_hex (const uint8_t* buf, size_t bytes);
void dc_remove_cr_chars (char*);
void dc_unify_lineends (char*);
void dc_replace_bad_utf8_chars (char*);
size_t dc_utf8_strlen (const char*);
void dc_truncate_str (char*, int approx_characters);
void dc_truncate_n_unwrap_str (char*, int approx_characters, int do_unwrap);
carray* dc_split_into_lines (const char* buf_terminated);
void dc_free_splitted_lines (carray* lines);
char* dc_insert_breaks (const char*, int break_every, const char* break_chars);
char* dc_str_from_clist (const clist*, const char* delimiter);
clist* dc_str_to_clist (const char*, const char* delimiter);
int dc_str_to_color (const char*);
char* encode_base64 (const char * in, int len);
void clist_free_content (const clist*);
int clist_search_string_nocase (const clist*, const char* str);
#define DC_INVALID_TIMESTAMP (-1)
#define DC_SECONDS_PER_DAY 86400
time_t dc_timestamp_from_date (struct mailimf_date_time * date_time);
char* dc_timestamp_to_str (time_t);
struct mailimap_date_time* dc_timestamp_to_mailimap_date_time (time_t);
long dc_gm2local_offset (void);
time_t mkgmtime (struct tm*);
time_t dc_smeared_time (dc_context_t*);
time_t dc_create_smeared_timestamp (dc_context_t*);
time_t dc_create_smeared_timestamps (dc_context_t*, int count);
#define DC_CREATE_ID_LEN 11
char* dc_create_id (void);
char* dc_create_incoming_rfc724_mid (time_t message_timestamp, uint32_t contact_id_from, dc_array_t* contact_ids_to);
char* dc_create_outgoing_rfc724_mid (const char* grpid, const char* addr);
char* dc_extract_grpid_from_rfc724_mid (const char* rfc724_mid);
char* dc_extract_grpid_from_rfc724_mid_list(const clist* rfc724_mid_list);
void dc_ensure_no_slash (char* pathNfilename);
void dc_validate_filename (char* filename);
char* dc_get_filename (const char* pathNfilename);
void dc_split_filename (const char* pathNfilename, char** ret_basename, char** ret_all_suffixes_incl_dot);
char* dc_get_filesuffix_lc (const char* pathNfilename);
int dc_get_filemeta (const void* buf, size_t buf_bytes, uint32_t* ret_width, uint32_t *ret_height);
char* dc_get_abs_path (dc_context_t*, const char* pathNfilename);
int dc_file_exist (dc_context_t*, const char* pathNfilename);
uint64_t dc_get_filebytes (dc_context_t*, const char* pathNfilename);
int dc_delete_file (dc_context_t*, const char* pathNFilename);
int dc_copy_file (dc_context_t*, const char* pathNFilename, const char* dest_pathNFilename);
int dc_create_folder (dc_context_t*, const char* pathNfilename);
int dc_write_file (dc_context_t*, const char* pathNfilename, const void* buf, size_t buf_bytes);
int dc_read_file (dc_context_t*, const char* pathNfilename, void** buf, size_t* buf_bytes);
char* dc_get_fine_pathNfilename (dc_context_t*, const char* pathNfolder, const char* desired_name);
int dc_is_blobdir_path (dc_context_t*, const char* path);
void dc_make_rel_path (dc_context_t*, char** pathNfilename);
int dc_make_rel_and_copy (dc_context_t*, char** pathNfilename);
#define DC_QUOTEHELPER(name) #name
#define DC_STRINGIFY(macro) DC_QUOTEHELPER(macro)
#define DC_MIN(X, Y) (((X) < (Y))? (X) : (Y))
#define DC_MAX(X, Y) (((X) > (Y))? (X) : (Y))
#ifdef __cplusplus
}
#endif
#endif