#ifndef gxclio_INCLUDED
#  define gxclio_INCLUDED
#include "gp.h"
typedef void *clist_file_ptr;
int clist_fopen(char fname[gp_file_name_sizeof], const char *fmode,
clist_file_ptr * pcf,
gs_memory_t * mem, gs_memory_t *data_mem,
bool ok_to_compress);
int clist_fclose(clist_file_ptr cf, const char *fname, bool delete);
int clist_unlink(const char *fname);
long clist_space_available(long requested);
int clist_fwrite_chars(const void *data, uint len, clist_file_ptr cf);
int clist_fread_chars(void *data, uint len, clist_file_ptr cf);
int clist_set_memory_warning(clist_file_ptr cf, int bytes_left);
int clist_ferror_code(clist_file_ptr cf);
long clist_ftell(clist_file_ptr cf);
void clist_rewind(clist_file_ptr cf, bool discard_data, const char *fname);
int clist_fseek(clist_file_ptr cf, long offset, int mode, const char *fname);
#endif