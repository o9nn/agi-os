#include "stdio_.h"
#include "string_.h"
#include "unistd_.h"
#include "gserror.h"
#include "gserrors.h"
#include "gsmemory.h"
#include "gp.h"
#include "gxclio.h"
int
clist_fopen(char fname[gp_file_name_sizeof], const char *fmode,
clist_file_ptr * pcf, gs_memory_t * mem, gs_memory_t *data_mem,
bool ok_to_compress)
{
if (*fname == 0) {
if (fmode[0] == 'r')
return_error(gs_error_invalidfileaccess);
*pcf =
(clist_file_ptr) gp_open_scratch_file(gp_scratch_file_name_prefix,
fname, fmode);
} else
*pcf = gp_fopen(fname, fmode);
if (*pcf == NULL) {
eprintf1("Could not open the scratch file %s.\n", fname);
return_error(gs_error_invalidfileaccess);
}
return 0;
}
int
clist_fclose(clist_file_ptr cf, const char *fname, bool delete)
{
return (fclose((FILE *) cf) != 0 ? gs_note_error(gs_error_ioerror) :
delete ? clist_unlink(fname) :
0);
}
int
clist_unlink(const char *fname)
{
return (unlink(fname) != 0 ? gs_note_error(gs_error_ioerror) : 0);
}
long
clist_space_available(long requested)
{
return requested;
}
int
clist_fwrite_chars(const void *data, uint len, clist_file_ptr cf)
{
return fwrite(data, 1, len, (FILE *) cf);
}
int
clist_fread_chars(void *data, uint len, clist_file_ptr cf)
{
FILE *f = (FILE *) cf;
byte *str = data;
switch (len) {
default:
return fread(str, 1, len, f);
case 8:
*str++ = (byte) getc(f);
case 7:
*str++ = (byte) getc(f);
case 6:
*str++ = (byte) getc(f);
case 5:
*str++ = (byte) getc(f);
case 4:
*str++ = (byte) getc(f);
case 3:
*str++ = (byte) getc(f);
case 2:
*str++ = (byte) getc(f);
case 1:
*str = (byte) getc(f);
}
return len;
}
int
clist_set_memory_warning(clist_file_ptr cf, int bytes_left)
{
return 0;
}
int
clist_ferror_code(clist_file_ptr cf)
{
return (ferror((FILE *) cf) ? gs_error_ioerror : 0);
}
long
clist_ftell(clist_file_ptr cf)
{
return ftell((FILE *) cf);
}
void
clist_rewind(clist_file_ptr cf, bool discard_data, const char *fname)
{
FILE *f = (FILE *) cf;
if (discard_data) {
char fmode[4];
freopen(fname, gp_fmode_wb, f);
strcpy(fmode, "w+");
strcat(fmode, gp_fmode_binary_suffix);
freopen(fname, fmode, f);
} else {
rewind(f);
}
}
int
clist_fseek(clist_file_ptr cf, long offset, int mode, const char *ignore_fname)
{
return fseek((FILE *) cf, offset, mode);
}