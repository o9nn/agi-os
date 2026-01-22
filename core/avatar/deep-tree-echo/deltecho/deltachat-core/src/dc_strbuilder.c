#include "dc_context.h"
void dc_strbuilder_init(dc_strbuilder_t* strbuilder, int init_bytes)
{
if (strbuilder==NULL) {
return;
}
strbuilder->allocated    = DC_MAX(init_bytes, 128);
strbuilder->buf          = malloc(strbuilder->allocated);
if (strbuilder->buf==NULL) {
exit(38);
}
strbuilder->buf[0]       = 0;
strbuilder->free         = strbuilder->allocated - 1 ;
strbuilder->eos          = strbuilder->buf;
}
char* dc_strbuilder_cat(dc_strbuilder_t* strbuilder, const char* text)
{
if (strbuilder==NULL || text==NULL) {
return NULL;
}
int len = strlen(text);
if (len > strbuilder->free) {
int add_bytes  = DC_MAX(len, strbuilder->allocated);
int old_offset = (int)(strbuilder->eos - strbuilder->buf);
strbuilder->allocated = strbuilder->allocated + add_bytes;
strbuilder->buf       = realloc(strbuilder->buf, strbuilder->allocated+add_bytes);
if (strbuilder->buf==NULL) {
exit(39);
}
strbuilder->free      = strbuilder->free + add_bytes;
strbuilder->eos       = strbuilder->buf + old_offset;
}
char* ret = strbuilder->eos;
strcpy(strbuilder->eos, text);
strbuilder->eos += len;
strbuilder->free -= len;
return ret;
}
void dc_strbuilder_catf(dc_strbuilder_t* strbuilder, const char* format, ...)
{
char  testbuf[1];
char* buf = NULL;
int   char_cnt_without_zero = 0;
va_list argp;
va_list argp_copy;
va_start(argp, format);
va_copy(argp_copy, argp);
char_cnt_without_zero = vsnprintf(testbuf, 0, format, argp);
va_end(argp);
if (char_cnt_without_zero < 0) {
va_end(argp_copy);
dc_strbuilder_cat(strbuilder, "ErrFmt");
return;
}
buf = malloc(char_cnt_without_zero+2 );
if (buf==NULL) {
va_end(argp_copy);
dc_strbuilder_cat(strbuilder, "ErrMem");
return;
}
vsnprintf(buf, char_cnt_without_zero+1, format, argp_copy);
va_end(argp_copy);
dc_strbuilder_cat(strbuilder, buf);
free(buf);
}
void dc_strbuilder_empty(dc_strbuilder_t* strbuilder)
{
strbuilder->buf[0] = 0;
strbuilder->free   = strbuilder->allocated - 1 ;
strbuilder->eos    = strbuilder->buf;
}