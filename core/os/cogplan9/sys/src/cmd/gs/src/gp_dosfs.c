#include "dos_.h"
#include "gx.h"
#include "gp.h"
#include "gpmisc.h"
void
gp_set_file_binary(int prnfno, bool binary)
{
union REGS regs;
regs.h.ah = 0x44;
regs.h.al = 0;
regs.rshort.bx = prnfno;
intdos(&regs, &regs);
if (regs.rshort.cflag != 0 || !(regs.h.dl & 0x80))
return;
if (binary)
regs.h.dl |= 0x20;
else
regs.h.dl &= ~0x20;
regs.h.dh = 0;
regs.h.ah = 0x44;
regs.h.al = 1;
intdos(&regs, &regs);
}
int
gp_setmode_binary(FILE * pfile, bool binary)
{
gp_set_file_binary(fileno(pfile), binary);
return 0;
}
const char gp_file_name_list_separator = ';';
const char gp_fmode_binary_suffix[] = "b";
const char gp_fmode_rb[] = "rb";
const char gp_fmode_wb[] = "wb";
uint gp_file_name_root(const char *fname, uint len)
{ int i = 0;
if (len == 0)
return 0;
if (len > 1 && fname[0] == '\\' && fname[1] == '\\') {
int k = 0;
for (i = 2; i < len; i++)
if (fname[i] == '\\' || fname[i] == '/')
if (k++) {
i++;
break;
}
} else if (fname[0] == '/' || fname[0] == '\\') {
i = 1;
} else if (len > 1 && fname[1] == ':') {
i = (len > 2 && (fname[2] == '/' || fname[2] == '\\') ? 3 : 2);
}
return i;
}
uint gs_file_name_check_separator(const char *fname, int len, const char *item)
{ if (len > 0) {
if (fname[0] == '/' || fname[0] == '\\')
return 1;
} else if (len < 0) {
if (fname[-1] == '/' || fname[-1] == '\\')
return 1;
}
return 0;
}
bool gp_file_name_is_parent(const char *fname, uint len)
{ return len == 2 && fname[0] == '.' && fname[1] == '.';
}
bool gp_file_name_is_current(const char *fname, uint len)
{ return len == 1 && fname[0] == '.';
}
const char *gp_file_name_separator(void)
{ return "/";
}
const char *gp_file_name_directory_separator(void)
{ return "/";
}
const char *gp_file_name_parent(void)
{ return "..";
}
const char *gp_file_name_current(void)
{ return ".";
}
bool gp_file_name_is_partent_allowed(void)
{ return true;
}
bool gp_file_name_is_empty_item_meanful(void)
{ return false;
}
gp_file_name_combine_result
gp_file_name_combine(const char *prefix, uint plen, const char *fname, uint flen,
bool no_sibling, char *buffer, uint *blen)
{
return gp_file_name_combine_generic(prefix, plen,
fname, flen, no_sibling, buffer, blen);
}