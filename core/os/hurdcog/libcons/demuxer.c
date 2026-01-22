#include "cons.h"
int
cons_demuxer (mach_msg_header_t *inp, mach_msg_header_t *outp)
{
int cons_fs_notify_server (mach_msg_header_t *inp, mach_msg_header_t *outp);
return (cons_fs_notify_server (inp, outp));
}