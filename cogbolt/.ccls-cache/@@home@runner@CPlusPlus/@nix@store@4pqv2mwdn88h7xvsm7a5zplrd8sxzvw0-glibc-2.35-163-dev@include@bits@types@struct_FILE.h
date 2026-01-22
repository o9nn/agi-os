#ifndef __struct_FILE_defined
#define __struct_FILE_defined 1
#if defined _IO_USE_OLD_IO_FILE && !defined _LIBC
# error "_IO_USE_OLD_IO_FILE should only be defined when building libc itself"
#endif
#if defined _IO_lock_t_defined && !defined _LIBC
# error "_IO_lock_t_defined should only be defined when building libc itself"
#endif
#include <bits/types.h>
struct _IO_FILE;
struct _IO_marker;
struct _IO_codecvt;
struct _IO_wide_data;
#ifndef _IO_lock_t_defined
typedef void _IO_lock_t;
#endif
struct _IO_FILE
{
int _flags;
char *_IO_read_ptr;
char *_IO_read_end;
char *_IO_read_base;
char *_IO_write_base;
char *_IO_write_ptr;
char *_IO_write_end;
char *_IO_buf_base;
char *_IO_buf_end;
char *_IO_save_base;
char *_IO_backup_base;
char *_IO_save_end;
struct _IO_marker *_markers;
struct _IO_FILE *_chain;
int _fileno;
int _flags2;
__off_t _old_offset;
unsigned short _cur_column;
signed char _vtable_offset;
char _shortbuf[1];
_IO_lock_t *_lock;
#ifdef _IO_USE_OLD_IO_FILE
};
struct _IO_FILE_complete
{
struct _IO_FILE _file;
#endif
__off64_t _offset;
struct _IO_codecvt *_codecvt;
struct _IO_wide_data *_wide_data;
struct _IO_FILE *_freeres_list;
void *_freeres_buf;
size_t __pad5;
int _mode;
char _unused2[15 * sizeof (int) - 4 * sizeof (void *) - sizeof (size_t)];
};
#define __getc_unlocked_body(_fp)					\
(__glibc_unlikely ((_fp)->_IO_read_ptr >= (_fp)->_IO_read_end)	\
? __uflow (_fp) : *(unsigned char *) (_fp)->_IO_read_ptr++)
#define __putc_unlocked_body(_ch, _fp)					\
(__glibc_unlikely ((_fp)->_IO_write_ptr >= (_fp)->_IO_write_end)	\
? __overflow (_fp, (unsigned char) (_ch))				\
: (unsigned char) (*(_fp)->_IO_write_ptr++ = (_ch)))
#define _IO_EOF_SEEN 0x0010
#define __feof_unlocked_body(_fp) (((_fp)->_flags & _IO_EOF_SEEN) != 0)
#define _IO_ERR_SEEN 0x0020
#define __ferror_unlocked_body(_fp) (((_fp)->_flags & _IO_ERR_SEEN) != 0)
#define _IO_USER_LOCK 0x8000
#endif