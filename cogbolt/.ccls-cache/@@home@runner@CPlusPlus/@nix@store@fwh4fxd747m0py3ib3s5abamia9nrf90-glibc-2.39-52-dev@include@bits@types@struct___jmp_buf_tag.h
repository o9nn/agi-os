#ifndef	__jmp_buf_tag_defined
#define	__jmp_buf_tag_defined 1
#include <bits/setjmp.h>
#include <bits/types/__sigset_t.h>
struct __jmp_buf_tag
{
__jmp_buf __jmpbuf;
int __mask_was_saved;
__sigset_t __saved_mask;
};
#endif