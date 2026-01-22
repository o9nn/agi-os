#ifndef ____mbstate_t_defined
#define ____mbstate_t_defined 1
#ifndef __WINT_TYPE__
# define __WINT_TYPE__ unsigned int
#endif
typedef struct
{
int __count;
union
{
__WINT_TYPE__ __wch;
char __wchb[4];
} __value;
} __mbstate_t;
#endif