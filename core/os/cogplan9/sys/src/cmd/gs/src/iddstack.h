#ifndef iddstack_INCLUDED
# define iddstack_INCLUDED
#ifndef dict_stack_DEFINED
# define dict_stack_DEFINED
typedef struct dict_stack_s dict_stack_t;
#endif
void dstack_set_top(dict_stack_t *);
bool dstack_dict_is_permanent(const dict_stack_t *, const ref *);
#endif