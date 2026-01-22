#ifndef gpcheck_INCLUDED
# define gpcheck_INCLUDED
int gs_return_check_interrupt(const gs_memory_t *mem, int code);
#ifdef CHECK_INTERRUPTS
int gp_check_interrupts(const gs_memory_t *mem);
# define process_interrupts(mem) discard(gp_check_interrupts(mem))
# define return_if_interrupt(mem)\
{ int icode_ = gp_check_interrupts(mem); \
if ( icode_ )\
return gs_note_error((icode_ > 0 ? gs_error_interrupt : icode_));\
}
# define return_check_interrupt(mem, code) \
return gs_return_check_interrupt(mem, code)
# define set_code_on_interrupt(mem, pcode) \
if (*(pcode) == 0)\
*(pcode) = (gp_check_interrupts(mem) != 0) ? gs_error_interrupt : 0;
#else
# define gp_check_interrupts(mem) 0
# define process_interrupts(mem) DO_NOTHING
# define return_if_interrupt(mem) DO_NOTHING
# define return_check_interrupt(mem, code) \
return (code)
# define set_code_on_interrupt(mem, code) DO_NOTHING
#endif
#endif