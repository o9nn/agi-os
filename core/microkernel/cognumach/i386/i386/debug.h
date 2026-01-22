#ifndef _I386_DEBUG_
#define _I386_DEBUG_
#ifndef __ASSEMBLER__
void dump_ss(const struct i386_saved_state *st);
#endif
#ifdef DEBUG
#define DEBUG_TRACE_LEN 512
#ifndef __ASSEMBLER__
#define DEBUG_TRACE _debug_trace(__FILE__,__LINE__)
void debug_trace_reset(void);
void debug_trace_dump(void);
#else
#define DEBUG_TRACE \
pushl $__LINE__ ;\
pushl $9f ;\
call __debug_trace ;\
addl $8,%esp ;\
.data ;\
9: .ascii __FILE__"\0" ;\
.text
#endif
#endif
#endif