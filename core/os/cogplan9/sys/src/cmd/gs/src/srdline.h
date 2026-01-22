#ifndef srdline_INCLUDED
# define srdline_INCLUDED
#ifndef stream_DEFINED
# define stream_DEFINED
typedef struct stream_s stream;
#endif
#define sreadline_proc(proc)\
int proc(stream *s_in, stream *s_out, void *readline_data,\
gs_const_string *prompt, gs_string *buf,\
gs_memory_t *bufmem, uint *pcount, bool *pin_eol,\
bool (*is_stdin)(const stream *))
extern sreadline_proc(sreadline);
#endif