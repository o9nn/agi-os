#ifndef istream_INCLUDED
# define istream_INCLUDED
int sread_proc(ref *, stream **, gs_ref_memory_t *);
int swrite_proc(ref *, stream **, gs_ref_memory_t *);
int s_handle_read_exception(i_ctx_t *, int, const ref *, const ref *,
int, op_proc_t);
int s_handle_write_exception(i_ctx_t *, int, const ref *, const ref *,
int, op_proc_t);
#endif