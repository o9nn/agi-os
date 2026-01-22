#ifndef files_INCLUDED
#  define files_INCLUDED
#define fptr(pref) (pref)->value.pfile
#define make_file(pref,a,id,s)\
make_tasv(pref,t_file,a,id,pfile,s)
int zget_stdin(i_ctx_t *, stream **);
int zget_stdout(i_ctx_t *, stream **);
int zget_stderr(i_ctx_t *, stream **);
bool zis_stdin(const stream *);
#define ref_stdio (i_ctx_p->stdio)
#define ref_stdin ref_stdio[0]
#define ref_stdout ref_stdio[1]
#define ref_stderr ref_stdio[2]
#define avm_invalid_file_entry avm_foreign
extern stream *const invalid_file_entry;
void make_invalid_file(ref *);
#define file_is_valid(svar,op)\
(svar = fptr(op), (svar->read_id | svar->write_id) == r_size(op))
#define file_is_invalid(svar,op)\
(svar = fptr(op), (svar->read_id | svar->write_id) != r_size(op))
#define check_file(svar,op)\
BEGIN\
check_type(*(op), t_file);\
if ( file_is_invalid(svar, op) ) return_error(e_invalidaccess);\
END
int file_switch_to_read(const ref *);
#define check_read_file(svar,op)\
BEGIN\
check_read_type(*(op), t_file);\
check_read_known_file(svar, op, return);\
END
#define check_read_known_file(svar,op,error_return)\
check_read_known_file_else(svar, op, error_return, svar = invalid_file_entry)
#define check_read_known_file_else(svar,op,error_return,invalid_action)\
BEGIN\
svar = fptr(op);\
if (svar->read_id != r_size(op)) {\
if (svar->read_id == 0 && svar->write_id == r_size(op)) {\
int fcode = file_switch_to_read(op);\
\
if (fcode < 0)\
error_return(fcode);\
} else {\
invalid_action;	\
}\
}\
END
int file_switch_to_write(const ref *);
#define check_write_file(svar,op)\
BEGIN\
check_write_type(*(op), t_file);\
check_write_known_file(svar, op, return);\
END
#define check_write_known_file(svar,op,error_return)\
BEGIN\
svar = fptr(op);\
if ( svar->write_id != r_size(op) )\
{	int fcode = file_switch_to_write(op);\
if ( fcode < 0 ) error_return(fcode);\
}\
END
extern const uint file_default_buffer_size;
#ifndef gs_file_path_ptr_DEFINED
#  define gs_file_path_ptr_DEFINED
typedef struct gs_file_path_s *gs_file_path_ptr;
#endif
FILE *lib_fopen(const gs_file_path_ptr pfpath, const gs_memory_t *mem, const char *);
int lib_file_open(const gs_file_path_ptr pfpath, i_ctx_t *, const char *, uint, byte *, uint,
uint *, ref *, gs_memory_t *);
#ifndef gs_ref_memory_DEFINED
#  define gs_ref_memory_DEFINED
typedef struct gs_ref_memory_s gs_ref_memory_t;
#endif
int file_read_string(const byte *, uint, ref *, gs_ref_memory_t *);
#ifdef iodev_proc_fopen
int file_open_stream(const char *, uint, const char *, uint, stream **,
gx_io_device *, iodev_proc_fopen_t, gs_memory_t *);
#endif
int filter_open(const char *, uint, ref *, const stream_procs *,
const stream_template *, const stream_state *,
gs_memory_t *);
void make_stream_file(ref *, stream *, const char *);
int file_close_finish(stream *);
int file_close_disable(stream *);
int file_close_file(stream *);
int file_close(ref *);
stream *file_alloc_stream(gs_memory_t *, client_name_t);
int zreadline_from(stream *s, gs_string *buf, gs_memory_t *bufmem,
uint *pcount, bool *pin_eol);
int zfilelineedit(i_ctx_t *i_ctx_p);
int zneedstdin(i_ctx_t *i_ctx_p);
int zneedstdout(i_ctx_t *i_ctx_p);
int zneedstderr(i_ctx_t *i_ctx_p);
#endif