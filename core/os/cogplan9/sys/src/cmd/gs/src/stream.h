#ifndef stream_INCLUDED
#  define stream_INCLUDED
#include "scommon.h"
#include "srdline.h"
typedef struct {
#define stream_proc_available(proc)\
int proc(stream *, long *)
stream_proc_available((*available));
#define stream_proc_seek(proc)\
int proc(stream *, long)
stream_proc_seek((*seek));
#define stream_proc_reset(proc)\
void proc(stream *)
stream_proc_reset((*reset));
#define stream_proc_flush(proc)\
int proc(stream *)
stream_proc_flush((*flush));
#define stream_proc_close(proc)\
int proc(stream *)
stream_proc_close((*close));
stream_proc_process((*process));
#define stream_proc_switch_mode(proc)\
int proc(stream *, bool)
stream_proc_switch_mode((*switch_mode));
} stream_procs;
struct stream_s {
stream_state_common;
stream_cursor cursor;
byte *cbuf;
uint bsize;
uint cbsize;
short end_status;
byte foreign;
byte modes;
#define s_mode_read 1
#define s_mode_write 2
#define s_mode_seek 4
#define s_mode_append 8
#define s_is_valid(s) ((s)->modes != 0)
#define s_is_reading(s) (((s)->modes & s_mode_read) != 0)
#define s_is_writing(s) (((s)->modes & s_mode_write) != 0)
#define s_can_seek(s) (((s)->modes & s_mode_seek) != 0)
gs_string cbuf_string;
long position;
stream_procs procs;
stream *strm;
int is_temp;
int inline_temp;
stream_state *state;
ushort read_id;
ushort write_id;
stream *prev, *next;
bool close_strm;
bool close_at_eod;
int (*save_close)(stream *);
FILE *file;
gs_const_string file_name;
uint file_modes;
long file_offset;
long file_limit;
};
extern_st(st_stream);
#define public_st_stream()	\
gs_public_st_composite_final(st_stream, stream, "stream",\
stream_enum_ptrs, stream_reloc_ptrs, stream_finalize)
#define STREAM_NUM_PTRS 6
#define s_init_ids(s) ((s)->read_id = (s)->write_id = 1)
#define s_init_read_id(s) ((s)->read_id = 1, (s)->write_id = 0)
#define s_init_write_id(s) ((s)->read_id = 0, (s)->write_id = 1)
#define s_init_no_id(s) ((s)->read_id = (s)->write_id = 0)
#define srptr cursor.r.ptr
#define srlimit cursor.r.limit
#define swptr cursor.w.ptr
#define swlimit cursor.w.limit
#define sendrp(s) ((s)->srptr >= (s)->srlimit)
#define sendwp(s) ((s)->swptr >= (s)->swlimit)
#define sseekable(s) s_can_seek(s)
int savailable(stream *, long *);
#define sreset(s) (*(s)->procs.reset)(s)
#define sflush(s) (*(s)->procs.flush)(s)
int sclose(stream *);
int sswitch(stream *, bool);
int spgetcc(stream *, bool);
#define spgetc(s) spgetcc(s, true)
#define sgetc(s)\
((int)((s)->srlimit - (s)->srptr > 1 ? (++((s)->srptr), (int)*(s)->srptr) : spgetc(s)))
int sgets(stream *, byte *, uint, uint *);
int sungetc(stream *, byte);
#define sputback(s) ((s)->srptr--)
#define seofp(s) (sendrp(s) && (s)->end_status == EOFC)
#define serrorp(s) (sendrp(s) && (s)->end_status == ERRC)
int spskip(stream *, long, long *);
#define sskip(s,nskip,pskipped) spskip(s, (long)(nskip), pskipped)
int s_process_read_buf(stream *);
int spputc(stream *, byte);
#define sputc(s,c)\
(!sendwp(s) ? (++((s)->swptr), *(s)->swptr=(c), 0) : spputc((s),(c)))
int sputs(stream *, const byte *, uint, uint *);
int s_process_write_buf(stream *, bool);
long stell(stream *);
int spseek(stream *, long);
#define sseek(s,pos) spseek(s, (long)(pos))
#define sbufptr(s) ((s)->srptr + 1)
#define sbufavailable(s) ((s)->srlimit - (s)->srptr)
#define sbufskip(s, n) ((s)->srptr += (n), 0)
#define max_min_left 1
#define sbuf_min_left(s) \
((s->end_status == EOFC || s->end_status == ERRC ? 0 : s->state->min_left))
#define s_declare_inline(s, cp, ep)\
register const byte *cp;\
const byte *ep
#define s_begin_inline(s, cp, ep)\
cp = (s)->srptr, ep = (s)->srlimit
#define s_end_inline(s, cp, ep)\
(s)->srptr = cp
#define sbufavailable_inline(s, cp, ep)\
(ep - cp)
#define sendbufp_inline(s, cp, ep)\
(cp >= ep)
#define sgetc_inline(s, cp, ep)\
((int)(sendbufp_inline(s, cp, ep) ? spgetc_inline(s, cp, ep) : *++cp))
#define spgetc_inline(s, cp, ep)\
(s_end_inline(s, cp, ep), (s)->inline_temp = spgetc(s),\
s_begin_inline(s, cp, ep), (s)->inline_temp)
#define sputback_inline(s, cp, ep)\
--cp
stream *s_alloc(gs_memory_t *, client_name_t);
stream_state *s_alloc_state(gs_memory_t *, gs_memory_type_ptr_t, client_name_t);
void s_init(stream *, gs_memory_t *);
void s_init_state(stream_state *, const stream_template *, gs_memory_t *);
void sread_string(stream *, const byte *, uint),
sread_string_reusable(stream *, const byte *, uint),
swrite_string(stream *, byte *, uint);
void sread_file(stream *, FILE *, byte *, uint),
swrite_file(stream *, FILE *, byte *, uint),
sappend_file(stream *, FILE *, byte *, uint);
int sread_subfile(stream *s, long start, long length);
int ssetfilename(stream *, const byte *, uint);
int sfilename(stream *, gs_const_string *);
void swrite_position_only(stream *);
void s_std_init(stream *, byte *, uint, const stream_procs *, int  );
void s_disable(stream *);
int s_std_null(stream *);
void s_std_read_reset(stream *), s_std_write_reset(stream *);
int s_std_read_flush(stream *), s_std_write_flush(stream *), s_std_noavailable(stream *, long *),
s_std_noseek(stream *, long), s_std_close(stream *), s_std_switch_mode(stream *, bool);
int s_filter_write_flush(stream *), s_filter_close(stream *);
extern const stream_procs s_filter_read_procs, s_filter_write_procs;
int s_init_filter(stream *fs, stream_state *fss, byte *buf, uint bsize,
stream *target);
stream *s_add_filter(stream **ps, const stream_template *template,
stream_state *ss, gs_memory_t *mem);
int s_close_filters(stream **ps, stream *target);
extern const stream_template s_NullE_template;
extern const stream_template s_NullD_template;
#endif