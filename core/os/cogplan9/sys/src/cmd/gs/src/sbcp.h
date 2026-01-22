#ifndef sbcp_INCLUDED
#  define sbcp_INCLUDED
extern const stream_template s_BCPE_template;
extern const stream_template s_TBCPE_template;
typedef struct stream_BCPD_state_s {
stream_state_common;
int (*signal_interrupt) (stream_state *);
int (*request_status) (stream_state *);
bool escaped;
int matched;
int copy_count;
const byte *copy_ptr;
} stream_BCPD_state;
#define private_st_BCPD_state()	\
gs_private_st_simple(st_BCPD_state, stream_BCPD_state, "(T)BCPDecode state")
extern const stream_template s_BCPD_template;
extern const stream_template s_TBCPD_template;
#endif