#ifndef dwtrace_INCLUDED
#  define dwtrace_INCLUDED
extern struct vd_trace_interface_s visual_tracer;
void visual_tracer_init(void);
void visual_tracer_close(void);
#endif