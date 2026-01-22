#ifndef gspaint_INCLUDED
#  define gspaint_INCLUDED
int gs_erasepage(gs_state *),
gs_fillpage(gs_state *),
gs_fill(gs_state *),
gs_eofill(gs_state *),
gs_stroke(gs_state *);
int gs_imagepath(gs_state *, int, int, const byte *);
#endif