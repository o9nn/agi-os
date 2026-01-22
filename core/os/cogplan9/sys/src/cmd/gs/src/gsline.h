#ifndef gsline_INCLUDED
# define gsline_INCLUDED
#include "gslparam.h"
int gs_setlinewidth(gs_state *, floatp);
float gs_currentlinewidth(const gs_state *);
int gs_setlinecap(gs_state *, gs_line_cap);
gs_line_cap gs_currentlinecap(const gs_state *);
int gs_setlinejoin(gs_state *, gs_line_join);
gs_line_join gs_currentlinejoin(const gs_state *);
int gs_setmiterlimit(gs_state *, floatp);
float gs_currentmiterlimit(const gs_state *);
int gs_setdash(gs_state *, const float *, uint, floatp);
uint gs_currentdash_length(const gs_state *);
const float *gs_currentdash_pattern(const gs_state *);
float gs_currentdash_offset(const gs_state *);
int gs_setflat(gs_state *, floatp);
float gs_currentflat(const gs_state *);
int gs_setstrokeadjust(gs_state *, bool);
bool gs_currentstrokeadjust(const gs_state *);
void gs_setdashadapt(gs_state *, bool);
bool gs_currentdashadapt(const gs_state *);
int gs_setcurvejoin(gs_state *, int);
int gs_currentcurvejoin(const gs_state *);
void gs_setaccuratecurves(gs_state *, bool);
bool gs_currentaccuratecurves(const gs_state *);
int gs_setdotlength(gs_state *, floatp, bool);
float gs_currentdotlength(const gs_state *);
bool gs_currentdotlength_absolute(const gs_state *);
int gs_setdotorientation(gs_state *);
int gs_dotorientation(gs_state *);
#ifndef gs_imager_state_DEFINED
# define gs_imager_state_DEFINED
typedef struct gs_imager_state_s gs_imager_state;
#endif
int gs_imager_setflat(gs_imager_state *, floatp);
bool gs_imager_currentdashadapt(const gs_imager_state *);
bool gs_imager_currentaccuratecurves(const gs_imager_state *);
#endif