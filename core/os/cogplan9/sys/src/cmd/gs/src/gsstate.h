#ifndef gsstate_INCLUDED
#  define gsstate_INCLUDED
#ifndef gs_state_DEFINED
#  define gs_state_DEFINED
typedef struct gs_state_s gs_state;
#endif
#ifndef gs_overprint_params_t_DEFINED
#  define gs_overprint_params_t_DEFINED
typedef struct gs_overprint_params_s    gs_overprint_params_t;
#endif
gs_state *gs_state_alloc(gs_memory_t *);
int gs_state_free(gs_state *);
int gs_gsave(gs_state *), gs_grestore(gs_state *), gs_grestoreall(gs_state *);
int gs_grestore_only(gs_state *);
int gs_gsave_for_save(gs_state *, gs_state **), gs_grestoreall_for_restore(gs_state *, gs_state *);
gs_state *gs_gstate(gs_state *);
gs_state *gs_state_copy(gs_state *, gs_memory_t *);
int gs_copygstate(gs_state *  , const gs_state *  ),
gs_currentgstate(gs_state *  , const gs_state *  ),
gs_setgstate(gs_state *  , const gs_state *  );
int gs_state_update_overprint(gs_state *, const gs_overprint_params_t *);
bool gs_currentoverprint(const gs_state *);
void gs_setoverprint(gs_state *, bool);
int gs_currentoverprintmode(const gs_state *);
int gs_setoverprintmode(gs_state *, int);
int gs_do_set_overprint(gs_state *);
int gs_initgraphics(gs_state *);
#include "gsdevice.h"
#include "gsline.h"
#include "gscolor.h"
#include "gsht.h"
#include "gscsel.h"
int gs_setscreenphase(gs_state *, int, int, gs_color_select_t);
int gs_currentscreenphase(const gs_state *, gs_int_point *, gs_color_select_t);
#define gs_sethalftonephase(pgs, px, py)\
gs_setscreenphase(pgs, px, py, gs_color_select_all)
#define gs_currenthalftonephase(pgs, ppt)\
gs_currentscreenphase(pgs, ppt, 0)
int gx_imager_setscreenphase(gs_imager_state *, int, int, gs_color_select_t);
int gs_setfilladjust(gs_state *, floatp, floatp);
int gs_currentfilladjust(const gs_state *, gs_point *);
void gs_setlimitclamp(gs_state *, bool);
bool gs_currentlimitclamp(const gs_state *);
void gs_settextrenderingmode(gs_state * pgs, uint trm);
uint gs_currenttextrenderingmode(const gs_state * pgs);
#include "gscpm.h"
gs_in_cache_device_t gs_incachedevice(const gs_state *);
#endif