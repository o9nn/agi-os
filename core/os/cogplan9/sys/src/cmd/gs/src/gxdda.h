#ifndef gxdda_INCLUDED
#  define gxdda_INCLUDED
#define dda_state_struct(sname, dtype, ntype)\
struct sname { dtype Q; ntype R; }
#define dda_step_struct(sname, dtype, ntype)\
struct sname { dtype dQ; ntype dR, NdR; }
typedef
dda_state_struct(_a, fixed, uint) gx_dda_state_fixed;
typedef dda_step_struct(_e, fixed, uint) gx_dda_step_fixed;
typedef struct gx_dda_fixed_s {
gx_dda_state_fixed state;
gx_dda_step_fixed step;
} gx_dda_fixed;
typedef struct gx_dda_fixed_point_s {
gx_dda_fixed x, y;
} gx_dda_fixed_point;
#define dda_init_state(dstate, init, N)\
(dstate).Q = (init), (dstate).R = (N)
#define dda_init_step(dstep, D, N)\
if ( (N) == 0 )\
(dstep).dQ = 0, (dstep).dR = 0;\
else if ( (D) < 0 )\
{ (dstep).dQ = -(int)((uint)-(D) / (N));\
if ( ((dstep).dR = -(D) % (N)) != 0 )\
--(dstep).dQ, (dstep).dR = (N) - (dstep).dR;\
}\
else\
{ (dstep).dQ = (D) / (N); (dstep).dR = (D) % (N); }\
(dstep).NdR = (N) - (dstep).dR
#define dda_init(dda, init, D, N)\
dda_init_state((dda).state, init, N);\
dda_init_step((dda).step, D, N)
#define dda_step_add(tostep, fromstep)\
(tostep).dQ +=\
((tostep).dR < (fromstep).NdR ?\
((tostep).dR += (fromstep).dR, (tostep).NdR -= (fromstep).dR,\
(fromstep).dQ) :\
((tostep).dR -= (fromstep).NdR, (tostep).NdR += (fromstep).NdR,\
(fromstep).dQ + 1))
#define dda_state_current(dstate) (dstate).Q
#define dda_current(dda) dda_state_current((dda).state)
#define dda_current_fixed2int(dda)\
fixed2int_var(dda_state_current((dda).state))
#define dda_state_next(dstate, dstep)\
(dstate).Q +=\
((dstate).R > (dstep).dR ?\
((dstate).R -= (dstep).dR, (dstep).dQ) :\
((dstate).R += (dstep).NdR, (dstep).dQ + 1))
#define dda_next(dda) dda_state_next((dda).state, (dda).step)
#define dda_state_previous(dstate, dstep)\
(dstate).Q -=\
((dstate).R <= (dstep).NdR ?\
((dstate).R += (dstep).dR, (dstep).dQ) :\
((dstate).R -= (dstep).NdR, (dstep).dQ + 1))
#define dda_previous(dda) dda_state_previous((dda).state, (dda).step)
#define dda_state_advance(dstate, dstep, nsteps)\
BEGIN\
uint n_ = (nsteps);\
(dstate).Q += (dstep).dQ * (nsteps);\
while ( n_-- )\
if ( (dstate).R > (dstep).dR ) (dstate).R -= (dstep).dR;\
else (dstate).R += (dstep).NdR, (dstate).Q++;\
END
#define dda_advance(dda, nsteps)\
dda_state_advance((dda).state, (dda).step, nsteps)
#define dda_state_translate(dstate, delta)\
((dstate).Q += (delta))
#define dda_translate(dda, delta)\
dda_state_translate((dda).state, delta)
#endif