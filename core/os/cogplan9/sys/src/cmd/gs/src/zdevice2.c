#include "math_.h"
#include "memory_.h"
#include "ghost.h"
#include "oper.h"
#include "dstack.h"
#include "estack.h"
#include "idict.h"
#include "idparam.h"
#include "igstate.h"
#include "iname.h"
#include "iutil.h"
#include "store.h"
#include "gxdevice.h"
#include "gsstate.h"
int z2copy(i_ctx_t *);
private int z2copy_gstate(i_ctx_t *);
private int push_callout(i_ctx_t *, const char *);
int
z2copy(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
int code = zcopy(i_ctx_p);
if (code >= 0)
return code;
if (!r_has_type(op, t_astruct))
return code;
return z2copy_gstate(i_ctx_p);
}
private int
zcurrentshowpagecount(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gx_device *dev = gs_currentdevice(igs);
if ((*dev_proc(dev, get_page_device))(dev) == 0) {
push(1);
make_false(op);
} else {
push(2);
make_int(op - 1, dev->ShowpageCount);
make_true(op);
}
return 0;
}
private int
zcurrentpagedevice(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gx_device *dev = gs_currentdevice(igs);
push(2);
if ((*dev_proc(dev, get_page_device))(dev) != 0) {
op[-1] = istate->pagedevice;
make_true(op);
} else {
make_null(op - 1);
make_false(op);
}
return 0;
}
private int
zsetpagedevice(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
int code;
if (r_has_type(op, t_dictionary)) {
check_dict_read(*op);
#if 0
if (!(r_is_local(op)))
return_error(e_invalidaccess);
#endif
code = zreadonly(i_ctx_p);
if (code < 0)
return code;
} else {
check_type(*op, t_null);
}
istate->pagedevice = *op;
pop(1);
return 0;
}
private int
zcallinstall(i_ctx_t *i_ctx_p)
{
gx_device *dev = gs_currentdevice(igs);
if ((dev = (*dev_proc(dev, get_page_device))(dev)) != 0) {
int code = (*dev->page_procs.install) (dev, igs);
if (code < 0)
return code;
}
return 0;
}
private int
zcallbeginpage(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gx_device *dev = gs_currentdevice(igs);
check_type(*op, t_integer);
if ((dev = (*dev_proc(dev, get_page_device))(dev)) != 0) {
int code = (*dev->page_procs.begin_page)(dev, igs);
if (code < 0)
return code;
}
pop(1);
return 0;
}
private int
zcallendpage(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gx_device *dev = gs_currentdevice(igs);
int code;
check_type(op[-1], t_integer);
check_type(*op, t_integer);
if ((dev = (*dev_proc(dev, get_page_device))(dev)) != 0) {
code = (*dev->page_procs.end_page)(dev, (int)op->value.intval, igs);
if (code < 0)
return code;
if (code > 1)
return_error(e_rangecheck);
} else {
code = (op->value.intval == 2 ? 0 : 1);
}
make_bool(op - 1, code);
pop(1);
return 0;
}
private bool
save_page_device(gs_state *pgs)
{
return
(r_has_type(&gs_int_gstate(pgs)->pagedevice, t_null) &&
(*dev_proc(gs_currentdevice(pgs), get_page_device))(gs_currentdevice(pgs)) != 0);
}
private int
z2gsave(i_ctx_t *i_ctx_p)
{
if (!save_page_device(igs))
return gs_gsave(igs);
return push_callout(i_ctx_p, "%gsavepagedevice");
}
private int
z2save(i_ctx_t *i_ctx_p)
{
if (!save_page_device(igs))
return zsave(i_ctx_p);
return push_callout(i_ctx_p, "%savepagedevice");
}
private int
z2gstate(i_ctx_t *i_ctx_p)
{
if (!save_page_device(igs))
return zgstate(i_ctx_p);
return push_callout(i_ctx_p, "%gstatepagedevice");
}
private int
z2copy_gstate(i_ctx_t *i_ctx_p)
{
if (!save_page_device(igs))
return zcopy_gstate(i_ctx_p);
return push_callout(i_ctx_p, "%copygstatepagedevice");
}
private int
z2currentgstate(i_ctx_t *i_ctx_p)
{
if (!save_page_device(igs))
return zcurrentgstate(i_ctx_p);
return push_callout(i_ctx_p, "%currentgstatepagedevice");
}
private bool
restore_page_device(const gs_state * pgs_old, const gs_state * pgs_new)
{
gx_device *dev_old = gs_currentdevice(pgs_old);
gx_device *dev_new;
gx_device *dev_t1;
gx_device *dev_t2;
bool samepagedevice = obj_eq(dev_old->memory, &gs_int_gstate(pgs_old)->pagedevice,
&gs_int_gstate(pgs_new)->pagedevice);
if ((dev_t1 = (*dev_proc(dev_old, get_page_device)) (dev_old)) == 0)
return false;
if (!samepagedevice)
dev_old->LockSafetyParams = false;
dev_new = gs_currentdevice(pgs_new);
if (dev_old != dev_new) {
if ((dev_t2 = (*dev_proc(dev_new, get_page_device)) (dev_new)) == 0)
return false;
if (dev_t1 != dev_t2)
return true;
}
return !samepagedevice;
}
private int
z2grestore(i_ctx_t *i_ctx_p)
{
if (!restore_page_device(igs, gs_state_saved(igs)))
return gs_grestore(igs);
return push_callout(i_ctx_p, "%grestorepagedevice");
}
private int
z2grestoreall(i_ctx_t *i_ctx_p)
{
for (;;) {
if (!restore_page_device(igs, gs_state_saved(igs))) {
bool done = !gs_state_saved(gs_state_saved(igs));
gs_grestore(igs);
if (done)
break;
} else
return push_callout(i_ctx_p, "%grestoreallpagedevice");
}
return 0;
}
private int
z2restore(i_ctx_t *i_ctx_p)
{
while (gs_state_saved(gs_state_saved(igs))) {
if (restore_page_device(igs, gs_state_saved(igs)))
return push_callout(i_ctx_p, "%restore1pagedevice");
gs_grestore(igs);
}
if (restore_page_device(igs, gs_state_saved(igs)))
return push_callout(i_ctx_p, "%restorepagedevice");
return zrestore(i_ctx_p);
}
private int
z2setgstate(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_stype(*op, st_igstate_obj);
if (!restore_page_device(igs, igstate_ptr(op)))
return zsetgstate(i_ctx_p);
return push_callout(i_ctx_p, "%setgstatepagedevice");
}
const op_def zdevice2_l2_op_defs[] =
{
op_def_begin_level2(),
{"0.currentshowpagecount", zcurrentshowpagecount},
{"0.currentpagedevice", zcurrentpagedevice},
{"1.setpagedevice", zsetpagedevice},
{"1copy", z2copy},
{"0gsave", z2gsave},
{"0save", z2save},
{"0gstate", z2gstate},
{"1currentgstate", z2currentgstate},
{"0grestore", z2grestore},
{"0grestoreall", z2grestoreall},
{"1restore", z2restore},
{"1setgstate", z2setgstate},
{"0.callinstall", zcallinstall},
{"1.callbeginpage", zcallbeginpage},
{"2.callendpage", zcallendpage},
op_def_end(0)
};
private int
push_callout(i_ctx_t *i_ctx_p, const char *callout_name)
{
int code;
check_estack(1);
code = name_enter_string(imemory, callout_name, esp + 1);
if (code < 0)
return code;
++esp;
r_set_attrs(esp, a_executable);
return o_push_estack;
}