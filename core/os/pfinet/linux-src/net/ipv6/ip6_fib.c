#include <linux/config.h>
#include <linux/errno.h>
#include <linux/types.h>
#include <linux/net.h>
#include <linux/route.h>
#include <linux/netdevice.h>
#include <linux/in6.h>
#ifdef 	CONFIG_PROC_FS
#include <linux/proc_fs.h>
#endif
#include <net/ipv6.h>
#include <net/ndisc.h>
#include <net/addrconf.h>
#include <net/ip6_fib.h>
#include <net/ip6_route.h>
#define RT6_DEBUG 2
#undef CONFIG_IPV6_SUBTREES
#if RT6_DEBUG >= 1
#define BUG_TRAP(x) ({ if (!(x)) { printk("Assertion (" #x ") failed at " __FILE__ "(%d):%s\n", __LINE__, __FUNCTION__); } })
#else
#define BUG_TRAP(x) do { ; } while (0)
#endif
#if RT6_DEBUG >= 3
#define RT6_TRACE(x...) printk(KERN_DEBUG x)
#else
#define RT6_TRACE(x...) do { ; } while (0)
#endif
struct rt6_statistics	rt6_stats;
enum fib_walk_state_t
{
#ifdef CONFIG_IPV6_SUBTREES
FWS_S,
#endif
FWS_L,
FWS_R,
FWS_C,
FWS_U
};
struct fib6_cleaner_t
{
struct fib6_walker_t w;
int (*func)(struct rt6_info *, void *arg);
void *arg;
};
#ifdef CONFIG_IPV6_SUBTREES
#define FWS_INIT FWS_S
#define SUBTREE(fn) ((fn)->subtree)
#else
#define FWS_INIT FWS_L
#define SUBTREE(fn) NULL
#endif
static void fib6_prune_clones(struct fib6_node *fn, struct rt6_info *rt);
static void fib6_repair_tree(struct fib6_node *fn);
static __u32	rt_sernum	= 0;
static struct timer_list ip6_fib_timer = {
NULL, NULL,
0,
~0UL,
fib6_run_gc
};
struct fib6_walker_t fib6_walker_list = {
&fib6_walker_list, &fib6_walker_list,
};
#define FOR_WALKERS(w) for ((w)=fib6_walker_list.next; (w) != &fib6_walker_list; (w)=(w)->next)
static __inline__ u32 fib6_new_sernum(void)
{
u32 n = ++rt_sernum;
if ((__s32)n <= 0)
rt_sernum = n = 1;
return n;
}
static __inline__ int addr_match(void *token1, void *token2, int prefixlen)
{
__u32 *a1 = token1;
__u32 *a2 = token2;
int pdw;
int pbi;
pdw = prefixlen >> 5;
pbi = prefixlen &  0x1f;
if (pdw)
if (memcmp(a1, a2, pdw << 2))
return 0;
if (pbi) {
__u32 mask;
mask = htonl((0xffffffff) << (32 - pbi));
if ((a1[pdw] ^ a2[pdw]) & mask)
return 0;
}
return 1;
}
static __inline__ int addr_bit_set(void *token, int fn_bit)
{
__u32 *addr = token;
return htonl(1U << ((~fn_bit)&0x1F)) & addr[fn_bit>>5];
}
static __inline__ int addr_diff(void *token1, void *token2, int addrlen)
{
__u32 *a1 = token1;
__u32 *a2 = token2;
int i;
addrlen >>= 2;
for (i = 0; i < addrlen; i++) {
__u32 xb;
xb = a1[i] ^ a2[i];
if (xb) {
int j = 31;
xb = ntohl(xb);
while (test_bit(j, &xb) == 0)
j--;
return (i * 32 + 31 - j);
}
}
return addrlen<<5;
}
static __inline__ struct fib6_node * node_alloc(void)
{
struct fib6_node *fn;
if ((fn = kmalloc(sizeof(struct fib6_node), GFP_ATOMIC)) != NULL) {
memset(fn, 0, sizeof(struct fib6_node));
rt6_stats.fib_nodes++;
}
return fn;
}
static __inline__ void node_free(struct fib6_node * fn)
{
rt6_stats.fib_nodes--;
kfree(fn);
}
static __inline__ void rt6_release(struct rt6_info *rt)
{
if (atomic_dec_and_test(&rt->rt6i_ref))
dst_free(&rt->u.dst);
}
static struct fib6_node * fib6_add_1(struct fib6_node *root, void *addr,
int addrlen, int plen,
int offset)
{
struct fib6_node *fn, *in, *ln;
struct fib6_node *pn = NULL;
struct rt6key *key;
int	bit;
int	dir = 0;
__u32	sernum = fib6_new_sernum();
RT6_TRACE("fib6_add_1\n");
fn = root;
if (plen == 0)
return fn;
do {
key = (struct rt6key *)((u8 *)fn->leaf + offset);
if (plen < fn->fn_bit ||
!addr_match(&key->addr, addr, fn->fn_bit))
goto insert_above;
if (plen == fn->fn_bit) {
if ((fn->fn_flags & RTN_RTINFO) == 0) {
rt6_release(fn->leaf);
fn->leaf = NULL;
}
fn->fn_sernum = sernum;
return fn;
}
fn->fn_sernum = sernum;
dir = addr_bit_set(addr, fn->fn_bit);
pn = fn;
fn = dir ? fn->right: fn->left;
} while (fn);
ln = node_alloc();
if (ln == NULL)
return NULL;
ln->fn_bit = plen;
ln->parent = pn;
ln->fn_sernum = sernum;
if (dir)
pn->right = ln;
else
pn->left  = ln;
return ln;
insert_above:
pn = fn->parent;
bit = addr_diff(addr, &key->addr, addrlen);
if (plen > bit) {
in = node_alloc();
ln = node_alloc();
if (in == NULL || ln == NULL) {
if (in)
node_free(in);
if (ln)
node_free(ln);
return NULL;
}
in->fn_bit = bit;
in->parent = pn;
in->leaf = fn->leaf;
atomic_inc(&in->leaf->rt6i_ref);
in->fn_sernum = sernum;
if (dir)
pn->right = in;
else
pn->left  = in;
ln->fn_bit = plen;
ln->parent = in;
fn->parent = in;
ln->fn_sernum = sernum;
if (addr_bit_set(addr, bit)) {
in->right = ln;
in->left  = fn;
} else {
in->left  = ln;
in->right = fn;
}
} else {
ln = node_alloc();
if (ln == NULL)
return NULL;
ln->fn_bit = plen;
ln->parent = pn;
ln->fn_sernum = sernum;
if (dir)
pn->right = ln;
else
pn->left  = ln;
if (addr_bit_set(&key->addr, plen))
ln->right = fn;
else
ln->left  = fn;
fn->parent = ln;
}
return ln;
}
static int fib6_add_rt2node(struct fib6_node *fn, struct rt6_info *rt)
{
struct rt6_info *iter = NULL;
struct rt6_info **ins;
ins = &fn->leaf;
for (iter = fn->leaf; iter; iter=iter->u.next) {
if (iter->rt6i_metric == rt->rt6i_metric) {
if ((iter->rt6i_dev == rt->rt6i_dev) &&
(iter->rt6i_flowr == rt->rt6i_flowr) &&
(ipv6_addr_cmp(&iter->rt6i_gateway,
&rt->rt6i_gateway) == 0)) {
if (!(iter->rt6i_flags&RTF_EXPIRES))
return -EEXIST;
iter->rt6i_expires = rt->rt6i_expires;
if (!(rt->rt6i_flags&RTF_EXPIRES)) {
iter->rt6i_flags &= ~RTF_EXPIRES;
iter->rt6i_expires = 0;
}
return -EEXIST;
}
}
if (iter->rt6i_metric > rt->rt6i_metric)
break;
ins = &iter->u.next;
}
rt->u.next = iter;
*ins = rt;
rt->rt6i_node = fn;
atomic_inc(&rt->rt6i_ref);
#ifdef CONFIG_RTNETLINK
inet6_rt_notify(RTM_NEWROUTE, rt);
#endif
rt6_stats.fib_rt_entries++;
if ((fn->fn_flags & RTN_RTINFO) == 0) {
rt6_stats.fib_route_nodes++;
fn->fn_flags |= RTN_RTINFO;
}
return 0;
}
static __inline__ void fib6_start_gc(struct rt6_info *rt)
{
if (ip6_fib_timer.expires == 0 &&
(rt->rt6i_flags & (RTF_EXPIRES|RTF_CACHE))) {
del_timer(&ip6_fib_timer);
ip6_fib_timer.expires = jiffies + ip6_rt_gc_interval;
add_timer(&ip6_fib_timer);
}
}
int fib6_add(struct fib6_node *root, struct rt6_info *rt)
{
struct fib6_node *fn;
int err = -ENOMEM;
fn = fib6_add_1(root, &rt->rt6i_dst.addr, sizeof(struct in6_addr),
rt->rt6i_dst.plen, (u8*) &rt->rt6i_dst - (u8*) rt);
if (fn == NULL)
return -ENOMEM;
#ifdef CONFIG_IPV6_SUBTREES
if (rt->rt6i_src.plen) {
struct fib6_node *sn;
if (fn->subtree == NULL) {
struct fib6_node *sfn;
sfn = node_alloc();
if (sfn == NULL)
goto st_failure;
sfn->leaf = &ip6_null_entry;
atomic_inc(&ip6_null_entry.rt6i_ref);
sfn->fn_flags = RTN_ROOT;
sfn->fn_sernum = fib6_new_sernum();
sn = fib6_add_1(sfn, &rt->rt6i_src.addr,
sizeof(struct in6_addr), rt->rt6i_src.plen,
(u8*) &rt->rt6i_src - (u8*) rt);
if (sn == NULL) {
node_free(sfn);
goto st_failure;
}
sfn->parent = fn;
fn->subtree = sfn;
if (fn->leaf == NULL) {
fn->leaf = rt;
atomic_inc(&rt->rt6i_ref);
}
} else {
sn = fib6_add_1(fn->subtree, &rt->rt6i_src.addr,
sizeof(struct in6_addr), rt->rt6i_src.plen,
(u8*) &rt->rt6i_src - (u8*) rt);
if (sn == NULL)
goto st_failure;
}
fn = sn;
}
#endif
err = fib6_add_rt2node(fn, rt);
if (err == 0) {
fib6_start_gc(rt);
if (!(rt->rt6i_flags&RTF_CACHE))
fib6_prune_clones(fn, rt);
}
if (err)
dst_free(&rt->u.dst);
return err;
#ifdef CONFIG_IPV6_SUBTREES
st_failure:
if (fn && !(fn->fn_flags&RTN_RTINFO|RTN_ROOT))
fib_repair_tree(fn);
dst_free(&rt->u.dst);
return err;
#endif
}
struct lookup_args {
int		offset;
struct in6_addr	*addr;
};
static struct fib6_node * fib6_lookup_1(struct fib6_node *root,
struct lookup_args *args)
{
struct fib6_node *fn;
int dir;
fn = root;
for (;;) {
struct fib6_node *next;
dir = addr_bit_set(args->addr, fn->fn_bit);
next = dir ? fn->right : fn->left;
if (next) {
fn = next;
continue;
}
break;
}
while ((fn->fn_flags & RTN_ROOT) == 0) {
#ifdef CONFIG_IPV6_SUBTREES
if (fn->subtree) {
struct fib6_node *st;
struct lookup_args *narg;
narg = args + 1;
if (narg->addr) {
st = fib6_lookup_1(fn->subtree, narg);
if (!(st->fn_flags & RTN_ROOT))
{
return st;
}
}
}
#endif
if (fn->fn_flags & RTN_RTINFO) {
struct rt6key *key;
key = (struct rt6key *) ((u8 *) fn->leaf +
args->offset);
if (addr_match(&key->addr, args->addr, key->plen))
return fn;
}
fn = fn->parent;
}
return NULL;
}
struct fib6_node * fib6_lookup(struct fib6_node *root, struct in6_addr *daddr,
struct in6_addr *saddr)
{
struct lookup_args args[2];
struct rt6_info *rt = NULL;
struct fib6_node *fn;
args[0].offset = (u8*) &rt->rt6i_dst - (u8*) rt;
args[0].addr = daddr;
#ifdef CONFIG_IPV6_SUBTREES
args[1].offset = (u8*) &rt->rt6i_src - (u8*) rt;
args[1].addr = saddr;
#endif
fn = fib6_lookup_1(root, args);
if (fn == NULL)
fn = root;
return fn;
}
static struct fib6_node * fib6_locate_1(struct fib6_node *root,
struct in6_addr *addr,
int plen, int offset)
{
struct fib6_node *fn;
for (fn = root; fn ; ) {
struct rt6key *key = (struct rt6key *)((u8 *)fn->leaf + offset);
if (plen < fn->fn_bit ||
!addr_match(&key->addr, addr, fn->fn_bit))
return NULL;
if (plen == fn->fn_bit)
return fn;
if (addr_bit_set(addr, fn->fn_bit))
fn = fn->right;
else
fn = fn->left;
}
return NULL;
}
struct fib6_node * fib6_locate(struct fib6_node *root,
struct in6_addr *daddr, int dst_len,
struct in6_addr *saddr, int src_len)
{
struct rt6_info *rt = NULL;
struct fib6_node *fn;
fn = fib6_locate_1(root, daddr, dst_len,
(u8*) &rt->rt6i_dst - (u8*) rt);
#ifdef CONFIG_IPV6_SUBTREES
if (src_len) {
BUG_TRAP(saddr!=NULL);
if (fn != NULL)
fn = fn->subtree;
if (fn)
fn = fib6_locate_1(fn, saddr, src_len,
(u8*) &rt->rt6i_src - (u8*) rt);
}
#endif
if (fn && fn->fn_flags&RTN_RTINFO)
return fn;
return NULL;
}
static struct rt6_info * fib6_find_prefix(struct fib6_node *fn)
{
if (fn->fn_flags&RTN_ROOT)
return &ip6_null_entry;
while(fn) {
if(fn->left)
return fn->left->leaf;
if(fn->right)
return fn->right->leaf;
fn = SUBTREE(fn);
}
return NULL;
}
static void fib6_repair_tree(struct fib6_node *fn)
{
int children;
int nstate;
struct fib6_node *child, *pn;
struct fib6_walker_t *w;
int iter = 0;
for (;;) {
RT6_TRACE("fixing tree: plen=%d iter=%d\n", fn->fn_bit, iter);
iter++;
BUG_TRAP(!(fn->fn_flags&RTN_RTINFO));
BUG_TRAP(!(fn->fn_flags&RTN_TL_ROOT));
BUG_TRAP(fn->leaf==NULL);
children = 0;
child = NULL;
if (fn->right) child = fn->right, children |= 1;
if (fn->left) child = fn->left, children |= 2;
if (children == 3 || SUBTREE(fn)
#ifdef CONFIG_IPV6_SUBTREES
|| (children && fn->fn_flags&RTN_ROOT)
#endif
) {
fn->leaf = fib6_find_prefix(fn);
#if RT6_DEBUG >= 2
if (fn->leaf==NULL) {
BUG_TRAP(fn->leaf);
fn->leaf = &ip6_null_entry;
}
#endif
atomic_inc(&fn->leaf->rt6i_ref);
return;
}
pn = fn->parent;
#ifdef CONFIG_IPV6_SUBTREES
if (SUBTREE(pn) == fn) {
BUG_TRAP(fn->fn_flags&RTN_ROOT);
SUBTREE(pn) = NULL;
nstate = FWS_L;
} else {
BUG_TRAP(!(fn->fn_flags&RTN_ROOT));
#endif
if (pn->right == fn) pn->right = child;
else if (pn->left == fn) pn->left = child;
#if RT6_DEBUG >= 2
else BUG_TRAP(0);
#endif
if (child)
child->parent = pn;
nstate = FWS_R;
#ifdef CONFIG_IPV6_SUBTREES
}
#endif
FOR_WALKERS(w) {
if (child == NULL) {
if (w->root == fn) {
w->root = w->node = NULL;
RT6_TRACE("W %p adjusted by delroot 1\n", w);
} else if (w->node == fn) {
RT6_TRACE("W %p adjusted by delnode 1, s=%d/%d\n", w, w->state, nstate);
w->node = pn;
w->state = nstate;
}
} else {
if (w->root == fn) {
w->root = child;
RT6_TRACE("W %p adjusted by delroot 2\n", w);
}
if (w->node == fn) {
w->node = child;
if (children&2) {
RT6_TRACE("W %p adjusted by delnode 2, s=%d\n", w, w->state);
w->state = w->state>=FWS_R ? FWS_U : FWS_INIT;
} else {
RT6_TRACE("W %p adjusted by delnode 2, s=%d\n", w, w->state);
w->state = w->state>=FWS_C ? FWS_U : FWS_INIT;
}
}
}
}
node_free(fn);
if (pn->fn_flags&RTN_RTINFO || SUBTREE(pn))
return;
rt6_release(pn->leaf);
pn->leaf = NULL;
fn = pn;
}
}
static void fib6_del_route(struct fib6_node *fn, struct rt6_info **rtp)
{
struct fib6_walker_t *w;
struct rt6_info *rt = *rtp;
RT6_TRACE("fib6_del_route\n");
*rtp = rt->u.next;
rt->rt6i_node = NULL;
rt6_stats.fib_rt_entries--;
FOR_WALKERS(w) {
if (w->state == FWS_C && w->leaf == rt) {
RT6_TRACE("walker %p adjusted by delroute\n", w);
w->leaf = rt->u.next;
if (w->leaf == NULL)
w->state = FWS_U;
}
}
rt->u.next = NULL;
if (fn->leaf == NULL) {
fn->fn_flags &= ~RTN_RTINFO;
rt6_stats.fib_route_nodes--;
fib6_repair_tree(fn);
}
#ifdef CONFIG_RTNETLINK
inet6_rt_notify(RTM_DELROUTE, rt);
#endif
rt6_release(rt);
}
int fib6_del(struct rt6_info *rt)
{
struct fib6_node *fn = rt->rt6i_node;
struct rt6_info **rtp;
#if RT6_DEBUG >= 2
if (rt->u.dst.obsolete>0) {
BUG_TRAP(rt->u.dst.obsolete>0);
return -EFAULT;
}
#endif
if (fn == NULL || rt == &ip6_null_entry)
return -ENOENT;
BUG_TRAP(fn->fn_flags&RTN_RTINFO);
if (!(rt->rt6i_flags&RTF_CACHE))
fib6_prune_clones(fn, rt);
for (rtp = &fn->leaf; *rtp; rtp = &(*rtp)->u.next) {
if (*rtp == rt) {
fib6_del_route(fn, rtp);
return 0;
}
}
return -ENOENT;
}
int fib6_walk_continue(struct fib6_walker_t *w)
{
struct fib6_node *fn, *pn;
for (;;) {
fn = w->node;
if (fn == NULL)
return 0;
if (w->prune && fn != w->root &&
fn->fn_flags&RTN_RTINFO && w->state < FWS_C) {
w->state = FWS_C;
w->leaf = fn->leaf;
}
switch (w->state) {
#ifdef CONFIG_IPV6_SUBTREES
case FWS_S:
if (SUBTREE(fn)) {
w->node = SUBTREE(fn);
continue;
}
w->state = FWS_L;
#endif
case FWS_L:
if (fn->left) {
w->node = fn->left;
w->state = FWS_INIT;
continue;
}
w->state = FWS_R;
case FWS_R:
if (fn->right) {
w->node = fn->right;
w->state = FWS_INIT;
continue;
}
w->state = FWS_C;
w->leaf = fn->leaf;
case FWS_C:
if (w->leaf && fn->fn_flags&RTN_RTINFO) {
int err = w->func(w);
if (err)
return err;
continue;
}
w->state = FWS_U;
case FWS_U:
if (fn == w->root)
return 0;
pn = fn->parent;
w->node = pn;
#ifdef CONFIG_IPV6_SUBTREES
if (SUBTREE(pn) == fn) {
BUG_TRAP(fn->fn_flags&RTN_ROOT);
w->state = FWS_L;
continue;
}
#endif
if (pn->left == fn) {
w->state = FWS_R;
continue;
}
if (pn->right == fn) {
w->state = FWS_C;
w->leaf = w->node->leaf;
continue;
}
#if RT6_DEBUG >= 2
BUG_TRAP(0);
#endif
}
}
}
int fib6_walk(struct fib6_walker_t *w)
{
int res;
w->state = FWS_INIT;
w->node = w->root;
fib6_walker_link(w);
res = fib6_walk_continue(w);
if (res <= 0)
fib6_walker_unlink(w);
return res;
}
static int fib6_clean_node(struct fib6_walker_t *w)
{
int res;
struct rt6_info *rt;
struct fib6_cleaner_t *c = (struct fib6_cleaner_t*)w;
for (rt = w->leaf; rt; rt = rt->u.next) {
res = c->func(rt, c->arg);
if (res < 0) {
w->leaf = rt;
res = fib6_del(rt);
if (res) {
#if RT6_DEBUG >= 2
printk(KERN_DEBUG "fib6_clean_node: del failed: rt=%p@%p err=%d\n", rt, rt->rt6i_node, res);
#endif
continue;
}
return 0;
}
BUG_TRAP(res==0);
}
w->leaf = rt;
return 0;
}
void fib6_clean_tree(struct fib6_node *root,
int (*func)(struct rt6_info *, void *arg),
int prune, void *arg)
{
struct fib6_cleaner_t c;
c.w.root = root;
c.w.func = fib6_clean_node;
c.w.prune = prune;
c.func = func;
c.arg = arg;
start_bh_atomic();
fib6_walk(&c.w);
end_bh_atomic();
}
static int fib6_prune_clone(struct rt6_info *rt, void *arg)
{
if (rt->rt6i_flags & RTF_CACHE) {
RT6_TRACE("pruning clone %p\n", rt);
return -1;
}
return 0;
}
static void fib6_prune_clones(struct fib6_node *fn, struct rt6_info *rt)
{
fib6_clean_tree(fn, fib6_prune_clone, 1, rt);
}
static struct fib6_gc_args
{
int			timeout;
int			more;
} gc_args;
static int fib6_age(struct rt6_info *rt, void *arg)
{
unsigned long now = jiffies;
if (rt->rt6i_flags & RTF_CACHE) {
if (atomic_read(&rt->u.dst.use) == 0 &&
(long)(now - rt->u.dst.lastuse) >= gc_args.timeout) {
RT6_TRACE("aging clone %p\n", rt);
return -1;
}
gc_args.more++;
}
if (rt->rt6i_flags&RTF_EXPIRES && rt->rt6i_expires) {
if ((long)(now - rt->rt6i_expires) > 0) {
RT6_TRACE("expiring %p\n", rt);
return -1;
}
gc_args.more++;
}
return 0;
}
void fib6_run_gc(unsigned long dummy)
{
if (dummy != ~0UL)
gc_args.timeout = (int)dummy;
else
gc_args.timeout = ip6_rt_gc_interval;
gc_args.more = 0;
fib6_clean_tree(&ip6_routing_table, fib6_age, 0, NULL);
del_timer(&ip6_fib_timer);
ip6_fib_timer.expires = 0;
if (gc_args.more) {
ip6_fib_timer.expires = jiffies + ip6_rt_gc_interval;
add_timer(&ip6_fib_timer);
}
}
#ifdef MODULE
void fib6_gc_cleanup(void)
{
del_timer(&ip6_fib_timer);
}
#endif