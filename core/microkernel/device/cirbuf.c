#include <string.h>
#include <device/cirbuf.h>
#include <kern/debug.h>
#include <kern/kalloc.h>
#if DEBUG
#include <mach/boolean.h>
boolean_t cb_check_enable = FALSE;
#define CB_CHECK(cb) if (cb_check_enable) cb_check(cb)
void
cb_check(struct cirbuf *cb)
{
if (!(cb->c_cf >= cb->c_start && cb->c_cf < cb->c_end))
panic("cf %p out of range [%p..%p)",
cb->c_cf, cb->c_start, cb->c_end);
if (!(cb->c_cl >= cb->c_start && cb->c_cl < cb->c_end))
panic("cl %p out of range [%p..%p)",
cb->c_cl, cb->c_start, cb->c_end);
if (cb->c_cf <= cb->c_cl) {
if (!(cb->c_cc == cb->c_cl - cb->c_cf))
panic("cc %x should be %x",
cb->c_cc,
cb->c_cl - cb->c_cf);
}
else {
if (!(cb->c_cc == cb->c_end - cb->c_cf
+ cb->c_cl - cb->c_start))
panic("cc %x should be %x",
cb->c_cc,
cb->c_end - cb->c_cf +
cb->c_cl - cb->c_start);
}
}
#else
#define CB_CHECK(cb)
#endif
int putc(
int c,
struct cirbuf *cb)
{
char *ow, *nw;
ow = cb->c_cl;
nw = ow+1;
if (nw == cb->c_end)
nw = cb->c_start;
if (nw == cb->c_cf)
return 1;
*ow = c;
cb->c_cl = nw;
cb->c_cc++;
CB_CHECK(cb);
return 0;
}
int getc(struct cirbuf *cb)
{
unsigned char *nr;
int c;
nr = (unsigned char *)cb->c_cf;
if (nr == (unsigned char *)cb->c_cl) {
CB_CHECK(cb);
return -1;
}
c = *nr;
nr++;
if (nr == (unsigned char *)cb->c_end)
nr = (unsigned char *)cb->c_start;
cb->c_cf = (char *)nr;
cb->c_cc--;
CB_CHECK(cb);
return c;
}
int
q_to_b( struct cirbuf *cb,
char *cp,
int count)
{
char * const ocp = cp;
int i;
while (count != 0) {
if (cb->c_cl == cb->c_cf)
break;
if (cb->c_cl < cb->c_cf)
i = cb->c_end - cb->c_cf;
else
i = cb->c_cl - cb->c_cf;
if (i > count)
i = count;
memcpy(cp, cb->c_cf, i);
cp += i;
count -= i;
cb->c_cf += i;
cb->c_cc -= i;
if (cb->c_cf == cb->c_end)
cb->c_cf = cb->c_start;
CB_CHECK(cb);
}
CB_CHECK(cb);
return cp - ocp;
}
int
b_to_q( char *cp,
int count,
struct cirbuf *cb)
{
int i;
char *lim;
while (count != 0) {
lim = cb->c_cf - 1;
if (lim < cb->c_start)
lim = cb->c_end - 1;
if (cb->c_cl == lim)
break;
if (cb->c_cl < lim)
i = lim - cb->c_cl;
else
i = cb->c_end - cb->c_cl;
if (i > count)
i = count;
memcpy(cb->c_cl, cp, i);
cp += i;
count -= i;
cb->c_cc += i;
cb->c_cl += i;
if (cb->c_cl == cb->c_end)
cb->c_cl = cb->c_start;
CB_CHECK(cb);
}
CB_CHECK(cb);
return count;
}
void
ndflush(struct cirbuf *cb,
int count)
{
int i;
while (count != 0) {
if (cb->c_cl == cb->c_cf)
break;
if (cb->c_cl < cb->c_cf)
i = cb->c_end - cb->c_cf;
else
i = cb->c_cl - cb->c_cf;
if (i > count)
i = count;
count -= i;
cb->c_cf += i;
cb->c_cc -= i;
if (cb->c_cf == cb->c_end)
cb->c_cf = cb->c_start;
CB_CHECK(cb);
}
CB_CHECK(cb);
}
void cb_clear(struct cirbuf *cb)
{
cb->c_cf = cb->c_start;
cb->c_cl = cb->c_start;
cb->c_cc = 0;
}
void
cb_alloc(
struct cirbuf *cb,
vm_size_t buf_size)
{
char *buf;
buf = (char *)kalloc(buf_size);
cb->c_start = buf;
cb->c_end = buf + buf_size;
cb->c_cf = buf;
cb->c_cl = buf;
cb->c_cc = 0;
cb->c_hog = buf_size - 1;
CB_CHECK(cb);
}
void
cb_free(struct cirbuf *cb)
{
vm_size_t size;
size = cb->c_end - cb->c_start;
kfree((vm_offset_t)cb->c_start, size);
}