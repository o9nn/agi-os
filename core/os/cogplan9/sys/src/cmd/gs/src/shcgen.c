#include "memory_.h"
#include "stdio_.h"
#include <stdlib.h>
#include "gdebug.h"
#include "gserror.h"
#include "gserrors.h"
#include "gsmemory.h"
#include "scommon.h"
#include "shc.h"
#include "shcgen.h"
typedef struct count_node_s count_node;
struct count_node_s {
long freq;
uint value;
uint code_length;
count_node *next;
count_node *left;
count_node *right;
};
#ifdef DEBUG
# define debug_print_nodes(nodes, n, tag, lengths)\
if ( gs_debug_c('W') ) print_nodes_proc(nodes, n, tag, lengths);
private void
print_nodes_proc(const count_node * nodes, int n, const char *tag, int lengths)
{
int i;
dlprintf1("[w]---------------- %s ----------------\n", tag);
for (i = 0; i < n; ++i)
dlprintf7("[w]node %d: f=%ld v=%d len=%d N=%d L=%d R=%d\n",
i, nodes[i].freq, nodes[i].value, nodes[i].code_length,
(nodes[i].next == 0 ? -1 : (int)(nodes[i].next - nodes)),
(nodes[i].left == 0 ? -1 : (int)(nodes[i].left - nodes)),
(nodes[i].right == 0 ? -1 : (int)(nodes[i].right - nodes)));
for (i = lengths; i > 0;) {
int j = i;
int len = nodes[--j].code_length;
while (j > 0 && nodes[j - 1].code_length == len)
--j;
dlprintf2("[w]%d codes of length %d\n", i - j, len);
i = j;
}
}
#else
# define debug_print_nodes(nodes, n, tag, lengths) DO_NOTHING
#endif
#define pn1 ((const count_node *)p1)
#define pn2 ((const count_node *)p2)
private int
compare_freqs(const void *p1, const void *p2)
{
long diff = pn2->freq - pn1->freq;
return (diff < 0 ? -1 : diff > 0 ? 1 : 0);
}
private int
compare_code_lengths(const void *p1, const void *p2)
{
int diff = pn1->code_length - pn2->code_length;
return (diff < 0 ? -1 : diff > 0 ? 1 : compare_freqs(p1, p2));
}
private int
compare_values(const void *p1, const void *p2)
{
return (pn1->value < pn2->value ? -1 :
pn1->value > pn2->value ? 1 : 0);
}
#undef pn1
#undef pn2
private void
hc_limit_code_lengths(count_node * nodes, uint num_values, int max_length)
{
int needed;
count_node *longest = nodes + num_values;
{
int length = longest[-1].code_length;
int next_length;
int avail = 0;
while ((next_length = longest[-1].code_length) > max_length) {
avail >>= length - next_length;
length = next_length;
(--longest)->code_length = max_length;
++avail;
}
needed = (nodes + num_values - longest) -
(avail >>= (length - max_length));
if_debug2('W', "[w]avail=%d, needed=%d\n",
avail, needed);
}
while (longest[-1].code_length == max_length)
--longest;
for (; needed > 0; --needed) {
int M1 = ++(longest[-1].code_length);
switch (max_length - M1) {
case 0:
--longest;
break;
case 1:
longest++->code_length = M1;
break;
default:
longest->code_length = M1 + 1;
longest[1].code_length = M1 + 1;
longest[2].code_length--;
longest += 3;
}
}
}
int
hc_compute(hc_definition * def, const long *freqs, gs_memory_t * mem)
{
uint num_values = def->num_values;
count_node *nodes =
(count_node *) gs_alloc_byte_array(mem, num_values * 2 - 1,
sizeof(count_node), "hc_compute");
int i;
count_node *lowest;
count_node *comb;
if (nodes == 0)
return_error(gs_error_VMerror);
for (i = 0; i < num_values; ++i)
nodes[i].freq = freqs[i], nodes[i].value = i;
qsort(nodes, num_values, sizeof(count_node), compare_freqs);
for (i = 0; i < num_values; ++i)
nodes[i].next = &nodes[i - 1],
nodes[i].code_length = 0,
nodes[i].left = nodes[i].right = 0;
nodes[0].next = 0;
debug_print_nodes(nodes, num_values, "after sort", 0);
for (lowest = &nodes[num_values - 1], comb = &nodes[num_values];;
++comb
) {
count_node *pn1 = lowest;
count_node *pn2 = pn1->next;
long freq = pn1->freq + pn2->freq;
lowest = pn2->next;
comb->freq = freq;
if (pn1->code_length <= pn2->code_length)
comb->left = pn1, comb->right = pn2,
comb->code_length = pn2->code_length + 1;
else
comb->left = pn2, comb->right = pn1,
comb->code_length = pn1->code_length + 1;
if (lowest == 0)
break;
if (freq < lowest->freq)
comb->next = lowest, lowest = comb;
else {
count_node *here = lowest;
while (here->next != 0 && freq >= here->next->freq)
here = here->next;
comb->next = here->next;
here->next = comb;
}
}
comb++->code_length = 0;
while (comb > nodes + num_values) {
--comb;
comb->left->code_length = comb->right->code_length =
comb->code_length + 1;
}
debug_print_nodes(nodes, num_values * 2 - 1, "after combine", 0);
qsort(nodes, num_values, sizeof(count_node), compare_code_lengths);
debug_print_nodes(nodes, num_values, "after re-sort", num_values);
hc_limit_code_lengths(nodes, num_values, def->num_counts);
debug_print_nodes(nodes, num_values, "after limit", num_values);
for (i = num_values; i > 0;) {
int j = i;
int len = nodes[--j].code_length;
while (j > 0 && nodes[j - 1].code_length == len)
--j;
qsort(&nodes[j], i - j, sizeof(count_node), compare_values);
i = j;
}
memset(def->counts, 0, sizeof(*def->counts) * (def->num_counts + 1));
for (i = 0; i < num_values; ++i) {
def->values[i] = nodes[i].value;
def->counts[nodes[i].code_length]++;
}
gs_free_object(mem, nodes, "hc_compute");
return 0;
}
int
hc_bytes_from_definition(byte * dbytes, const hc_definition * def)
{
int i, j;
byte *bp = dbytes;
const byte *lp = dbytes;
const byte *end = dbytes + def->num_values;
const ushort *values = def->values;
for (i = 1; i <= def->num_counts; i++)
for (j = 0; j < def->counts[i]; j++)
bp[*values++] = i;
while (lp < end) {
const byte *vp;
byte len = *lp;
for (vp = lp + 1; vp < end && vp < lp + 16 && *vp == len;)
vp++;
*bp++ = ((vp - lp - 1) << 4) + (len - 1);
lp = vp;
}
return bp - dbytes;
}
void
hc_sizes_from_bytes(hc_definition * def, const byte * dbytes, int num_bytes)
{
uint num_counts = 0, num_values = 0;
int i;
for (i = 0; i < num_bytes; i++) {
int n = (dbytes[i] >> 4) + 1;
int l = (dbytes[i] & 15) + 1;
if (l > num_counts)
num_counts = l;
num_values += n;
}
def->num_counts = num_counts;
def->num_values = num_values;
}
void
hc_definition_from_bytes(hc_definition * def, const byte * dbytes)
{
int v, i;
ushort counts[max_hc_length + 1];
memset(counts, 0, sizeof(counts[0]) * (def->num_counts + 1));
for (i = 0, v = 0; v < def->num_values; i++) {
int n = (dbytes[i] >> 4) + 1;
int l = (dbytes[i] & 15) + 1;
counts[l] += n;
v += n;
}
memcpy(def->counts, counts, sizeof(counts[0]) * (def->num_counts + 1));
for (i = 1, v = 0; i <= def->num_counts; i++) {
uint prev = counts[i];
counts[i] = v;
v += prev;
}
for (i = 0, v = 0; v < def->num_values; i++) {
int n = (dbytes[i] >> 4) + 1;
int l = (dbytes[i] & 15) + 1;
int j;
for (j = 0; j < n; n++)
def->values[counts[l]++] = v++;
}
}
void
hc_make_encoding(hce_code * encode, const hc_definition * def)
{
uint next = 0;
const ushort *pvalue = def->values;
uint i, k;
for (i = 1; i <= def->num_counts; i++) {
for (k = 0; k < def->counts[i]; k++, pvalue++, next++) {
hce_code *pce = encode + *pvalue;
pce->code = next;
pce->code_length = i;
}
next <<= 1;
}
}
uint
hc_sizeof_decoding(const hc_definition * def, int initial_bits)
{
uint size = 1 << initial_bits;
uint carry = 0, mask = (uint) ~ 1;
uint i;
for (i = initial_bits + 1; i <= def->num_counts;
i++, carry <<= 1, mask <<= 1
) {
carry += def->counts[i];
size += carry & mask;
carry &= ~mask;
}
return size;
}
void
hc_make_decoding(hcd_code * decode, const hc_definition * def,
int initial_bits)
{
{
hcd_code *pcd = decode;
const ushort *pvalue = def->values;
uint i, k, d;
for (i = 0; i <= initial_bits; i++) {
for (k = 0; k < def->counts[i]; k++, pvalue++) {
for (d = 1 << (initial_bits - i); d > 0;
d--, pcd++
)
pcd->value = *pvalue,
pcd->code_length = i;
}
}
}
{
uint dsize = hc_sizeof_decoding(def, initial_bits);
hcd_code *pcd = decode + (1 << initial_bits);
hcd_code *pcd2 = decode + dsize;
const ushort *pvalue = def->values + def->num_values;
uint entries_left = 0, slots_left = 0, mult_shift = 0;
uint i = def->num_counts + 1, j;
for (;;) {
if (slots_left == 0) {
if (entries_left != 0) {
slots_left = 1 << (i - initial_bits);
mult_shift = 0;
continue;
}
if (--i <= initial_bits)
break;
entries_left = def->counts[i];
continue;
}
if (entries_left == 0) {
entries_left = def->counts[--i];
mult_shift++;
continue;
}
--entries_left, --pvalue;
for (j = 1 << mult_shift; j > 0; j--) {
--pcd2;
pcd2->value = *pvalue;
pcd2->code_length = i - initial_bits;
}
if ((slots_left -= 1 << mult_shift) == 0) {
--pcd;
pcd->value = pcd2 - decode;
pcd->code_length = i + mult_shift;
}
}
}
}