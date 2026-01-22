#include "memory_.h"
#include <assert.h>
#include "gx.h"
#include "gscdefs.h"
#include "gserrors.h"
#include "gsstruct.h"
#include "gsutil.h"
#include "gzstate.h"
#include "gxdevice.h"
#include "gzht.h"
#include "gswts.h"
#include "gxdhtres.h"
#include "gsserial.h"
#include "gxdhtserial.h"
extern_gx_device_halftone_list();
typedef enum {
gx_ht_tf_none = 0,
gx_ht_tf_identity,
gx_ht_tf_complete
} gx_ht_tf_type_t;
typedef enum {
gx_ht_traditional,
gx_ht_wts
} gx_ht_order_type_t;
private int
gx_ht_write_tf(
const gx_transfer_map * pmap,
byte *                  data,
uint *                  psize )
{
int                     req_size = 1;
if ( pmap != 0 && pmap->proc != gs_identity_transfer)
req_size += sizeof(pmap->values);
if (req_size > *psize) {
*psize = req_size;
return gs_error_rangecheck;
}
if (req_size == 1)
*data = (byte)(pmap == 0 ? gx_ht_tf_none : gx_ht_tf_identity);
else {
*data++ = (byte)gx_ht_tf_complete;
memcpy(data, pmap->values, sizeof(pmap->values));
}
*psize = req_size;
return 0;
}
private int
gx_ht_read_tf(
gx_transfer_map **  ppmap,
const byte *        data,
uint                size,
gs_memory_t *       mem )
{
gx_ht_tf_type_t     tf_type;
gx_transfer_map *   pmap;
if (size == 0)
return_error(gs_error_rangecheck);
--size;
tf_type = (gx_ht_tf_type_t)*data++;
if (tf_type == gx_ht_tf_none) {
*ppmap = 0;
return 1;
}
rc_alloc_struct_1( pmap,
gx_transfer_map,
&st_transfer_map,
mem,
return_error(gs_error_VMerror),
"gx_ht_read_tf" );
pmap->id = gs_next_ids(mem, 1);
pmap->closure.proc = 0;
pmap->closure.data = 0;
if (tf_type == gx_ht_tf_identity) {
gx_set_identity_transfer(pmap);
return 1;
} else if (tf_type == gx_ht_tf_complete && size >= sizeof(pmap->values)) {
memcpy(pmap->values, data, sizeof(pmap->values));
pmap->proc = gs_mapped_transfer;
*ppmap = pmap;
return 1 + sizeof(pmap->values);
} else {
rc_decrement(pmap, "gx_ht_read_tf");
return_error(gs_error_rangecheck);
}
}
private int
gx_ht_write_component(
const gx_ht_order_component *   pcomp,
byte *                          data,
uint *                          psize )
{
const gx_ht_order *             porder = &pcomp->corder;
byte *                          data0 = data;
int                             code, levels_size, bits_size;
uint			    tmp_size = 0;
int                             req_size;
if (porder->wts != 0)
return_error(gs_error_unknownerror);
levels_size = porder->num_levels * sizeof(porder->levels[0]);
bits_size = porder->num_bits * porder->procs->bit_data_elt_size;
req_size =   1
+ enc_u_sizew(porder->width)
+ enc_u_sizew(porder->height)
+ enc_u_sizew(porder->shift)
+ enc_u_sizew(porder->num_levels)
+ enc_u_sizew(porder->num_bits)
+ 1
+ levels_size
+ bits_size;
code = gx_ht_write_tf(porder->transfer, data, &tmp_size);
if (code < 0 && code != gs_error_rangecheck)
return code;
req_size += tmp_size;
if (req_size > *psize) {
*psize = req_size;
return gs_error_rangecheck;
}
*data++ = (byte)gx_ht_traditional;
enc_u_putw(porder->width, data);
enc_u_putw(porder->height, data);
enc_u_putw(porder->shift, data);
enc_u_putw(porder->num_levels, data);
enc_u_putw(porder->num_bits, data);
*data++ = porder->procs - ht_order_procs_table;
memcpy(data, porder->levels, levels_size);
data += levels_size;
memcpy(data, porder->bit_data, bits_size);
data += bits_size;
tmp_size = *psize - (data - data0);
if ((code = gx_ht_write_tf(porder->transfer, data, &tmp_size)) == 0)
*psize = tmp_size + (data - data0);
return code;
}
private int
gx_ht_read_component(
gx_ht_order_component * pcomp,
const byte *            data,
uint                    size,
gs_memory_t *           mem )
{
gx_ht_order             new_order;
const byte *            data0 = data;
const byte *            data_lim = data + size;
gx_ht_order_type_t      order_type;
int                     i, code, levels_size, bits_size;
const gx_dht_proc *     phtrp = gx_device_halftone_list;
if (size == 0)
return_error(gs_error_rangecheck);
--size;
order_type = (gx_ht_order_type_t)*data++;
if (order_type != gx_ht_traditional)
return_error(gs_error_unknownerror);
if (size < 7)
return_error(gs_error_rangecheck);
enc_u_getw(new_order.width, data);
enc_u_getw(new_order.height, data);
enc_u_getw(new_order.shift, data);
enc_u_getw(new_order.num_levels, data);
enc_u_getw(new_order.num_bits, data);
if (data >= data_lim)
return_error(gs_error_rangecheck);
new_order.procs = &ht_order_procs_table[*data++];
levels_size = new_order.num_levels * sizeof(new_order.levels[0]);
bits_size = new_order.num_bits * new_order.procs->bit_data_elt_size;
if (data + bits_size + levels_size + 1 > data_lim)
return_error(gs_error_rangecheck);
code = gx_ht_alloc_ht_order( &new_order,
new_order.width,
new_order.height,
new_order.num_levels,
new_order.num_bits,
new_order.shift,
new_order.procs,
mem );
if (code < 0)
return code;
memset(&new_order.params, 0, sizeof(new_order.params));
memset(&new_order.screen_params, 0, sizeof(new_order.screen_params));
memcpy(new_order.levels, data, levels_size);
data += levels_size;
memcpy(new_order.bit_data, data, bits_size);
data += bits_size;
code = gx_ht_read_tf(&new_order.transfer, data, data_lim - data, mem);
if (code < 0) {
gx_ht_order_release(&new_order, mem, false);
return code;
}
data += code;
for (i = 0; phtrp[i] != 0; i++) {
const gx_device_halftone_resource_t *const *    pphtr = phtrp[i]();
const gx_device_halftone_resource_t *           phtr;
while ((phtr = *pphtr++) != 0) {
if ( phtr->num_levels * sizeof(phtr->levels[0]) >= levels_size &&
phtr->Width * phtr->Height * phtr->elt_size >= bits_size  &&
memcmp(phtr->levels, new_order.levels, levels_size) == 0  &&
memcmp(phtr->bit_data, new_order.bit_data, bits_size) == 0  ) {
gs_free_object(mem, new_order.bit_data, "gx_ht_read_component");
new_order.bit_data = (void *)phtr->bit_data;
gs_free_object(mem, new_order.levels, "gx_ht_read_component");
new_order.levels = (uint *)phtr->levels;
goto done;
}
}
}
done:
pcomp->corder = new_order;
pcomp->cname = 0;
return data - data0;
}
int
gx_ht_write(
const gx_device_halftone *  pdht,
const gx_device *           dev,
byte *                      data,
uint *                      psize )
{
int                         num_dev_comps = pdht->num_dev_comp;
int                         i, code;
uint                        req_size = 2, used_size = 2;
assert(pdht != 0 && pdht->components != 0);
for ( i = 0, code = gs_error_rangecheck;
i < num_dev_comps && code == gs_error_rangecheck;
i++) {
uint     tmp_size = 0;
assert(i == pdht->components[i].comp_number);
code = gx_ht_write_component( &pdht->components[i],
data,
&tmp_size );
req_size += tmp_size;
}
if (code < 0 && code != gs_error_rangecheck)
return code;
else if (*psize < req_size) {
*psize = req_size;
return 0;
}
req_size = *psize;
*data++ = (byte)pdht->type;
*data++ = (byte)num_dev_comps;
for (i = 0, code = 0; i < num_dev_comps && code == 0; i++) {
uint    tmp_size = req_size - used_size;
code = gx_ht_write_component( &pdht->components[i],
data,
&tmp_size );
used_size += tmp_size;
data += tmp_size;
}
if (code < 0) {
if (code == gs_error_rangecheck)
code = gs_error_unknownerror;
return code;
}
*psize = used_size;
return 0;
}
int
gx_ht_read_and_install(
gs_imager_state *       pis,
const gx_device *       dev,
const byte *            data,
uint                    size,
gs_memory_t *           mem )
{
gx_ht_order_component   components[GX_DEVICE_COLOR_MAX_COMPONENTS];
const byte *            data0 = data;
gx_device_halftone      dht;
int                     num_dev_comps;
int                     i, code;
memset(&dht.order, 0, sizeof(dht.order));
memset(&dht.rc, 0, sizeof(dht.rc));
dht.id = gs_no_id;
dht.components = components;
dht.lcm_width = 1;
dht.lcm_height = 1;
memset(components, 0, sizeof(components));
if (size-- < 1)
return_error(gs_error_rangecheck);
dht.type = (gs_halftone_type)(*data++);
num_dev_comps = dht.num_dev_comp = dht.num_comp = *data++;
for (i = 0, code = 0; i < num_dev_comps && code >= 0; i++) {
components[i].comp_number = i;
code = gx_ht_read_component(&components[i], data, size, mem);
if (code >= 0) {
size -= code;
data += code;
}
}
if (code >= 0)
code = gx_imager_dev_ht_install(pis, &dht, dht.type, dev);
if (code < 0) {
for (i = 0; i < num_dev_comps; i++)
gx_ht_order_release(&components[i].corder, mem, false);
}
return code < 0 ? code : data - data0;
}