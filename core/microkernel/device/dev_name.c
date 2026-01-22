#include <kern/printf.h>
#include <string.h>
#include <device/device_types.h>
#include <device/dev_hdr.h>
#include <device/conf.h>
int nulldev_reset(dev_t dev)
{
return (D_SUCCESS);
}
int nulldev_open(dev_t dev, int flags, io_req_t ior)
{
return (D_SUCCESS);
}
void nulldev_close(dev_t dev, int flags)
{
}
int nulldev_read(dev_t dev, io_req_t ior)
{
return (D_SUCCESS);
}
int nulldev_write(dev_t dev, io_req_t ior)
{
return (D_SUCCESS);
}
io_return_t nulldev_getstat(dev_t dev, dev_flavor_t flavor, dev_status_t data, mach_msg_type_number_t *count)
{
return (D_SUCCESS);
}
io_return_t nulldev_setstat(dev_t dev, dev_flavor_t flavor, dev_status_t data, mach_msg_type_number_t count)
{
return (D_SUCCESS);
}
int nulldev_portdeath(dev_t dev, mach_port_t port)
{
return (D_SUCCESS);
}
int nodev_async_in(dev_t dev, const ipc_port_t port, int x, filter_t* filter, unsigned int j)
{
return (D_INVALID_OPERATION);
}
int nodev_info(dev_t dev, int a, int* b)
{
return (D_INVALID_OPERATION);
}
vm_offset_t
nomap(dev_t dev, vm_offset_t off, int prot)
{
return -1;
}
boolean_t __attribute__ ((pure))
name_equal(const char *src,
int len,
const char *target)
{
while (--len >= 0)
if (*src++ != *target++)
return FALSE;
return *target == 0;
}
boolean_t dev_name_lookup(
const char *name,
dev_ops_t *ops,
int *unit)
{
const char *cp = name;
int len;
int j = 0;
int c;
dev_ops_t dev;
boolean_t found;
int slice_num = 0;
while ((c = *cp) != '\0' &&
!(c >= '0' && c <= '9'))
cp++;
len = cp - name;
if (c != '\0') {
while ((c = *cp) != '\0' &&
c >= '0' && c <= '9') {
j = j * 10 + (c - '0');
cp++;
}
}
found = FALSE;
dev_search(dev) {
if (name_equal(name, len, dev->d_name)) {
found = TRUE;
break;
}
}
if (!found) {
dev_indirect_t di;
dev_indirect_search(di) {
if (name_equal(name, len, di->d_name)) {
*ops = di->d_ops;
*unit = di->d_unit;
return (TRUE);
}
}
return (FALSE);
}
*ops = dev;
*unit = j;
j = dev->d_subdev;
if (j > 0) {
*unit *= j;
if (c == 's') {
cp++;
while ((c = *cp) != '\0' &&
c >= '0' && c <= '9') {
slice_num = slice_num * 10 + (c - '0');
cp++;
}
}
*unit += (slice_num << 4);
if (c >= 'a' && c < 'a' + j) {
*unit += (c - 'a' +1);
}
}
return (TRUE);
}
void
dev_set_indirection(const char *name, dev_ops_t ops, int unit)
{
dev_indirect_t di;
dev_indirect_search(di) {
if (!strcmp(di->d_name, name)) {
di->d_ops = ops;
di->d_unit = unit;
break;
}
}
}