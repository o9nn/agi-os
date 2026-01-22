#include <func_files.h>
#include <assert.h>
error_t
io_acpi_table (struct acpi_table *t, off_t offset, size_t *len, void *data)
{
error_t err;
size_t datalen;
assert_backtrace (t != 0);
datalen = t->datalen;
if (offset > datalen)
return EINVAL;
if ((offset + *len) > datalen)
*len = datalen - offset;
memcpy (data, t->data + offset, *len);
return err;
}
error_t
io_acpi_file (struct acpifs_dirent *e, off_t offset, size_t *len,
void *data)
{
size_t datalen;
struct acpi_table *table;
assert_backtrace (e->acpitable != 0);
table = e->acpitable;
datalen = table->datalen;
if (offset > datalen)
return EINVAL;
if ((offset + *len) > datalen)
*len = datalen - offset;
memcpy (data, table->data + offset, *len);
return 0;
}