#ifndef FUNC_FILES_H
#define FUNC_FILES_H
#include <acpifs.h>
#include "myacpi.h"
typedef int (*acpi_read_op_t) (struct acpi_table *t, void *data,
off_t offset, size_t *len);
#define DIR_TABLES_NAME	"tables"
error_t io_read_table (struct acpi_table *t, struct acpifs_dirent *e,
off_t offset, size_t *len, void *data);
error_t io_acpi_file (struct acpifs_dirent *e, off_t offset, size_t *len,
void *data);
#endif