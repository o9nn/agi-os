#include <sys/mman.h>
#include <sys/io.h>
#include <fcntl.h>
#include <inttypes.h>
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>
#include "myacpi.h"
#define __KERNEL__
#include <acpi/acpi.h>
int
acpi_get_num_tables(size_t *num_tables)
{
void *virt_addr;
bool found = false;
struct rsdp_descr2 rsdp = { 0 };
uintptr_t sdt_base = (uintptr_t)0;
bool is_64bit = false;
unsigned char *buf;
struct acpi_header *root_sdt;
struct acpi_header *next;
virt_addr = acpi_os_map_memory(ESCD, ESCD_SIZE);
if (virt_addr == MAP_FAILED)
return errno;
buf = (unsigned char *)virt_addr;
found = false;
for (int i = 0; i < ESCD_SIZE; i += 16)
{
if (!memcmp(&buf[i], RSDP_MAGIC, 8)) {
rsdp = *((struct rsdp_descr2 *)(&buf[i]));
found = true;
break;
}
}
if (!found) {
acpi_os_unmap_memory(virt_addr, ESCD_SIZE);
return ENODEV;
}
if (rsdp.v1.revision == 0) {
sdt_base = rsdp.v1.rsdt_addr;
is_64bit = false;
} else if (rsdp.v1.revision == 2) {
sdt_base = rsdp.xsdt_addr;
is_64bit = true;
} else {
acpi_os_unmap_memory(virt_addr, ESCD_SIZE);
return ENODEV;
}
acpi_os_unmap_memory(virt_addr, ESCD_SIZE);
root_sdt = acpi_os_map_memory(sdt_base, ESCD_SIZE);
if (root_sdt == MAP_FAILED)
return errno;
uint32_t ntables;
uint8_t sz_ptr;
sz_ptr = is_64bit ? 8 : 4;
ntables = (root_sdt->length - sizeof(*root_sdt)) / sz_ptr;
void *acpi_ptr = (void*)root_sdt + sizeof(*root_sdt);
*num_tables = 0;
for (int i = 0; i < ntables; i++)
{
if (is_64bit) {
uint64_t acpi_ptr64;
memcpy(&acpi_ptr64, acpi_ptr + i*sz_ptr, sizeof(acpi_ptr64));
next = acpi_os_map_memory(acpi_ptr64, ESCD_SIZE);
} else {
uint32_t acpi_ptr32;
memcpy(&acpi_ptr32, acpi_ptr + i*sz_ptr, sizeof(acpi_ptr32));
next = acpi_os_map_memory(acpi_ptr32, ESCD_SIZE);
}
if (next == MAP_FAILED)
return errno;
if (next->signature[0] == '\0' || next->length == 0) {
acpi_os_unmap_memory(next, ESCD_SIZE);
continue;
}
*num_tables += 1;
acpi_os_unmap_memory(next, ESCD_SIZE);
}
acpi_os_unmap_memory(root_sdt, ESCD_SIZE);
return 0;
}
int
acpi_get_tables(struct acpi_table **tables)
{
int err;
void *virt_addr;
bool found = false;
struct rsdp_descr2 rsdp = { 0 };
uintptr_t sdt_base = (uintptr_t)0;
bool is_64bit = false;
unsigned char *buf;
struct acpi_header *root_sdt;
struct acpi_header *next;
size_t ntables_actual;
int cur_tab = 0;
err = acpi_get_num_tables(&ntables_actual);
if (err)
return err;
*tables = malloc(ntables_actual * sizeof(**tables));
if (!*tables)
return ENOMEM;
virt_addr = acpi_os_map_memory(ESCD, ESCD_SIZE);
if (virt_addr == MAP_FAILED)
return errno;
buf = (unsigned char *)virt_addr;
found = false;
for (int i = 0; i < ESCD_SIZE; i += 16)
{
if (!memcmp(&buf[i], RSDP_MAGIC, 8)) {
rsdp = *((struct rsdp_descr2 *)(&buf[i]));
found = true;
break;
}
}
if (!found) {
acpi_os_unmap_memory(virt_addr, ESCD_SIZE);
return ENODEV;
}
if (rsdp.v1.revision == 0) {
sdt_base = rsdp.v1.rsdt_addr;
is_64bit = false;
} else if (rsdp.v1.revision == 2) {
sdt_base = rsdp.xsdt_addr;
is_64bit = true;
} else {
acpi_os_unmap_memory(virt_addr, ESCD_SIZE);
return ENODEV;
}
acpi_os_unmap_memory(virt_addr, ESCD_SIZE);
root_sdt = acpi_os_map_memory(sdt_base, ESCD_SIZE);
if (root_sdt == MAP_FAILED)
return errno;
uint32_t ntables;
uint8_t sz_ptr;
sz_ptr = is_64bit ? 8 : 4;
ntables = (root_sdt->length - sizeof(*root_sdt)) / sz_ptr;
void *acpi_ptr = (void*)root_sdt + sizeof(*root_sdt);
for (int i = 0; i < ntables; i++)
{
if (is_64bit) {
uint64_t acpi_ptr64;
memcpy(&acpi_ptr64, acpi_ptr + i*sz_ptr, sizeof(acpi_ptr64));
next = acpi_os_map_memory(acpi_ptr64, ESCD_SIZE);
} else {
uint32_t acpi_ptr32;
memcpy(&acpi_ptr32, acpi_ptr + i*sz_ptr, sizeof(acpi_ptr32));
next = acpi_os_map_memory(acpi_ptr32, ESCD_SIZE);
}
if (next == MAP_FAILED)
return errno;
if (next->signature[0] == '\0' || next->length == 0) {
acpi_os_unmap_memory(next, ESCD_SIZE);
continue;
}
uint32_t datalen = next->length - sizeof(*next);
void *data = (void *)((uintptr_t)next + sizeof(*next));
struct acpi_table *t = *tables + cur_tab;
memcpy(&t->h, next, sizeof(*next));
t->datalen = 0;
t->data = malloc(datalen);
if (!t->data) {
acpi_os_unmap_memory(next, ESCD_SIZE);
acpi_os_unmap_memory(root_sdt, ESCD_SIZE);
return ENOMEM;
}
t->datalen = datalen;
memcpy(t->data, data, datalen);
cur_tab++;
acpi_os_unmap_memory(next, ESCD_SIZE);
}
acpi_os_unmap_memory(root_sdt, ESCD_SIZE);
return 0;
}