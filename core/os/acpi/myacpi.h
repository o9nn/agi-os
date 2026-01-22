#ifndef MYACPI_H
#define MYACPI_H
#include <stdlib.h>
#include <inttypes.h>
#define ESCD 0xe0000U
#define RSDP_MAGIC (const unsigned char *)"RSD PTR "
#define ESCD_SIZE 0x20000U
struct rsdp_descr
{
uint8_t magic[8];
uint8_t checksum;
uint8_t oem_id[6];
uint8_t revision;
uint32_t rsdt_addr;
} __attribute__ ((packed));
struct rsdp_descr2
{
struct rsdp_descr v1;
uint32_t length;
uint64_t xsdt_addr;
uint8_t checksum;
uint8_t reserved[3];
} __attribute__ ((packed));
struct acpi_header
{
uint8_t signature[4];
uint32_t length;
uint8_t revision;
uint8_t checksum;
uint8_t oem_id[6];
uint8_t oem_table_id[8];
uint32_t oem_revision;
uint32_t creator_id;
uint32_t creator_revision;
} __attribute__ ((packed));
struct acpi_table
{
struct acpi_header h;
void *data;
size_t datalen;
} __attribute__ ((packed));
int acpi_get_num_tables(size_t *num_tables);
int acpi_get_tables(struct acpi_table **tables);
#endif