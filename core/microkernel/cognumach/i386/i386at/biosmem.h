#ifndef _X86_BIOSMEM_H
#define _X86_BIOSMEM_H
#include <mach/machine/vm_types.h>
#include <mach/machine/multiboot.h>
#define BIOSMEM_EBDA_PTR 0x40e
#define BIOSMEM_BASE        0x010000
#define BIOSMEM_BASE_END    0x0a0000
#define BIOSMEM_EXT_ROM     0x0e0000
#define BIOSMEM_ROM         0x0f0000
#define BIOSMEM_END         0x100000
void biosmem_register_boot_data(phys_addr_t start, phys_addr_t end,
boolean_t temporary);
#ifdef MACH_HYP
void biosmem_xen_bootstrap(void);
#else
void biosmem_bootstrap(const struct multiboot_raw_info *mbi);
#endif
unsigned long biosmem_bootalloc(unsigned int nr_pages);
phys_addr_t biosmem_directmap_end(void);
void biosmem_setup(void);
void biosmem_free_usable(void);
boolean_t biosmem_addr_available(phys_addr_t addr);
#endif