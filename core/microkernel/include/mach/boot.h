#ifndef _MACH_BOOT_
#define _MACH_BOOT_
#include <mach/machine/boot.h>
#ifndef __ASSEMBLER__
#include <mach/machine/vm_types.h>
struct boot_image_info
{
struct boot_module *first_bmod;
struct boot_rendezvous *first_rzv;
vm_offset_t start, end;
struct machine_boot_image_info mboot;
};
struct boot_module
{
int magic;
int (*init)(struct boot_image_info *bii);
vm_offset_t text;
vm_offset_t etext;
vm_offset_t data;
vm_offset_t edata;
vm_offset_t bss;
vm_offset_t ebss;
};
#define BMOD_VALID(bmod) ((bmod)->magic == BMOD_MAGIC)
#define BMOD_NEXT(bmod) ((struct boot_module*)((bmod)->edata))
struct boot_rendezvous
{
struct boot_rendezvous *next;
int code;
};
#endif
#define BMOD_MAGIC 0x424d4f44
#define BRZV_KERNEL 'K'
#define BRZV_BOOTSTRAP 'B'
#define BRZV_DATA 'D'
#endif