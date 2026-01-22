#ifndef _MACH_I386_MULTIBOOT_H_
#define _MACH_I386_MULTIBOOT_H_
#define MULTIBOOT_SEARCH 8192
#define MULTIBOOT_MAGIC 0x1badb002
#define MULTIBOOT_MUSTKNOW 0x0000ffff
#define MULTIBOOT_PAGE_ALIGN 0x00000001
#define MULTIBOOT_MEMORY_INFO 0x00000002
#define MULTIBOOT_VIDEO_MODE 0x00000004
#define MULTIBOOT_AOUT_KLUDGE 0x00010000
#define MULTIBOOT_VALID 0x2badb002
#define MULTIBOOT_MEMORY 0x00000001
#define MULTIBOOT_BOOT_DEVICE 0x00000002
#define MULTIBOOT_CMDLINE 0x00000004
#define MULTIBOOT_MODS 0x00000008
#define MULTIBOOT_AOUT_SYMS 0x00000010
#define MULTIBOOT_ELF_SHDR 0x00000020
#define MULTIBOOT_MEM_MAP 0x00000040
#define MULTIBOOT_FRAMEBUFFER 0x00001000
#define MULTIBOOT_VIDEO_MODE_TYPE_LINEARFB 0
#define MULTIBOOT_VIDEO_MODE_TYPE_EGA_TEXT 1
#define MULTIBOOT_VIDEO_PARAM_NO_PREFERENCE 0
#ifndef __ASSEMBLER__
#include <mach/machine/vm_types.h>
struct multiboot_module
{
vm_offset_t mod_start;
vm_offset_t mod_end;
vm_offset_t string;
unsigned reserved;
};
#ifdef __x86_64__
struct multiboot32_module
{
unsigned mod_start;
unsigned mod_end;
unsigned string;
unsigned reserved;
};
#endif
#define MB_ARD_MEMORY 1
#include <kern/macros.h>
#define MULTIBOOT_OS_MAGIC 0x1badb002
#define MULTIBOOT_OS_MEMORY_INFO 0x2
#define MULTIBOOT_OS_FLAGS MULTIBOOT_OS_MEMORY_INFO
#define MULTIBOOT_LOADER_MAGIC 0x2badb002
#define MULTIBOOT_LOADER_MEMORY 0x01
#define MULTIBOOT_LOADER_CMDLINE 0x04
#define MULTIBOOT_LOADER_MODULES 0x08
#define MULTIBOOT_LOADER_SHDR 0x20
#define MULTIBOOT_LOADER_MMAP 0x40
struct multiboot_header
{
uint32_t magic;
uint32_t flags;
uint32_t checksum;
uint32_t header_addr;
uint32_t load_addr;
uint32_t load_end_addr;
uint32_t bss_end_addr;
uint32_t entry_addr;
uint32_t mode_type;
uint32_t width;
uint32_t height;
uint32_t depth;
} __packed;
struct multiboot_color
{
uint8_t red;
uint8_t green;
uint8_t blue;
} __packed;
struct multiboot_raw_module {
uint32_t mod_start;
uint32_t mod_end;
uint32_t string;
uint32_t reserved;
} __packed;
struct multiboot_raw_mmap_entry {
uint32_t size;
uint64_t base_addr;
uint64_t length;
uint32_t type;
} __packed;
struct multiboot_framebuffer_info {
uint64_t framebuffer_addr;
uint32_t framebuffer_pitch;
uint32_t framebuffer_width;
uint32_t framebuffer_height;
uint8_t framebuffer_bpp;
#define MULTIBOOT_FRAMEBUFFER_TYPE_INDEXED 0
#define MULTIBOOT_FRAMEBUFFER_TYPE_RGB 1
#define MULTIBOOT_FRAMEBUFFER_TYPE_EGA_TEXT 2
uint8_t framebuffer_type;
union
{
struct
{
uint32_t framebuffer_palette_addr;
uint16_t framebuffer_palette_num_colors;
};
struct
{
uint8_t framebuffer_red_field_position;
uint8_t framebuffer_red_mask_size;
uint8_t framebuffer_green_field_position;
uint8_t framebuffer_green_mask_size;
uint8_t framebuffer_blue_field_position;
uint8_t framebuffer_blue_mask_size;
};
};
} __packed;
struct multiboot_raw_info {
uint32_t flags;
uint32_t mem_lower;
uint32_t mem_upper;
uint32_t unused0;
uint32_t cmdline;
uint32_t mods_count;
uint32_t mods_addr;
uint32_t shdr_num;
uint32_t shdr_size;
uint32_t shdr_addr;
uint32_t shdr_strndx;
uint32_t mmap_length;
uint32_t mmap_addr;
uint32_t unused1[9];
struct multiboot_framebuffer_info fb_info;
} __packed;
struct multiboot_os_module {
void *mod_start;
void *mod_end;
char *string;
};
struct multiboot_os_info {
uint32_t flags;
char *cmdline;
struct multiboot_module *mods_addr;
uint32_t mods_count;
};
#endif
#endif