#ifndef _XEN_PUBLIC_KEXEC_H
#define _XEN_PUBLIC_KEXEC_H
#include "xen.h"
#if defined(__i386__) || defined(__x86_64__)
#define KEXEC_XEN_NO_PAGES 17
#endif
#define KEXEC_TYPE_DEFAULT 0
#define KEXEC_TYPE_CRASH   1
typedef struct xen_kexec_image {
#if defined(__i386__) || defined(__x86_64__)
unsigned long page_list[KEXEC_XEN_NO_PAGES];
#endif
#if defined(__ia64__)
unsigned long reboot_code_buffer;
#endif
unsigned long indirection_page;
unsigned long start_address;
} xen_kexec_image_t;
#define KEXEC_CMD_kexec                 0
typedef struct xen_kexec_exec {
int type;
} xen_kexec_exec_t;
#define KEXEC_CMD_kexec_load            1
#define KEXEC_CMD_kexec_unload          2
typedef struct xen_kexec_load {
int type;
xen_kexec_image_t image;
} xen_kexec_load_t;
#define KEXEC_RANGE_MA_CRASH      0
#define KEXEC_RANGE_MA_XEN        1
#define KEXEC_RANGE_MA_CPU        2
#define KEXEC_RANGE_MA_XENHEAP    3
#define KEXEC_RANGE_MA_BOOT_PARAM 4
#define KEXEC_RANGE_MA_EFI_MEMMAP 5
#define KEXEC_RANGE_MA_VMCOREINFO 6
#define KEXEC_CMD_kexec_get_range       3
typedef struct xen_kexec_range {
int range;
int nr;
unsigned long size;
unsigned long start;
} xen_kexec_range_t;
#define VMCOREINFO_BYTES           (4096)
#define VMCOREINFO_NOTE_NAME       "VMCOREINFO_XEN"
void arch_crash_save_vmcoreinfo(void);
void vmcoreinfo_append_str(const char *fmt, ...)
__attribute__ ((format (printf, 1, 2)));
#define VMCOREINFO_PAGESIZE(value) \
vmcoreinfo_append_str("PAGESIZE=%ld\n", value)
#define VMCOREINFO_SYMBOL(name) \
vmcoreinfo_append_str("SYMBOL(%s)=%lx\n", #name, (unsigned long)&name)
#define VMCOREINFO_SYMBOL_ALIAS(alias, name) \
vmcoreinfo_append_str("SYMBOL(%s)=%lx\n", #alias, (unsigned long)&name)
#define VMCOREINFO_STRUCT_SIZE(name) \
vmcoreinfo_append_str("SIZE(%s)=%zu\n", #name, sizeof(struct name))
#define VMCOREINFO_OFFSET(name, field) \
vmcoreinfo_append_str("OFFSET(%s.%s)=%lu\n", #name, #field, \
(unsigned long)offsetof(struct name, field))
#define VMCOREINFO_OFFSET_ALIAS(name, field, alias) \
vmcoreinfo_append_str("OFFSET(%s.%s)=%lu\n", #name, #alias, \
(unsigned long)offsetof(struct name, field))
#endif