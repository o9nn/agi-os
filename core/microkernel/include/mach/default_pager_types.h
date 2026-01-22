#ifndef	_MACH_DEFAULT_PAGER_TYPES_H_
#define _MACH_DEFAULT_PAGER_TYPES_H_
typedef struct default_pager_info {
vm_size_t dpi_total_space;
vm_size_t dpi_free_space;
vm_size_t dpi_page_size;
} default_pager_info_t;
typedef struct default_pager_object {
vm_offset_t dpo_object;
vm_size_t dpo_size;
} default_pager_object_t;
typedef default_pager_object_t *default_pager_object_array_t;
typedef struct default_pager_page {
vm_offset_t dpp_offset;
} default_pager_page_t;
typedef default_pager_page_t *default_pager_page_array_t;
typedef char default_pager_filename_t[256];
typedef const char *const_default_pager_filename_t;
#endif