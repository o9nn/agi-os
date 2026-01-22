#ifndef gscpm_INCLUDED
#  define gscpm_INCLUDED
typedef enum {
cpm_show,
cpm_charwidth,
cpm_false_charpath,
cpm_true_charpath,
cpm_false_charboxpath,
cpm_true_charboxpath
} gs_char_path_mode;
typedef enum {
CACHE_DEVICE_NONE = 0,
CACHE_DEVICE_NOT_CACHING,
CACHE_DEVICE_NONE_AND_CLIP,
CACHE_DEVICE_CACHING
} gs_in_cache_device_t;
#endif