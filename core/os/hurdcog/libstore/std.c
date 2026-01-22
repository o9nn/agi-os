#include "store.h"
const struct store_class *const __attribute__ ((section ("store_std_classes")))
store_std_classes[] =
{
&store_device_class,
#if HAVE_PARTED_PARTED_H
&store_part_class,
#endif
&store_file_class,
&store_zero_class,
&store_task_class,
&store_ileave_class, &store_concat_class, &store_remap_class,
&store_query_class,
&store_copy_class, &store_gunzip_class, &store_bunzip2_class,
&store_url_open_class,
&store_nbd_class,
&store_typed_open_class,
0
};