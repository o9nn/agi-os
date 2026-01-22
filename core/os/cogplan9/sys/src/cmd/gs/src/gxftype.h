#ifndef gxftype_INCLUDED
#  define gxftype_INCLUDED
typedef enum {
ft_composite = 0,
ft_encrypted = 1,
ft_encrypted2 = 2,
ft_user_defined = 3,
ft_disk_based = 4,
ft_CID_encrypted = 9,
ft_CID_user_defined = 10,
ft_CID_TrueType = 11,
ft_Chameleon = 14,
ft_CID_bitmap = 32,
ft_TrueType = 42
} font_type;
typedef enum {
fbit_use_outlines = 0,
fbit_use_bitmaps = 1,
fbit_transform_bitmaps = 2
} fbit_type;
#endif