#ifndef sisparam_INCLUDED
# define sisparam_INCLUDED
#define LOG2_MAX_ISCALE_SUPPORT 3
#define MAX_ISCALE_SUPPORT (1 << LOG2_MAX_ISCALE_SUPPORT)
typedef struct stream_image_scale_params_s {
int Colors;
int BitsPerComponentIn;
uint MaxValueIn;
int WidthIn, HeightIn;
int BitsPerComponentOut;
uint MaxValueOut;
int WidthOut, HeightOut;
} stream_image_scale_params_t;
#define stream_image_scale_state_common\
stream_state_common;\
stream_image_scale_params_t params
typedef struct stream_image_scale_state_s {
stream_image_scale_state_common;
} stream_image_scale_state;
#endif