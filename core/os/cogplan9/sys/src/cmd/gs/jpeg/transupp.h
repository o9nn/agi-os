#ifndef TRANSFORMS_SUPPORTED
#define TRANSFORMS_SUPPORTED 1
#endif
#ifdef NEED_SHORT_EXTERNAL_NAMES
#define jtransform_request_workspace		jTrRequest
#define jtransform_adjust_parameters		jTrAdjust
#define jtransform_execute_transformation	jTrExec
#define jcopy_markers_setup			jCMrkSetup
#define jcopy_markers_execute			jCMrkExec
#endif
typedef enum {
JXFORM_NONE,
JXFORM_FLIP_H,
JXFORM_FLIP_V,
JXFORM_TRANSPOSE,
JXFORM_TRANSVERSE,
JXFORM_ROT_90,
JXFORM_ROT_180,
JXFORM_ROT_270
} JXFORM_CODE;
typedef struct {
JXFORM_CODE transform;
boolean trim;
boolean force_grayscale;
int num_components;
jvirt_barray_ptr * workspace_coef_arrays;
} jpeg_transform_info;
#if TRANSFORMS_SUPPORTED
EXTERN(void) jtransform_request_workspace
JPP((j_decompress_ptr srcinfo, jpeg_transform_info *info));
EXTERN(jvirt_barray_ptr *) jtransform_adjust_parameters
JPP((j_decompress_ptr srcinfo, j_compress_ptr dstinfo,
jvirt_barray_ptr *src_coef_arrays,
jpeg_transform_info *info));
EXTERN(void) jtransform_execute_transformation
JPP((j_decompress_ptr srcinfo, j_compress_ptr dstinfo,
jvirt_barray_ptr *src_coef_arrays,
jpeg_transform_info *info));
#endif
typedef enum {
JCOPYOPT_NONE,
JCOPYOPT_COMMENTS,
JCOPYOPT_ALL
} JCOPY_OPTION;
#define JCOPYOPT_DEFAULT  JCOPYOPT_COMMENTS
EXTERN(void) jcopy_markers_setup
JPP((j_decompress_ptr srcinfo, JCOPY_OPTION option));
EXTERN(void) jcopy_markers_execute
JPP((j_decompress_ptr srcinfo, j_compress_ptr dstinfo,
JCOPY_OPTION option));