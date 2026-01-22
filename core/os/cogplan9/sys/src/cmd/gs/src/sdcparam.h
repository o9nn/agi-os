#ifndef sdcparam_INCLUDED
#  define sdcparam_INCLUDED
int s_DCT_get_params(gs_param_list * plist, const stream_DCT_state * ss,
const stream_DCT_state * defaults);
int s_DCT_get_quantization_tables(gs_param_list * plist,
const stream_DCT_state * pdct,
const stream_DCT_state * defaults,
bool is_encode);
int s_DCT_get_huffman_tables(gs_param_list * plist,
const stream_DCT_state * pdct,
const stream_DCT_state * defaults,
bool is_encode);
int s_DCT_byte_params(gs_param_list * plist, gs_param_name key, int start,
int count, UINT8 * pvals);
int s_DCT_put_params(gs_param_list * plist, stream_DCT_state * pdct);
int s_DCT_put_quantization_tables(gs_param_list * plist,
stream_DCT_state * pdct,
bool is_encode);
int s_DCT_put_huffman_tables(gs_param_list * plist, stream_DCT_state * pdct,
bool is_encode);
#endif