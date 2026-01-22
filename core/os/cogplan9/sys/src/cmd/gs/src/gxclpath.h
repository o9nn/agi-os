#ifndef gxclpath_INCLUDED
# define gxclpath_INCLUDED
#define cap_join_known (1<<0)
#define cj_ac_sa_known (1<<1)
#define flatness_known (1<<2)
#define line_width_known (1<<3)
#define miter_limit_known (1<<4)
#define op_bm_tk_known (1<<5)
#define segment_notes_known (1<<6)
#define opacity_alpha_known (1<<7)
#define shape_alpha_known (1<<8)
#define alpha_known (1<<9)
#define misc2_all_known ((1<<10)-1)
#define fill_adjust_known (1<<10)
#define ctm_known (1<<11)
#define dash_known (1<<12)
#define clip_path_known (1<<13)
#define stroke_all_known ((1<<14)-1)
#define color_space_known (1<<14)
typedef enum {
cmd_dc_type_pure = 0,
cmd_dc_type_ht = 1,
cmd_dc_type_color = 2
} cmd_dc_type;
typedef enum {
cmd_op_misc2 = 0xd0,
cmd_opv_set_fill_adjust = 0xd2,
cmd_opv_set_ctm = 0xd3,
cmd_opv_set_color_space = 0xd4,
cmd_opv_set_misc2 = 0xd5,
cmd_opv_set_dash = 0xd6,
cmd_opv_enable_clip = 0xd7,
cmd_opv_disable_clip = 0xd8,
cmd_opv_begin_clip = 0xd9,
cmd_opv_end_clip = 0xda,
cmd_opv_begin_image_rect = 0xdb,
cmd_opv_begin_image = 0xdc,
cmd_opv_image_data = 0xdd,
cmd_opv_image_plane_data = 0xde,
cmd_opv_extend = 0xdf,
cmd_op_segment = 0xe0,
cmd_opv_rmoveto = 0xe0,
cmd_opv_rlineto = 0xe1,
cmd_opv_hlineto = 0xe2,
cmd_opv_vlineto = 0xe3,
cmd_opv_rmlineto = 0xe4,
cmd_opv_rm2lineto = 0xe5,
cmd_opv_rm3lineto = 0xe6,
cmd_opv_rrcurveto = 0xe7,
cmd_opv_min_curveto = cmd_opv_rrcurveto,
cmd_opv_hvcurveto = 0xe8,
cmd_opv_vhcurveto = 0xe9,
cmd_opv_nrcurveto = 0xea,
cmd_opv_rncurveto = 0xeb,
cmd_opv_vqcurveto = 0xec,
cmd_opv_hqcurveto = 0xed,
cmd_opv_scurveto = 0xee,
cmd_opv_max_curveto = cmd_opv_scurveto,
cmd_opv_closepath = 0xef,
cmd_op_path = 0xf0,
cmd_opv_fill = 0xf0,
cmd_opv_eofill = 0xf3,
cmd_opv_stroke = 0xf6,
cmd_opv_polyfill = 0xf9
} gx_cmd_xop;
typedef enum {
cmd_opv_ext_put_params = 0x00,
cmd_opv_ext_create_compositor = 0x01,
cmd_opv_ext_put_halftone = 0x02,
cmd_opv_ext_put_ht_seg = 0x03,
cmd_opv_ext_put_drawing_color = 0x04
} gx_cmd_ext_op;
#define cmd_segment_op_num_operands_values\
2, 2, 1, 1, 4, 6, 6, 6, 4, 4, 4, 4, 2, 2, 0, 0
#define cmd_misc2_op_name_strings\
"set_color", "set_color_short", "set_fill_adjust", "set_ctm",\
"set_color_space", "set_misc2", "set_dash", "enable_clip",\
"disable_clip", "begin_clip", "end_clip", "begin_image_rect",\
"begin_image", "image_data", "image_plane_data", "put_params"
#define cmd_segment_op_name_strings\
"rmoveto", "rlineto", "hlineto", "vlineto",\
"rmlineto", "rm2lineto", "rm3lineto", "rrcurveto",\
"hvcurveto", "vhcurveto", "nrcurveto", "rncurveto",\
"vqcurveto", "hqcurveto", "scurveto", "closepath"
#define cmd_path_op_name_strings\
"fill", "htfill", "colorfill", "eofill",\
"hteofill", "coloreofill", "stroke", "htstroke",\
"colorstroke", "polyfill", "htpolyfill", "colorpolyfill",\
"?fc?", "?fd?", "?fe?", "?ff?"
#define is_bits(d, n) !(((d) + ((fixed)1 << ((n) - 1))) & (-(fixed)1 << (n)))
#define cbuf_ht_seg_max_size (cbuf_size - 32)
dev_proc_fill_path(clist_fill_path);
dev_proc_stroke_path(clist_stroke_path);
dev_proc_fill_parallelogram(clist_fill_parallelogram);
dev_proc_fill_triangle(clist_fill_triangle);
#define state_neq(member)\
(cdev->imager_state.member != pis->member)
#define state_update(member)\
(cdev->imager_state.member = pis->member)
gx_color_index cmd_drawing_colors_used(gx_device_clist_writer *cldev,
const gx_drawing_color *pdcolor);
bool cmd_slow_rop(gx_device *dev, gs_logical_operation_t lop,
const gx_drawing_color *pdcolor);
int cmd_put_drawing_color(gx_device_clist_writer * cldev,
gx_clist_state * pcls,
const gx_drawing_color * pdcolor);
void cmd_clear_known(gx_device_clist_writer * cldev, uint known);
int cmd_write_ctm_return_length(gx_device_clist_writer * cldev, const gs_matrix *m);
int cmd_write_ctm(const gs_matrix *m, byte *dp, int len);
#define cmd_do_write_unknown(cldev, pcls, must_know)\
( ~(pcls)->known & (must_know) ?\
cmd_write_unknown(cldev, pcls, must_know) : 0 )
int cmd_write_unknown(gx_device_clist_writer * cldev, gx_clist_state * pcls,
uint must_know);
bool cmd_check_clip_path(gx_device_clist_writer * cldev,
const gx_clip_path * pcpath);
#endif