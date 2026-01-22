#ifndef gstrans_INCLUDED
# define gstrans_INCLUDED
#include "gstparam.h"
#include "gxcomp.h"
typedef enum {
PDF14_PUSH_DEVICE,
PDF14_POP_DEVICE,
PDF14_BEGIN_TRANS_GROUP,
PDF14_END_TRANS_GROUP,
PDF14_INIT_TRANS_MASK,
PDF14_BEGIN_TRANS_MASK,
PDF14_END_TRANS_MASK,
PDF14_SET_BLEND_PARAMS
} pdf14_compositor_operations;
#define PDF14_OPCODE_NAMES \
{\
"PDF14_PUSH_DEVICE      ",\
"PDF14_POP_DEVICE       ",\
"PDF14_BEGIN_TRANS_GROUP",\
"PDF14_END_TRANS_GROUP  ",\
"PDF14_INIT_TRANS_MASK  ",\
"PDF14_BEGIN_TRANS_MASK ",\
"PDF14_END_TRANS_MASK   ",\
"PDF14_SET_BLEND_PARAMS "\
}
#define PDF14_SET_BLEND_MODE (1 << 0)
#define PDF14_SET_TEXT_KNOCKOUT (1 << 1)
#define PDF14_SET_SHAPE_ALPHA (1 << 2)
#define PDF14_SET_OPACITY_ALPHA (1 << 3)
#ifndef gs_function_DEFINED
typedef struct gs_function_s gs_function_t;
# define gs_function_DEFINED
#endif
typedef struct gs_transparency_source_s {
float alpha;
gs_transparency_mask_t *mask;
} gs_transparency_source_t;
struct gs_pdf14trans_params_s {
pdf14_compositor_operations pdf14_op;
int changed;
bool Isolated;
bool Knockout;
gs_rect bbox;
gs_transparency_channel_selector_t csel;
gs_transparency_mask_subtype_t subtype;
int Background_components;
bool function_is_identity;
float Background[GS_CLIENT_COLOR_MAX_COMPONENTS];
float GrayBackground;
gs_function_t *transfer_function;
byte transfer_fn[MASK_TRANSFER_FUNCTION_SIZE];
gs_blend_mode_t blend_mode;
bool text_knockout;
gs_transparency_source_t opacity;
gs_transparency_source_t shape;
bool mask_is_image;
};
typedef struct gs_pdf14trans_params_s gs_pdf14trans_params_t;
typedef struct gs_pdf14trans_s {
gs_composite_common;
gs_pdf14trans_params_t params;
} gs_pdf14trans_t;
int gs_setblendmode(gs_state *, gs_blend_mode_t);
gs_blend_mode_t gs_currentblendmode(const gs_state *);
int gs_setopacityalpha(gs_state *, floatp);
float gs_currentopacityalpha(const gs_state *);
int gs_setshapealpha(gs_state *, floatp);
float gs_currentshapealpha(const gs_state *);
int gs_settextknockout(gs_state *, bool);
bool gs_currenttextknockout(const gs_state *);
gs_transparency_state_type_t
gs_current_transparency_type(const gs_state *pgs);
int gs_push_pdf14trans_device(gs_state * pgs);
int gs_pop_pdf14trans_device(gs_state * pgs);
void gs_trans_group_params_init(gs_transparency_group_params_t *ptgp);
int gs_begin_transparency_group(gs_state * pgs,
const gs_transparency_group_params_t *ptgp,
const gs_rect *pbbox);
int gs_end_transparency_group(gs_state *pgs);
void gs_trans_mask_params_init(gs_transparency_mask_params_t *ptmp,
gs_transparency_mask_subtype_t subtype);
int gs_begin_transparency_mask(gs_state *pgs,
const gs_transparency_mask_params_t *ptmp,
const gs_rect *pbbox, bool mask_is_image);
int gs_end_transparency_mask(gs_state *pgs,
gs_transparency_channel_selector_t csel);
int gs_init_transparency_mask(gs_state *pgs,
gs_transparency_channel_selector_t csel);
int gs_discard_transparency_layer(gs_state *pgs);
int gx_begin_transparency_group(gs_imager_state * pis, gx_device * pdev,
const gs_pdf14trans_params_t * pparams);
int gx_end_transparency_group(gs_imager_state * pis, gx_device * pdev);
int gx_init_transparency_mask(gs_imager_state * pis,
const gs_pdf14trans_params_t * pparams);
int gx_begin_transparency_mask(gs_imager_state * pis, gx_device * pdev,
const gs_pdf14trans_params_t * pparams);
int gx_end_transparency_mask(gs_imager_state * pis, gx_device * pdev,
const gs_pdf14trans_params_t * pparams);
int gx_discard_transparency_layer(gs_imager_state *pis);
int gs_is_pdf14trans_compositor(const gs_composite_t * pct);
#define NUM_PDF14_BUFFERS 3
#define NUM_ALPHA_CHANNELS 1
#define NUM_COLOR_CHANNELS 4
#define BITS_PER_CHANNEL 8
#define ESTIMATED_PDF14_ROW_SIZE(width) ((width) * BITS_PER_CHANNEL\
* (NUM_ALPHA_CHANNELS + NUM_COLOR_CHANNELS))
#define ESTIMATED_PDF14_ROW_SPACE(width) \
(NUM_PDF14_BUFFERS * ESTIMATED_PDF14_ROW_SIZE(width))
#endif