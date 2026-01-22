#ifndef gxblend_INCLUDED
# define gxblend_INCLUDED
typedef bits16 ArtPixMaxDepth;
#define ART_MAX_CHAN 16
void
art_blend_pixel(ArtPixMaxDepth * dst, const ArtPixMaxDepth * backdrop,
const ArtPixMaxDepth * src, int n_chan,
gs_blend_mode_t blend_mode);
void
art_blend_pixel_8(byte *dst, const byte *backdrop,
const byte *src, int n_chan, gs_blend_mode_t blend_mode);
byte art_pdf_union_8(byte alpha1, byte alpha2);
byte art_pdf_union_mul_8(byte alpha1, byte alpha2, byte alpha_mask);
void
art_pdf_composite_pixel_alpha_8(byte *dst, const byte *src, int n_chan,
gs_blend_mode_t blend_mode);
void
art_pdf_uncomposite_group_8(byte *dst,
const byte *backdrop,
const byte *src, byte src_alpha_g, int n_chan);
void
art_pdf_recomposite_group_8(byte *dst, byte *dst_alpha_g,
const byte *src, byte src_alpha_g,
int n_chan,
byte alpha, gs_blend_mode_t blend_mode);
void
art_pdf_composite_group_8(byte *dst, byte *alpha_g,
const byte *src,
int n_chan, byte alpha, gs_blend_mode_t blend_mode);
void
art_pdf_composite_knockout_simple_8(byte *dst,
byte *dst_shape,
const byte *src,
int n_chan, byte opacity);
void
art_pdf_composite_knockout_isolated_8(byte *dst,
byte *dst_shape,
const byte *src,
int n_chan,
byte shape,
byte alpha_mask, byte shape_mask);
void
art_pdf_composite_knockout_8(byte *dst,
byte *dst_alpha_g,
const byte *backdrop,
const byte *src,
int n_chan,
byte shape,
byte alpha_mask,
byte shape_mask, gs_blend_mode_t blend_mode);
#endif