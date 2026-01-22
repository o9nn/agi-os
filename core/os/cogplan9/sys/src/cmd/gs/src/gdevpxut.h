#ifndef gdevpxut_INCLUDED
# define gdevpxut_INCLUDED
int px_write_file_header(stream *s, const gx_device *dev);
int px_write_page_header(stream *s, const gx_device *dev);
int px_write_select_media(stream *s, const gx_device *dev,
pxeMediaSize_t *pms,
byte *media_source);
int px_write_file_trailer(FILE *file);
#define PX_PUT_LIT(s, bytes) px_put_bytes(s, bytes, sizeof(bytes))
void px_put_bytes(stream * s, const byte * data, uint count);
#define DA(a) pxt_attr_ubyte, (a)
void px_put_a(stream * s, px_attribute_t a);
void px_put_ac(stream *s, px_attribute_t a, px_tag_t op);
#define DUB(b) pxt_ubyte, (byte)(b)
void px_put_ub(stream * s, byte b);
void px_put_uba(stream *s, byte b, px_attribute_t a);
#define DS(i) (byte)(i), (byte)((i) >> 8)
void px_put_s(stream * s, uint i);
#define DUS(i) pxt_uint16, DS(i)
void px_put_us(stream * s, uint i);
void px_put_usa(stream *s, uint i, px_attribute_t a);
void px_put_u(stream * s, uint i);
#define DUSP(ix,iy) pxt_uint16_xy, DS(ix), DS(iy)
void px_put_usp(stream * s, uint ix, uint iy);
void px_put_usq_fixed(stream * s, fixed x0, fixed y0, fixed x1, fixed y1);
void px_put_ss(stream * s, int i);
void px_put_ssp(stream * s, int ix, int iy);
void px_put_l(stream * s, ulong l);
void px_put_r(stream * s, floatp r);
void px_put_rl(stream * s, floatp r);
void px_put_data_length(stream * s, uint num_bytes);
#endif