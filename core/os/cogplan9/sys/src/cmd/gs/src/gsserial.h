#ifndef gsserial_INCLUDED
# define gsserial_INCLUDED
#define enc_u_shift 7
#define enc_u_lim_1b (1U << enc_u_shift)
#define enc_u_lim_2b (1U << (2 * enc_u_shift))
extern int enc_u_size_uint(uint);
#define enc_u_sizew(w) \
( (uint)(w) < enc_u_lim_1b \
? 1 \
: (uint)(w) < enc_u_lim_2b ? 2 : enc_u_size_uint(w) )
#define enc_u_size2w(w1, w2) \
( ((uint)(w1) | (uint)(w2)) < enc_u_lim_1b \
? 2 \
: enc_u_size_uint(w1) + enc_u_size_uint(w2) )
#define enc_u_sizexy(xy) enc_u_size2w((xy).x, (xy).y)
#define enc_u_sizew_max ((8 * sizeof(uint) + enc_u_shift - 1) / enc_u_shift)
extern byte * enc_u_put_uint(uint, byte *);
extern const byte * enc_u_get_uint(uint *, const byte *);
extern byte * enc_u_get_uint_nc(uint *, byte *);
#define enc_u_putw(w, p) \
BEGIN \
if ((uint)(w) < enc_u_lim_1b) \
*(p)++ = (byte)(w); \
else if ((uint)(w) < enc_u_lim_2b) { \
*(p)++ = enc_u_lim_1b | ((w) & (enc_u_lim_1b - 1)); \
*(p)++ = (w) >> enc_u_shift; \
} else \
(p) = enc_u_put_uint((w), (p)); \
END
#define enc_u_put2w(w1, w2, p) \
BEGIN \
if (((uint)(w1) | (uint)(w2)) < enc_u_lim_1b) { \
*(p)++ = (w1); \
*(p)++ = (w2); \
} else { \
(p) = enc_u_put_uint((w1), (p)); \
(p) = enc_u_put_uint((w2), (p)); \
} \
END
#define enc_u_putxy(xy, p) enc_u_put2w((xy).x, (xy).y, (p))
#define enc_u_getw(w, p) \
BEGIN \
if (((w) = *(p)) >= enc_u_lim_1b) { \
uint tmp_w; \
\
(p) = enc_u_get_uint(&tmp_w, (p)); \
(w) = tmp_w; \
} else \
++(p); \
END
#define enc_u_getw_nc(w, p) \
BEGIN \
if (((w) = *(p)) >= enc_u_lim_1b) { \
uint tmp_w; \
\
(p) = enc_u_get_uint_nc(&tmp_w, (p)); \
(w) = tmp_w; \
} else \
++(p); \
END
#define enc_u_get2w(w1, w2, p) \
BEGIN \
enc_u_getw((w1), (p)); \
enc_u_getw((w2), (p)); \
END
#define enc_u_get2w_nc(w1, w2, p) \
BEGIN \
enc_u_getw_nc((w1), (p)); \
enc_u_getw_nc((w2), (p)); \
END
#define enc_u_getxy(xy, p) enc_u_get2w((xy).x, (xy).y, (p))
#define enc_u_getxy_nc(xy, p) enc_u_get2w_nc((xy).x, (xy).y, (p))
#define enc_s_shift0 6
#define enc_s_shift1 (enc_s_shift0 + 1)
#define enc_s_max_1b ((1U << enc_s_shift0) - 1)
#define enc_s_min_1b (-(int)enc_s_max_1b)
#define enc_s_max_2b ((1U << (enc_s_shift0 + enc_s_shift1) - 1))
#define enc_s_min_2b (-enc_s_max_2b)
#define enc_s_min_int ((int)(1U << (8 * sizeof(int) - 1)))
extern int enc_s_size_int(int);
#define enc_s_sizew_max ((8 * sizeof(int)) / enc_s_shift1 + 1)
#define enc_s_sizew(v) \
( (v) >= 0 ? enc_u_sizew((uint)(v) << 1) \
: (v) != enc_s_min_int ? enc_u_sizew((uint)-(v) << 1) \
: enc_s_sizew_max )
#define enc_s_sizexy(xy) (enc_s_sizew((xy).x) + enc_s_sizew((xy).y))
extern byte * enc_s_put_int(int, byte *);
extern const byte * enc_s_get_int(int *, const byte *);
extern byte * enc_s_get_int_nc(int *, byte *);
#define enc_s_putw(v, p) \
BEGIN \
if ((int)(v) <= enc_s_max_1b && (int)(v) >= enc_s_min_1b) \
*(p)++ = ((v) & enc_s_max_1b) \
| ((v) < 0 ? (enc_s_max_1b + 1) : 0); \
else \
(p) = enc_s_put_int((v), (p)); \
END
#define enc_s_putxy(xy, p) \
BEGIN \
enc_s_putw((xy).x, (p)); \
enc_s_putw((xy).y, (p)); \
END
#define enc_s_getw(v, p) \
BEGIN \
if (((v = *p) & (1U << enc_s_shift1)) != 0) { \
int tmp_v; \
\
(p) = enc_s_get_int(&tmp_v, (p)); \
(v) = tmp_v; \
} else { \
if (((v) & (1U << enc_s_shift0)) != 0) \
(v) = -((v) & enc_s_max_1b); \
++(p); \
} \
END
#define enc_s_getw_nc(v, p) \
BEGIN \
if (((v = *p) & (1U << enc_s_shift1)) != 0) { \
int tmp_v; \
\
(p) = enc_s_get_int_nc(&tmp_v, (p)); \
(v) = tmp_v; \
} else { \
if (((v) & (1U << enc_s_shift0)) != 0) \
(v) = -((v) & enc_s_max_1b); \
++(p); \
} \
END
#define enc_s_getxy(xy, p) \
BEGIN \
enc_s_getw((xy).x, (p)); \
enc_s_getw((xy).y, (p)); \
END
#define enc_s_getxy_nc(xy, p) \
BEGIN \
enc_s_getw_nc((xy).x, (p)); \
enc_s_getw_nc((xy).y, (p)); \
END
#endif