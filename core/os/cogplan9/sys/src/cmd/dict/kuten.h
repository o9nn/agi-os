#define J2S(_h, _l) { \
\
if (((_l) += (((_h)-- % 2) ? 0x1f : 0x7d)) > 0x7e) (_l)++; \
\
if (((_h) = ((_h) / 2 + 0x71)) > 0x9f) (_h) += 0x40; \
}
#define S2J(_h, _l) { \
\
if (((_l) -= 0x1f) > 0x60) (_l)--; \
\
if (((_h) -= 0x81) > 0x5e) (_h) -= 0x40; (_h) *= 2, (_h) += 0x21; \
\
if ((_l) > 0x7e) (_h)++, (_l) -= 0x5e; \
}
#define ISJKANA(_b) (0xa0 <= (_b) && (_b) < 0xe0)
#define CANS2JH(_h) ((0x81 <= (_h) && (_h) < 0xf0) && !ISJKANA(_h))
#define CANS2JL(_l) (0x40 <= (_l) && (_l) < 0xfd && (_l) != 0x7f)
#define CANS2J(_h, _l) (CANS2JH(_h) && CANS2JL(_l))
#define CANJ2SB(_b) (0x21 <= (_b) && (_b) < 0x7f)
#define CANJ2S(_h, _l) (CANJ2SB(_h) && CANJ2SB(_l))
#define JIS208MAX 8407
#define GB2312MAX 8795
#define BIG5MAX 13973
extern Rune tabjis208[JIS208MAX];
extern Rune tabgb2312[GB2312MAX];
extern Rune tabbig5[BIG5MAX];