#ifndef FTS_COMMON_H
#define FTS_COMMON_H
#define IS_NONASCII_APOSTROPHE(c) \
((c) == 0x2019 || (c) == 0xFF07)
#define IS_APOSTROPHE(c) \
((c) == 0x0027 || IS_NONASCII_APOSTROPHE(c))
#define IS_WB5A_APOSTROPHE(c) \
((c) == 0x0027 || (c) == 0x2019)
#define FTS_PREFIX_SPLAT_CHAR 0x002A
#define IS_PREFIX_SPLAT(c) \
((c) == FTS_PREFIX_SPLAT_CHAR)
#define IS_ASCII_VOWEL(c) \
((c) == 0x0041 || (c) == 0x0045 || (c) == 0x0048 || (c) == 0x0049 || \
(c) == 0x004F || (c) == 0x0055 || (c) == 0x0059 || (c) == 0x0061 || \
(c) == 0x0065 || (c) == 0x0068 || (c) == 0x0069 || (c) == 0x006F || \
(c) == 0x0075 || (c) == 0x0079)
#define IS_NONASCII_VOWEL(c) \
\
((c) == 0x00C0 || (c) == 0x00C1 || (c) == 0x00C2 || \
\
(c) == 0x00C8 || (c) == 0x00C9 || (c) == 0x00CA || \
\
(c) == 0x00CC || (c) == 0x00CD || (c) == 0x00CE || \
\
(c) == 0x00D2 || (c) == 0x00D3 || (c) == 0x00D4 || \
\
(c) == 0x00D9 || (c) == 0x00DA || (c) == 0x00DB || \
\
(c) == 0x00DD || \
\
(c) == 0x00E0 || (c) == 0x00E1 || (c) == 0x00E2 || \
\
(c) == 0x00E8 || (c) == 0x00E9 || (c) == 0x00EA || \
\
(c) == 0x00EC || (c) == 0x00ED || (c) == 0x00EE || \
\
(c) == 0x00F2 || (c) == 0x00F3 || (c) == 0x00F4 || \
\
(c) == 0x00F9 || (c) == 0x00FA || (c) == 0x00FB || \
\
(c) == 0x00FD )
#define IS_VOWEL(c) \
(IS_ASCII_VOWEL(c) || IS_NONASCII_VOWEL(c))
#endif