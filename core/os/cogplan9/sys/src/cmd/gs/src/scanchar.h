#ifndef scanchar_INCLUDED
# define scanchar_INCLUDED
extern const byte scan_char_array[max_stream_exception + 256];
#define scan_char_decoder (&scan_char_array[max_stream_exception])
#define min_radix 2
#define max_radix 36
#define ctype_name 100
#define ctype_btoken 101
#define ctype_space 102
#define ctype_other 103
#define ctype_exception 104
#define char_NULL 0
#define char_EOT 004
#define char_VT 013
#define char_DOS_EOF 032
#define char_CR '\r'
#if '\r' == '\n'
# define char_EOL 0x0a
#else
# define char_EOL '\n'
#endif
#endif