#ifndef spprint_INCLUDED
# define spprint_INCLUDED
#ifndef stream_DEFINED
# define stream_DEFINED
typedef struct stream_s stream;
#endif
#define stream_putc(s, c) spputc(s, c)
int stream_write(stream * s, const void *ptr, uint count);
int stream_puts(stream * s, const char *str);
const char *pprintg1(stream * s, const char *format, floatp v);
const char *pprintg2(stream * s, const char *format, floatp v1, floatp v2);
const char *pprintg3(stream * s, const char *format,
floatp v1, floatp v2, floatp v3);
const char *pprintg4(stream * s, const char *format,
floatp v1, floatp v2, floatp v3, floatp v4);
const char *pprintg6(stream * s, const char *format,
floatp v1, floatp v2, floatp v3, floatp v4,
floatp v5, floatp v6);
const char *pprintd1(stream * s, const char *format, int v);
const char *pprintd2(stream * s, const char *format, int v1, int v2);
const char *pprintd3(stream * s, const char *format,
int v1, int v2, int v3);
const char *pprintd4(stream * s, const char *format,
int v1, int v2, int v3, int v4);
const char *pprintld1(stream * s, const char *format, long v);
const char *pprintld2(stream * s, const char *format, long v1, long v2);
const char *pprintld3(stream * s, const char *format,
long v1, long v2, long v3);
const char *pprints1(stream * s, const char *format, const char *str);
const char *pprints2(stream * s, const char *format,
const char *str1, const char *str2);
const char *pprints3(stream * s, const char *format,
const char *str1, const char *str2, const char *str3);
#endif