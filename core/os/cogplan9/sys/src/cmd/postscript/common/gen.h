#define PROGRAMVERSION	"3.3.2"
#define NON_FATAL	0
#define FATAL		1
#define USER_FATAL	2
#define OFF		0
#define ON		1
#define FALSE		0
#define TRUE		1
#define BYTE		8
#define BMASK		0377
#define POINTS		72.3
#ifndef PI
#define PI		3.141592654
#endif
#define ONEBYTE		0
#define UTFENCODING	1
#define READING		ONEBYTE
#define WRITING		ONEBYTE
#define DOROUND	TRUE
#define DEFAULT_RES	72
#define PAGEHEIGHT	11.0 * DEFAULT_RES
#define PAGEWIDTH	8.5 * DEFAULT_RES
#define ABS(A)		((A) >= 0 ? (A) : -(A))
#define MIN(A, B)	((A) < (B) ? (A) : (B))
#define MAX(A, B)	((A) > (B) ? (A) : (B))