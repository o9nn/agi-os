#define NUL '\000'
#define SOH '\001'
#define STX '\002'
#define ETX '\003'
#define EOT '\004'
#define ENQ '\005'
#define ACK '\006'
#define BEL '\007'
#define BS '\010'
#define HT '\011'
#define NL '\012'
#define VT '\013'
#define FF '\014'
#define CR '\015'
#define SO '\016'
#define SI '\017'
#define DLE '\020'
#define DC1 '\021'
#define DC2 '\022'
#define DC3 '\023'
#define DC4 '\024'
#define NAK '\025'
#define SYN '\026'
#define ETB '\027'
#define CAN '\030'
#define EM '\031'
#define SUB '\032'
#define ESC '\033'
#define FS '\034'
#define GS '\035'
#define RS '\036'
#define US '\037'
#define DEL '\177'
#define OUTMODED -1
#define ALPHA 0
#define GIN 1
#define GRAPH 2
#define POINT 3
#define SPECIALPOINT 4
#define INCREMENTAL 5
#define RESET 6
#define EXIT 7
#define UP 0
#define DOWN 1
#define TEKXMAX 4096
#define TEKYMAX 3120
#define INTENSITY \
\
{ \
14, 16, 17, 19, 20, 22, 23, 25, \
28, 31, 34, 38, 41, 44, 47, 50, \
56, 62, 69, 75, 81, 88, 94,100, \
56, 62, 69, 75, 81, 88, 94,100, \
0, 1, 1, 1, 1, 1, 1, 2, \
2, 2, 2, 2, 3, 3, 3, 3, \
4, 4, 4, 5, 5, 5, 6, 6, \
7, 8, 9, 10, 11, 12, 12, 13, \
14, 16, 17, 19, 20, 22, 23, 25, \
28, 31, 34, 38, 41, 44, 47, 50, \
56, 62, 69, 75, 81, 88, 94,100, \
56, 62, 69, 75, 81, 88, 94,100, \
}
#define CHARHEIGHT {88, 82, 53, 48}
#define CHARWIDTH {56, 51, 34, 31}
#define TEKFONT 2
#define STYLES \
\
{ \
"[]", \
"[.5 2]", \
"[.5 2 4 2]", \
"[4 4]", \
"[8 4]", \
"[]" \
}
typedef struct {
int x;
int y;
} Point;
typedef struct {
char *name;
char *val;
} Fontmap;
#define FONTMAP \
\
{ \
"R", "Courier", \
"I", "Courier-Oblique", \
"B", "Courier-Bold", \
"CO", "Courier", \
"CI", "Courier-Oblique", \
"CB", "Courier-Bold", \
"CW", "Courier", \
"PO", "Courier", \
"courier", "Courier", \
"cour", "Courier", \
"co", "Courier", \
NULL, NULL \
}
char *get_font();