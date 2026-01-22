#define RES 240
#define HSCALE 2
#define VSCALE 5
#define HMI (12 * HSCALE)
#define VMI (8 * VSCALE)
#define LEFTMARGIN 0
#define RIGHTMARGIN 3168
#define TOPMARGIN 0
#define BOTTOMMARGIN 2640
#define ROWS 400
#define COLUMNS 200
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