#define LINESPP 66
#define TABSTOPS 8
#define POINTSIZE 10
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