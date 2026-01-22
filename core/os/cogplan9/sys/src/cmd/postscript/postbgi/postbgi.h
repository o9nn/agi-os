#define BRCHAR		033
#define BCHAR		034
#define BGRAPH		035
#define BSUB		042
#define BRET		043
#define BCALL		044
#define BEND		045
#define BERASE		046
#define BREP		047
#define BENDR		050
#define BSETX		051
#define BSETY		052
#define BSETXY		053
#define BINTEN		054
#define BVISX		055
#define BINVISX		056
#define BVISY		057
#define BINVISY		060
#define BVEC		061
#define BSVEC		062
#define BRECT		063
#define BPOINT1		064
#define BPOINT		065
#define BLINE		066
#define BCSZ		067
#define BLTY		070
#define BARC		071
#define BFARC		072
#define BFRECT		073
#define BRASRECT	074
#define BCOL		075
#define BFTRAPH		076
#define BPAT		077
#define BNOISE		0
#define BGISIZE		2
#define CHMASK		0177
#define DMASK		077
#define MSB		0100
#define SGNB		040
#define MSBMAG		037
#define X_COORD		0
#define Y_COORD		1
#define LONGVECTOR	2
#define SHORTVECTOR	3
#define VISIBLE		0
#define INVISIBLE	1
#define OUTLINE		0
#define FILL		1
#define SOLID		0
#define DOTTED		1
#define SHORTDASH	2
#define DASH		3
#define LONGDASH	4
#define DOTDASH		5
#define THREEDOT	6
#define STYLES								\
\
{								\
"[]",							\
"[.5 2]",							\
"[2 4]",							\
"[4 4]",							\
"[8 4]",							\
"[.5 2 4 2]",						\
"[.5 2 .5 2 .5 2 4 2]"					\
}
#define RED		0
#define GREEN		1
#define BLUE		2
typedef struct {
int	dx;
int	dy;
} Disp;
typedef struct {
char	*name;
char	*val;
} Fontmap;
#define FONTMAP								\
\
{								\
"R", "Courier",						\
"I", "Courier-Oblique",					\
"B", "Courier-Bold",					\
"CO", "Courier",						\
"CI", "Courier-Oblique",					\
"CB", "Courier-Bold",					\
"CW", "Courier",						\
"PO", "Courier",						\
"courier", "Courier",					\
"cour", "Courier",						\
"co", "Courier",						\
NULL, NULL							\
}
#define MAG(A, B)	(((A & MSBMAG) << 6) | (B & DMASK))
#define LINESPACE(A)	(8 * A)
char	*get_font();