#pragma lib "libhtml.a"
#pragma src "/sys/src/libhtml"
extern uchar*	fromStr(Rune* buf, int n, int chset);
extern Rune*	toStr(uchar* buf, int n, int chset);
enum
{
ApplMsword,
ApplOctets,
ApplPdf,
ApplPostscript,
ApplRtf,
ApplFramemaker,
ApplMsexcel,
ApplMspowerpoint,
UnknownType,
Audio32kadpcm,
AudioBasic,
ImageCgm,
ImageG3fax,
ImageGif,
ImageIef,
ImageJpeg,
ImagePng,
ImageTiff,
ImageXBit,
ImageXBit2,
ImageXBitmulti,
ImageXXBitmap,
ModelVrml,
MultiDigest,
MultiMixed,
TextCss,
TextEnriched,
TextHtml,
TextJavascript,
TextPlain,
TextRichtext,
TextSgml,
TextTabSeparatedValues,
TextXml,
VideoMpeg,
VideoQuicktime,
NMEDIATYPES
};
enum
{
HGet,
HPost
};
enum
{
UnknownCharset,
US_Ascii,
ISO_8859_1,
UTF_8,
Unicode,
NCHARSETS
};
enum {
FTtop,
FTself,
FTparent,
FTblank
};
typedef struct Token Token;
typedef struct Attr Attr;
#pragma incomplete Token
typedef struct Item Item;
typedef struct Itext Itext;
typedef struct Irule Irule;
typedef struct Iimage Iimage;
typedef struct Iformfield Iformfield;
typedef struct Itable Itable;
typedef struct Ifloat Ifloat;
typedef struct Ispacer Ispacer;
typedef struct Genattr Genattr;
typedef struct SEvent SEvent;
typedef struct Formfield Formfield;
typedef struct Option Option;
typedef struct Form Form;
typedef struct Table Table;
typedef struct Tablecol Tablecol;
typedef struct Tablerow Tablerow;
typedef struct Tablecell Tablecell;
typedef struct Align Align;
typedef struct Dimen Dimen;
typedef struct Anchor Anchor;
typedef struct DestAnchor DestAnchor;
typedef struct Map Map;
typedef struct Area Area;
typedef struct Background Background;
typedef struct Kidinfo Kidinfo;
typedef struct Docinfo Docinfo;
typedef struct Stack Stack;
typedef struct Pstate Pstate;
typedef struct ItemSource ItemSource;
typedef struct Lay Lay;
#pragma incomplete Lay
enum {
ALnone = 0, ALleft, ALcenter, ALright, ALjustify,
ALchar, ALtop, ALmiddle, ALbottom, ALbaseline,
};
struct Align
{
uchar	halign;
uchar	valign;
};
enum {
Dnone =		0,
Dpixels =	(1<<29),
Dpercent =	(2<<29),
Drelative =	(3<<29),
Dkindmask =	(3<<29),
Dspecmask =	(~Dkindmask)
};
struct Dimen
{
int	kindspec;
};
struct Background
{
Rune*	image;
int	color;
};
struct Item
{
Item*	next;
int	width;
int	height;
int	ascent;
int	anchorid;
int	state;
Genattr*genattr;
int	tag;
};
enum {
Itexttag,
Iruletag,
Iimagetag,
Iformfieldtag,
Itabletag,
Ifloattag,
Ispacertag
};
struct Itext
{
Item;
Rune*	s;
int	fnt;
int	fg;
uchar	voff;
uchar	ul;
};
struct Irule
{
Item;
uchar	align;
uchar	noshade;
int	size;
int	color;
Dimen	wspec;
};
struct Iimage
{
Item;
Rune*	imsrc;
int	imwidth;
int	imheight;
Rune*	altrep;
Map*	map;
int	ctlid;
uchar	align;
uchar	hspace;
uchar	vspace;
uchar	border;
Iimage*	nextimage;
void*	aux;
};
struct Iformfield
{
Item;
Formfield*formfield;
void*	aux;
};
struct Itable
{
Item;
Table*	table;
};
struct Ifloat
{
Item;
Item*	item;
int	x;
int	y;
uchar	side;
uchar	infloats;
Ifloat*	nextfloat;
};
struct Ispacer
{
Item;
int	spkind;
};
enum {
IFbrk	= 0x80000000,
IFbrksp	= 0x40000000,
IFnobrk	= 0x20000000,
IFcleft	= 0x10000000,
IFcright= 0x08000000,
IFwrap	= 0x04000000,
IFhang	= 0x02000000,
IFrjust	= 0x01000000,
IFcjust	= 0x00800000,
IFsmap	= 0x00400000,
IFindentshift	= 8,
IFindentmask	= (255<<IFindentshift),
IFhangmask	= 255
};
enum { Voffbias = 128 };
enum {
ISPnull,
ISPvline,
ISPhspace,
ISPgeneral
};
struct Genattr
{
Rune*	id;
Rune*	class;
Rune*	style;
Rune*	title;
SEvent*	events;
};
struct SEvent
{
SEvent*	next;
int	type;
Rune*	script;
};
enum {
SEonblur, SEonchange, SEonclick, SEondblclick,
SEonfocus, SEonkeypress, SEonkeyup, SEonload,
SEonmousedown, SEonmousemove, SEonmouseout,
SEonmouseover, SEonmouseup, SEonreset, SEonselect,
SEonsubmit, SEonunload,
Numscriptev
};
enum {
Ftext,
Fpassword,
Fcheckbox,
Fradio,
Fsubmit,
Fhidden,
Fimage,
Freset,
Ffile,
Fbutton,
Fselect,
Ftextarea
};
struct Formfield
{
Formfield*next;
int	ftype;
int	fieldid;
Form*	form;
Rune*	name;
Rune*	value;
int	size;
int	maxlength;
int	rows;
int	cols;
uchar	flags;
Option*	options;
Item*	image;
int	ctlid;
SEvent*	events;
};
enum {
FFchecked =	(1<<7),
FFmultiple =	(1<<6)
};
struct Option
{
Option*	next;
int	selected;
Rune*	value;
Rune*	display;
};
struct Form
{
Form*	next;
int	formid;
Rune*	name;
Rune*	action;
int	target;
int	method;
int	nfields;
Formfield*fields;
};
enum {
TFparsing =	(1<<7),
TFnowrap =	(1<<6),
TFisth =	(1<<5)
};
struct Table
{
Table*	next;
int	tableid;
Tablerow*rows;
int	nrow;
Tablecol*cols;
int	ncol;
Tablecell*cells;
int	ncell;
Tablecell***grid;
Align	align;
Dimen	width;
int	border;
int	cellspacing;
int	cellpadding;
Background background;
Item*	caption;
uchar	caption_place;
Lay*	caption_lay;
int	totw;
int	toth;
int	caph;
int	availw;
Token*	tabletok;
uchar	flags;
};
struct Tablecol
{
int	width;
Align	align;
Point	pos;
};
struct Tablerow
{
Tablerow*next;
Tablecell*cells;
int	height;
int	ascent;
Align	align;
Background background;
Point	pos;
uchar	flags;
};
struct Tablecell
{
Tablecell*next;
Tablecell*nextinrow;
int	cellid;
Item*	content;
Lay*	lay;
int	rowspan;
int	colspan;
Align	align;
uchar	flags;
Dimen	wspec;
int	hspec;
Background background;
int	minw;
int	maxw;
int	ascent;
int	row;
int	col;
Point	pos;
};
struct Anchor
{
Anchor*	next;
int	index;
Rune*	name;
Rune*	href;
int	target;
};
struct DestAnchor
{
DestAnchor*next;
int	index;
Rune*	name;
Item*	item;
};
struct Map
{
Map*	next;
Rune*	name;
Area*	areas;
};
struct Area
{
Area*	next;
int	shape;
Rune*	href;
int	target;
Dimen*	coords;
int	ncoords;
};
enum {
SHrect, SHcircle, SHpoly
};
enum {
FntR,
FntI,
FntB,
FntT,
NumStyle
};
enum {
Tiny,
Small,
Normal,
Large,
Verylarge,
NumSize
};
enum {
NumFnt = NumStyle*NumSize,
DefFnt = FntR*NumSize+Normal,
};
enum {
ULnone, ULunder, ULmid
};
enum {
FRnoresize =	(1<<0),
FRnoscroll =	(1<<1),
FRhscroll = 	(1<<2),
FRvscroll =	(1<<3),
FRhscrollauto = (1<<4),
FRvscrollauto =	(1<<5)
};
struct Kidinfo
{
Kidinfo*next;
int	isframeset;
Rune*	src;
Rune*	name;
int	marginw;
int	marginh;
int	framebd;
int	flags;
Dimen*	rows;
int	nrows;
Dimen*	cols;
int	ncols;
Kidinfo*kidinfos;
Kidinfo*nextframeset;
};
struct Docinfo
{
Rune*	src;
Rune*	base;
Rune*	doctitle;
Background background;
Iimage*	backgrounditem;
int	text;
int	link;
int	vlink;
int	alink;
int	target;
int	chset;
int	mediatype;
int	scripttype;
int	hasscripts;
Rune*	refresh;
Kidinfo*kidinfo;
int	frameid;
Anchor*	anchors;
DestAnchor*dests;
Form*	forms;
Table*	tables;
Map*	maps;
Iimage*	images;
};
extern int	dimenkind(Dimen d);
extern int	dimenspec(Dimen d);
extern void	freedocinfo(Docinfo* d);
extern void	freeitems(Item* ithead);
extern Item*	parsehtml(uchar* data, int datalen, Rune* src, int mtype, int chset, Docinfo** pdi);
extern void	printitems(Item* items, char* msg);
extern int	targetid(Rune* s);
extern Rune*	targetname(int targid);
extern int	validitems(Item* i);
#pragma varargck	type "I"	Item*
extern int	warn;
extern int	dbglex;
extern int	dbgbuild;
extern void*	emalloc(ulong);
extern void*	erealloc(void* p, ulong size);