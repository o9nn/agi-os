#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "dc_context.h"
#include "dc_tools.h"
#include "dc_saxparser.h"
static const char* s_ent[] = {
"lt;",      "<",	"gt;",      ">",	"quot;",    "\"",	"apos;",    "'",
"amp;",     "&",    "nbsp;",    " ",
"iexcl;",   "¡",	"cent;",    "¢",	"pound;",   "£",	"curren;",  "¤",
"yen;",     "¥",	"brvbar;",  "¦",	"sect;",    "§",	"uml;",     "¨",
"copy;",    "©",	"ordf;",    "ª",	"laquo;",   "«",	"not;",     "¬",
"shy;",     "-",	"reg;",     "®",	"macr;",    "¯",	"deg;",     "°",
"plusmn;",  "±",	"sup2;",    "²",	"sup3;",    "³",	"acute;",   "´",
"micro;",   "µ",	"para;",    "¶",	"middot;",  "·",	"cedil;",   "¸",
"sup1;",    "¹",	"ordm;",    "º",	"raquo;",   "»",	"frac14;",  "¼",
"frac12;",  "½",	"frac34;",  "¾",	"iquest;",  "¿",	"Agrave;",  "À",
"Aacute;",  "Á",	"Acirc;",   "Â",	"Atilde;",  "Ã",	"Auml;",    "Ä",
"Aring;",   "Å",	"AElig;",   "Æ",	"Ccedil;",  "Ç",	"Egrave;",  "È",
"Eacute;",  "É",	"Ecirc;",   "Ê",	"Euml;",    "Ë",	"Igrave;",  "Ì",
"Iacute;",  "Í",	"Icirc;",   "Î",	"Iuml;",    "Ï",	"ETH;",     "Ð",
"Ntilde;",  "Ñ",	"Ograve;",  "Ò",	"Oacute;",  "Ó",	"Ocirc;",   "Ô",
"Otilde;",  "Õ",	"Ouml;",    "Ö",	"times;",   "×",	"Oslash;",  "Ø",
"Ugrave;",  "Ù",	"Uacute;",  "Ú",	"Ucirc;",   "Û",	"Uuml;",    "Ü",
"Yacute;",  "Ý",	"THORN;",   "Þ",	"szlig;",   "ß",	"agrave;",  "à",
"aacute;",  "á",	"acirc;",   "â",	"atilde;",  "ã",	"auml;",    "ä",
"aring;",   "å",	"aelig;",   "æ",	"ccedil;",  "ç",	"egrave;",  "è",
"eacute;",  "é",	"ecirc;",   "ê",	"euml;",    "ë",	"igrave;",  "ì",
"iacute;",  "í",	"icirc;",   "î",	"iuml;",    "ï",	"eth;",     "ð",
"ntilde;",  "ñ",	"ograve;",  "ò",	"oacute;",  "ó",	"ocirc;",   "ô",
"otilde;",  "õ",	"ouml;",    "ö",	"divide;",  "÷",	"oslash;",  "ø",
"ugrave;",  "ù",	"uacute;",  "ú",	"ucirc;",   "û",	"uuml;",    "ü",
"yacute;",  "ý",	"thorn;",   "þ",	"yuml;",    "ÿ",	"OElig;",   "Œ",
"oelig;",   "œ",	"Scaron;",  "Š",	"scaron;",  "š",	"Yuml;",    "Ÿ",
"fnof;",    "ƒ",	"circ;",    "ˆ",	"tilde;",   "˜",	"Alpha;",   "Α",
"Beta;",    "Β",	"Gamma;",   "Γ",	"Delta;",   "Δ",	"Epsilon;", "Ε",
"Zeta;",    "Ζ",	"Eta;",     "Η",	"Theta;",   "Θ",	"Iota;",    "Ι",
"Kappa;",   "Κ",	"Lambda;",  "Λ",	"Mu;",      "Μ",	"Nu;",      "Ν",
"Xi;",      "Ξ",	"Omicron;", "Ο",	"Pi;",      "Π",	"Rho;",     "Ρ",
"Sigma;",   "Σ",	"Tau;",     "Τ",	"Upsilon;", "Υ",	"Phi;",     "Φ",
"Chi;",     "Χ",	"Psi;",     "Ψ",	"Omega;",   "Ω",	"alpha;",   "α",
"beta;",    "β",	"gamma;",   "γ",	"delta;",   "δ",	"epsilon;", "ε",
"zeta;",    "ζ",	"eta;",     "η",	"theta;",   "θ",	"iota;",    "ι",
"kappa;",   "κ",	"lambda;",  "λ",	"mu;",      "μ",	"nu;",      "ν",
"xi;",      "ξ",	"omicron;", "ο",	"pi;",      "π",	"rho;",     "ρ",
"sigmaf;",  "ς",	"sigma;",   "σ",	"tau;",     "τ",	"upsilon;", "υ",
"phi;",     "φ",	"chi;",     "χ",	"psi;",     "ψ",	"omega;",   "ω",
"thetasym;","ϑ",	"upsih;",   "ϒ",	"piv;",     "ϖ",	"ensp;",    " ",
"emsp;",    " ",	"thinsp;",  " ",	"zwnj;",    "" ,	"zwj;",     "" ,
"lrm;",     "" ,	"rlm;",     "" ,	"ndash;",   "–",	"mdash;",   "—",
"lsquo;",   "‘",	"rsquo;",   "’",	"sbquo;",   "‚",	"ldquo;",   "“",
"rdquo;",   "”",	"bdquo;",   "„",	"dagger;",  "†",	"Dagger;",  "‡",
"bull;",    "•",	"hellip;",  "…",	"permil;",  "‰",	"prime;",   "′",
"Prime;",   "″",	"lsaquo;",  "‹",	"rsaquo;",  "›",	"oline;",   "‾",
"frasl;",   "⁄",	"euro;",    "€",	"image;",   "ℑ",	"weierp;",  "℘",
"real;",    "ℜ",	"trade;",   "™",	"alefsym;", "ℵ",	"larr;",    "←",
"uarr;",    "↑",	"rarr;",    "→",	"darr;",    "↓",	"harr;",    "↔",
"crarr;",   "↵",	"lArr;",    "⇐",	"uArr;",    "⇑",	"rArr;",    "⇒",
"dArr;",    "⇓",	"hArr;",    "⇔",	"forall;",  "∀",	"part;",    "∂",
"exist;",   "∃",	"empty;",   "∅",	"nabla;",   "∇",	"isin;",    "∈",
"notin;",   "∉",	"ni;",      "∋",	"prod;",    "∏",	"sum;",     "∑",
"minus;",   "−",	"lowast;",  "∗",	"radic;",   "√",	"prop;",    "∝",
"infin;",   "∞",	"ang;",     "∠",	"and;",     "∧",	"or;",      "∨",
"cap;",     "∩",	"cup;",     "∪",	"int;",     "∫",	"there4;",  "∴",
"sim;",     "∼",	"cong;",    "≅",	"asymp;",   "≈",	"ne;",      "≠",
"equiv;",   "≡",	"le;",      "≤",	"ge;",      "≥",	"sub;",     "⊂",
"sup;",     "⊃",	"nsub;",    "⊄",	"sube;",    "⊆",	"supe;",    "⊇",
"oplus;",   "⊕",	"otimes;",  "⊗",	"perp;",    "⊥",	"sdot;",    "⋅",
"lceil;",   "⌈",	"rceil;",   "⌉",	"lfloor;",  "⌊",	"rfloor;",  "⌋",
"lang;",    "<",	"rang;",    ">",	"loz;",     "◊",	"spades;",  "♠",
"clubs;",   "♣",	"hearts;",  "♥",	"diams;",   "♦",
NULL,       NULL,
};
static char* xml_decode(char* s, char type)
{
char*       e = NULL;
char*       r = s;
const char* original_buf = s;
long        b = 0;
long        c = 0;
long        d = 0;
long        l = 0;
for (; *s; s++) {
while (*s == '\r') {
*(s++) = '\n';
if (*s == '\n') memmove(s, (s + 1), strlen(s));
}
}
for (s = r; ;) {
while (*s && *s != '&'  && !isspace(*s)) s++;
if (! *s)
{
break;
}
else if (type != 'c' && ! strncmp(s, "&#", 2))
{
if (s[2] == 'x') c = strtol(s + 3, &e, 16);
else c = strtol(s + 2, &e, 10);
if (! c || *e != ';') { s++; continue; }
if (c < 0x80) *(s++) = c;
else {
for (b = 0, d = c; d; d /= 2) b++;
b = (b - 2) / 5;
*(s++) = (0xFF << (7 - b)) | (c >> (6 * b));
while (b) *(s++) = 0x80 | ((c >> (6 * --b)) & 0x3F);
}
memmove(s, strchr(s, ';') + 1, strlen(strchr(s, ';')));
}
else if ((*s == '&' && (type == '&' || type == ' ' ))
)
{
for (b = 0; s_ent[b] && strncmp(s + 1, s_ent[b], strlen(s_ent[b])); b += 2)
;
if (s_ent[b++]) {
if ((c = strlen(s_ent[b])) - 1 > (e = strchr(s, ';')) - s) {
l = (d = (s - r)) + c + strlen(e);
if (r == original_buf) {
char* new_ret = malloc(l); if (new_ret == NULL) { return r; }
strcpy(new_ret, r);
r = new_ret;
}
else {
char* new_ret = realloc(r, l); if (new_ret == NULL) { return r; }
r = new_ret;
}
e = strchr((s = r + d), ';');
}
memmove(s + c, e + 1, strlen(e));
strncpy(s, s_ent[b], c);
}
else s++;
}
else if ((type == ' ' ) && isspace(*s))
{
*(s++) = ' ';
}
else s++;
}
return r;
}
#define XML_WS "\t\r\n "
static void def_starttag_cb (void* userdata, const char* tag, char** attr) { }
static void def_endtag_cb   (void* userdata, const char* tag) { }
static void def_text_cb     (void* userdata, const char* text, int len) { }
static void call_text_cb(dc_saxparser_t* saxparser, char* text, size_t len, char type)
{
if (text && len)
{
char bak = text[len], *text_new;
text[len] = '\0';
text_new = xml_decode(text, type);
saxparser->text_cb(saxparser->userdata, text_new, len);
if (text != text_new) { free(text_new); }
text[len] = bak;
}
}
static void do_free_attr(char** attr, int* free_attr)
{
#define FREE_KEY    0x01
#define FREE_VALUE  0x02
int i = 0;
while (attr[i]) {
if (free_attr[i>>1]&FREE_KEY   && attr[i])   { free(attr[i]); }
if (free_attr[i>>1]&FREE_VALUE && attr[i+1]) { free(attr[i+1]); }
i += 2;
}
attr[0] = NULL;
}
const char* dc_attr_find(char** attr, const char* key)
{
if (attr && key) {
int i = 0;
while (attr[i] && strcmp(key, attr[i])) {
i += 2;
}
if (attr[i]) {
return attr[i + 1];
}
}
return NULL;
}
void dc_saxparser_init(dc_saxparser_t* saxparser, void* userdata)
{
saxparser->userdata    = userdata;
saxparser->starttag_cb = def_starttag_cb;
saxparser->endtag_cb   = def_endtag_cb;
saxparser->text_cb     = def_text_cb;
}
void dc_saxparser_set_tag_handler(dc_saxparser_t* saxparser, dc_saxparser_starttag_cb_t starttag_cb, dc_saxparser_endtag_cb_t endtag_cb)
{
if (saxparser==NULL) {
return;
}
saxparser->starttag_cb = starttag_cb? starttag_cb : def_starttag_cb;
saxparser->endtag_cb   = endtag_cb?   endtag_cb   : def_endtag_cb;
}
void dc_saxparser_set_text_handler (dc_saxparser_t* saxparser, dc_saxparser_text_cb_t text_cb)
{
if (saxparser==NULL) {
return;
}
saxparser->text_cb = text_cb? text_cb : def_text_cb;
}
void dc_saxparser_parse(dc_saxparser_t* saxparser, const char* buf_start__)
{
char  bak = 0;
char* buf_start = NULL;
char* last_text_start = NULL;
char* p = NULL;
#define MAX_ATTR 100
char*   attr[(MAX_ATTR+1)*2];
int     free_attr[MAX_ATTR];
attr[0] = NULL;
if (saxparser==NULL) {
return;
}
buf_start = dc_strdup(buf_start__);
last_text_start = buf_start;
p               = buf_start;
while (*p)
{
if (*p=='<')
{
call_text_cb(saxparser, last_text_start, p - last_text_start, '&');
p++;
if (strncmp(p, "!--", 3)==0)
{
p = strstr(p, "-->");
if (p==NULL) { goto cleanup; }
p += 3;
}
else if (strncmp(p, "![CDATA[", 8)==0)
{
char* text_beg = p + 8;
if ((p = strstr(p, "]]>"))!=NULL)  {
call_text_cb(saxparser, text_beg, p-text_beg, 'c');
p += 3;
}
else {
call_text_cb(saxparser, text_beg, strlen(text_beg), 'c');
goto cleanup;
}
}
else if (strncmp(p, "!DOCTYPE", 8)==0)
{
while (*p && *p != '[' && *p != '>' ) p++;
if (*p==0) {
goto cleanup;
}
else if (*p=='[') {
p = strstr(p, "]>");
if (p==NULL) {
goto cleanup;
}
else {
p += 2;
}
}
else {
p++;
}
}
else if (*p=='?')
{
p = strstr(p, "?>");
if (p==NULL) { goto cleanup; }
p += 2;
}
else
{
p += strspn(p, XML_WS);
if (*p=='/')
{
p++;
p += strspn(p, XML_WS);
char* beg_tag_name = p;
p += strcspn(p, XML_WS "/>");
if (p != beg_tag_name)
{
bak = *p;
*p = '\0';
dc_strlower_in_place(beg_tag_name);
saxparser->endtag_cb(saxparser->userdata, beg_tag_name);
*p = bak;
}
}
else
{
do_free_attr(attr, free_attr);
char* beg_tag_name = p;
p += strcspn(p, XML_WS "/>");
if (p != beg_tag_name)
{
char* after_tag_name = p;
int attr_index = 0;
while (isspace(*p)) { p++; }
while (*p && *p!='/' && *p!='>')
{
char *beg_attr_name = p, *beg_attr_value = NULL, *beg_attr_value_new = NULL;
if ('='==*beg_attr_name) {
p++;
continue;
}
p += strcspn(p, XML_WS "=/>");
if (p != beg_attr_name)
{
char* after_attr_name = p;
p += strspn(p, XML_WS);
if (*p=='=')
{
p += strspn(p, XML_WS "=");
char quote = *p;
if (quote=='"' || quote=='\'')
{
p++;
beg_attr_value = p;
while (*p && *p != quote) { p++; }
if (*p) {
*p = '\0';
p++;
}
beg_attr_value_new = xml_decode(beg_attr_value, ' ');
}
else
{
beg_attr_value = p;
p += strcspn(p, XML_WS "/>");
bak = *p;
*p = '\0';
char* temp = dc_strdup(beg_attr_value);
beg_attr_value_new = xml_decode(temp, ' ');
if (beg_attr_value_new!=temp) { free(temp); }
*p = bak;
}
}
else
{
beg_attr_value_new = dc_strdup(NULL);
}
if (attr_index < MAX_ATTR)
{
char* beg_attr_name_new = beg_attr_name;
int   free_bits = (beg_attr_value_new != beg_attr_value)? FREE_VALUE : 0;
if (after_attr_name==p) {
bak = *after_attr_name;
*after_attr_name = '\0';
beg_attr_name_new = dc_strdup(beg_attr_name);
*after_attr_name = bak;
free_bits |= FREE_KEY;
}
else {
*after_attr_name = '\0';
}
dc_strlower_in_place(beg_attr_name_new);
attr[attr_index]         = beg_attr_name_new;
attr[attr_index+1]       = beg_attr_value_new;
attr[attr_index+2]       = NULL;
free_attr[attr_index>>1] = free_bits;
attr_index += 2;
}
}
while (isspace(*p)) { p++; }
}
char bak = *after_tag_name;
*after_tag_name = 0;
dc_strlower_in_place(beg_tag_name);
saxparser->starttag_cb(saxparser->userdata, beg_tag_name, attr);
*after_tag_name = bak;
p += strspn(p, XML_WS);
if (*p=='/')
{
p++;
*after_tag_name = 0;
saxparser->endtag_cb(saxparser->userdata, beg_tag_name);
}
}
}
p = strchr(p, '>');
if (p==NULL) { goto cleanup; }
p++;
}
last_text_start = p;
}
else
{
p++;
}
}
call_text_cb(saxparser, last_text_start, p - last_text_start, '&');
cleanup:
do_free_attr(attr, free_attr);
free(buf_start);
}