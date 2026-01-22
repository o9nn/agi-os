#include "api-structures.h"
#include "connectors.h"
#include "dict-common/dict-utils.h"
#include "disjunct-utils.h"
#include "linkage/linkage.h"
#include "post-process/post-process.h"
#include "print.h"
#include "tokenize/word-structures.h"
#include "wcwidth.h"
#define LEFT_WALL_SUPPRESS ("Wd")
#define RIGHT_WALL_SUPPRESS ("RW")
#define HEAD_CHR ('h')
#define DEPT_CHR ('d')
static size_t
set_centers(const Linkage linkage, int center[], int word_offset[],
bool print_word_0, int N_words_to_print)
{
int i, tot;
size_t n;
int start_word = print_word_0 ? 0 : 1;
int *link_len = alloca(linkage->num_words * sizeof(*link_len));
size_t max_bytes_in_line = 0;
memset(link_len, 0, linkage->num_words * sizeof(*link_len));
for (n = 0; n < linkage->num_links; n++)
{
Link *l = &linkage->link_array[n];
if (l->lw + 1 == l->rw)
{
link_len[l->rw] = strlen(l->link_name) +
(DEPT_CHR == connector_string(l->rc)[0]) +
(HEAD_CHR == connector_string(l->rc)[0]) +
(DEPT_CHR == connector_string(l->lc)[0]) +
(HEAD_CHR == connector_string(l->lc)[0]);
}
}
tot = 0;
for (i = start_word; i < N_words_to_print; i++)
{
int len, middle;
len = utf8_strwidth(linkage->word[i]);
middle = tot + (len/2);
#if 1
if (i > start_word)
center[i] = MAX(middle, center[i-1] + link_len[i] + 1);
else
#endif
center[i] = middle;
word_offset[i] = center[i] - middle;
tot += len+1 + word_offset[i];
max_bytes_in_line += word_offset[i] + 2*strlen(linkage->word[i]) + 1;
}
return max_bytes_in_line;
}
typedef struct
{
int N_rows;
int * link_heights;
int * row_starts;
} ps_ctxt_t;
static void left_append_string(dyn_str * string, const char * s, const char * t)
{
size_t twidth = strlen(t);
size_t swidth = utf8_strwidth(s);
for (size_t i = 0; i < twidth; )
{
if (i < swidth && *s != '\0')
{
int sw = utf8_charwidth(s);
if (sw < 0) sw = 2;
i += sw;
t += sw;
s += append_utf8_char(string, s);
}
else
{
append_utf8_char(string, t);
i++;
t++;
}
}
}
static void print_a_link(dyn_str * s, const Linkage linkage, LinkIdx link)
{
WordIdx l, r;
const char *label, *llabel, *rlabel;
l = linkage_get_link_lword(linkage, link);
r = linkage_get_link_rword(linkage, link);
label = linkage_get_link_label(linkage, link);
llabel = linkage_get_link_llabel(linkage, link);
rlabel = linkage_get_link_rlabel(linkage, link);
if (l == 0)
{
left_append_string(s, LEFT_WALL_DISPLAY, "               ");
}
else if (l == (linkage_get_num_words(linkage) - 1))
{
left_append_string(s, RIGHT_WALL_DISPLAY, "               ");
}
else
{
left_append_string(s, linkage_get_word(linkage, l), "               ");
}
left_append_string(s, llabel, "           ");
if (DEPT_CHR == llabel[0])
dyn_strcat(s, "   <---");
else if (HEAD_CHR == llabel[0])
dyn_strcat(s, "   >---");
else
dyn_strcat(s, "   ----");
left_append_string(s, label, "-----");
if (DEPT_CHR == rlabel[0])
dyn_strcat(s, "->  ");
else if (HEAD_CHR == rlabel[0])
dyn_strcat(s, "-<  ");
else
dyn_strcat(s, "--  ");
left_append_string(s, rlabel, "           ");
append_string(s, "     %s\n", linkage_get_word(linkage, r));
}
char * linkage_print_links_and_domains(const Linkage linkage)
{
int link, longest, j;
int N_links = linkage_get_num_links(linkage);
dyn_str * s = dyn_str_new();
const char ** dname;
compute_domain_names(linkage);
longest = 0;
for (link=0; link<N_links; link++)
{
assert (linkage_get_link_lword(linkage, link) < SIZE_MAX, "Missing word in link");
if (linkage_get_link_num_domains(linkage, link) > longest)
longest = linkage_get_link_num_domains(linkage, link);
}
for (link=0; link<N_links; link++)
{
assert (linkage_get_link_lword(linkage, link) < SIZE_MAX, "Missing word in link");
dname = linkage_get_link_domain_names(linkage, link);
for (j=0; j<linkage_get_link_num_domains(linkage, link); ++j) {
append_string(s, " (%s)", dname[j]);
}
for (; j<longest; j++) {
dyn_strcat(s, "    ");
}
dyn_strcat(s, "   ");
print_a_link(s, linkage, link);
}
dyn_strcat(s, "\n");
if (linkage_get_violation_name(linkage) != NULL) {
dyn_strcat(s, "P.P. violations:\n");
append_string(s, "        %s\n\n", linkage_get_violation_name(linkage));
}
return dyn_str_take(s);
}
char * linkage_print_disjuncts(const Linkage linkage)
{
const char * dj;
int w;
dyn_str * s = dyn_str_new();
int nwords = linkage->num_words;
for (w = 0; w < nwords; w++)
{
int pad = 21;
float cost;
const char *infword;
Disjunct *disj = linkage->chosen_disjuncts[w];
if (NULL == disj) continue;
infword = linkage->word[w];
pad += strlen(infword) - utf8_strwidth(infword);
dj = linkage_get_disjunct_str(linkage, w);
if (NULL == dj) dj = "";
cost = (float)linkage_get_disjunct_cost(linkage, w);
append_string(s, "%*s    %6s  %s\n",
pad, infword, cost_stringify(cost), dj);
}
return dyn_str_take(s);
}
static char *
build_linkage_postscript_string(const Linkage linkage,
bool display_walls, ps_ctxt_t *pctx)
{
bool print_word_0 = true, print_word_N = true;
int N_links = linkage->num_links;
Link *ppla = linkage->link_array;
if (!display_walls)
{
int N_wall_connectors = 0;
for (int j=0; j<N_links; j++)
{
if (0 == ppla[j].lw)
{
if (ppla[j].rw == linkage->num_words-1) continue;
if (easy_match(connector_string(ppla[j].lc), LEFT_WALL_SUPPRESS))
print_word_0 = false;
if (++N_wall_connectors > 1)
{
print_word_0 = true;
break;
}
}
}
N_wall_connectors = 0;
for (int j=0; j<N_links; j++)
{
if (ppla[j].rw == linkage->num_words-1)
{
if (easy_match(connector_string(ppla[j].lc), RIGHT_WALL_SUPPRESS))
print_word_N = false;
if (++N_wall_connectors > 1)
{
print_word_N = true;
break;
}
}
}
}
int N_words_to_print = linkage->num_words;
if (!print_word_N) N_words_to_print--;
dyn_str * string = dyn_str_new();
dyn_strcat(string, "[");
int doff = print_word_0 ? 0 : 1;
for (int j=doff; j<N_words_to_print; j++)
{
if ((j%10 == 0) && (j>doff)) dyn_strcat(string, "\n");
append_string(string, "(%s)", linkage->word[j]);
}
dyn_strcat(string,"]");
dyn_strcat(string,"\n");
dyn_strcat(string,"[");
int k = 0;
for (int link=0; link<N_links; link++) {
if (!print_word_0 && (ppla[link].lw == 0)) continue;
if (!print_word_N && (ppla[link].rw == linkage->num_words-1)) continue;
if ((k%7 == 0) && (k>0)) dyn_strcat(string,"\n");
k++;
append_string(string,"[%d %d %d",
ppla[link].lw - doff, ppla[link].rw - doff,
pctx->link_heights[link]);
append_string(string," (%s)]", ppla[link].link_name);
}
dyn_strcat(string,"]");
dyn_strcat(string,"\n");
dyn_strcat(string,"[");
for (int j=0; j < pctx->N_rows; j++)
{
if (j>0) append_string(string, " %d", pctx->row_starts[j]);
else append_string(string,"%d", pctx->row_starts[j]);
}
dyn_strcat(string,"]\n");
return dyn_str_take(string);
}
#define HEIGHT_INC 10
static void diagram_alloc_tmpmem(size_t **start, char ***pic, char ***xpic,
size_t *cur_height, size_t max_height,
size_t max_bytes, size_t num_cols)
{
assert(num_cols <= max_bytes, "Columns overflow");
assert(max_height > *cur_height, "New diagram height is too small");
*start = realloc(*start, max_height * sizeof(size_t));
*pic = realloc(*pic, max_height * sizeof(char *));
*xpic = realloc(*xpic, max_height * sizeof(char *));
for (size_t i = *cur_height; i < max_height; i++)
{
char *picmem = malloc(max_bytes * 2 + 2);
(*pic)[i] = picmem;
(*xpic)[i] = picmem + max_bytes;
memset((*pic)[i], ' ', num_cols);
(*pic)[i][num_cols] = '\0';
}
*cur_height = max_height;
}
typedef struct
{
Link *lnk;
unsigned int len;
} link_by_length;
static int by_link_len(const void *a, const void *b)
{
const link_by_length * const *la = a;
const link_by_length * const *lb = b;
return (*la)->len - (*lb)->len;
}
static void sort_link_lengths(Link *ppla, link_by_length *ll,
unsigned int N_links)
{
link_by_length *ll_tmp = alloca(N_links * sizeof(*ll_tmp));
link_by_length **lla = alloca(N_links * sizeof(*lla));
for (unsigned int j = 0; j < N_links; j++)
{
Link *lnk = &ppla[j];
assert(lnk->link_name != NULL, "Missing link name");
lla[j] = &ll_tmp[j];
ll_tmp[j].lnk = lnk;
ll_tmp[j].len = (unsigned int)(lnk->rw - lnk->lw);
}
qsort(lla, N_links, sizeof(*lla), by_link_len);
for (unsigned int j = 0; j < N_links; j++)
ll[j] = *lla[j];
}
static void diagram_free_tmpmem(size_t *start, char **pic, char **xpic,
size_t cur_height)
{
for (size_t i = 0; i < cur_height; i++)
free(pic[i]);
free(start);
free(pic);
free(xpic);
}
static char *
linkage_print_diagram_ctxt(const Linkage linkage,
bool display_walls,
size_t x_screen_width,
ps_ctxt_t *pctx)
{
if (x_screen_width <= 2)
{
prt_error("Error: Cannot print diagram: "
"Screen width %zu is too narrow.\n", x_screen_width);
return NULL;
}
bool display_short = true;
unsigned int i, j, k, cl, cr, inc, row, top_row, top_row_p1;
const char *s;
char *t;
bool print_word_0 = true, print_word_N = true;
int *center = alloca((linkage->num_words+1)*sizeof(int));
int *word_offset = alloca((linkage->num_words+1) * sizeof(*word_offset));
unsigned int N_links = linkage->num_links;
Link *ppla = linkage->link_array;
dyn_str * string;
unsigned int N_words_to_print;
if (0 == linkage->num_words) return strdup("");
link_by_length *ll = alloca(N_links * sizeof(*ll));
sort_link_lengths(ppla, ll, N_links);
string = dyn_str_new();
if (!display_walls)
{
int N_wall_connectors = 0;
for (j=0; j<N_links; j++)
{
if (0 == ppla[j].lw)
{
if (ppla[j].rw == linkage->num_words-1) continue;
if (easy_match(connector_string(ppla[j].lc), LEFT_WALL_SUPPRESS))
print_word_0 = false;
if (++N_wall_connectors > 1)
{
print_word_0 = true;
break;
}
}
}
N_wall_connectors = 0;
for (j=0; j<N_links; j++)
{
if (ppla[j].rw == linkage->num_words-1)
{
if (easy_match(connector_string(ppla[j].lc), RIGHT_WALL_SUPPRESS))
print_word_N = false;
if (++N_wall_connectors > 1)
{
print_word_N = true;
break;
}
}
}
}
N_words_to_print = linkage->num_words;
if (!print_word_N) N_words_to_print--;
size_t *start = NULL;
char **picture = NULL;
char **xpicture = NULL;
size_t max_height = 0;
size_t max_bytes = set_centers(linkage, center, word_offset,
print_word_0, N_words_to_print) +1;
unsigned int num_cols = center[N_words_to_print-1]+1;
if (max_bytes < num_cols) max_bytes = num_cols;
diagram_alloc_tmpmem(&start, &picture, &xpicture,
&max_height, HEIGHT_INC,
max_bytes, num_cols);
top_row = 0;
for (j=0; j<N_links; j++)
{
Link *lnk = ll[j].lnk;
if (!print_word_0 && (lnk->lw == 0)) continue;
if (!print_word_N && (lnk->rw == linkage->num_words-1)) continue;
cl = center[lnk->lw];
cr = center[lnk->rw];
for (row=0; row < max_height; row++)
{
for (k=cl+1; k<cr; k++)
{
if (picture[row][k] != ' ') break;
}
if (k == cr) break;
}
if (NULL != pctx)
{
pctx->link_heights[j] = row;
}
if (2*row+2 > max_height-1) {
lgdebug(+9, "Extending rows up to %u.\n", (2*row+2)+HEIGHT_INC);
diagram_alloc_tmpmem(&start, &picture, &xpicture,
&max_height, (2*row+2)+HEIGHT_INC,
max_bytes, num_cols);
}
if (row > top_row) top_row = row;
for (k=cl+1; k<cr; k++) {
picture[row][k] = '-';
}
s = lnk->link_name;
k = strlen(s);
inc = cl + cr + 2;
if (inc < k) inc = 0;
else inc = (inc-k)/2;
if (inc <= cl) {
t = picture[row] + cl + 1;
} else {
t = picture[row] + inc;
}
if (DEPT_CHR == connector_string(lnk->lc)[0] &&
(t > &picture[row][cl])) { picture[row][cl+1] = '<'; }
if (HEAD_CHR == connector_string(lnk->lc)[0]) { *(t-1) = '>'; }
while ((*s != '\0') && (*t == '-')) *t++ = *s++;
if (DEPT_CHR == connector_string(lnk->rc)[0]) { picture[row][cr-1] = '>'; }
if (HEAD_CHR == connector_string(lnk->rc)[0]) { *t = '<'; }
picture[row][cl] = '+';
picture[row][cr] = '+';
for (k=0; k<row; k++) {
if (picture[k][cl] == ' ') {
picture[k][cl] = '|';
}
if (picture[k][cr] == ' ') {
picture[k][cr] = '|';
}
}
}
t = xpicture[0];
if (print_word_0) k = 0; else k = 1;
for (; k<N_words_to_print; k++)
{
for (i = 0; i < (size_t) word_offset[k]; i++) *t++ = ' ';
s = linkage->word[k];
while (*s != '\0') { *t++ = *s++; }
*t++ = ' ';
}
*t = '\0';
if (display_short) {
for (k=0; picture[0][k] != '\0'; k++) {
if ((picture[0][k] == '+') || (picture[0][k] == '|')) {
xpicture[1][k] = '|';
} else {
xpicture[1][k] = ' ';
}
}
xpicture[1][k] = '\0';
for (row=0; row < top_row+1; row++) {
strcpy(xpicture[row+2], picture[row]);
}
top_row += 2;
} else {
for (row=0; row < top_row+1; row++) {
strcpy(xpicture[2*row+2],picture[row]);
for (k=0; picture[row][k] != '\0'; k++) {
if ((picture[row][k] == '+') || (picture[row][k] == '|')) {
xpicture[2*row+1][k] = '|';
} else {
xpicture[2*row+1][k] = ' ';
}
}
xpicture[2*row+1][k] = '\0';
}
top_row = 2*top_row + 2;
}
top_row_p1 = top_row + 1;
for (row = 0; row < top_row_p1; row++)
start[row] = 0;
if (NULL != pctx)
{
pctx->N_rows = 0;
pctx->row_starts[pctx->N_rows] = 0;
pctx->N_rows++;
}
if (print_word_0) i = 0; else i = 1;
unsigned int c = 0;
#define RIGHT_MARGIN 1
while (i < N_words_to_print)
{
unsigned int revrs;
unsigned int uwidth = 0;
unsigned int wwid;
do {
wwid = ((c == 0) * word_offset[i])
+ utf8_strwidth(linkage->word[i]+c) + 1;
if (x_screen_width - RIGHT_MARGIN < uwidth + wwid) break;
uwidth += wwid;
c = 0;
i++;
} while (i < N_words_to_print);
if (i < N_words_to_print) {
if (0 == uwidth)
{
uwidth = x_screen_width - RIGHT_MARGIN - (c == 0)*word_offset[i]-1;
c += utf8_chars_in_width(linkage->word[i]+c, uwidth);
}
if (NULL != pctx)
{
pctx->row_starts[pctx->N_rows] = i - (!print_word_0);
pctx->N_rows++;
}
}
dyn_strcat(string, "\n");
top_row_p1 = top_row + 1;
for (revrs = 0; revrs < top_row_p1; revrs++)
{
size_t glyph_width = 0;
row = top_row - revrs;
k = start[row];
for (j = k; (glyph_width < uwidth) && (xpicture[row][j] != '\0'); )
{
int gw = utf8_charwidth(&xpicture[row][j]);
if (gw < 0) gw = 2;
glyph_width += gw;
j += append_utf8_char(string, &xpicture[row][j]);
}
start[row] = j;
dyn_trimback(string);
dyn_strcat(string, "\n");
}
dyn_strcat(string, "\n");
}
diagram_free_tmpmem(start, picture, xpicture, max_height);
return dyn_str_take(string);
}
char * linkage_print_diagram(const Linkage linkage, bool display_walls, size_t screen_width)
{
if (!linkage) return NULL;
return linkage_print_diagram_ctxt(linkage, display_walls, screen_width, NULL);
}
void linkage_free_diagram(char * s)
{
if (NULL == s) return;
exfree(s, strlen(s)+1);
}
void linkage_free_disjuncts(char * s)
{
exfree(s, strlen(s)+1);
}
void linkage_free_links_and_domains(char * s)
{
exfree(s, strlen(s)+1);
}
static const char * trailer(bool print_ps_header);
static const char * header(bool print_ps_header);
char * linkage_print_postscript(const Linkage linkage, bool display_walls, bool print_ps_header)
{
char * ps, * qs, * ascii;
int size;
ps_ctxt_t ctx;
ctx.link_heights = (int *) alloca(linkage->num_links * sizeof(int));
ctx.row_starts = (int *) alloca((linkage->num_words + 1) * sizeof(int));
ascii = linkage_print_diagram_ctxt(linkage, display_walls, 8000, &ctx);
linkage_free_diagram(ascii);
ps = build_linkage_postscript_string(linkage, display_walls, &ctx);
size = strlen(header(print_ps_header)) + strlen(ps) + strlen(trailer(print_ps_header)) + 1;
qs = (char *) exalloc(sizeof(char)*size);
snprintf(qs, size, "%s%s%s", header(print_ps_header), ps, trailer(print_ps_header));
exfree(ps, strlen(ps)+1);
return qs;
}
void linkage_free_postscript(char * s)
{
exfree(s, strlen(s)+1);
}
char * linkage_print_pp_msgs(Linkage linkage)
{
if (linkage && linkage->lifo.pp_violation_msg)
return strdup(linkage->lifo.pp_violation_msg);
return strdup("");
}
void linkage_free_pp_msgs(char * s)
{
exfree(s, strlen(s)+1);
}
void print_disjunct_counts(Sentence sent)
{
size_t i;
int dcnt;
int t = 0;
int lcnt = 0, rcnt = 0;
for (i=0; i<sent->length; i++)
{
Disjunct *d = sent->word[i].d;
dcnt = count_disjuncts(d);
rcnt += right_connector_count(d);
lcnt += left_connector_count(d);
t += dcnt;
prt_error("%s(%d) ", sent->word[i].alternatives[0], dcnt);
}
prt_error("\n\\");
prt_error("Total: %d disjuncts, %d (%d+/%d-) connectors\n\n",
t, rcnt+lcnt, rcnt, lcnt);
}
static const char * trailer(bool print_ps_header)
{
static const char * trailer_string=
"diagram\n"
"\n"
"%%EndDocument\n"
;
if (print_ps_header) return trailer_string;
else return "";
}
static const char * header(bool print_ps_header)
{
static const char * header_string=
"%!PS-Adobe-2.0 EPSF-1.2\n"
"%%Pages: 1\n"
"%%BoundingBox: 0 -20 500 200\n"
"%%EndComments\n"
"%%BeginDocument: \n"
"\n"
"% compute size of diagram by adding\n"
"% #rows x 8.5\n"
"% (#rows -1) x 10\n"
"% \\sum maxheight x 10\n"
"/nulllink () def                     % The symbol of a null link\n"
"/wordfontsize 11 def      % the size of the word font\n"
"/labelfontsize 9 def      % the size of the connector label font\n"
"/ex 10 def  % the horizontal radius of all the links\n"
"/ey 10 def  % the height of the level 0 links\n"
"/ed 10 def  % amount to add to this height per level\n"
"/radius 10 def % radius for rounded arcs\n"
"/row-spacing 10 def % the space between successive rows of the diagram\n"
"\n"
"/gap wordfontsize .5 mul def  % the gap between words\n"
"/top-of-words wordfontsize .85 mul def\n"
"             % the delta y above where the text is written where\n"
"             % the major axis of the ellipse is located\n"
"/label-gap labelfontsize .1 mul def\n"
"\n"
"/xwordfontsize 10 def      % the size of the word font\n"
"/xlabelfontsize 10 def      % the size of the connector label font\n"
"/xex 10 def  % the horizontal radius of all the links\n"
"/xey 10 def  % the height of the level 0 links\n"
"/xed 10 def  % amount to add to this height per level\n"
"/xradius 10 def % radius for rounded arcs\n"
"/xrow-spacing 10 def % the space between successive rows of the diagram\n"
"/xgap wordfontsize .5 mul def  % the gap between words\n"
"\n"
"/centerpage 6.5 72 mul 2 div def\n"
"  % this number of points from the left margin is the center of page\n"
"\n"
"/rightpage 6.5 72 mul def\n"
"  % number of points from the left margin is the right margin\n"
"\n"
"/show-string-centered-dict 5 dict def\n"
"\n"
"/show-string-centered {\n"
"  show-string-centered-dict begin\n"
"  /string exch def\n"
"  /ycenter exch def\n"
"  /xcenter exch def\n"
"  xcenter string stringwidth pop 2 div sub\n"
"  ycenter labelfontsize .3 mul sub\n"
"  moveto\n"
"  string show\n"
"  end\n"
"} def\n"
"\n"
"/clear-word-box {\n"
"  show-string-centered-dict begin\n"
"  /string exch def\n"
"  /ycenter exch def\n"
"  /xcenter exch def\n"
"  newpath\n"
"  /urx string stringwidth pop 2 div def\n"
"  /ury labelfontsize .3 mul def\n"
"  xcenter urx sub ycenter ury sub moveto\n"
"  xcenter urx add ycenter ury sub lineto\n"
"  xcenter urx add ycenter ury add lineto\n"
"  xcenter urx sub ycenter ury add lineto\n"
"  closepath\n"
"  1 setgray fill\n"
"  0 setgray\n"
"  end\n"
"} def\n"
"\n"
"/diagram-sentence-dict 20 dict def\n"
"\n"
"/diagram-sentence-circle\n"
"{diagram-sentence-dict begin  \n"
"   /links exch def\n"
"   /words exch def\n"
"   /n words length def\n"
"   /Times-Roman findfont wordfontsize scalefont setfont\n"
"   /x 0 def\n"
"   /y 0 def\n"
"\n"
"   /left-ends [x dup words {stringwidth pop add gap add dup}\n"
"	                     forall pop pop] def\n"
"   /right-ends [x words {stringwidth pop add dup gap add} forall pop] def\n"
"   /centers [0 1 n 1 sub {/i exch def\n"
"		      left-ends i get\n"
"		      right-ends i get\n"
"		      add 2 div\n"
"		    } for ] def\n"
"\n"
"   x y moveto\n"
"   words {show gap 0 rmoveto} forall\n"
"\n"
"   .5 setlinewidth \n"
"\n"
"   links {dup 0 get /leftword exch def\n"
"          dup 1 get /rightword exch def\n"
"          dup 2 get /level exch def\n"
"          3 get /string exch def\n"
"          newpath\n"
"          string nulllink eq {[2] 1 setdash}{[] 0 setdash} ifelse\n"
"%          string nulllink eq {.8 setgray}{0 setgray} ifelse\n"
"          centers leftword get\n"
"	  y top-of-words add\n"
"          moveto\n"
"      \n"
"          centers rightword get\n"
"          centers leftword get\n"
"          sub 2  div dup\n"
"          radius \n"
"          lt {/radiusx exch def}{pop /radiusx radius def} ifelse\n"
"  \n"
"          \n"
" \n"
"          centers leftword get\n"
"	  y top-of-words add ey ed level mul add add\n"
"          centers rightword get\n"
"	  y top-of-words add ey ed level mul add add\n"
"	  radiusx\n"
"          arcto\n"
"          4 {pop} repeat\n"
"	  centers rightword get\n"
"          y top-of-words add ey ed level mul add add\n"
"	  centers rightword get\n"
"	  y top-of-words add\n"
"	  radiusx\n"
"	  arcto\n"
"          4 {pop} repeat\n"
"	  centers rightword get\n"
"	  y top-of-words add\n"
"	  lineto\n"
"\n"
"	  stroke\n"
"\n"
"          /radius-y    ey ed level mul add	  def\n"
"\n"
"	  /center-arc-x\n"
"	     centers leftword get centers rightword get add 2 div\n"
"	  def\n"
"	  \n"
"          /center-arc-y\n"
"             y top-of-words radius-y add add\n"
"	  def\n"
"\n"
"          /Courier-Bold findfont labelfontsize scalefont setfont \n"
"	  center-arc-x center-arc-y string clear-word-box\n"
"	  center-arc-x center-arc-y string show-string-centered\n"
"          } forall\n"
"	  end\n"
"  } def\n"
"\n"
"/diagramdict 20 dict def\n"
"\n"
"/diagram\n"
"{diagramdict begin\n"
"   /break-words exch def\n"
"   /links exch def\n"
"   /words exch def\n"
"   /n words length def\n"
"   /n-rows break-words length def\n"
"   /Times-Roman findfont wordfontsize scalefont setfont\n"
"\n"
"   /left-ends [0 dup words {stringwidth pop add gap add dup}\n"
"	                     forall pop pop] def\n"
"   /right-ends [0 words {stringwidth pop add dup gap add} forall pop] def\n"
"\n"
"   /lwindows [ break-words {left-ends exch get gap 2 div sub } forall ] def\n"
"   /rwindows [1 1 n-rows 1 sub {/i exch def\n"
"		      lwindows i get } for\n"
"	              right-ends n 1 sub get gap 2 div add\n"
"	      ] def\n"
"\n"
"\n"
"    /max 0 def\n"
"    0 1 links length 1 sub {\n"
"	/i exch def\n"
"	/t links i get 2 get def\n"
"	t max gt {/max t def} if\n"
"      } for\n"
"\n"
"    /max-height ed max mul ey add top-of-words add row-spacing add def\n"
"    /total-height n-rows max-height mul row-spacing sub def\n"
"\n"
"    /max-width 0 def            % compute the widest window\n"
"    0 1 n-rows 1 sub {\n"
"        /i exch def\n"
"        /t rwindows i get lwindows i get sub def\n"
"        t max-width gt {/max-width t def} if\n"
"      } for\n"
"\n"
"    centerpage max-width 2 div sub 0 translate  % centers it\n"
"   % rightpage max-width sub 0 translate      % right justified\n"
"                        % Delete both of these to make it left justified\n"
"\n"
"   n-rows 1 sub -1 0\n"
"     {/i exch def\n"
"	gsave\n"
"	newpath\n"
"        %/centering centerpage rwindows i get lwindows i get sub 2 div sub def\n"
"               % this line causes each row to be centered\n"
"        /centering 0 def\n"
"               % set centering to 0 to prevent centering of each row \n"
"\n"
"	centering -100 moveto  % -100 because some letters go below zero\n"
"        centering max-height n-rows mul lineto\n"
"        rwindows i get lwindows i get sub centering add\n"
"                       max-height n-rows mul lineto\n"
"        rwindows i get lwindows i get sub centering add\n"
"                       -100 lineto\n"
"	closepath\n"
"        clip\n"
"	lwindows i get neg n-rows i sub 1 sub max-height mul translate\n"
"        centerpage centering 0 translate\n"
"        words links diagram-sentence-circle\n"
"	grestore\n"
"     } for\n"
"     end\n"
"} def \n"
"\n"
"/diagramx\n"
"{diagramdict begin\n"
"   /break-words exch def\n"
"   /links exch def\n"
"   /words exch def\n"
"   /n words length def\n"
"   /n-rows break-words length def\n"
"   /Times-Roman findfont xwordfontsize scalefont setfont\n"
"\n"
"   /left-ends [0 dup words {stringwidth pop add gap add dup}\n"
"	                     forall pop pop] def\n"
"   /right-ends [0 words {stringwidth pop add dup gap add} forall pop] def\n"
"\n"
"   /lwindows [ break-words {left-ends exch get gap 2 div sub } forall ] def\n"
"   /rwindows [1 1 n-rows 1 sub {/i exch def\n"
"		      lwindows i get } for\n"
"	              right-ends n 1 sub get xgap 2 div add\n"
"	      ] def\n"
"\n"
"\n"
"    /max 0 def\n"
"    0 1 links length 1 sub {\n"
"	/i exch def\n"
"	/t links i get 2 get def\n"
"	t max gt {/max t def} if\n"
"      } for\n"
"\n"
"    /max-height xed max mul xey add top-of-words add xrow-spacing add def\n"
"    /total-height n-rows max-height mul xrow-spacing sub def\n"
"\n"
"    /max-width 0 def            % compute the widest window\n"
"    0 1 n-rows 1 sub {\n"
"        /i exch def\n"
"        /t rwindows i get lwindows i get sub def\n"
"        t max-width gt {/max-width t def} if\n"
"      } for\n"
"\n"
"    centerpage max-width 2 div sub 0 translate  % centers it\n"
"   % rightpage max-width sub 0 translate      % right justified\n"
"                        % Delete both of these to make it left justified\n"
"\n"
"   n-rows 1 sub -1 0\n"
"     {/i exch def\n"
"	gsave\n"
"	newpath\n"
"        %/centering centerpage rwindows i get lwindows i get sub 2 div sub def\n"
"               % this line causes each row to be centered\n"
"        /centering 0 def\n"
"               % set centering to 0 to prevent centering of each row \n"
"\n"
"	centering -100 moveto  % -100 because some letters go below zero\n"
"        centering max-height n-rows mul lineto\n"
"        rwindows i get lwindows i get sub centering add\n"
"                       max-height n-rows mul lineto\n"
"        rwindows i get lwindows i get sub centering add\n"
"                       -100 lineto\n"
"	closepath\n"
"        clip\n"
"	lwindows i get neg n-rows i sub 1 sub max-height mul translate\n"
"        centerpage centering 0 translate\n"
"        words links diagram-sentence-circle\n"
"	grestore\n"
"     } for\n"
"     end\n"
"} def \n"
"\n"
"/ldiagram\n"
"{diagramdict begin\n"
"   /break-words exch def\n"
"   /links exch def\n"
"   /words exch def\n"
"   /n words length def\n"
"   /n-rows break-words length def\n"
"   /Times-Roman findfont wordfontsize scalefont setfont\n"
"\n"
"   /left-ends [0 dup words {stringwidth pop add gap add dup}\n"
"	                     forall pop pop] def\n"
"   /right-ends [0 words {stringwidth pop add dup gap add} forall pop] def\n"
"\n"
"   /lwindows [ break-words {left-ends exch get gap 2 div sub } forall ] def\n"
"   /rwindows [1 1 n-rows 1 sub {/i exch def\n"
"		      lwindows i get } for\n"
"	              right-ends n 1 sub get gap 2 div add\n"
"	      ] def\n"
"\n"
"\n"
"    /max 0 def\n"
"    0 1 links length 1 sub {\n"
"	/i exch def\n"
"	/t links i get 2 get def\n"
"	t max gt {/max t def} if\n"
"      } for\n"
"\n"
"    /max-height ed max mul ey add top-of-words add row-spacing add def\n"
"    /total-height n-rows max-height mul row-spacing sub def\n"
"\n"
"    /max-width 0 def            % compute the widest window\n"
"    0 1 n-rows 1 sub {\n"
"        /i exch def\n"
"        /t rwindows i get lwindows i get sub def\n"
"        t max-width gt {/max-width t def} if\n"
"      } for\n"
"\n"
"   % centerpage max-width 2 div sub 0 translate  % centers it\n"
"   % rightpage max-width sub 0 translate      % right justified\n"
"                        % Delete both of these to make it left justified\n"
"\n"
"   n-rows 1 sub -1 0\n"
"     {/i exch def\n"
"	gsave\n"
"	newpath\n"
"        %/centering centerpage rwindows i get lwindows i get sub 2 div sub def\n"
"               % this line causes each row to be centered\n"
"        /centering 0 def\n"
"               % set centering to 0 to prevent centering of each row \n"
"\n"
"	centering -100 moveto  % -100 because some letters go below zero\n"
"        centering max-height n-rows mul lineto\n"
"        rwindows i get lwindows i get sub centering add\n"
"                       max-height n-rows mul lineto\n"
"        rwindows i get lwindows i get sub centering add\n"
"                       -100 lineto\n"
"	closepath\n"
"        clip\n"
"	lwindows i get neg n-rows i sub 1 sub max-height mul translate\n"
"        centerpage centering 0 translate\n"
"        words links diagram-sentence-circle\n"
"	grestore\n"
"     } for\n"
"     end\n"
"} def \n"
;
if (print_ps_header) return header_string;
else return "";
}
struct tokenpos
{
const char * token;
size_t wi;
size_t ai;
};
void print_sentence_word_alternatives(dyn_str *s, Sentence sent,
bool debugprint,
char * (*display)(Dictionary, const char *, const void **),
const void **arg, struct tokenpos *tokenpos)
{
size_t wi;
size_t ai;
size_t sentlen = sent->length;
size_t first_sentence_word = 0;
bool word_split = false;
Dictionary dict = sent->dict;
if (0 == sentlen)
{
prt_error("Error: Sentence length is 0 (reason unknown)\n");
return;
}
if (debugprint || (NULL != tokenpos))
;
else
{
if (dict->left_wall_defined &&
((NULL != sent->word[0].alternatives[0])) &&
(0 == strcmp(sent->word[0].alternatives[0], LEFT_WALL_WORD)))
first_sentence_word = 1;
if (dict->right_wall_defined &&
((NULL != sent->word[sentlen-1].alternatives[0])) &&
(0 == strcmp(sent->word[sentlen-1].alternatives[0], RIGHT_WALL_WORD)))
sentlen--;
if (sentlen - first_sentence_word > 1)
{
word_split = true;
}
else
{
for (wi=first_sentence_word; wi<sentlen; wi++)
{
Word w = sent->word[wi];
assert((NULL != w.alternatives) && (NULL != w.alternatives[0]) &&
('\0' != w.alternatives[0][0]), "Missing alt for word %zu", wi);
if (NULL != w.alternatives[1])
{
word_split = true;
break;
}
}
}
if (word_split && (NULL == display)) dyn_strcat(s, "String splits to:\n");
}
for (wi=first_sentence_word; wi<sentlen; wi++)
{
Word w = sent->word[wi];
size_t w_start = wi;
size_t max_nalt = 0;
#if 0
if (debugprint) lgdebug(0, "  word%d %c%c: %s\n   ",
wi, w.firstupper ? 'C' : ' ', sent->post_quote[wi] ? 'Q' : ' ',
#endif
if (debugprint) append_string(s, "  word%zu: %s\n", wi, w.unsplit_word);
assert((NULL != w.alternatives) && (NULL != w.alternatives[0]) &&
('\0' != w.alternatives[0][0]), "Missing alt for word %zu", wi);
for (wi = w_start; (wi == w_start) ||
((wi < sentlen) && (! sent->word[wi].unsplit_word)); wi++)
{
size_t nalt = altlen(sent->word[wi].alternatives);
max_nalt = MAX(max_nalt, nalt);
}
for (ai=0; ai < max_nalt; ai++)
{
if (debugprint)
{
if (0 < ai) dyn_strcat(s, "\n");
append_string(s, "   alt%zu:", ai);
}
int missing = 0;
for (wi = w_start; (wi == w_start) ||
((wi < sentlen) && (! sent->word[wi].unsplit_word)); wi++)
{
size_t nalts = altlen(sent->word[wi].alternatives);
const char *wt;
if (ai >= nalts)
{
missing++;
continue;
}
wt = sent->word[wi].alternatives[ai];
if ((NULL != tokenpos) && (0 == strcmp(tokenpos->token, wt)))
{
tokenpos->wi = wi;
tokenpos->ai = ai;
return;
}
if (!debugprint)
{
struct tokenpos firstpos = { wt };
print_sentence_word_alternatives(s, sent, false, NULL, NULL, &firstpos);
if (((firstpos.wi != wi) || (firstpos.ai != ai)) &&
firstpos.wi >= first_sentence_word)
{
if (NULL != display)
lgdebug(6, "Skipping repeated %s\n", wt);
continue;
}
}
if (debugprint)
{
while (missing-- > 0) append_string(s, " %s", "[missing]");
const char *opt_start = "", *opt_end = "";
if (sent->word[wi].optional)
{
opt_start = "{";
opt_end = "}";
}
if ('\0' == wt[0]) wt = "'\\0'";
append_string(s, " %s%s%s", opt_start, wt, opt_end);
}
if ((NULL == tokenpos) && (NULL != display))
{
char *info = display(sent->dict, wt, arg);
if (NULL == info) return;
append_string(s, "Token \"%s\" ", wt);
dyn_strcat(s, info);
free(info);
}
else if (word_split) append_string(s, " %s", wt);
}
}
wi--;
if (word_split && (NULL == display)) dyn_strcat(s, "\n\n");
if (debugprint) dyn_strcat(s, "\n");
}
if (debugprint) dyn_strcat(s, "\n");
}
void print_sentence_context(Sentence sent, dyn_str *outbuf)
{
size_t i, j;
const char **a, **b;
dyn_strcat(outbuf,
"\tFailing sentence contains the following words/morphemes:\n\t");
for (i=0; i<sent->length; i++)
{
for (a = sent->word[i].alternatives; NULL != *a; a++)
{
bool next_word = false;
for (j=0; j<sent->length; j++)
{
for (b = sent->word[j].alternatives; NULL != *b; b++)
{
if (0 == strcmp(*a, *b))
{
next_word = true;
if (a != b) break;
dyn_strcat(outbuf, *a);
dyn_strcat(outbuf, " ");
break;
}
}
if (next_word) break;
}
}
}
dyn_strcat(outbuf, "\n");
}