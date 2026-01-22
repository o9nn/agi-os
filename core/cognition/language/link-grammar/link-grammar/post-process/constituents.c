#include <stdint.h>
#include <string.h>
#include "api-structures.h"
#include "dict-common/dict-common.h"
#include "dict-common/dict-defines.h"
#include "error.h"
#include "linkage/linkage.h"
#include "post-process/post-process.h"
#include "post-process/pp-structures.h"
#include "string-set.h"
#include "utilities.h"
#define D_CONST 8
#define OPEN_BRACKET '['
#define CLOSE_BRACKET ']'
typedef enum {OPEN_TOK, CLOSE_TOK, WORD_TOK} CType;
typedef enum {NONE, STYPE, PTYPE, QTYPE, QDTYPE} WType;
typedef struct
{
const char * type;
const char * start_link;
size_t left;
size_t right;
int canon;
bool valid;
char domain_type;
} constituent_t;
typedef struct
{
String_set * phrase_ss;
WType * wordtype;
constituent_t * constituent;
int conlen;
} con_context_t;
typedef struct CNode_s CNode;
struct CNode_s
{
char * label;
CNode * child;
CNode * next;
int start, end;
};
static bool uppercompare(const char * s, const char * t)
{
while (is_connector_name_char(*s) || is_connector_name_char(*t))
{
if (*s++ != *t++) return false;
}
return true;
}
static void adjust_for_left_comma(con_context_t * ctxt, Linkage linkage, int c)
{
int w;
w = ctxt->constituent[c].left;
if (strcmp(linkage->word[w], ",") == 0)
w++;
ctxt->constituent[c].left = w;
}
static void adjust_for_right_comma(con_context_t *ctxt, Linkage linkage, int c)
{
int w;
w = ctxt->constituent[c].right;
if ((strcmp(linkage->word[w], ",") == 0) ||
(strcmp(linkage->word[w], RIGHT_WALL_WORD) == 0))
{
w--;
}
ctxt->constituent[c].right = w;
}
static void print_constituent(con_context_t *ctxt, Linkage linkage, int c)
{
size_t w;
err_msg(lg_Debug, "  c %2d %4s [%c] (%2zu-%2zu): ",
c, ctxt->constituent[c].type, ctxt->constituent[c].domain_type,
ctxt->constituent[c].left, ctxt->constituent[c].right);
for (w = ctxt->constituent[c].left; w <= ctxt->constituent[c].right; w++) {
err_msg(lg_Debug, "%s ", linkage->word[w]);
}
err_msg(lg_Debug, "\n");
}
typedef enum
{
CASE_S=1,
CASE_UNUSED=2,
CASE_REL_CLAUSE=3,
CASE_APPOS=4,
CASE_OPENER=5,
CASE_PPOPEN=6,
CASE_SVINV=7,
CASE_PART_MOD=8,
CASE_PART_OPEN=9,
} case_type;
static int gen_comp(con_context_t *ctxt, Linkage linkage,
int numcon_total, int numcon_subl,
const char * ctype1, const char * ctype2,
const char * ctype3, case_type x)
{
size_t w, w2, w3;
int c, c1, c2;
bool done;
c = numcon_total + numcon_subl;
for (c1=numcon_total; c1<numcon_total + numcon_subl; c1++)
{
if ((x==CASE_APPOS) && (post_process_match("MX#*", ctxt->constituent[c1].start_link)==0))
continue;
#ifdef REVIVE_DEAD_CODE
if ((x==CASE_UNUSED) && (ctxt->constituent[c1].domain_type=='t'))
continue;
#endif
if (ctxt->constituent[c1].domain_type=='z')
continue;
#ifdef REVIVE_DEAD_CODE
#endif
if ((x==CASE_S) &&
(((post_process_match("S", ctxt->constituent[c1].start_link) == 0) &&
(post_process_match("SX", ctxt->constituent[c1].start_link) == 0) &&
(post_process_match("SF", ctxt->constituent[c1].start_link) == 0)) ||
(post_process_match("S##w", ctxt->constituent[c1].start_link) != 0)))
continue;
if ((x==CASE_REL_CLAUSE) &&
((post_process_match("Rn", ctxt->constituent[c1].start_link) == 0) &&
(post_process_match("R*", ctxt->constituent[c1].start_link) == 0) &&
(post_process_match("MX#r", ctxt->constituent[c1].start_link) == 0) &&
(post_process_match("Mr", ctxt->constituent[c1].start_link) == 0) &&
(post_process_match("MX#d", ctxt->constituent[c1].start_link) == 0)))
continue;
if ((x==CASE_OPENER) && (ctxt->constituent[c1].domain_type!='f'))
continue;
if ((x==CASE_PPOPEN) && (ctxt->constituent[c1].domain_type!='g'))
continue;
if ((x==CASE_SVINV) && (post_process_match("SI", ctxt->constituent[c1].start_link)==0))
continue;
if ((x==CASE_PART_MOD) && (post_process_match("M", ctxt->constituent[c1].start_link)==0))
continue;
if ((x==CASE_PART_OPEN) && (post_process_match("COp", ctxt->constituent[c1].start_link)==0))
continue;
if (!(strcmp(ctxt->constituent[c1].type, ctype1)==0))
continue;
if (verbosity_level(D_CONST))
err_msg(lg_Debug, "Generating complement constituent for c %d of type %s\n\\",
c1, ctype1);
done = false;
for (w2 = ctxt->constituent[c1].left; (done == false) && (w2 != (size_t)-1); w2--)
{
for (w3 = ctxt->constituent[c1].right; w3<linkage->num_words; w3++)
{
for (c2 = numcon_total; (done == false) &&
(c2 < numcon_total + numcon_subl); c2++) {
if (!((ctxt->constituent[c2].left == w2) &&
(ctxt->constituent[c2].right == w3)) || (c2==c1))
continue;
if (!(strcmp(ctxt->constituent[c2].type, ctype2)==0))
continue;
if ((x==CASE_OPENER) || (x==CASE_PPOPEN) || (x==CASE_PART_OPEN))
{
w = ctxt->constituent[c1].right + 1;
if (w > ctxt->constituent[c2].right)
{
done = true;
continue;
}
ctxt->constituent[c].left = w;
ctxt->constituent[c].right = ctxt->constituent[c2].right;
}
else
{
w = ctxt->constituent[c1].left - 1;
if (w < ctxt->constituent[c2].left) {
done = true;
continue;
}
ctxt->constituent[c].right = w;
ctxt->constituent[c].left = ctxt->constituent[c2].left;
}
adjust_for_left_comma(ctxt, linkage, c1);
adjust_for_right_comma(ctxt, linkage, c1);
ctxt->constituent[c].type =
string_set_add(ctype3, ctxt->phrase_ss);
ctxt->constituent[c].domain_type = 'x';
ctxt->constituent[c].start_link =
string_set_add("XX", ctxt->phrase_ss);
if (verbosity_level(D_CONST))
{
err_msg(lg_Debug, "Larger c found: c %d (%s); ", c2, ctype2);
err_msg(lg_Debug, "Adding constituent:\n\\");
print_constituent(ctxt, linkage, c);
}
c++;
assert (c < ctxt->conlen, "Too many constituents");
done = true;
}
}
}
if (verbosity_level(D_CONST))
{
if (done == false)
err_msg(lg_Debug, "No constituent added, because no larger %s"
" was found\n", ctype2);
else
lg_error_flush();
}
}
numcon_subl = c - numcon_total;
return numcon_subl;
}
static void adjust_subordinate_clauses(con_context_t *ctxt, Linkage linkage,
int numcon_total,
int numcon_subl)
{
int c, c2;
size_t w, w2;
bool done;
for (c=numcon_total; c<numcon_total + numcon_subl; c++)
{
if ((post_process_match("MVs", ctxt->constituent[c].start_link) == 1) ||
(post_process_match("MVg", ctxt->constituent[c].start_link) == 1))
{
done = false;
for (w2 = ctxt->constituent[c].left-1; (false == done) && w2 != (size_t) -1; w2--)
{
for (c2 = numcon_total; c2 < numcon_total + numcon_subl; c2++)
{
if (!((ctxt->constituent[c2].left == w2) &&
(ctxt->constituent[c2].right >= ctxt->constituent[c].right)))
continue;
if ((strcmp(ctxt->constituent[c2].type, "S") == 0) ||
(strcmp(ctxt->constituent[c2].type, "NP") == 0)) {
done = true;
break;
}
if ((ctxt->constituent[c2].domain_type == 'v') ||
(ctxt->constituent[c2].domain_type == 'a'))
{
w = ctxt->constituent[c].left - 1;
ctxt->constituent[c2].right = w;
if (verbosity_level(D_CONST))
{
err_msg(lg_Debug, "Adjusting constituent %d:\n\\", c2);
print_constituent(ctxt, linkage, c2);
}
}
}
}
if (strcmp(linkage->word[ctxt->constituent[c].left], ",") == 0)
ctxt->constituent[c].left++;
}
}
}
static int merge_constituents(con_context_t *ctxt, Linkage linkage, int numcon_total)
{
int c1, c2=0;
for (c1 = 0; c1 < numcon_total; c1++)
{
ctxt->constituent[c1].valid = true;
ctxt->constituent[c1].canon = c1;
for (c2 = c1 + 1; c2 < numcon_total; c2++)
{
if ((ctxt->constituent[c1].left == ctxt->constituent[c2].left) &&
(ctxt->constituent[c1].right == ctxt->constituent[c2].right) &&
(strcmp(ctxt->constituent[c1].type, ctxt->constituent[c2].type) == 0))
{
ctxt->constituent[c2].canon = c1;
}
}
}
for (c1 = 0; c1 < numcon_total; c1++)
{
for (c2 = c1 + 1; c2 < numcon_total; c2++)
{
if (ctxt->constituent[c2].canon == ctxt->constituent[c1].canon)
ctxt->constituent[c2].valid = false;
}
}
return numcon_total;
}
static void generate_misc_word_info(con_context_t * ctxt, Linkage linkage)
{
size_t w1, w2, l1, l2;
const char * label1, * label2;
for (w1 = 0; w1 < linkage->num_words; w1++)
ctxt->wordtype[w1] = NONE;
for (l1 = 0; l1 < linkage_get_num_links(linkage); l1++) {
w1=linkage_get_link_rword(linkage, l1);
label1 = linkage_get_link_label(linkage, l1);
if ((uppercompare(label1, "S")) ||
(uppercompare(label1, "SX")) ||
(uppercompare(label1, "SF"))) {
ctxt->wordtype[w1] = STYPE;
for (l2 = 0; l2 < linkage_get_num_links(linkage); l2++) {
w2=linkage_get_link_lword(linkage, l2);
label2 = linkage_get_link_label(linkage, l2);
if ((w1 == w2) &&
((post_process_match("Pg#b", label2)==1) ||
(uppercompare(label2, "I")) ||
(uppercompare(label2, "PP")) ||
(post_process_match("Pv", label2)==1))) {
ctxt->wordtype[w1] = PTYPE;
}
}
}
if (post_process_match("QI#d", label1)==1) {
ctxt->wordtype[w1] = QTYPE;
for (l2 = 0; l2 < linkage_get_num_links(linkage); l2++) {
w2 = linkage_get_link_lword(linkage, l2);
label2 = linkage_get_link_label(linkage, l2);
if ((w1 == w2) && (post_process_match("D##w", label2)==1)) {
ctxt->wordtype[w1] = QDTYPE;
}
}
}
if (post_process_match("Mr", label1)==1) ctxt->wordtype[w1] = QDTYPE;
if (post_process_match("MX#d", label1)==1) ctxt->wordtype[w1] = QDTYPE;
}
}
static int new_style_conjunctions(con_context_t *ctxt, Linkage linkage, int numcon_total)
{
#ifdef DEBUG
int c;
for (c = 0; c < numcon_total; c++)
{
constituent_t *ct = &ctxt->constituent[c];
lgdebug(6, "ola %d valid=%d %s start=%s lr=%zu %zu\n", c,
ct->valid, ct->type, ct->start_link, ct->left, ct->right);
}
#endif
return numcon_total;
}
static int last_minute_fixes(con_context_t *ctxt, Linkage linkage, int numcon_total)
{
int c;
bool global_leftend_found, global_rightend_found;
size_t lastword;
for (c = 0; c < numcon_total; c++)
{
if (uppercompare(ctxt->constituent[c].start_link, "CP"))
{
ctxt->constituent[c].valid = false;
}
if ((uppercompare(ctxt->constituent[c].start_link, "YS")) ||
(uppercompare(ctxt->constituent[c].start_link, "YP")))
{
ctxt->constituent[c].right++;
}
if (strcmp(ctxt->constituent[c].start_link, "MVpn") == 0)
{
ctxt->constituent[c].type = string_set_add("NP", ctxt->phrase_ss);
}
if (strcmp(ctxt->constituent[c].start_link, "COn") == 0)
{
ctxt->constituent[c].type = string_set_add("NP", ctxt->phrase_ss);
}
if (strcmp(ctxt->constituent[c].start_link, "Mpn") == 0)
{
ctxt->constituent[c].type = string_set_add("NP", ctxt->phrase_ss);
}
if ((strcmp(ctxt->constituent[c].start_link, "Wdc") == 0) &&
(ctxt->constituent[c].left == 2))
{
ctxt->constituent[c].valid = false;
}
if ((post_process_match("A", ctxt->constituent[c].start_link) == 1) ||
(ctxt->constituent[c].domain_type == 'd') ||
(ctxt->constituent[c].domain_type == 'h')) {
if (ctxt->constituent[c].right-ctxt->constituent[c].left == 0)
{
ctxt->constituent[c].valid = false;
}
}
if ((ctxt->constituent[c].domain_type == 'h') &&
(strcmp(linkage->word[ctxt->constituent[c].left - 1], "$") == 0))
{
ctxt->constituent[c].left--;
}
}
for (c = 0; c < numcon_total; c++)
{
if ((ctxt->constituent[c].right == linkage->num_words - 3) &&
(ctxt->constituent[c].left == 1) &&
(strcmp(ctxt->constituent[c].type, "S") == 0))
{
size_t ln;
for (ln = 0; ln < linkage->num_links; ln++)
{
if ((linkage->link_array[ln].lw == linkage->num_words - 2) &&
(linkage->link_array[ln].rw == linkage->num_words - 1))
{
ctxt->constituent[c].right++;
break;
}
}
}
}
lastword = linkage->num_words - 2;
global_leftend_found = false;
global_rightend_found = false;
for (c = 0; c < numcon_total; c++)
{
if ((ctxt->constituent[c].left == 1) &&
(strcmp(ctxt->constituent[c].type, "S") == 0) &&
ctxt->constituent[c].valid)
{
global_leftend_found = true;
}
}
for (c = 0; c < numcon_total; c++)
{
if ((ctxt->constituent[c].right >= lastword) &&
(strcmp(ctxt->constituent[c].type, "S") == 0) &&
ctxt->constituent[c].valid)
{
global_rightend_found = true;
}
}
if ((global_leftend_found == false) || (global_rightend_found == false))
{
c = numcon_total;
ctxt->constituent[c].left = 1;
ctxt->constituent[c].right = linkage->num_words-1;
ctxt->constituent[c].type = string_set_add("S", ctxt->phrase_ss);
ctxt->constituent[c].valid = true;
ctxt->constituent[c].domain_type = 'x';
numcon_total++;
if (verbosity_level(D_CONST))
{
err_msg(lg_Debug, "Adding global sentence constituent:\n\\");
print_constituent(ctxt, linkage, c);
}
}
return numcon_total;
}
static int add_constituent(con_context_t *ctxt, int c, const Linkage linkage,
const Domain *domain,
int l, int r, const char * name)
{
int nwords = linkage->num_words-2;
c++;
if (l < 1) l=1;
if (r > nwords) r = nwords;
if (l > nwords) l = nwords;
assert(l <= r, "negative constituent length!");
ctxt->constituent[c].type = string_set_add(name, ctxt->phrase_ss);
ctxt->constituent[c].left = l;
ctxt->constituent[c].right = r;
ctxt->constituent[c].domain_type = domain->type;
ctxt->constituent[c].start_link =
linkage_get_link_label(linkage, domain->start_link);
return c;
}
static const char * cons_of_domain(const Linkage linkage, char domain_type)
{
switch (domain_type) {
case 'a':
return "ADJP";
case 'b':
return "SBAR";
case 'c':
return "VP";
case 'd':
return "QP";
case 'e':
return "ADVP";
case 'f':
return "SBAR";
case 'g':
return "PP";
case 'h':
return "QP";
case 'i':
return "ADVP";
case 'k':
return "PRT";
case 'n':
return "NP";
case 'p':
return "PP";
case 'q':
return "SINV";
case 's':
return "S";
case 't':
return "VP";
case 'u':
return "ADJP";
case 'v':
return "VP";
case 'y':
return "NP";
case 'z':
return "VP";
default:
{
err_ctxt ec = { linkage->sent };
err_msgc(&ec, lg_Error, "Illegal domain: %c\n", domain_type);
return "";
}
}
}
static int read_constituents_from_domains(con_context_t *ctxt, Linkage linkage,
int numcon_total)
{
size_t d, l, w2;
int c, w, c2, numcon_subl = 0;
PP_data *pp_data = &linkage->sent->constituent_pp->pp_data;
for (d = 0, c = numcon_total; d < pp_data->N_domains; d++, c++)
{
size_t leftmost, rightmost, leftlimit;
int rootleft;
List_o_links * dlink;
Domain domain = pp_data->domain_array[d];
rootleft = linkage_get_link_lword(linkage, domain.start_link);
if ((domain.type=='c') ||
(domain.type=='d') ||
(domain.type=='e') ||
(domain.type=='f') ||
(domain.type=='g') ||
(domain.type=='u') ||
(domain.type=='y'))
{
leftlimit = 0;
leftmost = linkage_get_link_lword(linkage, domain.start_link);
rightmost = linkage_get_link_lword(linkage, domain.start_link);
}
else
{
leftlimit = linkage_get_link_lword(linkage, domain.start_link) + 1;
leftmost = linkage_get_link_rword(linkage, domain.start_link);
rightmost = linkage_get_link_rword(linkage, domain.start_link);
}
for (dlink = domain.lol; dlink != NULL; dlink = dlink->next)
{
l = dlink->link;
if ((linkage_get_link_lword(linkage, l) < leftmost) &&
(linkage_get_link_lword(linkage, l) >= leftlimit))
{
leftmost = linkage_get_link_lword(linkage, l);
}
if (linkage_get_link_rword(linkage, l) > rightmost)
{
rightmost = linkage_get_link_rword(linkage, l);
}
}
c--;
c = add_constituent(ctxt, c, linkage, &domain, leftmost, rightmost,
cons_of_domain(linkage, domain.type));
if (domain.type == 'z')
{
c = add_constituent(ctxt, c, linkage, &domain, leftmost, rightmost, "S");
}
if (domain.type=='c')
{
c = add_constituent(ctxt, c, linkage, &domain, leftmost, rightmost, "S");
}
if ((post_process_match("Ce*", ctxt->constituent[c].start_link)==1) ||
(post_process_match("Rn", ctxt->constituent[c].start_link)==1))
{
c = add_constituent(ctxt, c, linkage, &domain, leftmost, rightmost, "SBAR");
}
if ((post_process_match("R*", ctxt->constituent[c].start_link)==1) ||
(post_process_match("MX#r", ctxt->constituent[c].start_link)==1))
{
w = leftmost;
if (strcmp(linkage->word[w], ",") == 0) w++;
c = add_constituent(ctxt, c, linkage, &domain, w, w, "WHNP");
}
if (post_process_match("Mj", ctxt->constituent[c].start_link) == 1)
{
w = leftmost;
if (strcmp(linkage->word[w], ",") == 0) w++;
c = add_constituent(ctxt, c, linkage, &domain, w, w+1, "WHPP");
c = add_constituent(ctxt, c, linkage, &domain, w+1, w+1, "WHNP");
}
if ((post_process_match("Ss#d", ctxt->constituent[c].start_link)==1) ||
(post_process_match("B#d", ctxt->constituent[c].start_link)==1))
{
c = add_constituent(ctxt, c, linkage, &domain, rootleft, rootleft, "WHNP");
c = add_constituent(ctxt, c, linkage, &domain,
rootleft, ctxt->constituent[c-1].right, "SBAR");
}
if (post_process_match("CP", ctxt->constituent[c].start_link)==1)
{
if (strcmp(linkage->word[leftmost], ",") == 0)
ctxt->constituent[c].left++;
c = add_constituent(ctxt, c, linkage, &domain, 1, linkage->num_words-1, "S");
}
if ((post_process_match("MVs", ctxt->constituent[c].start_link)==1) ||
(domain.type=='f'))
{
w = ctxt->constituent[c].left;
if (strcmp(linkage->word[w], ",") == 0)
w++;
if (strcmp(linkage->word[w], "when") == 0)
{
c = add_constituent(ctxt, c, linkage, &domain, w, w, "WHADVP");
}
}
if (domain.type=='t')
{
c = add_constituent(ctxt, c, linkage, &domain, leftmost, rightmost, "S");
}
if ((post_process_match("QI", ctxt->constituent[c].start_link) == 1) ||
(post_process_match("Mr", ctxt->constituent[c].start_link) == 1) ||
(post_process_match("MX#d", ctxt->constituent[c].start_link) == 1))
{
const char * name = "";
w = leftmost;
if (strcmp(linkage->word[w], ",") == 0) w++;
if (ctxt->wordtype[w] == NONE)
name = "WHADVP";
else if (ctxt->wordtype[w] == QTYPE)
name = "WHNP";
else if (ctxt->wordtype[w] == QDTYPE)
name = "WHNP";
else
assert(0, "Unexpected word type");
c = add_constituent(ctxt, c, linkage, &domain, w, w, name);
if (ctxt->wordtype[w] == QDTYPE)
{
for (w2 = w+1; w2 < rightmost; w2++)
if ((ctxt->wordtype[w2] == STYPE) || (ctxt->wordtype[w2] == PTYPE)) break;
ctxt->constituent[c].right = w2 - 1;
c = add_constituent(ctxt, c, linkage, &domain, w2, rightmost, "S");
}
}
if (ctxt->constituent[c].domain_type == '\0')
{
err_ctxt ec = { linkage->sent };
err_msgc(&ec, lg_Error, "No domain type assigned to constituent\n");
}
if (ctxt->constituent[c].start_link == NULL)
{
err_ctxt ec = { linkage->sent };
err_msgc(&ec, lg_Error, "No type assigned to constituent\n");
}
}
numcon_subl = c - numcon_total;
if (verbosity_level(D_CONST))
{
err_msg(lg_Debug, "Constituents added at first stage:\n\\");
for (c = numcon_total; c < numcon_total + numcon_subl; c++)
{
print_constituent(ctxt, linkage, c);
}
}
numcon_subl =
gen_comp(ctxt, linkage, numcon_total, numcon_subl, "SBAR", "S", "S", CASE_OPENER);
numcon_subl =
gen_comp(ctxt, linkage, numcon_total, numcon_subl, "PP", "S", "S", CASE_PPOPEN);
numcon_subl =
gen_comp(ctxt, linkage, numcon_total, numcon_subl, "S", "S", "S", CASE_PART_OPEN);
numcon_subl =
gen_comp(ctxt, linkage, numcon_total, numcon_subl, "VP", "S", "NP", CASE_S);
numcon_subl =
gen_comp(ctxt, linkage, numcon_total, numcon_subl, "SBAR", "NP", "NP", CASE_REL_CLAUSE);
numcon_subl =
gen_comp(ctxt, linkage, numcon_total, numcon_subl, "VP", "NP", "NP", CASE_PART_MOD);
numcon_subl =
gen_comp(ctxt, linkage, numcon_total, numcon_subl, "PP", "NP", "NP", CASE_PART_MOD);
numcon_subl =
gen_comp(ctxt, linkage, numcon_total, numcon_subl, "NP", "NP", "NP", CASE_APPOS);
numcon_subl =
gen_comp(ctxt, linkage, numcon_total, numcon_subl, "NP", "SINV", "VP", CASE_SVINV);
adjust_subordinate_clauses(ctxt, linkage, numcon_total, numcon_subl);
for (c = numcon_total; c < numcon_total + numcon_subl; c++)
{
if ((ctxt->constituent[c].domain_type=='p') &&
(strcmp(linkage->word[ctxt->constituent[c].left], ",")==0))
{
ctxt->constituent[c].left++;
}
}
while (true)
{
bool adjustment_made = false;
for (c = numcon_total; c < numcon_total + numcon_subl; c++)
{
for (c2 = numcon_total; c2 < numcon_total + numcon_subl; c2++)
{
if ((ctxt->constituent[c].left < ctxt->constituent[c2].left) &&
(ctxt->constituent[c].right < ctxt->constituent[c2].right) &&
(ctxt->constituent[c].right >= ctxt->constituent[c2].left))
{
if ((strcmp(linkage->word[ctxt->constituent[c2].right], ",") == 0) ||
(strcmp(linkage->word[ctxt->constituent[c2].right],
RIGHT_WALL_WORD) == 0))
{
if (verbosity_level(D_CONST))
err_msg(lg_Debug, "Adjusting %d to fix comma overlap\n", c2);
adjust_for_right_comma(ctxt, linkage, c2);
adjustment_made = true;
}
else if (strcmp(linkage->word[ctxt->constituent[c].left], ",") == 0)
{
if (verbosity_level(D_CONST))
err_msg(lg_Debug, "Adjusting c %d to fix comma overlap\n", c);
adjust_for_left_comma(ctxt, linkage, c);
adjustment_made = true;
}
else
{
if (verbosity_level(D_CONST))
{
err_ctxt ec = { linkage->sent };
err_msgc(&ec, lg_Warn,
"Warning: the constituents aren't nested! "
"Adjusting them. (%d, %d)\n", c, c2);
}
ctxt->constituent[c].left = ctxt->constituent[c2].left;
}
}
}
}
if (adjustment_made == false) break;
}
assert (numcon_total + numcon_subl < ctxt->conlen, "Too many constituents");
return numcon_subl;
}
static char *
exprint_constituent_structure(con_context_t *ctxt,
Linkage linkage, int numcon_total)
{
size_t w;
int c;
bool *leftdone = alloca(numcon_total * sizeof(bool));
bool *rightdone = alloca(numcon_total * sizeof(bool));
int best, bestright, bestleft;
dyn_str * cs = dyn_str_new();
assert (numcon_total < ctxt->conlen, "Too many constituents (b)");
for (c = 0; c < numcon_total; c++)
{
leftdone[c] = false;
rightdone[c] = false;
}
for (w = 1; w < linkage->num_words; w++)
{
while (1)
{
best = -1;
bestright = -1;
for (c = 0; c < numcon_total; c++)
{
if ((ctxt->constituent[c].left == w) &&
(leftdone[c] == false) && ctxt->constituent[c].valid &&
((int) ctxt->constituent[c].right >= bestright))
{
best = c;
bestright = ctxt->constituent[c].right;
}
}
if (best == -1)
break;
leftdone[best] = true;
dyn_strcat(cs, "[");
dyn_strcat(cs, ctxt->constituent[best].type);
dyn_strcat(cs, " ");
}
if (w < linkage->num_words - 1)
{
char *p;
char s[MAX_WORD];
strncpy(s, linkage->word[w], MAX_WORD-1);
s[MAX_WORD-1] = 0;
p = strchr(s, OPEN_BRACKET);
while (p)
{
*p = '{';
p = strchr(p, OPEN_BRACKET);
}
p = strchr(s, CLOSE_BRACKET);
while (p)
{
*p = '}';
p = strchr(p, CLOSE_BRACKET);
}
#if 0
if (linkage->chosen_disjuncts[w]->word[0]->status & WS_FIRSTUPPER)
upcase_utf8_str(s, s, MAX_WORD);
#endif
dyn_strcat(cs, s);
dyn_strcat(cs, " ");
}
while (1)
{
best = -1;
bestleft = -1;
for(c = 0; c < numcon_total; c++)
{
if ((ctxt->constituent[c].right == w) &&
(rightdone[c] == false) && ctxt->constituent[c].valid &&
((int) ctxt->constituent[c].left > bestleft)) {
best = c;
bestleft = ctxt->constituent[c].left;
}
}
if (best == -1)
break;
rightdone[best] = true;
dyn_strcat(cs, ctxt->constituent[best].type);
dyn_strcat(cs, "] ");
}
}
dyn_strcat(cs, "\n");
return dyn_str_take(cs);
}
static char * do_print_flat_constituents(con_context_t *ctxt, Linkage linkage)
{
int numcon_total= 0, numcon_subl;
Sentence sent = linkage->sent;
ctxt->phrase_ss = string_set_create();
generate_misc_word_info(ctxt, linkage);
if (NULL == sent->constituent_pp)
sent->constituent_pp = post_process_new(sent->dict->hpsg_knowledge);
do_post_process(sent->constituent_pp, linkage, linkage->is_sent_long);
numcon_subl = read_constituents_from_domains(ctxt, linkage, numcon_total);
numcon_total += numcon_subl;
assert (numcon_total < ctxt->conlen, "Too many constituents (c)");
numcon_total = merge_constituents(ctxt, linkage, numcon_total);
assert (numcon_total < ctxt->conlen, "Too many constituents (d)");
numcon_total = new_style_conjunctions(ctxt, linkage, numcon_total);
assert (numcon_total < ctxt->conlen, "Too many constituents (e)");
numcon_total = last_minute_fixes(ctxt, linkage, numcon_total);
assert (numcon_total < ctxt->conlen, "Too many constituents (f)");
char * q = exprint_constituent_structure(ctxt, linkage, numcon_total);
string_set_delete(ctxt->phrase_ss);
ctxt->phrase_ss = NULL;
post_process_free_data(&sent->constituent_pp->pp_data);
return q;
}
static char * print_flat_constituents(Linkage linkage)
{
size_t wts = linkage->num_words * sizeof(WType);
size_t cns = (linkage->num_links + linkage->num_words) * sizeof(constituent_t);
con_context_t ctxt;
memset(&ctxt, 0, sizeof(con_context_t));
ctxt.wordtype = (WType *) alloca(wts);
memset(ctxt.wordtype, 0, wts);
ctxt.conlen = linkage->num_links + linkage->num_words;
ctxt.constituent = (constituent_t *) alloca(cns);
memset(ctxt.constituent, 0, cns);
return do_print_flat_constituents(&ctxt, linkage);
}
static CType token_type (char *token)
{
if ((token[0] == OPEN_BRACKET) && (strlen(token) > 1))
return OPEN_TOK;
if ((strlen(token) > 1) && (token[strlen(token) - 1] == CLOSE_BRACKET))
return CLOSE_TOK;
return WORD_TOK;
}
static CNode * make_CNode(char *q)
{
CNode * cn;
cn = (CNode *) malloc(sizeof(CNode));
cn->label = strdup(q);
cn->child = cn->next = (CNode *) NULL;
cn->next = (CNode *) NULL;
cn->start = cn->end = -1;
return cn;
}
static CNode * parse_string(CNode * n, char **saveptr)
{
char *q;
CNode *m, *last_child=NULL;
while ((q = strtok_r(NULL, " ", saveptr))) {
switch (token_type(q)) {
case CLOSE_TOK :
q[strlen(q)-1]='\0';
assert(strcmp(q, n->label)==0,
"Constituent tree: Labels do not match.");
return n;
break;
case OPEN_TOK:
m = make_CNode(q+1);
m = parse_string(m, saveptr);
break;
case WORD_TOK:
m = make_CNode(q);
break;
default:
assert(0, "Constituent tree: Illegal token type");
}
if (n->child == NULL) {
last_child = n->child = m;
}
else {
last_child->next = m;
last_child = m;
}
}
assert(0, "Constituent tree: Constituent did not close");
return NULL;
}
static void print_tree(dyn_str * cs, int indent, CNode * n, int o1, int o2)
{
int i, child_offset;
CNode * m;
if (n == NULL) return;
if (indent)
for (i = 0; i < o1; ++i) dyn_strcat(cs, " ");
dyn_strcat(cs, "(");
dyn_strcat(cs, n->label);
dyn_strcat(cs, " ");
child_offset = o2 + strlen(n->label) + 2;
for (m = n->child; m != NULL; m = m->next)
{
if (m->child == NULL)
{
char * p;
p = strchr(m->label, '(');
while(p)
{
*p = '{';
p = strchr(p, '(');
}
p = strchr(m->label, ')');
while(p)
{
*p = '}';
p = strchr(p, ')');
}
dyn_strcat(cs, m->label);
if ((m->next != NULL) && (m->next->child == NULL))
dyn_strcat(cs, " ");
}
else
{
if (m != n->child)
{
if (indent) dyn_strcat(cs, "\n");
else dyn_strcat(cs, " ");
print_tree(cs, indent, m, child_offset, child_offset);
}
else
{
print_tree(cs, indent, m, 0, child_offset);
}
if ((m->next != NULL) && (m->next->child == NULL))
{
if (indent)
{
dyn_strcat(cs, "\n");
for (i = 0; i < child_offset; ++i)
dyn_strcat(cs, " ");
}
else dyn_strcat(cs, " ");
}
}
}
dyn_strcat(cs, ")");
}
static int assign_spans(CNode * n, int start)
{
int num_words=0;
CNode * m=NULL;
if (n==NULL) return 0;
n->start = start;
if (n->child == NULL) {
n->end = start;
return 1;
}
else {
for (m=n->child; m!=NULL; m=m->next) {
num_words += assign_spans(m, start+num_words);
}
n->end = start+num_words-1;
}
return num_words;
}
static CNode * linkage_constituent_tree(Linkage linkage)
{
char *p, *q, *saveptr;
CNode * root;
p = print_flat_constituents(linkage);
q = strtok_r(p, " ", &saveptr);
assert(token_type(q) == OPEN_TOK, "Illegal beginning of string");
root = make_CNode(q+1);
root = parse_string(root, &saveptr);
assign_spans(root, 0);
dyn_str_release(p);
return root;
}
static void linkage_free_constituent_tree(CNode * n)
{
CNode *m, *x;
for (m=n->child; m!=NULL; m=x) {
x=m->next;
linkage_free_constituent_tree(m);
}
free(n->label);
free(n);
}
char * linkage_print_constituent_tree(Linkage linkage, ConstituentDisplayStyle mode)
{
CNode * root;
if (!linkage) return NULL;
if (!linkage->sent->dict->hpsg_knowledge) return NULL;
if (mode == NO_DISPLAY)
{
return NULL;
}
else if (mode == MULTILINE || mode == SINGLE_LINE)
{
dyn_str * cs;
cs = dyn_str_new();
root = linkage_constituent_tree(linkage);
print_tree(cs, (mode==1), root, 0, 0);
linkage_free_constituent_tree(root);
dyn_strcat(cs, "\n");
return dyn_str_take(cs);
}
else if (mode == BRACKET_TREE)
{
return print_flat_constituents(linkage);
}
prt_error("Warning: Illegal mode %u for printing constituents\n"
"Allowed values: %d to %d\n", mode, NO_DISPLAY, MAX_STYLES);
return NULL;
}
void linkage_free_constituent_tree_str(char * s)
{
dyn_str_release(s);
}