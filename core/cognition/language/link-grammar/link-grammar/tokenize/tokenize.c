#ifndef _WIN32
#include <langinfo.h>
#endif
#include <limits.h>
#include "anysplit.h"
#include "api-structures.h"
#include "dict-common/dict-affix.h"
#include "dict-common/dict-api.h"
#include "dict-common/dict-common.h"
#include "dict-common/dict-defines.h"
#include "dict-common/dict-utils.h"
#include "dict-common/regex-morph.h"
#include "error.h"
#include "print/print-util.h"
#include "spellcheck.h"
#include "string-set.h"
#include "tokenize.h"
#include "tok-structures.h"
#include "utilities.h"
#include "wordgraph.h"
#include "word-structures.h"
#define MAX_STRIP 10
#define MAX_STRIP_ALT 5
#define SYNTHETIC_SENTENCE_MARK '>'
#define D_SW 6
#define D_UN 6
typedef const char *stripped_t[MAX_STRIP];
#define ENTITY_MARKER "<marker-entity>"
#define COMMON_ENTITY_MARKER "<marker-common-entity>"
#define REPLACEMENT_MARK "~"
#define CAP1st "<1stCAP>"
#define CAPnon "<nonCAP>"
static bool is_common_entity(Dictionary dict, const char * str)
{
if (word_contains(dict, str, COMMON_ENTITY_MARKER) == 1)
return true;
return false;
}
static bool is_entity(Dictionary dict, const char * str)
{
const char * regex_name;
if (word_contains(dict, str, ENTITY_MARKER) == 1)
return true;
regex_name = match_regex(dict->regex_root, str);
if (NULL == regex_name) return false;
return word_contains(dict, regex_name, ENTITY_MARKER);
}
#if defined HAVE_HUNSPELL || defined HAVE_ASPELL
static bool is_proper_name(const char * word, locale_t dict_locale)
{
return is_utf8_upper(word, dict_locale);
}
static bool contains_digits(const char * s, locale_t dict_locale)
{
mbstate_t mbs;
int nb = 1;
wchar_t c;
memset(&mbs, 0, sizeof(mbs));
while ((*s != 0) && (0 < nb))
{
nb = mbrtowc(&c, s, MB_CUR_MAX, &mbs);
if (nb < 0) return false;
if (iswdigit_l(c, dict_locale)) return true;
s += nb;
}
return false;
}
#endif
static char *utf8_str1chr(const char *s, const char *xc)
{
int len = utf8_charlen(xc);
if (len < 0) return NULL;
char *xc1 = strndupa(xc, len);
return strstr(s, xc1);
}
static bool in_afdict_class(Dictionary dict, afdict_classnum cn, const char *s)
{
if (0 == AFCLASS(dict->affix_table, cn)->length) return false;
const char *classchars = AFCLASS(dict->affix_table, cn)->string[0];
return NULL != utf8_str1chr(classchars, s);
}
static bool is_space(wchar_t wc, locale_t dict_locale)
{
if (iswspace_l(wc, dict_locale)) return true;
if (0xa0 == wc) return true;
if (0x2000 <= wc && wc <= 0x200d) return true;
if (0x2028 == wc) return true;
if (0x2029 == wc) return true;
if (0x202f == wc) return true;
if (0x205f == wc) return true;
if (0x2060 == wc) return true;
return false;
}
static void gwordqueue_add(const Sentence sent, Gword *const word)
{
word_queue_t *wq_element = malloc(sizeof(word_queue_t));
assert((NULL == sent->word_queue) == (NULL == sent->word_queue_last),
"Inconsistent word queue pointers");
if (NULL == sent->word_queue)
sent->word_queue = wq_element;
else
sent->word_queue_last->next = wq_element;
wq_element->word = word;
wq_element->next = NULL;
sent->word_queue_last = wq_element;
}
static void word_label(Sentence sent, Gword *w, const char *op,
const char *label)
{
const size_t s = (NULL == w->label) ? 0 : strlen(w->label);
char *new_label = alloca(s + strlen(label) + 1 + 2 + 1);
if (0 != s)
strcpy(new_label, w->label);
else
new_label[0] = '\0';
if (NULL == op)
strcat(new_label, "(");
else if ('\0' != new_label[0])
strcat(new_label, op);
strcat(new_label, label);
if (NULL == op) strcat(new_label, ")");
w->label = string_set_add(new_label, sent->string_set);
}
#ifdef CHECK_DUPLICATE_ALTS
#define D_WSAA 9
static bool word_start_another_alternative(Dictionary dict,
Gword *unsplit_word,
const char *altword0)
{
Gword **n;
lgdebug(+D_WSAA, "\n");
lgdebug(+D_WSAA, "Checking %s in alternatives of %zu:%s (prev %zu:%s)\n",
altword0, unsplit_word->node_num, unsplit_word->subword,
unsplit_word->prev[0]->node_num, unsplit_word->prev[0]->subword);
for (n = unsplit_word->prev[0]->next; NULL != *n; n++)
{
if ((*n)->unsplit_word != unsplit_word) continue;
lgdebug(D_WSAA, "Comparing alt %s\n\\", (*n)->subword);
if ((0 == strcmp((*n)->subword, altword0) ||
((0 == strncmp((*n)->subword, altword0, strlen((*n)->subword))) &&
!dictionary_word_is_known(dict, altword0))))
{
lgdebug(+D_UN, "Preventing alt starts with %s due to existing %zu:%s\n",
altword0, (*n)->node_num, (*n)->subword);
return true;
}
}
return false;
}
#undef D_WSAA
#endif
static char const *contraction_char[] = { "'", "’" };
static bool is_contraction_word(Dictionary dict, const char *s)
{
if (dict->affix_table && dict->affix_table->anysplit)
return false;
for (size_t i = 0; i < ARRAY_SIZE(contraction_char); i++)
{
if (NULL != strstr(s, contraction_char[i])) return true;
}
return false;
}
static bool is_afdict_punc(const Dictionary afdict, const char *word)
{
if (NULL == afdict) return false;
for (size_t punc = 0; punc < ARRAY_SIZE(affix_strippable); punc++)
{
if (AFDICT_UNITS == affix_strippable[punc]) continue;
const Afdict_class *punc_list = AFCLASS(afdict, affix_strippable[punc]);
size_t l_strippable = punc_list->length;
for (size_t i = 0; i < l_strippable - punc_list->Nregexes; i++)
{
const char *p = punc_list->string[i];
const char *w = word;
while ((*w == *p) && (*w != '\0')) { w++; p++; }
if (*w == *p) return true;
}
}
for (size_t punc = 0; punc < ARRAY_SIZE(affix_strippable); punc++)
{
if (AFDICT_UNITS == affix_strippable[punc]) continue;
const Afdict_class *punc_list = AFCLASS(afdict, affix_strippable[punc]);
for (size_t i = 0; i < punc_list->Nregexes; i++)
{
int start, end;
bool match_found =
matchspan_regex(punc_list->regex[i], word, &start, &end);
if (match_found && (start == 0) && (word[end] == '\0'))
return true;
}
}
return false;
}
static bool regex_guess(Dictionary dict, const char *word, Gword *gword)
{
const char *regex_name = match_regex(dict->regex_root, word);
if ((NULL != regex_name) && dict_has_word(dict, regex_name))
{
gword->status |= WS_REGEX;
gword->regex_name = regex_name;
return true;
}
return false;
}
#define PER_GWORD_FUNC(f) Gword *(f)(Sentence sent, Gword *w, unsigned int *arg)
static Gword *for_word_alt(Sentence sent, Gword *altp,
PER_GWORD_FUNC(*gword_func), unsigned int *arg)
{
if (NULL == altp) return NULL;
Gword *alternative_id = altp->alternative_id;
for (; altp->alternative_id == alternative_id; altp = altp->next[0])
{
if (NULL == altp) break;
Gword *gw = gword_func(sent, altp, arg);
if (NULL != gw) return gw;
if ((NULL == altp->next) || altp->issued_unsplit)
break;
}
return NULL;
}
static PER_GWORD_FUNC(gword_by_ordinal_position)
{
if (0 == arg[0]) return w;
arg[0]--;
return NULL;
}
static PER_GWORD_FUNC(set_word_status)
{
unsigned int status = *arg;
switch (status)
{
case WS_INDICT|WS_REGEX:
if (!(w->status & (WS_INDICT|WS_REGEX)))
{
if (dict_has_word(sent->dict, w->subword))
{
w->status |= WS_INDICT;
}
else
{
regex_guess(sent->dict, w->subword, w);
}
}
break;
#if defined HAVE_HUNSPELL || defined HAVE_ASPELL
case WS_RUNON:
case WS_SPELL:
if ((w->status & WS_INDICT) &&
!dict_has_word(sent->dict, w->subword))
{
status &= ~WS_INDICT;
}
w->status |= status;
break;
#endif
default:
assert(0, "Invalid status 0x%x\n", status);
}
lgdebug(+D_SW, "Word %s: status=%s\n", w->subword, gword_status(sent, w));
return NULL;
}
static PER_GWORD_FUNC(set_tokenization_step)
{
set_word_status(sent, w, (unsigned int []){WS_INDICT|WS_REGEX});
w->tokenizing_step = *arg;
lgdebug(+D_SW, "Word %s: status=%s tokenizing_step=%d\n",
w->subword, gword_status(sent, w), (int)w->tokenizing_step);
return NULL;
}
void tokenization_done(Sentence sent, Gword *altp)
{
for_word_alt(sent, altp, set_tokenization_step, (unsigned int[]){TS_DONE});
}
#define D_IWA 6
Gword *issue_word_alternative(Sentence sent, Gword *unsplit_word,
const char *label,
int prefnum, const char * const *prefix,
int stemnum, const char * const *stem,
int suffnum, const char * const *suffix)
{
int ai = 0;
const char * const *affix;
const char * const * const affixlist[] = { prefix, stem, suffix, NULL };
const int numlist[] = { prefnum, stemnum, suffnum };
enum affixtype { PREFIX, STEM, SUFFIX, END };
enum affixtype at;
const char infix_mark = INFIX_MARK(sent->dict->affix_table);
Gword *subword;
Gword *psubword = NULL;
const int token_tot = prefnum + stemnum + suffnum;
Morpheme_type morpheme_type = MT_NOT_SET;
Gword *alternative_id = NULL;
bool subword_eq_unsplit_word;
bool last_split = false;
int *strlen_cache = alloca(token_tot * sizeof(int));
#ifdef DEBUG
Gword *sole_alternative_of_itself = NULL;
#endif
if (unsplit_word->split_counter > MAX_SPLITS)
{
prt_error("Error: Word %s reached %d splits. "
"It will not get split further. The result is undefined.\n"
"Run with !verbosity="STRINGIFY(D_SW)" to debug\n",
unsplit_word->subword, MAX_SPLITS);
unsplit_word->tokenizing_step = TS_DONE;
return NULL;
}
lgdebug(+D_IWA, "(%s) Gword %zu:%s split (split_counter=%zu) into", label,
unsplit_word->node_num, unsplit_word->subword,
unsplit_word->split_counter);
int maxword = 0;
for (ai = 0, at = PREFIX; at < END; at++)
{
int affixnum = numlist[at];
char morpheme_sym[] = "pts";
for (affix = affixlist[at]; affixnum-- > 0; affix++, ai++)
{
strlen_cache[ai] = (int)strcspn(*affix, subscript_mark_str());
maxword = MAX(maxword, strlen_cache[ai]);
lgdebug(D_IWA, " %c:%s", morpheme_sym[at],
('\0' == (*affix)[0]) ? "[null]" : *affix);
}
}
char * const buff = alloca(maxword + 2);
const char *token;
for (ai = 0, at = PREFIX; at < END; at++)
{
int affixnum = numlist[at];
for (affix = affixlist[at]; affixnum-- > 0; affix++, ai++)
{
token = *affix;
switch (at)
{
case PREFIX:
if ('\0' != infix_mark)
{
size_t sz = strlen_cache[ai];
memcpy(buff, *affix, sz);
buff[sz] = infix_mark;
buff[sz+1] = '\0';
last_split = true;
token = buff;
}
if (is_contraction_word(sent->dict, unsplit_word->subword))
morpheme_type = MT_CONTR;
else
morpheme_type = MT_PREFIX;
break;
case STEM:
if (is_stem(token))
{
morpheme_type = MT_STEM;
last_split = true;
}
else if (is_afdict_punc(sent->dict->affix_table, token))
{
morpheme_type = MT_PUNC;
}
else
{
morpheme_type = MT_WORD;
}
break;
case SUFFIX:
if (((NULL == sent->dict->affix_table->anysplit) &&
('\0' != (*affix)[0]) &&
!is_utf8_alpha(*affix, sent->dict->lctype)) ||
'\0' == infix_mark)
{
if (is_contraction_word(sent->dict, unsplit_word->subword))
morpheme_type = MT_CONTR;
else
morpheme_type = MT_WORD;
break;
}
last_split = true;
buff[0] = infix_mark;
strcpy(&buff[1], *affix);
morpheme_type = MT_SUFFIX;
token = buff;
break;
case END:
assert(true, "affixtype END reached");
}
#ifdef CHECK_DUPLICATE_ALTS
if ((0 == ai) && (1 < token_tot) && (label[0] == 'r') &&
word_start_another_alternative(sent->dict, unsplit_word, token))
{
return NULL;
}
#endif
subword_eq_unsplit_word= (0 == strcmp(unsplit_word->subword, token));
if ((1 == token_tot) && subword_eq_unsplit_word)
{
Gword **q;
unsplit_word->issued_unsplit = true;
if (!(unsplit_word->status & WS_HASALT))
{
word_label(sent, unsplit_word, "+", label);
word_label(sent, unsplit_word, NULL, "IU");
lgdebug(D_IWA, " (issued_unsplit)\n");
return unsplit_word;
}
if (unsplit_word->status & WS_UNSPLIT)
{
if (0 < verbosity)
{
prt_error("Warning: Internal error: "
"word \"%s\" got issued more than once\n",
unsplit_word->subword);
}
return NULL;
}
for (q = unsplit_word->prev; *q; q++)
gwordlist_append(&(*q)->next, unsplit_word);
for (q = unsplit_word->next; *q; q++)
gwordlist_append(&(*q)->prev, unsplit_word);
word_label(sent, unsplit_word, "+", label);
word_label(sent, unsplit_word, NULL, "R");
unsplit_word->status |= WS_UNSPLIT;
alternative_id = unsplit_word->alternative_id;
#ifdef DEBUG
sole_alternative_of_itself = unsplit_word;
#endif
lgdebug(D_IWA, " (reconnected)");
}
else
{
subword = gword_new(sent, token);
subword->unsplit_word = unsplit_word;
subword->split_counter = unsplit_word->split_counter + 1;
subword->morpheme_type = morpheme_type;
if (MT_PUNC == morpheme_type)
tokenization_done(sent, subword);
if (!sent->dict->affix_table->pre_suf_class_exists && last_split &&
!(sent->dict->affix_table && sent->dict->affix_table->anysplit))
{
subword->status |= WS_INDICT;
subword->tokenizing_step = TS_DONE;
}
word_label(sent, subword, "+", label);
if (!subword_eq_unsplit_word)
gwordqueue_add(sent, subword);
if (unsplit_word->status & (WS_SPELL|WS_RUNON))
subword->status |= unsplit_word->status & (WS_SPELL|WS_RUNON);
if (0 == ai)
{
subword->start = unsplit_word->start;
if (REPLACEMENT_MARK[0] == label[0])
{
subword->end = unsplit_word->end;
}
else
{
subword->end = subword->start + strlen_cache[ai];
if (subword->status & WS_FIRSTUPPER)
{
int uclen = utf8_charlen(unsplit_word->subword);
int lclen = utf8_charlen(token);
if ((uclen > 0) && (lclen > 0))
subword->end += uclen - lclen;
}
}
if (unsplit_word->status & WS_FIRSTUPPER)
subword->status |= WS_FIRSTUPPER;
Gword **p;
alternative_id = subword;
for (p = unsplit_word->prev; NULL != *p; p++)
{
Gword **n;
gwordlist_append(&subword->prev, *p);
if (unsplit_word->status & WS_HASALT)
{
gwordlist_append(&(*p)->next, subword);
}
else
{
for(n = (*p)->next; NULL != *n; n++)
{
if (*n == unsplit_word)
{
*n = subword;
break;
}
}
assert(NULL != *n, "Adding subword '%s': "
"No corresponding next link for a prev link: "
"prevword='%s' word='%s'",
subword->subword, (*p)->subword, unsplit_word->subword);
}
}
}
if (token_tot-1 == ai)
{
Gword **n;
for (n = unsplit_word->next; NULL != *n; n++)
{
Gword **p;
gwordlist_append(&subword->next, *n);
if (unsplit_word->status & WS_HASALT)
{
gwordlist_append(&(*n)->prev, subword);
}
else
{
for(p = (*n)->prev; NULL != *p; p++)
{
if (*p == unsplit_word)
{
*p = subword;
break;
}
}
assert(NULL!=*p,
"Adding subword '%s': "
"No corresponding prev link for a next link"
"nextword='%s' word='%s'",
subword->subword, (*n)->subword, unsplit_word->subword);
}
}
}
if (0 < ai)
{
if (REPLACEMENT_MARK[0] == label[0])
{
subword->start = unsplit_word->start;
subword->end = unsplit_word->end;
}
else
{
subword->start = psubword->end;
subword->end = subword->start + strlen_cache[ai];
}
gwordlist_append(&psubword->next, subword);
gwordlist_append(&subword->prev, psubword);
}
subword->alternative_id = alternative_id;
psubword = subword;
}
}
}
unsplit_word->status |= WS_HASALT;
lgdebug(D_IWA, "\n");
#ifdef DEBUG
{
Gword **prev = unsplit_word->prev;
Gword *curr_alt = sole_alternative_of_itself ?
sole_alternative_of_itself : alternative_id;
Gword **alts;
assert(curr_alt, "'%s': No alt mark", unsplit_word->subword);
assert(prev, "'%s': No prev", unsplit_word->subword);
assert(prev[0], "'%s': No prev[0]", unsplit_word->subword);
assert(prev[0]->next, "%s': No next",prev[0]->subword);
assert(prev[0]->next[0], "'%s': No next[0]",prev[0]->subword);
for (alts = prev[0]->next; *alts; alts++)
{
if ((*alts)->unsplit_word != unsplit_word) continue;
Gword *calt = curr_alt;
Gword *oalt;
size_t token_no = token_tot;
if (*alts == curr_alt) break;
for (oalt = *alts; token_no > 0; oalt = oalt->next[0])
{
if (0 != (strcmp(oalt->subword, calt->subword)))
break;
calt = calt->next[0];
token_no--;
}
if (token_tot) continue;
prt_error("Error: >>>DEBUG>>>: '%s' "
"(alternative start '%s', len=%d): "
"Alternative already exists!\n",
curr_alt->subword, unsplit_word->subword, token_tot);
}
}
#endif
return alternative_id;
}
#undef D_IWA
#define D_RWW 6
static void remqueue_gword(const Sentence sent)
{
word_queue_t *const wq = sent->word_queue;
assert(NULL!=wq, "Trying to remove a word from an empty word queue");
Gword *w = wq->word;
lgdebug(+D_RWW, "Word '%s'%s%s\n", w->subword,
w->issued_unsplit ? " issued_unsplit" : "",
(w->status & WS_HASALT) ? " WS_HASALT" : "");
if (w->issued_unsplit && (w->status & WS_HASALT) && !(w->status & WS_UNSPLIT))
{
issue_word_alternative(sent, w, "RQ" ,0,NULL, 1,&w->subword, 0,NULL);
}
#if WORDGRAPH_PARSER
build_expressions(wq->word);
#endif
sent->word_queue = wq->next;
free(wq);
}
#undef D_RWW
static Gword *wordgraph_getqueue_word(Sentence sent)
{
Gword *w;
if (NULL == sent->word_queue) return NULL;
w = sent->word_queue->word;
return w;
}
static const char ** resize_alts(const char **arr, size_t len)
{
arr = realloc(arr, (len+2) * sizeof(char *));
arr[len+1] = NULL;
return arr;
}
void altappend(Sentence sent, const char ***altp, const char *w)
{
size_t n = altlen(*altp);
*altp = resize_alts(*altp, n);
(*altp)[n] = string_set_add(w, sent->string_set);
}
static void altfree(const char **alts)
{
free(alts);
}
#ifdef DEBUG_WORDGRAPH
static bool synthetic_split(Sentence sent, Gword *unsplit_word)
{
const char *const w = unsplit_word->subword;
const char *c = w;
const char *s = w;
int plevel = 0;
const char **alts = NULL;
bool can_split = false;
const size_t len = strlen(c);
char *alt = alloca(len+1);
#define SYNTHSPLIT_ERROR(e) ("Error: synthetic_split(): word '%s':" e "\n")
if (SYNTHETIC_SENTENCE_MARK != sent->orig_sentence[0]) return false;
assert(0 != len, "Empty-string word");
if (')' != w[len-1]) return false;
do
{
switch (*c)
{
case '(':
if (0 == plevel) s = c + 1;
plevel++;
break;
case ')':
case '+':
case '|':
if (1 == plevel)
{
if (c == s)
{
prt_error(SYNTHSPLIT_ERROR("(empty subword)."), w);
goto error;
}
strncpy(alt, s, c-s);
alt[c-s] = '\0';
if (0 == strcmp(alt, "()"))
{
strcpy(alt, w);
}
altappend(sent, &alts, alt);
s = c + 1;
if ('|' == *c)
{
if (alts)
issue_word_alternative(sent, unsplit_word, "SS", 0,NULL,
altlen(alts),alts, 0,NULL);
can_split = true;
free(alts);
alts = NULL;
}
}
if (')' == *c) plevel--;
break;
default:
if (!(((*c >= 'a') && (*c <= 'z')) ||
((*c >= 'A') && (*c <= 'Z')) ||
((*c >= '0') && (*c <= '9')) ||
('_' == *c)))
{
prt_error(SYNTHSPLIT_ERROR("('%c' not alphanumeric)."), w, *c);
goto error;
}
}
if (0 > plevel)
{
prt_error(SYNTHSPLIT_ERROR("extra ')'"), w);
goto error;
}
} while ('\0' != *++c);
if (0 < plevel)
{
prt_error(SYNTHSPLIT_ERROR("missing '('."), w);
goto error;
}
if (alts)
{
issue_word_alternative(sent, unsplit_word, "SS", 0,NULL,
altlen(alts),alts, 0,NULL);
can_split = true;
}
error:
free(alts);
return can_split;
}
#endif
static bool add_alternative_with_subscr(Sentence sent,
Gword * unsplit_word,
const char * prefix,
const char * word,
const char * suffix)
{
Dictionary dict = sent->dict;
Afdict_class * stemsubscr_list =
AFCLASS(dict->affix_table, AFDICT_STEMSUBSCR);
const char ** stemsubscr = stemsubscr_list->string;
size_t stemsubscr_count = stemsubscr_list->length;
bool word_is_in_dict = false;
bool issue_alternatives = (NULL != unsplit_word);
if (0 == stemsubscr_count)
{
if (issue_alternatives)
{
word_is_in_dict = true;
issue_word_alternative(sent, unsplit_word, "AWS",
(prefix ? 1 : 0),&prefix, 1,&word,
(suffix ? 1 : 0),&suffix);
}
else
{
word_is_in_dict = dict_has_word(dict, word);
}
}
else
{
size_t si;
size_t wlen = strlen(word);
size_t slen = 0;
char *w;
for (si = 0; si < stemsubscr_count; si++)
{
slen = MAX(slen, strlen(stemsubscr[si]));
}
w = alloca(wlen + slen + 1);
strcpy(w, word);
for (si = 0; si < stemsubscr_count; si++)
{
strcpy(&w[wlen], stemsubscr[si]);
if (dict_has_word(dict, w))
{
word_is_in_dict = true;
if (issue_alternatives)
{
issue_word_alternative(sent, unsplit_word, "AWS",
(prefix ? 1 : 0),&prefix, 1,(const char **)&w, 1,&suffix);
}
}
}
}
lgdebug(+D_SW,"Stem subscript not found: p:%s t:%s s:%s\n",
prefix ? prefix : "(none)", word, suffix ? suffix : "(none)");
return word_is_in_dict;
}
static bool suffix_split(Sentence sent, Gword *unsplit_word, const char *w)
{
int i, j;
Afdict_class *prefix_list, *suffix_list;
int p_strippable, s_strippable;
const char **prefix, **suffix;
const char *no_suffix = NULL;
bool word_can_split = false;
const Dictionary dict = sent->dict;
const char *wend = w + strlen(w);
char *newword = alloca(wend-w+1);
if (NULL == dict->affix_table) return false;
prefix_list = AFCLASS(dict->affix_table, AFDICT_PRE);
p_strippable = prefix_list->length;
prefix = prefix_list->string;
suffix_list = AFCLASS(dict->affix_table, AFDICT_SUF);
s_strippable = suffix_list->length;
suffix = suffix_list->string;
if (INT_MAX == s_strippable) return false;
for (i = 0; i <= s_strippable; i++, suffix++)
{
bool did_split = false;
size_t suflen = 0;
if (i < s_strippable)
{
suflen = strlen(*suffix);
if ((size_t) (wend-w) < suflen+1) continue;
if (0 == strncmp(wend-suflen, *suffix, suflen))
{
size_t sz = (wend-w)-suflen;
strncpy(newword, w, sz);
newword[sz] = '\0';
if ((is_contraction_word(dict, w) &&
dictionary_word_is_known(dict, newword)) ||
dict_has_word(dict, newword))
{
did_split = true;
word_can_split |=
add_alternative_with_subscr(sent, unsplit_word,
NULL, newword, *suffix);
}
}
}
else
{
suflen = 0;
suffix = &no_suffix;
}
if (did_split || 0==suflen)
{
for (j = 0; j < p_strippable; j++)
{
size_t prelen = strlen(prefix[j]);
if (suflen+prelen < (size_t) (wend-w)
&& strncmp(w, prefix[j], prelen) == 0)
{
strcpy(newword, w+prelen);
if (dict_has_word(dict, newword))
{
word_can_split |=
add_alternative_with_subscr(sent, unsplit_word, prefix[j],
newword, *suffix);
}
}
}
}
}
return word_can_split;
}
#define HEB_PRENUM_MAX 5
#define HEB_UTF8_BYTES 2
#define HEB_CHAREQ(s, c) (strncmp(s, c, HEB_UTF8_BYTES) == 0)
static bool mprefix_split(Sentence sent, Gword *unsplit_word, const char *word)
{
int i;
Afdict_class *mprefix_list;
int mp_strippable;
const char **mprefix;
const char *newword;
const char *w;
int sz = 0;
bool word_is_in_dict = false;
int split_prefix_i = 0;
const char *split_prefix[HEB_PRENUM_MAX];
bool *pseen;
int pfound;
Dictionary dict = sent->dict;
int wordlen;
int wlen;
int plen = 0;
Gword *altp;
bool split_check = (NULL == unsplit_word);
if (NULL == dict->affix_table) return false;
mprefix_list = AFCLASS(dict->affix_table, AFDICT_MPRE);
mp_strippable = mprefix_list->length;
if (0 == mp_strippable) return false;
mprefix = mprefix_list->string;
pseen = alloca(mp_strippable * sizeof(*pseen));
memset(pseen, 0, mp_strippable * sizeof(*pseen));
w = word;
wordlen = strlen(word);
do
{
pfound = -1;
for (i=0; i<mp_strippable; i++)
{
if (pseen[i])
continue;
if ((split_prefix_i > 0) &&
HEB_CHAREQ(mprefix[i], "ו") && (HEB_CHAREQ(w, "ו")))
{
continue;
}
plen = strlen(mprefix[i]);
wlen = strlen(w);
sz = wlen - plen;
if (strncmp(w, mprefix[i], plen) == 0)
{
if (-1 == pfound) pfound = i;
newword = w + plen;
if (!HEB_CHAREQ(mprefix[i], "ו") && (HEB_CHAREQ(newword, "ו")))
{
if (!HEB_CHAREQ(newword+HEB_UTF8_BYTES, "ו"))
continue;
if (newword[HEB_UTF8_BYTES+1])
newword += HEB_UTF8_BYTES;
}
pseen[i] = true;
split_prefix[split_prefix_i] = mprefix[i];
if (0 == sz)
{
word_is_in_dict = true;
lgdebug(+D_UN, "Whole-word prefix: %s\n", word);
if (split_check) return true;
altp = issue_word_alternative(sent, unsplit_word, "MPW",
split_prefix_i+1,split_prefix, 0,NULL, 0,NULL);
tokenization_done(sent, altp);
break;
}
if (dictionary_word_is_known(dict, newword))
{
word_is_in_dict = true;
lgdebug(+D_UN, "Splitting off a prefix: %.*s-%s\n",
wordlen-sz, word, newword);
if (split_check) return true;
altp = issue_word_alternative(sent, unsplit_word, "MPS",
split_prefix_i+1,split_prefix, 1,&newword, 0,NULL);
tokenization_done(sent, altp);
}
}
}
if ((-1 != pfound) && (i != pfound))
{
split_prefix[split_prefix_i] = mprefix[pfound];
plen = strlen(mprefix[pfound]);
w += plen;
}
#if 0
else
{
w = newword;
}
#endif
split_prefix_i++;
} while ((sz > 0) && (-1 != pfound) && (split_prefix_i < HEB_PRENUM_MAX));
return word_is_in_dict;
}
static bool is_capitalizable(const Dictionary dict, const Gword *word)
{
if (dict->disable_downcasing) return false;
if (MT_WALL == word->prev[0]->morpheme_type) return true;
if (MT_INFRASTRUCTURE == word->prev[0]->morpheme_type) return true;
if (strcmp(":", word->prev[0]->subword) == 0 ||
strcmp(".", word->prev[0]->subword) == 0 ||
strcmp("...", word->prev[0]->subword) == 0 ||
strcmp("…", word->prev[0]->subword) == 0 ||
strcmp("?", word->prev[0]->subword) == 0 ||
strcmp("!", word->prev[0]->subword) == 0 ||
strcmp("？", word->prev[0]->subword) == 0 ||
strcmp("！", word->prev[0]->subword) == 0)
return true;
if (in_afdict_class(dict, AFDICT_BULLETS, word->prev[0]->subword))
return true;
if (in_afdict_class(dict, AFDICT_QUOTES, word->prev[0]->subword))
return true;
return false;
}
#define D_MS 6
static bool morpheme_split(Sentence sent, Gword *unsplit_word, const char *word)
{
bool word_can_split;
if (0 < AFCLASS(sent->dict->affix_table, AFDICT_MPRE)->length)
{
word_can_split = mprefix_split(sent, unsplit_word, word);
lgdebug(+D_MS, "Tried mprefix_split word=%s can_split=%d\n",
word, word_can_split);
}
else
{
word_can_split = suffix_split(sent, unsplit_word, word);
lgdebug(+D_MS, "Tried to split word=%s can_split=%d\n",
word, word_can_split);
if ((NULL != unsplit_word) && is_utf8_upper(word, sent->dict->lctype) &&
is_capitalizable(sent->dict, unsplit_word) &&
!(unsplit_word->status & (WS_SPELL|WS_RUNON)))
{
int downcase_size = strlen(word)+MB_LEN_MAX+1;
char *const downcase = alloca(downcase_size);
downcase_utf8_str(downcase, word, downcase_size, sent->dict->lctype);
word_can_split |=
suffix_split(sent, unsplit_word, downcase);
lgdebug(+D_MS, "Tried to split lc=%s now can_split=%d\n",
downcase, word_can_split);
}
}
return word_can_split;
}
#if defined HAVE_HUNSPELL || defined HAVE_ASPELL
static bool is_known_word(Sentence sent, const char *word)
{
return (dict_has_word(sent->dict, word) ||
morpheme_split(sent, NULL, word));
}
static bool guess_misspelled_word(Sentence sent, Gword *unsplit_word,
Parse_Options opts)
{
Dictionary dict = sent->dict;
int runon_word_corrections = 0;
int num_guesses = 0;
int j, n;
char *sp = NULL;
const char *wp;
char **alternates = NULL;
const char *word = unsplit_word->subword;
if (spellcheck_test(dict->spell_checker, word)) return false;
n = spellcheck_suggest(dict->spell_checker, &alternates, word);
if (verbosity_level(+D_SW))
{
lgdebug(0, "spellcheck_suggest for %s:\\", word);
if (0 == n)
lgdebug(0, " (nothing)\n");
else
lgdebug(0, "\n\\");
for (j=0; j<n; j++)
{
if (n-1 != j)
lgdebug(0, "- %s\n\\", alternates[j]);
else
lgdebug(0, "- %s\n", alternates[j]);
}
}
for (int runon = 1; runon >=0; runon--)
{
for (j=0; j<n; j++)
{
Gword *altp;
if (alternates[j][0] == '\0') continue;
sp = strchr(alternates[j], ' ');
if (sp)
{
const char **runon_word = NULL;
bool unknown = false;
wp = alternates[j];
do
{
*sp = '\0';
unknown |= !is_known_word(sent, wp);
altappend(sent, &runon_word, wp);
wp = sp+1;
sp = strchr(wp, ' ');
} while (sp);
unknown |= !is_known_word(sent, wp);
altappend(sent, &runon_word, wp);
if (!unknown)
{
altp =
issue_word_alternative(sent, unsplit_word, "RO", 0,NULL,
altlen(runon_word),runon_word, 0,NULL);
for_word_alt(sent, altp, set_word_status,
(unsigned int []){WS_RUNON});
runon_word_corrections++;
}
free(runon_word);
alternates[j][0] = '\0';
}
else
{
if (runon == 1) continue;
if (is_known_word(sent, alternates[j]))
{
wp = alternates[j];
altp = issue_word_alternative(sent, unsplit_word,
REPLACEMENT_MARK "SP", 0,NULL,
1,&wp, 0,NULL);
for_word_alt(sent, altp, set_word_status,
(unsigned int[]){WS_SPELL});
num_guesses++;
}
if (num_guesses >= opts->use_spell_guess) break;
}
}
}
if (alternates) spellcheck_free_suggest(dict->spell_checker, alternates, n);
return ((num_guesses > 0) || (runon_word_corrections > 0));
}
#endif
static bool matchspan_fixed(const Afdict_class *mpunc, const char *w,
int *start, int *end)
{
const char *wend = &w[strlen(w)];
for (int i = 0; i < mpunc->length - mpunc->Nregexes; i++)
{
const char *affix = mpunc->string[i];
int f_start, f_end;
for (f_start = 1; w + f_start < wend; f_start++)
{
size_t sz = strcspn(affix, subscript_mark_str());
f_end = f_start + sz;
if (w + f_end > wend)
break;
if (0 == strncmp(w + f_start, affix, sz))
{
*start = f_start;
*end = f_end;
return true;
}
}
}
return false;
}
static bool mpunc_find(const Afdict_class *mpunc, int rnum, const char *w,
int *start, int *end)
{
bool rc;
if (rnum < 0)
rc = matchspan_fixed(mpunc, w, start, end);
else
rc = matchspan_regex(mpunc->regex[rnum], w, start, end);
if (unlikely(rc) && (unlikely(*start == 0) || unlikely(w[*end] == '\0')))
return false;
return rc;
}
static void prt_debug_mpunc(const char *label, const Afdict_class *mpunc,
int rnum, const char *w, int start, int end)
{
if (verbosity_level(+D_UN))
{
if (label != NULL)
prt_error("%s: ", label);
if (rnum >= 0)
prt_error("regex=/%s/ ", mpunc->regex[rnum]->pattern);
prt_error("matched \"%.*s\" in \"%s\" at [%d, %d)\n",
end-start, w+start, w, start, end);
}
}
static int split_mpunc(Sentence sent, const char *word, stripped_t split)
{
const Dictionary afdict = sent->dict->affix_table;
if (NULL == afdict) return 0;
if (utf8_strlen(word) <= 2) return 0;
const Afdict_class *mpunc = AFCLASS(afdict, AFDICT_MPUNC);
int Nsplit = 0;
while(*word != '\0')
{
#define NO_MATCH INT_MAX
int start = NO_MATCH, end = 0;
int matched_rnum = NO_MATCH;
for (int rnum = -1; rnum < mpunc->Nregexes; rnum++)
{
int ms, me;
if (unlikely(mpunc_find(mpunc, rnum, word, &ms, &me)))
{
if ((ms < start) || ((ms == start) && (me > end)))
{
start = ms;
end = me;
matched_rnum = rnum;
}
prt_debug_mpunc((matched_rnum == rnum) ? "Selected tmp":"Neglected",
mpunc, rnum, word, start, end);
}
}
if (start == NO_MATCH) break;
prt_debug_mpunc("Found", mpunc, matched_rnum, word, start, end);
const char *affix;
if (unlikely(start == end))
{
affix = NULL;
}
else
{
char *tmp = strndupa(word + start, end - start);
affix = string_set_add(tmp, sent->string_set);
}
if (start != 0)
{
if (Nsplit >= MAX_STRIP-1) goto max_strip_ovfl;
char *tmp = strndupa(word, start);
split[Nsplit++] = string_set_add(tmp, sent->string_set);
}
if (likely(affix != NULL))
{
if (Nsplit >= MAX_STRIP-1) goto max_strip_ovfl;
split[Nsplit++] = affix;
}
word += end;
}
if (unlikely(Nsplit > 0) && (*word != '\0'))
split[Nsplit++] = string_set_add(word, sent->string_set);
return Nsplit;
max_strip_ovfl:
lgdebug(+D_SW, "Too many tokens (>%d)\n", MAX_STRIP);
return 0;
}
static const char *strip_left(Sentence sent, const char * w,
stripped_t stripped,
size_t *n_stripped)
{
const Dictionary afdict = sent->dict->affix_table;
if (NULL == afdict) return (w);
const Afdict_class *lpunc = AFCLASS(afdict, AFDICT_LPUNC);
size_t l_strippable = lpunc->length;
size_t i;
*n_stripped = 0;
do
{
size_t rnum = 0;
for (i = 0; i < l_strippable; i++)
{
bool match_found = false;
const char *affix;
size_t sz;
if (i < l_strippable - lpunc->Nregexes)
{
affix = lpunc->string[i];
sz = strcspn(affix, subscript_mark_str());
if (strlen(w) < sz) continue;
match_found = (strncmp(w, affix, sz) == 0);
}
else
{
int start, end;
match_found = matchspan_regex(lpunc->regex[rnum], w, &start, &end);
if (unlikely(match_found && start != 0))
{
lgdebug(+D_UN, "/%s/ matches \"%s\" not at string start: "
"[%d, %d)\n", lpunc->regex[rnum]->pattern, w,
start, end);
match_found = false;
}
if (match_found)
{
sz = end - start;
affix = string_set_add(strndupa(w, sz), sent->string_set);
}
rnum++;
}
if (match_found)
{
lgdebug(+D_UN, "w='%s' found lpunc '%s'\n", w, affix);
stripped[(*n_stripped)++] = affix;
w += sz;
break;
}
}
} while ((i != l_strippable) && (*n_stripped < MAX_STRIP-1));
return (w);
}
static bool strip_right(Sentence sent,
const char *w,
const char **wend,
stripped_t stripped[MAX_STRIP_ALT],
size_t *n_stripped,
afdict_classnum classnum,
bool rootdigit,
int p)
{
const Dictionary dict = sent->dict;
const Dictionary afdict = dict->affix_table;
if (NULL == afdict) return false;
if (*n_stripped >= MAX_STRIP-1)
return false;
const char * temp_wend = *wend;
assert(temp_wend>w, "Unexpected empty-string word");
char *word = alloca(temp_wend-w+1);
Afdict_class *rword_list = AFCLASS(afdict, classnum);
size_t l_strippable = rword_list->length;
const char * const * rword = rword_list->string;
size_t sz;
size_t nrs = 0;
size_t i;
do
{
size_t altn = 0;
size_t rnum = 0;
for (i = 0; i < l_strippable; i++)
{
if (i < l_strippable - rword_list->Nregexes)
{
const char *t = rword[i];
size_t len = strcspn(t, subscript_mark_str());
if ((temp_wend-w) < (int)len) continue;
if (strncmp(temp_wend-len, t, len) == 0)
{
if (0 == altn)
{
lgdebug(+D_UN, "%d: %s: w='%s' rword '%.*s' at stripped[0,%zu]\n",
p, afdict_classname[classnum], temp_wend-len, (int)len, t, nrs);
stripped[1][*n_stripped+nrs] = NULL;
if (SUBSCRIPT_MARK == t[len])
{
stripped[0][*n_stripped+nrs] =
string_set_add(strndupa(t, len), sent->string_set);
}
else
{
stripped[0][*n_stripped+nrs] = t;
nrs++;
temp_wend -= len;
break;
}
altn = 1;
}
lgdebug(+D_UN, "%d: %s: w='%s' rword '%s' at stripped[%zu,%zu]\n",
p, afdict_classname[classnum], temp_wend-len, t, altn, nrs);
stripped[altn][*n_stripped+nrs] = t;
if (altn < MAX_STRIP_ALT-1)
stripped[altn+1][*n_stripped+nrs] = NULL;
if ((i+1 < l_strippable) && (0 == strncmp(rword[i+1], rword[i], len)))
{
altn++;
if (altn >= MAX_STRIP_ALT)
{
lgdebug(+1, "Warning: Ignoring %s: Too many %.*s units (>%d)\n",
rword[i], (int)len, rword[i], MAX_STRIP_ALT);
break;
}
}
else
{
nrs++;
temp_wend -= len;
break;
}
}
}
else if (classnum != AFDICT_UNITS)
{
int start, end;
word = strndupa(w, temp_wend - w);
bool match_found =
matchspan_regex(rword_list->regex[rnum], word, &start, &end);
if (unlikely(match_found && word[end] != '\0'))
{
lgdebug(+D_UN, "/%s/ matches \"%s\" not at string end: "
"[%d, %d)\n",
rword_list->regex[rnum]->pattern, word, start, end);
match_found = false;
}
if (match_found)
{
stripped[0][*n_stripped+nrs] =
string_set_add(word + start, sent->string_set);
stripped[1][*n_stripped+nrs] = NULL;
nrs++;
temp_wend -= end - start;
break;
}
rnum++;
}
}
} while ((i < l_strippable) && (temp_wend > w) && rootdigit &&
(*n_stripped+nrs < MAX_STRIP));
assert(w <= temp_wend, "A word should never start after its end...");
sz = temp_wend-w;
if ((0 == sz) && (1 == nrs))
return false;
strncpy(word, w, sz);
word[sz] = '\0';
if (rootdigit && (sz > 0) && !isdigit((unsigned int)temp_wend[-1]))
{
lgdebug(+D_UN, "%d: %s: return FALSE; root='%s' (0x%02x is not a digit)\n",
p, afdict_classname[classnum], word, (unsigned char)temp_wend[-1]);
return false;
}
lgdebug(+D_UN, "%d: %s: return %s; n_stripped=%zu+%zu, "
"wend='%s' temp_wend='%s'\n",
p, afdict_classname[classnum], (nrs>0)?"TRUE":"FALSE",
*n_stripped, nrs, *wend, temp_wend);
*n_stripped += nrs;
*wend = temp_wend;
return nrs > 0;
}
static void issue_r_stripped(Sentence sent,
Gword *unsplit_word,
const char *w,
const char *wend,
const stripped_t r_stripped[],
size_t n_stripped,
const char *label)
{
const size_t sz = (NULL==wend) ? strlen(w) : (size_t)(wend-w);
char *word;
const char **rtokens = NULL;
size_t ntokens = 0;
size_t i;
size_t altn = 0;
Gword *rstrip_alt;
if (0 != sz)
{
word = strndupa(w, sz);
altappend(sent, &rtokens, word);
lgdebug(+D_SW, "Issue root word w='%s' (alt %s)\n", word, label);
ntokens++;
}
for (i = n_stripped-1; (ssize_t)i >= 0; i--)
{
lgdebug(+D_SW, "Issue r_stripped w='%s' at [0,%zu] (%s)\n",
r_stripped[altn][i], i, label);
altappend(sent, &rtokens, r_stripped[altn][i]);
ntokens++;
}
rstrip_alt = issue_word_alternative(sent, unsplit_word, label,
0,NULL, ntokens, rtokens, 0,NULL);
for_word_alt(sent, rstrip_alt, set_word_status,
(unsigned int []){WS_INDICT|WS_REGEX});
for (i = 0; i < n_stripped; i++)
{
unsigned int position = (int)(n_stripped - i - 1);
if (ntokens > n_stripped) position++;
Gword *add_alt = for_word_alt(sent, rstrip_alt, gword_by_ordinal_position,
&position);
if (NULL == add_alt)
{
lgdebug(+1, "Warning: Internal error - r_striped alt too short.\n");
return;
}
add_alt->tokenizing_step = TS_DONE;
char *replabel = NULL;
if (NULL != r_stripped[1][i])
{
replabel = strdupa(label);
replabel[0] = REPLACEMENT_MARK[0];
}
for (size_t n = 1; n < MAX_STRIP_ALT; n++)
{
if (NULL == r_stripped[n][i]) break;
lgdebug(+D_SW, "Issue r_stripped w='%s' at [%zu,%zu] (%s)\n",
r_stripped[n][i], n, i, replabel);
Gword *altp = issue_word_alternative(sent, add_alt, replabel,
0,NULL, 1,&r_stripped[n][i], 0,NULL);
tokenization_done(sent, altp);
}
}
altfree(rtokens);
}
static void issue_dictcap(Sentence sent, bool is_cap,
Gword *unsplit_word, const char *word)
{
const char *dictcap[2];
Gword *altp;
dictcap[0] = is_cap ? CAP1st : CAPnon;
dictcap[1] = word;
lgdebug(+D_SW, "Adding %s word=%s RE=%s\n", dictcap[0], word,
NULL == unsplit_word->regex_name ? "" : unsplit_word->regex_name);
altp = issue_word_alternative(sent, unsplit_word, REPLACEMENT_MARK "dictcap",
0,NULL, 2,dictcap, 0,NULL);
if (NULL == altp)
{
prt_error("Warning: Word %s: Internal error: Issuing %s failed\n",
dictcap[1], dictcap[0]);
return;
}
altp->status |= WS_INDICT;
altp->morpheme_type = MT_FEATURE;
altp->tokenizing_step = TS_DONE;
if(is_cap && (NULL != unsplit_word->regex_name))
{
altp->next[0]->status |= WS_REGEX;
altp->next[0]->regex_name = unsplit_word->regex_name;
}
else
{
altp->status |= WS_FIRSTUPPER;
}
}
static const char *print_rev_word_array(Sentence sent, const char **w,
size_t size)
{
dyn_str *s = dyn_str_new();
int i;
const char *r;
for (i = size - 1; i >= 0; i--)
append_string(s, "[%d]='%s'%s", i, w[i], i>0 ? "," : "");
r = string_set_add(s->str, sent->string_set);
dyn_str_delete(s);
return r;
}
static bool is_re_capitalized(const char *regex_name)
{
return ((NULL != regex_name) && (NULL != strstr(regex_name, "CAPITALIZED")));
}
static void separate_word(Sentence sent, Gword *unsplit_word, Parse_Options opts)
{
Dictionary dict = sent->dict;
bool word_is_known = false;
bool word_can_split;
bool word_can_lrmsplit = false;
bool lc_word_is_in_dict = false;
bool stripped;
const char *wp;
const char *temp_wend;
size_t n_stripped = 0;
stripped_t x_stripped;
const char *units_wend = NULL;
size_t units_n_stripped = 0;
size_t sz = strlen(unsplit_word->subword);
const char *word = unsplit_word->subword;
const char *wend = &unsplit_word->subword[sz];
int downcase_size = sz+MB_LEN_MAX+1;
char *const downcase = alloca(downcase_size);
char *const temp_word = alloca(downcase_size);
char *const seen_word = alloca(downcase_size);
downcase[0] = '\0';
lgdebug(+D_SW, "Processing word: '%s'\n", word);
if (dict_has_word(dict, word))
{
lgdebug(+D_SW, "0: Adding '%s' as is, before split tries, status=%s\n",
word, gword_status(sent, unsplit_word));
issue_word_alternative(sent, unsplit_word, "W", 0,NULL, 1,&word, 0,NULL);
unsplit_word->status |= WS_INDICT;
word_is_known = true;
if (IS_GENERATION(sent->dict) && is_macro(word))
unsplit_word->tokenizing_step = TS_DONE;
}
if (unsplit_word->status & (WS_SPELL|WS_RUNON) ||
(unsplit_word->tokenizing_step == TS_DONE))
{
}
else
{
if ((MT_CONTR == unsplit_word->morpheme_type))
{
if (!word_is_known)
{
prt_error("Warning: Contracted word part %s is in '%s/%s' "
"but not in '%s/%s'\n", word,
dict->lang, dict->affix_table->name,
dict->lang, dict->name);
}
return;
}
wp = strip_left(sent, word, x_stripped, &n_stripped);
if (wp != word)
{
if (n_stripped >= MAX_STRIP-1)
{
lgdebug(+D_SW, "Left-strip of >= %d tokens\n", MAX_STRIP-1);
return;
}
if ('\0' != *wp)
x_stripped[n_stripped++] = wp;
issue_word_alternative(sent, unsplit_word, "rL",
0,NULL, n_stripped,x_stripped, 0,NULL);
if ('\0' == *wp)
{
if (n_stripped == 1)
{
lgdebug(+D_SW, "1: Word '%s' s a single token - done\n",
unsplit_word->subword);
return;
}
lgdebug(+D_SW, "1: Word '%s' consists of %zu left-puncts - "
"continue for possible regex alternative\n",
unsplit_word->subword, n_stripped);
}
n_stripped = 0;
word_can_lrmsplit = true;
}
lgdebug(+D_SW, "1: Continue with word %s status=%s\n",
word, gword_status(sent, unsplit_word));
stripped_t r_stripped[MAX_STRIP_ALT];
seen_word[0] = '\0';
do
{
int temp_n_stripped;
temp_n_stripped = n_stripped;
temp_wend = wend;
stripped = strip_right(sent, word, &wend, r_stripped, &n_stripped,
AFDICT_RPUNC, false, 2);
if (stripped)
{
sz = wend-word;
strncpy(temp_word, word, sz);
temp_word[sz] = '\0';
if (dict_has_word(dict, temp_word)) break;
wend = temp_wend;
n_stripped = temp_n_stripped;
}
units_wend = wend;
units_n_stripped = n_stripped;
stripped = strip_right(sent, word, &wend, r_stripped, &n_stripped,
AFDICT_UNITS, true, 3);
if (!stripped)
{
units_wend = NULL;
stripped = strip_right(sent, word, &wend, r_stripped, &n_stripped,
AFDICT_RPUNC, false, 4);
}
sz = wend-word;
strncpy(temp_word, word, sz);
temp_word[sz] = '\0';
if (0 == strcmp(temp_word, seen_word)) break;
strcpy(seen_word, temp_word);
} while (NULL == units_wend && stripped && (sz != 0) &&
!dict_has_word(dict, temp_word));
lgdebug(+D_SW, "After strip_right: n_stripped=(%s) "
"word='%s' wend='%s' units_wend='%s' temp_word='%s'\n",
print_rev_word_array(sent, r_stripped[0], n_stripped),
word, wend, units_wend, temp_word);
if (n_stripped >= MAX_STRIP-1)
{
lgdebug(+D_SW, "Right-strip of >= %d tokens\n", MAX_STRIP-1);
return;
}
if (units_n_stripped && (NULL != units_wend) && (0 != units_wend-word))
{
sz = units_wend-word;
strncpy(temp_word, word, sz);
temp_word[sz] = '\0';
if (dictionary_word_is_known(dict, temp_word))
{
issue_r_stripped(sent, unsplit_word, temp_word, NULL,
r_stripped, units_n_stripped, "rR2");
word_can_lrmsplit = true;
}
}
if (n_stripped > 0)
{
sz = wend-word;
strncpy(temp_word, word, sz);
temp_word[sz] = '\0';
if (!dictionary_word_is_known(dict, unsplit_word->subword) ||
(0 == sz) || dictionary_word_is_known(dict, temp_word))
{
issue_r_stripped(sent, unsplit_word, temp_word, NULL,
r_stripped, n_stripped, "rR3");
word_can_lrmsplit = true;
}
}
}
n_stripped = split_mpunc(sent, word, x_stripped);
if (n_stripped > 0)
{
issue_word_alternative(sent, unsplit_word, "M", 0,NULL,
n_stripped,x_stripped, 0,NULL);
word_can_lrmsplit = true;
}
lgdebug(+D_SW, "2: Continue with word=%s can_lrmsplit=%d status=%s\n",
word, word_can_lrmsplit, gword_status(sent, unsplit_word));
if ((dict->affix_table && dict->affix_table->anysplit) && !word_can_lrmsplit)
anysplit(sent, unsplit_word);
word_can_split = morpheme_split(sent, unsplit_word, word);
if (!word_is_known && (!word_can_split || is_contraction_word(dict, word)))
{
regex_guess(dict, word, unsplit_word);
}
lgdebug(+D_SW, "After split step, word=%s can_split=%d is_known=%d RE=%s\n",
word, word_can_split, word_is_known,
(NULL == unsplit_word->regex_name) ? "" : unsplit_word->regex_name);
if (is_utf8_upper(word, dict->lctype))
{
if (!test_enabled("dictcap"))
{
bool word_is_capitalizable = is_capitalizable(dict, unsplit_word);
if (word_is_capitalizable)
{
downcase_utf8_str(downcase, word, downcase_size, dict->lctype);
lc_word_is_in_dict = dict_has_word(dict, downcase);
if (lc_word_is_in_dict)
{
Gword *lc;
wp = downcase;
lgdebug(+D_SW, "Adding lc=%s is_capitalizable=1\n", wp);
lc = issue_word_alternative(sent, unsplit_word, "LC",
0,NULL, 1,&wp, 0,NULL);
if (NULL == lc)
{
prt_error("Warning: Word %s: Internal error: Issuing lc failed\n",
wp);
return;
}
lc->status |= WS_FIRSTUPPER;
}
else
{
}
}
lgdebug(+D_SW, "Word=%s lc=%s in_dict=%d is_known=%d can_split=%d "
"is_capitalizable=%d lc_is_in_dict=%d "
"is_entity=%d is_common_entity=%d\n",
word, downcase, !!(unsplit_word->status & WS_INDICT),
word_is_known, word_can_split,
word_is_capitalizable, lc_word_is_in_dict,
is_entity(dict, word), is_common_entity(dict, downcase));
if (!word_can_split && !word_is_known &&
(!word_is_capitalizable || (lc_word_is_in_dict &&
(is_common_entity(dict, downcase) || is_entity(dict, word)))))
{
if ((NULL != unsplit_word->regex_name))
{
lgdebug(+D_SW, "Adding uc word=%s RE=%s\n", word,
unsplit_word->regex_name);
issue_word_alternative(sent, unsplit_word, "REuc",
0,NULL, 1,&word, 0,NULL);
word_is_known = true;
if (test_enabled("is_entity") && is_entity(dict, word))
prt_error("is_entity(%s): %s\n", word, sent->orig_sentence);
}
}
word_is_known |= lc_word_is_in_dict;
}
else
{
if (!dict_has_word(dict, CAP1st) ||
!dict_has_word(dict, CAPnon))
{
prt_error("Error: Missing " CAP1st "/" CAPnon "in the dict\n");
return;
}
if (!(unsplit_word->status & WS_INDICT) &&
is_re_capitalized(unsplit_word->regex_name))
{
issue_dictcap(sent, true, unsplit_word, word);
}
downcase_utf8_str(downcase, word, downcase_size, dict->lctype);
if (dictionary_word_is_known(sent->dict, downcase))
issue_dictcap(sent, false, unsplit_word, downcase);
word_is_known = true;
}
}
if (!(word_is_known || lc_word_is_in_dict ||
(word_can_split && !is_contraction_word(dict, word))))
{
if ((NULL != unsplit_word->regex_name))
{
lgdebug(+D_SW, "Adding word '%s' for regex, match=%s\n",
word, unsplit_word->regex_name);
issue_word_alternative(sent, unsplit_word, "RE",
0,NULL, 1,&word, 0,NULL);
word_is_known = true;
}
}
word_is_known |= word_can_split;
#if defined HAVE_HUNSPELL || defined HAVE_ASPELL
if (!word_can_lrmsplit && !word_is_known &&
!contains_digits(word, dict->lctype) &&
!is_proper_name(word, dict->lctype) &&
opts->use_spell_guess && dict->spell_checker)
{
bool spell_suggest = guess_misspelled_word(sent, unsplit_word, opts);
lgdebug(+D_SW, "Spell suggest=%d\n", spell_suggest);
}
#endif
lgdebug(+D_SW, "END: Word '%s' in_dict=%d is_known=%d status=%s\n",
unsplit_word->subword, !!(unsplit_word->status & WS_INDICT),
word_is_known, gword_status(sent, unsplit_word));
#if 0
if (!word_is_known &&
!(unsplit_word->status & (WS_INDICT|WS_REGEX)))
unsplit_word->status |= WS_UNKNOWN;
#endif
}
static Gword *issue_sentence_word(const Sentence sent, const char *const s)
{
Gword *new_word;
Gword *last_word = sent->last_word;
assert(NULL!=last_word, "Start infrastructure subword is missing");
assert(NULL!=s, "subword must not be NULL");
assert('\0'!=s[0], "subword must not be an empty-string: "
"Last subword issued: '%s'", last_word->subword);
new_word = gword_new(sent, s);
new_word->unsplit_word = sent->wordgraph;
new_word->label = "S";
gwordlist_append(&last_word->next, new_word);
gwordlist_append(&new_word->prev, last_word);
gwordqueue_add(sent, new_word);
return new_word;
}
static void add_gword(Sentence sent, const char *w, const char *wend,
Morpheme_type morpheme_type)
{
const size_t sz = (NULL == wend) ? strlen(w) : (size_t)(wend - w);
char *const word = alloca(sz+1);
Gword *new_word;
strncpy(word, w, sz);
word[sz] = '\0';
new_word = issue_sentence_word(sent, word);
new_word->morpheme_type = morpheme_type;
new_word->alternative_id = sent->wordgraph;
if (NULL != wend)
{
new_word->start = w;
new_word->end = wend;
}
if (MT_WORD != morpheme_type)
{
new_word->tokenizing_step = TS_DONE;
if (MT_WALL == morpheme_type)
{
new_word->status |= WS_INDICT;
if (MT_INFRASTRUCTURE == new_word->prev[0]->morpheme_type)
new_word->start = sent->orig_sentence;
else
new_word->start = sent->orig_sentence + strlen(sent->orig_sentence);
new_word->end = new_word->start;
}
}
}
static void wordgraph_create(Sentence const sent)
{
Gword *new_word;
assert(NULL==sent->last_word, "wordgraph exists");
new_word = gword_new(sent, sent->orig_sentence);
assert(NULL!=sent->orig_sentence, "Sentence exists");
assert(NULL==sent->wordgraph, "wordgraph exists");
sent->wordgraph = sent->last_word = new_word;
new_word->label = "D";
new_word->morpheme_type = MT_INFRASTRUCTURE;
}
static void wordgraph_terminator(Sentence const sent)
{
assert(NULL != sent->last_word, "No wordgraph");
add_gword(sent, "(T)", NULL, MT_INFRASTRUCTURE);
sent->last_word->unsplit_word = NULL;
sent->last_word->label = "D";
sent->last_word->tokenizing_step = TS_DONE;
}
#define TOLERATE_BAD_UTF
#ifdef TOLERATE_BAD_UTF
#define BAD_UTF { nb = 0; word_start ++; continue; }
#else
#define BAD_UTF goto failure;
#endif
bool separate_sentence(Sentence sent, Parse_Options opts)
{
Dictionary dict = sent->dict;
sent->length = 0;
if (0 == sent->orig_sentence[0]) return false;
wordgraph_create(sent);
if (dict->left_wall_defined)
add_gword(sent, LEFT_WALL_WORD, NULL, MT_WALL);
mbstate_t mbs;
memset(&mbs, 0, sizeof(mbs));
const char * word_start = sent->orig_sentence;
#ifdef DEBUG_WORDGRAPH
if (SYNTHETIC_SENTENCE_MARK == sent->orig_sentence[0]) word_start++;
#endif
for(;;)
{
wchar_t c;
int nb = mbrtowc(&c, word_start, MB_CUR_MAX, &mbs);
if (0 > nb) BAD_UTF;
while (is_space(c, dict->lctype))
{
word_start += nb;
nb = mbrtowc(&c, word_start, MB_CUR_MAX, &mbs);
if (0 == nb) break;
if (0 > nb) BAD_UTF;
}
if ('\0' == *word_start) break;
const char * word_end = word_start;
nb = mbrtowc(&c, word_end, MB_CUR_MAX, &mbs);
if (0 > nb) BAD_UTF;
while (!is_space(c, dict->lctype) && (c != 0) && (0 < nb))
{
word_end += nb;
nb = mbrtowc(&c, word_end, MB_CUR_MAX, &mbs);
if (0 > nb) break;
}
if (0 > nb) BAD_UTF;
add_gword(sent, word_start, word_end, MT_WORD);
word_start = word_end;
if ('\0' == *word_start) break;
}
if (dict->right_wall_defined)
add_gword(sent, RIGHT_WALL_WORD, NULL, MT_WALL);
wordgraph_terminator(sent);
Gword *word;
while ((word = wordgraph_getqueue_word(sent)))
{
if (TS_DONE == word->tokenizing_step)
{
remqueue_gword(sent);
continue;
}
#ifdef DEBUG_WORDGRAPH
if (SYNTHETIC_SENTENCE_MARK == sent->orig_sentence[0])
synthetic_split(sent, word);
#else
if (0)
;
#endif
else
separate_word(sent, word, opts);
word->tokenizing_step = TS_DONE;
}
for (word = sent->wordgraph; NULL != word->next; word = word->next[0])
{
if ((word->morpheme_type != MT_INFRASTRUCTURE) &&
(word->morpheme_type != MT_WALL))
{
return true;
}
}
wordgraph_delete(sent);
return false;
#ifndef TOLERATE_BAD_UTF
failure:
#ifdef _WIN32
prt_error("Unable to process UTF8 input string.\n");
#else
prt_error("Unable to process UTF8 input string in current locale %s\n",
nl_langinfo(CODESET));
#endif
wordgraph_delete(sent);
return false;
#endif
}
static Word *word_new(Sentence sent)
{
const size_t len = sent->length;
sent->word = realloc(sent->word, (len+1)*sizeof(*sent->word));
memset(&sent->word[len], 0, sizeof(sent->word[0]));
sent->length++;
return &sent->word[len];
}
void free_words(Sentence sent)
{
for (WordIdx i = 0; i < sent->length; i++)
{
altfree(sent->word[i].alternatives);
free(sent->word[i].gwords);
}
free(sent->word);
}
bool word0_set(Sentence sent, char *w, Parse_Options opts)
{
word_new(sent);
altappend(sent, &sent->word[0].alternatives, w);
return setup_dialect(sent->dict, opts);
}
#ifdef FIXIT
static bool same_unsplit_word(Sentence sent, const Gword *w1, const Gword *w2)
{
return ((w1->unsplit_word == w2->unsplit_word) &&
(w1->unsplit_word != sent->wordgraph));
}
#endif
#define D_WPP 8
static void print_wordgraph_pathpos(const Wordgraph_pathpos *wp)
{
size_t i = 0;
if (NULL == wp)
{
lgdebug(+D_WPP, "Empty\n");
return;
}
lgdebug(+D_WPP, "\n");
for (; NULL != wp->word; wp++)
{
lgdebug(D_WPP, "%zu: %zu:word '%s', same=%d used=%d level=%zu\n",
i++, wp->word->node_num, wp->word->subword, wp->same_word,
wp->used, wp->word->hier_depth);
}
}
#undef D_WPP
#define D_FW 8
void flatten_wordgraph(Sentence sent, Parse_Options opts)
{
Wordgraph_pathpos *wp_new = NULL;
Wordgraph_pathpos *wp_old = NULL;
Wordgraph_pathpos *wpp_new, *wpp_old;
Gword *wg_word;
Gword **next;
const Gword *last_unsplit_word = NULL;
size_t max_words = 0;
bool right_wall_encountered = false;
assert(0 == sent->length, "Word array already exists.");
for (wg_word = sent->wordgraph->chain_next; wg_word;
wg_word = wg_word->chain_next)
{
wordgraph_hier_position(wg_word);
max_words++;
}
for (next = sent->wordgraph->next; *next; next++)
{
wordgraph_pathpos_add(&wp_new, *next,
false, false,
true);
}
do
{
assert(NULL != wp_new, "pathpos word queue is empty");
wp_old = wp_new;
wp_new = NULL;
print_wordgraph_pathpos(wp_old);
assert(0 < max_words, "Too many words (it may be an infinite loop)");
max_words--;
Word *wa_word = word_new(sent);
size_t curr_widx = sent->length - 1;
const Gword *unsplit_word = wp_old->word;
if (MT_INFRASTRUCTURE != unsplit_word->morpheme_type)
{
unsplit_word = wg_get_sentence_word(sent, (Gword *)unsplit_word);
if (unsplit_word != last_unsplit_word)
{
wa_word->unsplit_word = unsplit_word->subword;
last_unsplit_word = unsplit_word;
}
}
for (wpp_old = wp_old; NULL != wpp_old->word; wpp_old++)
{
wg_word = wpp_old->word;
if (MT_INFRASTRUCTURE == wg_word->morpheme_type)
continue;
if (wpp_old->same_word)
{
wa_word->optional = true;
}
else
{
assert(!wpp_old->used, "Word %zu:%s has been used",
wg_word->node_num, wpp_old->word->subword);
assert(!right_wall_encountered, "Extra word");
wg_word->sent_wordidx = curr_widx;
gwordlist_append(&wa_word->gwords, wg_word);
if ((MT_WALL == wg_word->morpheme_type) &&
(0 == strcmp(wg_word->subword, RIGHT_WALL_WORD)))
right_wall_encountered = true;
wpp_old->used = true;
}
}
for (wpp_old = wp_old; NULL != wpp_old->word; wpp_old++)
{
wg_word = wpp_old->word;
if (MT_INFRASTRUCTURE == wg_word->morpheme_type)
continue;
assert(NULL != wg_word->next[0], "Bad wordgraph: "
"'%s'->next[0]==NULL", wg_word->subword);
assert((NULL != wg_word->next[0]->prev) &&
(NULL != wg_word->next[0]->prev[0]), "Bad wordgraph: "
"'%s'->next[0]: No prev", wg_word->subword);
for (next = wg_word->next; NULL != *next; next++)
{
if (wg_word->hier_depth <= (*next)->hier_depth &&
(NULL == (*next)->prev[1]))
{
lgdebug(+D_FW, "Word %zu:%s(%zu) next %zu:%s(%zu) next_ok\n",
wg_word->node_num, wg_word->subword, wg_word->hier_depth,
(*next)->node_num, (*next)->subword, (*next)->hier_depth);
wpp_old->next_ok = true;
break;
}
}
if (wpp_old->next_ok)
{
lgdebug(+D_FW, "Advancing %zu:%s next_ok\n", wg_word->node_num,
wg_word->subword);
for (next = wg_word->next; NULL != *next; next++)
{
wordgraph_pathpos_add(&wp_new, *next,
false, false,
true);
}
}
}
for (wpp_old = wp_old; NULL != wpp_old->word; wpp_old++)
{
wg_word = wpp_old->word;
if (!wpp_old->next_ok)
{
bool same_alternative = false;
if (MT_INFRASTRUCTURE == wg_word->morpheme_type)
continue;
if (NULL != wp_new)
{
for (next = wg_word->next; NULL != *next; next++)
{
for (wpp_new = wp_new; NULL != wpp_new->word; wpp_new++)
{
if ((wpp_new->word != *next) &&
in_same_alternative(wpp_new->word, *next))
{
lgdebug(+D_FW, "same_alternative: %zu:%s and %zu:%s\n",
wpp_new->word->node_num, wpp_new->word->subword,
(*next)->node_num, (*next)->subword);
same_alternative = true;
break;
}
}
if (same_alternative) break;
}
}
lgdebug(+D_FW, "Advancing %zu:%s: ", wg_word->node_num,
wg_word->subword);
if (same_alternative)
{
lgdebug(D_FW, "No (same alt) used=%d\n", wpp_old->used);
wordgraph_pathpos_add(&wp_new, wg_word,
wpp_old->used, true,
true);
}
else
{
bool added = false;
for (next = wg_word->next; NULL != *next; next++)
added |= wordgraph_pathpos_add(&wp_new, *next,
false,
false,
true);
if (added)
{
lgdebug(D_FW, "Yes\n");
}
else
{
lgdebug(D_FW, "No (existing)\n");
}
}
}
}
wordgraph_pathpos_free(wp_old);
assert(wp_new != NULL, "No new wordgraph path");
} while ((NULL != wp_new[1].word) ||
(wp_new[0].word->morpheme_type != MT_INFRASTRUCTURE));
wp_new[0].word->sent_wordidx = sent->length;
wordgraph_pathpos_free(wp_new);
}
#undef D_FW