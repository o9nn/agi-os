#include <assert.h>
#include <stdlib.h>
#include <string.h>
#ifdef _MSC_VER
#define LINK_GRAMMAR_DLL_EXPORT 0
#endif
#include <dict-api.h>
#include <dict-defines.h>
#include "generator-utilities.h"
static const char *cond_subscript(const char *ow, bool leave_subscript)
{
const char *sm = strchr(ow, SUBSCRIPT_MARK);
if (sm == NULL) return ow;
static char w[MAX_WORD + 1];
strcpy(w, ow);
w[sm - ow] = leave_subscript ? SUBSCRIPT_DOT : '\0';
return w;
}
static void print_sent(size_t nwords, const char** words,
bool subscript)
{
for(WordIdx w = 0; w < nwords; w++)
{
assert(NULL != words[w] );
printf("%s", cond_subscript(words[w], subscript));
if (w < nwords-1) printf(" ");
}
printf("\n");
}
static void sent_odom(const Category* catlist,
size_t nwords, const char** words,
const Category_cost** cclist,
unsigned int* cclen,
const char** selected_words,
void (*callback)(void*),
void* data,
WordIdx cur_word)
{
if (cur_word >= nwords)
{
callback(data);
return;
}
const Category_cost *cc = cclist[cur_word];
if (cc == NULL)
{
selected_words[cur_word] = words[cur_word];
sent_odom(catlist, nwords, words,
cclist, cclen, selected_words, callback, data, cur_word+1);
return;
}
for (unsigned int catidx = 0; catidx < cclen[cur_word]; catidx++)
{
unsigned int catno = cc[catidx].num - 1;
unsigned int num_words = catlist[catno].num_words;
for (unsigned int widx = 0; widx < num_words; widx++)
{
selected_words[cur_word] = catlist[catno].word[widx];
sent_odom(catlist, nwords, words,
cclist, cclen, selected_words, callback, data, cur_word+1);
}
}
}
static void count_choices(void* data)
{
size_t* count = data;
(*count) ++;
}
typedef struct {
size_t nwords;
const char** selected_words;
bool subscript;
double chance;
size_t nprinted;
} sent_data;
static void print_word_choices(void* data)
{
sent_data* sd = data;
bool prt = (1.0 <= sd->chance) ||
(((double) rand()) / ((double) RAND_MAX) < sd->chance);
if (prt)
{
print_sent(sd->nwords, sd->selected_words, sd->subscript);
sd->nprinted ++;
}
}
static size_t print_several(const Category* catlist,
Linkage linkage, size_t nwords, const char** words,
bool subscript, double fraction)
{
const Category_cost* cclist[MAX_SENTENCE];
unsigned int cclen[MAX_SENTENCE];
const char* selected_words[MAX_SENTENCE];
for(WordIdx w = 0; w < nwords; w++)
{
const Category_cost* cc = linkage_get_categories(linkage, w);
cclist[w] = cc;
unsigned int dj_num_cats = 0;
cclen[w] = 1;
selected_words[w] = NULL;
if (NULL != cclist[w])
{
while (cc[dj_num_cats].num != 0) dj_num_cats++;
assert(dj_num_cats != 0 );
cclen[w] = dj_num_cats;
}
}
size_t num_word_choices = 0;
sent_odom(catlist, nwords, words,
cclist, cclen, selected_words,
count_choices, &num_word_choices, 0);
double chance = fraction / ((double) num_word_choices);
printf("# num possible word choices for linkage = %zu chance to print=%f\n",
num_word_choices, chance);
sent_data sd;
sd.nwords = nwords;
sd.selected_words = selected_words;
sd.subscript = subscript;
sd.chance = chance;
sd.nprinted = 0;
while (0 == sd.nprinted)
{
sent_odom(catlist, nwords, words,
cclist, cclen, selected_words,
print_word_choices, &sd, 0);
}
return sd.nprinted;
}
static const char *select_random_word(const Category *catlist,
const Category_cost *cc,
WordIdx w)
{
unsigned int dj_num_cats = 0;
while (cc[dj_num_cats].num != 0)
dj_num_cats++;
assert(dj_num_cats != 0 );
unsigned int r = (unsigned int)rand();
unsigned int catidx = r % dj_num_cats;
unsigned int catnum = cc[catidx].num - 1;
if (verbosity_level >= 5)
{
printf("Word %zu: r=%08x category %u/%u \"%u\";", w, r,
catidx, dj_num_cats, catnum);
}
unsigned int num_words = catlist[catnum].num_words;
r = (unsigned int)rand();
unsigned int dict_word_idx = r % num_words;
const char *word = catlist[catnum].word[dict_word_idx];
if (verbosity_level >= 5)
{
printf(" r=%08x word %u/%u \"%s\"\n",
r, dict_word_idx, num_words, word);
}
return word;
}
size_t print_sentences(const Category* catlist,
Linkage linkage, size_t nwords, const char** words,
bool subscript, double max_samples)
{
if (1.0 < max_samples)
{
return print_several(catlist, linkage, nwords, words,
subscript, max_samples);
}
for(WordIdx w = 0; w < nwords; w++)
{
const Category_cost* cc = linkage_get_categories(linkage, w);
if (cc == NULL)
{
printf("%s", cond_subscript(words[w], subscript));
}
else
{
const char *word = select_random_word(catlist, cc, w);
printf("%s", cond_subscript(word, subscript));
}
if (w < nwords-1) printf(" ");
}
printf("\n");
return 1;
}
void dump_categories(Dictionary dict, const Category *catlist)
{
#define TAB "   "
const int maxcol = 60;
printf(TAB"\"categories\": [\n");
for (unsigned int n = 0; catlist[n].num_words != 0; n++)
{
printf(TAB"{\n");
if (catlist[n].name[0] != '\0')
printf(TAB TAB"\"name\": %s,\n", catlist[n].name);
printf(TAB TAB"\"category_num\": %u,\n", n + 1);
printf(TAB TAB"\"exp\": %s,\n", lg_exp_stringify(catlist[n].exp));
printf(TAB TAB"\"num_words\": %u,\n", catlist[n].num_words);
printf(TAB TAB"\"words\": [\n");
printf(TAB TAB TAB);
int currcol = 0;
for (unsigned int wi = 0; wi < catlist[n].num_words; wi++)
{
const char *word = catlist[n].word[wi];
if ((currcol > 0) && currcol + (int)strlen(word) > maxcol)
{
if (wi != catlist[n].num_words - 1) printf("\n"TAB TAB TAB);
currcol = 0;
}
currcol += printf("\"%s\"", word);
if (wi != catlist[n].num_words - 1)
{
printf(",");
currcol++;
}
else
{
printf("\n");
}
}
printf(TAB TAB"]\n");
printf(TAB"}");
if (catlist[n+1].num_words != 0)
printf(",");
printf("\n");
}
printf(TAB"]\n");
printf("}\n");
}