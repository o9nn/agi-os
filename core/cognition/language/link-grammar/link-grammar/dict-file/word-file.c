#include "error.h"
#include "dict-common/dict-common.h"
#include "dict-common/dict-defines.h"
#include "dict-common/dict-internals.h"
#include "dict-common/dict-utils.h"
#include "dict-common/file-utils.h"
#include "string-set.h"
#include "read-dict.h"
#include "word-file.h"
static const char * get_a_word(Dictionary dict, FILE * fp)
{
char word[MAX_WORD+4];
const char * s;
int c, j;
do {
c = fgetc(fp);
} while ((c != EOF) && lg_isspace(c));
if (c == EOF) return NULL;
for (j=0; (j <= MAX_WORD-1) && (!lg_isspace(c)) && (c != EOF); j++)
{
word[j] = c;
c = fgetc(fp);
}
if (j >= MAX_WORD) {
word[MAX_WORD] = '\0';
prt_error("The dictionary contains a word that is too long: %s\n", word);
return "";
}
word[j] = '\0';
patch_subscript(word);
s = string_set_add(word, dict->string_set);
return s;
}
Dict_node * read_word_file(Dictionary dict, Dict_node * dn, char * filename)
{
Word_file * wf;
FILE * fp;
const char * s;
filename += 1;
if ((fp = dictopen(filename, "r")) == NULL) {
return NULL;
}
wf = malloc(sizeof (Word_file));
wf->file = string_set_add(filename, dict->string_set);
wf->next = dict->word_file_header;
dict->word_file_header = wf;
while ((s = get_a_word(dict, fp)) != NULL) {
if ('\0' == s[0])
{
fclose(fp);
free_insert_list(dn);
return NULL;
}
Dict_node * dn_new = dict_node_new();
dn_new->left = dn;
dn = dn_new;
dn->string = s;
dn->file = wf->file;
}
fclose(fp);
return dn;
}
void free_Word_file(Word_file * wf)
{
Word_file *wf1;
for (;wf != NULL; wf = wf1) {
wf1 = wf->next;
free(wf);
}
}