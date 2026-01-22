#ifndef _TOKENIZE_H
#define _TOKENIZE_H
#include "api-types.h"
#include "link-includes.h"
bool separate_sentence(Sentence, Parse_Options);
bool sentence_in_dictionary(Sentence);
void flatten_wordgraph(Sentence, Parse_Options);
void tokenization_done(Sentence, Gword *);
void free_words(Sentence);
void altappend(Sentence, const char ***, const char *);
bool word0_set(Sentence, char *, Parse_Options);
Gword *issue_word_alternative(Sentence sent, Gword *unsplit_word,
const char *label,
int prefnum, const char * const *prefix,
int stemnum, const char * const *stem,
int suffnum, const char * const *suffix);
#endif