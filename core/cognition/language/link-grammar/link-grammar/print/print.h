#ifndef _PRINT_H
#define _PRINT_H
#include "print/print-util.h"
#include "link-includes.h"
#define LEFT_WALL_DISPLAY  ("LEFT-WALL")
#define RIGHT_WALL_DISPLAY ("RIGHT-WALL")
void   print_disjunct_counts(Sentence sent);
struct tokenpos;
void   print_sentence_word_alternatives(dyn_str *, Sentence, bool,
char * (*)(Dictionary, const char *, const void **), const void **arg,
struct tokenpos *);
void print_sentence_context(Sentence, dyn_str*);
#endif