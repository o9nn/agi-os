#ifndef _WORDGRAPH_H
#define _WORDGRAPH_H
#include "api-structures.h"
#ifdef USE_WORDGRAPH_DISPLAY
#define lo(l) (l-'a')
#define WGR_SUB      (1<<lo('s'))
#define WGR_COMPACT  (1<<lo('c'))
#define WGR_PREV     (1<<lo('p'))
#define WGR_UNSPLIT  (1<<lo('u'))
#define WGR_DBGLABEL (1<<lo('d'))
#define WGR_DOTDEBUG (1<<lo('h'))
#define WGR_LEGEND   (1<<lo('l'))
#define WGR_X11      (1<<lo('x'))
#endif
#define IS_SENTENCE_WORD(sent, gword) (gword->unsplit_word == sent->wordgraph)
Gword *gword_new(Sentence, const char *);
void gwordlist_append(Gword ***, Gword *);
void gwordlist_free(Gword **);
size_t gwordlist_len(const Gword **);
const Gword ** gwordlist_copy(const Gword **);
void gwordlist_cfree(const Gword **);
void gword_set_print(const gword_set *);
void print_lwg_path(Gword **, const char *);
Gword *wg_get_sentence_word(const Sentence, Gword *);
#if 0
void gwordlist_append_list(const Gword ***, const Gword **);
#endif
void wordgraph_delete(Sentence);
const Gword **wordgraph_hier_position(Gword *);
void print_hier_position(const Gword *);
bool in_same_alternative(Gword *, Gword *);
Gword *find_real_unsplit_word(Gword *, bool);
size_t wordgraph_pathpos_len(Wordgraph_pathpos *);
Wordgraph_pathpos *wordgraph_pathpos_resize(Wordgraph_pathpos *, size_t);
bool wordgraph_pathpos_add(Wordgraph_pathpos **, Gword *, bool, bool, bool);
void wordgraph_pathpos_free(Wordgraph_pathpos *);
const char *gword_status(Sentence, const Gword *);
const char *gword_morpheme(Sentence sent, const Gword *w);
#endif