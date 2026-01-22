#include "dict-common/dict-api.h"
struct Word_file_struct
{
Word_file * next;
const char *file;
};
void free_Word_file(Word_file * wf);
Dict_node * read_word_file(Dictionary dict, Dict_node * dn, char * filename);