#ifndef _OPENCOG_LG_DICT_READER_H
#define _OPENCOG_LG_DICT_READER_H
#include <link-grammar/dict-api.h>
#include "LGDictExpContainer.h"
namespace opencog
{
HandleSeq getDictEntry(Dictionary, const std::string& word);
bool haveDictEntry(Dictionary, const std::string& word);
}
#endif