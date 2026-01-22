#ifndef _OPENCOG_DICTIONARY_H
#define _OPENCOG_DICTIONARY_H
#include <opencog/atomspace/AtomSpace.h>
namespace opencog
{
typedef std::map<Handle, HandleSeq> HandleSeqMap;
class Dictionary
{
AtomSpace* _as;
HandlePairSeq _pole_pairs;
HandleSeqMap _connectables;
HandleSeqMap _entries;
public:
Dictionary(AtomSpace*);
void add_pole_pair(const Handle&, const Handle&);
HandleSeq joints(const Handle&) const;
void add_to_lexis(const Handle&);
void add_to_lexis(const HandleSet& lex) {
for (const Handle& h: lex) add_to_lexis(h);
}
const HandleSeq& connectables(const Handle&) const;
const HandleSeq& entries(const Handle&) const;
};
}
#endif