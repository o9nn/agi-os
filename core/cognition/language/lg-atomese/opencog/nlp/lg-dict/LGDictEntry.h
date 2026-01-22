#ifndef _OPENCOG_LG_DICT_ENTRY_H
#define _OPENCOG_LG_DICT_ENTRY_H
#include <link-grammar/link-includes.h>
#include <opencog/atoms/core/FunctionLink.h>
#include <opencog/nlp/types/atom_types.h>
namespace opencog
{
class LGDictEntry : public FunctionLink
{
protected:
void init();
public:
LGDictEntry(const HandleSeq&&, Type=LG_DICT_ENTRY);
LGDictEntry(const LGDictEntry&) = delete;
LGDictEntry& operator=(const LGDictEntry&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
typedef std::shared_ptr<LGDictEntry> LGDictEntryPtr;
static inline LGDictEntryPtr LGDictEntryCast(const Handle& h)
{ return std::dynamic_pointer_cast<LGDictEntry>(h); }
static inline LGDictEntryPtr LGDictEntryCast(AtomPtr a)
{ return std::dynamic_pointer_cast<LGDictEntry>(a); }
#define createLGDictEntry std::make_shared<LGDictEntry>
class LGHaveDictEntry : public Link
{
protected:
void init();
public:
LGHaveDictEntry(const HandleSeq&&, Type=LG_HAVE_DICT_ENTRY);
LGHaveDictEntry(const LGHaveDictEntry&) = delete;
LGHaveDictEntry& operator=(const LGHaveDictEntry&) = delete;
virtual bool is_evaluatable() const { return true; }
virtual TruthValuePtr evaluate(AtomSpace*, bool);
static Handle factory(const Handle&);
};
typedef std::shared_ptr<LGHaveDictEntry> LGHaveDictEntryPtr;
static inline LGHaveDictEntryPtr LGHaveDictEntryCast(const Handle& h)
{ return std::dynamic_pointer_cast<LGHaveDictEntry>(h); }
static inline LGHaveDictEntryPtr LGHaveDictEntryCast(AtomPtr a)
{ return std::dynamic_pointer_cast<LGHaveDictEntry>(a); }
#define createLGHaveDictEntry std::make_shared<LGHaveDictEntry>
}
#endif