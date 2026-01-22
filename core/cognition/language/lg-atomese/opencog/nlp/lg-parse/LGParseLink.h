#ifndef _OPENCOG_LG_PARSE_H
#define _OPENCOG_LG_PARSE_H
#include <link-grammar/link-includes.h>
#include <opencog/atoms/core/FunctionLink.h>
#include <opencog/nlp/types/atom_types.h>
namespace opencog
{
class LGParseLink : public FunctionLink
{
protected:
void init();
std::string get_word_string(Linkage, int, const char*) const;
HandleSeq make_lg_conseq(Linkage, int, AtomSpace*) const;
HandleSeq make_conseq(Linkage, int, const char*, AtomSpace*) const;
ValuePtr make_djs(Linkage, const char*, AtomSpace*) const;
ValuePtr make_sects(Linkage, const char*, AtomSpace*) const;
ValuePtr make_bonds(Linkage, const char*, AtomSpace*) const;
ValuePtr make_words(Linkage, const char*, AtomSpace*) const;
Handle cvt_linkage(Linkage, int, const char*, const char*,
bool, AtomSpace*) const;
public:
LGParseLink(const HandleSeq&&, Type=LG_PARSE_LINK);
LGParseLink(const LGParseLink&) = delete;
LGParseLink& operator=(const LGParseLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
class LGParseMinimal : public LGParseLink
{
public:
LGParseMinimal(const HandleSeq&&, Type=LG_PARSE_MINIMAL);
LGParseMinimal(const LGParseMinimal&) = delete;
LGParseMinimal& operator=(const LGParseMinimal&) = delete;
};
class LGParseDisjuncts : public LGParseLink
{
public:
LGParseDisjuncts(const HandleSeq&&, Type=LG_PARSE_DISJUNCTS);
LGParseDisjuncts(const LGParseDisjuncts&) = delete;
LGParseDisjuncts& operator=(const LGParseDisjuncts&) = delete;
};
class LGParseSections : public LGParseLink
{
public:
LGParseSections(const HandleSeq&&, Type=LG_PARSE_SECTIONS);
LGParseSections(const LGParseSections&) = delete;
LGParseSections& operator=(const LGParseSections&) = delete;
};
class LGParseBonds : public LGParseLink
{
public:
LGParseBonds(const HandleSeq&&, Type=LG_PARSE_BONDS);
LGParseBonds(const LGParseBonds&) = delete;
LGParseBonds& operator=(const LGParseBonds&) = delete;
};
LINK_PTR_DECL(LGParseLink)
#define createLGParseLink CREATE_DECL(LGParseLink)
LINK_PTR_DECL(LGParseMinimal)
#define createLGParseMinimal CREATE_DECL(LGParseMinimal)
LINK_PTR_DECL(LGParseDisjuncts)
#define createLGParseDisjuncts CREATE_DECL(LGParseDisjuncts)
LINK_PTR_DECL(LGParseSections)
#define createLGParseSections CREATE_DECL(LGParseSections)
LINK_PTR_DECL(LGParseBonds)
#define createLGParseBonds CREATE_DECL(LGParseBonds)
}
#endif