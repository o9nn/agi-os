#ifndef _OPENCOG_LG_DICT_EXP_H
#define _OPENCOG_LG_DICT_EXP_H
#include <link-grammar/dict-api.h>
#include <opencog/atomspace/AtomSpace.h>
namespace opencog
{
class LGDictExpContainer
{
public:
LGDictExpContainer(Exp_type, const Exp* exp);
LGDictExpContainer(Exp_type, const std::vector<LGDictExpContainer>&);
HandleSeq to_handle(const Handle& h);
private:
void basic_flatten();
void basic_dnf();
void basic_normal_order();
Exp_type m_type;
std::string m_string;
char m_direction;
bool m_multi;
std::vector<LGDictExpContainer> m_subexps;
};
}
#endif